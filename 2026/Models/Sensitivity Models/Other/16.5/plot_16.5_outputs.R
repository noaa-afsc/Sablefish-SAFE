# Purpose: Diagnostic plots for ADMB model 16.5 (2026 data)
# Reads tem.rdat, tem.std, tem.cor, sable.rep, tem.par, tem.ctl, tem_2026_na_wh.dat
# Writes pngs + one combined pdf to 16.5/figs
#
# Index / fleet mapping (from .dat headers):
#   srv1 = Domestic LL survey RPW (not fit)   srv2 = Japanese LL survey RPW (not fit)
#   srv3 = Domestic LL survey RPN             srv4 = Japanese LL survey RPN
#   srv5 = Domestic LL fishery CPUE           srv6 = Japanese LL fishery CPUE
#   srv7 = GOA trawl survey biomass
#   fish1 = fixed gear pre-IFQ, fish4 = fixed gear IFQ (1995+), fish3 = trawl
#   fish5 / srv10 recent blocks are off in this ctl (ph_ifq_block2 = ph_LL_block2 = -4)

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(SPoRC)

mod_dir <- here::here("Mods", "___2026_Final_Mods_Nov", "16.5")
fig_dir <- file.path(mod_dir, "figs")
dir.create(fig_dir, showWarnings = FALSE)
comp_rds <- c("26.1" = here::here("Mods", "___2026_Final_Mods_Nov", "26.1_1Reg", "26.1_1reg_v1.2_model_results_matt.RDS"),
              "26.2" = here::here("Mods", "___2026_Final_Mods_Nov", "26.2_FAA", "26.2b_v1.2_model_results_matt.RDS"))

# Read in model outputs ---------------------------------------------------
rd <- dget(file.path(mod_dir, "tem.rdat"))
rep_lines <- readLines(file.path(mod_dir, "sable.rep"))
dat_lines <- readLines(file.path(mod_dir, "tem_2026_na_wh.dat"))
std <- read.table(file.path(mod_dir, "tem.std"), skip = 1, col.names = c("index", "name", "value", "sd"))
par_hdr <- readLines(file.path(mod_dir, "tem.par"), n = 1)

rep_val <- function(label) as.numeric(rep_lines[grep(paste0("^\\s*", label, "\\s*$"), rep_lines)[1] + 1])
B100 <- rep_val("B_100"); B40 <- rep_val("B_40"); B35 <- rep_val("B_35")
F40 <- rep_val("F_40"); F35 <- rep_val("F_35")
ABC <- rep_val("ABC for 2027"); OFL <- rep_val("OFL for 2027")
max_grad <- as.numeric(sub(".*Maximum gradient component = ", "", par_hdr))
n_par <- as.numeric(sub(".*Number of parameters = (\\d+).*", "\\1", par_hdr))

ts <- rd$t.series
yrs <- ts$year
endyr <- max(yrs)

# get a time series (and sd) out of the std file
std_series <- function(nm, yr) {
  s <- std[std$name == nm, ]
  data.frame(year = yr, est = s$value, sd = s$sd)
}

plots <- list()
save_fig <- function(p, name, w = 15, h = 10) {
  ggsave(file.path(fig_dir, paste0(name, ".png")), p, width = w, height = h, dpi = 200)
  plots[[name]] <<- p
}

# print a quick summary to console
cat("nLL:", rd$likecomp["obj.fun"], " | max grad:", max_grad, " | npar:", n_par, "\n",
    "M:", exp(std$value[std$name == "logm"]), " | B100/B40/B35:", B100, B40, B35, "\n",
    "F40/F35:", F40, F35, " | SSB", endyr, ":", ts$spbiom[ts$year == endyr], "\n",
    "2027 ABC/OFL:", ABC, OFL, "\n")

# Time series -------------------------------------------------------------
ssb <- std_series("ssbsd", yrs) %>% mutate(lci = est - 1.96 * sd, uci = est + 1.96 * sd)
rec <- std_series("pred_rec", yrs) %>%
  mutate(cv = sd / est, lci = est * exp(-1.96 * sqrt(log(1 + cv^2))), uci = est * exp(1.96 * sqrt(log(1 + cv^2))))

p_ssb <- ggplot(ssb, aes(x = year, y = est)) +
  geom_ribbon(aes(ymin = pmax(lci, 0), ymax = uci), alpha = 0.3) +
  geom_line(lwd = 1.3) +
  geom_hline(yintercept = B40, lty = 2, col = "red") +
  geom_hline(yintercept = B35, lty = 3, col = "red") +
  coord_cartesian(ylim = c(0, NA)) +
  labs(x = "Year", y = "SSB (kt)")

p_tb <- ggplot(ts, aes(x = year, y = totbiom)) +
  geom_line(lwd = 1.3) +
  coord_cartesian(ylim = c(0, NA)) +
  labs(x = "Year", y = "Total Biomass (kt)")

p_rec <- ggplot(rec, aes(x = year, y = est)) +
  geom_col(alpha = 0.5) +
  geom_errorbar(aes(ymin = lci, ymax = uci), width = 0) +
  coord_cartesian(ylim = c(0, max(rec$est) * 1.15)) +
  labs(x = "Year", y = "Age-2 Recruits (millions)")

p_f <- ggplot(ts, aes(x = year, y = fmort)) +
  geom_line(lwd = 1.3) +
  geom_hline(yintercept = F40, lty = 2, col = "red") +
  coord_cartesian(ylim = c(0, NA)) +
  labs(x = "Year", y = "Fishing Mortality")

p_catch <- ts %>%
  select(year, `Fixed Gear` = Catch_HAL, Trawl = Catch_TWL) %>%
  pivot_longer(-year, names_to = "Gear", values_to = "catch") %>%
  ggplot(aes(x = year, y = catch, fill = Gear)) +
  geom_col() +
  labs(x = "Year", y = "Predicted Catch (kt)", fill = "")

p_ratio <- ts %>%
  transmute(year, `SSB/B40` = spbiom / B40, `F/F40` = fmort / F40) %>%
  pivot_longer(-year) %>%
  ggplot(aes(x = year, y = value, color = name)) +
  geom_line(lwd = 1.3) +
  geom_hline(yintercept = 1, lty = 2) +
  labs(x = "Year", y = "Ratio", color = "")

save_fig((p_ssb + p_tb) / (p_rec + p_f) / (p_catch + p_ratio) &
           scale_x_continuous(breaks = seq(1960, 2020, 20)) & theme_sablefish(),
         "01_time_series", 18, 16)

# Phase plot --------------------------------------------------------------
phase <- ts %>% transmute(year, B = spbiom / B40, F = fmort / F40)
p_phase <- ggplot(phase, aes(x = B, y = F)) +
  geom_hline(yintercept = 1, lty = 2) +
  geom_vline(xintercept = 1, lty = 2) +
  geom_path() +
  geom_point() +
  geom_text(data = phase %>% filter(year %in% c(min(yrs), seq(1970, 2020, 10), endyr)),
            aes(label = year), vjust = -0.8, size = 5) +
  labs(x = "SSB/B40", y = "F/F40") +
  theme_sablefish()
save_fig(p_phase, "02_phase_plot", 10, 8)

# Index fits --------------------------------------------------------------
idx_names <- c("Domestic LL Survey RPW (not fit)", "Japanese LL Survey RPW (not fit)",
               "Domestic LL Survey RPN", "Japanese LL Survey RPN",
               "Domestic LL Fishery CPUE", "Japanese LL Fishery CPUE",
               "GOA Trawl Survey Biomass")

idx <- bind_rows(lapply(1:7, function(i) {
  d <- rd[[paste0("obssrv", i)]]
  out <- data.frame(Year = as.numeric(rownames(d)), obs = d[[1]], lci = d[[2]], uci = d[[3]],
                    pred = d[[4]], Index_Type = idx_names[i])
  # LL survey rows have the SE in both ci columns (from the .dat), so make a 95% ci
  if (isTRUE(all.equal(out$lci, out$uci))) {
    out$lci <- out$obs - 1.96 * d[[2]]
    out$uci <- out$obs + 1.96 * d[[2]]
  }
  out
})) %>% mutate(Index_Type = factor(Index_Type, levels = idx_names))

p_idx <- ggplot(idx) +
  geom_line(aes(x = Year, y = pred), lwd = 1.3, col = "red") +
  geom_pointrange(aes(x = Year, y = obs, ymin = lci, ymax = uci), color = "blue") +
  facet_wrap(~Index_Type, scales = "free", ncol = 2) +
  expand_limits(y = 0) +
  labs(x = "Year", y = "Index") +
  theme_sablefish()
save_fig(p_idx, "03_index_fits", 16, 18)

# log residuals for the indices that are actually fit
sdnr <- sapply(1:7, function(i) as.numeric(sub(".*SDNR\\d\\s+", "", rep_lines[grep(paste0("^\\s*SDNR", i, "\\s"), rep_lines)[1]])))
print(data.frame(index = idx_names, SDNR = sdnr))

p_idx_res <- idx %>%
  filter(!grepl("not fit", Index_Type)) %>%
  mutate(resid = log(obs / pred)) %>%
  ggplot(aes(x = Year, y = resid)) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_segment(aes(xend = Year, yend = 0)) +
  geom_point(size = 2) +
  facet_wrap(~Index_Type, scales = "free_x", ncol = 2) +
  labs(x = "Year", y = "log(obs/pred)") +
  theme_sablefish()
save_fig(p_idx_res, "04_index_residuals", 16, 12)

# Selectivity -------------------------------------------------------------
sel_map <- c(fish1 = "Fixed Gear Fishery (pre-IFQ)", fish4 = "Fixed Gear Fishery (IFQ)",
             fish3 = "Trawl Fishery", srv1 = "Domestic LL Survey",
             srv2 = "Japanese LL Survey", srv7 = "GOA Trawl Survey")
sel <- rd$agesel %>%
  mutate(Age = as.numeric(rownames(rd$agesel))) %>%
  pivot_longer(-Age, names_to = "col", values_to = "sel") %>%
  mutate(code = sub("sel\\..*", "", col), Sex = ifelse(grepl("\\.f$", col), "Female", "Male")) %>%
  filter(code %in% names(sel_map)) %>%
  mutate(Fleet = factor(sel_map[code], levels = sel_map))

p_sel <- ggplot(sel, aes(x = Age, y = sel, color = Sex)) +
  geom_line(lwd = 1.3) +
  facet_wrap(~Fleet) +
  labs(x = "Age", y = "Selectivity", color = "") +
  theme_sablefish()
save_fig(p_sel, "05_selectivity", 16, 10)

# Composition data --------------------------------------------------------
# pearson residuals use the multinomial N in the likelihood: nsamples (.dat) * weight (.ctl)
dat_after <- function(pattern) {
  i <- grep(pattern, dat_lines)[1]
  as.numeric(strsplit(trimws(dat_lines[i + 1]), "\\s+")[[1]])
}
lk_lines <- rep_lines[grep("^\\s*[0-9.eE+-]+\\s+[0-9.eE+-]+\\s+\\S+_(Likelihood|Penalty)", rep_lines)]
wts <- setNames(as.numeric(sub("^\\s*(\\S+).*", "\\1", lk_lines)), sub("^\\s*\\S+\\s+\\S+\\s+", "", lk_lines))
w <- function(key) unname(wts[grep(key, names(wts))[1]])

comp_long <- function(obs, exp, N, label) {
  o <- as.data.frame(as.table(obs)); e <- as.data.frame(as.table(exp))
  names(o) <- c("Year", "bin", "obs"); names(e) <- c("Year", "bin", "pred")
  left_join(o, e, by = c("Year", "bin")) %>%
    left_join(data.frame(Year = rownames(obs), N = N), by = "Year") %>%
    mutate(Year = as.numeric(as.character(Year)), bin = as.numeric(as.character(bin)),
           resid = (obs - pred) / sqrt(pred * (1 - pred) / N), Fleet = label)
}

srv7_N <- as.numeric(strsplit(trimws(dat_lines[grep("^# GOA trawl survey size composition", dat_lines) + 7]), "\\s+")[[1]])
n_fish_len <- dat_after("nsamples_fish_size\\(1,nyrs_fish_size") * w("Size_Composition_Likelihood_Fixed")
n_fish3_len <- dat_after("nsamples_fish3_size") * w("Size_Composition_Likelihood_Trawl")
n_srv1_len <- dat_after("nsamples_srv1_size") * w("Survey_Size_Composition_Likelihood_Domestic")
n_srv2_len <- dat_after("nsamples_srv2_size") * w("Survey_Size_Composition_Likelihood_Cooperative")

ac_all <- bind_rows(
  comp_long(rd$oac.fish1, rd$eac.fish1, dat_after("nsamples_fish_age\\(1,nyrs_fish_age") * w("Fishery_Age"), "Fixed Gear Fishery"),
  comp_long(rd$oac.srv1, rd$eac.srv1, dat_after("nsamples_srv1_age") * w("Survey_Age_Composition_Likelihood_DomesticLL"), "Domestic LL Survey"),
  comp_long(rd$oac.srv2, rd$eac.srv2, dat_after("nsamples_fish_age\\(1,nyrs_srv2_age") * w("Survey_Age_Composition_Likelihood_CooperativeLL"), "Japanese LL Survey")
) %>% mutate(Fleet = factor(Fleet, levels = unique(Fleet)))

lc_all <- bind_rows(
  comp_long(rd$olc.fish1.f, rd$elc.fish1.f, n_fish_len, "Fixed Gear Fishery Female"),
  comp_long(rd$olc.fish1.m, rd$elc.fish1.m, n_fish_len, "Fixed Gear Fishery Male"),
  comp_long(rd$olc.fish3.f, rd$elc.fish3.f, n_fish3_len, "Trawl Fishery Female"),
  comp_long(rd$olc.fish3.m, rd$elc.fish3.m, n_fish3_len, "Trawl Fishery Male"),
  comp_long(rd$olc.srv1.f, rd$elc.srv1.f, n_srv1_len, "Domestic LL Survey Female"),
  comp_long(rd$olc.srv1.m, rd$elc.srv1.m, n_srv1_len, "Domestic LL Survey Male"),
  comp_long(rd$olc.srv2.f, rd$elc.srv2.f, n_srv2_len, "Japanese LL Survey Female"),
  comp_long(rd$olc.srv2.m, rd$elc.srv2.m, n_srv2_len, "Japanese LL Survey Male"),
  comp_long(rd$olc.srv7.f, rd$elc.srv7.f, srv7_N * w("GOATrawl"), "GOA Trawl Survey Female"),
  comp_long(rd$olc.srv7.m, rd$elc.srv7.m, srv7_N * w("GOATrawl"), "GOA Trawl Survey Male")
) %>% mutate(Fleet = factor(Fleet, levels = unique(Fleet)))

# aggregated fits
agg_comp <- function(d, xlab) {
  d %>%
    group_by(Fleet, bin) %>%
    summarize(obs = mean(obs), pred = mean(pred), .groups = "drop") %>%
    ggplot() +
    geom_col(aes(x = bin, y = obs), alpha = 0.5) +
    geom_line(aes(x = bin, y = pred), lwd = 1.3, col = "red") +
    facet_wrap(~Fleet, scales = "free_y", ncol = 2) +
    labs(x = xlab, y = "Proportion") +
    theme_sablefish()
}
save_fig(agg_comp(ac_all, "Age") + facet_wrap(~Fleet, scales = "free_y", ncol = 1), "06_agecomp_aggregate", 12, 14)
save_fig(agg_comp(lc_all, "Length (cm)"), "07_lencomp_aggregate", 16, 20)

# pearson residual bubble plots (bubble size capped at 4 so a few outliers don't swamp everything)
bubble <- function(d, ylab, ncol = 1) {
  d %>%
    filter(is.finite(resid)) %>%
    mutate(Sign = ifelse(resid > 0, "Positive", "Negative")) %>%
    ggplot(aes(x = Year, y = bin)) +
    geom_point(aes(size = pmin(abs(resid), 4), color = Sign), alpha = 0.6) +
    scale_color_manual(values = c(Positive = "blue", Negative = "red")) +
    scale_size_area(max_size = 6) +
    facet_wrap(~Fleet, ncol = ncol, scales = "free_x") +
    labs(x = "Year", y = ylab, size = "|Resid|", color = "") +
    theme_sablefish()
}
save_fig(bubble(ac_all, "Age"), "08_agecomp_residuals", 16, 16)
save_fig(bubble(lc_all, "Length (cm)", ncol = 2), "09_lencomp_residuals", 20, 24)

# annual fits by fleet
annual_comp <- function(d, xlab) {
  ggplot(d) +
    geom_col(aes(x = bin, y = obs), alpha = 0.5) +
    geom_line(aes(x = bin, y = pred), col = "red") +
    facet_wrap(~Year) +
    labs(x = xlab, y = "Proportion") +
    theme_sablefish()
}
i <- 10
for (f in levels(ac_all$Fleet)) {
  save_fig(annual_comp(filter(ac_all, Fleet == f), "Age"),
           sprintf("%02d_agefits_%s", i, gsub(" ", "_", f)), 20, 16)
  i <- i + 1
}
for (f in levels(lc_all$Fleet)) {
  save_fig(annual_comp(filter(lc_all, Fleet == f), "Length (cm)"),
           sprintf("%02d_lenfits_%s", i, gsub(" ", "_", f)), 20, 16)
  i <- i + 1
}

# Likelihood components ---------------------------------------------------
lk <- data.frame(nLL = as.numeric(sub("^\\s*\\S+\\s+(\\S+)\\s+.*", "\\1", lk_lines)),
                 component = trimws(sub("^\\s*\\S+\\s+\\S+\\s+", "", lk_lines)))
lk <- bind_rows(
  lk,
  data.frame(nLL = unname(rd$likecomp[c("L.surv5", "L.surv6", "L.surv7")]),
             component = c("Domestic LL Fishery CPUE", "Japanese LL Fishery CPUE", "GOA Trawl Survey Biomass")),
  data.frame(nLL = sum(as.numeric(sub("^\\s*(\\S+)\\s+priors.*", "\\1", rep_lines[grep("priors", rep_lines)]))),
             component = "Priors")
) %>%
  filter(nLL > 0) %>%
  mutate(component = gsub("_", " ", sub("_Likelihood", "", component)),
         groups = case_when(grepl("Age", component) ~ "Age Comps",
                            grepl("Size", component) ~ "Length Comps",
                            grepl("Index|CPUE|Biomass", component) ~ "Indices",
                            grepl("Catch", component) ~ "Catch",
                            TRUE ~ "Penalties"))

p_lk <- ggplot(lk, aes(x = reorder(component, -nLL), y = nLL, fill = groups)) +
  geom_col() +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(x = "", y = "nLL", fill = "") +
  theme_sablefish()
save_fig(p_lk, "30_likelihood_components", 16, 10)

# Recruitment deviations --------------------------------------------------
styr_rec <- as.numeric(readLines(file.path(mod_dir, "tem.ctl"))[4])
rdev <- std[std$name == "log_rec_dev", ] %>% mutate(Year = seq(styr_rec, by = 1, length.out = n()))
p_rdev <- ggplot(rdev, aes(x = Year, y = value)) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_pointrange(aes(ymin = value - 1.96 * sd, ymax = value + 1.96 * sd)) +
  geom_vline(xintercept = min(yrs) - 0.5, lty = 3) +
  labs(x = "Year", y = "log Recruitment Deviation") +
  theme_sablefish()
save_fig(p_rdev, "31_rec_devs", 16, 7)

# Numbers at age ----------------------------------------------------------
naa <- rd$natage.female + rd$natage.male
naa_df <- as.data.frame(as.table(naa / rowSums(naa))) %>%
  setNames(c("Year", "Age", "prop")) %>%
  mutate(Year = as.numeric(as.character(Year)), Age = as.numeric(as.character(Age)))
p_naa <- ggplot(naa_df, aes(x = Year, y = Age, size = prop)) +
  geom_point(alpha = 0.5) +
  scale_size_area(max_size = 6) +
  labs(x = "Year", y = "Age", size = "Proportion") +
  theme_sablefish()
save_fig(p_naa, "32_numbers_at_age", 16, 9)

# Biology -----------------------------------------------------------------
bio <- rd$growthmat %>% mutate(Age = as.numeric(rownames(rd$growthmat)))
p_wt <- bio %>%
  select(Age, Female = wt.f, Male = wt.m) %>%
  pivot_longer(-Age, names_to = "Sex") %>%
  ggplot(aes(x = Age, y = value, color = Sex)) +
  geom_line(lwd = 1.3) +
  labs(x = "Age", y = "Weight (kg)", color = "")
p_mat <- ggplot(bio, aes(x = Age, y = mage)) +
  geom_line(lwd = 1.3) +
  labs(x = "Age", y = "Maturity x Weight (Female)")
save_fig((p_wt + p_mat) & theme_sablefish(), "33_biology", 16, 7)

# Projections -------------------------------------------------------------
nproj <- sum(std$name == "spawn_biom_proj")
proj <- bind_rows(
  std_series("spawn_biom_proj", endyr + seq_len(nproj)) %>% mutate(Type = "SSB (kt)"),
  std_series("pred_catch_proj", endyr + seq_len(nproj)) %>% mutate(Type = "Catch at maxABC (kt)")
) %>% mutate(lci = est - 1.96 * sd, uci = est + 1.96 * sd)
hist_ssb <- ssb %>% filter(year >= endyr - 25) %>% mutate(Type = "SSB (kt)")
p_proj <- ggplot(proj, aes(x = year, y = est)) +
  geom_ribbon(data = hist_ssb, aes(ymin = lci, ymax = uci), alpha = 0.3) +
  geom_line(data = hist_ssb, lwd = 1.3) +
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.3, fill = "red") +
  geom_line(lwd = 1.3, col = "red") +
  geom_hline(data = data.frame(Type = "SSB (kt)", y = B40), aes(yintercept = y), lty = 2) +
  facet_wrap(~Type, scales = "free_y") +
  expand_limits(y = 0) +
  labs(x = "Year", y = "") +
  theme_sablefish()
save_fig(p_proj, "35_projections", 16, 7)

# Compare to 26.1 and 26.2 ------------------------------------------------
comp_rds <- comp_rds[file.exists(comp_rds)]
if (length(comp_rds) > 0) {
  comp_df <- bind_rows(
    data.frame(Year = yrs, SSB = ts$spbiom, Rec = ts$Recr, TotB = ts$totbiom, Model = "16.5"),
    bind_rows(lapply(names(comp_rds), function(m) {
      x <- readRDS(comp_rds[m])
      data.frame(Year = min(yrs) - 1 + x$data$years, SSB = as.numeric(x$rep$SSB),
                 Rec = as.numeric(x$rep$Rec), TotB = as.numeric(x$rep$Total_Biom), Model = m)
    }))
  ) %>%
    pivot_longer(c(SSB, TotB, Rec)) %>%
    mutate(name = factor(name, levels = c("SSB", "TotB", "Rec"),
                         labels = c("SSB (kt)", "Total Biomass (kt)", "Age-2 Recruits (millions)")))
  p_comp <- ggplot(comp_df, aes(x = Year, y = value, color = Model)) +
    geom_line(lwd = 1.3) +
    facet_wrap(~name, scales = "free_y", ncol = 1) +
    expand_limits(y = 0) +
    labs(x = "Year", y = "", color = "Model") +
    theme_sablefish()
  save_fig(p_comp, "36_compare_26.1_26.2", 14, 14)
}

# Retrospective -----------------------------------------------------------
# peels are the 16.5/<terminal year> folders (same ctl, data trimmed to that year)
retro_dirs <- list.files(mod_dir, pattern = "^20[0-9]{2}$", full.names = TRUE)
if (length(retro_dirs) > 0) {
  # fleet F by year is only written to tem.rep
  rep_vec <- function(d, label) {
    r_rep <- readLines(file.path(d, "tem.rep"))
    as.numeric(strsplit(trimws(r_rep[grep(paste0("^", label, " "), r_rep)[1]]), "\\s+")[[1]][-1])
  }
  retro <- bind_rows(lapply(c(mod_dir, retro_dirs), function(d) {
    r_ts <- dget(file.path(d, "tem.rdat"))$t.series
    r_rep <- readLines(file.path(d, "sable.rep"))
    r_B40 <- as.numeric(r_rep[grep("^\\s*B_40\\s*$", r_rep)[1] + 1])
    data.frame(term_yr = max(r_ts$year), Year = r_ts$year, SSB = r_ts$spbiom, Rec = r_ts$Recr,
               Brat = r_ts$spbiom / r_B40, Frat = r_ts$Frat)
  })) %>%
    group_by(Year) %>%
    mutate(SSB_rel = SSB / SSB[term_yr == endyr] - 1, Rec_rel = Rec / Rec[term_yr == endyr] - 1) %>%
    ungroup() %>%
    mutate(term_yr = factor(term_yr))

  # Mohn's rho over the peels, at each peel's terminal year
  rho <- retro %>%
    filter(term_yr != endyr, Year == as.numeric(as.character(term_yr))) %>%
    summarize(SSB = mean(SSB_rel), Rec = mean(Rec_rel))
  print(rho)

  p_retro_ssb <- ggplot(retro, aes(x = Year, y = SSB, color = term_yr)) +
    geom_line(lwd = 1.1) +
    geom_point(data = retro %>% filter(Year == as.numeric(as.character(term_yr))), size = 2.5) +
    expand_limits(y = 0) +
    labs(x = "Year", y = "SSB (kt)", color = "Terminal Year")
  p_retro_rel <- ggplot(retro %>% filter(term_yr != endyr), aes(x = Year, y = SSB_rel, color = term_yr)) +
    geom_hline(yintercept = 0, lty = 2) +
    geom_line(lwd = 1.1) +
    geom_point(data = retro %>% filter(term_yr != endyr, Year == as.numeric(as.character(term_yr))), size = 2.5) +
    labs(x = "Year", y = "Relative Difference in SSB", color = "Terminal Year",
         title = paste0("Mohn's rho = ", round(rho$SSB, 3)))
  save_fig(wrap_plots(p_retro_ssb, p_retro_rel + guides(color = "none"), ncol = 1) &
             scale_color_discrete(drop = FALSE) & theme_sablefish(),
           "37_retro", 14, 14)

  # ABC / OFL / B40 for the year after each peel's terminal year, from sable.rep
  retro_abc <- bind_rows(lapply(c(retro_dirs, mod_dir), function(d) {
    r_rep <- readLines(file.path(d, "sable.rep"))
    r_val <- function(label) as.numeric(r_rep[grep(paste0("^\\s*", label, "\\s*$"), r_rep)[1] + 1])
    t_yr <- max(dget(file.path(d, "tem.rdat"))$t.series$year)
    data.frame(term_yr = t_yr, ABC = r_val(paste("ABC for", t_yr + 1)), OFL = r_val(paste("OFL for", t_yr + 1)),
               B40 = r_val("B_40"), F40 = r_val("F_40"),
               fratio = tail(rep_vec(d, "Fully_selected_F_LL") /
                               (rep_vec(d, "Fully_selected_F_LL") + rep_vec(d, "Fully_selected_F_Trawl")), 1),
               SSB_next = r_val(paste("Female_Spawning Biomass for", t_yr + 1)))
  }))
  print(retro_abc, digits = 3)
  write.csv(retro_abc, file.path(fig_dir, "retro_abc.csv"), row.names = FALSE)

  p_abc <- retro_abc %>%
    select(term_yr, ABC, OFL) %>%
    pivot_longer(-term_yr) %>%
    ggplot(aes(x = term_yr, y = value, color = name)) +
    geom_line(lwd = 1.1) +
    geom_point(size = 3) +
    expand_limits(y = 0) +
    labs(x = "Terminal Year of Peel", y = "Catch Limit for Following Year (kt)", color = "")
  p_b40 <- retro_abc %>%
    select(term_yr, `Projected SSB` = SSB_next, B40) %>%
    pivot_longer(-term_yr) %>%
    ggplot(aes(x = term_yr, y = value, color = name)) +
    geom_line(lwd = 1.1) +
    geom_point(size = 3) +
    expand_limits(y = 0) +
    labs(x = "Terminal Year of Peel", y = "kt", color = "")
  save_fig(wrap_plots(p_abc, p_b40, ncol = 1) & scale_x_continuous(breaks = retro_abc$term_yr) & theme_sablefish(),
           "38_retro_abc", 12, 12)

  # status by peel, each relative to that peel's own B40 / F40
  p_brat <- ggplot(retro, aes(x = Year, y = Brat, color = term_yr)) +
    geom_hline(yintercept = 1, lty = 2) +
    geom_line(lwd = 1.1) +
    geom_point(data = retro %>% filter(Year == as.numeric(as.character(term_yr))), size = 2.5) +
    labs(x = "Year", y = "SSB/B40", color = "Terminal Year")
  p_frat <- ggplot(retro, aes(x = Year, y = Frat, color = term_yr)) +
    geom_hline(yintercept = 1, lty = 2) +
    geom_line(lwd = 1.1) +
    geom_point(data = retro %>% filter(Year == as.numeric(as.character(term_yr))), size = 2.5) +
    labs(x = "Year", y = "F/F40", color = "Terminal Year")
  save_fig(wrap_plots(p_brat, p_frat + guides(color = "none"), ncol = 1) &
             scale_color_discrete(drop = FALSE) & theme_sablefish(),
           "39_retro_status", 14, 14)
}

# High correlations (|r| > 0.9) -------------------------------------------
cor_tok <- scan(file.path(mod_dir, "tem.cor"), what = "", skip = 2, quiet = TRUE)
npar_cor <- max(std$index); pos <- 1
cor_mat <- matrix(NA, npar_cor, npar_cor); cor_names <- character(npar_cor)
for (k in 1:npar_cor) {
  cor_names[k] <- cor_tok[pos + 1]
  cor_mat[k, 1:k] <- as.numeric(cor_tok[(pos + 4):(pos + 3 + k)])
  pos <- pos + 4 + k
}
cor_names <- make.unique(cor_names, sep = "_")
derived <- "^(ssbsd|pred_rec|spawn_biom_proj|tot_biom_proj|pred_catch_proj)"
hi <- which(abs(cor_mat) > 0.9 & lower.tri(cor_mat), arr.ind = TRUE)
hi_cor <- data.frame(par1 = cor_names[hi[, 1]], par2 = cor_names[hi[, 2]], r = cor_mat[hi]) %>%
  filter(!grepl(derived, par1), !grepl(derived, par2)) %>%
  arrange(desc(abs(r)))
write.csv(hi_cor, file.path(fig_dir, "high_correlations.csv"), row.names = FALSE)
write.csv(std %>% filter(!grepl(paste0(derived, "|log_rec_dev|log_F_devs"), name)),
          file.path(fig_dir, "key_parameters.csv"), row.names = FALSE)

# Combined pdf ------------------------------------------------------------
pdf(file.path(fig_dir, "16.5_diagnostics.pdf"), width = 16, height = 12, onefile = TRUE)
for (p in plots) print(p)
dev.off()
