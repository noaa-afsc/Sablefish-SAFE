# Purpose: Compare spatial models together, as well as singl region and faa models

library(here)
library(SPoRC)
library(tidyverse)
library(patchwork)
library(gridExtra)
source(here("R", "Diagnostic_Helpers.R"))

comparison_name <- "CPUE_TS"
do_spatial_comp <- 0                                                          # whether to do a quick figure comparing 3 and 5 reg spatial models, make sure to check path name is correct for reading in associated RDS ~line 70


out_dir <- here("Mods", "___2026_Final_Mods_Nov", "_Model Comparison Plots",comparison_name)

# Compare models ------------------------------------
# main comparison: 25.12, 26.1, 26.2, and 26.2b (list names are the model names in the figures)

mod_dir <- here("Mods", "___2026_Final_Mods_Nov")

###############################################################################################################################################################
#--- Adjust these for models want to plot....
models <- list(
"26.2_FAA" = readRDS(file.path(mod_dir, "26.2_FAA","R_out", "26.2_FAA_2026_v1.2_model_results.RDS")),
"26.2_FAA_CPUE" = readRDS(file.path(mod_dir, "26.2_FAA_CPUE", "R_out","26.2_FAA_CPUE_2026_v1.2_model_results.RDS")),
"26.2_FAA_TS" = readRDS(file.path(mod_dir, "26.2_FAA_TS", "R_out","26.2_FAA_TS_2026_v1.2_model_results.RDS")),
"26.2_FAA_TS_CPUE" = readRDS(file.path(mod_dir, "26.2_FAA_TS_CPUE", "R_out","26.2_FAA_TS_CPUE_2026_v1.2_model_results.RDS"))
)

# fleet naming stuff
fish_names <- list("26.2_FAA" = c("FG", "TF"),
                   "26.2_FAA_CPUE" = c("FG", "TF"),
                   "26.2_FAA_TS" = c("FG", "TF"),
                   "26.2_FAA_TS_CPUE" = c("FG", "TF"))

srv_names <- list("26.2_FAA" = c("BS LLS", "AI LLS", "GOA LLS", "GOA TS", "JPN LLS"),
                  "26.2_FAA_CPUE" = c("BS LLS", "AI LLS", "GOA LLS", "GOA TS", "JPN LLS"),
                  "26.2_FAA_TS" = c("BS LLS", "AI LLS", "GOA LLS", "GOA TS", "JPN LLS"),
                  "26.2_FAA_TS_CPUE" = c("BS LLS", "AI LLS", "GOA LLS", "GOA TS", "JPN LLS"))
###############################################################################################################################################################



###############################################################################################################################################################
#--- This is the default setup for the Base comparison
#models <- list(
#  "25.12"                    = readRDS(file.path(mod_dir, "25.12", "25.12_v1.2_model_results_matt.RDS")),
#  "26.1_1Reg"                = readRDS(file.path(mod_dir, "26.1_1Reg", "26.1_1reg_v1.2_model_results_matt.RDS")),
#  "26.2_FAA"                 = readRDS(file.path(mod_dir, "26.2_FAA","R_out", "26.2_FAA_2026_v1.2_model_results.RDS")),
#  "26.2b_FAA_3Flt_Fish_Dome" = readRDS(file.path(mod_dir, "26.2b_FAA_3Flt_Fish_Dome", "26.2b_FAA_3Flt_Fish_Dome_v1.2_model_results.RDS"))
#)

# fleet naming stuff
#fish_names <- list("25.12"                    = c("FG", "TF"),
#                   "26.1_1Reg"                = c("FG", "TF"),
#                   "26.2_FAA"                 = c("FG", "TF"),
#                   "26.2b_FAA_3Flt_Fish_Dome" = c("BS FG", "AI FG", "GOA FG", "TF"))

#srv_names <- list("25.12"                    = c("LLS", "GOA TS", "JPN LLS"),
#                  "26.1_1Reg"                = c("LLS", "GOA TS", "JPN LLS"),
#                  "26.2_FAA"                 = c("BS LLS", "AI LLS", "GOA LLS", "GOA TS", "JPN LLS"),
#                  "26.2b_FAA_3Flt_Fish_Dome" = c("BS LLS", "AI LLS", "GOA LLS", "GOA TS", "JPN LLS"))
###############################################################################################################################################################


dir.create(out_dir, showWarnings = FALSE)

# Aggregated SSB and Recruitment time series ---------------------------------

# helper to build aggregate time-series reuslts
build_ts_df <- function(metric3, metric5) {
  n_yr <- ncol(metric3)
  data.frame(
    year = rep(seq_len(n_yr), 2),
    model = rep(c("3-region", "5-region"), each = n_yr),
    bs = c(metric3[1, ], metric5[1, ]),
    ai = c(metric3[2, ], metric5[2, ]),
    goa = c(metric3[3, ], colSums(metric5[3:5, ])),
    total = c(colSums(metric3), colSums(metric5))
  ) %>%
    pivot_longer(c(bs, ai, goa, total), names_to = "region", values_to = "value") %>%
    mutate(
      region = factor(region, levels = c("bs", "ai", "goa", "total"),
                       labels = c("BS", "AI", "GOA", "Aggregated")),
      model = factor(model, levels = c("3-region", "5-region"))
    )
}

if(do_spatial_comp == 1){
  
  three_rg <- readRDS(here("Mods", "___2026_Final_Mods_Nov", "26.4_Spat_3_Reg", "26.3_3Reg_v1.2_model_results.RDS"))
  five_rg  <- readRDS(here("Mods", "___2026_Final_Mods_Nov", "26.3_Spat_5_Reg", "26.3_5reg_v1.2_model_results.RDS"))
  
  ssb_df <- build_ts_df(three_rg$rep$SSB[1, , ], five_rg$rep$SSB[1, , ]) %>% mutate(metric = "SSB")
  rec_df <- build_ts_df(three_rg$rep$Rec[1, , ], five_rg$rep$Rec[1, , ]) %>% mutate(metric = "Recruitment")
  ts_df <- bind_rows(ssb_df, rec_df) %>% mutate(year = year + 1959)  # model year 1 = 1960
  
  png(file.path(out_dir, "ts_comparison.png"), width = 15, height = 8, units = "in", res = 200)
  ggplot(ts_df, aes(x = year, y = value, color = model, lty = model)) +
    geom_line(linewidth = 1) +
    facet_grid(metric ~ region, scales = "free_y") +
    scale_x_continuous(breaks = scales::pretty_breaks(5)) +
    scale_y_continuous(breaks = scales::pretty_breaks(4)) +
    labs(x = "Year", y = "Value", lty = "Model", color = "Model") +
    theme_bw(base_size = 18)
  dev.off()
}


# Comparison plots ---------------------------------------------------------------
get_model_colors <- function(model_names) {
  setNames(ggthemes::colorblind_pal()(length(model_names)), model_names)
}

# natural mortality, weight-at-age, and maturity-at-age (no movement, single region)
plot_comp_biologicals <- function(models, model_colors, region_names, sex_names, rec_age) {
  data <- lapply(models, function(model) model$data)
  rep <- lapply(models, function(model) model$rep)
  bio_plots <- SPoRC::get_biological_plot(data, rep, names(models))
  bio_plots <- bio_plots[-1] # first element is the movement plots

  lapply(bio_plots, function(bio_plot) {
    bio_plot$data <- bio_plot$data %>%
      mutate(Age = Age + (rec_age - 1),
             Model = factor(Model, levels = names(models)))

    relabel_facets(bio_plot, region_names, sex_names) +
      scale_color_manual(values = model_colors) +
      guides(linetype = "none") # linetype is population (only one)
  })
}

# data used by each model (each model's panel only has rows for its own data types)
plot_comp_data_used <- function(models, fish_names, srv_names, region_names, styr) {
  data <- lapply(models, function(model) model$data)
  data_plot <- SPoRC::get_data_fitted_plot(data, names(models))

  data_plot$data <- data_plot$data %>%
    mutate(Type = drop_seas_pop(Type), # e.g. "Survey Index Seas 1 Fleet 3" -> "Survey Index Fleet 3"
           Type = ifelse(grepl("Fishery", Type),
                         name_fleets_by_model(Type, Model, fish_names),
                         name_fleets_by_model(Type, Model, srv_names)),
           Model = factor(Model, levels = names(models)),
           Year = Year + (styr - 1))

  data_plot <- data_plot + facet_grid(Model ~ Region, scales = "free_y", space = "free_y")
  relabel_facets(data_plot, region_names)
}

# time series: SSB, total biomass, recruitment, dynamic SSB0, and F by fleet
# returns list(all = every series in one grid, by_type = one plot per type of series)
plot_comp_ts <- function(models, fish_names, model_colors, region_names, styr, rec_age, show_ci = TRUE) {
  rep <- lapply(models, function(model) model$rep)
  sd_rep <- lapply(models, function(model) model$sd_rep)
  ts_plots <- SPoRC::get_ts_plot(rep, sd_rep, names(models), do_ci = show_ci)
  names(ts_plots) <- c("all", "f", "rec", "ssb", "total_biom", "ssb0", "ssb_and_ssb0") # SPoRC's order

  # facet labels for each series (the F rows become "<fleet> F")
  series_labels <- c("SSB" = "SSB (kt)",
                     "Total Biom" = "Total biomass (kt)",
                     "Recruitment" = "Recruits (millions)",
                     "Dynamic SSB0" = "Dynamic SSB0 (kt)")
  f_labels <- paste(unique(unlist(fish_names)), "F")

  # y axis titles
  y_labels <- c(all = "",
                f = "Fully selected F",
                rec = paste0("Age-", rec_age, " recruitment (millions)"),
                ssb = "SSB (kt)",
                total_biom = "Total biomass (kt)",
                ssb0 = "Dynamic SSB0 (kt)",
                ssb_and_ssb0 = "SSB (kt)")
  single_series <- c("rec", "ssb", "total_biom", "ssb0")

  for (ts_type in names(ts_plots)) {
    ts_plot <- ts_plots[[ts_type]]

    ts_plot$data <- ts_plot$data %>%
      mutate(Type = name_fleets_by_model(drop_seas_pop(Type), Model, fish_names), # "Seas 1 Fleet 2 F" -> "TF F"
             Type = ifelse(Type %in% names(series_labels), series_labels[Type], Type),
             Type = factor(Type, levels = c(series_labels, f_labels)),
             Model = factor(Model, levels = names(models)),
             Year = Year + (styr - 1))

    # one series: no need for a series facet, the y axis says what it is
    if (ts_type %in% single_series) ts_plot <- ts_plot + facet_grid(. ~ Region, scales = "free")

    ts_plot <- relabel_facets(ts_plot, region_names) +
      scale_color_manual(values = model_colors) +
      labs(y = y_labels[[ts_type]])

    if (show_ci) {
      has_ci_ribbon <- !is.null(ts_plot$mapping$fill)
      if (has_ci_ribbon) ts_plot <- ts_plot + scale_fill_manual(values = model_colors)
    } else {
      ts_plot$mapping[c("ymin", "ymax", "fill")] <- NULL # otherwise the (undrawn) CIs still set the y range
    }

    ts_plots[[ts_type]] <- ts_plot
  }

  list(all = ts_plots$all, by_type = ts_plots[names(ts_plots) != "all"])
}

# terminal year selectivity by fleet, colored by model: list(fishery, survey)
plot_comp_selex <- function(models, fish_names, srv_names, model_colors, sex_names, rec_age) {
  rep <- lapply(models, function(model) model$rep)
  selex_plots <- SPoRC::get_selex_plot(rep, names(models)) # terminal year by default
  names(selex_plots) <- c("fishery", "survey")
  fleet_names <- list(fishery = fish_names, survey = srv_names)
  y_labels <- c(fishery = "Fishery selectivity", survey = "Survey selectivity")

  for (fleet_type in names(selex_plots)) {
    selex_df <- selex_plots[[fleet_type]]$data %>%
      mutate(Fleet = name_fleets_by_model(Fleet, Model, fleet_names[[fleet_type]]),
             Fleet = factor(Fleet, levels = unique(unlist(fleet_names[[fleet_type]]))),
             Sex = sex_names[as.integer(sub("Sex ", "", Sex))],
             Sex = factor(Sex, levels = sex_names),
             Age = Bin + (rec_age - 1),
             Model = factor(Model, levels = names(models)))

    selex_plots[[fleet_type]] <- ggplot(selex_df, aes(x = Age, y = value, color = Model, linetype = Model)) +
      geom_line(linewidth = 1.2) +
      facet_grid(Sex ~ Fleet) +
      scale_color_manual(values = model_colors) +
      coord_cartesian(ylim = c(0, NA)) +
      labs(x = "Age", y = y_labels[[fleet_type]], color = "Model", linetype = "Model") +
      theme_sablefish() +
      theme(legend.key.width = unit(2, "lines"))
  }
  selex_plots
}

# helper to compare models
compare_models <- function(models, fish_names, srv_names, out_path, styr = 1960,
                           region_names = "AK", sex_names = c("Female", "Male"), rec_age = 2,calc_rec_st_yr = 20,
                           ts_ci = c(TRUE, FALSE), do_phase_plane = TRUE, comp_name = "model_comparison",
                           reference_points_opt = NULL, proj_model_opt = NULL, dir_tab, fig_dir, mod_yr = 2026) {

  # create path if needed
  dir.create(out_path, showWarnings = FALSE, recursive = TRUE)
  model_names <- names(models)
  model_colors <- get_model_colors(model_names)

  # biologicals
  bio_plots <- plot_comp_biologicals(models, model_colors, region_names, sex_names, rec_age)
  png_pages(file.path(out_path, "comp_bio.png"), width = 16, height = 8)
  for (bio_plot in bio_plots) print(bio_plot)
  dev.off() 


  # data used (page height from the number of rows: one per model x data type)
  data_plot <- plot_comp_data_used(models, fish_names, srv_names, region_names, styr)
  n_data_rows <- nrow(distinct(data_plot$data, Model, Type))
  png_pages(file.path(out_path, "comp_data.png"), width = 19, height = 2 + 0.25 * n_data_rows)
  print(data_plot)
  dev.off()

  # time series, with and / or without CIs
  for (show_ci in ts_ci) {
    file_suffix <- if (show_ci) "" else "_noci"
    ts_plots <- plot_comp_ts(models, fish_names, model_colors, region_names, styr, rec_age, show_ci)
    n_series <- length(unique(ts_plots$all$data$Type))

    png_pages(file.path(out_path, paste0("comp_ts", file_suffix, ".png")), width = 16, height = 2 + 2.2 * n_series)
    print(ts_plots$all)
    dev.off()

    png_pages(file.path(out_path, paste0("comp_ts_by_type", file_suffix, ".png")), width = 16, height = 9)
    for (ts_plot in ts_plots$by_type) print(ts_plot)
    dev.off()
  }

  
  # selectivity (fishery page, then survey page)
  selex_plots <- plot_comp_selex(models, fish_names, srv_names, model_colors, sex_names, rec_age)
  png_pages(file.path(out_path, "comp_selex.png"), width = 19, height = 9)
  print(selex_plots$fishery)
  print(selex_plots$survey)
  dev.off()

  # likelihoods (plot, then table)
  data <- lapply(models, function(model) model$data)
  rep <- lapply(models, function(model) model$rep)
  nll_plots <- SPoRC::get_nLL_plot(data, rep, model_names)
  png_pages(file.path(out_path, "comp_nll.png"), width = 19, height = 10)
  for (nll_plot in nll_plots) print(nll_plot)
  dev.off()

  # phase plane for each model (own reference points, not overlaid)
  if (do_phase_plane) {
    phase_plane_status <- data.frame()
    i <- 1
    for (model_name in model_names) {
      phase_plane <- plot_phase_plane(models[[model_name]], styr, title = model_name, rec_age = rec_age, calc_rec_st_yr =calc_rec_st_yr,
                                      dir_tab = dir_tab, fig_dir = fig_dir, model_name = model_name, mod_yr = mod_yr)

     # png(file.path(out_path, paste0("phase_plane_", model_name, ".png")), width = 10, height = 8, units = "in", res = 200)
    #  print(phase_plane[i])
    #  dev.off()
     # i <- i+1
      #phase_plane_status <- bind_rows(phase_plane_status, mutate(status, model = model_name, .before = 1))
    }
    #write.csv(phase_plane_status, file.path(out_path, "phase_plane.csv"), row.names = FALSE)
  }

  # key quantities (ABC etc.) for each model
  if (!is.null(reference_points_opt) && !is.null(proj_model_opt)) {
    key_quants <- SPoRC::get_key_quants(data, rep, reference_points_opt, proj_model_opt, model_names)
    write.csv(key_quants[[1]], file.path(out_path, paste0(comp_name, "_key_quants.csv")), row.names = FALSE)
    quant<-key_quants[[1]]
    return(invisible(key_quants))
  }
  invisible(NULL)
}

# Define HCR to use (NPFMC F40)
HCR_function <- function(x, frp, brp, alpha = 0.05) {
  stock_status <- x / brp                                                      # define stock status
  if(stock_status >= 1) f <- frp                                               # If stock status is > 1, use full Fref
  if(stock_status > alpha && stock_status < 1){                                # If stock status is between brp and alpha (limit/lower BRP), then linearly scale down Fref
    f <- frp * (stock_status - alpha) / (1 - alpha)
  }
  if(stock_status < alpha) f <- 0                                              # If stock status is less than alpha (limit/lower BRP), then no fishing
  return(f)
}

reference_points_opt <- list(SPR_x = 0.4,
                             t_spawn = 0,
                             sex_ratio_f = 0.5,
                             calc_rec_st_yr = 20,
                             rec_age = 2,
                             type = "single_region",
                             what = "SPR"
)
proj_model_opt <- list(n_proj_yrs = 2,
                       n_avg_yrs = 1,
                       HCR_function = HCR_function,
                       recruitment_opt = 'mean_rec',
                       fmort_opt = 'HCR')

compare_models(
  models = models,
  fish_names = fish_names,
  srv_names = srv_names,
  out_path = out_dir,
  styr = 1960,
  comp_name = comparison_name,
  reference_points_opt = reference_points_opt,
  proj_model_opt = proj_model_opt,
  dir_tab = out_dir, fig_dir = out_dir
)

model_names <- names(models)
ssb <- plot_comp_ts(models = models, fish_names = fish_names,
             model_colors = get_model_colors(model_names), region_names = "AK", styr =1960, rec_age =2 , show_ci = FALSE)
abc <- read.csv(here(out_dir,paste0(comparison_name,"_key_quants.csv"))) %>% select(!c(Region,Terminal_SSB0,B_over_DynB_Ref)) %>% 
  mutate(B_Status = Terminal_SSB/(B_Ref_Pt/0.4)) %>% mutate(across(-Model, round,2),
                                                            across(-Model, ~ format(., big.mark = ",", scientific = F, trim=TRUE))) %>%
  relocate(Catch_Advice, .before = Terminal_SSB) %>% relocate(Terminal_F, .after = B_Ref_Pt) %>% relocate(B_over_B_Ref, .before = Terminal_F) %>%
  relocate(B_Status, .before = Terminal_SSB)


ssb_plot_temp <- ssb$by_type$ssb +theme(legend.text = element_text(size = 20),legend.title = element_text(size = 20))
ssb_plot_temp$layers[[1]]$aes_params$linewidth <- 2 
recr_plot_temp <- ssb$by_type$rec +theme(legend.text = element_text(size = 20),legend.title = element_text(size = 20))
recr_plot_temp$layers[[1]]$aes_params$linewidth <- 2 
bio_plot_temp <- ssb$by_type$total_biom +theme(legend.text = element_text(size = 20),legend.title = element_text(size = 20))
bio_plot_temp$layers[[1]]$aes_params$linewidth <- 2
abc_table <-  tableGrob(abc, rows = NULL)

ssb_plot <- ssb_plot_temp / wrap_elements(abc_table)+ 
  plot_layout(heights = c(3, 1)) 
bio_plot <- bio_plot_temp / wrap_elements(abc_table)+ 
  plot_layout(heights = c(3, 1))
recr_plot <- recr_plot_temp / wrap_elements(abc_table)+ 
  plot_layout(heights = c(3, 1))

ssb_recr_plot <- wrap_plots(recr_plot_temp , ssb_plot_temp , abc_table, ncol =1)+ 
  plot_layout(heights = c(2,2, 1)) +plot_layout(guides = "collect")& theme(legend.position = "top")

ggsave(here(out_dir, paste0(comparison_name,"_ssb_abc_plot.png")), width=16, height=12, plot=ssb_plot)
ggsave(here(out_dir, paste0(comparison_name,"_bio_abc_plot.png")), width=16, height=12, plot=bio_plot)
ggsave(here(out_dir, paste0(comparison_name,"_recr_abc_plot.png")), width=16, height=12, plot=recr_plot)
ggsave(here(out_dir, paste0(comparison_name,"_ssb_recr_abc_plot.png")), width=16, height=14, plot=ssb_recr_plot)


