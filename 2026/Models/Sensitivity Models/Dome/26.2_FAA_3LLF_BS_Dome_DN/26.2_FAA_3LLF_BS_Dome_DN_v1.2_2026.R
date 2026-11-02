##########################################################################################################################################################

# Purpose: Model 26.2_FAA_3LLF_BS_Dome_DN (fleets as areas, 3 LLS survey fleets, 3 LLF fishery fleets + trawl; double normal (dome) selectivity for the BS LLF, logistic LLS) set up for SPoRC v1.2.0.9000.
# Creator: Matthew LH. Cheng (UAF CFOS)

##########################################################################################################################################################
# pak::pak("chengmatt/SPoRC@dev-popn-seasons")                                 # install dev version (v1.2)
# devtools::install_github("afsc-assessments/rema", dependencies = TRUE, build_vignettes = TRUE)
#remotes::install_github("noaa-afsc/SparseNUTS")

library(here)
library(SPoRC)
library(RTMB)
library(tidyverse)
library(reshape2)
library("ggsci")
library(gridExtra)
library(purrr)
library(corrplot)
library(rema)
library(fs)
library(magick)
library(pdftools)
library(tools)
library(tictoc)

##########################################################################################################################################################
tic()
##########################################################################################################################################################
#---------USER INPUTS--NEED TO UPDATE

mod_name <- '26.2_FAA_3LLF_BS_Dome_DN'                                         # Model name to use for saving files and labels
dat_name <- "SPoRC_FAA_3FG_1TF_v1.2"                                           # Name of data file
root_folder <- here("Mods", "___2026_Final_Mods_Nov")                          # The root folder where runs will be stored (sub folders named with mod_name)
mod_name_last <- '26.2_FAA_3LLF_2025_v1.2'                                     # Name of last operational model for model data addition/bridge analysis
mod_name_last_proj <- '23.5_2024'                                              # Name of last operational model for projections
mod_name_last_proj2 <- '23.5_2024'                                             # Name of 2nd to last operational model for projections (to get OFL in term_yr -1); NOTE: for 2026 since did not have a 2025 assessment, this is same as last operational assessment (if yearly assessments this would be the name of the model from 2 assessments back)
do_data_plots <- 0                                                             # whether to plot SAFE data plots (only do if have access to data folder and full data outputs)
do_bias_ramp <- 0                                                              # whether to update the recruitment bias ramp parameters; https://cdnsciencepub.com/doi/full/10.1139/f2011-092; NOTE: this is done after FRANCIS reweighting
do_francis <- 1                                                                # whether or not to do francis reweighting for this run, ==0 NO, ==1 YES
plot_francis <- 1                                                              # whether or not to do the Francis mean bin plot
do_sdrep <- 1                                                                  # whether to get the standard error report
do_comps <- 1                                                                  # whether to run the full suite of figs and tables for comps
do_pearson <- 1                                                                # whether to calculate and print out Pearson residual bubble plots ; Yes==1, No==0
do_spatial_comp <- 0                                                           # whether to graph ssb comparison with spatial model (only do if have a spatial model run and in appropriate 26.3_Spat_5Reg folder)
do_mod_bridge <- 1                                                             # whether to a graphical comparison to the last model 
do_hist_mod_retro <- 0                                                         # whether to do a full historical model retro (reads dat files from last ~10 assessments)
do_diags <- 1                                                                  # whether or not to do extra diagnostics (retrospective, jitter, profile likelihoods, etc) for this run; ==0 NO, ==1, YES
do_profile <- 0                                                                # whether to do the likelihood profiles (M, R0); only used when do_diags == 1
do_bridge_loo <- 0                                                             # whether to do the data bridge and leave-one-out runs; only used when do_diags == 1
do_mcmc <- 1                                                                   # whether to do MCMC
do_proj <- 1                                                                   # whether to run the full projection model
plot_all_proj_sims <- 0                                                        # whether to do projection plots of all sim iterations for SSB, Recr, etc...takes a long time to write since 1000 sims
do_alt_proj <- 0                                                               # whether to do alternate recruitment and harvest cap projections
do_ABC_whale <- 1                                                              # whether to adjust ABC and OFL for whale depredation (NOTE: starting in 2026 these adjustments are constant -- not scaled to % change in ABC or OFL -- based on sum(catch_whale_dep) as used to inflate LLF catch each year)
do_apport <- 0                                                                 # whether to run the full apportionment model
do_SIS <- 0                                                                    # whether to do the SIS calculation of last year's F to achieve last year's OFL
do_SARA <- 0                                                                   # whether to create the OST/SIS file for reporting final assessment outputs
save_all <- 1                                                                  # whether to save an Rdata file with ALL the data frames and objects in the environment (large file)

##########################################################################################################################################################
#--- INPUTS For Historic Retro Figure
#---# NOTE: All report/results files for models to be included (aside from current model) MUST BE INCLUDED in the R folder (in hist_model_retro_data folder)

hist_retro_st_yr <- 2018                                                       # enter model year for first stock assessment model to be included
hist_mods = c(16.5,16.5,16.5,21.12,21.12,23.5,23.5)                            # Vector of model names for each year of the retro
sporc_names = c("25.12_2025")                                                  # SPoRC model numbers to be used

##########################################################################################################################################################

##########################################################################################################################################################
#--- Inputs for Model Bridge
#---# NOTE: All 'report/results'*mod_name_Model_Results.rds and *mod_name_brps.csv files for models to be included (aside from current model) MUST BE INCLUDED in the R folder (in mod_bridge_data folder) 

mod_bridge_names <- c("26.2_FAA_2025_v1.2","26.2_FAA_2026_v1.2","26.1_1Reg_2026_v1.2")

##########################################################################################################################################################

##########################################################################################################################################################
#--- Inputs for Projections and Apportionment
#---# NOTE: prev proj csvs need to be in the "R" -> "proj_data" folder including last assessment exec_summary and associated summary table .csvs

nsims <- 1e3                                                                   # number of iterations for projection sims
nproj_yrs <- 14                                                                # number of years to do for projections
rec_age <- 2                                                                   # age of recruitment for projections
calc_rec_st_yr <- 20                                                           # first year for mean recruitment (usually corresponds to 1979 recruitment for AK)
spr_targ <- 0.4                                                                # target SPR (corresponding to F40/B40)
exec_summ_mod <-  "23.5_2024"                                                  # name of exec summary from last year, located in R folder -> proj_data
TAC <- data.frame(Region = c("BS","AI","WG","CG","WY","SEO"), 
                  yr_1 = c(8496,7940,4746,9744,2686,5660),
                  yr_2 = c(8996,7440,4687,9622,2652,5589))                     # Last 2 year's TAC values for BS/AI/WG/CG/WY/SEO; NOTE: can now pull directly from AKFIN, so need to code that into data pulls 
reccomend_apport <- "Carryover_No_wAI"                                         # The chosen apportionment method for making final summary tables (must match names used in apportionment.R script)
bio_age_filter <- 4                                                            # age to start summary biomass summation for the Exec Summary biomass table (Groundfish Plan Team Summary Table by Region; traditionally age-4+ for sablefish)

##########################################################################################################################################################
#--- Load in assessment data

dat <- readRDS(here(root_folder, mod_name, paste0(dat_name,".rds")))           # load the SPoRC data file

##########################################################################################################################################################
#--- Plotting labels (passed to most figures)

comp_regions <- data.frame(Region = 1, Region_Name = "AK")                     # region name (needs updating if do spatial model)
sex_names <- data.frame(Sex = 1:2, Sex_Name = c("Female", "Male"))             # sex names 
fish_names <- c("BS LLF", "AI LLF", "GOA LLF", "TF")                           # fishery fleet names, in fleet order (needs updating if use 3 LLF FAA models)
srv_names <- c("BS LLS", "AI LLS", "GOA LLS", "TS", "JPN LLS")                 # survey fleet names, in fleet order (needs updating if use 1 LLS single reg models)
mod_yr <- max(dat$yrs)                                                         # pull terminal year for naming files
model_years <- dat$yrs                                                          # model years (calendar)
term_yr <- max(model_years)

##########################################################################################################################################################
#--- Make various sub-directories

dir.create(here(root_folder,mod_name,"plots"))
plot_dir <- here(root_folder,mod_name,"plots")
dir.create(here(root_folder,mod_name,"tables"))
tab_dir <- here(root_folder,mod_name,"tables")
dir.create(here(root_folder,mod_name,"R_out"))
out_dir <- here(root_folder,mod_name,"R_out")
dir.create(here(root_folder,mod_name,"Projections"))
proj_dir <- here(root_folder,mod_name,"Projections")
dir.create(here(root_folder,mod_name,"Apportionment"))
apport_dir <- here(root_folder,mod_name,"Apportionment")

##########################################################################################################################################################
#--- Load various helped functions from "R" directory

# Load diagnostic and plotting scripts (not all functions, so may want to skip a few...list explicitly below the ones that are being used currently)
#list.files(path = here("R"), pattern = "\\.[Rr]$", full.names = TRUE) |> walk(source)

source(here("R", "Diagnostic_Helpers.R"))                                      # this is the main R script with most helper fxns, if you can't find a fxn it is probably somewhere in this script
source(here("R", "catch_plot.R"))
source(here("R", "survey_plot.R"))
source(here("R", "growth_plot.R"))
#source(here("R", "SAFE_tables.R"))
source(here("R", "mt_rec_bias_correction.R"))
#source(here("R", "plot_francis.R"))      # NOTE not a function yet
#source(here("R", "comp_figs.R"))         # NOTE not a function yet
source(here("R", "hist_model_retro.R"))
source(here("R", "model_bridge.R"))
source(here("R", "projections.R"))
source(here("R", "apportionment.R"))
#source(here("R", "OST_report.R"))
source(here("R", "SIS_F_OFL_last.R"))

########################################################################################################################
#--- Few quick data plots and tables for SAFE

if(do_data_plots == 1){
  plot_growth(fig_dir = plot_dir, data_file = dat)
  plot_catch(fig_dir = plot_dir, data_file = dat)
  plot_survey(fig_dir = plot_dir, data_file = dat, root = root_folder)
  make_tables(tab_dir = tab_dir, data_file = dat, root = root_folder, mod_name_last_proj = mod_name_last_proj, term_yr = term_yr)
}

########################################################################################################################
# build the SPoRC model

build_input_list_26.2_FAA_3LLF_BS_Dome_DN <- function(dat,
                                   Wt_FishAgeComps = NULL,
                                   Wt_FishLenComps = NULL,
                                   Wt_SrvAgeComps = NULL,
                                   Wt_SrvLenComps = NULL) {

  n_pop <- dat$n_pop
  n_seas <- dat$n_seas
  n_reg <- dat$nreg
  n_yrs <- dat$nyrs
  n_sex <- dat$nsex
  n_flt <- dat$nflt
  n_srv <- dat$nsrv_flt

  # Composition data weights ------------------------------------------------
  # starting weights; pass the Francis reweighted arrays to reproduce a finished model
  if(is.null(Wt_FishAgeComps)) Wt_FishAgeComps <- array(1, dim = c(n_reg, n_yrs, n_seas, n_sex, n_flt))
  if(is.null(Wt_FishLenComps)) Wt_FishLenComps <- array(1, dim = c(n_reg, n_yrs, n_seas, n_sex, n_flt))
  if(is.null(Wt_SrvAgeComps)) Wt_SrvAgeComps <- array(1, dim = c(n_reg, n_yrs, n_seas, n_sex, n_srv))
  if(is.null(Wt_SrvLenComps)) Wt_SrvLenComps <- array(1, dim = c(n_reg, n_yrs, n_seas, n_sex, n_srv))

  # Dimensions --------------------------------------------------------------
  input_list <- Setup_Mod_Dim(years = 1:n_yrs,                                 # vector of years
                              ages = 1:dat$nages,                              # vector of ages
                              lens = dat$length_bins,                          # length bins
                              n_pop = n_pop,                                   # number of populations, 1 == single panmictic population
                              n_seas = n_seas,                                 # number of seasons, 1 == annual time step
                              n_regions = n_reg,                               # number of regions, 1 == AK-wide (if 5 then 1 == BS, 2 == AI, 3 == WG, 4 == CG, 5 == EG)
                              n_sexes = n_sex,                                 # number of sexes, 1 == female, 2 == male
                              n_fish_fleets = n_flt,                           # 1-3 == BS/AI/GOA FG, 4 == TF (agg)
                              n_srv_fleets = n_srv,                            # 1-3 == BS/AI/GOA LLS, 4 == TS, 5 == JPN LLS
                              verbose = FALSE                                  # print setup messages
  )

  # Recruitment -------------------------------------------------------------
  input_list <- Setup_Mod_Rec(input_list = input_list,
                              do_rec_bias_ramp = 1,                            # do bias ramp (0 == don't do bias ramp, 1 == do bias ramp); # recruit bias adjustment following Methot and Taylor 2011; sets up ascending and descending limb, etc.; https://cdnsciencepub.com/doi/full/10.1139/f2011-092
                              bias_year = c(length(dat$styr:1980),     # breakpoints for bias ramp--NOTE these are flexibly coded and don't need to be changed as add more years to model (first entry == years no bias ramp - 1960 to 1980--years 1 to 21;
                                            length(dat$styr:1990),     # second entry == years ascending limb of bias ramp - 1981 to 1990--years 21 to 30;
                                            (length(dat$styr:          # third entry == years full bias correction - 1990 to 5 years from terminal year (currently 2023)--years 31 to 60 currently--start descending limb in this year
                                                      dat$endyr)-5),
                                            (length(dat$styr:          # fourth entry == years no bias correction at end of model - terminal year of recruitment estimate (2023)-- year 64 currently--(and again in terminal year of model), which is terminal year of model-1 since terminal year is fixed at average recruitment)
                                                      dat$endyr) - 1)),        # end bias adjustment recent years (defaults to endyr-1)
                              max_bias_ramp_fct = 0.9,                         # maximum bias correction applied in the full-correction years (1 == full Methot & Taylor correction)
                              sigmaR_switch = as.integer(length(dat$styr:1975)),  # sablefish includes 2 sigR values for historical period (before 1975) and recent period; lower sigR for historical period keeps initial age structure from having extreme devs
                              dont_est_recdev_last = 1,                        # set terminal year rec_dev to mean recruit, since little information at age-2 to estimate recruitment in terminal year
                              ln_sigmaR = array(log(c(0.4, 0.9)), dim = c(2, n_pop, n_reg)),  # early and late sigma_R values (index 1 == initial period, 2 == annual period)
                              rec_model = "mean_rec",                          # no SRR, just use mean recr with devs
                              sigmaR_spec = "fix",                             # fix sigmaR at the early and late values above (not estimated)
                              InitDevs_spec = NULL,                            # estimate all initial age structure deviations
                              RecDevs_spec = NULL,                             # estimate all recruitment deviations
                              init_age_strc = 1,                               # estimate initial age structure as exp mort from R0 and M, then estimate recr devs for each cohort in initial age structure; # Initial age structure is derived by assuming a geometric series (init_age_strc = 1; the alternative is iterating age structure to some equilibrium init_age_strc = 0),
                              # initialization F comes from fleet 1 only
                              init_F_prop = array(c(0.1, rep(0, n_flt - 1)), dim = c(n_reg, n_seas, n_flt))  # assume LLF fishing mortality is 0.1*aveF to account for longline fishing before model start date (1960)
  )

  # Biologicals -------------------------------------------------------------
  M_prior <- data.frame(                                                       # set up the prior on M
    popblk = 1,                                                                # M prior block structure: population block
    regionblk = 1,                                                             # region block
    yearblk = 1,                                                               # year block (single block, M constant over time)
    ageblk = 1,                                                                # age block (single block, M constant across ages)
    sexblk = 1:2,                                                              # sex blocks (sex-specific M; needs to match M_sexblk_spec below)
    mu = 0.085,                                                                # matches an average longevity of ~65 years, corresponds witht agging data estimate of M, and similar to M estimates in BC and NWFSC sablefish models
    sd = 0.1                                                                   # prior sd for M
  )

  input_list <- Setup_Mod_Biologicals(input_list = input_list,
                                      WAA = dat$WAA,                           # weight-at-age
                                      MatAA = dat$MatAA,                       # maturity at age
                                      AgeingError = as.matrix(dat$AgeError),   # ageing error matrix
                                      SizeAgeTrans = dat$SizeAgeTrans,         # size age transition matrix
                                      fit_lengths = 1,                         # fitting length compositions
                                      Use_M_prior = 1,                         # use natural mortality prior
                                      M_prior = M_prior,                       # mean and sd for M prior (defined above)
                                      M_spec = "est_ln_M",                     # estimate M in log space
                                      M_sexblk_spec = list(1, 2)               # estimate sex specific M (one par per sex)
  )

  # Movement / Tagging (none) ------------------------------------------------
  input_list <- Setup_Mod_Movement(input_list = input_list,
                                   use_fixed_movement = 1,                     # don't est move
                                   Fixed_Movement = NA,                        # use identity move
                                   do_recruits_move = 0                        # recruits don't move
  )
  input_list <- Setup_Mod_Tagging(input_list = input_list,
                                  use_conv_fish_tagging = rep(0, n_flt)        # don't fit/use tagging data
  )

  # Catch and fishing mortality ---------------------------------------------
  # A zero in ObsCatch marks a closure; v1.2 estimates F where ObsCatch is NA.
  # Zeroing the unfit NA cells keeps F at 0 there.
  ObsCatch <- dat$ObsCatch
  ObsCatch[dat$UseCatch == 0 & is.na(ObsCatch)] <- 0

  input_list <- Setup_Mod_Catch_and_F(input_list = input_list,
                                      ObsCatch = ObsCatch,                     # Observed Catch in mt (unfit NA cells zeroed above)
                                      UseCatch = dat$UseCatch,                 # Years in which catch data is available and should be fit in the model
                                      Use_F_pen = 1,                           # use normalization penalty on F devs
                                      ln_sigmaF = array(log(0.75), dim = c(n_reg, n_seas, n_flt)),  # the sd for the F prior (high value gives low 'weight' to the F prior; 1.0 is the default value)
                                      sigmaC_spec = 'fix',                     # Whether the variance term for fitting catch data is estimated or fixed
                                      ln_sigmaC = array(log(0.05), dim = c(n_reg, n_yrs, n_seas, n_flt))  # 0.05 is common value to fit catch relatively closely, but allow for some error
  )

  # Fishery indices and compositions ----------------------------------------
  input_list <- Setup_Mod_FishIdx_and_Comps(input_list = input_list,
                                            ObsFishIdx = dat$ObsFishIdx,       # Observed fishery indices (CPUE)
                                            ObsFishIdx_SE = dat$ObsFishIdx_SE * 2,  # not fitting CPUE, but if do the *2 is to ensure realistic CV that is ~2xs that of the LLS
                                            UseFishIdx = dat$UseFishIdx,       # Switch to identify years in which fishery indices exist and whether to fit them in the model
                                            ObsFishAgeComps = dat$ObsFishAgeComps,  # Observed fishery age comps
                                            UseFishAgeComps = dat$UseFishAgeComps,  # Switch identifying whether fishery age comps exist and whether to fit them
                                            ISS_FishAgeComps = dat$ISS_FishAgeComps,  # The input sample sizes for fishery age comps
                                            ObsFishLenComps = dat$ObsFishLenComps,  # Observed fishery length comps
                                            UseFishLenComps = dat$UseFishLenComps,  # Whether to fit fishery length comps in a given year
                                            ISS_FishLenComps = dat$ISS_FishLenComps,  # Input sample size for fishery length comps
                                            fish_idx_type = c("biom", "biom", "biom", "none"),  # Define units (biomass vs. abundance/numbers) for fishery indices (none is used if no index for a given fleet)
                                            FishAgeComps_LikeType = c("Multinomial", "Multinomial", "Multinomial", "none"),  # age comp likelihoods by fleet (none if not fit or not available for given fleet)
                                            FishLenComps_LikeType = c("Multinomial", "Multinomial", "Multinomial", "Multinomial"),  # length comp likelihoods by fleet
                                            FishAgeComps_Type = c("spltRjntS_Year_1-terminal_Fleet_1",  # fit all comps using Joint by sex approach (comps sum to 1 across both sexes in a given year)
                                                                  "spltRjntS_Year_1-terminal_Fleet_2",
                                                                  "spltRjntS_Year_1-terminal_Fleet_3",
                                                                  "none_Year_1-terminal_Fleet_4"),  # NOTE: entries are needed for all years and all fleets; to agg some years then disagg the rest, use two entries for that fleet
                                            FishLenComps_Type = c("spltRjntS_Year_1-terminal_Fleet_1",  # not actually fitting any fishery length comps, only use age comps
                                                                  "spltRjntS_Year_1-terminal_Fleet_2",
                                                                  "spltRjntS_Year_1-terminal_Fleet_3",
                                                                  "spltRjntS_Year_1-terminal_Fleet_4"),
                                            FishAge_comp_agg_type = c(1, 1, 1, NA),  # for age comps, identifies the sequence of the normalizing, aggregation, and ageing error process (matches ADMB sablefish)
                                            FishLen_comp_agg_type = c(0, 0, 0, 0)  # for length comps, identifies the sequence of the normalizing, aggregation, and ageing error process (matches ADMB sablefish)
  )

  # Survey indices and compositions -----------------------------------------
  input_list <- Setup_Mod_SrvIdx_and_Comps(input_list = input_list,
                                           ObsSrvIdx = dat$ObsSrvIdx,          # Observed fishery independent survey indices
                                           ObsSrvIdx_SE = dat$ObsSrvIdx_SE * 1.5,  # to ensure more realistic CV multiply obs CV by 1.5, this helps ensure not overfitting LLS (essentially acts a lambda); gives the GOA LLS effective CV of ~0.075
                                           UseSrvIdx = dat$UseSrvIdx,          # Whether to fit fishery independent surveys in each year
                                           ObsSrvAgeComps = dat$ObsSrvAgeComps,  # Observed age comps for fishery independent surveys
                                           ISS_SrvAgeComps = dat$ISS_SrvAgeComps,  # Input sample size for fishery independent survey age comps
                                           UseSrvAgeComps = dat$UseSrvAgeComps,  # Whether to fit fishery independent survey age comps in a given year
                                           ObsSrvLenComps = dat$ObsSrvLenComps,  # Observed fishery independent survey length comps
                                           UseSrvLenComps = dat$UseSrvLenComps,  # Whether to fit fishery independent survey length comps in a given year
                                           ISS_SrvLenComps = dat$ISS_SrvLenComps,  # Input sample size for fishery independent survey length comps
                                           srv_idx_type = c("abd", "abd", "abd", "biom", "abd"),  # Define units for survey indices; NOAA LLS uses RPNs (abd), Trawl Survey uses biomass, JPN LLS uses RPNs (abd)
                                           SrvAgeComps_LikeType = c("Multinomial", "Multinomial", "Multinomial", "none", "Multinomial"),  # age comp likelihoods by survey fleet (none if not fit or not available for given fleet)
                                           SrvLenComps_LikeType = c("Multinomial", "Multinomial", "Multinomial", "Multinomial", "Multinomial"),  # length comp likelihoods by survey fleet
                                           SrvAgeComps_Type = c("spltRjntS_Year_1-terminal_Fleet_1",  # age comp structure by survey fleet (spltRjntS == split by region, joint across sexes)
                                                                "spltRjntS_Year_1-terminal_Fleet_2",
                                                                "spltRjntS_Year_1-terminal_Fleet_3",
                                                                "none_Year_1-terminal_Fleet_4",
                                                                "spltRjntS_Year_1-terminal_Fleet_5"),
                                           SrvLenComps_Type = c("spltRjntS_Year_1-terminal_Fleet_1",  # only actually fitting length comps for LLS
                                                                "spltRjntS_Year_1-terminal_Fleet_2",
                                                                "spltRjntS_Year_1-terminal_Fleet_3",
                                                                "spltRjntS_Year_1-terminal_Fleet_4",
                                                                "spltRjntS_Year_1-terminal_Fleet_5"),
                                           SrvAge_comp_agg_type = c(1, 1, 1, NA, 1),  # for age comps, identifies the sequence of the normalizing, aggregation, and ageing error process (matches ADMB sablefish)
                                           SrvLen_comp_agg_type = c(0, 0, 0, 0, 0)  # for length comps, identifies the sequence of the normalizing, aggregation, and ageing error process (matches ADMB sablefish)
  )

  # Data weighting ----------------------------------------------------------
  input_list <- Setup_Mod_Weighting(input_list = input_list,
                                    Wt_Catch = 1,                              # Weight (lambda) for catch data
                                    Wt_FishIdx = 1,                            # Weight (lambda) for fishery indices data
                                    Wt_SrvIdx = 1,                             # Weight (lambda) for survey indices data
                                    Wt_Rec = 1,                                # Weight (lambda) for stock-recruit penalty term
                                    Wt_F = 1,                                  # Weight (lambda) for fishing mortality penalty term
                                    Wt_Tagging = 0,                            # not fitting tagging data
                                    Wt_FishAgeComps = Wt_FishAgeComps,         # Weights (lambda) for fishery age comp data (updated by Francis reweighting)
                                    Wt_FishLenComps = Wt_FishLenComps,         # Weights (lambda) for fishery length comp data (updated by Francis reweighting)
                                    Wt_SrvAgeComps = Wt_SrvAgeComps,           # Weights (lambda) for survey age comp data (updated by Francis reweighting)
                                    Wt_SrvLenComps = Wt_SrvLenComps            # Weights (lambda) for survey length comp data (updated by Francis reweighting)
  )

  # Fishery selectivity -----------------------------------------------------
  # Setup priors for selectivity
  # Define valid fleet-block combinations
  fleet_blocks <- data.frame(
    fleet = c(1, 2, 3, 4),                                                     # fleets corresponding to time blocks (no time blocks for any fishery fleet)
    block = c(1, 1, 1, 1)                                                      # corresponding time blocks
  )
  # Define sex and parameter combinations
  sex_par <- expand.grid(sex = 1:2, par = 1:2)                                 # par1 == a50, par2 == delta (slope)
  # Merge to get all valid combinations
  fish_selex_structure <- merge(fleet_blocks, sex_par)

  # Lognormal priors on a50 and slope for the logistic fleets. Here, the BS fixed gear 
  # fleet is dbnrml, so parameter 1 represents the peak age on the age scale (raw), parameter 2, 5, and 6
  # are fixed and not used. Only the additional parameters 3 and 4 are used and they repersent the ascending limb and the 
  # descending limb. Parameter 1 not penalized since lognormal prior 
  fish_selex_prior <- cbind(
    region = 1,
    fish_selex_structure %>% dplyr::filter(fleet != 1),
    mu = 4,                                                                    # All selex prior means (means should be defined in normal space)
    sd = 1.5                                                                   # All selex prior sd
  ) %>%
    dplyr::bind_rows(cbind(region = 1,
                           merge(data.frame(fleet = 1, block = 1), expand.grid(sex = 1:2, par = 3:4)),
                           mu = 10,                                            # prior mean for the double normal width pars (BS fixed gear)
                           sd = 3))                                            # weak prior sd on the width pars

  input_list <- Setup_Mod_Fishsel_and_Q(input_list = input_list,
                                        cont_tv_fish_sel = c("none_Fleet_1", "none_Fleet_2", "none_Fleet_3", "none_Fleet_4"),  # whether continuous time-varying selectivity is used for any fishery fleet
                                        fish_sel_blocks = c("none_Fleet_1", "none_Fleet_2", "none_Fleet_3", "none_Fleet_4"),  # fishery selectivity time blocks (none for any fleet)
                                        fish_sel_model = c("dbnrml_Fleet_1",   # fishery selectivity form; double normal (dome-shaped) selectivity for BS fixed gear
                                                           "logist1_Fleet_2",  # logistic selectivity for AI fixed gear
                                                           "logist1_Fleet_3",  # logistic selectivity for GOA fixed gear
                                                           "logist1_Fleet_4"),  # logistic selectivity for trawl gear fleet
                                        fish_q_blocks = c("none_Fleet_1", "none_Fleet_2", "none_Fleet_3", "none_Fleet_4"),  # fishery indices catchability time blocks (none for any fleet)
                                        fish_fixed_sel_pars_spec = c("est_all", "est_all", "est_all", "est_all"),  # Estimate all fixed effects for fishery selectivity; fix and share parameters later via the parameter mapping
                                        fish_q_spec = c("fix", "fix", "fix", "fix"),  # DO NOT estimate fishery q for any fleet (no CPUE index fit)
                                        # make it so that both ascending and descending limbs are raw Gaussians, so the
                                        # selex-at-first and last-age anchors (parameters 5, 6) drop out
                                        fish_sel_dbnrml_raw = rbind(c(1, 1), c(0, 0), c(0, 0), c(0, 0)),  # double normal only (BS fixed gear): 1 == leave the ascending/descending limbs as raw Gaussians instead of anchoring them to the first/last bin
                                        Use_fish_selex_prior = 1,              # Using selex priors
                                        fish_selex_prior = fish_selex_prior    # lognormal selex prior dataframe defined above
  )

  # Survey selectivity ------------------------------------------------------
  # Setup priors for selectivity
  # Define valid fleet-block combinations
  fleet_blocks <- data.frame(
    fleet = c(1, 1, 2, 2, 3, 3, 4, 5),                                         # fleets for each time block (BS, AI, GOA LLS have 2 time blocks each; TS and JPN LLS have 1 each)
    block = c(1, 2, 1, 2, 1, 2, 1, 1)                                          # corresponding time blocks
  )
  # Define sex and parameter combinations
  sex_par <- expand.grid(sex = 1:2, par = 1:2)                                 # par1 == a50, par2 == delta (slope)
  # Merge to get all valid combinations
  srv_selex_structure <- merge(fleet_blocks, sex_par) %>%
    dplyr::filter(!(fleet == 4))                                               # remove priors for the trawl survey (selex pars fixed, not estimated); NEEDS TO MATCH PARAMETER MAPPING

  # as for the fishery: logistic fleets keep the a50/slope prior, BS LLS gets weak width priors
  # on parameters 3 and 4 in both blocks
  srv_selex_prior <- cbind(
    region = 1,
    srv_selex_structure,
    mu = 3,                                                                    # All selex prior means (means should be defined in normal space)
    sd = 1.5                                                                   # All selex prior sd
  )

  input_list <- Setup_Mod_Srvsel_and_Q(input_list = input_list,
                                       cont_tv_srv_sel = c("none_Fleet_1", "none_Fleet_2", "none_Fleet_3",  # whether continuous time-varying selectivity is used for any survey fleet
                                                           "none_Fleet_4", "none_Fleet_5"),
                                       srv_sel_blocks = c("Block_1_Year_1-56_Fleet_1",  # survey selectivity time blocks if not TV specified above; early time block BS LLS-- 1960 to 2015
                                                          "Block_2_Year_57-terminal_Fleet_1",  # Recent time block for BS LLS--2016 to terminal year
                                                          "Block_1_Year_1-56_Fleet_2",  # Early time block AI LLS-- 1960 to 2015
                                                          "Block_2_Year_57-terminal_Fleet_2",  # Recent time block for AI LLS--2016 to terminal year
                                                          "Block_1_Year_1-56_Fleet_3",  # Early time block GOA LLS-- 1960 to 2015
                                                          "Block_2_Year_57-terminal_Fleet_3",  # Recent time block for GOA LLS--2016 to terminal year
                                                          "none_Fleet_4",      # No blocks for trawl survey
                                                          "none_Fleet_5"),     # No blocks for JPN LLS
                                       srv_sel_model = c("logist1_Fleet_1",     # Survey selectivity form; logistic selectivity for BS LLS
                                                         "logist1_Fleet_2",    # logistic selectivity for AI LLS
                                                         "logist1_Fleet_3",    # logistic selectivity for GOA LLS
                                                         "exponential_Fleet_4",  # exponential declining selectivity for trawl survey (not estimated)
                                                         "logist1_Fleet_5"),   # logistic selectivity for JPN LLS
                                       srv_q_blocks = c("none_Fleet_1", "none_Fleet_2", "none_Fleet_3",  # survey indices catchability time blocks (no q time blocks for any fleets)
                                                        "none_Fleet_4", "none_Fleet_5"),
                                       srv_fixed_sel_pars_spec = c("est_all", "est_all", "est_all", "fix", "est_all"),  # Estimate all fixed effects for survey selectivity except the trawl survey (fixed); fix and share parameters later via the parameter mapping
                                       srv_q_spec = c("est_all", "est_all", "est_all", "fix", "est_all"),  # Estimate q for all survey fleets except the trawl survey (fixed)
                                       t_srv = array(0.5, dim = c(n_reg, n_seas, n_srv)),  # survey timing as a fraction of the year (mid-year); v1.2 defaults this to 1 (end of year) so it must be set
                                       Use_srv_selex_prior = 1,                # Using selex priors
                                       srv_selex_prior = srv_selex_prior       # lognormal selex prior dataframe defined above
  )

  # Selectivity parameter mapping and starting values stuff ------------------------
  # NOTE: the parameter mapping is a bit complex, but more digestible when you look at how the parameter array is filled.
  #   The dimensions of the selectivity arrays are n_regions, max_sel_pars (the max number of parameters in any selectivity
  #   function being used, 2 for logistic), max_sel_blks (the max number of time blocks in any fleet), n_sexes, n_fleets.
  #   When entering factors, use a unique number for each parameter being estimated, and repeat a number to share the
  #   current par with an earlier par (i.e., if 2 is repeated then the current par uses the same value as par 2).
  #   For fleets with fewer time blocks than max_sel_blks, repeat the factors to fill the unused block slots.
  #   For selex functions with fewer pars than max_sel_pars, repeat the par number in the slot that isn't used.
  #   If a parameter is fixed (not estimated), put NA in that slot.
  # Can use this code to see how the parameter array is constructed:
  # array(c(...), dim = dim(input_list$par$fish_fixed_sel_pars))
  # Double normal slots (SPoRC::Get_Selex, Selex_Model 4) for BS fixed gear and BS LLS:
  #   1  peak age, on the age scale          estimated
  #   2  plateau width, logit                fixed at -10: plateau ends one age after the peak
  #   3  ln ascending width                  estimated
  #   4  ln descending width                 estimated
  #   5  logit selex at first age            unused (raw ascending limb)
  #   6  logit selex at last age             unused (raw descending limb)
  dbnrml_est <- c(1, 3, 4)

  fish_map <- array(NA, dim = dim(input_list$par$fish_fixed_sel_pars))
  fish_map[1, dbnrml_est, 1, , 1] <- 1:6                                      # BS fixed gear, 3 pars x 2 sexes
  fish_map[1, 1:2, 1, , 2] <- 7:10                                            # AI fixed gear, 2 pars x 2 sexes
  fish_map[1, 1:2, 1, , 3] <- 11:14                                           # GOA fixed gear
  fish_map[1, 1:2, 1, , 4] <- 15:18                                           # TF
  input_list$map$fish_fixed_sel_pars <- factor(fish_map)

  input_list$map$srv_fixed_sel_pars <-
    factor(c(rep(c(1:4), 1),                                                   # BS LLS (logistic) females, 2pars x 2 blocks
             rep(c(5:8), 1),                                                   # BS LLS (logistic) males
             rep(c(9:12), 1),                                                  # AI LLS females
             rep(c(13:16), 1),                                                 # AI LLS males
             rep(c(17:20), 1),                                                 # GOA LLS females
             rep(c(21:24), 1),                                                 # GOA LLS males
             rep(NA, 4),                                                       # TS, not estimated, 2 pars x 1 blocks (repeated for 2nd block associated with LLS)
             rep(NA, 4),
             rep(c(25:26), 2),                                                 # JPN LLS females, 2pars x 1 blocks (repeated for 2nd block associated with LLS)
             rep(c(27:28), 2)))                                                # JPN LLS males

  # Starting values. Logistic fleets start at a50 = slope = 3 as before. The double normal basically starts
  # as a peak at age 4 with a steep ascent and a gentle descent (a weak dome). The fixed plateau
  # slot has to be set here because it is mapped off and is not on the log scale.
  dbnrml_start <- c(4, -10, log(2), log(400), 0, 0)
  input_list$par$fish_fixed_sel_pars[1, 1:2, , , 2:4] <- log(3)
  input_list$par$fish_fixed_sel_pars[1, , , , 1] <- dbnrml_start
  input_list$par$srv_fixed_sel_pars[1,,,,] <- log(3)

  return(input_list)

} # end build_input_list_26.2_FAA_3LLF_BS_Dome_DN

input_list <- build_input_list_26.2_FAA_3LLF_BS_Dome_DN(dat)
data <- input_list$data
parameters <- input_list$par
mapping <- input_list$map


########################################################################################################################
# Francis Reweigthing ---------------------------------------------------------------
########################################################################################################################

if(do_francis == 1) {
  
    # Francis-reweighted composition weights from the final v1.0.0 fit (3 LLF fleet model). Weights sit in
    # the sex-1 slot, which is where the joint-sex likelihood reads them.
    Wt_FishAgeComps <- array(NA, dim = c(dat$nreg, dat$nyrs, 1, dat$nsex, dat$nflt))
    Wt_FishAgeComps[1,,1,1,1] <- 0.32738389408743
    Wt_FishAgeComps[1,,1,1,2] <- 0.0384641383698822
    Wt_FishAgeComps[1,,1,1,3] <- 0.108429721130408
    Wt_FishLenComps <- array(NA, dim = c(dat$nreg, dat$nyrs, 1, dat$nsex, dat$nflt))
    Wt_FishLenComps[1,,1,1,4] <- 0.148309741477483
    Wt_SrvAgeComps <- array(NA, dim = c(dat$nreg, dat$nyrs, 1, dat$nsex, dat$nsrv_flt))
    Wt_SrvAgeComps[1,,1,1,1] <- 0.557674225591586
    Wt_SrvAgeComps[1,,1,1,2] <- 1.80575078996907
    Wt_SrvAgeComps[1,,1,1,3] <- 0.334941060312132
    Wt_SrvAgeComps[1,,1,1,5] <- 0.174666187909886
    Wt_SrvLenComps <- array(NA, dim = c(dat$nreg, dat$nyrs, 1, dat$nsex, dat$nsrv_flt))
    Wt_SrvLenComps[1,,1,1,5] <- 0.340630200507588
    
    input_list <- build_input_list_26.2_FAA_3LLF_BS_Dome_DN(dat,
                            Wt_FishAgeComps = Wt_FishAgeComps,
                            Wt_FishLenComps = Wt_FishLenComps,
                            Wt_SrvAgeComps = Wt_SrvAgeComps,
                            Wt_SrvLenComps = Wt_SrvLenComps)
    
    # these three must be pulled out right before fitting so the updated mapping is picked up
    data <- input_list$data
    parameters <- input_list$par
    mapping <- input_list$map
    
    sabie_rtmb_model <- fit_model(data,
                                  parameters,
                                  mapping,
                                  random = NULL,
                                  newton_loops = 3,
                                  silent = F
    )
    
    # Run francis (updates the comp weights in data)
      sabie_rtmb_model_francis <- run_francis(
        data,
        parameters,
        mapping,
        NULL,
        10,                     # number of Francis iterations
        3
      )
      
      sabie_rtmb_model <- sabie_rtmb_model_francis$obj
      data <- sabie_rtmb_model$data      # save weights
      
      sabie_rtmb_model <- fit_model(data,
                                    parameters,
                                    mapping,
                                    random = NULL,
                                    newton_loops = 3,
                                    silent = F
      )
      
      saveRDS(sabie_rtmb_model_francis, file = here(out_dir, paste0(mod_name,"_",mod_yr,"_v1.2_francis_model_results.RDS")))
      
  if(plot_francis == 1){
    source(here("R","plot_francis.R"))
  }  # end plot Francis
      
  # Recruitment Bias Correction ---------------------------------------------------------------

      if(do_bias_ramp == 1){    # do this after Francis

              
              # Get standard error report
              sabie_rtmb_model$sd_rep <- RTMB::sdreport(sabie_rtmb_model)
              sigr2 <- c(rep(exp(parameters$ln_sigmaR[1])^2, 15), rep(exp(parameters$ln_sigmaR[2])^2, length(16:(dat$nyrs-1))))
              dont_est_recdev_last <- 1
              
              bias_ramp <- mt_rec_bias_correction(
                data, plot_dir,mod_name, mod_yr,
                sabie_rtmb_model$sd_rep,
                sigr2,
                dont_est_recdev_last = 1
              )
              
              bias_ramp$best_params
              write.csv(bias_ramp$best_params,here(plot_dir,paste0(mod_name,"_recr_bias_corr.csv")))
              
              # Saves the current screen plot to a PNG file
              dev.copy(png, here(plot_dir,paste0(mod_name,"_recr_bias_corr.png")))
              dev.off()  # Mandatory to close the file and finish the save
              
              
              data$bias_year <- c(as.numeric(bias_ramp$best_params[1]),     # breakpoints for bias ramp--NOTE these are flexibly coded and don't need to be changed as add more years to model (first entry == years no bias ramp - 1960 to 1980--years 1 to 21;
                                  as.numeric(bias_ramp$best_params[2]),     # second entry == years ascending limb of bias ramp - 1981 to 1990--years 21 to 30;
                                  as.numeric(bias_ramp$best_params[3]),
                                  as.numeric(bias_ramp$best_params[4]))
              #data$max_bias_ramp_fct <- as.numeric(bias_ramp$best_params[5])   # just leave at 0.9 
      
    } # end_bias_correction

} # end do_francis

########################################################################################################################
# Fit Model ---------------------------------------------------------------
########################################################################################################################

    # (fit_model stores data, parameters, and mapping in the returned object)
    sabie_rtmb_model <- fit_model(data,
                                  parameters,
                                  mapping,
                                  random = NULL,
                                  newton_loops = 3,
                                  silent = TRUE
    )
    rep <- sabie_rtmb_model$rep
    if(do_sdrep == 1){
      sabie_rtmb_model$sd_rep <- RTMB::sdreport(sabie_rtmb_model)
      sd <- sabie_rtmb_model$sd_rep
      sd_df <- as.data.frame(summary(sd))
      sd_df$parameter <- rownames(sd_df)
      sd_df <- sd_df[, c("parameter", "Estimate", "Std. Error")]
    }

    saveRDS(sabie_rtmb_model, here(out_dir, paste0(mod_name,"_",mod_yr,"_v1.2_model_results.RDS")))
    saveRDS(input_list, file = here(out_dir, paste0(mod_name,"_",mod_yr,"_v1.2_input_list.RDS")))
    
    file.copy(from = here(out_dir, paste0(mod_name,"_",mod_yr,"_v1.2_model_results.RDS")), 
              to = here("R","mod_bridge_data", paste0(mod_name,"_",mod_yr,"_v1.2_model_results.RDS")), overwrite = T)
    file.copy(from = here(out_dir, paste0(mod_name,"_",mod_yr,"_v1.2_model_results.RDS")), 
              to = here("R","hist_model_retro_data", paste0(mod_name,"_",mod_yr,"_v1.2_model_results.RDS")), overwrite = T)
    
    # Check convergence, will let you know if high correlation, large gradient, or high SE for any pars
    messages <- capture.output({
      post_optim_sanity_checks(sd,rep,gradient_tol = 0.00001,se_tol = 2,corr_tol = 0.95)
    }, type = "message")
    
    writeLines(messages,  here(tab_dir, paste0(mod_name,"_",mod_yr,"_v1.2_convergence.txt")))
    write.csv(sd_df,here(tab_dir, paste0(mod_name,"_",mod_yr,"_v1.2_sd_rep.csv")))
    
########################################################################################################################
# Figures ---------------------------------------------------------------
########################################################################################################################

    styr <- dat$styr
    
    # Main results: time series, catch fits, index fits, selectivity (ts, ts_by_type, catch_fits, idx_fits, selex pngs)
    plot_main_results(model = sabie_rtmb_model, comp_regions = comp_regions, sex_names = sex_names, fish_names = fish_names, 
                      srv_names = srv_names, fig_dir = plot_dir, dir_tab = tab_dir, styr = styr, model_name = mod_name, 
                      mod_yr = mod_yr)
    
    all_plots <- plot_all_basic(list(data), list(rep), list(sd), mod_name, out_path = here(plot_dir))
    
    plot_phase_plane(sabie_rtmb_model, styr, title = NULL, rec_age = rec_age, calc_rec_st_yr = calc_rec_st_yr, alpha = 0.05, dir_tab = tab_dir, 
                     fig_dir = plot_dir,model_name = mod_name, mod_yr = mod_yr)
    
    #--- plot phase plane using the B40 corresponding to an Ave Recruitment from 1980 - 2019 (this roughly corresponds to the 2019 assessment -- lowest SSB in time series) 
    #    Gives a comparison of what phase plane is if we used a constant mean recr assumption corresponding to the late 2010s era
    plot_phase_plane(sabie_rtmb_model, styr, title = NULL, rec_age = (dat$nyrs - (calc_rec_st_yr + 39)), calc_rec_st_yr = calc_rec_st_yr, 
                     alpha = 0.05, fig_dir = proj_dir, dir_tab = proj_dir, 
                     model_name = paste(mod_name,"ALT_B40_2010"), mod_yr = mod_yr)
    
    
    file.copy(from = here(tab_dir, paste0(mod_name,"_",mod_yr,"_v1.2_brps.csv")), 
              to = here("R","mod_bridge_data", paste0(mod_name,"_",mod_yr,"_v1.2_brps.csv")), overwrite = T)
    file.copy(from = here(tab_dir, paste0(mod_name,"_",mod_yr,"_v1.2_brps.csv")), 
              to = here("R","hist_model_retro_data", paste0(mod_name,"_",mod_yr,"_v1.2_brps.csv")), overwrite = T)
    
    #-- est pars table
    est_pars <- data.frame(name = names(sd$par.fixed), 
                           pars = sd$par.fixed) %>% 
      group_by(name) %>% 
      count() %>% 
      rename(n_pars = n)
    write.csv(est_pars, here(tab_dir, paste0(mod_name,"_",mod_yr,"_v1.2_par_count_table.csv")))
  

if(do_comps == 1){     # whether to run comp figs
  source(here("R","comp_figs.r"))
}
if(do_spatial_comp == 1){
  source(here("R","agg_ssb_comp.r"))
}
########################################################################################################################
# Quick check on ABC ---------------------------------------------------------------
########################################################################################################################
    sexrat <- dat$sex_ratio
    yield_ratio <- dat$yld_ratio                                               # 3 year average catch/ABC for last 3 full catch years (assess_term_yr-4 : assess_term_yr-1)
    
    reference_points_opt <- list(data = data,
                                 rep = rep,
                                 SPR_x = spr_targ,
                                 type = 'single_region',
                                 what = 'SPR',
                                 calc_rec_st_yr = calc_rec_st_yr,
                                 rec_age = rec_age,
                                 sex_ratio_f = array(dat$sex_ratio, dim = c(
                                   data$n_pop,
                                   data$n_regions)),
                                 t_spawn = data$t_spawn
                                 )
    
    ref_pt <- Get_Reference_Points(data = data,
                                   rep = rep,
                                   SPR_x = spr_targ,
                                   type = 'single_region',
                                   what = 'SPR',
                                   calc_rec_st_yr = calc_rec_st_yr,
                                   rec_age = rec_age,
                                   sex_ratio_f = array(dat$sex_ratio, dim = c(
                                     data$n_pop,
                                     data$n_regions)))

    n_yrs <- length(data$years)                                                 # terminal year of the fitted model
    n_ages = length(data$ages)
    model_years <- dat$styr + data$years - 1
    term_yr <- max(model_years)
    n_proj_yrs <- nproj_yrs
    
    rep_term_yr <- function(x, along = 3) abind::abind(replicate(n_proj_yrs,                # repeat the terminal year of a [pop, region, year, ...] array over the projection years; unlike rep(each =) this keeps regions lined up, so the same code works for the spatial models
                                                                 abind::asub(x, n_yrs, along, drop = FALSE), simplify = FALSE), along = along)
    WAA <- rep_term_yr(data$WAA)
    WAA_fish <- rep_term_yr(data$WAA_fish)
    MatAA <- rep_term_yr(data$MatAA)
    fish_sel <- rep_term_yr(rep$fish_sel)
    Movement <- rep_term_yr(rep$Movement, along = 4)
    natmort <- rep_term_yr(rep$natmort)
    proj_years <- term_yr + seq_len(n_proj_yrs - 1)                                # projection year 1 replays term_yr, so it is dropped
    
    # Define independent SPR reference points to use in HCR
    f_ref_pt_glob = array(ref_pt$f_ref_pt, dim = c(data$n_regions, n_proj_yrs))
    b_ref_pt_glob = array(ref_pt$b_ref_pt, dim = c(data$n_pop, data$n_regions, n_proj_yrs))
    
    # ---- recruitment options: define windows once, by index ----
    rec_idx <- list(
      long_term_avg = calc_rec_st_yr:(n_yrs - rec_age),
      recent_avg    = (n_yrs - 5):(n_yrs - rec_age),
      high_rec_avg  = (n_yrs - 10):(n_yrs - 5),
      fixed_period_early = calc_rec_st_yr:40,
      fixed_period_mid = calc_rec_st_yr:50,
      fixed_period_late = calc_rec_st_yr:60
    )
    
    recruitment_options <- lapply(rec_idx, function(idx)
      array(rep$Rec[,,idx],
            dim = c(data$n_pop, data$n_regions, length(idx)))
    )
    
    # Do projection to get ABC
    get_abc_vals <- Do_Population_Projection(n_proj_yrs = n_proj_yrs,              # projection years
                                             n_regions = data$n_regions,
                                             n_ages = length(data$ages),
                                             n_sexes = data$n_sexes,
                                             sexratio = array(dat$sex_ratio, dim = c(data$n_pop, data$n_regions, n_proj_yrs,data$n_sexes)),
                                             n_fish_fleets = data$n_fish_fleets,
                                             do_recruits_move = 0,
                                             recruitment = recruitment_options[["long_term_avg"]],           # same window as the reference points (calc_rec_st_yr = 20)
                                             terminal_NAA = array(rep$NAA[,,n_yrs,,,], dim = c(data$n_pop, data$n_regions, data$n_seas, n_ages, data$n_sexes)),
                                             terminal_NAA0 = array(rep$NAA0[,,n_yrs,,,], dim = c(data$n_pop, data$n_regions, data$n_seas, n_ages, data$n_sexes)),
                                             terminal_F = array(rep$Fmort[,n_yrs,,], dim = c(data$n_regions, data$n_seas, data$n_fish_fleets)),
                                             n_pop = data$n_pop,
                                             natmort = natmort,
                                             WAA = WAA,
                                             WAA_fish = WAA_fish,
                                             MatAA = MatAA, 
                                             fish_sel = fish_sel,
                                             Movement = Movement,
                                             f_ref_pt = f_ref_pt_glob,
                                             b_ref_pt = b_ref_pt_glob,
                                             HCR_function = HCR_function,
                                             recruitment_opt = "mean_rec",
                                             fmort_opt = "HCR", 
                                             t_spawn = data$t_spawn)
    
    # ABC for subsequent years (dimensions, region, year, fleet)
    sum(get_abc_vals$proj_Catch[,,2,,])                                             # year 2 = the actual first projected year
    saveRDS(get_abc_vals, file = here(out_dir, paste0(mod_name,"_",mod_yr,"_v1.2_ABC_proj.RDS")))
    
    proj_model_opt <- list(n_proj_yrs = n_proj_yrs,              # projection years
                           n_regions = data$n_regions,
                           n_ages = length(data$ages),
                           n_sexes = data$n_sexes,
                           sexratio = array(dat$sex_ratio, dim = c(data$n_pop, data$n_regions, n_proj_yrs,data$n_sexes)),
                           n_fish_fleets = data$n_fish_fleets,
                           do_recruits_move = 0,
                           recruitment = recruitment_options[["long_term_avg"]],           # same window as the reference points (calc_rec_st_yr = 20)
                           terminal_NAA = array(rep$NAA[,,n_yrs,,,], dim = c(data$n_pop, data$n_regions, data$n_seas, n_ages, data$n_sexes)),
                           terminal_NAA0 = array(rep$NAA0[,,n_yrs,,,], dim = c(data$n_pop, data$n_regions, data$n_seas, n_ages, data$n_sexes)),
                           terminal_F = array(rep$Fmort[,n_yrs,,], dim = c(data$n_regions, data$n_seas, data$n_fish_fleets)),
                           n_pop = data$n_pop,
                           natmort = natmort,
                           WAA = WAA,
                           WAA_fish = WAA_fish,
                           MatAA = MatAA, 
                           fish_sel = fish_sel,
                           Movement = Movement,
                           f_ref_pt = f_ref_pt_glob,
                           b_ref_pt = b_ref_pt_glob,
                           HCR_function = HCR_function,
                           recruitment_opt = "mean_rec",
                           fmort_opt = "HCR", 
                           t_spawn = data$t_spawn,
                           n_avg_yrs = 1)
    
    out <- get_key_quants(
      data                  = list(data),
      rep                   = list(rep),
      reference_points_opt  = reference_points_opt,
      proj_model_opt        = proj_model_opt,
      model_names           = mod_name
    )
    
   abc <- as.data.frame(out[[1]])
   write.csv(abc,here(tab_dir,paste0(mod_name,"_",mod_yr,"_v1.2_abc.csv")))
    
    tiff(filename=here(tab_dir,paste0(mod_name,"_",mod_yr,"_v1.2_abc.png")),units = "in", width=13,height=1, res = 100)
    abc_plot <- gridExtra::grid.table(abc, rows=NULL)
    dev.off()
    
########################################################################################################################
# Diagnostics ---------------------------------------
########################################################################################################################
  
    if(do_mod_bridge == 1){     # whether to run the full suite of figs and tables and comps
      mod_bridge(rtmb_mod = sabie_rtmb_model, fig_dir = plot_dir, mods_sporc = mod_bridge_names, styr = dat$styr, name_mod = mod_name, 
                 bref = b_ref_pt_glob[,,1],mod_yr)
    }
    
    if(do_hist_mod_retro == 1){
      hist_mod_retro(rtmb_mod = sabie_rtmb_model, fig_dir = plot_dir,  SA_first_YR = hist_retro_st_yr, 
                     SA_curr_YR = dat$endyr, mods_sporc = sporc_names, Model_Names = hist_mods, styr = dat$styr, 
                     name_mod = mod_name, bref = b_ref_pt_glob[,,1],mod_yr)
    }
    
if(do_diags == 1) {

# Retrospective -------------------------------------------------------------
#sabie_rtmb_model <- readRDS(here(root_folder,mod_name,paste0(mod_name,"_v1.2_model_results.RDS")))
retro <- run_retro(sabie_rtmb_model, plot_dir, tab_dir, comp_regions, styr, 
                   data_lag = FALSE, do_francis = (do_francis == 1), name_mod = mod_name,mod_yr=mod_yr)     # no data lag
retro_lag <- run_retro(sabie_rtmb_model, plot_dir, tab_dir, comp_regions, styr, 
                       data_lag = TRUE, do_francis = (do_francis == 1), name_mod = mod_name,mod_yr=mod_yr)  # lag age comps 1 yr


if(do_profile == 1){

# Likelihood profiles ---------------------------------------------------------
#sabie_rtmb_model <- readRDS(here(root_folder,mod_name,paste0(mod_name,"_v1.2_model_results.RDS")))
prof_pars <- sabie_rtmb_model$env$parList(sabie_rtmb_model$env$last.par.best)

# profile female (idx = 1) and male (idx = 2) M separately; the other sex's M is still estimated
m_fit_f <- prof_pars$ln_M[1]
m_prof_f <- SPoRC::do_likelihood_profile(
  data = sabie_rtmb_model$data,
  parameters = prof_pars,
  mapping = sabie_rtmb_model$mapping,
  random = NULL,
  what = "ln_M",
  idx = 1,
  min_val = round(m_fit_f - 1.3, 2),
  max_val = round(m_fit_f + 0.5, 2),
  inc = 0.05,
  do_par = TRUE,
  n_cores = 7
)

m_fit_m <- prof_pars$ln_M[2]
m_prof_m <- SPoRC::do_likelihood_profile(
  data = sabie_rtmb_model$data,
  parameters = prof_pars,
  mapping = sabie_rtmb_model$mapping,
  random = NULL,
  what = "ln_M",
  idx = 2,
  min_val = round(m_fit_m - 1.3, 2),
  max_val = round(m_fit_m + 0.5, 2),
  inc = 0.05,
  do_par = TRUE,
  n_cores = 7
)

r0_fit <- prof_pars$ln_global_R0
r0_prof <- SPoRC::do_likelihood_profile(
  data = sabie_rtmb_model$data,
  parameters = prof_pars,
  mapping = sabie_rtmb_model$mapping,
  random = NULL,
  what = "ln_global_R0",
  min_val = round(r0_fit - 2.5, 2),
  max_val = round(r0_fit + 4, 2),
  inc = 0.1,
  do_par = TRUE,
  n_cores = 7
)

m_prof_plots_f <- plot_likelihood_profile(m_prof_f, m_fit_f, "ln(M)", "Female natural mortality profile")
m_prof_plots_m <- plot_likelihood_profile(m_prof_m, m_fit_m, "ln(M)", "Male natural mortality profile")
r0_prof_plots <- plot_likelihood_profile(r0_prof, r0_fit, "ln(R0)", "Recruitment (R0) profile")

png_pages(here(plot_dir, paste0(mod_name,"_",mod_yr,"_v1.2_profile.png")), width = 10, height = 7)
print(m_prof_plots_f$overlay)
print(m_prof_plots_f$faceted)
print(m_prof_plots_m$overlay)
print(m_prof_plots_m$faceted)
print(r0_prof_plots$overlay)
print(r0_prof_plots$faceted)
dev.off()

} # end do_profile

# Jitter ------------------------------------------------------------------
#sabie_rtmb_model <- readRDS(here(root_folder,mod_name,paste0(mod_name,"_v1.2_model_results.RDS")))

jit <- do_jitter(
  sabie_rtmb_model$data,
  sabie_rtmb_model$parameters,
  sabie_rtmb_model$mapping,
  random = NULL,
  sd = 1,
  n_jitter = 200,
  n_newton_loops = 3,
  n_cores = 10,
  do_par = TRUE,
  par_vec = sabie_rtmb_model$env$last.par.best, 
  return_start_values = TRUE
)

saveRDS(jit, here(out_dir, paste0(mod_name,"_",mod_yr,"_v1.2_jitter.RDS")))
png_pages(here(plot_dir, paste0(mod_name,"_",mod_yr,"_v1.2_jitter.png")), width = 10, height = 7)
print(plot_jitter(jit$jitter_df, sabie_rtmb_model$rep,styr))
dev.off()

if(do_bridge_loo == 1){

# Data Bridge -------------------------------------------------------------
#--- Note that some of the Diagnostics_Helper.R code for the LOO and Bridge analyses looks to be hard coded to fleet structure etc, so may not work with alternate setups in future
#mod <- readRDS(here(root_folder,mod_name,paste0(mod_name,"_v1.2_model_results.RDS"))) # model w/ new data
mod <- sabie_rtmb_model
prev_mod_bridge <- readRDS(here("R","mod_bridge_data",paste0(mod_name_last,"_model_results.rds")))         # previous model w/o new data

# last year's data, then + catch, fishery ages, fishery lens, survey index, survey ages, survey lens
bridge <- run_bridge(mod, prev_mod_bridge)
saveRDS(bridge, here(out_dir, paste0(mod_name,"_",mod_yr,"_v1.2_bridge.RDS")))

bridge_df <- get_ssb_rec_df(bridge, styr = dat$styr)
png_pages(here(plot_dir, paste0(mod_name,"_",mod_yr,"_v1.2_bridge.png")), width = 13, height = 7)
print(plot_runs(bridge_df, "SSB", tail(names(bridge), 1))) # last step is the full model
print(plot_runs(bridge_df, "SSB", tail(names(bridge), 1), rel = TRUE))
print(plot_runs(bridge_df, "Recruitment", tail(names(bridge), 1)))
print(plot_runs(bridge_df, "Recruitment", tail(names(bridge), 1), rel = TRUE))
dev.off()

# LOO ---------------------------------------------------------------------
#mod <- readRDS(here(root_folder,mod_name,paste0(mod_name,"_v1.2_model_results.RDS")))

# remove each fleet's index and comps (fishery catch stays in)
loo <- run_loo(mod,
               fish_names = fish_names,
               srv_names = srv_names)
saveRDS(loo, here(out_dir, paste0(mod_name,"_",mod_yr,"_v1.2_loo.RDS")))

loo_df <- get_ssb_rec_df(loo, styr = dat$styr)
png_pages(here(plot_dir, paste0(mod_name,"_",mod_yr,"_v1.2_loo.png")), width = 13, height = 7)
print(plot_runs(loo_df, "SSB", "Full model"))
print(plot_runs(loo_df, "SSB", "Full model", rel = TRUE))
print(plot_runs(loo_df, "Recruitment", "Full model"))
print(plot_runs(loo_df, "Recruitment", "Full model", rel = TRUE))
dev.off()

} # end do_bridge_loo

} # end do_diags

########################################################################################################################
# MCMC --------------------------------------------------------------------
########################################################################################################################

if(do_mcmc == 1) {

mod <-  sabie_rtmb_model                        #readRDS(here(out_dir,paste0(mod_name,"_",mod_yr,"_v1.2_model_results.RDS")))
dat <- dat                                      #readRDS(here(root_folder, mod_name, paste0(dat_name,".rds")))

# turn off bias ramp
mod$data$do_rec_bias_ramp <- 0

# fit model (already has francis weights)
tmp_mle_mod <- fit_model(mod$data, mod$parameters, mod$mapping, random = NULL, 3)
tmp_mle_mod$data <- mod$data
tmp_mle_mod$sd_rep <- RTMB::sdreport(tmp_mle_mod)

# do MCMC
mcmc <- SparseNUTS::sample_snuts(
  obj = tmp_mle_mod,
  num_samples = 2e4,
  num_warmup = 500,                                                         
  chains = 4, cores = 4,
  control = list(adapt_delta = 0.999),
  skip_optimization = TRUE
)

saveRDS(mcmc, here(out_dir, paste0(mod_name,"_",mod_yr,"_v1.2_mcmc.RDS")))

# get mcmc stuff
#mcmc <- readRDS( here(out_dir,paste0(mod_name,"_",mod_yr,"_v1.2_mcmc.RDS")))
mcmc_diag <- run_mcmc_diag(mcmc = mcmc, mle_mod = tmp_mle_mod,
                           fig_dir = here(plot_dir),dir_tab = here(tab_dir),
                           styr = dat$styr, ref_mod = mod, mle_overlay = mod, model_name = mod_name, mod_yr = mod_yr)

} # end do_mcmc


########################################################################################################################
# Projections + Apportionment ---------------------------------------
########################################################################################################################

if(do_proj == 1){        # whether to run the full projection model

  abc_final <- do_proj_scenarios(mod_file = sabie_rtmb_model, do_ABC_whale = do_ABC_whale, styr = dat$styr, nsims = nsims, 
                    nproj_yrs = nproj_yrs, rec_age = rec_age, calc_rec_st_yr = calc_rec_st_yr, spr_targ = spr_targ,
                    sex_rat = sexrat, yield_ratio = yield_ratio, dir_out = proj_dir, plot_all_proj_sims = plot_all_proj_sims,
                    model_name = mod_name, mod_yr = mod_yr, do_alt_proj = do_alt_proj, mod_name_last_proj = mod_name_last_proj,
                    mod_name_last_proj2 = mod_name_last_proj2, fish_names = fish_names)
  abc_final$abc_ofl
                                                              
if(do_apport == 1){      # whether to run the full apportionment model
  do_apportionment(abc_final = abc_final, TAC = TAC, mod_file = sabie_rtmb_model, do_ABC_whale = do_ABC_whale, styr = dat$styr, 
                                   dir_out = apport_dir,model_name = mod_name, mod_yr = mod_yr, mod_name_last_proj = mod_name_last_proj,
                                   reccomend_apport = reccomend_apport, bio_age_filter = bio_age_filter)
}
}
    
########################################################################################################################
# Final SIS Reporting ---------------------------------------
########################################################################################################################  
    if(do_SIS == 1){
      sis_f_ofl(mod_file = sabie_rtmb_model, dir_out = proj_dir, term_yr = term_yr,
                model_name = mod_name, mod_yr = mod_yr, mod_name_last_proj = mod_name_last_proj)
    }
    if(do_SARA == 1){
      sara()
    }
########################################################################################################################
    dirs_print <- c(plot_dir,tab_dir,proj_dir,apport_dir)                                                            # print all figures to a single pdf
    
    file.copy(from = here(root_folder,mod_name, paste0("Model_",mod_name,"_setup.pdf")), 
              to = here(plot_dir, paste0(mod_name,"_",mod_yr,"_v1.2_setup.pdf")), overwrite = T)
    
    fig_files <- dir_ls(dirs_print, recurse = TRUE, regexp = "\\.(png|jpg|jpeg)$", ignore.case = TRUE) %>%
      str_subset(pattern = "REMA", negate = TRUE)                                                                    # 2. Find all image files (e.g., png, jpg, pdf) recursively across directories
    pdf_files <- dir_ls(dirs_print, recurse = TRUE, glob = "*.pdf") %>%
      str_subset(pattern = "plot_results", negate = TRUE)
    
    temp_png_pdfs <- tempfile(fileext = rep(".pdf", length(fig_files)))
    for (i in seq_along(fig_files)) {
      img <- image_read(fig_files[i])
      image_write(img, path = temp_png_pdfs[i], format = "pdf")
    }
    all_pdfs_to_merge <- c(as.character(pdf_files), temp_png_pdfs)
    pdf_combine(input = all_pdfs_to_merge, output = here(root_folder,mod_name,paste0(mod_name,"_",mod_yr,"_v1.2_All_Figs.pdf")))
    
    unlink(temp_png_pdfs)

    compactPDF(here(root_folder,mod_name,paste0(mod_name,"_",mod_yr,"_v1.2_All_Figs.pdf")),gs_quality = "ebook")
    
    if(save_all == 1){
      save.image(file=here(out_dir,paste0(mod_name,"_",mod_yr,"_v1.2_all_outputs.RData")))                            # Save all the data frames in case want to reload data without doing full data pulls
    }
    print("Against all odds you did it...time for some bourbon.")
########################################################################################################################
    res <- toc(quiet = TRUE) 
    elapsed_minutes <- (res$toc - res$tic) / 60
    print(paste0("Total Code Run Time Was: ", round(elapsed_minutes, 2), " minutes. Better douse that computer with a miller lite."))
########################################################################################################################