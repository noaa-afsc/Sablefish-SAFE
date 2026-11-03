//==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+
//
//  Statistical, separable age-structured population model for sablefish
//  Alaska Fisheries Science Center, October 2008
//  D. Hanselman:dana.hanselman@noaa.gov
//  UPDATED (and commented)  by D. Goethel: daniel.goethel@noaa.gov (10/15/20)
//####################################################################

// ### File Structure ###########################################################################################
//  OLD_Input file:   tem.dat, as in tem(plate).....provides the settings for likelihoods, phases, data weights, SRR settings
//  Control file: tem.ctl.....same as tem.dat, but no comments...read this into tpl so need to make same changes here as to tem.dat
//  Data File: tem_2020_na_wh.dat....actual data inputs updated each year with new catch, age comp, length comp, etc.
//  Program file: tem.tpl
//  Output files: tem.rep, tem.std, proj.dat, newproj.dat
// ###############################################################################################

// ### Fleet/Area Structure #############################################################################################
// Single Area (combined across GOA, BS, AI)
// Split sexes, seperate weight at ages, age-length keys and unsexed
// All fishery catch include  catch and discards
// LL catch is adjusted (increased) for whale depredation (CPUE is not ajusted)
// DOM LL survey RPN/RPW is adjusted (increased) for whale depredation
// All fisheries and surveys are across entire model domain (GOA, BS, AI)

// srv1= Domestic longline survey RPW (not fit in model)                 (logistic selectivity)
// srv2= Japanese longline survey (not fit in model)                     (logistic selectivity)
// srv3= Domestic LL survey RPN (fit in model)                           (logistic selectivity)
// srv4 = Japanese LL survey RPN (fit in model)                          (logistic selectivity)
// srv5 = Domestic LL fishery RPW (fit in model)                         (logistic selectivity)
// srv6 = Japanese LL fishery RPW (fit in model)                         (logistic selectivity)
// srv7 = NMFS bottom trawl survey (currently GOA only; fit in model)    (POWER FUNCTION selectivity)
// srv8 = Post-IFQ DOM LL fishery q par  RPW (fit in model)              (logistic selectivity)
// srv9 = JPN LL q par for 1990-1994 (not estimated just set to qsrv2, only used if qsrv2 is not estimated)
// srv10 = DOM LL survey RPN sel par recent time block
              // account for recent changes in selectivity and age/length comps due to strong 2014, 2016, 2017 yc

// fish1 = U.S. Longline fishery (fit in model)                           (logistic selectivity)
// fish2 = Japanese LL fishery (fit in model)                             (logistic selectivity)
// fish3 = U.S. trawl fishery (fit in model)                              (GAMMA FUNCTION selectivity)
// fish4 = Post IFQ LL fishery sel par                                    (logistic selectivity)                 // Dana has it listed as Japanese trawl fishery, but I think this is some sort of hold over no longer used...fish4 sel coefficients are clearly post_IFQ LL
// fish5 = Post IFQ LL fishery sel par second time block                  (logistic selectivity)
                        // account for recent changes in selectivity and age/length comps due to strong 2014, 2016, 2017 yc

// ############################################################################################

// ### Estimated Parameters Base Model #########################################################################################

//   RECRUITMENT
// log_mean_rec = mean recruitment estimate                                                                       (1 parameter)
// log_rec_dev = Recruitment yearly devs from mean Rec                                                            ((nages-2)+(nyrs-1)  parameters; est devs for each age at initial equil (except age 1 and plus group) and for each year (except terminal year))

//   NATURAL MORTALITY
// log_M          = Base Natural Mortality                                                                         (1 parameter)  
// log_M_devs     = Yearly Deviations from M                                                                       (NOT EST BASE; nyrs parameters)
// log_M_devs_age = Age Deviations from M                                                                          (NOT EST BASE; nages parameters)

//   FISHING MORTALITY
// log_avg_F_Fish1 = F multiplier for LL fishery                                                                   (1 parameter)  
// log_avg_F_fish3 = F multiplier for Trawl fishery                                                                (1 parameter)
// log_F_devs_fish1 = F yearly devs for LL fishery                                                                 (nyrs  parameters)
// log_F_devs_fish3 = F yearly devs for Trawl fishery                                                              (nyrs  parameters)

//   CATCHABILITY COEFFICIENTS
// log_q_srv1 = q DOM LL survey RPN/RPW                                                                            (1 parameter)
// log_q_srv2 = q JPN LL survey RPN/RPW                                                                            (1 parameter)
// log_q_srv5 = q DOM LL fishery RPW                                                                               (1 parameter)
// log_q_srv6 = q JPN LL fishery RPW                                                                               (1 parameter)
// log_q_srv7 = q DOM trawl survey                                                                                 (1 parameter)
// log_q_srv8 = q DOM LL fishery RPW post-IFQ                                                                      (1 parameter)

//   LOGISTIC SELECTIVITY PARAMETERS FISHERY
// log_a50_fish1_f = DOM LL fishery age at 50% selection females                                                   (1 parameter)
// log_a50_fish1_m = DOM LL fishery age at 50% selection males                                                     (1 parameter)
// log_delta_fish1_f = DOM LL fishery diff in age at 50% and 90% selection females                                 (NOT EST BASE; USE FISH4 FEMALE EST; 1 parameter)
// log_delta_fish1_m = DOM LL fishery diff in age at 50% and 90% selection males                                   (NOT EST BASE; USE FISH4 FEMALE EST; 1 parameter)
// log_a50_fish2 = JPN LL fishery age at 50% selection                                                             (1 parameter)
// log_delta_fish2 = JPN LL fishery diff in age at 50% and 90% selection                                           (NOT EST BASE; USE FISH4 FEMALE EST; 1 parameter)
// log_a50_fish4_f = DOM LL fishery post-IFQ age at 50% selection females                                          (1 parameter)
// log_a50_fish4_m = DOM LL fishery post-IFQ age at 50% selection males                                            (1 parameter)
// log_delta_fish4_f = DOM LL fishery post-IFQ diff in age at 50% and 90% selection females                        (1 parameter; USED FOR ALL OTHER DELTAS)
// log_delta_fish4_m = DOM LL fishery post-IFQ diff in age at 50% and 90% selection males                          (NOT EST BASE; USE FISH4 FEMALE EST; 1 parameter)
// log_a50_fish5_f = DOM LL fishery post-IFQ recent time block age at 50% selection females                        (NOT EST BASE; EST 1 parameter when active)
// log_a50_fish5_m = DOM LL fishery post-IFQ recent time block age at 50% selection males                          (NOT EST BASE; EST 1 parameter when active)
// log_delta_fish5_f = DOM LL fishery post-IFQ recent time block diff in age at 50% and 90% selection females      (NOT EST BASE; USE FISH4 FEMALE EST WHEN ACTIVE; 1 parameter)
// log_delta_fish5_m = DOM LL fishery post-IFQ recent time block diff in age at 50% and 90% selection males        (NOT EST BASE; USE FISH4 FEMALE EST WHEN ACTIVE; 1 parameter)

//   GAMMA FUNCTION SELECTIVITY PARAMETERS FISHERY
// log_a50_fish3_f = DOM Trawl fishery age at 50% selection females                                                (1 parameter)
// log_a50_fish3_m = DOM Trawl fishery age at 50% selection males                                                  (1 parameter)
// log_delta_fish3_f = DOM Trawl fishery diff in age at 50% and 90% selection females                              (1 parameter)
// log_delta_fish3_m = DOM Trawl fishery diff in age at 50% and 90% selection males                                (1 parameter)

//   LOGISTIC SELECTIVITY PARAMETERS SURVEY
// log_a50_srv1_f = DOM LL survey age at 50% selection females                                                     (1 parameter)
// log_a50_srv1_m = DOM LL survey age at 50% selection males                                                       (1 parameter)
// log_delta_srv1_f = DOM LL survey diff in age at 50% and 90% selection females                                   (NOT EST BASE; USE SRV2 MALE EST; 1 parameter)
// log_delta_srv1_m = DOM LL survey diff in age at 50% and 90% selection males                                     (NOT EST BASE; USE SRV2 MALE EST;1 parameter)
// log_a50_srv2 = JPN LL survey age at 50% selection females                                                       (1 parameter)
// log_a50_srv2 = JPN LL survey age at 50% selection males                                                         (1 parameter)
// log_delta_srv2 = JPN LL survey diff in age at 50% and 90% selection females                                     (NOT EST BASE; USE SRV2 MALE EST;1 parameter)
// log_delta_srv2 = JPN LL survey diff in age at 50% and 90% selection males                                       (1 parameter; USED FOR ALL LL SURVEYS)
// log_a50_srv10_f = DOM LL survey recent time block age at 50% selection females                                  (NOT EST BASE; EST 1 parameter when active)
// log_a50_srv10_m = DOM LL survey recent time block age at 50% selection males                                    (NOT EST BASE; EST 1 parameter when active)
// log_delta_srv10_f = DOM LL survey recent time block diff in age at 50% and 90% selection females                (NOT EST BASE; USE SRV2 MALE EST WHEN ACTIVE; 1 parameter)
// log_delta_srv10_m = DOM LL survey recent time block diff in age at 50% and 90% selection males                  (NOT EST BASE; USE SRV2 MALE EST WHEN ACTIVE; 1 parameter)

//   POWER FUNCTION SELECTIVITY PARAMETERS SURVEY
// log_a50_srv7_f = DOM Trawl survey age at 50% selection females                                                  (1 parameter)
// log_a50_srv7_m = DOM Trawl survey age at 50% selection males                                                    (1 parameter)

//   SPR F VALUES
// mF50 = FSPR50 for  SPR calcs                                                                                    (1 parameter)
// mF40 = FSPR40 for  SPR calcs                                                                                    (1 parameter)
// mF35 = FSPR35 for  SPR calcs                                                                                    (1 parameter)

/////////////////
// estimable but phase<0 for Base
//////////////////////

// steepness               (no SRR used)
// log_Rzero               (no SRR used)
// sigr                    (recruit sigma input)
// mdelta                  (M set equal for males and females; this male offset not estimated)
// selectivity parameters  (4 selectivity variations coded, only use those parameters noted as estimated above)

// ############################################################################################


// ############################################################################################
//### Changes in August 2016 ##################################################################################### 
// added SARA outputs, new SDNR summaries at end
// standard deviation for SSB
// Fixed hessian problem on tot_bio_proj(endyr+1)
// revised sable-r-report.cxx, and mhp-func.cpp to reflect new C++ protocols
// Added executive material sections and new report sable.rep with SDs
// Cleaned up a couple of messes like srv22, srv2q
//########################################################################################

//### Changes in October 2020 #####################################################################################
// add switches and  estimation for  selectivity and survey parameters in a new recent time block to account for recent changes in targeting/age-length comp in fishery and LL survey due to large 2014, 2016, and 2017 ycs
// add ability to estimate yearly Mdevs
// #####################################################################################

//==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+