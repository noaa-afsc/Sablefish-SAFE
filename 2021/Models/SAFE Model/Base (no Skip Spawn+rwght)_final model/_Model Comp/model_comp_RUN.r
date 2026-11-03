###############################################################
# Code to run model comparisons
# Created by Daniel Goethel (NOAA) and Kristen Omori (VIMS)
#############################################################
rm(list=(ls()))
library(MASS)
library(Hmisc)
library(gridExtra)
library(dplyr)
library(magrittr)
library(tibble)
library(tidyr)
library(gplots)
library(ggplot2)
library(grid)
library(ggsci)
############################################################
# The function r code, model_comp.r, must be saved in the input directory with the following files
# Requires 4 output files from the Alaskan sablefish tem.tpl model for each model run being compared
# Each file must end with the number corresponding to its place in the model names vector (mod_names_input)
# Files include:
#                sable1.rep  # report file from the tem.tpl
#                tem1.par    # parameter file from tem.tpl
#                tem.rdat    # S output file containing similar outputs to sable1.rep, output from tem.tpl based on sable-r-report.cxx code
#                tem.std     # standard deviation file from the tem.tp (IF it exists...not required to run comparisons)
############################################################

##############################################################
# The comparison function requires 5 inputs
#               sable_model_comp <- function(comp_group,model_names,dat_num, dir_input, dir_output)
#               comp_group   # Name of the comparison group to use in saving files
#               model_names  # Vector of names for each model run to label outputs in graphics (must match the order of numbers of the input files described above)
#               dat_num      # The number of model runs to be compared
#               dir_input    # The directory where the input files are stored
#               dir_output   # The directory where the output graphics will be saved
#################################################################

########## INPUTS #########################################################################################################################################################
#dir_input_input<-"E:\\NOAA FILES\\Work Projects\AFSC\\Sablefish Assessment\\2021 Assessment\\Sept PT Meeting\\Sensitivity Runs\\___No Sel q block (final model building scenarios)\\10. _All Updates Model Comparison\\model comp"

dir_input_input<-getwd()

dat_num_input<-4                                      # Number of models to be compared
mod_names_input<-c("21.12_Proposed_No_Skip_Spawn", "16.5_Cont", 
                   "21.10_Proposed_w_Skip_Spawn","21.12_Proposed_No_Reweight")    # Model names

comp_group_input<-"Final 2021 Model Comparisons"                 # Group name
###################################################################################################################################################################

########## Run Comparison Function #########################################################################################################################################################
setwd(dir_input_input)
dir.create(paste0(dir_input_input, "\\Plots"))
dir_output_input<-paste0(dir_input_input, "\\Plots")

source("model_comp.r")
sable_model_comp(comp_group_input,mod_names_input,dat_num_input,dir_input_input,dir_output_input)
########## END ########################################################################################################################################################
