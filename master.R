#-----------------------------------------------------------------------------
# Study.......: PNI_clinical
# Date........: 2019-06-21
# Author......: petstr (peter.strom@ki.se)
# Purpose.....: Perineural invasion is a pathway of tumour spread. In this
#               study we investigate if it is a clinically relevant marker 
#               in prostate cancer diagnosis, i.e. does it have an independent
#               prognostic value. 
# Data used...: SCB2_PERSON_MAIN
#               SCB2_S0_INCA
#               SCB2_S0_PSA
#               SOS2_DEAD_CAUSE
# Output......: 
#-----------------------------------------------------------------------------

#-- Libraries ----------------------------------------------------------------
wants <- c("tidyverse", "lubridate")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])
lapply(wants, require, character.only = TRUE)
rm(wants, has)

#-- Main program -------------------------------------------------------------
# source("raw_to_analysis.R")  # Only need once!
source("survival.R")

##############################################################################
## NOTES about the data
##############################################################################
