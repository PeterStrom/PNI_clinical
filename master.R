#-----------------------------------------------------------------------------
# Study.......: PNI_clinical
# Date........: 2019-06-21
# Author......: petstr (peter.strom@ki.se)
# Purpose.....: Perineural invasion is a pathway of tumour spread. In this
#               study we investigate if it is a clinically relevant marker 
#               in prostate cancer diagnosis, i.e. does it have an independent
#               prognostic value. 
# Data used...: SCB2_PERSON_MAIN
#               SCB2_S0_PSA
#               SOS2_DEAD_CAUSE
# Output......: 
#-----------------------------------------------------------------------------

#-- Libraries ----------------------------------------------------------------
wants <- c("tidyverse", "lubridate", "Epi", "survival", "survminer", "openxlsx")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])
lapply(wants, require, character.only = TRUE)
rm(wants, has)

#-- Main program -------------------------------------------------------------
event_threshold = 0.2  # ng/mL also common value is 0.4
first_psa_below = 0.2  # ng/mL
end_of_study = as.Date("2017-12-31")

# source("raw_to_analysis.R")  # Only need once!
load("../data/analysis_data.RData")

source("analysis_to_survival.R")
source("create_variables.R")
source("survival.R")
source("Table1.R")
