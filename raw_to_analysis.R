# TODO:
# 1. Check agreement of PerineuralInvasion in DTA and my own variable from python.
# 2. Link PSA relaps, death, migration

# Read data and selcect columns of interest.
data_path <- "../data/raw_data"
SCB_PERS <- read_csv(file.path(data_path, 'sthlm0', 'SCB2_PERSON_MAIN.csv'))  # Personal characteristics
SCB_INCA <- read_csv(file.path(data_path, 'sthlm0', 'SCB2_S0_INCA.csv'))  # Quality cancer register INCA
SCB_PSA_ <- read_csv(file.path(data_path, 'sthlm0', 'SCB2_S0_PSA.csv'))  # STHLM0 PSA register
SOS_DEAD <- read_csv(file.path(data_path, 'sthlm0', 'SOS2_DEAD_CAUSE.csv'))  # Cause of death register
LINK <- read_tsv(file.path(data_path, 'sthlm3',  'STHLM3_STUDY.txt'))  # Link between STHLM0 and STHLM3
DTA <- readxl::read_excel(file.path(data_path, 'PatiOrdning_Ren.xlsx'))  # Postop data STHLM3
                   
# Columns to use from STHLM3 postop (DTA).
keep <- c("Studieid",
         "Birth",
         "TotalPSA",
         "Inclusion_age",
         "VisitDate",
         "Cancerlength",
         "CancerNumberBiopsy",
         "Diagcat_HGPIN",
         "Diagcat_IDC",
         "DuctalCancer",
         "ExtraprostaticExtension",
         "DiagGleason1",
         "DiagGleason2",
         "DiagGleasonSum",
         "PalpFind_T1",
         "PalpFind_T2",
         "PalpFind_T3",
         "PalpFind_T4",
         "PerineuralInvasion",
         "ProstateVolume",
         "TotalNumberBiopsy",
         "Op_datum",  # OP variables below.
         "Vikt_i__gram",                 
         "Tumorstorlek_1",
         "Gleason1",
         "Gleason2",
         "Summa",
         "Tertiar_grad",
         "Tumorstorlek_2",
         "Gl_summa_tumor_2",
         "Ant_kortlar_utrymda_o_antal_pos",
         "Resektionsrand_po__el_neg",
         "Vesikelinvasion_pos_el_neg",
         "Extraprostatisk_vaxt_pos_el_neg",
         "Perineural_vaxt",
         "pT")

DTA <- DTA[keep]
table(DTA$PerineuralInvasion)
head(SCB2_PERS)
sub <- SCB2_PERS %>% filter(LOPNR %in% LINK$LOPNR)
sub <- LINK %>% filter(PERSON_ID %in% DTA$Studieid)

diag_date = as.Date("2015-01-01")


men_moving_out_of_sthlm <- function(SCB_PERS, start_var){
  # Function to get the year STHLM0 men are moving out of the county.
  # 
  # Args:
  #     SCB_PERS: A data.frame with SCB total population register with STHLM0 men. Must contain
  #               LOPNR, COUNTY_20XX columns and 'start_var'.
  #     start_var: a column in the data.frame with first date to consider, e.g. diagose date.
  # 
  # Return:
  #   A data.frame with LOPNR and the first year of moving (change_county_date).
  # 
  # NOTE: Migration from STHLM County Extract first year when not living in STHLM - Emigration 
  # will be coded end of that year: year-12-31
  
  # Select columns
  col = c("LOPNR", paste("COUNTY_", 2008:2017, sep = ""), start_var)
  df_emi_sthlm <- SCB_PERS[, col]
  
  # Make long format
  df_emi_sthlm <- gather(df_emi_sthlm, key = "county_year", value = "county", COUNTY_2008:COUNTY_2017)
  
  # Make variable for each year
  df_emi_sthlm['change_county_date'] <- as.numeric(substr(df_emi_sthlm$county_year, 8, 11))
  df_emi_sthlm$county_year <- NULL
  
  # Make indicator variabel for men NOT in Stockholm for ear year.
  df_emi_sthlm <- df_emi_sthlm %>% 
    group_by(LOPNR) %>% 
    mutate(flag = ifelse(county != 1 | is.na(county), 1, 0))  # Count == 1 for Stockholm
  
  # Filter years after 'start_var'.
  df_emi_sthlm <- filter(df_emi_sthlm, change_county_date >= year(start_var))
  
  # Select the first year of moving out of Stockholm for those that do, and code the last
  # day of the year for each man.
  df_emi_sthlm <- df_emi_sthlm %>% 
    arrange(LOPNR, change_county_date) %>% 
    filter(flag == 1) %>%  # flag == 1 means not Stockholm or missing info.
    group_by(LOPNR) %>% 
    filter(row_number() == 1) %>% 
    mutate(emidate_sthlm = as.Date(paste(change_county_date, "-", "12", "-", "31", sep = ""),
                                   format = "%Y-%m-%d"))
  
  return(df_emi_sthlm)
}

a <- men_moving_out_of_sthlm(SCB_PERS = SCB2_PERS, start_var = "BIRTH_DATE")
