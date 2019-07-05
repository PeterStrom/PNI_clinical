##############################################################################
# NOTES 
# C61	Malign tumör i prostata | Underliggande dödsorsak ("ULORSAK") -> no events.
# sum(DTA$ULORSAK == 'C61')
##############################################################################

##############################################################################
# Read data and selcect columns of interest.
##############################################################################
data_path <- "../data/raw_data"
SCB_PERS <- read_csv(file.path(data_path, 'sthlm0', 'SCB2_PERSON_MAIN.csv'))  # Personal characteristics
SCB_PSA <- read_csv(file.path(data_path, 'sthlm0', 'SCB2_S0_PSA.csv'))  # STHLM0 PSA register
SOS_DEAD <- read_csv(file.path(data_path, 'sthlm0', 'SOS2_DEAD_CAUSE.csv'))  # Cause of death register
LINK_rid_REG <- read_tsv(file.path(data_path, 'sthlm3',  'STHLM3_STUDY.txt'))  # Link between STHLM0 and STHLM3
LINK_rid_studieid <- readxl::read_excel(file.path(data_path, 'PADoIPToStudieID_20180917.xlsx'))
DTA <- readxl::read_excel(file.path(data_path, 'PatiOrdning_Ren.xlsx'))  # Postop data STHLM3
PNI_info <- read_csv(file.path(data_path, 'pni_core.csv'))

##############################################################################
# Combine the link information for STHLM3 and STHLM0.
##############################################################################
keep <- c("REGnr",
          "Studieid__contact_patologsvar_",
          "VisitDate",
          "rid")
LINK_rid_studieid <- LINK_rid_studieid[keep]
LINK_rid_studieid <- LINK_rid_studieid %>% 
  rename(Studieid = Studieid__contact_patologsvar_,
                                                  RID = rid)
LINK <- left_join(x=LINK_rid_studieid, y=LINK_rid_REG, by="RID")

##############################################################################
# Columns to use from STHLM3 postop (DTA).
##############################################################################
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
DTA$Op_datum <- as.Date(format(DTA$Op_datum, format='%Y-%m-%d'))
DTA$end_of_study <- end_of_study
DTA$Birth <- as.Date(DTA$Birth, format = "%Y%m%d")
DTA$Inclusion_age <- as.numeric(DTA$Inclusion_age)
DTA$VisitDate <- as.Date(format(DTA$VisitDate, format='%Y-%m-%d'))
DTA$TotalPSA <- as.numeric(DTA$TotalPSA)

# Remove 6 duplicated rows (very similar so keep first)
DTA <- DTA %>%
  group_by(Studieid) %>%
  filter(row_number() == 1) %>%
  ungroup()

# Remove 4 benign cases.
benign = c("D10035047", "D10062831", "D10075489", "D10260723")
DTA <- DTA[!(DTA$Studieid %in% benign), ]

# Make a tracker for cohort selection.
tracker <- data.frame(text = "All with RP in STHLM3", n = dim(DTA)[1])

##############################################################################
# Aggregate n PNI cores per man and add to DTA.
##############################################################################
pni <- aggregate(PNI_info$slide_pni, by=list(Category=PNI_info$REGNR), FUN=sum)
pni <- pni %>% rename(REGnr = Category, pni_n_slides = x)

pni <- left_join(x=pni, y=LINK[, 1:2], by="REGnr")
pni$REGnr <- NULL
pni <- pni[!is.na(pni$Studieid), ]
DTA <- left_join(x=DTA, y=pni, by="Studieid")

##############################################################################
# From cause of death register add death date.
##############################################################################
keep <- c("X_DODSDAT",
          "LOPNR")
SOS_DEAD <- SOS_DEAD[keep]

SOS_DEAD <- left_join(x=SOS_DEAD, y=LINK[, c("Studieid", "LOPNR")], by="LOPNR")
SOS_DEAD$LOPNR <- NULL
DTA <- left_join(x=DTA, y=SOS_DEAD, by="Studieid")

##############################################################################
# Men moving out of Stockholm, first add operation date.
##############################################################################
SCB_PERS <- left_join(x=SCB_PERS, y=LINK[, c("Studieid", "LOPNR")], by="LOPNR")
SCB_PERS$LOPNR <- NULL
SCB_PERS <- left_join(x=DTA, y=SCB_PERS, by="Studieid")
SCB_PERS$Op_datum <- substr(SCB_PERS$Op_datum, 1, 10)

men_moving_out_of_sthlm <- function(SCB_PERS){
  # Function to get the year STHLM0 men are moving out of the county.
  # 
  # Args:
  #     SCB_PERS: A data.frame with SCB total population register with STHLM0
  #               men. Must contain LOPNR, COUNTY_20XX columns and 'start_var'.
  #     id: an ID variable unique for each subject.
  #     start_var: a column in the data.frame with first date to consider, e.g.
  #               diagose date.
  # 
  # Return:
  #   A data.frame with LOPNR and the first year of moving (change_county_date).
  # 
  # NOTE: Migration from STHLM County Extract first year when not living in
  #       STHLM - Emigration will be coded end of that year: year-12-31.
  
  # Select columns
  col = c("Studieid", paste("COUNTY_", 2008:2017, sep = ""), "Op_datum")
  df_emi_sthlm <- SCB_PERS[, col]
  
  # Make long format
  df_emi_sthlm <- gather(df_emi_sthlm,
                         key="county_year",
                         value="county",
                         COUNTY_2008:COUNTY_2017)
  
  # Make variable for each year
  df_emi_sthlm['change_county_date'] <- as.numeric(substr(df_emi_sthlm$county_year,
                                                          8, 11))
  df_emi_sthlm$county_year <- NULL
  
  # Make indicator variabel for men NOT in Stockholm for ear year.
  df_emi_sthlm <- df_emi_sthlm %>% 
    group_by(Studieid) %>% 
    mutate(flag = ifelse(county != 1 | is.na(county), 1, 0))  # Count == 1 for Stockholm
  
  # Filter years after "Op_datum".
  df_emi_sthlm <- filter(df_emi_sthlm, change_county_date >= year(Op_datum))
  
  # Select the first year of moving out of Stockholm for those that do, and
  # code the last day of the year for each man.
  df_emi_sthlm <- df_emi_sthlm %>% 
    arrange(Studieid, change_county_date) %>% 
    filter(flag == 1) %>%  # flag == 1 means not Stockholm or missing info.
    group_by(Studieid) %>% 
    filter(row_number() == 1) %>% 
    mutate(emidate_sthlm = as.Date(paste(change_county_date, "-12-31", sep = ""),
                                   format = "%Y-%m-%d"))
  
  return(df_emi_sthlm)
}

move <- men_moving_out_of_sthlm(SCB_PERS=SCB_PERS)
move <- move[, c("Studieid", "emidate_sthlm")]
DTA <- left_join(x=DTA, y=move, by="Studieid")

##############################################################################
# Add PSA data
##############################################################################
# Keep only rows corresponing to TotalPSA
SCB_PSA <- SCB_PSA %>% filter(PSA_RESULT_TYPE == 1)

SCB_PSA[(SCB_PSA$RESULT_RAW == "<0,05") & (!is.na(SCB_PSA$RESULT_RAW)), ]$X_RESULT <- 0.05
SCB_PSA <- SCB_PSA[c("LOPNR", "X_SAMPLE_DATE", "X_RESULT")]

# Remove Missing date or value on PSA
SCB_PSA$X_SAMPLE_DATE <- as.Date(format(SCB_PSA$X_SAMPLE_DATE, format='%Y-%m-%d'))
SCB_PSA <-SCB_PSA[SCB_PSA$X_RESULT >= 0, ]
SCB_PSA <- SCB_PSA %>% drop_na(X_SAMPLE_DATE)

# Select all PSA tests after operation date.
SCB_PSA <- left_join(x=LINK[, c("Studieid", "LOPNR")], y=SCB_PSA, by="LOPNR")
SCB_PSA$LOPNR <- NULL
SCB_PSA <- left_join(x=DTA[, c("Studieid", "Op_datum")], y=SCB_PSA, by="Studieid")
SCB_PSA <- SCB_PSA %>% filter(Op_datum < X_SAMPLE_DATE)
SCB_PSA$Op_datum <- NULL
DTA <- left_join(x=DTA, y=SCB_PSA, by="Studieid")

##############################################################################
# Save analysis data
##############################################################################
save(DTA, tracker, file="../data/analysis_data.RData")
