###########################################################################
## Create variables.
###########################################################################
DTA$Age <- with(DTA, cut(Inclusion_age, c(0, 55, 60, 65, 100)))
DTA$age_5yr <- DTA$Inclusion_age / 5

DTA$PSA <- with(DTA, cut(TotalPSA, c(1, 3, 5, 10, Inf), right = FALSE))

DTA$GS <- "3 + 3"
DTA[DTA$DiagGleasonSum == 7 & DTA$DiagGleason1 == 3, ]$GS <- "3 + 4"
DTA[DTA$DiagGleasonSum == 7 & DTA$DiagGleason1 == 4, ]$GS <- "4 + 3"
DTA[DTA$DiagGleasonSum > 7.5, ]$GS <- "4 + 4 or higher"
DTA$GS <- as.factor(DTA$GS)

DTA$dre <- with(DTA, PalpFind_T2 == "Ja" | PalpFind_T3 == "Ja" | PalpFind_T4 == "Ja")
DTA$dre <- ifelse(DTA$dre, "Positive", "Negative")
DTA$dre <- as.factor(DTA$dre)

# Take Missing values on PNI from the other PNI variable. Only one missing on
# "Ja" so set it to 1.
DTA[DTA$PerineuralInvasion == "Nej" & is.na(DTA$pni_n_slides), 'pni_n_slides'] <- 0
DTA[DTA$PerineuralInvasion == "Ja" & is.na(DTA$pni_n_slides), 'pni_n_slides'] <- 1

DTA$PNI <- DTA$pni_n_slides
DTA$PNI[DTA$PNI > 3] <- "4 or more"
DTA$PNI <- factor(DTA$PNI)

DTA$pni_any <- DTA$pni_n_slides > 0
DTA$pni_any <- ifelse(DTA$pni_any, "Positive", "Negative")
DTA$pni_any <- as.factor(DTA$pni_any)

DTA$Cancerlength_cat <- with(DTA, cut(DTA$Cancerlength, c(0, 5, 10, 20, 40, Inf), right = FALSE))
DTA$ca_length_10mm <- DTA$Cancerlength / 10
  
# Variables from RP
# There are a few missing values in the RP variables, impute these with the mode.
# We also have info of Lymph node positivity (Ant_kortlar_utrymda_o_antal_pos) but
# it is almost entiarly missing (698 missing).

# Pathological stage
DTA$pat_stage <- 'T2'  # 7 missing imputed as pT2
tmp <- tolower(DTA$pT)
DTA$pat_stage[grepl('p3|pt3|t3', tmp)] <- 'T3a'
DTA$pat_stage[grepl('3b|4', tmp)] <- 'T3b/T4'  # Only one case of T4
DTA$pat_stage <- as.factor(DTA$pat_stage)

# Positive surgical margin
tmp = grepl('pos', DTA$Resektionsrand_po__el_neg)  # 4 missing imputed as neg.
DTA$surg_margin <- 'Negative'
DTA$surg_margin[tmp] <- 'Positive'
DTA$surg_margin <- as.factor(DTA$surg_margin)
  
# Extraprostatic extension
tmp = grepl('pos|ja', DTA$Extraprostatisk_vaxt_pos_el_neg)  # 10 missing imputed as neg.
DTA$extraprostatic_extension <- 'Negative'
DTA$extraprostatic_extension[tmp] <- 'Positive'
DTA$extraprostatic_extension <- as.factor(DTA$extraprostatic_extension)
  
# Seminal vesicle invasion
tmp = grepl('pos|ja', DTA$Vesikelinvasion_pos_el_neg)  # 20 missing imputed as neg.
DTA$seminal_vesicle_invasion <- 'Negative'
DTA$seminal_vesicle_invasion[tmp] <- 'Positive'
DTA$seminal_vesicle_invasion <- as.factor(DTA$seminal_vesicle_invasion)

# Format for pretty print in the hazard plot.
DTA$`Age (per 5yr)` <- DTA$age_5yr
DTA$`Gleason score` <- DTA$GS
DTA$`Digital rectal examination` <- DTA$dre
DTA$`Perineural invasion` <- DTA$pni_any
DTA$`Cancer length (per 10mm)` <- DTA$ca_length_10mm
DTA$`Pathological stage` <- DTA$pat_stage
DTA$`Surgical margin` <- DTA$surg_margin
DTA$`Extraprostatic extension` <- DTA$extraprostatic_extension
DTA$`Seminal vesicle invasion` <- DTA$seminal_vesicle_invasion
