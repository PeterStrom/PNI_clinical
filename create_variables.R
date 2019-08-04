###########################################################################
## Create variables.
###########################################################################
DTA$Age <- with(DTA, cut(Inclusion_age, c(0, 55, 60, 65, 100)))
DTA$`Age (per 5yr)` <- DTA$Inclusion_age / 5

DTA$PSA <- with(DTA, cut(TotalPSA, c(1, 3, 5, 10, Inf), right = FALSE))

DTA$`Gleason score` <- "3 + 3"
DTA[DTA$DiagGleasonSum == 7 & DTA$DiagGleason1 == 3, ]$`Gleason score` <- "3 + 4"
DTA[DTA$DiagGleasonSum == 7 & DTA$DiagGleason1 == 4, ]$`Gleason score` <- "4 + 3"
DTA[DTA$DiagGleasonSum > 7.5, ]$`Gleason score` <- "4 + 4 or higher"
DTA$`Gleason score` <- as.factor(DTA$`Gleason score`)

DTA$`Digital rectal examination` <- with(DTA,
                                         PalpFind_T2 == "Ja" | PalpFind_T3 == "Ja" | PalpFind_T4 == "Ja")
DTA$`Digital rectal examination` <- ifelse(DTA$`Digital rectal examination`, "Positive", "Negative")
DTA$`Digital rectal examination` <- as.factor(DTA$`Digital rectal examination`)

# Take Missing values on PNI from the other PNI variable. Only one missing on
# "Ja" so set it to 1.
DTA[DTA$PerineuralInvasion == "Nej" & is.na(DTA$pni_n_slides), 'pni_n_slides'] <- 0
DTA[DTA$PerineuralInvasion == "Ja" & is.na(DTA$pni_n_slides), 'pni_n_slides'] <- 1

DTA$PNI <- DTA$pni_n_slides
DTA$PNI[DTA$PNI > 3] <- "4 or more"
DTA$PNI <- factor(DTA$PNI)

DTA$`Perineural invasion` <- DTA$pni_n_slides > 0
DTA$`Perineural invasion` <- ifelse(DTA$`Perineural invasion`, "Positive", "Negative")
DTA$`Perineural invasion` <- as.factor(DTA$`Perineural invasion`)

DTA$Cancerlength_cat <- with(DTA, cut(DTA$Cancerlength, c(0, 5, 10, 20, 40, Inf), right = FALSE))
DTA$`Cancer length (per 10mm)` <- DTA$Cancerlength / 10

# Variables from RP
# There are a few missing values in the RP variables, impute these with the mode.
# We also have info of Lymph node positivity (Ant_kortlar_utrymda_o_antal_pos) but
# it is almost entiarly missing (698 missing).

# Pathological stage
DTA$`Pathological stage` <- 'T2'  # 7 missing imputed as pT2
tmp <- tolower(DTA$pT)
DTA$`Pathological stage`[grepl('p3|pt3|t3', tmp)] <- 'T3a'
DTA$`Pathological stage`[grepl('3b|4', tmp)] <- 'T3b/T4'  # Only one case of T4
DTA$`Pathological stage` <- as.factor(DTA$`Pathological stage`)

# Positive surgical margin
tmp = grepl('pos', DTA$Resektionsrand_po__el_neg)  # 4 missing imputed as neg.
DTA$`Surgical margin` <- 'Negative'
DTA$`Surgical margin`[tmp] <- 'Positive'
DTA$`Surgical margin` <- as.factor(DTA$`Surgical margin`)

# Extraprostatic extension
tmp = grepl('pos|ja', DTA$Extraprostatisk_vaxt_pos_el_neg)  # 10 missing imputed as neg.
DTA$`Extraprostatic extension` <- 'Negative'
DTA$`Extraprostatic extension`[tmp] <- 'Positive'
DTA$`Extraprostatic extension` <- as.factor(DTA$`Extraprostatic extension`)

# Seminal vesicle invasion
tmp = grepl('pos|ja', DTA$Vesikelinvasion_pos_el_neg)  # 20 missing imputed as neg.
DTA$`Seminal vesicle invasion` <- 'Negative'
DTA$`Seminal vesicle invasion`[tmp] <- 'Positive'
DTA$`Seminal vesicle invasion` <- as.factor(DTA$`Seminal vesicle invasion`)




