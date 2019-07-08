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
DTA$PNI <- factor(DTA$PNI, levels(DTA$PNI)[rev(1:5)])

DTA$`Perineural invasion` <- DTA$pni_n_slides > 0
DTA$`Perineural invasion` <- ifelse(DTA$`Perineural invasion`, "Positive", "Negative")
DTA$`Perineural invasion` <- as.factor(DTA$`Perineural invasion`)

DTA$Cancerlength_cat <- with(DTA, cut(DTA$Cancerlength, c(0, 5, 10, 20, 40, Inf), right = FALSE))
DTA$`Cancer length (per 10mm)` <- DTA$Cancerlength / 10
