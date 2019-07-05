###########################################################################
## Create variables.
###########################################################################
DTA$age_cat <- with(DTA, cut(Inclusion_age, c(0, 55, 60, 65, 70, 100)))

DTA$psa_cat <- with(DTA, cut(TotalPSA, c(0, 3, 5, 10, 10000), right = FALSE))
DTA$PSA_sqrt <- sqrt(DTA$TotalPSA)

DTA$volume_cat <- with(DTA, cut(ProstateVolume, c(0, 35, 50, 1000)))

DTA$GS <- DTA$DiagGleasonSum 
DTA$GS <- ifelse(DTA$GS == 7 & DTA$DiagGleason1 == 4, 7.5, DTA$GS)
DTA$GS <- ifelse(DTA$GS > 7.5, '8-10', DTA$GS)

DTA$dre <- as.numeric(with(DTA,
                           PalpFind_T2 == "Ja" | PalpFind_T3 == "Ja" | PalpFind_T4 == "Ja"))

# Take Missing values on PNI from the other PNI variable. Only one missing on
# "Ja" so set it to 1.
DTA[DTA$PerineuralInvasion == "Nej" & is.na(DTA$pni_n_slides), ]$pni_n_slides <- 0
DTA[DTA$PerineuralInvasion == "Ja" & is.na(DTA$pni_n_slides), ]$pni_n_slides <- 1

DTA$pni <- as.numeric(DTA$pni_n_slides > 0)

