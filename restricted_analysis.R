DTA67 <- DTA[DTA$ISUP %in% c('1', '2'), ]
DTA8p <- DTA[DTA$ISUP %in% c('3', '4-5'), ]
DTA67$ISUP <- as.character(DTA67$ISUP)
DTA8p$ISUP <- as.character(DTA8p$ISUP)
DTA$PNI_ISUP <- (DTA$ISUP %in% c('3', '4-5')) & (DTA$`Perineural invasion` == 'Positive')

# Only ISUP 1 and 2
vars_hr = c("`Perineural invasion`",
            "`Age (per 5yr)`",
            "PSA",
            "`Digital rectal examination`",
            "`Cancer length (per 10mm)`",
            "ISUP")

adjust <- paste(vars_hr, collapse = ' + ')
formula = as.formula(paste0("Surv(time, event) ~ ", adjust))

hr_biop <- coxph(formula, data = DTA67, ties="breslow")

ggf <- ggforest(hr_biop)
ggsave(plot = ggf, filename="../output/HR_only_biopsy_markers_restricted_67.pdf")

# Only ISUP 3 and 4-5
hr_biop <- coxph(formula, data = DTA8p, ties="breslow")

ggf <- ggforest(hr_biop)
ggsave(plot = ggf, filename="../output/HR_only_biopsy_markers_restricted_8p.pdf")

# Interaction term with positive PNI and ISUP >= 3.
vars_hr = c("`Perineural invasion`",
            "`Age (per 5yr)`",
            "PSA",
            "`Digital rectal examination`",
            "`Cancer length (per 10mm)`",
            "ISUP",
            "PNI_ISUP")

adjust <- paste(vars_hr, collapse = ' + ')
formula = as.formula(paste0("Surv(time, event) ~ ", adjust))

hr_biop <- coxph(formula, data = DTA, ties="breslow")

ggf <- ggforest(hr_biop)
ggsave(plot = ggf, filename="../output/HR_only_biopsy_markers_interaction.pdf")
