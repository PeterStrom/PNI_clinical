##############################################################################
# Lexis diagram.
##############################################################################
pniL <- Lexis( entry = list(cy=cal.yr(DTA$Op_datum),
                            age=Inclusion_age),
               exit = list( cy=cal.yr(DTA$exit_date)),
               exit.status = (exit_cause == "psa_relapse_date")*1,
               data = DTA )

pdf("../output/Lexis_calendar.pdf")
plot(pniL, 1:2, lwd=1, col="grey",
     grid=TRUE, lty.grid=1, col.grid=gray(0.7),
     xlim=2012.8 + c(0, 6.2), xaxs="i",
     ylim=49 + c(0, 26), yaxs="i", las=1,
     xlab="Calendar Year",
     ylab="Age",
     main="Lexis diagram of time from RP to PSA relapse")
points(pniL, 1:2, pch=c(NA, 20)[pniL$lex.Xst+1],
       col=c("blue", "red")[(pniL$pni_n_slides>0)+1], lwd=1, cex=.8)
dev.off()

##############################################################################
# Cox regression.
##############################################################################
DTA$time = (DTA$exit_date - DTA$Op_datum) / 365.25
DTA$event = (DTA$exit_cause == "psa_relapse_date")*1

followup_tilme = list(mean_followup = mean(DTA$time),
                      median_followup = median(DTA$time),
                      iqr_followup = IQR(DTA$time))

# HR only biopsy markers
vars_hr = c("`Perineural invasion`",
            "`Age (per 5yr)`",
            "PSA",
            "`Digital rectal examination`",
            "`Cancer length (per 10mm)`",
            "ISUP")

adjust <- paste(vars_hr, collapse = ' + ')
formula = as.formula(paste0("Surv(time, event) ~ ", adjust))

hr_biop <- coxph(formula, data = DTA, ties="breslow")
cox.zph(hr_biop)

ggf <- ggforest(hr_biop)
ggsave(plot = ggf, filename="../output/HR_only_biopsy_markers.png")
ggsave(plot = ggf, filename="../output/HR_only_biopsy_markers.pdf")

# HR biopsy and RP markers
vars_hr = c("`Perineural invasion`",
            "`Age (per 5yr)`",
            "PSA",
            "`Digital rectal examination`",
            "`Cancer length (per 10mm)`",
            "ISUP",
            "`Pathological stage`",
            "`Surgical margin`")

adjust <- paste(vars_hr, collapse = ' + ')
formula = as.formula(paste0("Surv(time, event) ~ ", adjust))

hr_rp <- coxph(formula, data = DTA, ties="breslow")
cox.zph(hr_rp)

ggf <- ggforest(hr_rp)
ggsave(plot = ggf, filename="../output/HR_including_RP_markers.png")
ggsave(plot = ggf, filename="../output/HR_including_RP_markers.pdf")

# Survival
hr_surv <- coxph(Surv(time, event) ~ `Age (per 5yr)` + PSA + `Digital rectal examination` + 
                   `Cancer length (per 10mm)` + ISUP + `Perineural invasion`,
                 data = DTA, ties="breslow")
surv5yr <- stdCoxph(fit=hr_surv, data=DTA, X="Perineural invasion", t=5)
std_surv_5yr <- round(summary(surv5yr)$est.table[[1]], 2)
print(std_surv_5yr)
