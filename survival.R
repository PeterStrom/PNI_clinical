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
     xlim=2012.8 + c(0, 5.5), xaxs="i",
     ylim=49 + c(0, 26), yaxs="i", las=1,
     xlab="Calendar Year",
     ylab="Age",
     main="Lexis diagram of time from RP to PSA relapse")
points( pniL, 1:2, pch=c(NA, 3)[pniL$lex.Xst+1],
        col=c("blue", "red")[(pniL$pni_n_slides>0)+1], lwd=1.5, cex=1.5 )
dev.off()

##############################################################################
# Cox regression.
##############################################################################
DTA$time = (DTA$exit_date - DTA$Op_datum) / 365.25
DTA$event = (DTA$exit_cause == "psa_relapse_date")*1

# HR
vars_hr = c("`Perineural invasion`",
            "`Age (per 5yr)`",
            "PSA",
            "`Digital rectal examination`",
            "`Cancer length (per 10mm)`",
            "`Gleason score`")

adjust <- paste(vars_hr, collapse = ' + ')
formula = as.formula(paste0("Surv(time, event) ~ ", adjust))

hr <- coxph(formula, data = DTA)
cox.zph(hr)

ggf <- ggforest(hr)
ggsave(plot = ggf, filename="../output/HR.png")

# Survival
get_mode <- function(x){names(sort(-table(x)))[1]}
get_mode_factors <- function(x){
  val <- get_mode(x)
  return(x[x == val][1])
}

newdata = data.frame(row=1)
newdata$`Age (per 5yr)` <- median(DTA$`Age (per 5yr)`)
newdata$PSA = get_mode_factors(DTA$PSA)
newdata$`Digital rectal examination` = get_mode_factors(DTA$`Digital rectal examination`)
newdata$`Cancer length (per 10mm)` = median(DTA$`Cancer length (per 10mm)`)
newdata$`Gleason score` = get_mode_factors(DTA$`Gleason score`)

vars_surv = c("`Age (per 5yr)`",
              "PSA",
              "`Digital rectal examination`",
              "`Cancer length (per 10mm)`",
              "`Gleason score`")

adjust <- paste(vars_surv, collapse = ' + ')
formula = as.formula(paste0("Surv(time, event) ~ ",
                            adjust,
                            " + strata(`Perineural invasion`)"))

hr_surv <- coxph(formula, data = DTA)
cox.zph(hr_surv)

surv <- survfit(hr_surv, newdata=newdata, censor = TRUE, type = 'kalbfleisch-prentice')

# 3 year survival for PNI and no PNI, at median and mode for adjusting variables.
surv3yr <- summary(surv, times=3)[c("surv", "lower", "upper")]
surv3yr <- lapply(surv3yr, function(x){formatC(x, digits = 2, format = "f")})
surv_ci <- function(x){with(surv3yr, paste0(surv[x], " (", lower[x], "-", upper[x], ")"))}
surv3yr <- lapply(c(unexposed=1, exposed=2), surv_ci)

gg <- ggsurvplot(surv,
                 data=DTA,
                 ylim=c(0.85, 1),
                 risk.table=TRUE,
                 palette = "jco",
                 tables.theme = theme_cleantable(),
                 tables.y.text = FALSE,
                 tables.height = 0.16,
                 legend.title = "Perineural Invasion",
                 legend.labs = c("No", "Yes"),
                 size=0.5)

ggsave(plot = print(gg), filename="../output/Adjusted_survival.png")
