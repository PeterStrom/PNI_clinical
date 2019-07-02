load("../data/analysis_data.RData")

##############################################################################
# Remove subjects who still have high PSA at first test after RP
##############################################################################
psa_first_high <- DTA %>% 
  group_by(Studieid) %>%
  filter(X_SAMPLE_DATE == min(X_SAMPLE_DATE)) %>%
  filter(X_RESULT >= first_psa_below)

psa_first_high <- unique(psa_first_high$Studieid)

DTA <- DTA[!(DTA$Studieid %in% psa_first_high), ]

##############################################################################
# Define event.
##############################################################################
psa_relapse <- DTA %>% 
  group_by(Studieid) %>%
  filter(X_RESULT >= event_threshold) %>%
  filter(X_SAMPLE_DATE == min(X_SAMPLE_DATE)) %>%
  select(c(Studieid, X_SAMPLE_DATE, X_RESULT)) %>%
  rename(psa_relapse_date=X_SAMPLE_DATE, psa_relapse_value=X_RESULT)

DTA <- left_join(x=DTA, y=psa_relapse, by="Studieid")

# Keep one row per subject.
DTA$X_RESULT <- NULL
DTA <- DTA %>%
  group_by(Studieid) %>%
  filter(row_number() == 1)

##############################################################################
# Create time and censor/event variables.
##############################################################################
DTA <- transform(DTA,
                 exit_date = pmin(psa_relapse_date,
                                  emidate_sthlm,
                                  X_DODSDAT,
                                  end_of_study, na.rm = TRUE))

date_names <- c("psa_relapse_date",
                "emidate_sthlm",
                "X_DODSDAT")

DTA$exit_cause <-"End of study"

for (i in date_names){
  DTA$exit_cause <- ifelse((DTA$exit_date == DTA[[i]]) & !is.na(DTA[i]),
                           i,
                           DTA$exit_cause)
}
attributes(DTA$exit_cause) <- NULL
# Inspect that dates and causes are correct.
# select(DTA, c(psa_relapse_date,
#               emidate_sthlm,
#               X_DODSDAT,
#               end_of_study,
#               exit_date,
#               exit_cause))

##############################################################################
# Lexis diagram.
##############################################################################
pniL <- Lexis( entry = list(cy=cal.yr(DTA$Op_datum)-cal.yr(DTA$Op_datum),
                            age=Inclusion_age),
               exit = list( cy=cal.yr(DTA$exit_date)-cal.yr(DTA$Op_datum)),
               exit.status = (exit_cause == "psa_relapse_date")*1,
               data = DTA )

pdf("../output/Lexis.pdf")
# col=c("blue","red")[(pniL$pni_n_slides>0)+1]
plot( pniL, 1:2, lwd=1, col="grey",
      grid=TRUE, lty.grid=1, col.grid=gray(0.7),
      xlim=0+c(0,5.3), xaxs="i",
      ylim=49+c(0,26), yaxs="i", las=1,
      xlab="Years since operation",
      ylab="Age at operation",
      main="Lexis diagram of time from RP to PSA relapse")
points( pniL, 1:2, pch=c(NA,3)[pniL$lex.Xst+1],
        col=c("blue","red")[(pniL$pni_n_slides>0)+1], lwd=1.5, cex=1.5 )
dev.off()


pniL <- Lexis( entry = list(cy=cal.yr(DTA$Op_datum),
                            age=Inclusion_age),
               exit = list( cy=cal.yr(DTA$exit_date)),
               exit.status = (exit_cause == "psa_relapse_date")*1,
               data = DTA )

pdf("../output/Lexis_calender.pdf")
# col=c("blue","red")[(pniL$pni_n_slides>0)+1]
plot( pniL, 1:2, lwd=1, col="grey",
      grid=TRUE, lty.grid=1, col.grid=gray(0.7),
      xlim=2012.8+c(0,5.5), xaxs="i",
      ylim=49+c(0,26), yaxs="i", las=1,
      xlab="Calender year of operation",
      ylab="Age at operation",
      main="Lexis diagram of time from RP to PSA relapse")
points( pniL, 1:2, pch=c(NA,3)[pniL$lex.Xst+1],
        col=c("blue","red")[(pniL$pni_n_slides>0)+1], lwd=1.5, cex=1.5 )
dev.off()

