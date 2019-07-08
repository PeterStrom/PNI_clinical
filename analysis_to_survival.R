##############################################################################
# Remove subjects who still have high PSA at first test after RP
##############################################################################
psa_first_high <- DTA %>% 
  group_by(Studieid) %>%
  filter(X_SAMPLE_DATE == min(X_SAMPLE_DATE)) %>%
  filter(X_RESULT >= first_psa_below)

psa_first_high <- unique(psa_first_high$Studieid)

DTA <- DTA[!(DTA$Studieid %in% psa_first_high), ]

tracker <- tracker %>% add_row(text="first PSA low",
                               n=n_distinct(DTA$Studieid))

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
