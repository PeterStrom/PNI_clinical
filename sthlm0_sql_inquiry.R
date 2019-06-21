########################################################################################
##  Load Data & Packages 
########################################################################################
library("RODBC")
library("dplyr")

# Kosmos database (MEB)
dbhandle <- odbcConnect("kosmos")
metadata <- sqlQuery(dbhandle, 'select * from STHLM0.INFORMATION_SCHEMA.TABLES')
print(metadata)

# NPCR-INCA
SCB2_S0_INCA <- sqlQuery(dbhandle, 'select * from STHLM0.STHLM0.SCB2_S0_INCA')
SCB2_S0_INCA <- rename(SCB2_S0_INCA, LOPNR = lopnr)

# Derived data set including emigration & family history of PC
SCB2_PERSON_MAIN <- sqlQuery(dbhandle, 'select * from STHLM0.STHLM0.SCB2_PERSON_MAIN')

# Cause of Death Registry
SOS2_DEAD_CAUSE <- sqlQuery(dbhandle, 'select * from STHLM0.STHLM0.SOS2_DEAD_CAUSE')

# PSA data
SCB2_S0_PSA <- sqlQuery(dbhandle, 'select * from STHLM0.STHLM0.SCB2_S0_PSA')

########################################################################################
## Save Data sets
########################################################################################
write.csv(SCB2_S0_INCA, file = file.path("Desktop", "sthlm0_peter", "SCB2_S0_INCA.csv"))
write.csv(SOS2_DEAD_CAUSE, file = file.path("Desktop", "sthlm0_peter", "SOS2_DEAD_CAUSE.csv"))
write.csv(SCB2_S0_PSA, file = file.path("Desktop", "sthlm0_peter", "SCB2_S0_PSA.csv"))
write.csv(SCB2_PERSON_MAIN, file = file.path("Desktop", "sthlm0_peter", "SCB2_PERSON_MAIN.csv"))

################################## end of program #################################
