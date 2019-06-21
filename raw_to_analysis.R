data_path <- "../data/raw_data"
SCB2_PERS <- read_csv(file.path(data_path, 'sthlm0', 'SCB2_PERSON_MAIN.csv'))
SCB2_INCA <- read_csv(file.path(data_path, 'sthlm0', 'SCB2_S0_INCA.csv'))
SCB2_PSA_ <- read_csv(file.path(data_path, 'sthlm0', 'SCB2_S0_PSA.csv'))
SOS2_DEAD <- read_csv(file.path(data_path, 'sthlm0', 'SOS2_DEAD_CAUSE.csv'))
head(SCB2_PERS)
