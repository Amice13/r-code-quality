#####################################
### create predictors ##
#####################################
library(dplyr)

######## partisan segregation ###########

# read vote share data 

voteshare <- read.csv("countypres_2000-2016.csv",
                      colClasses=c('numeric', 
                                   rep('character', 7),
                                   rep('numeric', 3)))

# subset the data to only include vote share from 2016
voteshare2016 <- voteshare %>% filter(year == 2016)

# inspect NAs
voteshare2016[is.na(voteshare2016$state_po),]
voteshare2016[is.na(voteshare2016$FIPS),]
voteshare2016[is.na(voteshare2016$candidatevotes),]

# there are some missing values in these three columns
# in the calculation process, we will remove any row in which state_po or county FIPS is missing
voteshare2016 <- voteshare2016[-c(8662:8664),]

# calculate White's isolation index
# exclude any row in which there is either no state abbreviation or no county FIPS

# number of Clinton voters in a county
num_clinton_voters_county <- voteshare2016 %>%
  filter(!is.na(state_po) & !is.na(FIPS)) %>%
  group_by(state_po, FIPS) %>%
  filter(candidate == "Hillary Clinton") %>%
  summarise(n_iC = candidatevotes) 

# number of Trump voters in a county
num_trump_voters_county <- voteshare2016 %>%
  filter(!is.na(state_po) & !is.na(FIPS)) %>%
  group_by(state_po, FIPS) %>%
  filter(candidate == "Donald Trump") %>%
  summarise(n_iT = candidatevotes) 

# number of Clinton voters in a state
num_clinton_voters_state <- voteshare2016 %>%
  filter(!is.na(state_po) & !is.na(FIPS)) %>%
  group_by(state_po) %>%
  filter(candidate == "Hillary Clinton") %>%
  summarise(N_C = sum(candidatevotes, na.rm = T))

# number of Trump voters in a state
num_trump_voters_state <- voteshare2016 %>%
  filter(!is.na(state_po) & !is.na(FIPS)) %>%
  group_by(state_po) %>%
  filter(candidate == "Donald Trump") %>%
  summarise(N_T = sum(candidatevotes, na.rm = T))

# merge the above indices
# for the county-level measures, merge by state and county FIPS
state_isolation <- left_join(num_clinton_voters_county, num_trump_voters_county,
                             by = c("state_po" = "state_po", 
                                    "FIPS" = "FIPS"))
# for the state-level measures, merge by state
state_isolation <- left_join(state_isolation,num_clinton_voters_state,
                             by = c("state_po" = "state_po"))
state_isolation <- left_join(state_isolation,num_trump_voters_state,
                             by = c("state_po" = "state_po"))

# total number of voters voted for the two major political parties
state_isolation$n_i <- state_isolation$n_iC + state_isolation$n_iT

# create the segregation index
state_isolation_index <- state_isolation %>%
  group_by(state_po) %>%
  summarise(b_ct = sum((n_iC/N_C)*(n_iT/n_i)), # Clinton voters' probability of interaction with Trump voters
            b_tc = sum((n_iT/N_T)*(n_iC/n_i)), # Trump voters' probability of interaction with Clinton voters
            b_tt = sum((n_iT/N_T)*(n_iT/n_i)), 
            b_cc = sum((n_iC/N_C)*(n_iC/n_i))) 

# create a clean list of states
states <- voteshare2016 %>%
  filter(!is.na(state_po)) %>%
  select(state,state_po) %>%
  distinct()

# merge them together so that the final dataset include both full names and abbreviations of states
state_isolation_index <- left_join(state_isolation_index, states,
                                   by = "state_po")

# reverse the scales so that a higher values means more segregation
state_isolation_index$b_ct <- 1-state_isolation_index$b_ct
state_isolation_index$b_tc <- 1-state_isolation_index$b_tc
