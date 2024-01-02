#######################################################
# Example for multidimensional cube queries to survstat
#######################################################

#---------------------------------------------
# Preparation

# Import querying functionality
source("survstat_webservice_multidim.r")

# Load precomputed hierarchy for regional dimension (countries and states)
# We need this because the crawler will only return the most detailed level
# of each dimension (the county in this case), so we will join the state
# information in the end.
county_state = readRDS("county_state.Rds")

#---------------------------------------------
# Exploration

# Get overview over all potential hierarchies
all_hierarchies <- getHierarchies("SurvStat", "English")

# Get overview over all available diseases
all_diseases = get_diseases()

# Get all strata
# (cartesian product of all values of the dimensions provided in the list)
strat = get_strata("SurvStat", "English",
  list("Year of notification","Sex","Age stratification: children coarse"))

# How many strata do we have, and thus how many calls to survstat will we need?
nrow(strat)

#---------------------------------------------
# Crawling

# Get the weekly time series stratified by the strata as given in a dataframe
weekly_timeseries = get_weekly_timeseries_stratified(
  disease = "Rotavirus gastroenteritis",
  save_path="rota_temp.Rds", # path for temporal save (quering takes long, so backups are sensible)
  strata_frame=strat, # all the strata that we want to query
  save_every=50 # after 50 queries, save the result temporarily
)

# Finally, join the hierarchy information so we also have a state column
weekly_timeseries %<>% left_join(county_state)

# Save the data
saveRDS(weekly_timeseries,"Rotavirus.Rds")
write.csv(weekly_timeseries,"Rotavirus.csv",row.names = FALSE)

#---------------------------------------------
# Analysis

# Aggregate and plot epicurve
counts = weekly_timeseries %>% group_by(Year,Week) %>% summarize(Count=sum(Count,na.rm = T))
plot(counts$Count,type="l")

# ...