library(did) # Callaway & Sant'Anna
library(tidyverse)
library(haven) #Import foreign statistical formats into R via the embedded 'ReadStat' C library

updatedcounty_data <- read_dta("UpdatedCountyCrimeData-2010.dta")

# variables: crimerate, shalll, treat_year, year, id for units, groupname, notyettreated nevertreated

x <- updatedcounty_data %>%  #  97,493
  filter(shalll == 1) %>% 
  group_by(fipsid) %>% 
  filter(row_number() == 1) %>% 
  mutate(treat_year = year) %>%  # create variable: treat-year
  select(fipsid, treat_year) %>% 
  ungroup()

county_data <- updatedcounty_data %>% #link to x to create treat_year variable, 97,493
  left_join(x) %>% 
  mutate(treat_year = if_else(is.na(treat_year), 0, treat_year) # treat year is 0 if never treated
  )

# what is fisid?
county_data %>%  # 3282 cross-sectional counties?
  group_by(fipsid) %>% 
  summarise(n = n())

# balanced?
county_data %>%  # 3282 cross-sectional counties?
  group_by(fipsid) %>% 
  summarise(n = n()) %>% 
  filter(n != 30)

# not balanced, how?
#
# This is the unbalanced counties table
# A tibble: 42 x 2
# fipsid     n
# <dbl> <int>
#   1   2030     3
# 2   2040     3
# 3   2060   103
# 4   2080     3
# 5   2120     3
# 6   2140    11
# 7   2160     4
# 8   2190     3
# 9   2200     4
# 10   2210     3
# # ... with 32 more rows

# check 2060
county_data %>% 
  filter(fipsid == 2060) %>% 
  group_by(year) %>% 
  summarise(n = n())
# 30 years of data, some duplicate rows.


# clean raw data by removing duplicate rows.
county_data1 <- distinct(county_data) # 97,420, remove 73 rows.

# balanced?
county_data1 %>%  
  group_by(fipsid) %>% 
  summarise(n = n()) %>% 
  filter(n != 30) 
# not balanced, 41 counties have less than 30 years of data
# for "balanced" counties, they have missing data for some of the variables.
# be cautious about "unbalanced" issue in the final analysis.
###################################################################
###################################################################

# Estimating the effect on y2 (constant) and y (dynamic treatment)
atts <- att_gt(yname = "lratmur", # LHS variable
               tname = "year", # time variable
               idname = "fipsid", # id variable
               gname = "treat_year", # first treatment period variable
               data = county_data1, # data
               xformla = NULL, # no covariates
               #xformla = ~ l_police, # with covariates
               est_method = "dr", # "dr" is doubly robust. "ipw" is inverse probability weighting. "reg" is regression
               control_group = "nevertreated", # set the comparison group which is either "nevertreated" or "notyettreated" 
               bstrap = TRUE, # if TRUE compute bootstrapped SE
               biters = 100, # number of bootstrap iterations
               print_details = FALSE, # if TRUE, print detailed results
               clustervars = "stnumber", # cluster level
               panel = TRUE) # whether the data is panel or repeated cross-sectional


# Aggregate ATT
agg_effects <- aggte(atts, type = "group", balance_e=TRUE)
summary(agg_effects)

# Group-time ATTs
summary(atts)

# Plot group-time ATTs
ggdid(atts)

# Event-study
agg_effects_es <- aggte(atts, type = "dynamic")
summary(agg_effects_es)

# Plot event-study coefficients
ggdid(agg_effects_es)



# Change control to notyettreated --------------------------------------------------

# Estimating the effect on y2 (constant) and y (dynamic treatment)
atts <- att_gt(yname = "lratmur", # LHS variable
               tname = "year", # time variable
               idname = "fipsid", # id variable
               gname = "treat_year", # first treatment period variable
               data = county_data1, # data
               xformla = NULL, # no covariates
               #xformla = ~ l_police, # with covariates
               est_method = "dr", # "dr" is doubly robust. "ipw" is inverse probability weighting. "reg" is regression
               control_group = "notyettreated", # set the comparison group which is either "nevertreated" or "notyettreated" 
               bstrap = TRUE, # if TRUE compute bootstrapped SE
               biters = 100, # number of bootstrap iterations
               print_details = FALSE, # if TRUE, print detailed results
               clustervars = "stnumber", # cluster level
               panel = TRUE) # whether the data is panel or repeated cross-sectional


# Aggregate ATT
agg_effects <- aggte(atts, type = "group", balance_e=TRUE)
summary(agg_effects)

# Group-time ATTs
summary(atts)

# Plot group-time ATTs
ggdid(atts)

# Event-study
agg_effects_es <- aggte(atts, type = "dynamic")
summary(agg_effects_es)

# Plot event-study coefficients
ggdid(agg_effects_es)

# Use covariates ----------------------------------------------------------

xformla = as.formula(~ pwf65o + ppwm5064 + ppwf2029 + ppbm65o + ppbf5064)

# Estimating the effect on y2 (constant) and y (dynamic treatment)
atts <- att_gt(yname = "lratmur", # LHS variable
               tname = "year", # time variable
               idname = "fipsid", # id variable
               gname = "treat_year", # first treatment period variable
               data = county_data1, # data
               xformla = xformla , # no covariates
               #xformla = ~ l_police, # with covariates
               est_method = "dr", # "dr" is doubly robust. "ipw" is inverse probability weighting. "reg" is regression
               control_group = "nevertreated", # set the comparison group which is either "nevertreated" or "notyettreated" 
               bstrap = TRUE, # if TRUE compute bootstrapped SE
               biters = 100, # number of bootstrap iterations
               print_details = FALSE, # if TRUE, print detailed results
               clustervars = "stnumber", # cluster level
               panel = TRUE) # whether the data is panel or repeated cross-sectional


# Aggregate ATT
agg_effects <- aggte(atts, type = "group", balance_e=TRUE)
summary(agg_effects)

# Group-time ATTs
summary(atts)

# Plot group-time ATTs
# ggdid(atts)

# Event-study
agg_effects_es <- aggte(atts, type = "dynamic")
summary(agg_effects_es)

# Plot event-study coefficients
ggdid(agg_effects_es)



# Change control to notyettreated --------------------------------------------------

# Estimating the effect on y2 (constant) and y (dynamic treatment)
atts <- att_gt(yname = "lratmur", # LHS variable
               tname = "year", # time variable
               idname = "fipsid", # id variable
               gname = "treat_year", # first treatment period variable
               data = county_data1, # data
               xformla = xformla , # no covariates
               #xformla = ~ l_police, # with covariates
               est_method = "dr", # "dr" is doubly robust. "ipw" is inverse probability weighting. "reg" is regression
               control_group = "notyettreated", # set the comparison group which is either "nevertreated" or "notyettreated" 
               bstrap = TRUE, # if TRUE compute bootstrapped SE
               biters = 100, # number of bootstrap iterations
               print_details = FALSE, # if TRUE, print detailed results
               clustervars = "stnumber", # cluster level
               panel = TRUE) # whether the data is panel or repeated cross-sectional


# Aggregate ATT
agg_effects <- aggte(atts, type = "group", balance_e=TRUE)
summary(agg_effects)

# Group-time ATTs
summary(atts)

# Plot group-time ATTs
# ggdid(atts)

# Event-study
agg_effects_es <- aggte(atts, type = "dynamic")
summary(agg_effects_es)

# Plot event-study coefficients
ggdid(agg_effects_es)
