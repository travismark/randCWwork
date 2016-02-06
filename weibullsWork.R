# Create some Weibull Output 
# March 1 2013:  moving all my one-off examples from weibulls.R so I can source weibulls.R (calculation functions)
# January 2015: updated to use in the new clockworkETL

library("RODBC", quietly = TRUE)
library("readr", quietly = TRUE)
library("purrr", quietly = TRUE)

user.name <- "root"
pw <- "password"
server <- "localhost"
port <- 3306
db <- "etl_workspace_dev"
conn <- odbcDriverConnect(paste0("DRIVER={MySQL ODBC 5.3 ANSI Driver};Server=",server,";Port",port,";Database=",db,";UID=",user.name,";PWD=",pw))

source("./weibull_functions.R")
#weibullPN_info<-read.csv("./tblWUCinfo.csv",header=TRUE)
#class_names <- read.csv("./class_names.csv",header=TRUE)

# sample_tow <- read.csv('c:/work/cwetl/clockworkETL_upload/sample_tow.csv', stringsAsFactors = FALSE)
# sample_tow_curr <- sqlQuery(conn, 'select * from etl_workspace_dev.sample_tow')
# for(ii in seq_along(1:length(sample_tow))){
#   sample_tow[sample_tow[,ii] %in% "\\N",ii] <- NA
# }
# sample_tow$INSTALL_DT <- as.Date(sample_tow$INSTALL_DT)
# sample_tow$REMOVAL_DT <- as.Date(sample_tow$REMOVAL_DT)
# sample_tow$CONSQ_DT <- as.Date(sample_tow$CONSQ_DT)
# sample_tow$REMOVAL_FCODE <- as.integer(sample_tow$REMOVAL_FCODE)
# sample_tow$CONSQ_COPY <- as.integer(sample_tow$CONSQ_COPY)
# sqlSave(conn, dat = sample_tow, tablename = "sample_tow", rownames = FALSE, append = TRUE)

# Extract Data ------------------------------------------------------------

parameter <- sqlQuery(conn, "SELECT * FROM parameter", stringsAsFactors = FALSE)
parameter_value <- sqlQuery(conn, "SELECT * FROM parameter_value")
interval <- sqlQuery(conn, "SELECT * FROM `interval`") # could add a where tenant_project = some parameter
interval_set <- sqlQuery(conn, "SELECT * FROM interval_parameter_set")
interval_set_map <- sqlQuery(conn, "SELECT * FROM interval_parameter_set_map")

# Transform Data ----------------------------------------------------------

# merge and de-normalize the interval data
#  exclude parameter fields that aren't useful for removal rate calculations
interval_data <- MergeRemovalRateInput(parameter, parameter_value, interval, interval_set_map)

# removal rates for each specific group (requires at least 2 causal intervals, otherwise will not fit distribution)
#   use nested data frames in tidy package and map functions to each nested element with purrr package

# fit some dists and get the interval parameter set map
system.time(output_list <- GatherReliabilityDistributions(interval_data, parameter))
reliability_distribution_save <- bind_rows(output_list[[1]])
interval_parameter_set_map <- bind_rows(output_list[[2]])

# Nest data, then calculate and save distribution
system.time(distribution_weibull_save <- group_by(interval_data, interval_parameter_set_id) %>% 
              nest() %>% # store the data in the data frame as a 1-element list with a dataframe inside
              mutate(dist_fit = purrr::map(data, ~ CalculateOneRemovalDistribution( # map returns a 1-element list of the fit
                       data.frame("interval_value"=.$interval_value, "causal"=.$causal,"weibull")))) )

# Turn important distribution information into fields for the reliability_distribution table
distribution_weibull <- distribution_weibull_save %>% 
  unnest(dist_fit %>% purrr::map(glance.fitdistcens)) %>% # order matters - must use unnest before mutate
  mutate(causal_events = purrr::map_int(data, ~ sum(.$causal==1)), # map_int returns an integer
         censored_events = purrr::map_int(data, ~ sum(.$causal==0))) %>%
  select(-c(data,dist_fit)) # calculate anderson darling here, before dropping the data
distribution_weibull$plot=NA
distribution_weibull$anderson_darling_adjusted=NA

# Calculate distribution parameters (1 row for each parameter)
#   exclude the distributions that did not fit (filter out NAs)
distribution_parameter <- distribution_weibull_save %>% 
  filter(!is.na(dist_fit)) %>% unnest(dist_fit %>% purrr::map(tidy.fitdistcens))

# use a glance-like function to get distribution summaries (dist mean, name, nloglik, etc.)
# use a tidy-like function to get parameter information (one row per distribution parameter: estimate and sd)


# Now Exponentials --------------------------------------------------------

system.time(distribution_exp_save <- group_by(interval_data, interval_parameter_set_id) %>% 
              nest() %>% # store the data in the data frame as a 1-element list with a dataframe inside
              mutate(dist_fit = purrr::map(data, ~ CalculateOneRemovalDistribution( # map returns a 1-element list of the fit
                data.frame("interval_value"=.$interval_value, "causal"=.$causal),"exp"))) )

distribution_exp <- distribution_exp_save %>% 
  unnest(dist_fit %>% purrr::map(glance.fitdistcens)) %>% # order matters - must use unnest before mutate
  mutate(causal_events = purrr::map_int(data, ~ sum(.$causal==1)), # map_int returns an integer
         censored_events = purrr::map_int(data, ~ sum(.$causal==0))) %>%
  select(-c(data,dist_fit)) # calculate anderson darling here, before dropping the data
distribution_exp$plot=NA
distribution_exp$anderson_darling_adjusted=NA

distribution_parameter <- distribution_exp_save %>% 
  filter(!is.na(dist_fit)) %>% unnest(dist_fit %>% purrr::map(tidy.fitdistcens))


# old ---------------------------------------------------------------------


distribution_weibull <- summarise(distribution_weibull_save, 
                                  interval_parameter_set_id = interval_parameter_set_id,
                                  distribution_type_id      = 1, # later use name then get id from join
                                  causal_events             = c_events[[1]], 
                                  censored_events           = s_events[[1]], 
                                  distribution_mean         = dist_fit[1][[1]][2]*gamma(1+1/dist_fit[1][[1]][1]),
                                  anderson_darling_adjusted = NA,
                                  negative_log_likelihood   = dist_fit[5][[1]],
                                  plot                      = NA) %>% 
  add_rownames(var = "id")

distribution_parameter_weibull <- rbind(
  summarise(distribution_weibull_save,
            reliability_distribution_id    = interval_parameter_set_id,
            distribution_type_parameter_id = 1, # later use name (rate), then merge to distribution_type_parameter table to get id
            parameter_value                = dist_fit[1][[1]][2],
            standard_error                 = dist_fit[2][[1]][2]),
  summarise(distribution_weibull_save,
            reliability_distribution_id    = interval_parameter_set_id,
            distribution_type_parameter_id = 2,
            parameter_value                = dist_fit[1][[1]][1],
            standard_error                 = dist_fit[2][[1]][1])
) %>% 
  filter(!is.na(parameter_value)) %>% # remove parameters as NA for distributions that didn't fit
  add_rownames(var = "id")


# Define Standard Distribution Information --------------------------------

## hard code some things for now
distribution_type <- data.frame(id               = c(1, 2), 
                                name             = c("weibull", "exponential"),
                                stringsAsFactors = FALSE)
distribution_type_parameter <- data.frame(id                   = c(1, 2, 3),
                                          distribution_type_id = c(1, 1, 2),
                                          parameter_number     = c(1, 2, 1),
                                          name                 = c("scale", "shape", "mean"),
                                          stringsAsFactors     = FALSE)


# Replace Names with IDs for Database and Adjust Names -------------------------------------

# Add Distribution Type to distribution
#   left join because some distributions did not fit and have no distribution type
#   add row names as id
distribution_weibull <- left_join(distribution_weibull, distribution_type, by=c("distribution_type" = "name")) %>%
  rename(distribution_type_id = id) %>% add_rownames(var = "id") %>% select(-distribution_type)
# Add Distribution Type Parameter to Distribution Parameter
distribution_parameter <- inner_join(distribution_parameter, 
                                     select(distribution_type_parameter, id, name),
                                            by=c("parameter_name" = "name")) %>% 
  rename(distribution_type_parameter_id = id, reliability_distribution_id = interval_parameter_set_id) %>% 
  add_rownames(var = "id") %>% select(-c(parameter_name))

# Load data to Database ---------------------------------------------------

sqlSave(conn, dat = distribution_type, tablename = "distribution_type", rownames = FALSE, append = TRUE)
sqlSave(conn, dat = distribution_type_parameter, tablename = "distribution_type_parameter", rownames = FALSE, append = TRUE)
sqlSave(conn, dat = distribution_weibull, tablename = "reliability_distribution", rownames = FALSE, append = TRUE)
sqlSave(conn, dat = distribution_parameter, tablename = "reliability_distribution_parameter", rownames = FALSE, append = TRUE)


# all specific weibulls
distribution_expo_save <- group_by(interval, interval_parameter_set_id) %>% 
  do(c_events = sum(.$causal==1), s_events = sum(.$causal==0),
     dist_fit = CalculateOneRemovalDistribution(data.frame("interval_value"=.$interval_value,
                                                           "causal"=.$causal,"expo")))
distribution_expo <- summarise(distribution_weibull_save, interval_parameter_set_id = interval_parameter_set_id, 
                               causal_events = c_events[[1]], censored_events = s_events[[1]],
                               p1 = dist_fit[1][[1]][1],
                               p2 = dist_fit[1][[1]][2],
                               distribution_mean = dist_fit[1][[1]][1],
                               negative_log_likelihood = dist_fit[5][[1]])

# calculate all weibulls
system.time(allweibulls <- gatherallweibulls(weibulls_initial,reliability_parameter,verbose=TRUE,unbug=FALSE,
                                            plot=TRUE,modkm=TRUE,plotdir="./plots/"))
write.csv(allweibulls,file="Apache Weibulls.csv",quote=FALSE)
# perform distribution comparison tests
source("./stattest_functions.R")
system.time(alltests <- testallweibulls(allweibulls,weibulls_initial,reliability_parameter,
                                      verbose=TRUE,doAllGroups=TRUE))
write.csv(alltests,file="Apache Tests.csv",quote=FALSE)

# calculate consequences
source("./consequence_functions.R")
conseq_initial <- mergeConsqInput(reliability_parameter,reliability_interval,reliability_interval_parameter)

# build optimization constraint matrix
constraint_matrix <- matchDistToInterval(allweibulls,weibulls_initial,reliability_parameter)
write.csv(constraint_matrix, file="Constraint Matrix.csv",quote = FALSE, row.names = TRUE)
