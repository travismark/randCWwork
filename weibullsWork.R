# Create some Weibull Output 
# March 1 2013:  moving all my one-off examples from weibulls.R so I can source weibulls.R (calculation functions)
# January 2015: updated to use in the new clockworkETL

# consider the lifelines python library
# TODO: if a certain parameter field/type only has one parameter value the code calculates duplicate distributions - 
#   # one aggregating, and one not.  but these use the same data.  check for this and don't fit distributions if so

library("RODBC", quietly = TRUE)

user_name <- "root"
pw        <- "password"
server    <- "localhost"
port      <- 3306
db        <- "etl_workspace_dev"
conn <- odbcDriverConnect(paste0("DRIVER={MySQL ODBC 5.3 ANSI Driver};Server=",server,";Port",port,";Database=",db,";UID=",user_name,";PWD=",pw))
rm(list = c("user_name", "pw", "server", "port", "db"))

source("./weibull_functions.R")
source("./conseq_functions.R")

# Extract Data ------------------------------------------------------------
print("Extracting Data")

parameter       <- sqlQuery(conn, "SELECT * FROM parameter", stringsAsFactors = FALSE)
parameter_value <- sqlQuery(conn, "SELECT * FROM parameter_value")
interval        <- sqlQuery(conn, "SELECT * FROM `interval`") # could add a where tenant_project = some parameter
parameter_names <- sqlQuery(conn, "SELECT * FROM parameter_value", stringsAsFactors = FALSE)
interval$interval_start_cy <- year(interval$interval_start_date)

# Before loading interval_set and interval_set_map delete any data that is not tied to an interval
sqlQuery(conn, "DELETE FROM interval_parameter_set_map 
         WHERE interval_parameter_set_id IN 
         (SELECT id FROM interval_parameter_set WHERE id NOT IN (SELECT interval_parameter_set_id FROM `interval`))")
sqlQuery(conn, "DELETE FROM interval_parameter_set WHERE id NOT IN (SELECT interval_parameter_set_id FROM `interval`)")

interval_set <- sqlQuery(conn, "SELECT * FROM interval_parameter_set")
interval_set_map <- sqlQuery(conn, "SELECT * FROM interval_parameter_set_map")

# Transform Data: first Calc Dists -------------------------------------------
print("Transforming Data")

# Define Standard Distribution Information
#  Hard code for now
distribution_type <- data.frame(id               = c(1, 2), 
                                name             = c("weibull", "exponential"),
                                stringsAsFactors = FALSE)
distribution_type_parameter <- data.frame(id                   = c(1, 2, 3),
                                          distribution_type_id = c(1, 1, 2),
                                          parameter_number     = c(1, 2, 1),
                                          name                 = c("scale", "shape", "mean"),
                                          stringsAsFactors     = FALSE)

# options for user
modkm    <- TRUE
plots    <- TRUE
plot_dir <- "./plots/"
min_interval_time <- 1

# exchange ids for parameter names in parameter_names
parameter_names <- inner_join(parameter_names,select(parameter,id,plot_name),
                              by = c("parameter_id" = "id")) %>% 
  select(parameter_value_id = id, -parameter_id, value, parameter_name = plot_name)

# Merge and de-normalize the interval data
#  exclude parameter fields that aren't useful for removal rate calculations
interval_data <- MergeRemovalRateInput(parameter, parameter_value, interval, interval_set_map)

# Removal rates for each specific group (requires at least 2 causal intervals, otherwise will not fit distribution)
#   use nested data frames in tidy package and map functions to each nested element with purrr package

# Fit distributions and get the interval parameter set map
system.time(output_list <- GatherReliabilityDistributions(interval_data, parameter, min_interval_time=min_interval_time, verbose=TRUE)) # 9+ seconds
reliability_distribution_save <- bind_rows(output_list[[1]])
interval_parameter_set_map    <- bind_rows(output_list[[2]])
rm(output_list)

# Use the previously-defined interval_parameter_set_ids for the last (all-parameters-specified) distributions 
#  and adjust the rest by this quantity.  Remove those from the interval_parameter_set_map table b/c already defined
#  the interval_parameter_set_map table has set_ids with sets going from most-general to most-specific
# 1: increment dist table
reliability_distribution_save$interval_parameter_set_id <-  
  reliability_distribution_save$interval_parameter_set_id + max(interval_set$id)
first_all_specific_interval <- max(reliability_distribution_save$interval_parameter_set_id)-max(interval_set)+1

# 2: set tail end of table to prev-defined set_ids
reliability_distribution_save[
  reliability_distribution_save$interval_parameter_set_id >= 
    first_all_specific_interval,]$interval_parameter_set_id <- seq(1,max(interval_set))

# 3: reorder distribution table by set_id
reliability_distribution_save <- arrange(reliability_distribution_save, interval_parameter_set_id)

# 4: reset the interval_parameter_set_ids so the already-defined ids match to the current ids
#first_all_specific_interval <- max(interval_parameter_set_map$interval_parameter_set_id)-max(interval_set)+1
new_index_table <- data.frame("old_id"=seq(from = 1, to = max(interval_parameter_set_map$interval_parameter_set_id)),
                              "new_id"=c(seq(from = max(interval_set) + 1, to = max(interval_parameter_set_map$interval_parameter_set_id)),
                                         seq(from = 1, to = max(interval_set))))
interval_parameter_set_map <- inner_join(interval_parameter_set_map,new_index_table,
                                         by = c("interval_parameter_set_id" = "old_id")) %>%
  select(-interval_parameter_set_id, interval_parameter_set_id = new_id, parameter_value_id)
# save these for reference in plotting
interval_parameter_set_map_ref <- interval_parameter_set_map 

# 5: remove prev-defined set_ids from set_map table (those at the start)
interval_parameter_set_map <- 
  interval_parameter_set_map[interval_parameter_set_map$interval_parameter_set_id > max(interval_set),]

# 6: add id and adjust to de-conflict with previous ids
interval_parameter_set_map    <- add_rownames(interval_parameter_set_map, var = "id")
interval_parameter_set_map$id <- as.integer(interval_parameter_set_map$id) + max(interval_set_map$id)
# make the interval set table (only includes those that do not conflict)
interval_parameter_set <- data.frame("id"=unique(interval_parameter_set_map$interval_parameter_set_id))


# use a glance-like function to get distribution summaries (dist mean, name, nloglik, etc.)
# use a tidy-like function to get parameter information (one row per distribution parameter: estimate and sd)

print("Calculating Kaplan Meier and Anderson Darling; Plotting")
# Build the parameter value name reference table for plotting and attach to baseline distribution table
interval_parameter_set_map_ref <- inner_join(interval_parameter_set_map_ref,parameter_names,
                                             by="parameter_value_id") %>% select(-parameter_value_id)

reliability_distribution_save <- mutate(reliability_distribution_save, parameter_names = 
                                          purrr::map(interval_parameter_set_id, ~ GetMatchingParameterNames(., interval_parameter_set_map_ref)))
# Build the distribution tables and plots
#  weibull
#  order matters - must use unnest before mutate # 2/11/2016 - 36 seconds for weibull; 2/12: 5 seconds; 2/18 35 seconds with plots
system.time(distribution_weibull <- reliability_distribution_save %>%
              unnest(dist_fit_w %>% purrr::map2(parameter_names, ~ 
                                                  glance.survreg_1(.x, .y, modkm, plots, plot_dir))) %>%
  mutate(causal_events = purrr::map_int(data, ~ sum(.$causal==1 & .$interval_value > 0)), # map_int returns an integer
         censored_events = purrr::map_int(data, ~ sum(.$causal==0 & .$interval_value > 0)),
         plot = NA) %>%
  dplyr::select(-c(data, dist_fit_w, dist_fit_e, parameter_names)))

# exponential
distribution_exp <- reliability_distribution_save %>%
  unnest(dist_fit_e %>% purrr::map2(parameter_names, ~ 
                                      glance.survreg_1(.x, .y, modkm, plots, plot_dir))) %>%
  mutate(causal_events = purrr::map_int(data, ~ sum(.$causal==1 & .$interval_value > 0)), # map_int returns an integer
         censored_events = purrr::map_int(data, ~ sum(.$causal==0 & .$interval_value > 0)),
         plot = NA) %>%
  dplyr::select(-c(data, dist_fit_w, dist_fit_e))

# Combine into a single table, replace name with distribution_type_id, and add row ids
reliability_distribution <- bind_rows(distribution_weibull,distribution_exp) %>% 
  left_join(distribution_type, by=c("distribution_type" = "name")) %>%
  rename(distribution_type_id = id) %>% select(-distribution_type, -parameter_names) %>% add_rownames(var = "id")

# Calculate distribution parameters (1 row for each parameter)
#   exclude the distributions that did not fit (filter out NAs)
distribution_parameter_weibull <- reliability_distribution_save %>% 
  filter(!is.na(dist_fit_w)) %>% unnest(dist_fit_w %>% purrr::map(tidy.survreg_1))
distribution_parameter_exp <- reliability_distribution_save %>% 
  filter(!is.na(dist_fit_e)) %>% unnest(dist_fit_e %>% purrr::map(tidy.survreg_1)) %>%
  select(-c(data, dist_fit_w, dist_fit_e, parameter_names)) # since the tidy function returns a 1-row df the unnest doesn't drop the rest of these

# Combine into a single table and:
#  attach the distribution_type_id and distribution_type_parameter_id by joining to interval_parameter_set_id and distribution_type_id
#  attach the distribution_id by matching on distribution_type_id
distribution_id_info <- rename(distribution_type, distribution_type_id = id, distribution_type_name = name) %>% 
  inner_join(distribution_type_parameter, by = "distribution_type_id") %>% 
  rename(parameter_name = name, distribution_type_parameter_id = id) %>% select(-parameter_number)

distribution_parameter <- bind_rows(distribution_parameter_weibull, distribution_parameter_exp) %>%
  inner_join(distribution_id_info, by = "parameter_name") %>%
  inner_join(select(reliability_distribution,distribution_type_id, interval_parameter_set_id, id), 
             by = c("distribution_type_id", "interval_parameter_set_id")) %>%
  rename(reliability_distribution_id = id) %>% 
  select(-c(interval_parameter_set_id, parameter_name, 
            distribution_type_name, distribution_type_id)) %>%
  add_rownames(var = "id")


# Consequences ------------------------------------------------------------

# hard-code probability type and class to match LCM
probability_class <- data.frame("id" = c(1, 2, 3), 
                                "name" = c("unscheduled removal", "life limit", 
                                           "initial unserviceable"), stringsAsFactors = FALSE)
probability_type <- data.frame("id" = c(1, 2, 3, 4), 
                                "name" = c("no action", "no fault found", 
                                           "condemn", "repair"), stringsAsFactors = FALSE) # matches demand pro sequence

# already have the proper interval_parameter_set_ids and maps from distribution calculation. use the same ones for consequence and nrts calculations.
# ignore the interval_parameter_set_ids that include parameters that are incompatible with consequences
conseq_save <- select(reliability_distribution_save, -dist_fit_w, -dist_fit_e) %>% 
  mutate(conseq_include = purrr::map_lgl(parameter_names, ~ !any(.$parameter_name %in% parameter[parameter$consequence == 0, ]$plot_name))) %>%
  filter(conseq_include) %>% select(-conseq_include)

# merge in data relevant to consequences
interval_data_conseq <- MergeConsequenceInput(parameter, parameter_value, interval, interval_set_map)
conseq_save <- mutate(conseq_save, data = purrr::map(data, ~ select(., interval_id) 
                                                     %>% inner_join(interval_data_conseq, by = "interval_id")))

# calculate consequences by:
  # location type (2 or 3) (informed by nrts field)
  # removal type (unscheduled or life limit)
  # interval parameter set id
  # consequence (repair or condemn)

consequence <- unnest(conseq_save, data %>% purrr::map(tidy.consequence)) %>%
  inner_join(probability_type, by = c("probability_type_name" = "name")) %>% # dropped that NA consequence
  rename(probability_type_id = id) %>% select(-probability_type_name) %>%
  #inner_join(probability_class, by = c("probability_class_name" = "name")) # TODO: add this back once interval type isn't NA
  rename(probability_class_id = probability_class_name) %>% # change this to mirror probability type id when interval type isn't NA
  add_rownames(var = "id")

consequence$probability_class_id <- 1 ### I have to make something up because this field isn't nullable.
  
rm(distribution_id_info)
rm(reliability_distribution_save)
rm(conseq_save)

# Load -------------------------------------------------
print("Loading Data")

sqlSave(conn, dat = distribution_type, tablename = "distribution_type", rownames = FALSE, append = TRUE)
sqlSave(conn, dat = distribution_type_parameter, tablename = "distribution_type_parameter", rownames = FALSE, append = TRUE)
sqlSave(conn, dat = interval_parameter_set, tablename = "interval_parameter_set", rownames = FALSE, append = TRUE)
sqlSave(conn, dat = interval_parameter_set_map, tablename = "interval_parameter_set_map", rownames = FALSE, append = TRUE)
sqlSave(conn, dat = reliability_distribution, tablename = "reliability_distribution", rownames = FALSE, append = TRUE)
sqlSave(conn, dat = distribution_parameter, tablename = "reliability_distribution_parameter", rownames = FALSE, append = TRUE)
sqlSave(conn, dat = probability_class, tablename = "probability_class", rownames = FALSE, append = TRUE)
sqlSave(conn, dat = probability_type, tablename = "probability_type", rownames = FALSE, append = TRUE)
sqlSave(conn, dat = consequence, tablename = "consequence", rownames = FALSE, append = TRUE)

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

# https://support.sas.com/documentation/cdl/en/statug/63033/HTML/default/viewer.htm#statug_lifereg_sect023.htm#