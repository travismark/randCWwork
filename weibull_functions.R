### Fit many distributions

# Load packages

suppressPackageStartupMessages(library("survival", quietly = TRUE))
suppressPackageStartupMessages(library("fitdistrplus", quietly = TRUE))
suppressPackageStartupMessages(library("plyr", quietly = TRUE))
suppressPackageStartupMessages(library("dplyr", quietly = TRUE))
suppressPackageStartupMessages(library("tidyr", quietly = TRUE))
suppressPackageStartupMessages(library("readr", quietly = TRUE))
suppressPackageStartupMessages(library("purrr", quietly = TRUE))
suppressPackageStartupMessages(library("lubridate", quietly = TRUE))

# Define Functions: 

MergeRemovalRateInput <- function(parameter, parameter_value, interval, interval_set_map) {
  # Converts Parameter Information (set, set_map, parameter, set_parameter) and Interval tables into 
  #   a single table for use in gatherAllWeibulls()
  #   Drops consequence and NRTS information
  #   Keeps parameter ids - no need to use names (ignores parameter_value table)
  # Args:
  #   parameter: WUC, PN, etc.
  #   parameter_value: specific values of those parameters
  #   interval: age intervals with consequence
  #   interval_set_map: connects sets of parameter values to their intervals (many to many, so intervals can share sets)
  # Returns:
  #   interval_parameter_value: a dataframe with parameter values (ids) and interval information
  
  ## Make sure all interval data have the same number of parameter values
  if (length(unique(table(interval_set_map$interval_parameter_set_id))) > 1) {
    stop(paste("Analysis requires the same quantity of parameter values for each interval. Currently intervals have:",
               paste(unique(table(interval_set_map$interval_parameter_set_id)), collapse=", ")))
  }
  
  ## Drop unnecessary information
  interval_set_map$id          <- NULL
  interval$consequence         <- NULL
  interval$nrts                <- NULL
  interval$interval_type       <- NULL
  interval$interval_start_date <- NULL
  interval$interval_end_date   <- NULL
  interval$tenant_project_id   <- NULL
  interval$interval_start_cy   <- NULL
  # Drop parameters that aren't possible for calculating weibulls
  parameter <- parameter[parameter$removal_rate==1,]
  # Then the remaining fields
  parameter$plot_name      <- NULL
  parameter$removal_rate   <- NULL
  parameter$consequence    <- NULL
  parameter$nrts           <- NULL
  parameter$do_not_combine <- NULL
  parameter_value$value    <- NULL
  
  ## Merge three tables together to make one
  interval_parameter_value <- inner_join(interval_set_map, parameter_value,
                                        by=c("parameter_value_id" = "id")) %>% 
    inner_join(parameter, by=c("parameter_id" = "id")) %>% 
    inner_join(interval, by=("interval_parameter_set_id"))

  # De-normalize interval_parameter_value table and use parameter names as field names
  interval_parameter_value <- select(interval_parameter_value, -parameter_id) %>% # drop parameter id
    spread(key=name, value=parameter_value_id) %>% # denormalize
    select(-id) # drop interval id (used to distinguish intervals in previous call)
  interval_parameter_value <- interval_parameter_value[,c( # move interval value and causal to the end
    colnames(interval_parameter_value)[!(colnames(interval_parameter_value) %in% c("interval_value", "causal"))],
    c("interval_value", "causal"))]
  
  return(interval_parameter_value)
} # end MergeRemovalRateInput

CalculateOneRemovalDistribution <- function(df, distribution_type="weibull"){
  # Returns a distribution fit, or NA if not enough data
  # Args:
  #  df: subset of the de-normalized interval dataframe - only interval_value and causal fields
  #  distribution_type: string - either exponential or weibull
  # Returns:
  #  a distribution fit object
  
  # Make sure distribution type is useful
  if (!distribution_type %in% c("weibull", "exponential", "exp", "expo")) {
    stop("Distribution type must be one of weibull or exponential")
  }
  # Set to standard form of exponential
  if (distribution_type %in% c("exp", "expo")) {
    distribution_type <- "exponential"
  }
  
  # Drop the non-positive interval times
  df <- df[df$interval_value > 0, ]
  # If too little data, then return NA
  if (sum(df$causal == 1) < 3) {
    return(NA)
  }
  # If all causal events are the same number then no distribution can be fit; return NA
  if (length(unique(df[df$causal == 1, ]$interval_value))==1) {
    return(NA)
  }
  
  # If enough good data, then fit a distribution
  # df_for_fits <- CensUncens(df)
  df_for_fits <- Surv(df$interval_value, df$causal)
  
  options(warn=-1)
  if (distribution_type %in% "weibull") {
    # distribution_fit <- fitdistcens(df_for_fits,"weibull")
    distribution_fit <- survreg(df_for_fits ~ 1, dist = "weibull", model = TRUE, y = FALSE)
  }
  else {
    # distribution_fit <- fitdistcens(df_for_fits,"weibull",start=list(scale=median(df_for_fits$left)),fix.arg=list(shape=1))
    distribution_fit <- survreg(df_for_fits ~ 1, dist = "exp", model = TRUE, y = FALSE)
  }
  options(warn=0)
  
  return(distribution_fit)
} # end CalculateOneRemovalDistribution

CensUncens <- function(df) {
  # Transforms a subset of the interval data into a format digestible for fitdistcens() functions
  # Args:
  #   df: a subset of the repair/removals data as broken down by a dplyr::group_by call
  # Out:
  #   newdf: the repair/removals data as a two-column dataframe with just the accrued interval values
  #    event time is in the left field; if not censored event time is also in right, if censored then NA
  
  if (nrow(df[df$causal==0, ])==0) {
    censdf <- NULL # set to null b/c empty
  } else { 
    censdf <- data.frame(df[df$causal==0, ]$interval_value,NA)
    colnames(censdf)<-c("left","right")
  }
  if (nrow(df[df$causal==1, ])==0) {
    uncensdf <- NULL # set to null b/c empty
  } else {
    uncensdf <- data.frame(df[df$causal==1, ]$interval_value, df[df$causal==1, ]$interval_value)
    colnames(uncensdf) <- c("left", "right")
  }
  return(rbind(censdf, uncensdf)) # bind_rows turns the data frame into a tbl_df, which doens't work with fitdistcens
} # end CensUncens

tidy.fitdistcens <- function(x, ...){
  # Extracts distribution information from a distribution fit object
  #  Based on the broom package's tidy method
  #  Returns one row for each distribution parameter
  # Args:
  #  x: a fitted distribution object of class fitidstcens
  # Returns:
  #  data frame with parameter information  
  data.frame(parameter_name   = names(x$estimate), 
             parameter_value  = unname(x$estimate),
             standard_error   = unname(x$sd),
             stringsAsFactors = FALSE)
}

glance.fitdistcens <- function(x, ...){
  # Extracts distribution information from a distribution fit object
  #  Based on broom's glance method
  #  Returns one row for each distribution
  # Args:
  #  x: a fitted distribution object of class fitidstcens
  # Returns:
  #  data frame with distribution information
  
  if(class(x) %in% "fitdistcens"){
    data.frame("distribution_type"       = x$distname,
               "negative_log_likelihood" = x$loglik,
               "distribution_mean"       = CalculateDistMean.fitdistcens(x),
               stringsAsFactors          = FALSE)
  } else {
    data.frame("distribution_type"       = NA,
               "negative_log_likelihood" = NA,
               "distribution_mean"       = NA)
  }
}

tidy.survreg_1 <- function(x, ...){
  # Calculates the interesting parameter statistics for 
  #  survival regression of function form x ~ 1
  #  i.e. a simple probability distribution
  #  Based on broom's tidy method
  #  Returns one row for each distribution
  # Args:
  #  x: a fitted distribution object of class survreg
  # Returns:
  #  data frame with parameter information  
  
  # Translate and rename the parameters
  if (x$dist %in% "weibull") {
    scale <- unname(exp(x$icoef[1]))
    shape <- unname(1/exp(x$icoef[2]))
    x_se  <- diag(x$var)
    to_return <- data.frame(parameter_name   = c("scale", "shape"), 
                            parameter_value  = c(scale, shape),
                            standard_error   = unname(c(sqrt(x_se[1]) * scale, sqrt(x_se[2]) * shape)),
                            stringsAsFactors = FALSE)
  } else if (x$dist %in% "exponential") {
    mean_param <- exp(x$icoef)
    to_return <- data.frame(parameter_name   = "mean",
                            parameter_value  = mean_param,
                            standard_error   = unname(mean_param * sqrt(x$var)),
                            stringsAsFactors = FALSE)
  } else {
    to_return <- data.frame(parameter_name   = NA,
                            parameter_value  = NA,
                            standard_error   = NA)
  }
  return(to_return)
}

glance.survreg_1 <- function(x, parameter_names, modkm = FALSE, plots = FALSE, plot_dir = "./", ...){
  # Calculates the interesting distribution statistics for 
  #  survival regression of function form x ~ 1
  #  i.e. a simple probability distribution
  #  Based on broom's glance method
  #  Returns one row for each distribution
  # Args:
  #  x: a fitted distribution object of class survreg
  # Returns:
  #  data frame with distribution information  
  
  source_package <- class(x)
  anderson_darling_adjusted <- NA
  if (source_package %in% "survreg") { # if a distribution was fit and it was from survival package
    if (modkm) { 
      # Find the modified Kaplan Meier rank and the fitted distribution and non-parameteric values
      # TODO: Use classes to automate these calls, i.e. don't have to supply source_package: see ?class
      ranked_points <- CalculateModKM(x$model[[1]], x$icoef, x$dist, source_package) # example: 3.22 seconds w/o ADA
      # Find anderson darling statistic
      anderson_darling_adjusted <- CalculateADA(ranked_points) # about 1/3 of calc time
    }
    if (plots) {
      # save the plots to disk
      CreateWeibullPlot(ranked_points, parameter_names, x$icoef, x$dist, plot_dir)
    }
    data.frame("distribution_type"         = x$dist,
               "negative_log_likelihood"   = x$loglik[1],
               "distribution_mean"         = CalculateDistMean.survreg_1(x),
               "anderson_darling_adjusted" = anderson_darling_adjusted,
               stringsAsFactors            = FALSE)
  } else { # no distribution was fit
    data.frame("distribution_type"         = NA,
               "negative_log_likelihood"   = NA,
               "distribution_mean"         = NA,
               "anderson_darling_adjusted" = NA)
  }
}

CalculateDistMean.fitdistcens <- function(x){
  # Calculate distribution mean for fitdistcens object
  # Args:
  #  x: fitdistcens object
  # Returns
  #  the distribution mean as double
  if (x$distname %in% "weibull") {
    return(x$estimate[2]*gamma(1+1/x$estimate[1]))
  } else if (x$distname %in% "exponential") {
    return(1/x$estimate[1])
  }
}

CalculateDistMean.survreg_1 <- function(x) {
  # Calculate distribution mean for survreg object
  #  as simple `data ~ 1` call
  # Args:
  #  x: survreg object
  # Returns
  #  the distribution mean as double  
 if (x$dist %in% "weibull") {
   scale <- exp(x$icoef[1])
   shape <- 1/exp(x$icoef[2])
   return(scale*gamma(1+1/shape))
 } else if (x$dist %in% "exponential") {
   return(exp(x$icoef))
 }
}

GetMatchingParameterNames <- function(x, set_map_ref) {
  # Filter a interval parameter set map reference table to a single interval_parameter_set_id
  # Args
  #  x: an interval_parameter_set_id
  #  set_map_ref: the interval_parameter_set_map table with names - includes all data, even the previously exisiting sets
  # Returns
  #  a subset of the set_map_ref table
  set_map_ref <- filter(set_map_ref,interval_parameter_set_id==x)
}

# this will always break out by all classifiers (must give it classifier table)
# uses GetWeibullsFromDF and CensUncens functions
# previously gatherallweibulls
GatherReliabilityDistributions <- function(interval_table, parameter_table, verbose=FALSE, unbug=FALSE, plots=FALSE, modkm=FALSE, plotdir=getwd()){
  # Returns a data frame with fitted distribution objects for: 
  #  > all previously-defined interval_parameter_sets
  #  > as well as new interval_parameter_sets that aggregate over paramters, like all object types or locations
  # Args:
  #  interval_table: de-normalized interval data - includes classifiers
  #  parameter_table: classifier parameters table straight from database
  #  customized parameters controlling execution:
  #   > modkm: will calculate anderson darling adjusted statistics.  also required to plot
  #   > plots: will create plots
  # Returns: a list of two data frames for saving to the output database: 
  #  interval_parameter_set_map and reliability_distribution
  
  if (plots & !modkm) {
    stop("Modified kaplan meier statistics are required for plotting.")
  }
  if (!verbose) {
    print("Please Wait")
  }
  
  # Say there are n classifier columns total and m of them CAN be grouped (like including all part numbers from a class)
  # Total of sum of n-m Choose (n-m):0 combinations and calls to CalcDistOneParamCombo()
  # E.g WUC + 5 other classifiers gives sum(5 choose 5:0) or 32
  
  # get the classifier group names - first sort by whether they can be grouped across (e.g. removals from two WUCs shouldn't be combined)
  parameter_table <- parameter_table[order(parameter_table$do_not_combine,decreasing=TRUE),]
  class_col_names <- unlist(parameter_table$name) # unlist to get a vector
  class_col_types <- unlist(parameter_table$do_not_combine) # whether data from multiple values in the field can be combined into the same weibull
  class_col_dnc   <- class_col_names[class_col_types]
  class_count_dnc <- sum(class_col_types) # quantity of always-specific classifier fields - a constant throughout (dnc = do not combine)
  class_count_tot <- length(class_col_names) # quantity of classifier columns, including those that cannot be combined (e.g. WUC)
  class_count_cbn <- class_count_tot - class_count_dnc # quantity of combinable classifier columns
  
  reliability_distribution_list   <- list() # initialize an empty list to hold distribution data frames
  interval_parameter_set_map_list <- list() # initialize an empty list to hold int_param_set_map data frames
  max_interval_parameter_set_id   <- 0 # to make new interval_parameter_set_ids unique in the distribution tables
  
  # Assumes:
  # > there's a field 'causal' that is 0/1 for suspension or causal
  # > there's a field 'interval_value' that is the accrued age in that interval
  # > these are last in the interval data frame
  
  # Use Two loops to calculate all the distributions:
  # Loop once per quantity of classifier columns to specify (quantity of combinable/defaulatble fields) e.g. 0 through 5
  for (ii in seq(from=0, to=(class_count_cbn))) { 
    # e.g. 0 is specify only the do-not-combine classifiers
    # e.g. 1 is specify the do-not-combine classifiers plus one additional classifier
    # Get all the possible combinations with a single call to combn() during this outer loop
    # combn gives all the ways to choose 'ii' digits out of quantity of defaulatable classifier columns 
    #  (others are always specific)
    classifier_combo_table <- combn(class_count_tot:(1+class_count_dnc), ii) 
    # Loop through these columns - once per possible way to choose the classifier columns
    for (jj in seq_len(ncol(classifier_combo_table))) { 
      include_param_names <- class_col_names[sort(c(seq(class_count_dnc),classifier_combo_table[,jj]))]
      if (verbose) {
        print(paste("Calculating distributions specifying:",paste(include_param_names,collapse=", ")))
      }
      # Calculate distributions and nest data
      intmd_dists <- CalcDistOneParamCombo(interval_table,
                                  include_param_names, # break-out (specify) these fields
                                  class_col_dnc)
#       if (sum(class(intmd_dists) %in% "data.frame") == 0) {
#         # this particular combination of parameters didn't yield any unique groups, so skip it
#         next
#       }
      # Add an integer to each interval_parameter_set_id to make unique
      intmd_dists$interval_parameter_set_id <- intmd_dists$interval_parameter_set_id + max_interval_parameter_set_id
      # Increment this max count integer
      max_interval_parameter_set_id <- max_interval_parameter_set_id + nrow(intmd_dists)
      # Create the interval_paramter_set_map table:
      #   copy off the interval_parameter_set_id and parameter ids
      #   gather into key/value pairs for each, then drop the keys because the values are unique ids
      intmd_param_set_map <- intmd_dists[,seq(1+length(include_param_names))] %>%
        gather(key, parameter_value_id, -interval_parameter_set_id) %>% dplyr::select(-key) %>%
        arrange(interval_parameter_set_id, parameter_value_id)
      # delete the parameter fields from the distribution table - not needed anymore (keep int_param_set_id)
      intmd_dists <- intmd_dists[, -seq(from=2,to=1+length(include_param_names))]
      # save the data frames in the lists
      interval_parameter_set_map_list <- append(interval_parameter_set_map_list, list(intmd_param_set_map))
      reliability_distribution_list   <- append(reliability_distribution_list, list(intmd_dists))
      # TODO optimize this appending by preallocating? or something here: https://stackoverflow.com/questions/2436688/append-an-object-to-a-list-in-r-in-amortized-constant-time
    } # end inner for
  } # end outer for
  
  return(list(reliability_distribution_list, interval_parameter_set_map_list))
} # end GatherReliabilityDistributions

# previously calloneDDPLY
CalcDistOneParamCombo <- function(interval_table, include_param_names, always_include_names) {
  # Calculates distributions for one set of parameter types to group by
  #   uses dplyr's group_by to split, apply, and combine
  # Args:
  #   interval_table: the full de-normalized interval table
  #   include_param_names: vector of field names to break apart a classifier into its specific values
  # Returns:
  #   final calculated weibulls for this combination of specific and default classifiers

  # group by with a vector of string names
  # https://stackoverflow.com/questions/21208801/group-by-multiple-columns-in-dplyr-using-string-vector-input
  # 1) use symbols (list) then use group_by_ (with an underscore at the end)
  include_param_names  <- lapply(include_param_names, as.symbol)
  always_include_names <- lapply(always_include_names, as.symbol)
  other_names          <- include_param_names[!(include_param_names %in% always_include_names)]
  
  ### I commented this out because the last distributions are being removed in the weibulls-Work document - previosuly this was to deconflict with the parameter sets that already existsed
#   # Skip this combination of grouping classifiers if they give any non-unique data (i.e. if some are the same for all data)
#   distributions_nested <- group_by_(interval_table, .dots=include_param_names) # it's okay if these yield only one group
#   all_groups <- n_groups(distributions_nested)
#   for (ii in seq(other_names)) {
#     if (n_groups(group_by_(interval_table, .dots=other_names[-ii])) == all_groups) {
#       # If excluding a classifier yields the same number of groups as leaving them all in
#       #  then this classifier is not useful here.  Skip this entirely. The distirbution will show up elsewhere.
#       return(NA)
#     }
#   }
  
  # Nest data, then calculate and save distribution
  # Call CalculateOneRemovalDistribution twice on each group to calculate weibull and exponential distributions
  distributions_nested <- group_by_(interval_table, .dots=include_param_names) %>% 
                nest() %>% # store the data in the data frame as a 1-element list with a dataframe inside
                mutate(dist_fit_w = purrr::map(data, ~ CalculateOneRemovalDistribution( # map returns a 1-element list of the fit
                  data.frame("interval_value"=.$interval_value, "causal"=.$causal),"weibull")),
                  dist_fit_e = purrr::map(data, ~ CalculateOneRemovalDistribution( # map returns a 1-element list of the fit
                    data.frame("interval_value"=.$interval_value, "causal"=.$causal),"exp"))) %>%
    add_rownames(var = "interval_parameter_set_id")
  
  distributions_nested$interval_parameter_set_id <- as.integer(distributions_nested$interval_parameter_set_id)
  
  return(distributions_nested)
} # end CalcDistOneParamCombo

# previously getmodkm
CalculateModKM <- function(interval_value, params, distribution, source_package) {
  # Calculates Modified Kaplan Meier and distribution values for each event in the distribution
  #  Also re-parameterizes survreg distribution parameters
  # Args:
  #  interval_value: simplified interval data for the distribution in a 2-column data frame
  #   > interval value
  #   > causal or non-causal (1/0)
  #  params: parameters from the fit
  #  distribution: string of distribution type, like weibull
  #  source_pacakge: string of survreg or fitdistcens
  # Returns:
  #  a data frame with the modified values, rank, and ages
  
  # TODO: Use classes to automate these calls, i.e. don't have to check source_package
  if (source_package %in% "survreg") {
    if (distribution %in% "weibull") {
      scale <- unname(exp(params[1]))
      shape <- unname(1/exp(params[2]))
    } else if (distribution %in% "exponential") {
      scale <- unname(exp(params))
      shape <- 1
    }
  } else if (source_package %in% "fitdistcens") {
    if (distribution %in% "weibull") {
      scale <- unname(params[2])
      shape <- unname(params[1])
    } else if (distribution %in% "exponential") {
      scale <- unname(params)
      shape <- 1
    }
  } else {
    stop("Failed attempting to calculate kaplan meier statistics for a fit 
         of type other than fitdistcens or survreg")
  }
  
  # Extract and sort interval data into two data frames with the distribution probability
  causal_data <- data.frame("interval_value" = sort(interval_value[, 1][interval_value[, 2]==1]),
                            "fit_probability" = pweibull(sort(interval_value[, 1][interval_value[, 2]==1]),shape,scale),
                            "causal" = 1)
  # If some suspension data exists, create the data frame and combine with causal (order matters)
  if (sum(interval_value[, 2]==0)>0) {
    suspension_data <- data.frame("interval_value" = sort(interval_value[, 1][interval_value[, 2]==0]),
                                  "fit_probability" = pweibull(sort(interval_value[, 1][interval_value[, 2]==0]),shape,scale),
                                  "causal" = 0)
    interval_value <- bind_rows(causal_data, suspension_data)
  } else {
    interval_value <- causal_data
  }
  # Order by event time and causal b/f suspension, then add row name as rank
  interval_value <- arrange(interval_value, interval_value, -causal) %>% add_rownames(var = "rank") 
  interval_value$rank <- as.integer(interval_value$rank)
  
  # Calculate Modified Kaplan-Meier rank (just for the non-censored events)
  #  using nrow(interval_value) allows calculating reverse-rank from rank
  max_rank <- nrow(interval_value)
  
  # Calculate:
  # p_prime - Normal Kaplan Meier statistic (matches JMP and fitsurv from survival package)
  # p       - Modified Kaplan Meier statistic
  interval_value <- mutate(interval_value, 
                           p_intmd =   (max_rank - row_number())^causal / 
                            (max_rank - row_number() + 1)^causal,
                           p_prime = 1 - cumprod(p_intmd), 
                           prev_p_prime = lag(p_prime, 1L, default = 0),
                           p = (1 - ((1 - p_prime) + (1 - prev_p_prime)) / 2)) %>%
    select(-prev_p_prime,-p_intmd)
  # using mutate with cumprod is much faster than vapply and prod(1:rank)
  # other uses of mutate are mostly for elegance
  
  return(interval_value)
} # end CalculateModKM

CalculateADA <- function(ranked_points) {
  # Calculate's a distribution's anderson darling adjusted statistic
  # Args
  #  ranked_points: interval data with modified Kaplan Meier ranks (empirical dist) and distribution fit
  #   p is modified kaplan meier statistic, p_prime is standard kaplan meier statistic
  # Returns
  #  the anderson darling adjusted statistic (a double)
  
  # Only take the plotted (noncensored) points
  ranked_points      <- ranked_points[ranked_points$causal==1,] 
  lgth               <- nrow(ranked_points) # number of noncensored points
  ranked_points$rank <- seq_len(lgth) # recalculate the rank
  
  ranked_points <- mutate(ranked_points, 
                          prev_fit_prob = lag(fit_probability, 1L, default = 0),
                          prev_p = lag(p, 1L, default = 0),
                          ada_contribution = -fit_probability - log(1-fit_probability) + 
                            prev_fit_prob + log(1-prev_fit_prob) + 
                            2*log(1-fit_probability)*prev_p - 2*log(1-prev_fit_prob)*prev_p +
                            log(fit_probability)*prev_p^2 - log(1-fit_probability)*prev_p^2 - 
                            # log(0) is infinity, so must use ifelse here:
                            ifelse(prev_fit_prob==0,0,log(prev_fit_prob)*prev_p^2) +
                            log(1-prev_fit_prob)*prev_p^2
  )
  
  # the n+1th row value doesn't belong in the data frame
  final_p <- ranked_points$p[lgth]
  final_fit <- ranked_points$fit_probability[lgth]
  
  final_point <- -(1-1E-12) - log(1-(1-1E-12)) + 
    ranked_points$fit_probability[lgth] + log(1 - final_fit) + 
    2*log(1-(1-1E-12))*final_p - 2*log(1-final_fit)*final_p + 
    log(1-(1E-12))*final_p^2 - log(1-(1-(1E-12)))*final_p^2 -
    log(final_fit)*final_p^2 + log(1-final_fit)*final_p^2
  
  return(lgth*(sum(ranked_points$ada_contribution,final_point)))
} # end CalculateADA

MakeTitlePretty <- function(plot_title, parameter_separator, max_char) {
  # Recursive function to add line breaks to a title for weibull plots
  # Args:
  #   plot_title: original or modified plot title
  #   parameter_separator: character string with which parameters are spaced apart in the title
  #   max_char: ideal number of characters to fit on a plot row
  # Returns
  #   a plot title or part of a plot title
  
  ends_vector <- gregexpr(parameter_separator, plot_title)[[1]] # where parameters end on the same line
  # Four cases:
  #  1) plot title is less than the character limit: return title w/o recursion
  #  2) there's only one parameter on this so: return title w/o recursion
  #  3) the first parameter is over the limit - split it into its own line and recurse
  #  4) some other parameter is over the limit - make everything before this parameter its own line and recurse
  
  # case 1 - short line
  if (nchar(plot_title) <= max_char) { 
    return(plot_title)
  }
  
  # case 2 - one param
  if (ends_vector[1] == -1) { 
    return(plot_title)
  }
  
  ends_vector <- c(ends_vector,nchar(plot_title))
  too_long_param <- which(ends_vector > max_char)[1] # first parameter after max_char characters
  # case 3 - split is on first param
  if(too_long_param == 1) {
    split_this_many_before <- 0 # split the first (only) separator that's over the limit
  } else {
  # case 4
    split_this_many_before <- 1
  }
  
  plot_line       <- substr(plot_title, start = 1, stop = ends_vector[too_long_param-split_this_many_before]-1)
  remaining_title <- substr(plot_title, 
                            start = ends_vector[too_long_param-split_this_many_before] + nchar(parameter_separator), 
                            stop = nchar(plot_title))
  # recurse
  plot_title <- paste(plot_line, MakeTitlePretty(remaining_title, parameter_separator, max_char),sep = "\n")
  
  return(plot_title)
}

CreateWeibullPlot <- function(ranked_points, dist_param_names, params, distribution_type, plotdir = "./") {
  # Save a probability plot of the events and their fit
  #   overlayed with a histogram of the censored values
  #   uses weibull-scale log-log axes:
  #     x is log(time)
  #     y is log(log(1/(1-Unreliability(t))))
  # Args:
  #   ranked_points: modified Kaplan Meier ranked points for all intervals
  #   parameter_names: sample of original data frame with classifiers
  # params: two parameters for the weibull distribution
  # catgs: grouping categories passed to ddply to define which factors to not use "ALL"
  # uses Modified Kaplan-Meier method for plotting uncensored and censored data
  
  # Extract Parameters
  if (distribution_type %in% "weibull") {
    params[1] <- (exp(params[1])) # scale
    params[2] <- (1/exp(params[2])) # shape
    params[3] <- params[1]*gamma(1+1/params[2])
    names(params) <- c("scale", "shape", "dist_mean")
  } else if (distribution_type %in% "exponential") {
    params <- exp(params)
    names(params) <- "mean"
  } else (
    stop("Trying to make a weibull plot from a distribution other than weibull or exponential")
  )
  
  # Build the plot title as a string
  #   hopefully there are no
  ## TODO: find a better way to order these parameter names
  dist_param_names    <- arrange(dist_param_names, parameter_name) # order alphabetically
  parameter_separator <- "; "
  plot_title <- paste(dist_param_names$parameter_name,dist_param_names$value,sep=":",collapse=parameter_separator)
  # try to split into new lines about every 40 characters
  plot_title <- MakeTitlePretty(plot_title, parameter_separator, max_char = 40)
  distribution_type_title <-paste0(toupper(substr(distribution_type,1,1)),
                                   substr(distribution_type,2,nchar(distribution_type)))
    
  # Plot
  op <- par() # save the plot parameters
  # dots on plot
  ## TODO: make the filename more informative
  png(file = paste(plot_dir,"WeibullPlot_", distribution_type, "_", 
                   dist_param_names$interval_parameter_set_id[1], ".png", sep=""), 
      width=5.96, height=4.54, units="in", res=144)
  par(mar=c(3.6, 4, 4, 2) + 0.1) # push the plot down a little
  weibplot(ranked_points[ranked_points$causal==1, ]$interval_value,
           ranked_points[ranked_points$causal==1, ]$p,
           forcexlim=c(.9, log10(max(ranked_points$interval_value)) + .02),
           forceylim=c(-7, 0), xlab="", ylab="Percent", col=rgb(255/255, 0/255, 0/255), pch=16,
           main=paste0("Removal Plot with ", distribution_type_title, " Fit\n",plot_title))
  title(sub="Flight Hours",line=2)
  abline(h=c(log(-log(1-c(.001, .01, .1)))),v=c(c(seq(-1,3))),col=rgb(1, 0, 0)) # Red
  abline(h=c(log(-log(1-c(.003, .02, .05, .25, .5, .75, .9, .96, .99, .999)))), col=rgb(38/255, 201/255, 38/255))
  abline(v=c(log10(seq(7, 9)), log10(seq(20, 90, 10)), log10(seq(200, 900, 100)), 
             log10(seq(2000, 9000, 1000))), col=rgb(38/255, 201/255, 38/255)) # Green
  # fitted line
  if (distribution_type %in% "weibull") {
    line.41 <- qweibull(seq(0.0001, .99, .0005), scale = params[1], shape = params[2], log.p=F)
  } else { # exponential
    line.41 <- qweibull(seq(0.0001, .99, .0005), scale = params[1], shape = 1, log.p=F)
  }
  
  lines(x=log10(line.41), y=log(-log(1-seq(.0001, .99, .0005))))
  # legend
  if (distribution_type %in% "weibull") {
    legend_text <- c(paste("Shape: ", round(params[2], digits=3)), paste("Scale: ",round(params[1], digits=3)), 
                     paste("Mean: ", round(params[3], digits=3)), 
                     paste("Observed: ", nrow(ranked_points[ranked_points$causal==1, ])), 
                     paste("Censored: ", nrow(ranked_points[ranked_points$causal==0, ])))
  } else { 
    legend_text <- c(paste("Exponential\nMean: ", round(params[1], digits=3)), 
                     paste("Observed: ", nrow(ranked_points[ranked_points$causal==1, ])), 
                     paste("Censored: ", nrow(ranked_points[ranked_points$causal==0,])))
  }
  legend(x="topleft", legend=legend_text, cex=.85, ncol=1, inset=c(-0.05,-0.03))
  
  #histogram (if there are censored values)
  if(nrow(ranked_points[ranked_points$causal==0, ])==0) { # just plot a zero at median of non-censored hours
    text(x=log10(median(ranked_points[ranked_points$causal==1, ]$interval_value)), y=-7, labels="0")
  } else { # plot the histogram
  par(new=T, mar=c(3.14, 4.1, 4.1, 2.1)) #5.96 x 4.54 to line up the histogram to the bottom of the plot area
  hist_holder <- hist(log10(ranked_points[ranked_points$causal==0, ]$interval_value), 
                      breaks="Sturges",plot=F) # to get the bin heights and counts
  hist_plot   <- hist(log10(ranked_points[ranked_points$causal==0, ]$interval_value), 
                      breaks="Sturges", xlim=c(.9, log10(max(ranked_points$interval_value)) + .02), 
                      ylim=c(0, max(hist_holder$counts)*2.2), border=1, 
                      col=rgb(149/255, 184/255, 251/255), ylab=NULL, xlab=NULL, 
                      main=NULL, labels=T, axes=F, plot=T)
  } # end if there is enough data to plot the histogram
  options(warn=-1)
  par(op)
  options(warn=0)
  invisible(dev.off())
} # end plot Function

###plotting functions###
# fix the scale to Weibull:
# draw the plot:
weibplot <- function(x, y, log='xy', ..., forceylim=c(0,0), forcexlim=c(0,0)) {
  x <- log(x,10)
  y <- log(-log(1-y))
  xlg <- TRUE # hard-coded for now
  ylg <- TRUE
  yl <- ifelse(forceylim==c(0,0),range(y),forceylim)
  xl <- ifelse(forcexlim==c(0,0),range(x),forcexlim)
  plot(x,y,...,axes=FALSE,ylim=yl,xlim=xl)
  if(xlg){drawlogaxis(1,xl)}else{axis(1,at=pretty(xl),labels=pretty(xl))}
  if(ylg){drawweibaxis()}else{axis(2,at=pretty(yl),labels=pretty(yl))}
  box()
}

# draw the axes:
drawlogaxis <- function(side,range)  {
  par(tck=0.02)
  mlog <- floor(min(range))
  Mlog <- ceiling(max(range))
  SeqLog <- c(mlog:Mlog)
  Nlog <- (Mlog-mlog)+1
  axis(side,at=SeqLog,labels=10^SeqLog)
  ats <- log(seq(from=2,to=9,by=1),10)
  mod <- NULL
  for(i in SeqLog)
  {
    mod <- c(mod,rep(i,length(ats)))
  }
  ats <- rep(ats,Nlog)
  ats <- ats+mod
  par(tck=0.02/3)
  axis(side,at=ats,labels=NA)
}

drawweibaxis <- function()  {
  par(tck=0.02)
  SeqWeib <- c(.001,.003,.01,.02,.05,.1,.25,.5,.75,.9,.96,.99,.999)
  axis(2,labels=SeqWeib,at=(log(-log(1-SeqWeib))),las=2)
}
### end plotting functions ###

# 
matchDistToInterval <- function(allweibulls,allevents,parameter_table) {
  # Builds a 2-d binary matrix whether a particular removal is included in a particular weibull
  # Will be used by the optimization model as constants for the constraints 
  #   - event must be included in one and only one weibull
  #  includes both causal and suspension events
  # Args:
  #   allweibulls: calculated weibull distribution
  #   allevents: the full events/interval table
  #   parameter_table: 
  # Returns:
  #   data frame of zeros or ones - only for the weibulls that have fit a distribution (could this be a matrix?)
  
  # get the classifier group names - first sort by whether they can be grouped across (e.g. removals from two WUCs shouldn't be combined)
  parameter_table <- parameter_table[order(parameter_table$do_not_combine,decreasing=TRUE),]
  classColNames <- unlist(parameter_table$name) # unlist to get a vector #c("WUC","LOCATION","PN","NHA_PN","LAST_REPAIR","REPAIR_COUNT")
  classColType <- unlist(parameter_table$do_not_combine) # whether data from multiple values in the field can be combined into the same weibull
  classCountToIgnore <- sum(classColType) # number of always-specific classifier fields - a constant throughout
  
  # drop weibulls that did not fit b/c too few data
  weibullTable <- allweibulls[!is.na(allweibulls$NLogLik),]
  
  # drop unnecessary fields - only need the valid combinations of classifier / parameter fields
  # also make sure the fields are in the same order
  weibullTable <- weibullTable[,classColNames]
  eventTable <- allevents[,classColNames]
  # add id fields
  weibullTable$wId <- 1:nrow(weibullTable)
  eventTable$eId <- 1:nrow(eventTable)
  
  # loop over the weibulls, build a vector of 0/1 if the event is included in this weibull, and then bind it together
  first <- TRUE
  for (ii in seq(nrow(weibullTable))) {
    oneVector <- rep(1,nrow(eventTable)) # initialize to 1 - assume event is in weibull
    # loop over classifier fields and pare down data (ignore those with "ALL")
    spCatg <- weibullTable[ii,-ncol(weibullTable)] # ignore the id
    for (jj in colnames(spCatg)) {
      if (!spCatg[1,jj] %in% "ALL") {
        # set to zero those events that are not included
        oneVector[-which(eventTable[,jj] %in% spCatg[1,jj])] <- 0
      }
    }
    if(first){
      oneMatrix <- oneVector
      first <- FALSE
    } else {
      oneMatrix <- c(oneMatrix,oneVector)
    }
  }
  # convert "matrix" vector to an actual matrix by splitting into a column per weibull
  oneMatrix <- matrix(oneMatrix,ncol=nrow(weibullTable),nrow=nrow(eventTable),byrow=FALSE)
}