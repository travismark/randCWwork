### Fit many distributions

# Load packages

suppressPackageStartupMessages(library("survival", quietly = TRUE))
suppressPackageStartupMessages(library("fitdistrplus", quietly = TRUE))
suppressPackageStartupMessages(library("plyr", quietly = TRUE))
suppressPackageStartupMessages(library("dplyr", quietly = TRUE))
suppressPackageStartupMessages(library("tidyr", quietly = TRUE))
suppressPackageStartupMessages(library("readr", quietly = TRUE))
suppressPackageStartupMessages(library("purrr", quietly = TRUE))

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
  # Drop parameters that aren't possible for calculating weibulls
  parameter <- parameter[parameter$removal_rate==1,]
  # Then the remaining fields
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
    distribution_fit <- survreg(df_for_fits ~ 1, dist="weibull", model = TRUE, y = FALSE)
  }
  else {
    # distribution_fit <- fitdistcens(df_for_fits,"weibull",start=list(scale=median(df_for_fits$left)),fix.arg=list(shape=1))
    distribution_fit <- survreg(df_for_fits ~ 1, dist="exp", model = TRUE, y = FALSE)
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

glance.survreg_1 <- function(x, modkm = FALSE, plots = FALSE, ...){
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
      ranked_points <- CalculateModKM(x$model[[1]], x$icoef, x$dist, source_package) # about 2/3 of calc time
      # Find anderson darling statistic
      anderson_darling_adjusted <- CalculateADA(ranked_points) # about 1/3 of calc time
    }
    if (plots) {
      # record the plots
      # getPlotFromDF(ranked_points, head(df), weib[[1]], catgs, plotdir, unbug) #ranked points, sample of input data, break-out categories
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
  class_col_names <- unlist(parameter_table$name) # unlist to get a vector #c("WUC","LOCATION","PN","NHA_PN","LAST_REPAIR","REPAIR_COUNT")
  class_col_types <- unlist(parameter_table$do_not_combine) # whether data from multiple values in the field can be combined into the same weibull
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
        print(paste("Now calculating distributions specifying:",paste(include_param_names,collapse=", ")))
      }
      # Calculate distributions and nest data
      intmd_dists <- CalcDistOneParamCombo(interval_table,
                                  include_param_names) # break-out (specify) these fields
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
CalcDistOneParamCombo <- function(interval_table, include_param_names) {
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
  include_param_names <- lapply(include_param_names, as.symbol)
  
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
  interval_value$p_intmd <- ((nrow(interval_value) - row(interval_value)[, 1])^interval_value$causal) /
    ((nrow(interval_value) - row(interval_value)[, 1] + 1)^interval_value$causal)
  
  # First row has a unique formula
  interval_value$p_prime[1] <- 1 - interval_value$p_intmd[1]
  interval_value$p[1]       <- 1 - ((1-interval_value$p_prime[1])+1) / 2
  
  # Calculate remaining values
  # p_prime - Normal Kaplan Meier statistic (matches JMP and fitsurv from survival package)
  # p     - Modified Kaplan Meier statistic
  interval_value$p_prime[-1] <- sapply(interval_value$rank[-1], GetPPrime, interval_value$p_intmd)
  interval_value$p[-1]       <- sapply(interval_value$rank[-1], GetP, interval_value$p_prime)
  
  return(interval_value) # return interval value table
} # end CalculateModKM

# Used in CalculateModKM
GetPPrime <- function(rank, p_intmd_all_rows){
  # 1 less the product of this and all the previous p_intermediates
  rank <- rank[[1]][1]
  return(1 - prod(p_intmd_all_rows[1:rank]))
}

# Used in CalculateModKM
GetP <- function(rank, p_prime_all_rows){
  # 1 less the average of this and the previous (1-p_prime)
  rank <- rank[[1]][1]
  return(1 - ((1-p_prime_all_rows[rank]) + (1-p_prime_all_rows[rank-1])) / 2)
}


CalculateADA <- function(ranked_points) {
  # Calculate's a distribution's anderson darling adjusted statistic
  # Args
  #  ranked_points: interval data with modified Kaplan Meier ranks (empirical dist) and distribution fit
  # Returns
  #  the anderson darling adjusted statistic (a double)
  
  # Only take the ploted (noncensored) points
  ranked_points      <- ranked_points[ranked_points$causal==1,] 
  lgth               <- nrow(ranked_points) # number of noncensored points
  ranked_points$rank <- seq_len(lgth) # recalculate the rank
  
  # use the findArow function to get A values of each row, then add the n+1th row A value
  A <- sum(apply(X=ranked_points, MARGIN=1, FUN=findArow, ranked_points$fit_probability)) - 
    (1-1E-12) - log(1-(1-1E-12)) + 
    ranked_points$fit_probability[lgth] + log(1 - ranked_points$fit_probability[lgth])
  # B values
  B <- sum(apply(X=ranked_points, MARGIN=1, FUN=findBrow,
                 fit_probability=ranked_points$fit_probability,non_par=ranked_points$p)) +
    2*log(1-(1-1E-12))*ranked_points$p[lgth] - 2*log(1-ranked_points$fit_probability[lgth])*ranked_points$p[lgth]
  # C values
  C <- sum(apply(X=ranked_points, MARGIN=1, FUN=findCrow,
                 fit_probability=ranked_points$fit_probability,non_par=ranked_points$p)) +
    log(1-(1E-12))*ranked_points$p[lgth]^2 -
    log(1-(1-(1E-12)))*ranked_points$p[lgth]^2 -
    log(ranked_points$fit_probability[lgth])*ranked_points$p[lgth]^2 +
    log(1-ranked_points$fit_probability[lgth])*ranked_points$p[lgth]^2
  
  return(lgth*(A+B+C))
} # end CalculateADA

## Three helper functions for calculating anderson darling statistic
# returns a single numeric value:  A in the AD* (anderson darling adjusted) calculation. 
# to be used in the AD* function and with apply by row;  needs a single row from the ranked points data frame and the entire fitprob column from "both"
findArow <- function(input,fit_probability) {
  # input is a matrix with named fields
  if (input['rank']==1) { # check rank - first row is special
    out <- (-input['fit_probability']) - log(1-input['fit_probability']) + 0 + log(1-0)
  } else { # not the first row
    out <- (-input['fit_probability']) - log(1-input['fit_probability']) + fit_probability[input['rank']-1] + log(1-fit_probability[input['rank']-1])
  } # end not the first row
} # end find A
# returns a column for the "both" data frame: B in the AD* calculation
#  like A, but also requires the non parameteric CDF value for all the points
findBrow <- function(input, fit_probability,non_par) {
  if (input['rank']==1) { # first row is special
    out <- 0
  } else { # not the first row
    out <- 2*log(1-input['fit_probability'])*non_par[input['rank']-1] - 2*log(1-fit_probability[input['rank']-1])*non_par[input['rank']-1] 
  } # end not the first row
} # end find B
# returns a column for the "both" data frame: C in the AD* calculation
findCrow <- function(input, fit_probability,non_par) {
  if (input['rank']==1) { # first row is special
    out <- 0
  } else { # not the first row
    out <- log(input['fit_probability'])*non_par[input['rank']-1]^2 - log(1-input['fit_probability'])*non_par[input['rank']-1]^2 - 
      log(fit_probability[input['rank']-1])*non_par[input['rank']-1]^2 + log(1-fit_probability[input['rank']-1])*non_par[input['rank']-1]^2
  } # end not the first row
} # end find C


BetaTest<-function(input,weibnll) {
  # Find the p value of the test comparing the weibull fit to the exponential fit
  # Args: 
  #  input: censored and uncensored times 
  #  weibnll: negative log likelihood of the weibull fit for these times
  # Returns:
  #  list with pvalue from chi squared test and the exponential fit mean parameter
  
  expofit<-fitdistcens(input,"weibull",start=list(scale=median(input$left)),fix.arg=list(shape=1)) # most likely expo fit to work
  teststat<-2*(weibnll-expofit$loglik)
  pvalue<-1-pchisq(teststat,1)
  (list(pval=pvalue,param=expofit[[1]]))
}

getWeibullFromDF <- function(df, plot=FALSE, catgs="",modkm, plotdir,unbug) {
  # Outputs a row with fitted weibulls and can generate a weibull plot in the specified directory
  # Args:
  #   df: a subset of the removals/repair interval value/age (TOW) data as broken down by a previous call to ddply()
  #   plot: boolean if the function should call getPlotFromDF to save a plot
  #   catgs: text string of all the specific classifiers used in the previous steps to pass df to this function
  #   modkm: boolean if the function should calculate the modified kaplan meier plot points; required for plotting
  #          determines whether to calculate anderson darling statistics
  #   plotdir: directory to store plots
  #   unbug: boolean if unbug remarks should be printed to console
  # Returns:
  #   out: a row with weibull & exponential paramaters and goodness of fit info for one set of classifiers 
  #        ddply will append this to the classifiers columns to make a row of the final output
  #   calls getPlotFromDF to make a plot, if option is on
  
  #make sure the passed data frame has enough data: at least three failure/removal/events
  #      (anything less may give errors in fitting the weibull with mle method)
  if(nrow(df[df$causal==1,])<3) {
    # with too little data:  report the number of events and give NAs for everything else   
    out<-data.frame(NA,NA,NA,NA,NA,0,0,NA,NA,NA,NA,NA,NA,NA)
    colnames(out)<-c("shape","scale","shapeSE","scaleSE","DistMean","Events","Censored","NLogLik","BetaTestPval","AD*weib","AD*expo","AD*logn","AD*norm","ExpoScale")
    out[1,6]<-length(df[df$causal==1,8])
    out[1,7]<-length(df[df$causal==0,8])
    (out) # no plot generated
  } else {
    #shape the intervalValue table into a two-column dataframe with time on wing and censored information for fitdistcens()
    newdf<-CensUncens(df)
    # make sure the causal failures are not all the same number;  otherwise the weibull is horizontal and impossible to fit
    if(length(unique(newdf[,2][complete.cases(newdf[,2])]))==1) {
      # with too little data:  report the number of events and give NAs for everything else   
      out<-data.frame(NA,NA,NA,NA,NA,0,0,NA,NA,NA,NA,NA,NA,NA)
      colnames(out)<-c("shape","scale","shapeSE","scaleSE","DistMean","Events","Censored","NLogLik","BetaTestPval","AD*weib","AD*expo","AD*logn","AD*norm","ExpoScale")
      out[1,6]<-length(df[df$causal==1,8])
      out[1,7]<-length(df[df$causal==0,8])
      (out) # no plot generated 
    } else {
      #calculate a weibull from that new 2-column dataframe
      if(unbug){print(df[1,])}
      if(unbug){print(dim(newdf))}
      options(warn=-1)
      weib<-fitdistcens(newdf,"weibull")
      options(warn=0)
      #find the modified Kaplan Meier rank and the fitted values 
      if (modkm) {
        rankedpoints<-getModKM(newdf,weib[[1]]) # takes cens/uncens interval values (TOW) and weibull parameters
      }
      #find the exponential parameters and the beta = 1 p value
      betaout<-BetaTest(newdf,weib$loglik)
      #calculate the Anderson Darling Adjusted statistics for weibull,exponential,lognormal and normal distributions
      if (modkm) {
        ADstats<-calcADAvals(rankedpoints,newdf,betaout[[2]])
      } else {ADstats<-data.frame(NA,NA,NA,NA)} # finding modkm is slow so option out
      #save a plot of the weibull. this requires modkm
      if (plot) {
        getPlotFromDF(rankedpoints, head(df), weib[[1]], catgs, plotdir, unbug) #ranked points, sample of input data, break-out categories
      }
      
      # save the scale and shape parameters & their standard errors
      #   & # of events and beta-test-p-value in a dataframe
      out<-cbind(t(data.frame(weib[1])),t(data.frame(weib[2])))
      out<-as.data.frame(out)
      out[1,5]<-weib[[1]][2]*gamma(1+1/weib[[1]][1]) # mean time b/w events (scale*gamma(1+1/shape))
      out[1,6]<-length(is.na(newdf$right)[is.na(newdf$right)==F]) # uncensored events
      out[1,7]<-length(is.na(newdf$right)[is.na(newdf$right)==T]) # censored
      out[1,8]<-weib$loglik # negative log likelihood of the weibull fit (will be used to compare fits)
      out[1,9]<-betaout[[1]] # get the Log-Likelihood Ratio Test p-value for Beta=1 test (approximation of Minitab Wald Test p-value)
      out[1,10]<-ADstats[1] # weibull AD* statistic
      out[1,11]<-ADstats[2] # exponential AD* statistic
      out[1,12]<-ADstats[3] # lognormal AD* statistic
      out[1,13]<-ADstats[4] # normal AD* statistic
      out[1,14]<-betaout[[2]] # exponential scale (1/rate) parameter
      colnames(out)<-c("shape","scale","shapeSE","scaleSE","DistMean","Events","Censored","NLogLik","BetaTestPval","AD*weib","AD*expo","AD*logn","AD*norm","ExpoScale") 
      (out)
    } # end if there are different values in the causal events
  } # end if there are enough data
} # end getWeibullFromDF function

# plot a probability graph plot of the events and their fit 
#   overlayed with a histogram of the censored values
# called from within getweibullsfromdf
# both: modKM-ranked points for all repairs
# df: sample of original data frame with classifiers
# params: two parameters for the weibull distribution
# catgs: grouping categories passed to ddply to define which factors to not use "ALL"
# uses Modified Kaplan-Meier method for plotting uncensored and censored data
getPlotFromDF<- function(both, df, params, catgs, plotdir, unbug=FALSE) {
  # assign titles for the plot
  title.WUC<-as.character(df$WUC[1])
  title.name<-as.character(wucnames[wucnames$WUC==title.WUC,2])
  # this looks if the different classifiers are specific (broken out) or general (all)
  # there is almost certainly a better way to do this, but...
  title.Loc<-ifelse(strsplit(catgs,"LOCATION")==catgs,"ALL",as.character(df$LOCATION[1]))
  title.Loc<-ifelse(title.Loc=="N/A","NA",title.Loc) # png name can't have a slash in it
  # distinguishing between nha_pn and pn is more involved:  first check for nha_pn
  title.NPN<-ifelse(strsplit(catgs,"NHA_PN")==catgs,"ALL",as.character(df$NHA_PN[1]))
  # then split on nha_pn and check what wasn't split for PN or check both sides for PN 
  a<-strsplit(paste0(" ",catgs," "),"NHA_PN")[[1]] # have to get the first element of the list that strsplit returns
  b<-0
  if(length(a)==1){ # if nha_pn not found, check catgs string
         title.PN<-ifelse(strsplit(catgs,"PN")==catgs,"ALL",as.character(df$PN[1]))}
  else {
        for(ii in 1:2){ifelse(strsplit(a[ii],"PN")[[1]]==a[ii],b<-b+0,b<-b+1)}; # if b==0 then no match
            if(b>0){title.PN<-as.character(df$PN[1])}
            else {title.PN<-"ALL"}
       }
  title.RC<-ifelse(strsplit(catgs,"REPAIR_COUNT")==catgs,"ALL",as.character(df$REPAIR_COUNT[1]))
  title.LR<-ifelse(strsplit(catgs,"LAST_REPAIR")==catgs,"ALL",as.character(df$LAST_REPAIR[1]))
    
  # Plot
  op<-par() # save the plot parameters
  #dots
  png(file = paste(plotdir,"WeibullPlot._WUC_",title.WUC,";Loc_",title.Loc,";PN_",title.PN,";RepInt_",
                   title.RC,".png",sep=""), width=5.96, height=4.54, units="in", res=144)
  par(mar=c(3.6, 4, 4, 2) + 0.1) # push the plot down a little
  weibplot(both$interval_value[which(both$cens==1)],both$p[which(both$cens==1)],forcexlim=c(.9,log10(max(both$interval_value))+.02),
           forceylim=c(-7,0), xlab="",ylab="Percent",col=rgb(255/255,0/255,0/255),pch=16,
           main=paste("Weibull Removal Plot\n",title.name,
                      ifelse(title.Loc=="ALL"," - All Locations",ifelse(title.Loc=="NA"," - No Majority Location",paste(" - ",title.Loc))),
                      "\n",ifelse(title.PN=="ALL","All Part Numbers - ",paste("PN ",title.PN," - ")),
                      ifelse(title.RC=="ALL","All Removal Intervals",paste("Removal Interval ",title.RC))))
  title(sub="Flight Hours",line=2)
  abline(h=c(log(-log(1-c(.001,.01,.1)))),v=c(c(seq(-1,3))),col=rgb(1,0,0)) # Red
  abline(h=c(log(-log(1-c(.003,.02,.05,.25,.5,.75,.9,.96,.99,.999)))),col=rgb(38/255,201/255,38/255))
  abline(v=c(log10(seq(7,9)),log10(seq(20,90,10)),log10(seq(200,900,100)),log10(seq(2000,9000,1000))),col=rgb(38/255,201/255,38/255)) # Green
  #fitted line
  line.41<-qweibull(seq(0.0001,.99,.0005),params[1],params[2],log.p=F)
  lines(x=log10(line.41),y=log(-log(1-seq(.0001,.99,.0005))))
  #legend
  legend(x="topleft",legend=c(paste("Shape: ",round(params[1],digits=3)),paste("Scale: ",round(params[2],digits=3)),
                              paste("Mean: ",round(params[2]*gamma(1+1/params[1]),digits=3)),paste("Observed: ",length(both$p[which(both$cens==1)])),
                              paste("Censored: ",length(both$p[which(both$cens==0)]))),cex=.85,ncol=1,inset=c(-0.05,-0.03))
  #histogram (if there are censored values)
  if(length(both$p[which(both$cens==0)])==0) { # just plot a zero at median of non-censored hours
    text(x=log10(median(both$interval_value[which(both$cens==1)])),y=-7,labels="0")
  } else { # plot the histogram
  par(new=T,mar=c(3.14,4.1,4.1,2.1)) #5.96 x 4.54 to line up the histogram to the bottom of the plot area
  histholder<-hist(log10(both$interval_value[which(both$cens==0)]),breaks="Sturges",plot=F) # to get the bin heights and counts
  histplot<-hist(log10(both$interval_value[which(both$cens==0)]),breaks="Sturges",xlim=c(.9,log10(max(both$interval_value))+.02),ylim=c(0,max(histholder$counts)*2.2),border=1,col=rgb(149/255,184/255,251/255),ylab=NULL,xlab=NULL,main=NULL,labels=T,axes=F,plot=T)
  } # end if there is enough data to plot the histogram
  options(warn=-1)
  par(op)
  options(warn=0)
  invisible(dev.off())
} # end plot Function


###plotting functions###
#fix the scale to Weibull:
# draw the plot:
weibplot <- function(x,y,log='xy',...,forceylim=c(0,0),forcexlim=c(0,0))
{
  x <- log(x,10)
  y <- log(-log(1-y))
  xlg = TRUE # hard-coded for now
  ylg = TRUE
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