

CensUncensm1 <- function(df) {
  # Transforms a subset of the repair/removals ERO data into a format digestible for fitdistcens() functions
  # Args:
  #   df: a subset of the repair/removals data as broken down by a ddply() call according to specific and general classifiers
  # Out:
  #   newdf: the repair/removals data as a two-column dataframe with just the TOW
  
  if(sum(is.na(df$FstSttDt))==0) {censdf<-NULL # set to null b/c empty
  } else { censdf<-data.frame(df[is.na(df$FstSttDt),"diff1"],NA) # paste0("diff",1) to use other diffs
           colnames(censdf)<-c("left","right")}
  if(sum(!is.na(df$FstSttDt))==0) {uncensdf<-NULL # set to null b/c empty
  } else {uncensdf<-data.frame(df[!is.na(df$FstSttDt),"diff1"],df[!is.na(df$FstSttDt),"diff1"])
          colnames(uncensdf)<-c("left","right")}
  newdf<-rbind(censdf,uncensdf)
  (newdf)
} # end CensUncens

getWeibullFromDFm1 <- function(df, plot, catgs,modkm, plotdir,unbug,ontwth)  {
  # Outputs a row with fitted weibulls and can generate a weibull plot in the specified directory
  # Args:
  #   df: a subset of the removals/repair days-to-removal data as broken down by a previous call to ddply()
  #   plot: boolean if the function should call getPlotFromDF to save a plot
  #   catgs: text string of all the specific classifiers used in the previous steps to pass df to this function
  #   modkm: boolean if the function should calculate the modified kaplan meier plot points; required for plotting
  #          determines whether to calculate anderson darling statistics
  #   plotdir: directory to store plots
  #   unbug: boolean if unbug remarks should be printed to console
  # Out:
  #   out: a row with weibull & exponential paramaters and goodness of fit info for one set of classifiers 
  #        ddply will append this to the classifiers columns to make a row of the final output
  #   calls getPlotFromDF to make a plot, if option is on
  
  #make sure the passed data frame has enough data: at least thirty #prev. three# failure/removal/events
  #      (anything less may give errors in fitting the weibull with mle method)
  if(sum(!is.na(df$FstSttDt))<150) {
    # with too little data:  report the number of events and give NAs for everything else   
    out<-data.frame(NA,NA,NA,NA,NA,0,0,NA,NA,NA)
    colnames(out)<-c("shape","scale","shapeSE","scaleSE","MeanTime","Events","Censored","NLogLik","BetaTestPval","ExpoScale")
    out[1,6]<-sum(!is.na(df$FstSttDt))
    out[1,7]<-sum(is.na(df$FstSttDt))
    (out) # no plot generated
  } else {
    #shape the totals table into a two-column dataframe with time on wing and censored information for fitdistcens()
    newdf<-CensUncensm1(df)
    # make sure the causal failures are not all the same number;  otherwise the weibull is horizontal and impossible to fit
    if(length(unique(newdf[,2][complete.cases(newdf[,2])]))==1) {
      # with too little data:  report the number of events and give NAs for everything else   
      out<-data.frame(NA,NA,NA,NA,NA,0,0,NA,NA,NA)
      colnames(out)<-c("shape","scale","shapeSE","scaleSE","MeanTime","Events","Censored","NLogLik","BetaTestPval","ExpoScale")
      out[1,6]<-sum(!is.na(df$FstSttDt))
      out[1,7]<-sum(is.na(df$FstSttDt))
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
        rankedpoints<-getModKM(newdf,weib[[1]]) # takes cens/uncens TOW and weibull parameters
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
      out[1,10]<-betaout[[2]] # exponential scale (1/rate) parameter
      colnames(out)<-c("shape","scale","shapeSE","scaleSE","MeanTime","Events","Censored","NLogLik","BetaTestPval","ExpoScale") 
      (out)
    } # end if there are different values in the causal events
  } # end if there are enough data
} # end getWeibullFromDF function

# this will always break out by all classifiers (must hardcode classifier names)
# uses GetWeibullsFromDF and CensUncens functions
gatherallweibullsm1 <- function(weibulls_table,verbose=FALSE,unbug=FALSE,plot=FALSE,modkm=FALSE,plotdir=getwd(),ontwth=1){
  # Args
  # Returns
    # data frame with all
  if(plot==TRUE & modkm==FALSE) {stop("modified kaplan meier statistics are required for plotting")}
  if(verbose==FALSE){print("Please Wait")}
  # Six classifier columns including WUC, which is always specified/included
  # Total of 5 Choose 5:0 combinations for a total of 32 calls to calloneddply()
  classcolnames<-c("NSN")
  numcolnames<-c("shape", "scale", "shapeSE", "scaleSE", "MeanTime", "Events", "Censored", 
                 "NLogLik","BetaTestPval","ExpoScale")
                 #"NLogLik","BetaTestPval", "AD*weib", "AD*expo", "AD*logn","AD*norm","ExpoScale")
  cls<-length(classcolnames) # number of classifier columns, including WUC
  first <- 1 # initialize a dummy binary variable to start or append to a data frame
  weibs<-data.frame()  #  initialize an empty data frame to hold output
  
  for(ii in seq(from=0,to=(cls-1))) { # loop once per classifier column [except wuc] 
    # call combn() one time during an interation of this outer loop
    mat<-combn(cls:2,ii) # gives all the ways to choose 'ii' digits out of quantity of classifier columns less one [b/c wuc is always in]
    for(jj in seq_len(dim(mat)[2])) {
      # call calloneddply() once per column of the combn output
      weibs1<-calloneDDPLYm1(weibulls_table,include=sort(c(1,mat[,jj])),plots=plot,classcolnames,numcolnames,modkm,plotdir,verbose,unbug,ontwth)
      if(first==1) {
        weibs<-weibs1 # create
        first<-0
      } else {
        weibs<-rbind(weibs,weibs1)
      } # end else
    } # end inner for
  } # end outer for
  # convert description columns to factors
  for (jj in seq(from=2,to=length(classcolnames))){
    weibs[,jj]<-factor(weibs[,jj])
  }
  (weibs)
} # end gatherallweibulls


calloneDDPLYm1<-function(weibulls_table,include,plots,classifiernames,numericalnames,modkm,plotdir,verbose,unbug,ontwth) {
  includenames<-classifiernames[include]
  excludenames<-classifiernames[-include]
  if(verbose){print(includenames)}
  # Define classifiers as one long string: (for loop is a fail, but I'm stumped)
  classifiers<-as.character("") # initialize
  for (ii in seq(include)) {
    classifiers<-paste(classifiers,includenames[ii],sep="")
  }
  # Call the ddply
  weibs<-ddply(weibulls_table,includenames,getWeibullFromDFm1,plot=plots,catgs=classifiers,modkm,plotdir,unbug,ontwth)
  # Add the "ALL" description columns to the end of the data frame
  for (ii in seq(excludenames)) {
    weibs[,length(weibs)+1]<-"ALL"
    names(weibs)[length(weibs)]<-excludenames[ii]
  }
  # Move description columns to the correct order
  weibs<-weibs[,c(classifiernames,numericalnames)]
  (weibs)
} # end calloneDDPLY

# Find the p value of the test comparing the weibull fit to the exponential fit
# takes as inputs: censored and uncensored times and the negative log likelihood of the weibull fit
BetaTest<-function(input,weibnll) {
  expofit<-fitdistcens(input,"weibull",start=list(scale=median(input$left)),fix.arg=list(shape=1)) # most likely expo fit to work
  teststat<-2*(weibnll-expofit$loglik)
  pvalue<-1-pchisq(teststat,1)
  (list(pval=pvalue,param=expofit[[1]]))
}