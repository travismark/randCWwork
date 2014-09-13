

CensUncensm1 <- function(df,specCode=1) {
  # Transforms a subset of the repair/removals ERO data into a format digestible for fitdistcens() functions - 1st removal
  # Args:
  #   df: a subset of the repair/removals data as broken down by a ddply() call according to specific and general classifiers
  #   specCode: a code for which removals to return: 1st(1), 2nd(2), or all-except-1st(3)
  # Out:
  #   newdf: the repair/removals data as a two-column dataframe with just the TOW
  if(specCode==1){
    if(sum(is.na(df$FstSttDt))==0) {censdf<-NULL # set to null b/c empty
    } else { censdf<-data.frame(df[is.na(df$FstSttDt),"diff1"],NA) # paste0("diff",1) to use other diffs
             colnames(censdf)<-c("left","right")}
    if(sum(!is.na(df$FstSttDt))==0) {uncensdf<-NULL # set to null b/c empty
    } else {uncensdf<-data.frame(df[!is.na(df$FstSttDt),"diff1"],df[!is.na(df$FstSttDt),"diff1"])
            colnames(uncensdf)<-c("left","right")}
    newdf<-rbind(censdf,uncensdf)
    (newdf)
  }
  else if(specCode==2){
    # take out cases that never fail the first time b/c they've already been censored in diff1 (they were never on test)
    #df<-df[-which(is.na(df$FstSttDt) & is.na(df$ScdSttDt)),]   
    # 2nd removal and on  sometimes has missing diffs where Start date isn't null b/c the diffs were zero or negative - remove these rows
    #df<-df[-which(is.na(df$diff2) & !is.na(df$ScdSttDt)),]
    # take out cases where 1st failures are the very last thing to happen to a part - therefore suspension time for 2nd removal is zero, and converted to NA
    #df<-df[-which(df$LstSttDt==df$FstSttDt),]
    df<-df[!is.na(df$diff2),]
    if(sum(is.na(df$ScdSttDt))==0) {censdf<-NULL # set to null b/c empty
    } else { censdf<-data.frame(df[is.na(df$ScdSttDt),"diff2"],NA) # paste0("diff",1) to use other diffs
             colnames(censdf)<-c("left","right")}
    if(sum(!is.na(df$ScdSttDt))==0) {uncensdf<-NULL # set to null b/c empty
    } else {uncensdf<-data.frame(df[!is.na(df$ScdSttDt),"diff2"],df[!is.na(df$ScdSttDt),"diff2"])
            colnames(uncensdf)<-c("left","right")}
    newdf<-rbind(censdf,uncensdf)
    (newdf)
  }
  else if (specCode==3){ # 2nd, 3rd, 4th, 5th removals
    df2<-df[!is.na(df$diff2),] # take out rows with diff2==NA b/c they aren't under test or valid (0 or negative fail time)
    if(sum(is.na(df2$ScdSttDt))==0) {censdf2<-NULL # set to null b/c empty
    } else { censdf2<-data.frame(df2[is.na(df2$ScdSttDt),"diff2"],NA) # paste0("diff",1) to use other diffs
             colnames(censdf2)<-c("left","right")}
    if(sum(!is.na(df2$ScdSttDt))==0) {uncensdf2<-NULL # set to null b/c empty
    } else {uncensdf2<-data.frame(df2[!is.na(df2$ScdSttDt),"diff2"],df2[!is.na(df2$ScdSttDt),"diff2"])
            colnames(uncensdf2)<-c("left","right")}
    df3<-df[!is.na(df$diff3),]
    if(sum(is.na(df3$ThdSttDt))==0) {censdf3<-NULL # set to null b/c empty
    } else { censdf3<-data.frame(df3[is.na(df3$ThdSttDt),"diff3"],NA) # paste0("diff",1) to use other diffs
             colnames(censdf3)<-c("left","right")}
    if(sum(!is.na(df3$ScdSttDt))==0) {uncensdf3<-NULL # set to null b/c empty
    } else {uncensdf3<-data.frame(df3[!is.na(df3$ThdSttDt),"diff3"],df3[!is.na(df3$ThdSttDt),"diff3"])
            colnames(uncensdf3)<-c("left","right")}
    df4<-df[!is.na(df$diff4),]
    if(sum(is.na(df4$FurSttDt))==0) {censdf4<-NULL # set to null b/c empty
    } else { censdf4<-data.frame(df4[is.na(df4$FurSttDt),"diff4"],NA) # paste0("diff",1) to use other diffs
             colnames(censdf4)<-c("left","right")}
    if(sum(!is.na(df$FurSttDt))==0) {uncensdf4<-NULL # set to null b/c empty
    } else {uncensdf4<-data.frame(df4[!is.na(df4$FurSttDt),"diff4"],df4[!is.na(df4$FurSttDt),"diff4"])
            colnames(uncensdf4)<-c("left","right")}
    df5<-df[!is.na(df$diff5),]
    if(sum(is.na(df5$FvhSttDt))==0) {censdf5<-NULL # set to null b/c empty
    } else { censdf5<-data.frame(df5[is.na(df5$FvhSttDt),"diff5"],NA) # paste0("diff",1) to use other diffs
             colnames(censdf5)<-c("left","right")}
    if(sum(!is.na(df5$FvhSttDt))==0) {uncensdf5<-NULL # set to null b/c empty
    } else {uncensdf5<-data.frame(df5[!is.na(df5$FvhSttDt),"diff5"],df5[!is.na(df5$FvhSttDt),"diff5"])
            colnames(uncensdf5)<-c("left","right")}
    newdf<-rbind(censdf2,uncensdf2,censdf3,uncensdf3,censdf4,uncensdf4,censdf5,uncensdf5)
  }
} # end CensUncens


getDistribsFromSubsetm1 <- function(df, distrib, plot, catgs,modkm, plotdir,unbug,ontwth)  {
  # Outputs a row with fitted weibulls and can generate a weibull plot in the specified directory
  # Args:
  #   df: a subset of the removals/repair days-to-removal data as broken down by a previous call to ddply()
  #   distrib: for now either weibull (failure rates) or lognormal (shipping times) - both use fitdistrplus's equations
  #       weibull takes a totals db and uses suspensions, lognormal takes a rawEros db and will not use suspensions
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

  if(distrib=="weibull"){
    #shape the totals table into a two-column dataframe with time on wing and censored information for fitdistcens()
    newdf<-CensUncensm1(df,specCode=ontwth) # if I want to to 1 and 2, or 1 and 3, I need to make ontwth different than the code I pass here
    #calculate a weibull from that new 2-column dataframe
    if(unbug){c(print(df[1,],dim(newdf),newdf[1,]),sum(!is.na(newdf[,2])))}
    options(warn=-1)
    # weibull fails with too few points and other reasons - can adjust starting conditions and retry (or return all NAs)
    # sometimes I get a singlur within the fit
    weib<-tryCatch(fitdistcens(newdf,"weibull"),error=function(cond){return(NA)})
    if(unbug){print("done fitting the weibull")}
    # if fit was successful, perform a few more steps
    if(class(weib)=="fitdistcens"){
        options(warn=0)
        #find the modified Kaplan Meier rank and the fitted values 
        if (modkm) {
          rankedpoints<-getModKM(newdf,weib[[1]]) # takes cens/uncens TOW and weibull parameters
        }
        #find the exponential parameters and the beta = 1 p value
        betaout<-tryCatch(BetaTest(newdf,weib$loglik),error=function(cond){return(NA)})
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
        out[1,6]<-length(is.na(newdf$right)[is.na(newdf$right)==FALSE]) # uncensored events
        out[1,7]<-length(is.na(newdf$right)[is.na(newdf$right)==TRUE]) # censored
        #out[1,8]<-weib$loglik # negative log likelihood of the weibull fit (will be used to compare fits)
        out[1,8]<-betaout[[1]] # get the Log-Likelihood Ratio Test p-value for Beta=1 test (approximation of Minitab Wald Test p-value)
        out[1,9]<-tryCatch(betaout[[2]],error=function(cond){return(NA)}) # exponential scale (1/rate) parameter
      } # end if weibull fit
    else {
      out<-data.frame(NA,NA,NA,NA,NA,0,0,NA,NA)
      if(ontwth==1){
        out[1,6]<-sum(!is.na(df$FstSttDt))
        out[1,7]<-sum(is.na(df$FstSttDt))
      }
      if(ontwth==2){
        df<-df[!is.na(df$diff2),] # take out rows with diff2==NA b/c they aren't under test or valid (0 or negative fail time)
        out[1,6]<-sum(!is.na(df$ScdSttDt))
        out[1,7]<-sum(is.na(df$ScdSttDt))
      }
      if(ontwth==3){
        df2<-df[!is.na(df$diff2),] # take out rows with diff2==NA b/c they aren't under test or valid (0 or negative fail time)
        df3<-df[!is.na(df$diff3),];df4<-df[!is.na(df$diff4),];df5<-df[!is.na(df$diff5),]
        out[1,6]<-sum(!is.na(df[!is.na(df$diff2),]$ScdSttDt))+sum(!is.na(df[!is.na(df$diff3),]$ThdSttDt))+
          sum(!is.na(df[!is.na(df$diff4),]$FurSttDt))+sum(!is.na(df[!is.na(df$diff5),]$FvhSttDt))
        out[1,7]<-sum(is.na(df[!is.na(df$diff2),]$ScdSttDt))+sum(is.na(df[!is.na(df$diff3),]$ThdSttDt))+
          sum(is.na(df[!is.na(df$diff4),]$FurSttDt))+sum(is.na(df[!is.na(df$diff5),]$FvhSttDt))
      }
      # no plot generated 
    } # end else oh the weibull didn't fit
    colnames(out)<-c("shape","scale","shapeSE","scaleSE","MeanTime","Events","Censored","BetaTestPval","ExpoScale") 
    (out)
  }
  else if(distrib=="lognormal"){
    leadtimes<-df[!is.na(df$LeadTime),"LeadTime"]
    oneleadtime<-tryCatch(fitdist(leadtimes,densfun="lognormal"),error=function(cond){return(NA)})
    if(unbug){print(length(leadtimes))}
    # if fit was successful, perform a few more steps
    if(class(oneleadtime)=="fitdistr"){
      if(unbug){print("successful fit")}
      out<-cbind(t(data.frame(oneleadtime[1])),t(data.frame(oneleadtime[2])))
      out<-as.data.frame(out)
      out[1,5]<-exp(oneleadtime[[1]][1]) # median time b/w order and receive
      out[1,6]<-exp(oneleadtime[[1]][1]+(oneleadtime[[1]][2])^2/2) # mean
      out[1,7]<-oneleadtime$n # events
    }
    else { # did not fit successfully
      out<-data.frame(NA,NA,NA,NA,0,0,0)
      out[1,5]<-mean(df$LeadTimes,na.rm=TRUE)
      out[1,5]<-mean(df$LeadTimes,na.rm=TRUE)
      out[1,5]<-sum(!is.na(df$LeadTimes))
  }
  colnames(out)<-c("meanlog","sdlog","meanlogSE","sdlogSE","MedianTime","MeanTime","Events") 
  (out)
  }
  else stop("that distribution does not qualify")
} # end getWeibullFromDF function

# this will only break out by specified classifiers
# uses GetWeibullsFromDF and CensUncens functions
gatherDistFitsm1<-function(weibulls_table,distrib="weibull",classTypes="NSN",ontwth=1,verbose=FALSE,unbug=FALSE,plot=FALSE,modkm=FALSE,plotdir=getwd()){
  # Args
    # ontwth:  code for which weibulls to fit
        # 1: just 1st failure; 2: just 1st and 2nd failure; 3: 1st and group all other failures into a second failure; 4: both 2 and 3
  # Returns
    # data frame with all
  if(plot==TRUE & modkm==FALSE) {stop("modified kaplan meier statistics are required for plotting")}
  if(verbose==FALSE){print("Please Wait")}
  # Six classifier columns including WUC, which is always specified/included
  # Total of 5 Choose 5:0 combinations for a total of 32 calls to calloneddply()
  classcolnames<-classTypes#c("NSN")
  cls<-length(classcolnames) # number of classifier columns, including WUC
  first <- 1 # initialize a dummy binary variable to start or append to a data frame
  weibs<-data.frame()  #  initialize an empty data frame to hold output
  
  for(ii in seq(from=0,to=(cls-1))) { # loop once per classifier column [except wuc] 
    # call combn() one time during an interation of this outer loop
    mat<-combn(cls:2,ii) # gives all the ways to choose 'ii' digits out of quantity of classifier columns less one [b/c wuc is always in]
    for(jj in seq_len(dim(mat)[2])) {
      # call calloneddply() once per column of the combn output
      weibs1<-calloneDDPLYm1(weibulls_table,distrib,include=sort(c(1,mat[,jj])),plots=plot,classcolnames,modkm,plotdir,verbose,unbug,ontwth,classTypes)
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


calloneDDPLYm1<-function(weibulls_table,distrib,include,plots,classifiernames,modkm,plotdir,verbose,unbug,ontwth,classTypes) {
  includenames<-classifiernames[include]
  excludenames<-classifiernames[-include]
  if(verbose){print(includenames)}
  # Define classifiers as one long string: (for loop is a fail, but I'm stumped)
  classifiers<-as.character("") # initialize
  for (ii in seq(include)) {
    classifiers<-paste(classifiers,includenames[ii],sep="")
  }
  # Call the ddply
  weibs<-ddply(weibulls_table,includenames,getDistribsFromSubsetm1,distrib,plot=plots,catgs=classifiers,modkm,plotdir,unbug,ontwth)
  # Add the "ALL" description columns to the end of the data frame
  for (ii in seq(excludenames)) {
    weibs[,length(weibs)+1]<-"ALL"
    names(weibs)[length(weibs)]<-excludenames[ii]
  }
  numericalnames<-colnames(weibs)[-seq(length(classTypes))]
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

