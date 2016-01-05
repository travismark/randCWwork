### Fit many weibulls and produce weibull plots

# Define Functions: #

CensUncens <- function(df) {
  # Transforms a subset of the repair/removals TOW data into a format digestible for fitdistcens() functions
  # Args:
  #   df: a subset of the repair/removals data as broken down by a ddply() call according to specific and general classifiers
  # Out:
  #   newdf: the repair/removals data as a two-column dataframe with just the TOW
  
  if(dim(subset(df,CENS==0))[1]==0) {censdf<-NULL # set to null b/c empty
  } else { censdf<-data.frame(subset(df,CENS==0)$TOW,NA)
          colnames(censdf)<-c("left","right")}
  if(dim(subset(df,CENS==1))[1]==0) {uncensdf<-NULL # set to null b/c empty
  } else {uncensdf<-data.frame(subset(df,CENS==1)$TOW,subset(df,CENS==1)$TOW)
          colnames(uncensdf)<-c("left","right")}
  newdf<-rbind(censdf,uncensdf)
  (newdf)
} # end CensUncens


getWeibullFromDF <- function(df, plot=FALSE, catgs="",modkm, plotdir,unbug)  {
  # Outputs a row with fitted weibulls and can generate a weibull plot in the specified directory
  # Args:
  #   df: a subset of the removals/repair TOW data as broken down by a previous call to ddply()
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
  
  #make sure the passed data frame has enough data: at least three failure/removal/events
  #      (anything less may give errors in fitting the weibull with mle method)
  if(length(df[df$CENS==1,8])<3) {
    # with too little data:  report the number of events and give NAs for everything else   
    out<-data.frame(NA,NA,NA,NA,NA,0,0,NA,NA,NA,NA,NA,NA,NA)
    colnames(out)<-c("shape","scale","shapeSE","scaleSE","MeanTime","Events","Censored","NLogLik","BetaTestPval","AD*weib","AD*expo","AD*logn","AD*norm","ExpoScale")
    out[1,6]<-length(df[df$CENS==1,8])
    out[1,7]<-length(df[df$CENS==0,8])
    (out) # no plot generated
  } else {
      #shape the newTOW table into a two-column dataframe with time on wing and censored information for fitdistcens()
      newdf<-CensUncens(df)
      # make sure the causal failures are not all the same number;  otherwise the weibull is horizontal and impossible to fit
      if(length(unique(newdf[,2][complete.cases(newdf[,2])]))==1) {
        # with too little data:  report the number of events and give NAs for everything else   
        out<-data.frame(NA,NA,NA,NA,NA,0,0,NA,NA,NA,NA,NA,NA,NA)
        colnames(out)<-c("shape","scale","shapeSE","scaleSE","MeanTime","Events","Censored","NLogLik","BetaTestPval","AD*weib","AD*expo","AD*logn","AD*norm","ExpoScale")
        out[1,6]<-length(df[df$CENS==1,8])
        out[1,7]<-length(df[df$CENS==0,8])
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
        out[1,10]<-ADstats[1] # weibull AD* statistic
        out[1,11]<-ADstats[2] # exponential AD* statistic
        out[1,12]<-ADstats[3] # lognormal AD* statistic
        out[1,13]<-ADstats[4] # normal AD* statistic
        out[1,14]<-betaout[[2]] # exponential scale (1/rate) parameter
        colnames(out)<-c("shape","scale","shapeSE","scaleSE","MeanTime","Events","Censored","NLogLik","BetaTestPval","AD*weib","AD*expo","AD*logn","AD*norm","ExpoScale") 
        (out)
     } # end if there are different values in the causal events
  } # end if there are enough data
} # end getWeibullFromDF function


# Find the p value of the test comparing the weibull fit to the exponential fit
# takes as inputs: censored and uncensored times and the negative log likelihood of the weibull fit
BetaTest<-function(input,weibnll) {
  expofit<-fitdistcens(input,"weibull",start=list(scale=median(input$left)),fix.arg=list(shape=1)) # most likely expo fit to work
  teststat<-2*(weibnll-expofit$loglik)
  pvalue<-1-pchisq(teststat,1)
  (list(pval=pvalue,param=expofit[[1]]))
}


# this will always break out by all classifiers (must give it classifier names)
# uses GetWeibullsFromDF and CensUncens functions
gatherallweibulls <- function(weibulls_table,verbose=FALSE,unbug=FALSE,plot=FALSE,modkm=FALSE,plotdir=getwd()){
  if(plot==TRUE & modkm==FALSE) {stop("modified kaplan meier statistics are required for plotting")}
  if(verbose==FALSE){print("Please Wait")}
  # Six classifier columns including WUC, which is always specified/included
  # Total of 5 Choose 5:0 combinations for a total of 32 calls to calloneddply()
  classcolnames<-c("WUC","LOCATION","PN","NHA_PN","LAST_REPAIR","REPAIR_COUNT")
  numcolnames<-c("shape", "scale", "shapeSE", "scaleSE", "MeanTime", "Events", "Censored", 
                 "NLogLik","BetaTestPval", "AD*weib", "AD*expo", "AD*logn","AD*norm","ExpoScale")
  cls<-length(classcolnames) # number of classifier columns, including WUC
  first <- 1 # initialize a dummy binary variable to start or append to a data frame
  weibs<-data.frame()  #  initialize an empty data frame to hold output
  
  for(ii in seq(from=0,to=(cls-1))) { # loop once per classifier column [except wuc] 
    # call combn() one time during an interation of this outer loop
    mat<-combn(cls:2,ii) # gives all the ways to choose 'ii' digits out of quantity of classifier columns less one [b/c wuc is always in]
    for(jj in seq_len(dim(mat)[2])) {
      # call calloneddply() once per column of the combn output
      weibs1<-calloneDDPLY(weibulls_table,include=sort(c(1,mat[,jj])),plots=plot,classcolnames,numcolnames,modkm,plotdir,verbose,unbug)
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


calloneDDPLY<-function(weibulls_table,include,plots,classifiernames,numericalnames,modkm,plotdir,verbose,unbug) {
  includenames<-classifiernames[include]
  excludenames<-classifiernames[-include]
  if(verbose){print(includenames)}
  # Define classifiers as one long string: (for loop is a fail, but I'm stumped)
  classifiers<-as.character("") # initialize
  for (ii in seq(include)) {
    classifiers<-paste(classifiers,includenames[ii],sep="")
  }
  # Call the ddply
  weibs<-ddply(weibulls_table,includenames,getWeibullFromDF,plot=plots,catgs=classifiers,modkm,plotdir,unbug)
  # Add the "ALL" description columns to the end of the data frame
  for (ii in seq(excludenames)) {
    weibs[,length(weibs)+1]<-"ALL"
    names(weibs)[length(weibs)]<-excludenames[ii]
  }
  # Move description columns to the correct order
  weibs<-weibs[,c(classifiernames,numericalnames)]
  (weibs)
} # end calloneDDPLY



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
  weibplot(both$tow[which(both$cens==1)],both$p[which(both$cens==1)],forcexlim=c(.9,log10(max(both$tow))+.02),
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
    text(x=log10(median(both$tow[which(both$cens==1)])),y=-7,labels="0")
  } else { # plot the histogram
  par(new=T,mar=c(3.14,4.1,4.1,2.1)) #5.96 x 4.54 to line up the histogram to the bottom of the plot area
  histholder<-hist(log10(both$tow[which(both$cens==0)]),breaks="Sturges",plot=F) # to get the bin heights and counts
  histplot<-hist(log10(both$tow[which(both$cens==0)]),breaks="Sturges",xlim=c(.9,log10(max(both$tow))+.02),ylim=c(0,max(histholder$counts)*2.2),border=1,col=rgb(149/255,184/255,251/255),ylab=NULL,xlab=NULL,main=NULL,labels=T,axes=F,plot=T)
  } # end if there is enough data to plot the histogram
  options(warn=-1)
  par(op)
  options(warn=0)
  invisible(dev.off())
} # end plot Function

# gives a data frame with the modified Kaplan Meier ranks and 
#   both Censored and non-Censored data
# takes a data frame with "left" and "right" columns from CensUncens
#   function and the weibull parameters from the fit
getModKM <- function(towtimes,params) {
  cens<-data.frame(tow=sort(towtimes$left[is.na(towtimes$right)==T]),
                   fitprob=pweibull(sort(towtimes$left[is.na(towtimes$right)==T]),params[1],params[2]))
  uncens<-data.frame(tow=sort(towtimes$left[is.na(towtimes$right)==F]),
                     fitprob=pweibull(sort(towtimes$left[is.na(towtimes$right)==F]),params[1],params[2]))
  if (length(cens[,1])>0) { # combine both data frames if there are both censored and 
  #  non-censored data points. Otherwise just use the non-censored data
      both<-rbind(cbind(cens,cens=0),cbind(uncens,cens=1))
      } else { both<-cbind(uncens,cens=1)} # no way there could be zero non-censored data points
      # b/c there are tests before this function is called in the getWeibullsFromDf function
  both<-both[order(both$tow),]
  both$rank<-seq(1,length(both$tow)) # rank censored and uncensored data together
  # calculate Modified Kaplan-Meier rank (just the non-censored events) with two for-loops
  for(jj in 1:length(both$tow)) { # find intermediate value
    both$p_intmd[jj]<-((length(both$tow)-jj)^both$cens[jj])/((length(both$tow)-jj+1)^both$cens[jj])
    #if(b==1){print(jj);b<-1000}else{b<-b-1}
  } # end make p intermediate
  for(jj in 1:length(both$tow)) { # calculate p rank (y-axis value)
    both$p_prime[jj]<-1-prod(both$p_intmd[1:jj]) # p prime
    if (jj==1) {
      both$p[jj]<-1-((1-both$p_prime[jj])+1)/2
    } else {
      both$p[jj]<-1-((1-both$p_prime[jj])+(1-both$p_prime[jj-1]))/2
    } # end if it's the first row
  } # end calculate p rank (y-axis value) 
  (both) # return both
} # end getModKM

# return a data frame with the four anderson darling adjusted statistics
# takes "both" data frame with modified Kaplan Meier ranks (empirical dist) and weibull fit
#     as well as original cens/uncens data and the exponential parameters
calcADAvals<-function(rankedpts, origTOW, expoparam) {
  # calculate the two missing distributions
  normalfit<-fitdistcens(origTOW,"norm")
  lognormalfit<-fitdistcens(origTOW,"lnorm")
  
  #only take the plot (noncensored) points
  rankedpts<-rankedpts[rankedpts$cens==1,] 
  rankedpts$rank<-seq_len(length(rankedpts[,1]))#recalculate the rank
  
  #find AD for the weibull distribution
  weibAD<-getOneADstat(rankedpts)
  
  #recalculate the fits for the exponential distribution
  rankedpts$fitprob<-pexp(q=rankedpts$tow,rate=1/expoparam[1]) # takes the rate
  #and find the AD stat for this
  expoAD<-getOneADstat(rankedpts)
  # do the same for Normal and Lognormal distributions
  rankedpts$fitprob<-plnorm(rankedpts$tow,meanlog=lognormalfit$estimate[1],sdlog=lognormalfit$estimate[2])
  lognormAD<-getOneADstat(rankedpts)
  rankedpts$fitprob<-pnorm(rankedpts$tow,mean=normalfit$estimate[1],sd=normalfit$estimate[2])
  normalAD<-getOneADstat(rankedpts)
  #output all four GOF stats
  out<-c(weibAD,expoAD,lognormAD,normalAD)
} # end calculate anderson darling statistics function

# gets an anderson darling adjusted stat
# requires the "both" dataframe with the right fitprob for that distribution
#     as well as the empirical modified Kaplan Meier value plot points (p)
getOneADstat<-function(rankedpts) {
  lgth<-length(rankedpts$p)
  # use the findArow function to get A values of each row, then add the n+1th row A value
  A<-sum(apply(X=as.matrix(rankedpts),MARGIN=1,FUN=findArow,fitprob=rankedpts$fitprob)) - (1-1E-12) - log(1-(1-1E-12)) +
    rankedpts$fitprob[lgth] + log(1-rankedpts$fitprob[lgth])
  B<-sum(apply(X=as.matrix(rankedpts),MARGIN=1,FUN=findBrow,fitprob=rankedpts$fitprob,nonpar=rankedpts$p)) +
           2*log(1-(1-1E-12))*rankedpts$p[lgth] - 2*log(1-rankedpts$fitprob[lgth])*rankedpts$p[lgth]
  C<-sum(apply(X=as.matrix(rankedpts),MARGIN=1,FUN=findCrow,fitprob=rankedpts$fitprob,nonpar=rankedpts$p)) +
           log(1-(1E-12))*rankedpts$p[lgth]^2 -
           log(1-(1-(1E-12)))*rankedpts$p[lgth]^2 -
           log(rankedpts$fitprob[lgth])*rankedpts$p[lgth]^2 +
           log(1-rankedpts$fitprob[lgth])*rankedpts$p[lgth]^2
  out<-lgth*(A+B+C)
}

# returns a column for the "both" data frame:  A in the AD* (anderson darling adjusted) calculation. 
# to be used in the AD* function and with apply by row;  needs the "both" data frame and the fitprob column from "both"
findArow<-function(input,fitprob) {
  if (input[4]==1) { # first row is special
    out<- (-input[2]) - log(1-input[2]) + 0 + log(1-0)
  } else { # not the first row
    out<- (-input[2]) - log(1-input[2]) + fitprob[input[4]-1] + log(1-(fitprob[input[4]-1]))
  } # end not the first row
} # end find A
# returns a column for the "both" data frame: B in the AD* calculation
findBrow<-function(input,fitprob,nonpar) {
  if (input[4]==1) { # first row is special
    out<- 0
  } else { # not the first row
    out<- 2*log(1-input[2])*nonpar[input[4]-1] - 2*log(1-fitprob[input[4]-1])*nonpar[input[4]-1] 
  } # end not the first row
} # end find B
# returns a column for the "both" data frame: C in the AD* calculation
findCrow<-function(input,fitprob,nonpar) {
  if (input[4]==1) { # first row is special
    out<- 0
  } else { # not the first row
    out<- log(input[2])*nonpar[input[4]-1]^2 - log(1-input[2])*nonpar[input[4]-1]^2 - 
      log(fitprob[input[4]-1])*nonpar[input[4]-1]^2 + log(1-fitprob[input[4]-1])*nonpar[input[4]-1]^2
  } # end not the first row
} # end find C

# testA<-apply(X=as.matrix(ranksubdf),MARGIN=1,FUN=findArow,fitprob=ranksubdf$fitprob)
# testB<-apply(X=as.matrix(ranksubdf),MARGIN=1,FUN=findBrow,fitprob=ranksubdf$fitprob,nonpar=ranksubdf$p)
# testC<-apply(X=as.matrix(ranksubdf),MARGIN=1,FUN=findCrow,fitprob=ranksubdf$fitprob,nonpar=ranksubdf$p)

## plot
# plot(uncens$tow,uncens$yval,log="xy")
#  http://rtricks.wordpress.com/2009/10/30/decimal-log-scale-on-a-plot/

#log-scale example:
# op<-par()
# lx <- seq(1,5, length=41)
# yl <- expression(e^{-frac(1,2) * {log[10](x)}^2})
# y <- exp(-.5*lx^2)
# par(mfrow=c(2,1), mar=par("mar")+c(0,1,0,0))
# plot(10^lx, y, log="xy", type="l", col="purple",
#      +      main="Log-Log plot", ylab=yl, xlab="x")
# plot(10^lx, y, log="xy", type="o", pch='.', col="forestgreen",
#      +      main="Log-Log plot with custom axes", ylab=yl, xlab="x",
#      +      axes = FALSE, frame.plot = TRUE)
# my.at <- 10^(1:5)
# axis(1, at = my.at, labels = formatC(my.at, format="fg"))
# at.y <- 10^(-5:-1)
# axis(2, at = at.y, labels = formatC(at.y, format="fg"), col.axis="red")
# par(op)

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