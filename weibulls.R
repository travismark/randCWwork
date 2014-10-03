library("fitdistrplus")
library("plyr")
#setwd("C:/Users/tbaer/Desktop/cbm/october redo weibulls/R work")
#weibulls_initial<-read.csv("newTOW_LOC.csv",header=T)


#shapes a data frame (with columns named SumOfTow and Censored) into
# one appropriate for giving to the fitdistcens function
CensUncens <- function(df) {
  if(dim(subset(df,Censored==1))[1]==0) {censdf<-NULL # set to null b/c empty
  } else { censdf<-data.frame(subset(df,Censored==1)$SumOfTOW,NA)
           colnames(censdf)<-c("left","right")}
  if(dim(subset(df,Censored==0))[1]==0) {uncensdf<-NULL # set to null b/c empty
  } else {uncensdf<-data.frame(subset(df,Censored==0)$SumOfTOW,subset(df,Censored==0)$SumOfTOW)
          colnames(uncensdf)<-c("left","right")}
  newdf<-rbind(censdf,uncensdf)
  (newdf)
} # end CensUncens

#Outputs a row with fitted weibulls and can generate a weibull plot in the working dir.
#takes a data frame (newTOW_Loc), a plot binary if a plot should be generated, and the grouping categories passed previously to ddply
#requires a column named "SumOfTOW" and "Censored"
#uses CensUncens to break apart the data
getWeibullsFromDF <- function(df, plot=0, catgs="")  {
  #make sure the passed data frame has enough data: at least two failure/removal/events
  #      (anything less gives errors in fitting the weibull with mle method)
  if(length(df[df$Censored==0,8])<2) {
    # with too little data:  report the number of events and give NAs for everything else   
    out<-data.frame(NA,NA,NA,NA,NA,0,0,NA)
    colnames(out)<-c("shape","scale","shapeSE","scaleSE","MeanTime","Events","Censored","BetaTestPval")
    out[1,6]<-length(df[df$Censored==0,8])
    out[1,7]<-length(df[df$Censored==1,8])
    (out) # no plot generated
    } else {
  #print(df[1,c(1,2,4,5)]) # debug
  #shape the newTOW table into a two-column dataframe with time on wing and censored information
  newdf<-CensUncens(df)
  #calculate a weibull from that new 2-column dataframe
  options(warn=-1)
  weib<-fitdistcens(newdf,"weibull")
  options(warn=0)
  #find the modified Kaplan Meier rank and the fitted values 
  rankedpoints<-getModKM(newdf,weib[[1]]) # takes cens/uncens TOW and weibull parameters
  #calculate Anderson Darling Adjusted statistics for weibull,exponential,lognormal and normal distributions
  
  
  #save a plot of the weibull
  if (plot==1) {
    getPlotFromDF(rankedpoints, head(df), weib[[1]], catgs) #ranked points, sample of input data, break-out categories
  }
  
  # save the scale and shape parameters & their standard errors 
  #   & # of events and beta-test-p-value in a dataframe
  out<-cbind(t(data.frame(weib[1])),t(data.frame(weib[2])))
  out<-as.data.frame(out)
  out[1,5]<-weib[[1]][2]*gamma(1+1/weib[[1]][1]) # mean time b/w events (scale*gamma(1+1/shape))
  out[1,6]<-length(is.na(newdf$right)[is.na(newdf$right)==F]) # uncensored events
  out[1,7]<-length(is.na(newdf$right)[is.na(newdf$right)==T]) # censored
  out[1,8]<-BetaTest(newdf,weib$loglik) # get the Log-Likelihood Ratio Test p-value (approximation of Minitab p-value)
  colnames(out)<-c("shape","scale","shapeSE","scaleSE","MeanTime","Events","Censored","BetaTestPval") 
  (out)
    } # end if there are enough data
} # end getWeibullsFromDF function


#call the function to split out by WUC, location, PN, and repair interval
#testply<-ddply(weibulls_initial,c("WUC","Loc","PN","adjRepInt"),getWeibullsFromDF)
#call the function to split out by WUC, location, PN (all repair intervals)
#testply1<-ddply(weibulls_initial,c("WUC","Loc","PN"),getWeibullsFromDF)
#individual WUCs
#testply2<-ddply(weibulls_initial,"WUC",getWeibullsFromDF)
#rename the final two columns to clarify that they're standard error
#colnames(testply)[c(length(colnames(testply))-1,length(colnames(testply)))]<-c("shapeSE","scaleSE")

#perform statistical tests

# Find the p value of the test comparing the weibull fit to the exponential fit
# takes as inputs: censored and uncensored times and the negative log likelihood of the weibull fit
BetaTest<-function(input,weibnll) {
  expofit<-fitdistcens(input,"weibull",start=list(scale=median(input$right,na.rm=T)),fix.arg=list(shape=1))
  teststat<-2*(weibnll-expofit$loglik)
  (1-pchisq(teststat,1))
}


# this will always break out by WUC, Location, PN, and adjRepairInterval (hard-coded)
# uses GetWeibullsFromDF and CensUncens functions
gatherallweibulls <- function(weibulls_table){
  # Full breakout weibulls:
  weibs<-ddply(weibulls_table,c("WUC","Loc","PN","adjRepInt"),getWeibullsFromDF,plot=1,catgs="WUCLocPNadjRepInt")
  # Make the adjRepInt a factor
  weibs$adjRepInt<-factor(weibs$adjRepInt)
  # Get the default weibulls:
  #  1: All Repair Intervals
    print(1)
    weibs1<-ddply(weibulls_table,c("WUC","Loc","PN"),getWeibullsFromDF,plot=1,catgs="WUCLocPN")
    # add in the ALL
    tempdf<-weibs1[,4:11]  # hold onto this data
    weibs1[,4]<-factor("ALL")
    weibs1[,5:12]<-tempdf  # return data to new df
    colnames(weibs1)<-c("WUC","Loc","PN","adjRepInt","shape","scale","shapeSE","scaleSE","MeanTime","Events","Censored","BetaTestPval")
  #  2: All Locations
    print(2)
    weibs2<-ddply(weibulls_table,c("WUC","PN","adjRepInt"),getWeibullsFromDF,plot=1,catgs="WUCPNadjRepInt")
    tempdf<-weibs2[,2:11]  # hold onto this data
    weibs2[,2]<-factor("ALL")
    weibs2[,3:12]<-tempdf  # return data to new df
    colnames(weibs2)<-c("WUC","Loc","PN","adjRepInt","shape","scale","shapeSE","scaleSE","MeanTime","Events","Censored","BetaTestPval")
  #  3: All Part Numbers
    print(3)
    weibs3<-ddply(weibulls_table,c("WUC","Loc","adjRepInt"),getWeibullsFromDF,plot=1,catgs="WUCLocadjRepInt") 
    tempdf<-weibs3[,3:11]  # hold onto this data
    weibs3[,3]<-factor("ALL")
    weibs3[,4:12]<-tempdf  # return data to new df
    colnames(weibs3)<-c("WUC","Loc","PN","adjRepInt","shape","scale","shapeSE","scaleSE","MeanTime","Events","Censored","BetaTestPval")
  #  4: All Repair Intervals and Locations
    print(4)
    weibs4<-ddply(weibulls_table,c("WUC","PN"),getWeibullsFromDF,plot=1,catgs="WUCPN")  
    tempdf<-weibs4[,3:10]  # hold onto this data
    weibs4[,3]<-weibs4[,2] # move PN data over a column
    weibs4[,c(2,4)]<-factor("ALL")
    weibs4[,5:12]<-tempdf  # return data to new df
    colnames(weibs4)<-c("WUC","Loc","PN","adjRepInt","shape","scale","shapeSE","scaleSE","MeanTime","Events","Censored","BetaTestPval")
  #  5: All Repair Intervals and Part Numbers
    print(5)
    weibs5<-ddply(weibulls_table,c("WUC","Loc"),getWeibullsFromDF,plot=1,catgs="WUCLoc")  
    tempdf<-weibs5[,3:10]  # hold onto this data
    weibs5[,3:4]<-factor("ALL")
    weibs5[,5:12]<-tempdf  # return data to new df
    colnames(weibs5)<-c("WUC","Loc","PN","adjRepInt","shape","scale","shapeSE","scaleSE","MeanTime","Events","Censored","BetaTestPval")
  #  6: All Locations and Part Numbers
    print(6)
    weibs6<-ddply(weibulls_table,c("WUC","adjRepInt"),getWeibullsFromDF,plot=1,catgs="WUCadjRepInt")  
    tempdf<-weibs6[,3:10]  # hold onto this data
    weibs6[,4]<-weibs6[,2] #move adjRepInt data over two columns
    weibs6[,2:3]<-factor("ALL")
    weibs6[,5:12]<-tempdf  # return data to new df
    colnames(weibs6)<-c("WUC","Loc","PN","adjRepInt","shape","scale","shapeSE","scaleSE","MeanTime","Events","Censored","BetaTestPval")
  #  7:  All Locations, Part Numbers, and Repair Intervals
    print(7)
    weibs7<-ddply(weibulls_table,"WUC",getWeibullsFromDF,plot=1,catgs="WUC") 
    tempdf<-weibs7[,2:9]  # hold onto this data
    weibs7[,2:4]<-factor("ALL")
    weibs7[,5:12]<-tempdf  # return data to new df
    colnames(weibs7)<-c("WUC","Loc","PN","adjRepInt","shape","scale","shapeSE","scaleSE","MeanTime","Events","Censored","BetaTestPval")
    #combine all sets
    weibs<-rbind(weibs,weibs1,weibs2,weibs3,weibs4,weibs5,weibs6,weibs7)
}

#then<-proc.time()[3]
#allweibulls<- gatherallweibulls(weibulls_initial) #ALL THE WEIBULLz
#proc.time()[3]-then
#write.csv(allweibulls,file="Apache Weibulls.csv",quote=F)

# plot a probability graph plot of the events and their fit 
#   overlayed with a histogram of the censored values
# called from within getweibullsfromdf
# towtimes:  newdf from getweibullsfromdf function:  includes time of event and censor
# params: two parameters for the weibull distribution
# catgs: grouping categories passed to ddply to define which factors to not use "ALL"
# uses Modified Kaplan-Meier method for plotting uncensored and censored data
getPlotFromDF<- function(both, df, params, catgs) {
  # assign titles for the plot
  title.WUC<-df$WUC[1]
  title.Loc<-ifelse(strsplit(catgs,"Loc")==catgs,"ALL",as.character(df$Loc[1]))
  title.Loc<-ifelse(title.Loc=="N/A","NA",title.Loc) # png name can't have a slash in it
  title.PN<-ifelse(strsplit(catgs,"PN")==catgs,"ALL",as.character(df$PN[1]))
  title.aRI<-ifelse(strsplit(catgs,"adjRepInt")==catgs,"ALL",as.character(df$adjRepInt[1]))
  
  # Plot
  op<-par() # save the plot parameters
  #dots
  path<-"C:/Users/tbaer/Desktop/cbm/october redo weibulls/R work/plots/"
  png(file = paste(path,"WeibullPlot._WUC_",title.WUC,";Loc_",title.Loc,";PN_",title.PN,";RepInt_",
                   title.aRI,".png",sep=""), width=5.96, height=4.54, units="in", res=144)
  weibplot(both$tow[which(both$cens==1)],both$p[which(both$cens==1)],forcexlim=c(.9,log10(max(both$tow))+.02),
           forceylim=c(-7,0), xlab="Hours Elapsed",ylab="Percent",col=rgb(255/255,0/255,0/255),pch=16,
           main=paste("WeibullPlot: WUC ",title.WUC,"; Loc ",title.Loc,"\n PN ",title.PN,"; RepInt ",title.aRI))
  abline(h=c(log(-log(1-c(.001,.01,.1)))),v=c(c(seq(-1,3))),col=rgb(1,0,0)) # Red
  abline(h=c(log(-log(1-c(.003,.02,.05,.25,.5,.75,.9,.96,.99,.999)))),col=rgb(38/255,201/255,38/255))
  abline(v=c(log10(seq(7,9)),log10(seq(20,90,10)),log10(seq(200,900,100)),log10(seq(2000,9000,1000))),col=rgb(38/255,201/255,38/255)) # Green
  #fitted line
  line.41<-qweibull(seq(0.0001,.99,.0005),params[1],params[2],log.p=F)
  lines(x=log10(line.41),y=log(-log(1-seq(.0001,.99,.0005))))
  #histogram (if there are censored values)
  if(length(both$p[which(both$cens==0)])==0) { # just plot a zero at median of non-censored hours
    text(x=log10(median(both$tow[which(both$cens==1)])),y=-7,labels="0")
    #text(x=log10(150),y=log(-log(1-(.001))),labels="0")
  } else { # plot the histogram
  par(new=T,mar=c(4.58,4.1,4.1,2.1)) #5.96 x 4.54 to line up the histogram to the bottom of the plot area
  histholder<-hist(log10(both$tow[which(both$cens==0)]),breaks="FD",plot=F) # to get the bin heights and counts
  histplot<-hist(log10(both$tow[which(both$cens==0)]),breaks="FD",xlim=c(.9,log10(max(both$tow))+.02),ylim=c(0,max(histholder$counts)*2.2),border=1,col=rgb(149/255,184/255,251/255),ylab=NULL,xlab=NULL,main=NULL,labels=T,axes=F,plot=T)
  } # end if there is enough data to plot the histogram
  options(warn=-1)
  par(op)
  options(warn=0)
  invisible(dev.off())
} # end Function

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
} # end Function

# return a data frame with the four anderson darling adjusted statistics
# takes "both" data frame with modified Kaplan Meier ranks (empirical dist) and weibull fit
#     as well as original cens/uncens data
calcADAvals<-function(rankedpts, origTOW) {
  
  
}

# gets an anderson darling adjusted stat
# requires the "both" dataframe with the right fitprob for that distribution
#     as well as the empirical modified Kaplan Meier value (p)
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
  print(c(A,B,C))
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

#testA<-apply(X=as.matrix(ranksubdf),MARGIN=1,FUN=findArow,fitprob=ranksubdf$fitprob)
#testB<-apply(X=as.matrix(ranksubdf),MARGIN=1,FUN=findBrow,fitprob=ranksubdf$fitprob,nonpar=ranksubdf$p)
#testC<-apply(X=as.matrix(ranksubdf),MARGIN=1,FUN=findCrow,fitprob=ranksubdf$fitprob,nonpar=ranksubdf$p)

#####################################################

#par(op)

################################
#fix the scale to Weibull:
# draw the plot:
weibplot <- function(x,y,log='xy',...,forceylim=c(0,0),forcexlim=c(0,0))
{
  #par(tck=0.02)
  xlg <- FALSE
  ylg <- FALSE
  if('x'%in%strsplit(log,'')[[1]]){x <- log(x,10);xlg=TRUE}
  if('y'%in%strsplit(log,'')[[1]]){y <- log(-log(1-y));ylg=TRUE}
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

