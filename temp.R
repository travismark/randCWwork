library(survival)

## make a new fitdistcens function to output the hessian
fitdistcens2<-function (censdata, distr, start = NULL, fix.arg = NULL, ...) 
{
  if (missing(censdata) || !(is.vector(censdata$left) & is.vector(censdata$right) & 
                               length(censdata[, 1]) > 1)) 
    stop("datacens must be a dataframe with two columns named left \n            and right and more than one line")
  leftsupright <- censdata$left > censdata$right
  leftsupright <- leftsupright[!is.na(leftsupright)]
  if (any(leftsupright)) 
    stop("each left value must be less or equal to the corresponding right value")
  if (!is.character(distr)) 
    distname <- substring(as.character(match.call()$distr), 
                          2)
  else distname <- distr
  ddistname <- paste("d", distname, sep = "")
  if (!exists(ddistname, mode = "function")) 
    stop(paste("The ", ddistname, " function must be defined"))
  pdistname <- paste("p", distname, sep = "")
  if (!exists(pdistname, mode = "function")) 
    stop(paste("The ", pdistname, " function must be defined"))
  dots <- list(...)
  if (length(dots) == 0) 
    dots = NULL
  mle <- mledist(censdata, distname, start, fix.arg, ...)
  if (mle$convergence > 0) 
    stop("the function mle failed to estimate the parameters, \n        with the error code ", 
         mle$convergence)
  estimate <- mle$estimate
  varcovar <- solve(mle$hessian)
  sd <- sqrt(diag(varcovar))
  correl <- cov2cor(varcovar)
  loglik <- mle$loglik
  n <- nrow(censdata)
  npar <- length(estimate)
  aic <- -2 * loglik + 2 * npar
  bic <- -2 * loglik + log(n) * npar
  return(structure(list(estimate = estimate, sd = sd, cor = correl, hess = mle$hessian ,
                        loglik = loglik, aic = aic, bic = bic, censdata = censdata, 
                        distname = distname, fix.arg = as.list(fix.arg), dots = dots), 
                   class = "fitdistcens"))
}

subdf<-weibulls_initial[weibulls_initial$WUC=="06A" & weibulls_initial$PN=="7-311310001-41" ,]
weibSurv<-Surv(time=subdf$SumOfTOW,event=subdf$DeltaCens,type="right")
weibSurvFit<-survfit(weibSurv~subdf$Loc)
weibSurvReg<-survreg(weibSurv~1,dist="weibull")
a<-(scale=exp(weibSurvReg$icoef[1]))
b<-(shape=1/exp(weibSurvReg$icoef[2]))
normSurvReg<-survreg(weibSurv~1,dist="gaussian")
(mean=normSurvReg$icoef[1])
(sd=exp(normSurvReg$icoef[2]))
lognormSurvReg<-survreg(weibSurv~1,dist="lognormal")
expoSurvReg<-survreg(weibSurv~1,dist="exponential")


censsubdf<-CensUncens(subdf)
weibsubdf<-fitdistcens(censsubdf,"weibull")
nmsubdf<-fitdistcens(censsubdf,"norm")

weibSurvReg.byLoc<-survreg(weibSurv~Loc,data=subdf,dist="weibull")


catgs<-c("WUC","Loc")

# Move the Description columns around if necessary
if (length(moves) != 0) {
  for (ii in seq(length(moves))) {weibs[,moves[[ii]][2]]<-weibs[,moves[[ii]][1]]}
}

if (length(cols2skip)==0 | length(cols2skip)==(quantdesccol-1) | sum(cols2skip)==sum(quantdescol:(quantdescol-quantcols2skip+1))) { 
  # do nothing if zero OR all description columns missing OR if final columns are missing (no desc cols to the right of a missing column)
} else if 


#  1: Full breakout weibulls:
weibs1<-ddply(weibulls_table,c("WUC","Loc","PN","adjRepInt"),getWeibullsFromDF,plot=1,catgs="WUCLocPNPlatformLastRepairadjRepInt")
# Make the adjRepInt a factor
weibs$adjRepInt<-factor(weibs$adjRepInt)
weibullcolnames<-c("WUC","Loc","PN","adjRepInt","shape","scale","shapeSE","scaleSE","MeanTime","Events","Censored","BetaTestPval","AD*weib","AD*expo","AD*logn","AD*norm")
# Get the default weibulls:
#  2: All Repair Intervals
weibs2<-calloneDDPLY(weibulls_table,4,numerix,descrix,list())
#  3: All Locations
weibs3<-calloneDDPLY(weibulls_table,2,numerix,descrix,list(c(2,3),c(3,4)))
#  4: All Part Numbers
weibs4<-calloneDDPLY(weibulls_table,3,numerix,descrix,list(c(3,4)))
#  5: All Repair Intervals and Locations
weibs5<-calloneDDPLY(weibulls_table,c(2,4),numerix,descrix,list(c(2,3)))
#  6: All Repair Intervals and Part Numbers
weibs6<-calloneDDPLY(weibulls_table,c(3,4),numerix,descrix,list())
#  7: All Locations and Part Numbers
weibs7<-calloneDDPLY(weibulls_table,c(2,3),numerix,descrix,list(c(2,4)))
#  8:  All Locations, Part Numbers, and Repair Intervals
weibs8<-calloneDDPLY(weibulls_table,c(2,3,4),numerix,descrix,list())
#combine all sets
weibs<-rbind(weibs1,weibs2,weibs3,weibs4,weibs5,weibs6,weibs7,weibs8)

#################################################
#####USE THE SURVIVAL PACKAGE INSTEAD############
#################################################
#Outputs a row with fitted weibulls and can generate a weibull plot in the working dir.
#takes a data frame (newTOW_Loc), a plot binary if a plot should be generated, and the grouping categories passed previously to ddply
#requires a column named "SumOfTOW" and "Censored"
#uses CensUncens to break apart the data
getWeibullsFromDF1 <- function(df, plot=0, catgs="")  {
  #make sure the passed data frame has enough data: at least two failure/removal/events
  #      (anything less gives errors in fitting the weibull with mle method)
  if(length(df[df$DeltaCens==1,8])<2) {
    # with too little data:  report the number of events and give NAs for everything else   
    out<-data.frame(NA,NA,NA,NA,NA,0,0,NA,NA,NA,NA,NA)
    colnames(out)<-c("shape","scale","shapeSE","scaleSE","MeanTime","Events","Censored","BetaTestPval","AD*weib","AD*expo","AD*logn","AD*norm")
    out[1,6]<-length(df[df$DeltaCens==1,8])
    out[1,7]<-length(df[df$DeltaCens==0,8])
    (out) # no plot generated
  } else {
    # Shape the data frame into a survivor object
    inputSurv<-Surv(time=df$SumOfTOW,event=df$DeltaCens,type="right")
    # Fit the weibull
    options(warn=-1)
    weibSurvReg<-survreg(inputSurv~1,dist="weibull")
    options(warn=0)
    weibparams=c(shape=1/exp(weibSurvReg$icoef[2]),scale=exp(weibSurvReg$icoef[1])) #Survreg gives wacky parameters
    #find the modified Kaplan Meier rank and the fitted values 
      #shape the newTOW table into a two-column dataframe with time on wing and censored information
      newdf<-CensUncens(df)
    rankedpoints<-getModKM(newdf,weibparams) # takes cens/uncens TOW and weibull parameters
    
    #find the exponential parameters and the beta = 1 p value
    betaout<-BetaTest1(inputSurv,weibSurvReg$loglik[1])
    #calculate the Anderson Darling Adjusted statistics for weibull,exponential,lognormal and normal distributions
    ADstats<-calcADAvals1(rankedpoints,inputSurv,betaout[[2]])
    
    #save a plot of the weibull
    if (plot==1) {
      getPlotFromDF(rankedpoints, head(df), weib[[1]], catgs) #ranked points, sample of input data, break-out categories
    }
    
    # save the scale and shape parameters & their standard errors 
    #   & # of events and beta-test-p-value in a dataframe
    #out<-cbind(t(data.frame(weib[1])),t(data.frame(weib[2])))
    out<-t(weibparams)
    out<-as.data.frame(out)
    # get standard errors:
    se<-summary(weibSurvReg)$table[,2] # sqrt of var elements of the the var/covar matrix
    #weibupper=c(shape=1/exp(weibSurvReg$icoef[2]-se[2]),scale=exp(weibSurvReg$icoef[1]+se[1]))
    out[1,3]<-weibparams[1]*(exp(se[2])-1) # SE in weibull parameter terms
    out[1,4]<-weibparams[2]*(exp(se[1])-1) # "
    out[1,5]<-weibparams[[2]]*gamma(1+1/weibparams[[1]]) # mean time b/w events (scale*gamma(1+1/shape))
    out[1,6]<-sum(inputSurv[,2]) # uncensored events
    out[1,7]<-length(inputSurv[,2])-sum(inputSurv[,2]) # censored
    out[1,8]<-betaout[[1]] # get the Log-Likelihood Ratio Test p-value (approximation of Minitab p-value)
    out[1,9]<-ADstats[1] # weibull AD* statistic
    out[1,10]<-ADstats[2] # exponential AD* statistic
    out[1,11]<-ADstats[3] # lognormal AD* statistic
    out[1,12]<-ADstats[4] # normal AD* statistic
    colnames(out)<-c("shape","scale","shapeSE","scaleSE","MeanTime","Events","Censored","BetaTestPval","AD*weib","AD*expo","AD*logn","AD*norm") 
    (out)
  } # end if there are enough data
} # end getWeibullsFromDF function

BetaTest1<-function(input,weibnll) {
  expoSurvReg<-survreg(input~1,dist="exponential")
  teststat<-2*(weibnll-expoSurvReg$loglik[1])
  pvalue<-1-pchisq(teststat,1)
  (list(pval=pvalue,param=expoSurvReg[[1]]))
}

calcADAvals1<-function(rankedpts, origTOW, expoparam) {
  # calculate the two missing distributions
  normalfit<-survreg(origTOW~1,dist="gaussian")
  lognormalfit<-survreg(origTOW~1,dist="lognormal")
  
  #only take the plot (noncensored) points
  rankedpts<-rankedpts[rankedpts$cens==1,] 
  rankedpts$rank<-seq_len(length(rankedpts[,1]))#recalculate the rank
  
  #find AD for the weibull distribution
  weibAD<-getOneADstat(rankedpts)
  
  #recalculate the fits for the exponential distribution
  rankedpts$fitprob<-pexp(q=rankedpts$tow,rate=1/exp(expoparam[1])) # takes the rate
  #and find the AD stat for this
  expoAD<-getOneADstat(rankedpts)
  # do the same for Normal and Lognormal distributions
  rankedpts$fitprob<-plnorm(rankedpts$tow,meanlog=lognormalfit$icoef[1],sdlog=exp(lognormalfit$icoef[2]))
  lognormAD<-getOneADstat(rankedpts)
  rankedpts$fitprob<-pnorm(rankedpts$tow,mean=normalfit$icoef[1],sd=exp(normalfit$icoef[2]))
  normalAD<-getOneADstat(rankedpts)
  #output all four GOF stats
  out<-c(weibAD,expoAD,lognormAD,normalAD)
}

gatherallweibulls1 <- function(weibulls_table){
  numerix<-12 # number of numeric columns
  descrix<-6 # number of description columns
  #  1: Full breakout weibulls:
  weibs1<-ddply(weibulls_table,c("WUC","Loc","PN","Platform","LastRepair","adjRepInt"),getWeibullsFromDF1,plot=0,catgs="WUCLocPNPlatformLastRepairadjRepInt")
  # Make the adjRepInt a factor
  weibs1$adjRepInt<-factor(weibs1$adjRepInt)
  weibullcolnames<-c("WUC","Loc","PN","Platform","LastRepair","adjRepInt","shape","scale","shapeSE","scaleSE","MeanTime","Events","Censored","BetaTestPval","AD*weib","AD*expo","AD*logn","AD*norm")
  # Get the default weibulls:
  #  2: All Repair Intervals
  weibs2<-calloneDDPLY1(weibulls_table,6,numerix,descrix,list(),weibullcolnames)
  #  3: All Repair Types
  weibs3<-calloneDDPLY1(weibulls_table,5,numerix,descrix,list(c(5,6)),weibullcolnames)
  #  4: All Platform Types
  weibs4<-calloneDDPLY1(weibulls_table,4,numerix,descrix,list(c(4,5),c(5,6)),weibullcolnames)
  #  5: All Part Numbers
  weibs5<-calloneDDPLY1(weibulls_table,3,numerix,descrix,list(c(3,4),c(4,5),c(5,6)),weibullcolnames)
  #  6: All Locations
  weibs6<-calloneDDPLY1(weibulls_table,2,numerix,descrix,list(c(2,3),c(3,4),c(4,5),c(5,6)),weibullcolnames)
  #  7: All Repair Intervals and Repair Types
  weibs7<-calloneDDPLY1(weibulls_table,c(5,6),numerix,descrix,list(),weibullcolnames)
  #  8: All Repair Intervals and Platform Types
  weibs8<-calloneDDPLY1(weibulls_table,c(4,6),numerix,descrix,list(c(4,5)),weibullcolnames)  
  #  9: All Repair Intervals and Part Numbers
  weibs9<-calloneDDPLY1(weibulls_table,c(3,6),numerix,descrix,list(c(3,4),c(4,5)),weibullcolnames)    
  #  10: All Repair Intervals and Locations
  weibs10<-calloneDDPLY1(weibulls_table,c(2,6),numerix,descrix,list(c(2,3),c(3,4),c(4,5)),weibullcolnames)  
  #  11: All Platform Types and Repair Types
  weibs11<-calloneDDPLY1(weibulls_table,c(4,5),numerix,descrix,list(c(3,6)),weibullcolnames)    
  #  12: All Platform Types and Part Numbers
  weibs12<-calloneDDPLY1(weibulls_table,c(3,4),numerix,descrix,list(c(3,5),c(4,6)),weibullcolnames)  
  #  13: All Platform Types and Locations
  weibs13<-calloneDDPLY1(weibulls_table,c(2,4),numerix,descrix,list(c(2,3),c(3,5),c(4,6)),weibullcolnames)
  #  14: All Repair Types and Part Numbers
  weibs14<-calloneDDPLY1(weibulls_table,c(3,5),numerix,descrix,list(c(3,4),c(4,6)),weibullcolnames)
  #  15: All Repair Types and Locations
  weibs15<-calloneDDPLY1(weibulls_table,c(2,5),numerix,descrix,list(c(2,3),c(3,4),c(4,6)),weibullcolnames)
  #  16: All Locations and Part Numbers
  weibs16<-calloneDDPLY1(weibulls_table,c(2,3),numerix,descrix,list(c(2,4),c(3,5),c(4,6)),weibullcolnames)
  #  17: All Repair Intervals, Repair Types and Platform Types
  weibs17<-calloneDDPLY1(weibulls_table,c(4,5,6),numerix,descrix,list(),weibullcolnames) 
  #  18: All Repair Intervals, Repair Types and Part Numbers
  weibs18<-calloneDDPLY1(weibulls_table,c(3,5,6),numerix,descrix,list(c(3,4)),weibullcolnames) 
  #  19: All Repair Intervals, Platform Types and Part Numbers
  weibs19<-calloneDDPLY1(weibulls_table,c(3,4,6),numerix,descrix,list(c(3,5)),weibullcolnames)
  #  20: All Repair Repair Types, PLatform Types and Part Numbers
  weibs20<-calloneDDPLY1(weibulls_table,c(3,4,5),numerix,descrix,list(c(3,6)),weibullcolnames)
  #  21: All Locations, Platform Types and Repair Types
  weibs21<-calloneDDPLY1(weibulls_table,c(2,4,5),numerix,descrix,list(c(2,3),c(3,6)),weibullcolnames)  
  #  22: All Locations, Platform Types and Repair Intervals
  weibs22<-calloneDDPLY1(weibulls_table,c(2,4,6),numerix,descrix,list(c(2,3),c(3,5)),weibullcolnames)  
  #  23: All Locations, Repair Types and Repair Intervals
  weibs23<-calloneDDPLY1(weibulls_table,c(2,5,6),numerix,descrix,list(c(2,3),c(3,4)),weibullcolnames) 
  #  24: All Locations, Part Numbers and Repair Types
  weibs24<-calloneDDPLY1(weibulls_table,c(2,3,5),numerix,descrix,list(c(2,4),c(3,6)),weibullcolnames)  
  #  25: All Locations, Part Numbers, and Repair Intervals
  weibs25<-calloneDDPLY1(weibulls_table,c(2,4,6),numerix,descrix,list(c(2,4),c(3,5)),weibullcolnames) 
  #  26: All Locations, Part Numbers and Platform Types
  weibs26<-calloneDDPLY1(weibulls_table,c(2,3,4),numerix,descrix,list(c(2,5),c(3,6)),weibullcolnames) 
  #  27: All Part Numbers, Platform Types, Repair Types and Repair Intervals
  weibs27<-calloneDDPLY1(weibulls_table,c(3,4,5,6),numerix,descrix,list(),weibullcolnames)
  #  28: All Locations, Platform Types, Repair Types and Repair Intervals
  weibs28<-calloneDDPLY1(weibulls_table,c(2,4,5,6),numerix,descrix,list(c(2,3)),weibullcolnames)
  #  29: All Locations, Part Numbers, Repair Types and Repair Intervals
  weibs29<-calloneDDPLY1(weibulls_table,c(2,3,5,6),numerix,descrix,list(c(2,4)),weibullcolnames)
  #  30: All Locations, Part Numbers, Platform Types and Repair Intervals
  weibs30<-calloneDDPLY1(weibulls_table,c(2,3,4,6),numerix,descrix,list(c(2,5)),weibullcolnames)
  #  31: All Locations, Part Numbers, Platform Types and Repair Types
  weibs31<-calloneDDPLY1(weibulls_table,c(2,3,4,5),numerix,descrix,list(c(2,6)),weibullcolnames) 
  #  32: All Everything
  weibs32<-calloneDDPLY1(weibulls_table,c(2,3,4,5,6),numerix,descrix,list(),weibullcolnames)
  
  #combine all sets
  weibs<-rbind(weibs1,weibs2,weibs3,weibs4,weibs5,weibs6,weibs7,weibs8,weibs9,weibs10,weibs11,weibs12,weibs13,weibs14,weibs15,weibs16,
               weibs17,weibs18,weibs18,weibs20,weibs21,weibs22,weibs23,weibs24,weibs25,weibs26,weibs27,weibs28,weibs29,weibs30,weibs31,weibs32)
  
}

calloneDDPLY1<-function(weibulls_table,cols2skip,quantnumcol,quantdesccol,moves,columnames) {
  quantcols2skip<-length(cols2skip) # total number of Description columns to skip
  holdfirst<- quantnumcol - quantcols2skip + 1 # first column of data to hold onto
  holdlast<-quantnumcol + quantdesccol - quantcols2skip # last column of data to hold onto
  pastefirst<-quantdesccol + 1 # first column of data to paste into
  pastlast<-quantnumcol + quantdesccol
  
  #define classifiers: (for loop is a fail, but I'm stumped)
  classifiers<-as.character("") # initialize
  for (ii in seq(quantdesccol)[-cols2skip]) {
    classifiers<-paste(classifiers,names(weibulls_table)[ii],sep="")
  }
  #call the ddply
  weibs<-ddply(weibulls_table,names(weibulls_table[1:quantdesccol][-cols2skip]),getWeibullsFromDF1,plot=0,catgs=classifiers)
  
  # Hold on to the numerical columns
  tempdf<-weibs[,(quantdesccol-quantcols2skip+1):(quantnumcol+quantdesccol-quantcols2skip)]
  
  # Move description columns around
  if (length(moves) != 0) {
    for (ii in seq(from=length(moves),to=1,by=-1)) {weibs[,moves[[ii]][2]]<-weibs[,moves[[ii]][1]]}
  }
  
  # Assign grouped columns to "ALL"
  weibs[cols2skip]<-factor("ALL")
  
  # Return held numerical columns to the end of the data frame
  weibs[,(quantdesccol+1):(quantdesccol+quantnumcol)]<-tempdf
  
  # Reassign the column names
  colnames(weibs)<-columnames
  (weibs)
}

then<-proc.time()[3]
allweibullswSurv<-gatherallweibulls1(weibulls_initial)
proc.time()[3]-then
write.csv(allweibullswSurv,file="Apache Weibulls useSurvpakcage.csv",quote=FALSE)

####
#SE is messed up.  the following gives me a better SE but it still is 
#   less accurate than the fitdistcens package
#Use Survivor parameters as the starting ones for a fitdistcens parameterization 
weibSurv<-Surv(time=subdf$SumOfTOW,event=subdf$DeltaCens,type="right")
a<-(exp(weibSurvReg$icoef[1]))
b<-(1/exp(weibSurvReg$icoef[2]))
weibsubdf<-fitdistcens(censsubdf,"weibull",start=list(scale=a[[1]],shape=b[[1]]))

################################33
NAY<-data.frame(nouns=c("trgb","mxsn", "int gb","lhngb","rhngb","apuC"),before=c(21,6,10,8,10,19),
                after=c(12,6,10,8,9,10))
barbefore<-barplot(NAY[,2],ylim=c(0,22),names.arg=NAY[,1],main="FY11 No Actions Yet",legend.text=c("Before","After"),args.legend=list(fill=c("grey","darkred"),x="top"))
par(new=T)
barafter<-barplot(NAY[,3],ylim=c(0,22),col="darkred",axes=F)
text(barbefore,.8+NAY[,2]-c(1.8,0,0,0,0,0),NAY[,2],col="black")
text(barafter,.8,NAY[,3],col="white")


