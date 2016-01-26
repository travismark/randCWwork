# Winter 2012/2013 | Travis Baer | Clockwork
# Compare weibull distributions and pare down total list if they are similar
# H0: all parameters are the same
library("fitdistrplus")
library("plyr")
setwd("C:/Users/tbaer/Desktop/cbm/october redo weibulls/R work")

then<-proc.time()[3]
alltests<-testallweibulls(allweibulls,weibulls_initial)
proc.time()[3]-then
write.csv(alltests,file="Apache StatTests.csv",quote=FALSE)
sum(alltests$pvalue>.05)/length(alltests$pvalue)

testallweibulls<-function(allweibulls,allremovals,doAllGroups=FALSE) {
  # remove the rows with NA in the weibull fit columns from the data b/c they aren't used
  #allweibullsNONA<-allweibulls[complete.cases(allweibulls),]
  allweibullsNONA<-allweibulls[is.na(allweibulls$shape)==FALSE,]
  classcolnames<-c("WUC","Loc","PN","Platform","LastRepair","adjRepInt")
        # "C2class","C2spec" are classifiers for the tests but not weibulls so leave them out
  #numcolnames<-c("Events1","Censored1","chi2stat","pvalue","Events2","Censored2")
  # specify the columns to include and those to exclude
  cls<-length(classcolnames) # number of classifier columns, including WUC
  first <- TRUE # initialize a dummy binary variable to start or append to a data frame
  tests<-data.frame()  #  initialize an empty data frame to hold output
  for(ii in seq(from=1,to=(cls-1))) { # loop once per classifier column [except wuc] 
    # call combn() one time during an interation of this outer loop
    mat<-combn(cls:2,ii) # gives all the ways to choose 'ii' digits out of quantity of classifier columns less one [b/c wuc is always in]
    for(jj in seq_len(dim(mat)[2])) {
      # call testoneset() once per column of the combn output
      tests1<-testoneset(allweibullsNONA,include=sort(c(1,mat[,jj])),classcolnames,allremovals,doAllGroups)
      if(first) {
        tests<-tests1 # create
        print(dim(tests))
        first<-FALSE
      } else {
        tests<-rbind(tests,tests1)
        print(dim(tests))
      } # end else
    } # end inner for
  } # end outer for
  # convert description columns to factors
  for (jj in seq(from=2,to=length(classcolnames))){
    tests[,jj]<-factor(tests[,jj])
  }
  (tests)
} # end testallweibulls

# first makes green, then gives red to ddply
testoneset<-function(weibullsout,include,classcolnames,removalin,doAllGroups=FALSE) {
  includenames<-classcolnames[include] # include WUC in this
  # runs every test for that combination of classifiers
  exclude<-classcolnames[-include] # find the excluded weibull classifiers
  #exclude<-exclude[-1] # leave out WUC from exclude
  # pare down weibull data
  #weibullSet<-weibullsout[,exclude]=='ALL'
  weibullSet<-weibullsout
  for(ii in exclude) {weibullSet<-weibullSet[weibullSet[,ii]=='ALL',]} # pare down the weibull output set
  for(ii in include) {weibullSet<-weibullSet[weibullSet[,ii]!='ALL',]} # reference to column number or name both work
  print(include)
  first <- TRUE # initialize a dummy binary variable to start or append to a data frame
  # call a ddply once per included classifier (excluding WUC); break down on it and WUC (always 2 classifiers)
  for(jj in seq(from=2,to=length(include))) {
    if(first) { # create new df or append 
    tests<-ddply(weibullSet,includenames[-jj],testOneSubset,include[-jj],includenames[jj],removalin,doAllGroups)
    first <- FALSE}
    else {tests1<-ddply(weibullSet,includenames[-jj],testOneSubset,include[-jj],includenames[jj],removalin)
    tests<-rbind(tests,tests1)} # end check on first
  }
  (tests)
} # end testOneSet

# gets red, gives purple
testOneSubset<- function(someweibulls,otherclassdigits,classifier,remv_set,doAllGroups=FALSE) {
  # Compares each weibull passed to it (a subset of the total) to every other weibull passed to it.
  # Focuses on one classifier, like PN.  Other classifiers may be specified or "ALL."  PNs, for example,
  # are compared to eachother within the same location, platform type, etc.
  if(length(someweibulls[,1])<2) { # if there's only one row in the subset no comparison is possible
    (NULL) }   # return nothing    
  else {
    for(kk in otherclassdigits) { # ddply gives this fn a df with one factor per 'other classifier.'
      # this loop cuts down the removal data to only include ones whose 'other classifier' matches
      #print(dim(remv_set))
      remv_set<-remv_set[as.character(remv_set[,kk])==as.character(someweibulls[1,kk]),]
      remv_set<-droplevels(remv_set)
  }
  mat<-combn(length(someweibulls[,1]),2) # gives all the ways to choose 2 digits out of quantity of rows in subset to test
  for(jj in seq_len(dim(mat)[2])) { # pick two rows and test them
    if(jj==1){test<-testOnePair(rows=mat[,jj],someweibulls,classifier,remv_set)
    } else {test1<-testOnePair(rows=mat[,jj],someweibulls,classifier,remv_set)
    test<-rbind(test,test1)}
  }
  ### if more combinations are desired, go through the following: (e.g. mix 3 PN out of 5 together)
  if(doAllGroups) {
    if(length(someweibulls[,1])>4) { # number of combinations grow exponentially: 2^(number of rows)
      # skip this instance and put in a row that alerts the user
      test<-rbind(test,c("TOO","MANY","COMBINATIONS:","SKIPPING",classifier,"within","classifiers",names(someweibulls)[otherclassdigits]))
    } else if(length(someweibulls[,1])==2 | length(someweibulls[,1])==3) { # if only two rows in the subset the testOnePair() will take care of it
      # If only 3 rows the 3-row case is already calculated - the "ALL" case
      ### Do Nothing 
      } else { # call a function to test the groupings
        # must find all combinations, which requires two combn()s
        for (mm in seq(from=3,to=length(someweibulls[,1])-1)) { # combine mm number of rows together for each
              #  don't want all combinations because that's already been calculated, so subtract one from length(someweibulls)
          for (nn in seq_len(dim(combn(length(someweibulls[,1]),mm))[2])) { # for each column (specific combination of rows)
            test<-rbind(test,testOneMany(rows=combn(length(someweibulls[,1]),mm)[,nn],someweibulls,classifier,remv_set))
          } # end looping across columns (nn)
        } # end looping across that number of things to combine (mm)
      } # end checking if there are the right number of rows to run this testing
    } # end checking if the user wants to run all these extra groupings and tests
  (test)
  } # end check on enough data
} # end testOneSubset

testOneMany<-function(rows,someweibulls,classifier,remv_small) {
  # Tests for equal shape and scale weibull parameters for one group of weibulls
  # use with apply();  passed just two rows of the allweibulls table
  manyweibulls<-droplevels(someweibulls[rows,])
  # initialize the data frame to hold the results
  out<-data.frame("","","","","","","","",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,stringsAsFactors=FALSE)
  colnames(out)<-c("WUC","Loc","PN","Platform","LastRepair","adjRepInt","C2class","C2spec","Events1",
                   "Censored1","chi2stat","pvalue","Events2","Censored2","Shape1","Scale1","Mean1",
                   "Shape2","Scale2","Mean2","Shape1&2","scale1&2","Mean1&2")
  for(ii in 1:6) {out[1,ii]<-as.character(manyweibulls[1,ii])}  # Colon notation isn't working for some reason
  #out[1,1:6]<-as.character(twoweibulls[1,1:6]) # transfer the classifier columns
  out[1,7]<-classifier # transfer the one specific classifier type
  groupedtogether<-unique(as.character(manyweibulls[,classifier]))
  #print(groupedtogether)
  #print(classifier)
  z<-groupedtogether[1]
  for(bb in 2:length(groupedtogether)){z<-paste(z,groupedtogether[bb])} # transfer the specific classifier of the second row
  out[1,8]<-z
  #out[1,9:10]<- # the information for group one and group two don't mean much when we have more than two groups
  NLLspec1<-NA # two negative log likelihoods
  NLLspec2<-NA
  # table already has the individual log-likelihoods; calculate:  
  NLLsumofindiv<-sum(manyweibulls$NLogLik)
  # find the log-likelihood of the combined data set fit:
  #   first get the superset of removal data from all classifier sets together:
  supset<-remv_small[remv_small[,classifier]==as.character(levels(manyweibulls[,classifier])[1]),]
  print(levels(droplevels(remv_small[,classifier])))
  for (dd in 2:length(manyweibulls$PN)){supset<-rbind(supset,remv_small[remv_small[,"PN"]==as.character(levels(manyweibulls[,"PN"])[dd]),])}
  supset<-droplevels(supset) # drop factor levels
  censsupdf<-CensUncens(supset) # and fit
  options(warn=-1) # NANs in the fitting produce warnings
  weibsupdf<-fitdistcens(censsupdf,"weibull")  # second, fit the data to a weibull
  options(warn=0)
  out[1,c(9,13)]<-length(censsupdf$right)-sum(is.na(censsupdf$right)) # events
  out[1,c(10,14)]<-sum(is.na(censsupdf$right)) # censorings
  NLLcomb<-weibsupdf$loglik # negative log likelihood of the combined fit
  teststat<-2*(NLLsumofindiv-NLLcomb) # chi squared test statistic for:
  pval<-pchisq(teststat,2,lower.tail=FALSE) # hypothesis test: null: shape=shape & scale=scale
  out[1,11]<-teststat
  out[1,12]<-pval
  # leave the columns describing the "two" weibull inputs NA
  # but transfer parameters from combining the two populations
  out[1,21:23]<-c(weibsupdf[[1]][1],weibsupdf[[1]][2],weibsupdf[[1]][2]*gamma(1+1/weibsupdf[[1]][1]))
  (out)
}

manyweibulls<-tshweibulls[tshweibulls$WUC=="T700" & tshweibulls$Loc!="ALL" 
            & (tshweibulls$PN=="5130T00G01" | tshweibulls$PN=="6035T00G01" | tshweibulls$PN=="6044T06G01")
            & tshweibulls$adjRepInt=="ALL" & tshweibulls$Platform=="ALL" & tshweibulls$LastRepair=="ALL",]
manyweibulls<-droplevels(manyweibulls)

supset<-remv_small[remv_small[,"PN"]==as.character(levels(manyweibulls[,"PN"])[1]),]
for (dd in 2:3){supset<-rbind(supset,remv_small[remv_small[,"PN"]==as.character(levels(manyweibulls[,"PN"])[dd]),])}
supset<-droplevels(supset)

remove(out)
test1<-testOneMany(c(1,2),weibulls,"Loc",remv_small)

testOnePair<- function(rows,someweibulls,classifier,remv_small) {
# Tests for equal shape and scale weibull parameters for one pair of weibulls
# use with apply();  passed just two rows of the allweibulls table
twoweibulls<-someweibulls[rows,]
#print(twoweibulls)
# initialize the data frame to hold the results
out<-data.frame("","","","","","","","",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,stringsAsFactors=FALSE)
colnames(out)<-c("WUC","Loc","PN","Platform","LastRepair","adjRepInt","C2class","C2spec","Events1",
                 "Censored1","chi2stat","pvalue","Events2","Censored2","Shape1","Scale1","Mean1",
                 "Shape2","Scale2","Mean2","Shape1&2","scale1&2","Mean1&2")
for(ii in 1:6) {out[1,ii]<-as.character(twoweibulls[1,ii])}  # Colon notation isn't working for some reason
#out[1,1:6]<-as.character(twoweibulls[1,1:6]) # transfer the classifier columns
out[1,7]<-classifier # transfer the one specific classifier type
out[1,8]<-as.character(twoweibulls[2,classifier]) # transfer the specific classifier of the second row
out[1,9:10]<-twoweibulls[1,12:13] # transfer the removal numbers of first row
NLLspec1<-twoweibulls$NLogLik[1] # two negative log likelihoods
NLLspec2<-twoweibulls$NLogLik[2]
# table already has two of the required log-likelihoods.  to find the third:
#   first get the superset of removal data from both classifier sets together
supset<-remv_small[remv_small[,classifier]==as.character(twoweibulls[1,classifier]) | 
                     remv_small[,classifier]==as.character(twoweibulls[2,classifier]),] 
supset<-droplevels(supset)
censsupdf<-CensUncens(supset)
options(warn=-1) # NANs in the fitting produce warnings
weibsupdf<-fitdistcens(censsupdf,"weibull")  # second, fit the data to a weibull
options(warn=0)
NLLcomb<-weibsupdf$loglik # negative log likelihood of the combined fit
teststat<-2*((NLLspec1+NLLspec2)-NLLcomb) # chi squared test statistic for:
pval<-pchisq(teststat,2,lower.tail=FALSE) # hypothesis test: null: shape=shape & scale=scale
out[1,11]<-teststat
out[1,12]<-pval
out[1,13:14]<-twoweibulls[2,12:13] # transfer the removal numbers of the second weibull/row
out[1,15:17]<-twoweibulls[1,c(7,8,11)] # transfer first parameters
out[1,18:20]<-twoweibulls[2,c(7,8,11)] # transfer second parameters
out[1,21:23]<-c(weibsupdf[[1]][1],weibsupdf[[1]][2],weibsupdf[[1]][2]*gamma(1+1/weibsupdf[[1]][1])) # transfer parameters from combining the two populations
(out)
} # end testOnePair()


for (ii in levels(allweibulls$WUC)) {
  #print(ii)
  ### Location first: Compare Each individual location against all-the-other locations
  # Loop across each Location
  for (jj in levels(droplevels(allweibulls[allweibulls$WUC==ii,])$Loc)) {
  # skip the ALL parameter
  #print(jj)
    if(jj!="ALL") {
    # we already have the individual location and grouped locations ("ALL")
    # find the all-the-other locations weibull and negative log likelihood
    subdf<-weibulls_initial[weibulls_initial$WUC==ii & weibulls_initial$Loc!=jj,]
    subdf<-droplevels(subdf)
    censsubdf<-CensUncens(subdf)
    weibsubdf<-fitdistcens(censsubdf,"weibull")
    nextrow<-length(out[,1])+1
    # All negative log likelihoods are now known. find the test statistic and get the p value
    # test statistic is the reduced-model log likelihood less the  no-restriction (ALL) model log likelihood
    NLLall<-allweibulls[allweibulls$WUC==ii & allweibulls$Loc=="ALL" & allweibulls$PN=="ALL"
                             & allweibulls$adjRepInt=="ALL" & allweibulls$Platform=="ALL" 
                             & allweibulls$LastRepair=="ALL",]$NLogLik
    NLLgen<-weibsubdf$loglik
    NLLspec<-allweibulls[allweibulls$WUC==ii & allweibulls$Loc==jj & allweibulls$PN=="ALL"
                              & allweibulls$adjRepInt=="ALL" & allweibulls$Platform=="ALL" 
                              & allweibulls$LastRepair=="ALL",]$NLogLik
    teststat<-2*((NLLspec+NLLgen)-NLLall)
    pval<-pchisq(teststat,2,lower.tail=FALSE)
    # output results
    out[nextrow,1]<-ii;    out[nextrow,2]<-"Loc";    out[nextrow,3]<-jj
    out[nextrow,4]<-pval;  out[nextrow,5]<-teststat;    out[nextrow,6]<-NLLgen
    out[nextrow,7]<-NLLspec;   out[nextrow,8]<-NLLall
    
    } # end making sure to loop over the "ALL" parameter
  } # end looping over all Locations
} # end looping over all WUCs
# remove the first row that I used for initialization
out<-out[-1,]
} # end testOnePair


write.csv(out,"stattests.csv")

findweibNLL <- function(sh=1,sc,data) {
  n<-length(data[,1]) # number of data points
  cens<-data[which(is.na(data[,2])),1]
  uncens<-data[which((data[,2])>0),1]
  alld<-data[,1]
  # log likelihood is two parts:  sum(log(h)) for events + sum(log(H)) for all
  out<-sum(log(sh*sc^-sh*uncens^(sh-1))) # sum of log of hazard for uncensored events
  out<-out-sum((alld/sc)^sh)
}


##########################################################
#  Test out the method by hand ###########################
##########################################################

###########
#ex 1: 06A parts -43 and -41
###########
# part -43
subdf<-weibulls_initial[weibulls_initial$WUC=="06A" & weibulls_initial$PN=="7-311310001-43" ,]
subdf<-droplevels(subdf)
censsubdf43<-CensUncens(subdf)
weibsubdf43<-fitdistcens(censsubdf43,"weibull")
expsubdf43<-fitdistcens(censsubdf43,"weibull",fix=list(shape=1),start=list(scale=1155.18))#median(censsubdf43[,1])))
dash43<-findweibNLL(weibsubdf43[[1]][1],weibsubdf43[[1]][2],censsubdf43)
dash43exp<-findweibNLL(1,expsubdf43[[1]][1],censsubdf43)

# part -41
subdf<-weibulls_initial[weibulls_initial$WUC=="06A" & weibulls_initial$PN=="7-311310001-41" ,]
subdf<-droplevels(subdf)
censsubdf41<-CensUncens(subdf)
weibsubdf41<-fitdistcens(censsubdf41,"weibull")
expsubdf41<-fitdistcens(censsubdf41,"weibull",fix=list(shape=1),start=list(scale=916.039))#median(censsubdf41[,1])))
dash41<-findweibNLL(weibsubdf41[[1]][1],weibsubdf41[[1]][2],censsubdf41)
dash41exp<-findweibNLL(1,expsubdf41[[1]][1],censsubdf41)


#combine
subdf<-weibulls_initial[weibulls_initial$WUC=="06A" & (weibulls_initial$PN=="7-311310001-41" | weibulls_initial$PN=="7-311310001-43") ,]
subdf<-droplevels(subdf)
censsubdfboth<-CensUncens(subdf)
weibsubdfboth<-fitdistcens(censsubdfboth,"weibull")
dash41and43comb<-findweibNLL(weibsubdfboth[[1]][1],weibsubdfboth[[1]][2],censsubdfboth)

dash41;dash43;dash41and43comb

2*((dash41+dash43)-dash41and43comb) # likelihood ratio statistic

###########
#ex 2: 06G01 -5 and -7
###########
with(droplevels(weibulls_initial[weibulls_initial$WUC=="06G01",]),table(WUC,PN))
# part -5
subdf<-weibulls_initial[weibulls_initial$WUC=="06G01" & weibulls_initial$PN=="7-311340001-5" ,]
subdf<-droplevels(subdf)
censsubdf5<-CensUncens(subdf)
weibsubdf5<-fitdistcens(censsubdf5,"weibull")
dash5<-findweibNLL(weibsubdf5[[1]][1],weibsubdf5[[1]][2],censsubdf5)

# part -7
subdf<-weibulls_initial[weibulls_initial$WUC=="06G01" & weibulls_initial$PN=="7-311340001-7" ,]
subdf<-droplevels(subdf)
censsubdf7<-CensUncens(subdf)
weibsubdf7<-fitdistcens(censsubdf7,"weibull")
dash7<-findweibNLL(weibsubdf7[[1]][1],weibsubdf7[[1]][2],censsubdf7)

dash5;dash7;

#combine
subdf<-weibulls_initial[weibulls_initial$WUC=="06G01" & (weibulls_initial$PN=="7-311340001-5" | weibulls_initial$PN=="7-311340001-7") ,]
subdf<-droplevels(subdf)
censsubdfboth<-CensUncens(subdf)
weibsubdfboth<-fitdistcens(censsubdfboth,"weibull")
dash5and7comb<-findweibNLL(weibsubdfboth[[1]][1],weibsubdfboth[[1]][2],censsubdfboth)

2*((dash5+dash7)-dash5and7comb) # likelihood ratio statistic


###########
#ex 3: 06A Ft Ruck -51, Rep 1 vs Rep 2
###########
with(droplevels(weibulls_initial[weibulls_initial$WUC=="06A",]),table(WUC,PN))
# 1st rep int
subdf<-with(weibulls_initial,weibulls_initial[WUC=="06A" & PN=="7-311310001-51" & Loc=="FT RUCKER" & adjRepInt==1,])
subdf<-droplevels(subdf)
censsubdf1<-CensUncens(subdf)
weibsubdf1<-fitdistcens(censsubdf1,"weibull")
rep1<-findweibNLL(weibsubdf1[[1]][1],weibsubdf1[[1]][2],censsubdf1)

# 2nd rep int
subdf<-with(weibulls_initial,weibulls_initial[WUC=="06A" & PN=="7-311310001-51" & Loc=="FT RUCKER" & adjRepInt==2,])
subdf<-droplevels(subdf)
censsubdf2<-CensUncens(subdf)
weibsubdf2<-fitdistcens(censsubdf2,"weibull")
rep2<-findweibNLL(weibsubdf2[[1]][1],weibsubdf2[[1]][2],censsubdf2)

rep1;rep2;

rep1w2<-findweibNLL(weibsubdf2[[1]][1],weibsubdf2[[1]][2],censsubdf1)
rep2w1<-findweibNLL(weibsubdf1[[1]][1],weibsubdf1[[1]][2],censsubdf2)
rep1w2;rep2w1

###########
#ex 4: 06A NSWA -51, Rep 1 vs Rep 2
###########
# 1st rep int
subdf<-with(weibulls_initial,weibulls_initial[WUC=="06A" & PN=="7-311310001-51" & Loc=="NSWA" & adjRepInt==1,])
subdf<-droplevels(subdf)
censsubdf1<-CensUncens(subdf)
weibsubdf1<-fitdistcens(censsubdf1,"weibull")
rep1<-findweibNLL(weibsubdf1[[1]][1],weibsubdf1[[1]][2],censsubdf1)

# 2nd rep int
subdf<-with(weibulls_initial,weibulls_initial[WUC=="06A" & PN=="7-311310001-51" & Loc=="NSWA" & adjRepInt==2,])
subdf<-droplevels(subdf)
censsubdf2<-CensUncens(subdf)
weibsubdf2<-fitdistcens(censsubdf2,"weibull")
rep2<-findweibNLL(weibsubdf2[[1]][1],weibsubdf2[[1]][2],censsubdf2)

rep1;rep2;

rep1w2<-findweibNLL(weibsubdf2[[1]][1],weibsubdf2[[1]][2],censsubdf1)
rep2w1<-findweibNLL(weibsubdf1[[1]][1],weibsubdf1[[1]][2],censsubdf2)
rep1w2;rep2w1
