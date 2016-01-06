# Winter 2012/2013 | Travis Baer | Clockwork
# Compare weibull distributions and pare down total list if they are similar
# H0: all parameters are the same

library("fitdistrplus")
library("plyr")

testallweibulls<-function(allweibulls,allremovals,verbose=FALSE,doAllGroups=FALSE) {
  # Perform negative log likelihood tests on a set of fitted weibulls and
  #   run additional weibulls and tests for groups of classes within the same classifier set.
  # Args:
  #   allweibulls: dataframe: output from gatherallweibulls(): a 19-column dataframe weith weibulls 
  #     for all combinations of a set of removal data
  #   allremovals: dataframe: removal data and the same input data that goes into gatherallweibulls()
  #   verbose: if TRUE will print out some weibull-testing progress
  #   doAllGroups: T/F binary: allows the user to skip running additional weibulls and statistical tests
  #     for class groups (saves time and space in the csv)
  # Out: dataframe: 23 column table with parameters and statistical tests
  
  if(verbose==FALSE){print("Please Wait")}
  # remove the rows with NA in the weibull fit columns from the data b/c they aren't used
  # allweibullsNONA<-allweibulls[complete.cases(allweibulls),] - sometimes the SE come out NA but parameters are found
  allweibullsNONA<-allweibulls[is.na(allweibulls$shape)==FALSE,]
  classcolnames<-c("WUC","LOCATION","PN","NHA_PN","LAST_REPAIR","REPAIR_COUNT")
        # "C2class","C2spec" are classifiers for the tests but not weibulls so leave them out
  # numcolnames<-c("Events1","Censored1","chi2stat","pvalue","Events2","Censored2")
  # specify the columns to include and those to exclude
  cls<-length(classcolnames) # number of classifier columns, including WUC
  first <- TRUE # initialize a dummy binary variable to start or append to a data frame
  tests<-data.frame()  #  initialize an empty data frame to hold output
  for(ii in seq(from=1,to=(cls-1))) { # loop once per classifier column [except wuc] 
    # call combn() one time during an interation of this outer loop
    mat<-combn(cls:2,ii) # gives all the ways to choose 'ii' digits out of quantity of classifier columns less one [b/c wuc is always in]
    for(jj in seq_len(dim(mat)[2])) {
      # call testoneset() once per column of the combn output
      tests1<-testoneset(allweibullsNONA,include=sort(c(1,mat[,jj])),classcolnames,allremovals,doAllGroups,verbose)
      if(first) {
        tests<-tests1 # create
        if(verbose){print(dim(tests))}
        first<-FALSE
      } else {
        tests<-rbind(tests,tests1)
        if(verbose){print(dim(tests))}
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
testoneset<-function(weibullsout,include,classcolnames,removalin,doAllGroups,verbose) {
  # Pares down the weibull data and makes call to testonesubset() through several calls to ddply().
  # Used in conjuction with the weibull statistical tests and dependent on testallweibulls()
  # Args:
  #   weibullsout: data frame: the entire weibull output database from gatherallweibulls()
  #   include: a vector of integers: describing which classifier columns to keep specific for 
  #     the following analysis
  #   classcolnames: a vector of characters with the classifier column names
  #   removalin: dat aframe: the entire weibull removal data input: same as passed to gatherallweibulls()
  #   doAllGroups: logical TRUE/FALSE: TRUE if additional groups of weibulls should be fit and tested
  #   verbose: if TRUE will print out some weibull-testing progress
  # Out: 
  #  returns to testallweibulls() a subset of the total output data frame
  
  includenames<-classcolnames[include] # include WUC in this
  # runs every test for that combination of classifiers
  exclude<-classcolnames[-include] # find the excluded weibull classifiers
  #exclude<-exclude[-1] # leave out WUC from exclude
  # pare down weibull data
  #weibullSet<-weibullsout[,exclude]=='ALL'
  weibullSet<-weibullsout
  for(ii in exclude) {weibullSet<-weibullSet[weibullSet[,ii]=='ALL',]} # pare down the weibull output set
  for(ii in includenames) {weibullSet<-weibullSet[weibullSet[,ii]!='ALL',]} # reference to column number or name both work
  if(verbose){print(classcolnames[include])} # to track progress
  first <- TRUE # initialize a dummy binary variable to start or append to a data frame
  # call a ddply once per included classifier (excluding WUC, so start at 2); 
     # break down on it and WUC (always 2 classifiers)
  for(jj in seq(from=2,to=length(include))) {
    if(first) { # create new df or append 
    tests<-ddply(weibullSet,includenames[-jj],testOneSubset,include[-jj],includenames[jj],removalin,doAllGroups)
    first <- FALSE}
    else {tests1<-ddply(weibullSet,includenames[-jj],testOneSubset,include[-jj],includenames[jj],removalin,doAllGroups)
    tests<-rbind(tests,tests1)} # end check on first
  }
  (tests)
} # end testOneSet

# gets red, gives purple
testOneSubset<- function(someweibulls,otherclassdigits,classifier,remv_set,doAllGroups) {
  # Compares each weibull passed to it (a subset of the total) to every other weibull passed to it.
  #  Focuses on one classifier, like PN.  Other classifiers may be specified or "ALL."  PNs, for example,
  #  are compared to eachother within the same location, platform type, etc. OR holding other classifiers ALL
  # Args:
  #   someweibulls: dataframe: subset of the weibulls table
  #   otherclassdigits: vector of integers: specifies which classifier columns are specific and should have
  #     just one value in the resulting someweibulls dataframe
  #   classifier: character: the one classifier that will be used to compare removal data populations
  #   revm_set:  dataframe: the full removals database that was passed to gatherallweibulls()
  #   doAllGroups: the logical that decides whether to fit additional weibulls and perform more tests
  # Out:
  #   Returns a part of the final output dataframe

  if(length(someweibulls[,1])<2) { # if there's only one row in the subset no comparison is possible
    (NULL) }   # return nothing    
  else {
    ### in case the classifier columns in someweibulls (weibulls table) and remv_set (removals)
    # are in different orders change the otherclassdigits into the correct integer values for remv_set
    # so that it subsets correctly
    remvclassnames<-rep("",length(otherclassdigits)) # initialize new vector
    for(bb in 1:length(otherclassdigits)) {
      remvclassnames[bb]<-names(allweibulls)[otherclassdigits[bb]]
    }
      
    for(kk in remvclassnames) { # ddply gives this fn a weibulls df with one factor per specific 'other classifier.'
      # this loop cuts down the removal data to only include ones whose 'other classifier' matches
      #if(classifier=="adjRepInt") {print(someweibulls[1,kk])}
      remv_set<-remv_set[as.character(remv_set[,kk])==as.character(someweibulls[1,kk]),]
      remv_set<-droplevels(remv_set)
      #if(classifier=="adjRepInt") {print(kk);print(dim(remv_set))}
       } # end for
  mat<-combn(length(someweibulls[,1]),2) # gives all the ways to choose 2 digits out of quantity of rows in subset to test
  for(jj in seq_len(dim(mat)[2])) { # pick two rows and test them
    if(jj==1){
      #if(classifier=="adjRepInt") {print(dim(remv_set))}
      test<-testOnePair(rows=mat[,jj],someweibulls,classifier,remv_set)
    } else {test1<-testOnePair(rows=mat[,jj],someweibulls,classifier,remv_set)
    test<-rbind(test,test1)}
    } # end for 
  ### if more combinations are desired, go through the following: (e.g. mix 3 PN out of 5 together)
  if(doAllGroups) {
    if(length(someweibulls[,1])>20) { # number of combinations grow exponentially: 2^(number of rows)
      # skip this instance and put in a row that alerts the user
      test<-rbind(test,c("TOO","MANY","COMBINATIONS:","SKIPPING",classifier,"within","classifiers",names(someweibulls)[otherclassdigits]))
    } else if(length(someweibulls[,1])==2) { #| length(someweibulls[,1])==3) { # if only two rows in the subset the testOnePair() will take care of it
      # If only 3 rows the 3-row case is already calculated - the "ALL" case
      ### Do Nothing 
      } else { # call a function to test the groupings
        # must find all combinations, which requires two combn()s
        for (mm in seq(from=3,to=length(someweibulls[,1]))) {#-1)) { # combine mm number of rows together for each (2 has already been done; start on 3)
              #  don't want all combinations either because that's already been calculated; subtract one from length(someweibulls)
          for (nn in seq_len(dim(combn(length(someweibulls[,1]),mm))[2])) { # for each column (specific combination of rows)
            test<-rbind(test,testOneMany(rows=combn(length(someweibulls[,1]),mm)[,nn],someweibulls,classifier,remv_set))
          } # end looping across columns (nn)
        } # end looping across that number of things to combine (mm)
      } # end checking if there are the right number of rows to run this testing
    } # end checking if the user wants to run all these extra groupings and tests
  (test)
  } # end check on enough data
} # end testOneSubset


# used to find weibulls for and test groups within classifiers (2 out of 3 PN compared to 3rd PN)
testOneMany<-function(rows,someweibulls,classifier,remv_small) {
  # Tests for equal shape and scale weibull parameters for one group of weibulls
  #   use with apply();  passed just two rows of the allweibulls table
  # Args:
  #   rows:  vector of integers: rows to select from someweibulls and include in a new weibull (at least 2)
  #   someweibulls:  dataframe: weibulls output table for this classifier set; subsetted here and previously
  #   classifier: character: specifying the one classifier to 
  #   remv_small: dataframe: entire removal input dataframe; subsetted previously for only "other classifier
  #                matches:" at min just that WUC. further subsetted here down to just the "rows" classifiers
  # Out:
  #   23-column data frame for the final statistical test output
  
  #manyweibulls<-droplevels(someweibulls[rows,]) # droplevels is breaking my world
  manyweibulls<-someweibulls[rows,]
  manyweibulls[,1:6]<-droplevels(manyweibulls[,1:6])
  # initialize the data frame to hold the results
  out<-data.frame("","","","","","","","",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,stringsAsFactors=FALSE)
  colnames(out)<-c("WUC","LOCATION","PN","NHA_PN","LAST_REPAIR","REPAIR_COUNT","C2class","C2spec","Events1",
                   "Censored1","chi2stat","pvalue","Events2","Censored2","Shape1","Scale1","Mean1",
                   "Shape2","Scale2","Mean2","Shape1&2","scale1&2","Mean1&2")
  #out[1,1:6]<-as.character(twoweibulls[1,1:6]) # transfer the classifier columns
  # Paste the held-out data: can be as few as one row
  for(ii in 1:6) {out[1,ii]<-as.character(manyweibulls[1,ii])}  # Colon notation isn't working for some reason
  EXgroupedtogether<-unique(as.character(someweibulls[-rows,classifier]))
  out[1,classifier]<-"independently" # specific classifiers for held-out sample.  one of columns 2:6
  out[1,7]<-classifier # transfer the one specific classifier type
  # Paste the grouped data: can be as few as three rows
  groupedtogether<-unique(as.character(manyweibulls[,classifier]))
  #print(groupedtogether)
  #print(classifier)
  z<-groupedtogether[1]
  for(bb in 2:length(groupedtogether)){z<-paste(z,groupedtogether[bb])} # transfer the specific classifier of the second row
  out[1,8]<-z
  #out[1,9:10]<- # the information for group one and group two don't mean much when we have more than two groups
  #NLLspec1<-NA # two negative log likelihoods
  #NLLspec2<-NA
  # table already has the individual log-likelihoods; calculate:  
  NLLsumofindiv<-sum(manyweibulls$NLogLik)
  # find the log-likelihood of the combined data set fit:
  #   first get the superset of removal data from all classifiers of this set together:
  #       start with the first subset
  supset<-remv_small[remv_small[,classifier]==as.character(levels(manyweibulls[,classifier])[1]),]
  #print(levels(droplevels(remv_small[,classifier]))) droplevels giving HEADACHES
  #       then add the remaining ones (2:the end)
  for (dd in 2:length(manyweibulls[,classifier])){supset<-rbind(supset,
            remv_small[remv_small[,classifier]==as.character(levels(manyweibulls[,classifier])[dd]),])}
  supset<-droplevels(supset) # drop unused factor levels
  censsupdf<-CensUncens(supset) # and prepare for fitting
  options(warn=-1) # NANs in the fitting produce warnings
  weibsupdf<-fitdistcens(censsupdf,"weibull")  # second, fit the data to a weibull
  options(warn=0)
  out[1,c(9,13)]<-length(censsupdf$right)-sum(is.na(censsupdf$right)) # events.  same in both
  out[1,c(10,14)]<-sum(is.na(censsupdf$right)) # censorings
  NLLcomb<-weibsupdf$loglik # negative log likelihood of the combined fit
  teststat<-2*(NLLsumofindiv-NLLcomb) # chi squared test statistic for:
  pval<-pchisq(teststat,2*length(manyweibulls[,classifier])-2,lower.tail=FALSE) # hypothesis test: null: shape=shape & scale=scale
  out[1,11]<-teststat
  out[1,12]<-pval
  # leave the columns describing the "two" weibull inputs NA
  # but transfer parameters from combining the two populations
  out[1,21:23]<-c(weibsupdf[[1]][1],weibsupdf[[1]][2],weibsupdf[[1]][2]*gamma(1+1/weibsupdf[[1]][1]))
  (out)
}


testOnePair<- function(rows,someweibulls,classifier,remv_small) {
# Tests for equal shape and scale weibull parameters for one pair of weibulls
# use with apply();  passed just two rows of the allweibulls table
twoweibulls<-someweibulls[rows,]
# initialize the data frame to hold the results
out<-data.frame("","","","","","","","",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,stringsAsFactors=FALSE)
colnames(out)<-c("WUC","LOCATION","PN","NHA_PN","LAST_REPAIR","REPAIR_COUNT","C2class","C2spec","Events1",
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

findweibNLL <- function(sh=1,sc,data) {
  n<-length(data[,1]) # number of data points
  cens<-data[which(is.na(data[,2])),1]
  uncens<-data[which((data[,2])>0),1]
  alld<-data[,1]
  # log likelihood is two parts:  sum(log(h)) for events + sum(log(H)) for all
  out<-sum(log(sh*sc^-sh*uncens^(sh-1))) # sum of log of hazard for uncensored events
  out<-out-sum((alld/sc)^sh)
}
