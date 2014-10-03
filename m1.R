###
# time to first failure - summer 2014
###

#######################
# python data used to split excel file into different csvs
# import xlrd
# import csv
# wb = xlrd.open_workbook('C:\\Users\\tbaer\\Desktop\\m1a1\\M1A1 Alion RAW EROs.xlsx')
# for ii in wb.sheets():
#   sh = wb.sheet_by_name(ii.name)
#   the_csv_file = open(ii.name + '.csv', 'wb')
#   wr = csv.writer(the_csv_file, quoting=csv.QUOTE_ALL)
#   for rownum in xrange(sh.nrows):
#     wr.writerow(sh.row_values(rownum))
#   the_csv_file.close() 
########################


## adjustments to data
#1) remove 54 rows from sheet 2000 with data in 2009
#2) remove records with Ordered before platform birth (~1% of data) - done for rawEros

# below not done
#2) remove records with Ordered before start (~10% of data)
#2) change SN 579699 to 4/1 birthday (from 5/1/2006)

################# 
### flaws in my method
# if SL qty > 1 I don't know which one is removed, so I cannot get a good estimate of shape parametet
# I don't know when new parts (like new electronics) are installed, for two reasons 1) to add a new part or 2) to replace old part
# I assume all parts are on the vehicle at all times - SOE does as well (total time on test is  1,726,593 days)

require(MASS)
require(plyr)
require(dplyr)
require(hexbin)
require(fitdistrplus)
require(RODBC)
`%notin%` <- function(x,y) !(x %in% y) 
origPar<-par()

##### load the datas - adjust formats and add other data
setwd("C:/Users/tbaer/Desktop/m1a1")
# eros
rawEros<-read.csv("1995.csv",stringsAsFactors=FALSE)
rawEros$Sheet<-'1995'
for (ii in seq(from=1996,to=2014)) {
  a<-paste(ii,'.csv',collapse="",sep="")
  a<-read.csv(a,stringsAsFactors=FALSE)
  a$Sheet<-ii
  rawEros<-invisible(rbind(rawEros,a))
}
remove(a);remove(ii)
# string loaded as factors as default so that the first load wouldn't exclude other factor types - convert a few back
rawEros$Apps.<-as.factor(rawEros$Apps.)
rawEros$Category..Code<-as.factor(rawEros$Category..Code)
# take out these inane periods
colnames(rawEros)<-gsub(".","",colnames(rawEros),fixed=TRUE)
# split date into two fields
getElementOfString<-function(text,elnum){
  lt<-strsplit(text,"-")
  lt[[1]][elnum]
}
rawEros$Start<-sapply(X=rawEros$Date,FUN=getElementOfString,1)
rawEros$End<-sapply(X=rawEros$Date,FUN=getElementOfString,2)

length(unique(rawEros$SerialNumber)) # 2883
SNs<-unique(rawEros$SerialNumber) # store these
plot(sort(table(rawEros$SerialNumber),decreasing=TRUE));head(sort(-table(rawEros$SerialNumber)),10)
head(sort(table(rawEros$SerialNumber),decreasing=TRUE),1)/nrow(rawEros) # proportion of data with serial number = 0
plot(sort(table(rawEros$Unit),decreasing=TRUE));head(sort(-table(rawEros$Unit)),10)
head(sort(table(rawEros$Unit),decreasing=TRUE),1)/nrow(rawEros) # proportion of data with unit = 21410
# as.Date(32768, origin = "1899-12-30") 


# for every serial number: for every NSN: find days until first order
# SN - NSN - platform starting date - first order date

##############  load prod year
# platform serial number DOB
prodYear<-read.csv("M1A1 Location-Production Year.csv",stringsAsFactors=FALSE)
prodYear$IN.SERVICE<-as.Date(prodYear$IN.SERVICE, origin = "1899-12-30")
plot(prodYear$IN.SERVICE)
nrow(prodYear) # 449

# reduce EROs data to only those SNs that match our SN list
nrow(rawEros) # 257967
rawEros<-(rawEros[rawEros$SerialNumber %in% prodYear$SerialNumber,])
nrow(rawEros) # 194568
plot(sort(table(rawEros$SerialNumber),decreasing=TRUE))


rawEros$Ordered<-as.Date(rawEros$Ordered, origin = '1899-12-30')
# some received dates are blank or "Cancelled" - change these to NA
rawEros$Received[which(rawEros$Received=="")] <- NA;rawEros$Received[which(rawEros$Received=="CANCELLED")] <- NA
rawEros$Received<-as.numeric(rawEros$Received)
rawEros$Received<-as.Date(rawEros$Received, origin = '1899-12-30')
rawEros$Start<-as.Date(rawEros$Start, format = '%m/%d/%Y')
rawEros$End<-as.Date(rawEros$End, format = '%m/%d/%Y')
# add FSC and Group
rawEros$NSN<-as.numeric(rawEros$NSN)
rawEros$FSC<-floor(rawEros$NSN,-9)/1000000000
rawEros$Group<-floor(rawEros$NSN,-11)/100000000000
rawEros$NSN<-as.character(rawEros$NSN)
# add the platform SN birthday to the rawEros data frame
rawEros<-merge(rawEros,prodYear[,c("SerialNumber","IN.SERVICE")],by="SerialNumber")
nrow(rawEros) # 194568

########################################### data verification and deletion

rawEros<-rawEros[-which(rawEros$Sheet==2000 & rawEros$Ordered>as.Date("2005-01-01")),] # take out 54 points ################################
nrow(rawEros[rawEros$Sheet==2000,]) # 9758

## start vs. platform birth
# how many records are removed b/c Ordered < Platform Birth
sum(rawEros$IN.SERVICE>rawEros$Ordered,na.rm=TRUE)/nrow(rawEros) # only 1%
rawEros<-rawEros[which(rawEros$Ordered>=rawEros$IN.SERVICE),] # take out 1% of data ######################################################
nrow(rawEros) # 173341

## max ordered date
system.time(SNmaxStart<-summarise(group_by(rawEros,"SerialNumber"),"LstSttDt"=max(Start,na.rm=TRUE)))

############################################ analysises
######################### two three four and five next removals
# rawEros[rawEros$ERO=="P9085"&rawEros$SerialNumber==572348,] an example of a SN/ERO combo with THREE eros
#  #  need to also account for different start dates
# take the next date after the previous removal (or birth) that's in a new ERO
# calculate days different in here, as well as suspension times
first5Ordered<-function(dataframe,unbug=FALSE){
  # args
  # dataframe - subest of rawEros with one platform SN and one NSN
  # returns
  # dataframe with 5 failure dates (ero starts) and 5 differences
  # save the SN birthday before further subsetting 
  Birthday<-dataframe[1,"IN.SERVICE"]
  if(unbug){print(dataframe$SerialNumber[1]);print(dataframe$FSC[1]);print(dataframe$NSN[1])}
  dataframe<-dataframe[dataframe$Start>dataframe$IN.SERVICE,] # ignore dates before the SN's Birthday
  FstSttDt<-min(dataframe$Start,na.rm=TRUE) # no zeros - this returns Inf sometimes b/c I take out na's, but I'll take them out later
  thisEro<-dataframe[dataframe$Start==FstSttDt,"ERO"][1] # to handle many instances of the same ERO - just one index - next date filter will exclude others
  FstEndDt<-dataframe[dataframe$Start==FstSttDt,"End"][1]
  if(unbug){print(1);print(length(thisEro));print(thisEro)}
  if(length(thisEro)>1){stop("multiple eros with the same date and part Start")}
  #FstEndDt<-dataframe[dataframe$ERO==thisEro & dataframe$Start==FstSttDt,"End"][1] # end date of the ERO for the first Start date (all end dates are the same)
  if(unbug){print(c(FstSttDt,FstEndDt))}
  
  #dataframe<-dataframe[dataframe$Start>FstOrdDt & dataframe$ERO != thisEro,] # ignore dates before 1st ero start date
  dataframe<-dataframe[dataframe$Start>FstSttDt,] # ignore dates before 1st removal (ERO start date)
  ScdSttDt<-min(dataframe$Start,na.rm=TRUE)
  thisEro<-dataframe[dataframe$Start==ScdSttDt,"ERO"][1]
  ScdEndDt<-dataframe[dataframe$Start==ScdSttDt,"End"][1]
  if(unbug){print(2);print(length(thisEro));print(thisEro)}
  if(length(thisEro)>1){stop("multiple eros with the same date and part Start")}
  # end date of the ERO
  if(unbug){print(c(ScdSttDt,ScdEndDt))}
  
  dataframe<-dataframe[dataframe$Start>ScdSttDt,] # ignore dates before 2nd removal
  ThdSttDt<-min(dataframe$Start,na.rm=TRUE)
  thisEro<-dataframe[dataframe$Start==ThdSttDt,"ERO"][1]
  ThdEndDt<-dataframe[dataframe$Start==ThdSttDt,"End"][1]
  if(unbug){print(3);print(length(thisEro));print(thisEro)}
  if(length(thisEro)>1){stop("multiple eros with the same date and part Start")}
  #ThdEndDt<-dataframe[dataframe$ERO==thisEro,"End"][1] # end date of the ERO
  
  dataframe<-dataframe[dataframe$Start>ThdSttDt,] # ignore dates before 3rd removal
  FurSttDt<-min(dataframe$Start,na.rm=TRUE)
  thisEro<-dataframe[dataframe$Start==FurSttDt,"ERO"][1]
  FurEndDt<-dataframe[dataframe$Start==FurSttDt,"End"][1]
  if(unbug){print(4);print(length(thisEro));print(thisEro)}
  if(length(thisEro)>1){stop("multiple eros with the same date and part Start")}
  #FurEndDt<-dataframe[dataframe$ERO==thisEro,"End"][1] # end date of the ERO
  
  #dataframe<-dataframe[dataframe$Start>FurSttDt & dataframe$ERO != thisEro,] # ignore dates before 4th removal
  dataframe<-dataframe[dataframe$Start>FurSttDt,] # ignore dates before 4th removal
  FvhSttDt<-min(dataframe$Start,na.rm=TRUE)
  thisEro<-dataframe[dataframe$Start==FvhSttDt,"ERO"][1]
  FvhEndDt<-dataframe[dataframe$Start==FvhSttDt,"End"][1]
  if(unbug){print(5);print(length(thisEro));print(thisEro)}
  if(length(thisEro)>1){stop("multiple eros with the same date and part Start")}
  #FvhEndDt<-dataframe[dataframe$ERO==thisEro,"End"][1] # end date of the ERO
  
  # convert to dates
  Birthday<-as.Date(Birthday, origin = '1899-12-30') # I could also get the meter reading difference
  FstSttDt<-as.Date(FstSttDt, origin = '1899-12-30') # use start date of ero instead of Sttered date
  ScdSttDt<-as.Date(ScdSttDt, origin = '1899-12-30') # unique ero is SN & ERO & Start Date ?
  ThdSttDt<-as.Date(ThdSttDt, origin = '1899-12-30') # I'm not accounting for downtime for other parts in between
  FurSttDt<-as.Date(FurSttDt, origin = '1899-12-30') # I'm not accounting for parts with sl qty > 1 (remove 1 part, then the 2nd different part a week later = 1 week removal time currently) 
  FvhSttDt<-as.Date(FvhSttDt, origin = '1899-12-30') # I'm not accounting for new PN introductions and PN retirements and PN interchangeability/subsitutability
  FstEndDt<-as.Date(FstEndDt, origin = '1899-12-30')
  ScdEndDt<-as.Date(ScdEndDt, origin = '1899-12-30')
  ThdEndDt<-as.Date(ThdEndDt, origin = '1899-12-30') # doesn't handle overlapping eros - gives negative time, which I zero out
  FurEndDt<-as.Date(FurEndDt, origin = '1899-12-30') # its' 2.5 years between the first birthday and the first part removal - what gives?
  FvhEndDt<-as.Date(FvhEndDt, origin = '1899-12-30')
  
  # now calculated the date differences, using suspension times where applicable for first removal that doesnt happen 
  diff1<-as.numeric(FstSttDt-Birthday) # in service data will be the same for this SN in all rows, pick 1
  diff2<-as.numeric(ScdSttDt-FstEndDt); if(unbug){print(c(ScdSttDt,FstEndDt,ScdSttDt-FstEndDt))}
  diff3<-as.numeric(ThdSttDt-ScdEndDt)
  diff4<-as.numeric(FurSttDt-ThdEndDt) # some Infs b/c some dates are min(dates,na.rm=T) when data frame is empty
  diff5<-as.numeric(FvhSttDt-FurEndDt)
  
  # also return the last End Date for this NSN/SN combination BUT THIS ISN'T WORKING SO I'M RETURN NA AND HATING IT
  #LstEroEnd<-as.Date(ifelse(is.na(as.character(ScdEndDt)),FstEndDt,ifelse(is.na(as.character(ThdEndDt)),ScdEndDt,
  #             ifelse(is.na(as.character(FurEndDt)),ThdEndDt,ifelse(is.na(as.character(FvhEndDt)),FurEndDt,NA)))),origin='1899-12-30')
  LstEroEnd<-NA
  
  data.frame(FstSttDt,ScdSttDt,ThdSttDt,FurSttDt,FvhSttDt,diff1,diff2,diff3,diff4,diff5,LstEroEnd)
}

# test dataset
a<-rawEros[1:2000,]
a<-rawEros[1:50,]
#system.time(b<-ddply(a,c("SerialNumber","FSC"),first5Ordered))
system.time(c<-ddply(a,c("SerialNumber","NSN"),first5Ordered,TRUE))
rawEros[rawEros$NSN==1015011799369 & rawEros$SerialNumber==572348,]# an example
rawEros[rawEros$NSN==6240000446914 & rawEros$SerialNumber==649032,]# another
rawEros[rawEros$NSN==1015012093482 & rawEros$SerialNumber==572348,]# another
rawEros[rawEros$NSN==4210011791059 & rawEros$SerialNumber==572352,]# with negative time
rawEros[rawEros$NSN==4330011182868 & rawEros$SerialNumber==572352,]# another with negative time
rawEros[rawEros$NSN==5120002237397 & rawEros$SerialNumber==572348,]# another with negative time
rawEros[rawEros$NSN==2530013458887 & rawEros$SerialNumber==572348,]# another with negative time

system.time(SNfscminOrdered5<-ddply(subsetofthedata,c("SerialNumber","FSC"),first5Ordered,unbug=FALSE))

#system.time(SNfscminOrdered5<-ddply(rawEros,c("SerialNumber","FSC"),first5Ordered,unbug=FALSE))
system.time(SNnsnminOrdered5<-ddply(rawEros,c("SerialNumber","NSN"),first5Ordered)) # 900 seconds
#colnames(SNnsnminOrdered)<-c("SerialNumber","NSN","NSN.FstOrdDt")

# save/load that data
#write.csv(SNfscminOrdered5,"SNfscminOrdered5.csv")
#write.csv(SNnsnminOrdered5,"SNnsnminOrdered5.csv")
#SNfscminOrdered5<-read.csv("SNfscminOrdered5.csv")
##   SNnsnminOrdered5<-read.csv("SNnsnminOrdered5.csv") ##

# FSC NSN and group
SNnsnminOrdered5$NSN<-as.numeric(SNnsnminOrdered5$NSN)
SNnsnminOrdered5$FSC<-floor(SNnsnminOrdered5$NSN,-9)/1000000000 # add fsc
SNnsnminOrdered5$Grp<-floor(SNnsnminOrdered5$NSN,-11)/100000000000 # add group
SNnsnminOrdered5$NSN<-as.character(SNnsnminOrdered5$NSN)

# makes ugly INF and NANs - make the Infs NA, NaN are treated as NA (not vice versa)
SNnsnminOrdered5$diff2[which(SNnsnminOrdered5$diff2==Inf)]<-NA
SNnsnminOrdered5$diff3[which(SNnsnminOrdered5$diff3==Inf)]<-NA
SNnsnminOrdered5$diff4[which(SNnsnminOrdered5$diff4==Inf)]<-NA
SNnsnminOrdered5$diff5[which(SNnsnminOrdered5$diff5==Inf)]<-NA
nrow(SNnsnminOrdered5) # 117185
# only 20% of 2nd removals are non-zero at this point
sum(!is.na(SNnsnminOrdered5$diff2))/length(is.na(SNnsnminOrdered5$diff2))
# zero out the negative or zero removal times
SNnsnminOrdered5$diff2[which(SNnsnminOrdered5$diff2<=0)]<-NA
SNnsnminOrdered5$diff3[which(SNnsnminOrdered5$diff3<=0)]<-NA
SNnsnminOrdered5$diff4[which(SNnsnminOrdered5$diff4<=0)]<-NA
SNnsnminOrdered5$diff5[which(SNnsnminOrdered5$diff5<=0)]<-NA
# now only 18% of 2nd removals are non-zero 
sum(!is.na(SNnsnminOrdered5$diff2))/length(is.na(SNnsnminOrdered5$diff2))

write.csv(SNnsnminOrdered5,"moddSNnsnminOrdered5.csv")


################## try to fit some data
# remove the 0's b/c won't work with weibull

# do the same data cleaning for nsn data
SNnsnminOrdered<-SNnsnminOrdered[-which(SNnsnminOrdered$diff==0),];nrow(SNnsnminOrdered) # remove 1, now 117184 rows
SNnsnminOrdered<-SNnsnminOrdered[-which(is.na(SNnsnminOrdered$FSC)),];nrow(SNnsnminOrdered) # remove 21, now 117163
# do the same data cleaning for 5-removal nsn data
nrow(SNnsnminOrdered5) # 117185
SNnsnminOrdered5<-SNnsnminOrdered5[-which(is.na(SNnsnminOrdered5$FSC)),];nrow(SNnsnminOrdered5) # remove 22, now 117163

SNnsnminOrdered5<-read.csv("moddSNnsnminOrdered5.csv",stringsAsFactors=FALSE) ################################### read HERE 
# fix the dates
SNnsnminOrdered5$FstSttDt<-as.Date(SNnsnminOrdered5$FstSttDt, origin = '1899-12-30') 
SNnsnminOrdered5$ScdSttDt<-as.Date(SNnsnminOrdered5$ScdSttDt, origin = '1899-12-30') 
SNnsnminOrdered5$ThdSttDt<-as.Date(SNnsnminOrdered5$ThdSttDt, origin = '1899-12-30') 
SNnsnminOrdered5$FurSttDt<-as.Date(SNnsnminOrdered5$FurSttDt, origin = '1899-12-30') 
SNnsnminOrdered5$FvhSttDt<-as.Date(SNnsnminOrdered5$FvhSttDt, origin = '1899-12-30') 
# IT'S STILL MISSING SERIAL NUMBER START AND END DATES, but these are unnnecessary when using the "totals" db b/c its merged after this df

### weibulls with the first removal from each NSN grouped into FSC
#################################################################### SKIP TO TOTALS
# optimisation frequently fails.  try a few different methods
trytofit<-function(df,categ,thisCateg,startingscale,onetwo=1){
  # fits a weilbull, or returns error that is handled outside
  # args: df: dataframe: a subset of the data frame
  #  categ: string: a category to match on: fsc, group, or nsn
  #  thisCateg: one individual value of the category
  # startingscale: numeric: a starting scale to try, vary so that it has better chance to
  # onetwo: numeric: fit one weibull (the first removal) or two (this plus all the others as a second)
  # returns - fitdistr class, with parameters and standard errors
  onetwo<-paste0("diff",onetwo)
  df<-df[which(df[,categ]==thisCateg),]
  suppressWarnings(fitdistr(x=df[,onetwo],densfun='weibull',
                            start=list(shape=1,scale=startingscale),lower=0.1,upper=10000))
}
# define this function - don't fit anything with fewer than n=10
fitOneCategWeibulls<-function(categ,diffDF,justFirst=TRUE){
  # fits one category of weibulls
  # args: categ: string: fit group nsns to fit by Group, FSC, or just NSN
  #   diffdf: dataframe: has rows per nsn with difference between birth and 1st removal, 1st and 2nd, etc.
  #   justFirst: boolean: true if fit only the first removal
  # returns: weibs2store df that has zero or more weibulls per classification (nsn, fsc, or group)
  # create the initial weibull storage database to return
  tl<-length(unique(diffDF[,categ]))
  if(justFirst){
    weibs2store<-data.frame("categ"=1:tl,"n"=1:tl,"shape"=1:tl,
                            "scale"=1:tl,"shapese"=1:tl,"scalese"=1:tl)
  }
  else{ # find two weibulls per categ
    weibs2store<-data.frame("categ"=1:tl,"n"=1:tl,"shape"=1:tl,
                            "scale"=1:tl,"shapese"=1:tl,"scalese"=1:tl,
                            "n2"=1:tl,"shape2"=1:tl,
                            "scale2"=1:tl,"shapese2"=1:tl,"scalese2"=1:tl)
  }
  colnames(weibs2store)[1]<-categ # assign to fsc, grp, or nsn
  for (ii in 1:length(unique(diffDF[,categ]))){
    thisCateg<-unique(diffDF[,categ])[ii]
    weibs2store[ii,1]<-thisCateg # store the fsc
    # store number of rows
    n<-nrow(diffDF[diffDF[,categ]==unique(diffDF[,categ])[ii],])
    weibs2store[ii,2]<-n
    # then store the fit (skip errors)
    if (n>9) {suppressWarnings(b<-tryCatch(expr=fitdistr(x=diffDF[diffDF[,categ]==thisCateg,"diff1"],densfun="weibull"),
                                           error=function(cond){return(tryCatch(expr=trytofit(diffDF,categ,thisCateg,mean(diffDF[diffDF[,categ]==thisCateg,"diff1"])),
                                                                                error=function(cond){return(tryCatch(expr=trytofit(diffDF,categ,thisCateg,median(diffDF[diffDF[,categ]==thisCateg,"diff1"])),
                                                                                                                     error=function(cond){return(tryCatch(expr=trytofit(diffDF,categ,thisCateg,100),
                                                                                                                                                          error=function(cond){return(NA)}))}))}))}))
              weibs2store[ii,3]<-b[[1]][1] # shape coef
              weibs2store[ii,4]<-b[[1]][2] # scale coef
              weibs2store[ii,5]<-tryCatch(expr=b[[2]][1],error=function(cond){return(NA)})
              weibs2store[ii,6]<-tryCatch(expr=b[[2]][2],error=function(cond){return(NA)})
    }
    else {
      weibs2store[ii,3]<-NA;weibs2store[ii,4]<-NA;weibs2store[ii,5]<-NA;weibs2store[ii,6]<-NA
    }
    # if fitting more that just the first removal, do so here within the loop
    if(!justFirst){
      otherDiffDF<-diffDF[diffDF[,categ]==unique(diffDF[,categ])[ii],]
      # first gather all the other data to fit one weibull
      otherDiffs<-c(otherDiffDF$diff2,otherDiffDF$diff3,otherDiffDF$diff4,otherDiffDF$diff5)
      otherDiffs<-otherDiffs[!is.na(otherDiffs)]
      # store number of rows
      n<-length(otherDiffs)
      weibs2store[ii,]$n2<-n
      # then store the fit (skip errors)
      if (n>9) {suppressWarnings(b<-tryCatch(expr=fitdistr(x=otherDiffs,densfun="weibull"),
                                             error=function(cond){return(tryCatch(expr=fitdistr(x=otherDiffs,densfun="weibull",mean(otherDiffs)),
                                                                                  error=function(cond){return(tryCatch(expr=fitdistr(x=otherDiffs,densfun="weibull",median(otherDiffs)),
                                                                                                                       error=function(cond){return(tryCatch(expr=fitdistr(x=otherDiffs,densfun="weibull",100),
                                                                                                                                                            error=function(cond){return(NA)}))}))}))}))
                weibs2store[ii,]$shape2<-b[[1]][1] # shape coef
                weibs2store[ii,]$scale2<-b[[1]][2] # scale coef
                weibs2store[ii,]$shapese2<-tryCatch(expr=b[[2]][1],error=function(cond){return(NA)})
                weibs2store[ii,]$scalese2<-tryCatch(expr=b[[2]][2],error=function(cond){return(NA)})
      }
      else {
        weibs2store[ii,8]<-NA;weibs2store[ii,9]<-NA;weibs2store[ii,10]<-NA;weibs2store[ii,11]<-NA
      }
    }
  }
  return(weibs2store)
}

system.time(weibs2storeGrp<-fitOneCategWeibulls("Grp",SNnsnminOrdered5,justFirst=FALSE))
system.time(weibs2storeFSC<-fitOneCategWeibulls("FSC",SNnsnminOrdered5,justFirst=FALSE))
system.time(weibs2storeNSN<-fitOneCategWeibulls("NSN",SNnsnminOrdered5,justFirst=FALSE))

write.csv(weibs2storeGrp,"thirdPassGrpweibs.csv")
write.csv(weibs2storeFSC,"thirdPassFSCweibs.csv")
write.csv(weibs2storeNSN,"thirdPassNSNweibs.csv")
shapes<-as.numeric(weibs2storeFSC$shape) # 142 NAs with just median, 65 with med & no start, 62 with med and mean and no start
hist(shapes,breaks=20)#[-which(shapes2==max(shapes2,na.rm=TRUE))],breaks=40)
sum(shapes2<2,na.rm=TRUE)/sum(shapes2>0,na.rm=TRUE) # prop. shape between 1 and 2
sum(shapes2<1,na.rm=TRUE)/sum(shapes2>0,na.rm=TRUE) # prop. shape between 0 and 1
nrow(weibs2storeGrp[is.na(weibs2storeGrp[,4]),])/nrow(weibs2storeGrp) # prop. that didn't fit
nrow(weibs2storeGrp[is.na(weibs2storeGrp[weibs2storeGrp$n>9,4]),])/nrow(weibs2storeGrp[weibs2storeGrp$n>9,]) # prop that didn't fit and had > 9 data points
nrow(weibs2storeFSC[is.na(weibs2storeFSC[weibs2storeFSC$n>9,4]),])/nrow(weibs2storeFSC[weibs2storeFSC$n>9,]) # prop that didn't fit and had > 9 data points
nrow(weibs2storeNSN[is.na(weibs2storeNSN[weibs2storeNSN$n>9,4]),])/nrow(weibs2storeNSN[weibs2storeNSN$n>9,]) # prop that didn't fit and had > 9 data points
shapes2<-as.numeric(weibs2storeFSC$shape2) # 142 NAs with just median, 65 with med & no start, 62 with med and mean and no start
hist(shapes2,breaks=20)#[-which(shapes2==max(shapes2,na.rm=TRUE))],breaks=40)

# test
oneNSN1<-SNnsnminOrdered5[SNnsnminOrdered5$NSN==1015011799369,"diff1"]
otras<-SNnsnminOrdered5[SNnsnminOrdered5$NSN==1015011799369,]
oneNSN2<-c(otras$diff2,otras$diff3,otras$diff4,otras$diff5)
oneNSN2<-oneNSN2[!is.na(oneNSN2)]
p1<-hist(oneNSN1,plot=FALSE);p2<-hist(oneNSN2,plot=FALSE)
plot(p1,col=rgb(0,0,1,1/4), xlim=c(0,max(p2$breaks,p1$breaks)),ylim=c(0,max(p1$counts,p2$counts)))
plot(p2,col=rgb(1,0,0,1/4), xlim=c(0,max(p2$breaks,p1$breaks)),ylim=c(0,max(p1$counts,p2$counts)),add=TRUE)

### fit NSNs
tl<-length(unique(SNnsnminOrdered5$NSN))
weibs2storeNSN<-data.frame("NSN"=1:tl,"n"=1:tl,"shape"=1:tl,
                           "scale"=1:tl,"shapese"=1:tl,"scalese"=1:tl)
# optimisation frequently fails.  try a few different methods
trytofitNSN<-function(df,thisNSN,startingscale){
  suppressWarnings(fitdistr(x=df[df$NSN==thisNSN,"diff1"],
                            densfun='weibull',start=list(shape=1,scale=startingscale),lower=0.1,upper=10000))
}
# don't fit anything under 10 here as well
system.time(for (ii in 1:length(unique(SNnsnminOrdered$NSN))){
  thisNSN<-unique(SNnsnminOrdered$NSN)[ii]
  weibs2storeNSN[ii,1]<-thisNSN # store the fsc
  # store number of rows
  n<-nrow(SNnsnminOrdered[SNnsnminOrdered$NSN==unique(SNnsnminOrdered$NSN)[ii],])
  weibs2storeNSN[ii,2]<-n
  # then store the fit (skip errors)
  if (n>9) {suppressWarnings(b<-tryCatch(expr=fitdistr(x=SNnsnminOrdered[SNnsnminOrdered$NSN==thisNSN,"diff"],densfun="weibull"),
                                         error=function(cond){return(tryCatch(expr=trytofitNSN(SNnsnminOrdered,thisNSN,mean(SNnsnminOrdered[SNnsnminOrdered$NSN==thisNSN,"diff"])),
                                                                              error=function(cond){return(tryCatch(expr=trytofitNSN(SNnsnminOrdered,thisNSN,median(SNnsnminOrdered[SNnsnminOrdered$NSN==thisNSN,"diff"])),
                                                                                                                   error=function(cond){return(tryCatch(expr=trytofitNSN(SNnsnminOrdered,thisNSN,100),
                                                                                                                                                        error=function(cond){return(NA)}))}))}))}))
            weibs2storeNSN[ii,3]<-b[[1]][1] # shape coef
            weibs2storeNSN[ii,4]<-b[[1]][2] # scale coef
            weibs2storeNSN[ii,5]<-tryCatch(expr=b[[2]][1],error=function(cond){return(NA)})
            weibs2storeNSN[ii,6]<-tryCatch(expr=b[[2]][2],error=function(cond){return(NA)})
  }
  else {
    weibs2storeNSN[ii,3]<-NA;weibs2storeNSN[ii,4]<-NA;weibs2storeNSN[ii,5]<-NA;weibs2storeNSN[ii,6]<-NA
  }
} )

write.csv(weibs2storeNSN,"NSNweibs.csv")
shapes2<-as.numeric(weibs2storeNSN$shape) # 142 NAs with just median, 65 with med & no start, 62 with med and mean and no start
hist(shapes2,breaks=20)#[-which(shapes2==max(shapes2,na.rm=TRUE))],breaks=40)
sum(shapes2<2,na.rm=TRUE)/sum(shapes2>0,na.rm=TRUE) # prop. shape between 1 and 2
sum(shapes2<1,na.rm=TRUE)/sum(shapes2>0,na.rm=TRUE) # prop. shape between 0 and 1
nrow(weibs2storeNSN[is.na(weibs2storeNSN[,4]),])/nrow(weibs2storeNSN) # prop. that didn't fit


### fit NSNs
tl<-length(unique(SNnsnminOrdered$Grp))
weibs2storeGrp<-data.frame("Group"=1:tl,"n"=1:tl,"shape"=1:tl,
                           "scale"=1:tl,"shapese"=1:tl,"scalese"=1:tl)
# optimisation frequently fails.  try a few different methods
trytofitGrp<-function(df,thisGrp,startingscale){
  suppressWarnings(fitdistr(x=df[df$Grp==thisGrp,"diff"],
                            densfun='weibull',start=list(shape=1,scale=startingscale),lower=0.1,upper=10000))
}
# don't fit anything under 10 here as well
system.time(for (ii in 1:length(unique(SNnsnminOrdered$Grp))){
  thisGrp<-unique(SNnsnminOrdered$Grp)[ii]
  weibs2storeGrp[ii,1]<-thisGrp # store the Grp
  # store number of rows
  n<-nrow(SNnsnminOrdered[SNnsnminOrdered$Grp==unique(SNnsnminOrdered$Grp)[ii],])
  weibs2storeGrp[ii,2]<-n
  # then store the fit (skip errors)
  if (n>9) {suppressWarnings(b<-tryCatch(expr=fitdistr(x=SNnsnminOrdered[SNnsnminOrdered$Grp==thisGrp,"diff"],densfun="weibull"),
                                         error=function(cond){return(tryCatch(expr=trytofitGrp(SNnsnminOrdered,thisGrp,mean(SNnsnminOrdered[SNnsnminOrdered$Grp==thisGrp,"diff"])),
                                                                              error=function(cond){return(tryCatch(expr=trytofitGrp(SNnsnminOrdered,thisNSN,median(SNnsnminOrdered[SNnsnminOrdered$Grp==thisGrp,"diff"])),
                                                                                                                   error=function(cond){return(tryCatch(expr=trytofitGrp(SNnsnminOrdered,thisGrp,1000),
                                                                                                                                                        error=function(cond){return(NA)}))}))}))}))
            weibs2storeGrp[ii,3]<-b[[1]][1] # shape coef
            weibs2storeGrp[ii,4]<-b[[1]][2] # scale coef
            weibs2storeGrp[ii,5]<-tryCatch(expr=b[[2]][1],error=function(cond){return(NA)})
            weibs2storeGrp[ii,6]<-tryCatch(expr=b[[2]][2],error=function(cond){return(NA)})
  }
  else {
    weibs2storeGrp[ii,3]<-NA;weibs2storeGrp[ii,4]<-NA;weibs2storeGrp[ii,5]<-NA;weibs2storeGrp[ii,6]<-NA
  }
} )

write.csv(weibs2storeGrp,"Groupweibs.csv")
shapes2<-as.numeric(weibs2storeGrp$shape) # 142 NAs with just median, 65 with med & no start, 62 with med and mean and no start
hist(shapes2,breaks=20)#[-which(shapes2==max(shapes2,na.rm=TRUE))],breaks=40)
sum(shapes2<2,na.rm=TRUE)/sum(shapes2>0,na.rm=TRUE) # prop. shape between 1 and 2
sum(shapes2<1,na.rm=TRUE)/sum(shapes2>0,na.rm=TRUE) # prop. shape between 0 and 1
nrow(weibs2storeGrp[is.na(weibs2storeGrp[,4]),])/nrow(weibs2storeGrp) # prop. that didn't fit


################################
# side project - is poisson or negative binomial good for spares form to estimate failures per year
fails<-rweibull(1000,1.2,10)
plot(fails)
failsdf<-data.frame("times"=fails,"cumtimes"=fails[1])
for (ii in 2:length(fails)){
  failsdf[ii,2]<-failsdf[ii,1]+failsdf[ii-1,2]
}
################################


## fit weibulls for all-other removals (1st removal was calculated previously)




################################# SHIPPING DATA
## which metric to use to see overlap in orders?
# proportion of orders of an NSN with other NSNs
# proportion of orders of an NSN where it was the longest wait time - 
#     only apply wait time this proportion of times
rawEros$LeadTime<-rawEros$Received-rawEros$Ordered
sum(is.na(rawEros$LeadTime)) # 21745
# take out the negative ones lead times
sum(rawEros$LeadTime<0,na.rm=TRUE)/nrow(rawEros) # only 277 of these, 0.16%
rawEros[which(rawEros$LeadTime<0),"LeadTime"]<-NA
hist(as.numeric(rawEros[rawEros$FSC==1095,"LeadTime"])) # fairly normal fsc, weapons
hist(as.numeric(rawEros[rawEros$FSC==5925,"LeadTime"])) # long outliers, circuit breakers

a<-head(unique(rawEros$NSN)) # initial toy dataset



wasLongestLeadTimeInERO<-function(subsetRE,fullRawEros,unbug=FALSE){
  # A function for use with ddply on each NSN
  # Args: subsetRE - subset of raw EROs that only includes rows with this one NSN
  #       fullRawEROS - the entire EROs df
  # Returns: ppnLLT - proportion of times this NSN is the longest lead time in the ERO (or tied with)
  eros4NSN<-unique(subsetRE$ERO) # all eros that that NSN is in
  eros4NSN<-eros4NSN[which(!is.na(eros4NSN))]
  NSN4eros<-unique(subsetRE$NSN) # just the one NSN
  NSN4eros<-NSN4eros[which(!is.na(NSN4eros))]
  # for each ERO, find if the NSN has the longest lead time there
  a<-0 # inialize ero count
  for (ii in 1:length(eros4NSN)){ # for each ERO with this NSN
    justRelEro<-fullRawEros[fullRawEros$ERO==eros4NSN[ii],]
    # if this NSN has the longest lead time of the ERO, increment a
    if(NSN4eros %in% justRelEro[which(justRelEro$LeadTime==max(as.numeric(justRelEro[justRelEro$ERO==eros4NSN[ii],"LeadTime"]),na.rm=TRUE)),"NSN"]) {
      a<-a+1 # sometimes no orders are received in an ERO, giving a false to above check
    }
  }
  data.frame("ppnLLT"=a/length(eros4NSN))
  if(unbug){print(c(round(ii,0),NSN4eros))}
}

# a lot of duplicate work - better to find the one or group of NSNs that have the 
#   longest lead times in an ERO

subsetofthedata<-rawEros[1:20000,]
system.time(LLT<-ddply(.data=subsetofthedata,.variables="NSN",.fun=wasLongestLeadTimeInERO,rawEros,TRUE))
#system.time(LLT<-ddply(.data=rawEros,.variables="NSN",.fun=wasLongestLeadTimeInERO,rawEros)) # full dataset passed
## - NOT FINISHED - moving on to other things


############## some efforts to find the suspensions
# there are so many parts that are never removed from a tank (almost 97% of the parts, 8000+ NSNs per 448 Serial Numbers)
partnumbers<-data.frame("NSN"=(unique(rawEros$NSN)),stringsAsFactors = FALSE)
serialnumbers<-data.frame("SerialNumber"=unique(rawEros$SerialNumber))
system.time(totals<-merge(x=partnumbers,y=serialnumbers,by=NULL))
system.time(totals<-join(x=totals,y=SNnsnminOrdered5,by=c("SerialNumber","NSN"),type="full"))
# bring over in service date
totals<-join(x=totals,y=prodYear[,c("SerialNumber","IN.SERVICE")],by="SerialNumber",type="full") # may have to trim down if SNminOrdered has the difference column
# bring over max date this serial number ordered a part
totals<-join(x=totals,y=SNmaxStart,by="SerialNumber",type="full")
## suspension data
totals[which(is.na(totals$diff1)),"diff1"]<-as.numeric(totals[which(is.na(totals$diff1)),"LstSttDt"]-totals[which(is.na(totals$diff1)),"IN.SERVICE"])
# need the other differences - only have one suspension per row
system.time(totals[which(is.na(as.character(totals$ScdSttDt)) & !is.na(as.character(totals$FstSttDt))),"diff2"]<-
              as.numeric(totals[which(is.na(as.character(totals$ScdSttDt)) & !is.na(as.character(totals$FstSttDt))),"LstSttDt"]-
                           totals[which(is.na(as.character(totals$ScdSttDt)) & !is.na(as.character(totals$FstSttDt))),"FstSttDt"]))
# I really need to subtract the previous END date instead of the previous START date, but I can't get that working right now!
system.time(totals[which(is.na(as.character(totals$ThdSttDt)) & !is.na(as.character(totals$ScdSttDt))),"diff3"]<-
              as.numeric(totals[which(is.na(as.character(totals$ThdSttDt)) & !is.na(as.character(totals$ScdSttDt))),"LstSttDt"]-
                           totals[which(is.na(as.character(totals$ThdSttDt)) & !is.na(as.character(totals$ScdSttDt))),"ScdSttDt"]))
system.time(totals[which(is.na(as.character(totals$FurSttDt)) & !is.na(as.character(totals$ThdSttDt))),"diff4"]<-
              as.numeric(totals[which(is.na(as.character(totals$FurSttDt)) & !is.na(as.character(totals$ThdSttDt))),"LstSttDt"]-
                           totals[which(is.na(as.character(totals$FurSttDt)) & !is.na(as.character(totals$ThdSttDt))),"ThdSttDt"]))
system.time(totals[which(is.na(as.character(totals$FvhSttDt)) & !is.na(as.character(totals$FurSttDt))),"diff5"]<-
              as.numeric(totals[which(is.na(as.character(totals$FvhSttDt)) & !is.na(as.character(totals$FurSttDt))),"LstSttDt"]-
                           totals[which(is.na(as.character(totals$FvhSttDt)) & !is.na(as.character(totals$FurSttDt))),"FurSttDt"]))
## need to remove a few zeros (again) b/c of suspension times
length(which(totals$diff1<=0));totals$diff1[which(totals$diff1<=0)]<-NA
length(which(totals$diff2<=0));totals$diff2[which(totals$diff2<=0)]<-NA
length(which(totals$diff3<=0));totals$diff3[which(totals$diff3<=0)]<-NA
length(which(totals$diff4<=0));totals$diff4[which(totals$diff4<=0)]<-NA
length(which(totals$diff5<=0));totals$diff5[which(totals$diff5<=0)]<-NA
# FSC NSN and group
totals$NSN<-as.numeric(totals$NSN)
totals$FSC<-floor(totals$NSN,-9)/1000000000 # add fsc
totals$Grp<-floor(totals$NSN,-11)/100000000000 # add group
totals$NSN<-as.character(totals$NSN)
# this SN has an ERO before its supposed birthday rawEros[rawEros$NSN %in% 4330011182868 & rawEros$SerialNumber == 585978,] 
#   it gets an error diff1 value; same with the same NSN and SN 585976;  there are 46 cases, so remove them
dim(totals);dim(totals[which(totals$diff1==Inf),])
totals<-totals[-which(totals$diff1==Inf),]
dim(totals)

# dim should be 3630994, not one more. if theres a bad row at the end try totals<-totals[-nrow(totals),]
# write to a database (before it's too late)
myconn<-odbcConnectAccess("C:/Users/tbaer/Desktop/m1a1/totalsDB")
#system.time(sqlSave(myconn,totals,rownames=FALSE,varTypes=c(NSN="Number",SerialNumber="Text",FstSttDt="Date",ScdSttDt="Date",
#             ThdSttDt="Date",FurSttDt="Date",FvhSttDt="Date",IN.SERVICE="Date",LstSttDt="Date"))) # about 13 minutes
system.time(totals<-sqlQuery(myconn,"SELECT * FROM totals")) # about six minutes
remove(myconn)
# if reading db, may need to convert dates back to R's date format with as.Date()
# missed the FSC and Grp


# for fitdistcens: 1 is a success (fail), 0 is a defer (suspension)
#### too big to store in csv format
###### need other suspension times later
####
print(object.size(totals),units="MB")
dim(totals) # 3,630,994
# take out a weird SN  -   totals<-totals[totals$SerialNumber!=642701,] # no longer there
source("C:/Users/tbaer/Desktop/m1a1/randCWwork/m1functions.R")
# subsetting by NSN - it doesn't like the character - have to use %in%
system.time(leftandright<-CensUncensm1(totals[totals$NSN %in% 5963014746208,])) # 5310009388387
system.time(zweib<-fitdistcens(leftandright,distr="weibull")) # sometimes singular - not enough failures
#zweib<-fitdistcens(data.frame("left"=a[,1],"right"=a[,1]),dist="weibull") # works (groups of m1's suspended at same time)
plot(zweib);zweib$estimate
mean(rweibull(5000,shape=zweib$estimate[1],scale=zweib$estimate[2]))

# compare my numbers to SOE
# find the costliest NSN, the engine 2835014087048
#system.time(totalCost1<-ddply(rawEros,"NSN",function(df){data.frame("totalCost"=sum(df$Cost))}))
# plyr function takes 17 seconds, dplyr function takes 0.03 seconds
system.time(totalCost2<-summarise(group_by(rawEros, NSN),"totalCost"=sum(Cost,na.rm=TRUE)))
totalCost<-totalCost2;remove(totalCost1);remove(totalCost2)
head(arrange(totalCost,-totalCost))#head(totalCost[order(-totalCost$totalCost),])
# print out some summary stats by NSN to compare to EROS
system.time(nsnSummary<-summarise(group_by(rawEros, NSN),"totalCost"=sum(Cost,na.rm=TRUE),"n"=n(),
                                  "firstOrd"=min(Ordered),"lastOrd"=max(Ordered)))
write.csv(nsnSummary,"eroNSNsummary.csv")

# total days on test for oldest part (hard to say when a new/upgraded part is installed)
# soe -  1,726,593 - parts * days/failure / slqty
# my method is slightly different - I'm using last ordered date for each NSN
sum(totals$LstOrdDt-totals$IN.SERVICE) # not sure how to do this

##### routine for fitting many weibulls
hist(table(SNnsnminOrdered5$NSN)) # how many true failures each NSN has
sum(table(SNnsnminOrdered5$NSN)>30,na.rm=TRUE)/length(table(SNnsnminOrdered5$NSN)) # 12% of data
sum(table(SNnsnminOrdered5$NSN)>150,na.rm=TRUE) # 121 NSNs
sum(table(SNnsnminOrdered5$NSN)>200,na.rm=TRUE) # 44 NSNs
# start with using 150 as as a cutoff of failures to improve chances of fitting and test speed
theseNSNs<-rownames(table(SNnsnminOrdered5$NSN))[which(table(SNnsnminOrdered5$NSN)>150)] # 200 works with no NAs, so does 50 (has a NaN error?). 20 does not
theseNSNs<-2530015319542
datasubset<-totals[totals$NSN %in% theseNSNs,]

source("C:/Users/tbaer/Desktop/m1a1/randCWwork/m1functions.R")
system.time(weibs<-gatherDistFitsm1(datasubset,"weibull","NSN",unbug=FALSE,modkm=FALSE,ontwth=3))
system.time(weibs<-gatherDistFitsm1(totals,"NSN",unbug=FALSE,modkm=FALSE,ontwth=1))
weibs[,"shape"]<-as.numeric(as.character(weibs[,"shape"]))
head(weibs[order(weibs$Events),])
hist(totals[totals$NSN %in% 2540012553347 & !is.na(totals$FstSttDt),"diff1"])
head(weibs[order(runif(nrow(weibs))),],10) # ten random ones
hist(weibs$shape)


# collect them
system.time(weibsAN<-gatherDistFitsm1(totals,"weibull","NSN",unbug=FALSE,modkm=FALSE,ontwth=1)) # 377 seconds; 435
system.time(weibsAF<-gatherDistFitsm1(totals,"weibull","FSC",unbug=FALSE,modkm=FALSE,ontwth=1)) # 80 seconds
system.time(weibsAG<-gatherDistFitsm1(totals,"weibull","Grp",unbug=FALSE,modkm=FALSE,ontwth=1)) # 76 seconds
system.time(weibsBN<-gatherDistFitsm1(totals,"weibull","NSN",unbug=FALSE,modkm=FALSE,ontwth=2)) # 51 seconds - many few data points b/c so few 2nd removals
system.time(weibsBF<-gatherDistFitsm1(totals,"weibull","FSC",unbug=FALSE,modkm=FALSE,ontwth=2)) # 8  seconds
system.time(weibsBG<-gatherDistFitsm1(totals,"weibull","Grp",unbug=FALSE,modkm=FALSE,ontwth=2)) # 5  seconds
system.time(weibsCN<-gatherDistFitsm1(totals,"weibull","NSN",unbug=FALSE,modkm=FALSE,ontwth=3)) # 263 seconds; 181
system.time(weibsCF<-gatherDistFitsm1(totals,"weibull","FSC",unbug=FALSE,modkm=FALSE,ontwth=3)) # 14 seconds
system.time(weibsCG<-gatherDistFitsm1(totals,"weibull","Grp",unbug=FALSE,modkm=FALSE,ontwth=3)) # 7 seconds

# get three weibulls all together
newdf<-CensUncensm1(totals,specCode=1)
hist(newdf[,1]) # all events
hist(newdf[!is.na(newdf[,2]),][,1]) # just failures
hist(newdf[is.na(newdf[,2]),][,1]) # just suspensions
weib<-tryCatch(fitdistcens(newdf,"weibull"),error=function(cond){return(NA)})
oneTotalWeibA<-data.frame("shape"=weib[[1]][1],"scale"=weib[[1]][2],
                          "MeanTime"=weib[[1]][2]*gamma(1+1/weib[[1]][1]),
                          "Events"= sum(!is.na(newdf[,2])), "Censored"=sum(is.na(newdf[,2])))
newdf<-CensUncensm1(totals,specCode=2)
hist(newdf[,1]) # all events
hist(newdf[!is.na(newdf[,2]),][,1]) # just failures
hist(newdf[is.na(newdf[,2]),][,1]) # just suspensions
weib<-tryCatch(fitdistcens(newdf,"weibull"),error=function(cond){return(NA)})
oneTotalWeibB<-data.frame("shape"=weib[[1]][1],"scale"=weib[[1]][2],
                          "MeanTime"=weib[[1]][2]*gamma(1+1/weib[[1]][1]),
                          "Events"= sum(!is.na(newdf[,2])), "Censored"=sum(is.na(newdf[,2])))
newdf<-CensUncensm1(totals,specCode=3)
hist(newdf[,1]) # all events
hist(newdf[!is.na(newdf[,2]),][,1]) # just failures
hist(newdf[is.na(newdf[,2]),][,1]) # just suspensions
weib<-tryCatch(fitdistcens(newdf,"weibull"),error=function(cond){return(NA)})
oneTotalWeibC<-data.frame("shape"=weib[[1]][1],"scale"=weib[[1]][2],
                          "MeanTime"=weib[[1]][2]*gamma(1+1/weib[[1]][1]),
                          "Events"= sum(!is.na(newdf[,2])), "Censored"=sum(is.na(newdf[,2])))



#write.csv(oftenweibsAN,"weibsAN",row.names=F);write.csv(oftenweibsAF,"weibsAF",row.names=F);write.csv(oftenweibsAG,"weibsAG",row.names=F)
#write.csv(oftenweibsBN,"weibsBN",row.names=F);write.csv(oftenweibsBF,"weibsBF",row.names=F);write.csv(oftenweibsBG,"weibsBG",row.names=F)
write.csv(weibsCN,"weibsCN",row.names=F);write.csv(weibsCF,"weibsCF",row.names=F);write.csv(weibsCG,"weibsCG",row.names=F)

weibsAN<-read.csv("weibsAN.csv",stringsAsFactors=FALSE);weibsAF<-read.csv("weibsAF.csv",stringsAsFactors=FALSE);weibsAG<-read.csv("weibsAG.csv",stringsAsFactors=FALSE) # first failure
weibsBN<-read.csv("weibsBN.csv",stringsAsFactors=FALSE);weibsBF<-read.csv("weibsBF.csv",stringsAsFactors=FALSE);weibsBG<-read.csv("weibsBG.csv",stringsAsFactors=FALSE) # second failure
weibsCN<-read.csv("weibsCN.csv",stringsAsFactors=FALSE);weibsCF<-read.csv("weibsCF.csv",stringsAsFactors=FALSE);weibsCG<-read.csv("weibsCG.csv",stringsAsFactors=FALSE) # second-fifth failures

#### for doe
# shape
hist(weibsAN$shape[weibsAN$shape<8]) # NSN 1st removal
anecdf<-ecdf(weibsAN$shape[weibsAN$shape<8])
quantile(anecdf,0.025);quantile(anecdf,0.975)
plot(hist(weibsAN$shape[weibsAN$shape<8]));abline(col="salmon",v=c(quantile(anecdf,0.025),quantile(anecdf,0.975)))
hist(weibsCN$shape[weibsCN$shape<8]) # NSN else-1st removal
anecdf<-ecdf(weibsCN$shape[weibsCN$shape<8])
quantile(anecdf,0.025);quantile(anecdf,0.975)
plot(hist(weibsCN$shape[weibsCN$shape<8]));abline(col="salmon",v=c(quantile(anecdf,0.025),quantile(anecdf,0.975)))

# verificication
head(weibsAN[order(-weibsAN$Events/weibsAN$Censored),])
head(weibsBN[order(-weibsBN$Events/weibsBN$Censored),])
thisNSN2ndfail<-totals[totals$NSN %in% 1015011660265 & !is.na(totals$diff2),"diff2"]
hist(thisNSN2ndfail)
a<-fitdistcens(CensUncensm1(totals[totals$NSN==1015011660265 & !is.na(totals$diff2),],specCode=2),distr="weibull")
plot(a)
hist(weibsAN[weibsAN$shape<=5,]$shape)
hist(weibsBN[weibsBN$shape<=5,]$shape)
hist(weibsCN[weibsCN$shape<=5,]$shape)

hist(weibsAF[weibsAF$shape<=5,]$shape)
hist(weibsBF[weibsBF$shape<=5,]$shape)
hist(weibsCF[weibsCF$shape<=5,]$shape)

hist(weibsAG[weibsAG$shape<=5,]$shape)
hist(weibsBG[weibsBG$shape<=5,]$shape)
hist(weibsCG[weibsCG$shape<=5,]$shape)

# not enough data
totals[totals$NSN %in% 9905013175486 & !is.na(totals$diff2),]
fitdist(totals[totals$NSN %in% 9905013175486 & !is.na(totals$diff2),"diff2"],distr="weibull")
fitdistr(totals[totals$NSN %in% 9905013175486 & !is.na(totals$diff2),"diff2"],densfun="weibull")
fitdistcens(CensUncensm1(totals[totals$NSN %in% 9905013175486 & !is.na(totals$diff2),],2),distr="weibull")


########### whittle the weibulls down to >30 failures (10?)
# define cutoff # of events
ctf<-30
ANgC<-weibsAN[weibsAN$Events>ctf,"NSN"]; length(ANgC)/nrow(weibsAN)
AFgC<-weibsAF[weibsAF$Events>ctf,"FSC"]; length(AFgC)/nrow(weibsAF)
AGgC<-weibsAG[weibsAG$Events>ctf,"Grp"]; length(AGgC)/nrow(weibsAG)
# start with all NSNs, then left join onto it - doesn't work, have to stich together pieces
weibs2use<-data.frame("NSN"=unique(weibsAN$NSN),"FSC"=0,"Grp"=0,
                      "wfm"=factor("Grp",levels=c("NSN","FSC","Grp","All")),stringsAsFactors=FALSE)
weibs2use$FSC<-floor(as.numeric(weibs2use$NSN),-9)/1000000000
weibs2use$Grp<-floor(as.numeric(weibs2use$NSN),-11)/100000000000
# NSNs that exceed cutoff
NSNs2use<-weibs2use[weibs2use$NSN %in% ANgC,"NSN"] # NSNs I want to use
weibs2useNSN<-inner_join(weibs2use,select(weibsAN[weibsAN$NSN %in% NSNs2use,],NSN,shape,scale,MeanTime,Events,Censored),by="NSN")
weibs2useNSN$wfm<-"NSN"
FSCs2use<-AFgC#unique(weibs2use[weibs2use$FSC %in% AFgC,"FSC"]) # FSCs I may want to use
#weibs2useFSC<-inner_join(weibs2use[weibs2use$NSN %notin% weibs2useNSN$NSN,],select(weibsAF[weibsAF$FSC %in% FSCs2use,],FSC,shape,scale,MeanTime,Events,Censored),by="FSC") # this isn't working
weibs2useFSC<-merge(weibs2use[weibs2use$NSN %notin% weibs2useNSN$NSN,],select(weibsAF[weibsAF$FSC %in% FSCs2use,],FSC,shape,scale,MeanTime,Events,Censored),by="FSC")
weibs2useFSC$wfm<-"FSC"
Grps2use<-AGgC
weibs2useGrp<-merge(weibs2use[weibs2use$NSN %notin% weibs2useNSN$NSN & weibs2use$NSN %notin% weibs2useFSC$NSN,]
                    ,select(weibsAG[weibsAG$Grp %in% Grps2use,],Grp,shape,scale,MeanTime,Events,Censored),by="Grp")
weibs2useGrp$wfm<-"Grp"
weibs2useAll<-merge(weibs2use[weibs2use$NSN %notin% weibs2useNSN$NSN & weibs2use$NSN %notin% weibs2useFSC$NSN & 
                                weibs2use$NSN %notin% weibs2useGrp$NSN,],oneTotalWeibA)
weibs2useAll$wfm<-"All"
# bring together
weibs2useT<-rbind(weibs2useNSN,weibs2useFSC,weibs2useGrp,weibs2useAll)
dim(weibs2use);dim(weibs2useT)
weibs2useTA<-weibs2useT # totals for 1st removal - for saving
write.csv(weibs2useTA,"ABCweibuls30failscutoffFirstRem.csv",row.names=FALSE)
# then need to do same for C
CNgC<-weibsCN[weibsCN$Events>ctf,"NSN"]; length(CNgC)/nrow(weibsCN)
CFgC<-weibsCF[weibsCF$Events>ctf,"FSC"]; length(CFgC)/nrow(weibsCF)
CGgC<-weibsCG[weibsCG$Events>ctf,"Grp"]; length(CGgC)/nrow(weibsCG)
# start with all NSNs, then left join onto it - doesn't work, have to stich together pieces
weibs2use<-data.frame("NSN"=unique(weibsCN$NSN),"FSC"=0,"Grp"=0,
                      "wfm"=factor("Grp",levels=c("NSN","FSC","Grp","All")),stringsAsFactors=FALSE)
weibs2use$FSC<-floor(as.numeric(weibs2use$NSN),-9)/1000000000
weibs2use$Grp<-floor(as.numeric(weibs2use$NSN),-11)/100000000000
# NSNs that exceed cutoff
NSNs2use<-weibs2use[weibs2use$NSN %in% CNgC,"NSN"] # NSNs I want to use - there was an error here: NSNs2use<-weibs2use[weibs2use$NSN %in% ANgC,"NSN"] 
weibs2useNSN<-inner_join(weibs2use,select(weibsCN[weibsCN$NSN %in% NSNs2use,],NSN,shape,scale,MeanTime,Events,Censored),by="NSN")
weibs2useNSN$wfm<-"NSN"
FSCs2use<-CFgC#unique(weibs2use[weibs2use$FSC %in% AFgC,"FSC"]) # FSCs I may want to use
#weibs2useFSC<-inner_join(weibs2use[weibs2use$NSN %notin% weibs2useNSN$NSN,],select(weibsAF[weibsAF$FSC %in% FSCs2use,],FSC,shape,scale,MeanTime,Events,Censored),by="FSC") # this isn't working
weibs2useFSC<-merge(weibs2use[weibs2use$NSN %notin% weibs2useNSN$NSN,],select(weibsCF[weibsCF$FSC %in% FSCs2use,],FSC,shape,scale,MeanTime,Events,Censored),by="FSC")
weibs2useFSC$wfm<-"FSC"
Grps2use<-CGgC
weibs2useGrp<-merge(weibs2use[weibs2use$NSN %notin% weibs2useNSN$NSN & weibs2use$NSN %notin% weibs2useFSC$NSN,]
                    ,select(weibsCG[weibsCG$Grp %in% Grps2use,],Grp,shape,scale,MeanTime,Events,Censored),by="Grp")
# there was an error here earlier, previous line: ,select(weibsCG[weibsAG$Grp %in% Grps2use,],Grp,shape,scale,MeanTime,Events,Censored),by="Grp")
weibs2useGrp$wfm<-"Grp"
weibs2useAll<-merge(weibs2use[weibs2use$NSN %notin% weibs2useNSN$NSN & weibs2use$NSN %notin% weibs2useFSC$NSN & 
                                weibs2use$NSN %notin% weibs2useGrp$NSN,],oneTotalWeibC)
weibs2useAll$wfm<-"All"
# bring together
weibs2useT<-rbind(weibs2useNSN,weibs2useFSC,weibs2useGrp,weibs2useAll)
dim(weibs2use);dim(weibs2useT)
weibs2useTC<-weibs2useT # totals for 3rd removal
write.csv(weibs2useTC,"ABCweibuls30failscutoffTwoToFive.csv",row.names=FALSE)
#########
# now the m1a1 parts in model
nsnstofind<-read.csv("m1a1nsnsinmodel0908.csv",col.names="NSN",header=FALSE)
nsnstofind$FSC<-floor(nsnstofind$NSN,-9)/1000000000
nsnstofind$Grp<-floor(nsnstofind$NSN,-11)/100000000000
nsnstofind$InSOE<-"Yes"
nsnstofind$NSN<-as.character(nsnstofind$NSN) # make sure join column is the same structure
weibs2use<-nsnstofind
# bring over everything that matches - A
## A
#weibs2useTA$NSN<-as.character(weibs2useTA$NSN)
nsnstofindAmatch<-inner_join(weibs2use,select(weibs2useTA,NSN,shape,scale,MeanTime,Events,Censored,wfm),by="NSN")
weibs2useNSN<-select(nsnstofindAmatch,NSN)
# with no match, try to match FSC
FSCs2use<-weibsAF[weibsAF$Events>ctf,"FSC"]
weibs2useFSC<-merge(weibs2use[weibs2use$NSN %notin% weibs2useNSN$NSN,],select(weibsAF[weibsAF$FSC %in% FSCs2use,],FSC,shape,scale,MeanTime,Events,Censored),by="FSC")
weibs2useFSC$wfm<-"FSC"
weibs2useFSC$InSOE<-"No"
Grps2use<-weibsAG[weibsAG$Events>ctf,"Grp"]
weibs2useGrp<-merge(weibs2use[weibs2use$NSN %notin% weibs2useNSN$NSN & weibs2use$NSN %notin% weibs2useFSC$NSN,]
                    ,select(weibsAG[weibsAG$Grp %in% Grps2use,],Grp,shape,scale,MeanTime,Events,Censored),by="Grp")
weibs2useGrp$wfm<-"Grp"
weibs2useGrp$InSOE<-"No"
weibs2useAll<-merge(weibs2use[weibs2use$NSN %notin% weibs2useNSN$NSN & weibs2use$NSN %notin% weibs2useFSC$NSN & 
                                weibs2use$NSN %notin% weibs2useGrp$NSN,],oneTotalWeibA)
weibs2useAll$wfm<-"All"
weibs2useAll$InSOE<-"No"
# bring together
weibs2useT<-rbind(nsnstofindAmatch,weibs2useFSC,weibs2useGrp,weibs2useAll)
dim(nsnstofind);dim(weibs2useT)
weibs2useTAmodelNSN<-weibs2useT # totals for 1st removal - for saving
write.csv(weibs2useTAmodelNSN,"modelNSNweibuls30failscutoffFirstRem.csv",row.names=FALSE)
## C
#nsnstofind$NSN<-as.numeric(nsnstofind$NSN)
#weibs2useTC$NSN<-as.character(weibs2useTC$NSN)
weibs2use<-nsnstofind
nsnstofindCmatch<-inner_join(weibs2use,select(weibs2useTC,NSN,shape,scale,MeanTime,Events,Censored,wfm),by="NSN")
FSCs2use<-weibsCF[weibsCF$Events>ctf,"FSC"]
weibs2useFSC<-merge(weibs2use[weibs2use$NSN %notin% weibs2useNSN$NSN,],select(weibsCF[weibsCF$FSC %in% FSCs2use,],FSC,shape,scale,MeanTime,Events,Censored),by="FSC")
weibs2useFSC$wfm<-"FSC"
weibs2useFSC$InSOE<-"No"
Grps2use<-weibsCG[weibsCG$Events>ctf,"Grp"]
weibs2useGrp<-merge(weibs2use[weibs2use$NSN %notin% weibs2useNSN$NSN & weibs2use$NSN %notin% weibs2useFSC$NSN,]
                    ,select(weibsCG[weibsCG$Grp %in% Grps2use,],Grp,shape,scale,MeanTime,Events,Censored),by="Grp")
# previous line was an error 0925 - ,select(weibsAG[weibsAG$Grp %in% Grps2use,],Grp,shape,scale,MeanTime,Events,Censored),by="Grp")
weibs2useGrp$wfm<-"Grp"
weibs2useGrp$InSOE<-"No"
weibs2useAll<-merge(weibs2use[weibs2use$NSN %notin% weibs2useNSN$NSN & weibs2use$NSN %notin% weibs2useFSC$NSN & 
                                weibs2use$NSN %notin% weibs2useGrp$NSN,],oneTotalWeibC)
# previous line was an error 0925 - weibs2use$NSN %notin% weibs2useGrp$NSN,],oneTotalWeibA)
weibs2useAll$wfm<-"All"
weibs2useAll$InSOE<-"No"
# bring together
weibs2useT<-rbind(nsnstofindCmatch,weibs2useFSC,weibs2useGrp,weibs2useAll)
dim(nsnstofind);dim(weibs2useT)
weibs2useTCmodelNSN<-weibs2useT # totals for 1st removal - for saving
write.csv(weibs2useTCmodelNSN,"modelNSNweibuls30failscutoffTwoToFive.csv",row.names=FALSE)
#####################################

# fix the shapes in the model - 09/29
setwd("C:/Users/tbaer/Desktop/m1a1/doe")
myconn<-odbcConnectAccess("C:/Users/tbaer/Desktop/m1a1/doe/m1baselineCorrectShape")
system.time(theurr<-sqlQuery(myconn,"SELECT DISTINCT DOC.NSN, [*Unscheduled Removal rates].* FROM DOC INNER JOIN [*Unscheduled Removal rates] 
ON DOC.[Object Type] = [*Unscheduled Removal rates].[LRU  type]"))
theurr$NSN<-as.character(theurr$NSN)
newurr<-inner_join(theurr,weibs2useTCmodelNSN,"NSN")
newurr[newurr$`Completed Repairs`==1,"Shape"]<-newurr[newurr$`Completed Repairs`==1,"shape"]
repdegprop<-0.20
newurr[newurr$`Completed Repairs`==1,"Rate"]<-(1/(newurr[newurr$`Completed Repairs`==1,"MTTF (thousands of miles)"]*(1-repdegprop)/gamma(1+1/newurr[newurr$`Completed Repairs`==1,"shape"])))
newurr$shape<-NULL #drop shape
system.time(sqlSave(myconn,newurr,rownames=FALSE,varTypes=c(NSN="Number",SerialNumber="Text",FstSttDt="Date",ScdSttDt="Date",
             ThdSttDt="Date",FurSttDt="Date",FvhSttDt="Date",IN.SERVICE="Date",LstSttDt="Date"))) # about 13 minutes
close(myconn)

########## compare drop in MTTF
# NSN
weibsN<-join(select(weibsAN,NSN,scale,shape,MeanTime,Events,Censored),select(weibsBN,NSN,scale,shape,MeanTime,Events,Censored),by="NSN")
weibsN<-join(weibsN,select(weibsCN,NSN,scale,shape,MeanTime,Events,Censored),by="NSN")
colnames(weibsN)<-c("NSN","scale1","shape1","mean1","Events1","Censored1","scale2","shape2","mean2","Events2","Censored2","scale3","shape3","mean3","Events3","Censored3")
weibsNpos<-weibsN[which(!is.na(weibsN$scale1) & !is.na(weibsN$scale2) & !is.na(weibsN$scale3)),]
weibsNg10<-weibsNpos[which(weibsNpos$Events1>=10 & weibsNpos$Events2>10 & weibsNpos$Events3>10),]
mean(weibsNg10$mean1/weibsNg10$mean2);mean(weibsNg10$mean1/weibsNg10$mean3)
#mean(weibsNg10$scale1/weibsNg10$scale2);mean(weibsNg10$scale1/weibsNg10$scale3)
#1-1/mean(weibsNg10$scale1/weibsNg10$scale2);1-1/mean(weibsNg10$scale1/weibsNg10$scale3) # proportion drop from full
weibsNg30<-weibsNpos[which(weibsNpos$Events1>=30 & weibsNpos$Events2>30 & weibsNpos$Events3>30),]
mean(weibsNg30$mean1/weibsNg30$mean2);mean(weibsNg30$mean1/weibsNg30$mean3)
#mean(weibsNg30$scale1/weibsNg30$scale2);mean(weibsNg30$scale1/weibsNg30$scale3)
#1-1/mean(weibsNg30$scale1/weibsNg30$scale2);1-1/mean(weibsNg30$scale1/weibsNg30$scale3)
hist(weibsNg30$scale3/weibsNg30$scale1)
hist(1-(weibsNg30$scale3/weibsNg30$scale1))
hist(weibsNg30$mean1)
hist(weibsNg30$mean3)
weibsNg30<-weibsNpos[which(weibsNpos$Events1>=5 & weibsNpos$Events2>5 & weibsNpos$Events3>5),]
mean(weibsNg30$mean1/weibsNg30$mean2);mean(weibsNg30$mean1/weibsNg30$mean3)
1-1/mean(weibsNg30$scale1/weibsNg30$scale2);1-1/mean(weibsNg30$scale1/weibsNg30$scale3)
va<-2
weibsNg30<-weibsNpos[which(weibsNpos$Events1>=va & weibsNpos$Events3>va),]
mean(weibsNg30$mean1/weibsNg30$mean3)
mean(weibsNg30$mean3/weibsNg30$mean1);median(weibsNg30$mean3/weibsNg30$mean1)
sum(weibsNg30$mean1>weibsNg30$mean3)/length(weibsNg30$mean1)
1-mean(weibsNg30$mean3/weibsNg30$mean1);1-median(weibsNg30$mean3/weibsNg30$mean1)
# 2: 10% decrease, 20: 4% increase, 120: 16% decrease, 200: 24% decrease
hist(weibsNg30$mean3/weibsNg30$mean1,breaks=40);abline(v=0)
plot(weibsNg30$mean3,weibsNg30$mean1,ylab="New MTTF",xlab="Repaired MTTF",xlim=c(0,1e7));abline(0,1)
# FSC
weibsF<-join(select(weibsAF,FSC,scale,MeanTime,Events,Censored),select(weibsBF,FSC,scale,MeanTime,Events,Censored),by="FSC")
weibsF<-join(weibsF,select(weibsCF,FSC,scale,MeanTime,Events,Censored),by="FSC")
colnames(weibsF)<-c("FSC","scale1","mean1","Events1","Censored1","scale2","mean2","Events2","Censored2","scale3","mean3","Events3","Censored3")
weibsFpos<-weibsF[which(!is.na(weibsF$scale1) & !is.na(weibsF$scale2) & !is.na(weibsF$scale3)),]
va<-1
weibsFgva<-weibsFpos[which(weibsFpos$Events1>=va & weibsFpos$Events3>=va),]
1-1/mean(weibsFgva$scale1/weibsFgva$scale2);1-1/mean(weibsFgva$scale1/weibsFgva$scale3)
mean(weibsFgva$mean3/weibsFgva$mean1);median(weibsFgva$mean3/weibsFgva$mean1)
sum(weibsFgva$mean1>weibsFgva$mean3)/length(weibsFgva$mean1);length(weibsFgva$mean1)
1-mean(weibsFgva$mean3/weibsFgva$mean1);1-median(weibsFgva$mean3/weibsFgva$mean1)
par(mfrow=c(2,1))
hist(weibsFgva$mean3/weibsFgva$mean1,breaks=20,xlab="Repaired MTTF Multiple of First",
     main="FSC Codes Left of the Red Lines Have Decreasing MTTF After a Repair");abline(v=1,col="red")
plot(weibsFgva$mean3,weibsFgva$mean1,ylab="New MTTF",xlab="Repaired MTTF");abline(0,1,col="red")
par(mfrow=c(1,1))
weibsFgva[which(weibsFgva$mean3==max(weibsFgva$mean3)),]

# Group
weibsG<-join(select(weibsAG,Grp,scale,Events,Censored),select(weibsBG,Grp,scale,Events,Censored),by="Grp")
weibsG<-join(weibsG,select(weibsCG,Grp,scale,Events,Censored),by="Grp")
colnames(weibsG)<-c("FSC","scale1","Events1","Censored1","scale2","Events2","Censored2","scale3","Events3","Censored3")
weibsGpos<-weibsF[which(!is.na(weibsG$scale1) & !is.na(weibsG$scale2) & !is.na(weibsG$scale3)),]
va<-30
weibsGgva<-weibsGpos[which(weibsGpos$Events1>=va & weibsGpos$Events2>va & weibsGpos$Events3>va),]
1-1/mean(weibsGgva$scale1/weibsGgva$scale2);1-1/mean(weibsGgva$scale1/weibsGgva$scale3)

####
# some more stats
sum(weibsAN$Events)/(sum(weibsAN$Censored)+sum(weibsAN$Events)) # fraction of parts that fail
# average age of platform
mean(as.Date("2014-06-01")-prodYear$IN.SERVICE) /365
####

########## compare drop in shape
weibsNposS<-weibsN[which(!is.na(weibsN$shape1) & !is.na(weibsN$shape2) & !is.na(weibsN$shape3)),]
weibsNg10S<-weibsNposS[which(weibsNposS$Events1>=10 & weibsNposS$Events2>10 & weibsNposS$Events3>10),]
mean(weibsNg10S$shape1/weibsNg10S$shape2);mean(weibsNg10S$shape1/weibsNg10S$shape3)
1-1/mean(weibsNg10S$shape1/weibsNg10S$shape2);1-1/mean(weibsNg10S$shape1/weibsNg10S$shape3) # proportion drop from full

sum(weibsAN$shape<1,na.rm=TRUE)/sum(!is.na(weibsAN$shape)) # 27% of new parts exhibit infant mortality
hist(weibsAN[weibsAN$shape<3,"shape"])
sum(weibsBN$shape<1,na.rm=TRUE)/sum(!is.na(weibsBN$shape))
sum(weibsCN$shape<1,na.rm=TRUE)/sum(!is.na(weibsCN$shape))

# plot
source("C:/Users/tbaer/Desktop/m1a1/randCWwork/m1functions.R")
myconn<-odbcConnectAccess("C:/Users/tbaer/Desktop/m1a1/totalsDB") # 2835015482910 -eng
system.time(totalsSub<-sqlQuery(myconn,"SELECT * FROM totals WHERE NSN = 6110015147369")) # 3.2 seconds #1240015455913 #2530015319542
system.time(leftandright0<-CensUncensm1(totalsSub[totalsSub$NSN %in% 6110015147369,],1)) # 5310009388387;5830013823218
system.time(zweib1<-fitdistcens(leftandright0,distr="weibull")) # sometimes singular - not enough failures
plot(zweib1);zweib1$estimate;gamma(1+1/zweib1$estimate[1])*zweib1$estimate[2]
(fitmedian<-zweib1$estimate[2]*log(2)^(1/zweib1$estimate[1]))
(meanestimate<-mean(rweibull(50000,shape=zweib1$estimate[1],scale=zweib1$estimate[2])))
zexpo1<-fitdistcens(leftandright0,distr="weibull",fix.arg=list(shape=1),start=list(scale=meanestimate))# try expo
sum(leftandright0$left)/sum(!is.na(leftandright0$right)) # soedst-style mttf - total time on test over total failures
# be sure to adjust x and y axis limits
firsthist<-hist(leftandright0[!is.na(leftandright0$right),"left"])
hist(leftandright0[!is.na(leftandright0$right),"left"],breaks=seq(from=0,to=3000,by=250),main="New Part Time to Failure",xlab="Days",col="royalblue",ylim=c(0,25)) # 600x350
mean(leftandright0[!is.na(leftandright0$right),"left"]) # mean time to fail (only failures)
median(leftandright0[!is.na(leftandright0$right),"left"])# median time to fail (only failures)

system.time(leftandright1<-CensUncensm1(totalsSub[totalsSub$NSN %in% 6110015147369,],3))
system.time(zweib2<-fitdistcens(leftandright1,distr="weibull"))
plot(zweib2);zweib2$estimate;gamma(1+1/zweib2$estimate[1])*zweib2$estimate[2]
(fitmedian<-zweib2$estimate[2]*log(2)^(1/zweib2$estimate[1]))
zexpo2<-fitdistcens(leftandright1,distr="weibull",fix.arg=list(shape=1),start=list(scale=meanestimate))# try expo
sum(leftandright1$left)/sum(!is.na(leftandright1$right)) # soedst-style mttf - total time on test over total failures
# be sure to adjust the x and y axis limits
secondhist<-hist(leftandright1[!is.na(leftandright1$right),"left"])
hist(leftandright1[!is.na(leftandright1$right),"left"],main="Repaired Part Time to Failure",xlab="Days",col="royalblue",xlim=c(0,3000),breaks=seq(from=0,to=4000,by=250),ylim=c(0,25)) 
mean(leftandright1[!is.na(leftandright1$right),"left"]) # mean time to fail (only failures)
median(leftandright1[!is.na(leftandright1$right),"left"])# median time to fail (only failures)

#cdf
plot(ecdf(rweibull(10000,zweib1$estimate[1],zweib1$estimate[2])))
lines(ecdf(rweibull(10000,zweib2$estimate[1],zweib2$estimate[2])),col="blue")
#pdf
hist(rweibull(10000,zweib1$estimate[1],zweib1$estimate[2]),probability=TRUE)
plot(density(rweibull(10000,zweib2$estimate[1],zweib2$estimate[2])),xlim=c(0,40000),main="Failure Probability Is Greater for\nRepaired Part Until",
     xlab="Days")
lines(density(rweibull(10000,zweib1$estimate[1],zweib1$estimate[2])),col="blue")
legend("topright",c("New Parts","Repaired Parts"),fill=c("black","blue"))


24204/447*4 # miles per year 
24204/447*4/365 # miles per day
# one example pdf of wear out to infant mortality

### hazard function
# S = 1 - F
# h = f/S = f/(1-F)
fortheX<-seq(1,30000,100)
S1 <- 1 - pweibull(fortheX,zweib1$estimate[1],zweib1$estimate[2])
h1 <- dweibull(fortheX,zweib1$estimate[1],zweib1$estimate[2])/S1
S2 <- 1 - pweibull(fortheX,zweib2$estimate[1],zweib2$estimate[2])
h2 <- dweibull(fortheX,zweib2$estimate[1],zweib2$estimate[2])/S2
plot(fortheX,h1,type="l",xlab="Days",ylab="Hazard Rate",main="Failure Rate",ylim=c(0,.0007));lines(fortheX,h2,col="royalblue")
legend("top",c("New Part","Repaired Part"),fill=c("black","royalblue"))

### wtf confint
# 50/50 mixture
set.seed(101)
n<-2000000
dist1<-rnorm(n,2,2)
dist2<-rnorm(n,4,3)
#mean
(totMu<-mean(dist1)/2+mean(dist2)/2)
#variance
(totVar<-sum((c(dist1,dist2)-totMu)^2))/(2*n) # 7.5
#variance estimate
(2^2/2+3^2/2) + (2-3)^2/2 + (4-3)^2/2 # 7.5
#linear comb var
(2^2+3^2)/2

# with a few more
dist3<-rnorm(n,2.1,2)
dist4<-rnorm(n,3.8,2.9)
#mean
(totMu<-mean(dist1)/4+mean(dist2)/4+mean(dist3)/4+mean(dist4)/4)
#variance
(totVar<-sum((c(dist1,dist2,dist3,dist4)-totMu)^2)/(4*n)) # 7.21
#variance estimate
(var(dist1)/4+var(dist2)/4+var(dist3)/4+var(dist4)/4 + 
   (mean(dist1)-totMu)^2/4 + (mean(dist2)-totMu)^2/4 + (mean(dist3)-totMu)^2/4 + (mean(dist4)-totMu)^2/4)  # 7.21
#linear comb var
(var(dist1)/4+var(dist2)/4+var(dist3)/4+var(dist4)/4)

## try a different method
set.seed(101)
n<-50000 # like number of histories
n1<-10;n2<-15;n3<-20;n4<-5
dist1<-rnorm(n*n1,.8,0.05) # where 10 is number of platforms deployed
dist2<-rnorm(n*n2,.7,0.07)
dist3<-rnorm(n*n3,.9,0.03)
dist4<-rnorm(n*n4,.6,0.11)
totalD<-n1+n2+n3+n4
(totMu<-mean(dist1)*n1/totalD+mean(dist2)*n2/totalD+mean(dist3)*n3/totalD+mean(dist4)*n4/totalD) # 0.79
(totVar<-sum((c(dist1,dist2,dist3,dist4)-totMu)^2)/(totalD*n)) # 0.0144, empirical variance
# variance estimate:  
# assuming mixture of normal theory : #Sum([mixprob]*[var]+[mixprob]*([availability]-[expavail])^2) AS rowvarmix # then maybe average this (/4)
sum(n1/totalD*var(dist1)+n1/totalD*(mean(dist1)-totMu)^2+n2/totalD*var(dist2)+n2/totalD*(mean(dist2)-totMu)^2+
      n3/totalD*var(dist3)+n3/totalD*(mean(dist3)-totMu)^2+n4/totalD*var(dist4)+n4/totalD*(mean(dist4)-totMu)^2) # 0.0144, 0.12 % avail

# assuming simple weighted average
(totalVarWt<-var(dist1)*n1/totalD+var(dist2)*n2/totalD+var(dist3)*n3/totalD+var(dist4)*n4/totalD) # 0.135 or
(totalVarWt<-var(dist1)*(n1/totalD)^2+var(dist2)*(n2/totalD)^2+var(dist3)*(n3/totalD)^2+var(dist4)*(n4/totalD)^2) # .0455

# cool hist
hist(c(dist1,dist2,dist3,dist4),col="grey")
hist(dist2,add=T,col="orange")
hist(dist1,add=T,col="blue")
hist(dist3,add=T,col="yellow")
hist(dist4,add=T,col="red")
abline(v=totMu,col="purple",lwd=3)
#avgMean<-mean(rnorm(1))

## try with real example
set.seed(101)
n<-50 # number of histories - wk 519
n1<-142;n2<-98;n3<-328;n4<-65;n5<-111;n6<-59;n7<-18
dist1<-rnorm(n*n1,0.9358265,2.18359422683716E-02) # where n1 is number of platforms deployed
dist2<-rnorm(n*n2,0.9001246,2.82451796531677E-02)
dist3<-rnorm(n*n3,0.9030827,1.50580132007599E-02)
dist4<-rnorm(n*n4,0.9725072,2.09958839416504E-02)
dist5<-rnorm(n*n5,0.8932196,2.93550491333008E-02)
dist6<-rnorm(n*n6,0.9136784,0.036519193649292)
dist7<-rnorm(n*n7,0.8910694,7.13001203536987E-02)
(totalD<-n1+n2+n3+n4+n5+n6+n7)
(totMu<-mean(dist1)*n1/totalD+mean(dist2)*n2/totalD+mean(dist3)*n3/totalD+mean(dist4)*n4/totalD+
   mean(dist5)*n5/totalD+mean(dist6)*n6/totalD+mean(dist7)*n7/totalD) # 0.913
(totVar<-sum((c(dist1,dist2,dist3,dist4,dist5,dist6,dist7)-totMu)^2)/(totalD*n)) # 0.001112, empirical variance
# variance estimate:  
# assuming mixture of normal theory : #Sum([mixprob]*[var]+[mixprob]*([availability]-[expavail])^2) AS rowvarmix # then maybe average this (/4)
sum(n1/totalD*var(dist1)+n1/totalD*(mean(dist1)-totMu)^2+n2/totalD*var(dist2)+n2/totalD*(mean(dist2)-totMu)^2+
      n3/totalD*var(dist3)+n3/totalD*(mean(dist3)-totMu)^2+n4/totalD*var(dist4)+n4/totalD*(mean(dist4)-totMu)^2+
      n5/totalD*var(dist5)+n5/totalD*(mean(dist5)-totMu)^2+n6/totalD*var(dist6)+n6/totalD*(mean(dist6)-totMu)^2+
      n7/totalD*var(dist7)+n7/totalD*(mean(dist7)-totMu)^2) # 0.001112 var 0.1% avail
# but this isn't the variance I'm after
# cool hist
hist(c(dist1,dist2,dist3,dist4,dist5,dist6,dist7),col="grey")
empd<-ecdf(c(dist1,dist2,dist3,dist4,dist5,dist6,dist7))
abline(lwd=3,v=quantile(empd,c(0.025,0.975)))
hist(dist2,add=T,col="orange")
hist(dist1,add=T,col="blue")
hist(dist3,add=T,col="yellow")
hist(dist4,add=T,col="red")
hist(dist5,add=T,col="pink")
hist(dist6,add=T,col="green")
hist(dist7,add=T,col="salmon")
abline(v=totMu,col="purple",lwd=3)

#timetime<-data.frame("n"=c(10,100,500,1000,5000,10000,50000),"time"=0)
set.seed(102)
n=10000;
allhist<-data.frame("hist"=seq(1:n),"avail"=0,"var"=0)
#timetime[7,2]<-
mu1<-0.9358265;sd1<-2.18359422683716E-02;mu2<-0.9001246;sd2<-2.82451796531677E-02
mu3<-0.9030827;sd3<-1.50580132007599E-02;mu4<-0.9725072;sd4<-2.09958839416504E-02
mu5<-0.8932196;sd5<-2.93550491333008E-02;mu6<-0.9136784;sd6<-0.036519193649292
mu7<-0.8910694;sd7<-7.13001203536987E-02
system.time(for (ii in seq(1:n)){
  n<-1
  dist1<-rnorm(n*n1,mu1,sd1) # where n1 is number of platforms deployed
  dist2<-rnorm(n*n2,mu2,sd2)
  dist3<-rnorm(n*n3,mu3,sd3)
  dist4<-rnorm(n*n4,mu4,sd4)
  dist5<-rnorm(n*n5,mu5,sd5)
  dist6<-rnorm(n*n6,mu6,sd6)
  dist7<-rnorm(n*n7,mu7,sd7)
  allhist[ii,2]<-mean(c(dist1,dist2,dist3,dist4,dist5,dist6,dist7)) # stores average availability of 1 history
  allhist[ii,3]<-var(c(dist1,dist2,dist3,dist4,dist5,dist6,dist7))
})
#[1]
tenkhist<-allhist # tenk took 768 seconds, 13 minutes
#fiftykhist<-allhist # took 3781 seconds, 63 minutes 
hist(fiftykhist[,2],col=rgb(0,1,1,1/3));hist(tenkhist[,2],add=T,col=rgb(1,0,0,1/3))
abline(v=mean(fiftykhist[,2]),col=rgb(0,1,1));mean(fiftykhist[,2])
abline(v=mean(tenkhist[,2]),col=rgb(1,0,0));mean(tenkhist[,2])
var(fiftykhist[,2]);var(tenkhist[,2]) # var of mean of histories is used for prediction interval
sqrt(var(fiftykhist[,2]));sqrt(var(tenkhist[,2])) # used for prediction interval

sd(fiftykhist[,2])*qt(0.975,49999)*sqrt(1+1/50000);sd(tenkhist[,2])*qt(0.975,9999)*sqrt(1+1/10000) # for predints widths
sd(fiftykhist[,2])*qt(0.975,49999)/sqrt(50000);sd(tenkhist[,2])*qt(0.975,9999)/sqrt(10000) # for confint widths
var(fiftykhist[,2])/50000^2;var(tenkhist[,2])/10000^2 # small, but probably correct

#
mean(allhist[,2]) # history average
var(allhist[,2]) # variance of averages (variance across histories)
mean(allhist[,3]) # history average variance (average of within histories)

# calculate variance empirically
# first try using equal mixing probabilities
totalDist<-7 # number of distributions put together
totalDepl<-n1+n2+n3+n4+n5+n6+n7
(totMu<-mean(allhist[,2]))
# treat each x equally
sum(1/totalDist*(sd1)^2+1/totalDist*(mu1-totMu)^2+1/totalDist*(sd2)^2+1/totalDist*(mu2-totMu)^2+
      1/totalDist*(sd3)^2+1/totalDist*(mu3-totMu)^2+1/totalDist*(sd4)^2+1/totalDist*(mu4-totMu)^2+
      1/totalDist*(sd5)^2+1/totalDist*(mu5-totMu)^2+1/totalDist*(sd6)^2+1/totalDist*(mu6-totMu)^2+
      1/totalDist*(sd7)^2+1/totalDist*(mu7-totMu)^2) # ?
sum(1/totalD*(sd1)^2+1/totalDist*(mu1-totMu)^2+1/totalDist*(sd2)^2+1/totalDist*(mu2-totMu)^2+
      1/totalD*(sd3)^2+1/totalDist*(mu3-totMu)^2+1/totalDist*(sd4)^2+1/totalDist*(mu4-totMu)^2+
      1/totalD*(sd5)^2+1/totalDist*(mu5-totMu)^2+1/totalDist*(sd6)^2+1/totalDist*(mu6-totMu)^2+
      1/totalD*(sd7)^2+1/totalDist*(mu7-totMu)^2)/totalDist^2 # ?
# use deployed to weight bases/OT x's
sum(n1/totalDepl*(sd1)^2+n1/totalDepl*(mu1-totMu)^2+n2/totalDepl*(sd2)^2+n2/totalDepl*(mu2-totMu)^2+
      n3/totalDepl*(sd3)^2+n3/totalDepl*(mu3-totMu)^2+n4/totalDepl*(sd4)^2+n4/totalDepl*(mu4-totMu)^2+
      n5/totalDepl*(sd5)^2+n5/totalDepl*(mu5-totMu)^2+n6/totalDepl*(sd6)^2+n6/totalDepl*(mu6-totMu)^2+
      n7/totalDepl*(sd7)^2+n7/totalDepl*(mu7-totMu)^2) # ?
sum(n1/totalDepl*(sd1)^2+n1/totalDepl*(mu1-totMu)^2+n2/totalDepl*(sd2)^2+n2/totalDepl*(mu2-totMu)^2+
      n3/totalDepl*(sd3)^2+n3/totalDepl*(mu3-totMu)^2+n4/totalDepl*(sd4)^2+n4/totalDepl*(mu4-totMu)^2+
      n5/totalDepl*(sd5)^2+n5/totalDepl*(mu5-totMu)^2+n6/totalDepl*(sd6)^2+n6/totalDepl*(mu6-totMu)^2+
      n7/totalDepl*(sd7)^2+n7/totalDepl*(mu7-totMu)^2)/totalDepl^2 # ?
sum(n1/totalDepl*(sd1)^2+n1/totalDepl*(mu1-totMu)^2+n2/totalDepl*(sd2)^2+n2/totalDepl*(mu2-totMu)^2+
      n3/totalDepl*(sd3)^2+n3/totalDepl*(mu3-totMu)^2+n4/totalDepl*(sd4)^2+n4/totalDepl*(mu4-totMu)^2+
      n5/totalDepl*(sd5)^2+n5/totalDepl*(mu5-totMu)^2+n6/totalDepl*(sd6)^2+n6/totalDepl*(mu6-totMu)^2+
      n7/totalDepl*(sd7)^2+n7/totalDepl*(mu7-totMu)^2)/totalDist^2 # ?


# below using original mixing probabilities
totalD<-n1+n2+n3+n4+n5+n6+n7
# another way to calculate totMu
(totMu<-0.9358265*n1/totalD+0.9001246*n2/totalD+0.9030827*n3/totalD+0.9725072*n4/totalD+
   0.8932196*n5/totalD+0.9136784*n6/totalD+0.8910694*n7/totalD) # 0.913
sum(n1/totalD*sqrt(2.18359422683716E-02)+n1/totalD*(0.9358265-totMu)^2+n2/totalD*sqrt(2.82451796531677E-02)+n2/totalD*(0.9001246-totMu)^2+
      n3/totalD*sqrt(1.50580132007599E-02)+n3/totalD*(0.9030827-totMu)^2+n4/totalD*sqrt(2.09958839416504E-02)+n4/totalD*(0.9725072-totMu)^2+
      n5/totalD*sqrt(2.93550491333008E-02)+n5/totalD*(0.8932196-totMu)^2+n6/totalD*sqrt(0.036519193649292)+n6/totalD*(0.9136784-totMu)^2+
      n7/totalD*sqrt(7.13001203536987E-02)+n7/totalD*(0.8910694-totMu)^2) # ?
sum(n1/totalD*sqrt(2.18359422683716E-02)+n1/totalD*(0.9358265-totMu)^2+n2/totalD*sqrt(2.82451796531677E-02)+n2/totalD*(0.9001246-totMu)^2+
      n3/totalD*sqrt(1.50580132007599E-02)+n3/totalD*(0.9030827-totMu)^2+n4/totalD*sqrt(2.09958839416504E-02)+n4/totalD*(0.9725072-totMu)^2+
      n5/totalD*sqrt(2.93550491333008E-02)+n5/totalD*(0.8932196-totMu)^2+n6/totalD*sqrt(0.036519193649292)+n6/totalD*(0.9136784-totMu)^2+
      n7/totalD*sqrt(7.13001203536987E-02)+n7/totalD*(0.8910694-totMu)^2)/7^2 # ?
sum(n1/totalD*sqrt(2.18359422683716E-02)+n1/totalD*(0.9358265-totMu)^2+n2/totalD*sqrt(2.82451796531677E-02)+n2/totalD*(0.9001246-totMu)^2+
      n3/totalD*sqrt(1.50580132007599E-02)+n3/totalD*(0.9030827-totMu)^2+n4/totalD*sqrt(2.09958839416504E-02)+n4/totalD*(0.9725072-totMu)^2+
      n5/totalD*sqrt(2.93550491333008E-02)+n5/totalD*(0.8932196-totMu)^2+n6/totalD*sqrt(0.036519193649292)+n6/totalD*(0.9136784-totMu)^2+
      n7/totalD*sqrt(7.13001203536987E-02)+n7/totalD*(0.8910694-totMu)^2)/(totalD)^2 # ?
sum(n1/totalD*sqrt(2.18359422683716E-02)+n1/totalD*(0.9358265-totMu)^2+n2/totalD*sqrt(2.82451796531677E-02)+n2/totalD*(0.9001246-totMu)^2+
      n3/totalD*sqrt(1.50580132007599E-02)+n3/totalD*(0.9030827-totMu)^2+n4/totalD*sqrt(2.09958839416504E-02)+n4/totalD*(0.9725072-totMu)^2+
      n5/totalD*sqrt(2.93550491333008E-02)+n5/totalD*(0.8932196-totMu)^2+n6/totalD*sqrt(0.036519193649292)+n6/totalD*(0.9136784-totMu)^2+
      n7/totalD*sqrt(7.13001203536987E-02)+n7/totalD*(0.8910694-totMu)^2)/(50000)^2 # not relevant b/c I'm still working on predint

## weighted average of confidence & prediction bounds; work backwards to get variance

# could try treating each individual HMMWV as a part of the mixture



#######
# lead times
rawEros$LeadTime<-as.numeric(rawEros$Received-rawEros$Ordered)
df<-rawEros[rawEros$NSN %in% 5120011151142,]
fitdistr(rawEros[rawEros$NSN %in% 5120011151142,"LeadTime"],densfun="lognormal")
# take out Cancelled ones? no, they're already NA received
# switch 0 days to 0.1
erosForLT<-rawEros
erosForLT<-erosForLT[!is.na(erosForLT$LeadTime),] # take out NAs
erosForLT[erosForLT$LeadTime<0,"LeadTime"]<-NA
erosForLT<-erosForLT[!is.na(erosForLT$LeadTime),] # take out NAs
erosForLT[erosForLT$LeadTime==0,"LeadTime"]<-as.numeric(0.1)
# exclude negatives
# plot a few
par(mar=c(4,4,1,1))
par(mfcol=c(1,2))
h<-hist(as.numeric(erosForLT[erosForLT$FSC %in% 1095,"LeadTime"]),probability=TRUE,
        xlab="Lead Time (days)",ylab="Probability Density",main="Lead Time for FSC 1095 - Weapons",ylim=c(0,0.09)) # fairly normal fsc, weapons
oneShip<-fitdist(as.numeric(erosForLT[erosForLT$FSC %in% 1095,"LeadTime"]),distr="lnorm")
abline(v=exp(oneShip$estimate[1]),col="orange",lwd=2)
text(16,0.06,paste0("median = ",round(exp(oneShip$estimate[1]),1)),col="orange")#;text(16,0.065,"mdn",col="orange")
abline(v=exp(oneShip$estimate[1]+oneShip$estimate[2]^2/2),col="blue",lwd=2)
text(16,0.07,paste0("mean = ",round(exp(oneShip$estimate[1]+oneShip$estimate[2]^2/2),1)),col="blue")#;text(16,0.075,"mean",col="blue")
text(16,0.08,paste0("orders = ",length(erosForLT[erosForLT$FSC %in% 1095,"LeadTime"])))
lines(density(rlnorm(100000,oneShip$estimate[1],oneShip$estimate[2])),col="red",lwd=2,lty=2)
#xfit<-seq(from=min(as.numeric(erosForLT[erosForLT$FSC %in% 1095,"LeadTime"])),
          #to=max(as.numeric(erosForLT[erosForLT$FSC %in% 1095,"LeadTime"])),length=40)
#yfit<-rlnorm(xfit,oneShip$estimate[1],oneShip$estimate[2])
#yfit<-yfit*diff(h$mids[1:2])*length(as.numeric(erosForLT[erosForLT$FSC %in% 1095,"LeadTime"]))
#lines(xfit,yfit)

# long outliers, circuit breakers
h<-hist(as.numeric(erosForLT[erosForLT$FSC %in% 5925,"LeadTime"]),probability=TRUE,breaks=20,
        xlab="Lead Time (days)",ylab="Probability Density",main="Long-tailed Lead Time for FSC 5925 - Circuit Breakers",ylim=c(0,0.09)) # fairly normal fsc, weapons
oneShip<-fitdist(as.numeric(erosForLT[erosForLT$FSC %in% 5925,"LeadTime"]),distr="lnorm")
abline(v=exp(oneShip$estimate[1]),col="orange",lwd=2)
text(40,0.06,paste0("median = ",round(exp(oneShip$estimate[1]),1)),col="orange");#text(40,0.07,"mdn",col="orange")
abline(v=exp(oneShip$estimate[1]+oneShip$estimate[2]^2/2),col="blue",lwd=2)
text(40,0.07,paste0("mean = ",round(exp(oneShip$estimate[1]+oneShip$estimate[2]^2/2),1)),col="blue")#;text(40,0.09,"mean",col="blue")
text(40,0.08,paste0("orders = ",length(erosForLT[erosForLT$FSC %in% 5925,"LeadTime"])))
lines(density(rlnorm(100000,oneShip$estimate[1],oneShip$estimate[2])),col="red",lwd=2,lty=2)

## nsns
h<-hist(as.numeric(erosForLT[erosForLT$NSN %in% 4730012965759,"LeadTime"]),xlim=c(0,100),breaks=70,probability=TRUE)
oneShip<-fitdist(as.numeric(erosForLT[erosForLT$NSN %in% 4730012965759,"LeadTime"]),distr="lnorm")
lines(density(rlnorm(100000,oneShip$estimate[1],oneShip$estimate[2])),col="red",lwd=2,lty=2)
theM1ShipTimesAllYears[theM1ShipTimesAllYears$NSN %in% "4730012965759",]

# some statistics
hist(as.numeric(erosForLT$LeadTime),xlim=c(0,365),breaks=280)
sum(as.numeric(erosForLT$LeadTime==0.1))/length(as.numeric(erosForLT$LeadTime))# 4.8% of lead times are 0
sum(as.numeric(erosForLT$LeadTime>365))/length(as.numeric(erosForLT$LeadTime)) # 0.3$ of lead times over a year
sum(as.numeric(erosForLT$LeadTime>(365/4)))/length(as.numeric(erosForLT$LeadTime)) # 2.6% of lead times over a quarter
with(theM1ShipTimesAllYears[theM1ShipTimesAllYears$Events>1,],
     sum(ActMeanTime>ActMedianTime)/length(Events))
head(theM1ShipTimesAllYears[theM1ShipTimesAllYears$ActMeanTime<theM1ShipTimesAllYears$ActMedianTime,]) # median is higher
with(erosForLT[erosForLT$FSC %in% 5925,],sum(as.numeric(LeadTime>(365/4)))/length(as.numeric(LeadTime))) # 2.1% of lead times over a quarter
with(erosForLT[erosForLT$FSC %in% 5925,],sum(as.numeric(LeadTime>(365/12)))/length(as.numeric(LeadTime))) # 10.4% above a month
with(erosForLT[erosForLT$FSC %in% 1095,],sum(as.numeric(LeadTime>(365/12)))/length(as.numeric(LeadTime))) # 10.4% above a month


source("C:/Users/tbaer/Desktop/m1a1/randCWwork/m1functions.R")
system.time(theM1ShipTimes<-gatherDistFitsm1(rawEros,distrib="lognormal",classTypes=c("NSN","Apps")))
# getting error, try w/o apps?
subsetoferos<-erosForLT[1:20000,]
system.time(theM1ShipTimes<-gatherDistFitsm1(subsetoferos,distrib="lognormal",unbug=FALSE))#classTypes=c("NSN","Apps")))


fitanlt<-function(df){
  leadtimes<-df[!is.na(df$LeadTime),"LeadTime"]
  oneleadtime<-fitdistr(as.numeric(leadtimes),densfun="lognormal")
  out<-cbind(t(data.frame(oneleadtime[1])),t(data.frame(oneleadtime[2])))
  out<-as.data.frame(out)
  out[1,5]<-exp(oneleadtime[[1]][1]) # median time b/w order and receive
  out[1,6]<-exp(oneleadtime[[1]][1]+(oneleadtime[[1]][2])^2/2) # mean
  out[1,7]<-median(leadtimes)
  out[1,8]<-mean(leadtimes)
  out[1,9]<-oneleadtime$n # events
  colnames(out)<-c("meanlog","sdlog","meanlogSE","sdlogSE","CalcMedianTime","CalcMeanTime",
                   "ActMedianTime","ActMeanTime","Events") 
  (out)
}

system.time(theM1ShipTimesSplitByAppsAllYears<-ddply(erosForLT,c("NSN","Apps"),fitanlt))
system.time(theM1ShipTimesAllYears<-ddply(erosForLT,c("NSN"),fitanlt))

#write.csv(theM1ShipTimesSplitByAppsAllYears,"theM1ShipTimesSplitByAppsAllYears.csv",row.names=FALSE)
#write.csv(theM1ShipTimesAllYears,"theM1ShipTimesAllYears.csv",row.names=FALSE)
theM1ShipTimesAllYears<-read.csv("theM1ShipTimesAllYears.csv")


abc<-arrange(summarise(group_by(rawEros,NSN,Apps),n()),NSN)
def<-arrange(summarise(group_by(abc,NSN,n())),NSN)
sum(def[,2]>1);def[which(def[,2]>1),]

# data just in FY12 and beyond
erosForLTFY1214<-erosForLT[erosForLT$End>'2011-10-01',];dim(erosForLTFY1214)
system.time(theM1ShipTimesSplitByAppsFY1214<-ddply(erosForLTFY1214,c("NSN","Apps"),fitanlt))
system.time(theM1ShipTimesFY1214<-ddply(erosForLTFY1214,c("NSN"),fitanlt))
write.csv(theM1ShipTimesSplitByAppsFY1214,"theM1ShipTimesSplitByAppsFY1214.csv",row.names=FALSE)
write.csv(theM1ShipTimesFY1214,"theM1ShipTimesFY1214.csv",row.names=FALSE)
median(erosForLTFY1214$LeadTime,na.rm=TRUE);mean(erosForLTFY1214$LeadTime,na.rm=TRUE)
allLT<-fitdist(erosForLTFY1214$LeadTime,distr="lnorm")
exp(allLT[[1]][1]);exp(allLT[[1]][1]+allLT[[1]][1]^2/2) # median/mean
exp(allLT[[1]][1]+0.009745865);exp(allLT[[1]][1]-0.009745865) # +/1 1 SE
exp(allLT[[1]][1]+allLT[[1]][1]/4);exp(allLT[[1]][1]-allLT[[1]][1]/4)
exp(allLT[[1]][1]+allLT[[1]][1]/4+allLT[[1]][1]^2/2);exp(allLT[[1]][1]-allLT[[1]][1]/4+allLT[[1]][1]^2/2)

###########################################################################
## DOE etc.
#to get the correct factor to adjust lognormal shipping times to get +50/-50% of mean and median 
#                     (add log(fraction) to meanlog, where fraction is fraction of 100% current)
meanlog<-2.302585
sdlog<-.7
(mdn<-exp(meanlog))
(mn<-exp(meanlog+sdlog^2/2))
(mdn<-exp(meanlog-meanlog/4));(mdn<-exp(meanlog+meanlog/4))
a<-0.5; meanlog/(log(mn*a)-sdlog^2/2)
a<-2; meanlog/(log(mn*a)-sdlog^2/2)
a<-seq(from=0.5,to=1.5,by=0.1)
b<-meanlog/(log(mn/a)-sdlog^2/2)
plot(a,b)
c<-meanlog/(log(mn/a))
(exp(max(c)*meanlog+sdlog^2/2));(exp(meanlog+sdlog^2/2));(exp(min(c)*meanlog+sdlog^2/2))
(exp(meanlog/max(c)+sdlog^2/2));(exp(meanlog+sdlog^2/2));(meanlog/exp(min(c)+sdlog^2/2))


meanlog<-2.6
sdlog<-.9
(mdn<-exp(meanlog))
(mn<-exp(meanlog+sdlog^2/2))
b<-meanlog/(log(mn/a)-sdlog^2/2)
c<-meanlog/(log(mn/a))
a<-c(0.5,1.5)
y<-meanlog/log(1/a)
exp(meanlog*y)

abc<-seq(from=log(1/2),to=log(2),length.out=17)
plot(abc,exp(abc))


## initialize the OAI 09/23
# read data
dataCN<-odbcConnectAccess2007(paste0(getwd(),"/doe/m1baselineOAIInitialized.mdb")) #  100101001
system.time(oai<-sqlQuery(dataCN, "SELECT * FROM [*Object Attributes Initial] WHERE [Object Type]=100101001"))
print(object.size(oai),units="MB")
system.time(oai<-sqlQuery(dataCN, "SELECT * FROM [*Object Attributes Initial]")) # 34 seconds
print(object.size(oai),units="MB")
# update the probabalistic age
system.time(oai$`Probabilistic age`<-runif(nrow(oai)))
# output result
system.time(sqlSave(dataCN,oai,rownames=FALSE)) # 287 seconds  varTypes=c(NSN="Number",SerialNumber="Text",FstSttDt="Date",ScdSttDt="Date",
# remove connection
close(dataCN)


# hist of current ages
hist(oai[oai$Objecttype!=100101001,"AFHRv"])
# fleet goes 24204 miles per quarter - 447 platforms
24204/447*4 # platform average miles per year - 216
24204/447*4*10 # platform averages before a rebuild - 2166
sum(oai$CompletedRepairs>0)/nrow(oai) # 10% of parts with 1 CR

##### 09/23:
## 1: what is the average age of a part following a weibull distribution in steady state relative to its mttf? expo, shape
set.seed(099)
mttf<-100
shape1<-1.5 # characteristic life should be higher than mttf
shape2<-0.75 # characteristic life should be lower than mttf
scale1<-mttf/gamma(1+1/shape1)
scale2<-mttf/gamma(1+1/shape2)
fails11<-(rweibull(100000,shape1,scale1)) # time @ first failure
fails12<-(rweibull(10000,shape1,scale1)+fails11) # time @ second failure
fails13<-(rweibull(10000,shape1,scale1)+fails12) # time @ third failure
fails14<-(rweibull(10000,shape1,scale1)+fails13) # time @ fourth failure
df1<-cbind(fails11,fails12,fails13,fails14)
quantile(ecdf(fails11),0.5) # median
hist(fails11);abline(v=quantile(ecdf(fails11),0.5),col="orange");abline(v=mttf,col="blue")
fails21<-rweibull(10000,shape2,scale2)
hist(fails21);abline(v=median(fails21),col="orange");abline(v=mttf,col="blue")
fails31<-rweibull(10000,1,mttf)
hist(fails31);abline(v=median(fails31),col="orange");abline(v=mttf,col="blue")
plot(c(shape1,shape2,1,1.25),c(median(fails11),median(fails21),median(fails31),median(rweibull(10000,1.25,mttf/gamma(1+1/1.25)))),ylab="median")

## 2: is it okay to calculate mttf as total time for all parts on test over number of failures (treating all parts as the same part) or 
     #  should I count this as suspensions and a few failures?
lr<-data.frame("left"=c(5,3,6,6,5,2,2,8,7,2,2,1,4),"right"=c(5,3,6,NA,NA,NA,2,8,7,2,NA,1,4))
dist1<-fitdistcens(lr,distr="weibull")
plot(dist1)
sum(lr$left/sum(!is.na(lr$right))) # mttf one way - total time on test / total failures
dist1$estimate[2]*gamma(1+1/dist1$estimate[1]) # mttf a second way
# but this is a bad way to do it b/c my weibull is only an approximation
# should really do it with real data( as sampled above )
left<-fails11;sum(left)
right<-fails11
censFrac<-0.5# fraction censored
right[head(order(runif(length(right))),length(fails11)*censFrac)]<-NA # assign 10% as censored
left[which(is.na(right))]<-runif(sum(is.na(right)),0,left[which(is.na(right))]);sum(left) # assign random censoring time to those 10%
weibEst<-fitdistcens(data.frame(left,right),distr="weibull")
weibEst$estimate[2]*gamma(1+1/weibEst$estimate[1]) # mean estimate - overestimate, maybe a better way to assign random censor times
weibEst
sum(left)/sum(!is.na(right))# other mean estimate
# now see what happens if mttf is 100 days but I only ever see 25 days
sum(fails11<25)/length(fails11)
f10fails11<-fails11
f10fails11[f10fails11>25]<-25
rightEyCn<-f10fails11;rightEyCn<-rightEyCn[rightEyCn==25]<-NA
zweibEyCn<-fitdistcens(data.frame("left"=f10fails11,"right"=rightEyCn),distr="weibull")


## mttf plot
#m1mttf<-read.table("clipboard",header=TRUE)
dataCN<-odbcConnectAccess2007(paste0(getwd(),"/doe/m1baseline.mdb")) #  100101001
system.time(urr<-sqlQuery(dataCN, "SELECT urr.*, count.Count FROM [*Unscheduled Removal rates] AS urr INNER JOIN 
                          (SELECT DOC.[Object Type], Count(DOC.Slot) AS Count FROM DOC GROUP BY DOC.[Object Type]) AS 
                          [count] ON urr.[LRU  type] = count.[object type];"))
print(object.size(urr),units="MB")
close(dataCN)
hist(urr$MTTF,breaks=40)
hist(urr[urr$MTTF<3e6,"MTTF"])
urr$medttf<-(1000/urr$Rate)*(log(2))^(1/urr$Shape) # urr$medttf<-urr$MTTF*(log(2))^(1/urr$Shape) # median time to fail
sum(urr$MTTF<1000000)/nrow(urr) # 81% of parts are below 1 million miles mttf
sum(urr$MTTF<500000)/nrow(urr) # 62% of parts are below 500k miles mttf
sum(urr$MTTF<100000)/nrow(urr) # 31% of parts are below 100k miles mttf
sum(urr$MTTF<10000)/nrow(urr) # 6% of parts are below 10k miles mttf
sum(urr$MTTF<3e6)/nrow(urr) # 84% of parts are below 300000k miles mttf
sum(urr$medttf<5e5)/nrow(urr) # 64

# with hist of initial ages and mark at typical platform age
par(mfrow=c(2,1)) # it's masking the axis labels I'll have to do them separately
hist(oai[oai$Objecttype!=100101001,"AFHRv"],xlim=c(0,4e5),ylab="Part Quantity",xlab="Miles",main = "Currently Installed Part Ages",axes=FALSE)
axis(2,at=c(0,1e5,2e5,3e5,4e5,5e5),labels=c("0","","","","",""))
axis(1,at=c(0,1e5,2e5,3e5,4e5),labels=c("0","100K","200K","300K","400K"))
mtext(text=c("200K","400K"),side=2,line=1,at=c(2e5,4e5))
abline(v=24204/447*4*(447/40),col="orange") # average miles accrued between depot visits (10 year interval)
#hist(urr[urr$MTTF<5e5,"MTTF"],probability=FALSE,breaks=40,xlab="Days to Failure",main="MTTF Histogram for Bottom 62% of Part Numbers - out of 1551 Total")
# oai is in miles, mttfs are in days, convert the latter
(a<-24204/447*4/365) # miles per day per platform for forecast period
sum(urr$medttf*a<4e5)/nrow(urr) # miles instead of days - 80%
hist(urr[urr$medttf*a<4e5,"medttf"]*a,probability=FALSE,breaks=40,xlab="Miles to Failure",axes=FALSE,
     ylab="Part Number Quantity",main="Median TTF Histogram for Bottom 80% of Part Numbers - out of 1551 Total")
axis(2,at=c(0,50,100,150,200,250,300,350),labels=c("0","","100","","200","","300",""))
axis(1,at=c(0,1e5,2e5,3e5,4e5),labels=c("0","100K","200K","300K","400K"))
mtext(text=c("100","200","300"),side=2,line=1,at=c(100,200,300))
#hist(urr$medttf,probability=FALSE,breaks=40,xlab="Miles to Failure",main="Median TTF Histogram for Bottom 64% of Part Numbers - out of 1551 Total")
# medians have are more dispersed because the parts with the same (high) mttfs have different shapes
abline(v=24204/447*4*(447/40),col="orange")
par(mfrow=c(1,1))

# quantity of new parts in an m1 that exhibit at least some wear out
sum(((urr$Shape>1 & urr$`Completed Repairs`==0)*urr$Count))/sum(urr[urr$`Completed Repairs`==0,"Count"]) # 94%
# quantity of part numbers in an m1 that exhibit at least some wear out as new
sum(urr$Shape>1 & urr$`Completed Repairs`==0)/nrow(urr[urr$`Completed Repairs`==0,]) # 94%
# quantity of repaired parts in an m1 that exhibit at least some wear out
sum(((urr$Shape>1 & urr$`Completed Repairs`==1)*urr$Count))/sum(urr[urr$`Completed Repairs`==1,"Count"]) # 75%
# quantity of part numbers in an m1 that exhibit at least some wear out as repaired
sum(urr$Shape>1 & urr$`Completed Repairs`==1)/nrow(urr[urr$`Completed Repairs`==1,]) # 75%

par(mfrow=c(1,2))
hist(urr[urr$`Completed Repairs`==0,"Shape"],ylim=c(0,800),xlab="Shape Parameter",main="New Part Wear Out Level")
hist(urr[urr$`Completed Repairs`==1,"Shape"],ylim=c(0,800),xlab="Shape Parameter",main="Repaired Part Wear Out Level")
par(mfrow=c(1,1))

# 201281001
onepart<-urr[urr$`LRU  type`==201281001,]
onepart0<-onepart[1,];onepart1<-onepart[2,]

### hazard function
# S = 1 - F
# h = f/S = f/(1-F)
fortheX<-seq(1,8000,10)
S1 <- 1 - pweibull(fortheX,zweib1$estimate[1],zweib1$estimate[2])
h1 <- dweibull(fortheX,zweib1$estimate[1],zweib1$estimate[2])/S1
S2 <- 1 - pweibull(fortheX,zweib2$estimate[1],zweib2$estimate[2])
h2 <- dweibull(fortheX,zweib2$estimate[1],zweib2$estimate[2])/S2
plot(fortheX,h1,type="l",xlab="Days",ylab="Hazard Rate",main="Failure Rate");lines(fortheX,h2,col="royalblue")
legend("top",c("New Part","Repaired Part"),fill=c("black","royalblue"))

# one NSN - 1015, weapons
a<-weibsAN[weibsAN$NSN %in% 5963014746208,] # 1015011772671 gun tube # track 2530015319542
c<-weibsCN[weibsCN$NSN %in% 5963014746208,]
fortheX<-seq(1,50000,100) # 140000
S1 <- 1 - pweibull(fortheX,a$shape,a$scale)
h1<- a$shape*((1/a$scale)^a$shape)*fortheX^(a$shape-1)
S2 <- 1 - pweibull(fortheX,c$shape,c$scale)
h2<- c$shape*((1/c$scale)^c$shape)*fortheX^(c$shape-1)
plot(fortheX,h1,type="l",xlab="Days",ylab="Hazard Rate",main="Instantaneous Failure Rate",lwd=2)
lines(fortheX,h2,col="royalblue",lwd=2)
legend("topright",c("New Part","Repaired Part"),fill=c("black","royalblue"),cex=0.85,bty="n")
#plot(fortheX,h1,ylim=c(0,7e3),type="l") # 0.00005
#lines(fortheX,h2,ylim=c(0,7e3))#,ylim=c(0,0.00005))

par(mfrow=c(1,1))
par(mar=c(4,4.1,4.1,2.1))
# Survival Function
plot(fortheX,S1,type="l",main="Gun Mount Tube MTTF Decreases  10%; Wear Out Turns to Infant Mortality",xlab="Days",ylab="Probability of Survival")
lines(fortheX,S2,col="royalblue")
legend("topright",legend=c("New Part","Repaired Part"),fill=c("black","royalblue"))
abline(v=fortheX[which(abs(S1-S2)==(min(abs(S1-S2))))])
text(x=-7500+fortheX[which(abs(S1-S2)==(min(abs(S1-S2))))],y=.8,label=paste(round(fortheX[which(abs(S1-S2)==(min(abs(S1-S2))))],-2),"days\n at cross"))
abline(v=c(a$MeanTime,c$MeanTime),col=c("black","royalblue"))
text(x=8000+a$MeanTime,y=0.6,label=paste(round(a$MeanTime,-2),"\n mean days"))
text(x=-8000+c$MeanTime,y=0.9,label=paste(round(c$MeanTime,-2),"\n mean days"),col="royalblue")
# pdf
#histogram
windows(width=10, height=6)
par(mfrow=c(1,2))
fortheX=seq(0,6e4,100)
secondrandomdata<-rweibull(10000,c$shape,c$scale)
firstrandomdata<-rweibull(10000,a$shape,a$scale)
abc<-hist(rweibull(10000,a$shape,a$scale),probability=TRUE,ylim=c(0,0.000240),col=rgb(.3,.3,.3,.5),xlim=c(0,4.5e4),
          main="New Part Failure Times",xlab="Days",ylab="Probability Density",axes=T,
          breaks=seq(0,2499+max(c(firstrandomdata,secondrandomdata)),by=2500))
#axis(1,at=c(0,20000,40000,60000,80000,100000),labels=c(0,"20K","40K","60K","80K","100K"))
#axis(2)
lines(fortheX,dweibull(fortheX,a$shape,a$scale),lwd=2)
abline(v=a$scale*(log(2))^(1/a$shape),col="orange",lwd=2)
#text(x=a$scale*(log(2))^(1/a$shape)+20000,y=4e-5,col="darkorange",
#     label=paste(round(a$scale*(log(2))^(1/a$shape),-3),"median\ndays to fail"))
abline(v=a$scale*gamma(1+1/a$shape),col="blue",lwd=2)
#text(x=a$scale*(log(2))^(1/a$shape)+20000,y=2e-5,col="blue",
#     label=paste(round(a$scale*gamma(1+1/a$shape),-3),"mean\ndays to fail"))
# 2
hist(secondrandomdata,probability=TRUE,main="Repaired Part Failure Times",#Electronic Control Orders: Repaired Vs. New
     xlab="Days",breaks=seq(from=0,to=max(secondrandomdata)+abc$breaks[2]-abc$breaks[1],by=abc$breaks[2]-abc$breaks[1]),ylab="Probability Density",
     ylim=c(0,0.000240),xlim=c(0,45000),col=rgb(.2,.2,1,.5),axes=T)
#axis(1,at=c(0,20000,40000,60000,80000,100000),labels=c(0,"20K","40K","60K","80K","100K"))
#axis(2)
lines(fortheX,dweibull(fortheX,c$shape,c$scale),lwd=2)
abline(v=c$scale*(log(2))^(1/c$shape),col="orange",lwd=2)
#text(x=c$scale*(log(2))^(1/c$shape)+3e4,y=4e-5,col="darkorange",
#     label=paste(round(c$scale*(log(2))^(1/c$shape),-2),"median\ndays to fail"))
abline(v=c$scale*gamma(1+1/c$shape),col="blue",lwd=2)
#text(x=c$scale*(log(2))^(1/c$shape)+3e4,y=2e-5,col="blue",
#     label=paste(round(c$scale*gamma(1+1/c$shape),-3),"mean\ndays to fail"))
#legend("topright",legend=c("New Part","Repaired Part"),fill=c("black","royalblue"),cex=0.01)
par(mfrow=c(1,1))
#title("testtime")
# hazard
windows(width=10, height=8) # to fix this awful legend problem
plot(fortheX,h1,type="l",xlab="Days",lwd=2,ylab="Failure Rate",main="Instantaneous Failure Rate",
     cex=1.2,cex.axis=1.2,cex.lab=1.5,ylim=c(0,0.0004))
lines(fortheX,h2,col="royalblue",lwd=2)
legend("topright",legend=c("New Part","Repaired Part"),fill=c("black","royalblue"),bty="n",cex=1.5)
abline(v=fortheX[which(abs(h1-h2)==(min(abs(h1-h2))))])
text(x=4500+fortheX[which(abs(h1-h2)==(min(abs(h1-h2))))],y=0.00016,cex=1.3,
     label=paste(round(fortheX[which(abs(h1-h2)==(min(abs(h1-h2))))],-2),"days\n at cross"))
abline(v=c(a$MeanTime,c$MeanTime),col=c("black","royalblue"))
text(x=-2600+a$MeanTime,y=0.000031,label=paste(round(a$MeanTime,-2),"\n mean days"))
text(x=-2600+c$MeanTime,y=0.000128,label=paste(round(c$MeanTime,-2),"\n mean days"),col="royalblue")

s3<-1 - pweibull(fortheX,a$shape,a$scale);plot(fortheX,s3,type="l")
s4<-1 - pweibull(fortheX,c$shape,a$scale);lines(fortheX,s4,type="l",col="red")
abline(v=fortheX[which(abs(s3-s4)==(min(abs(s3-s4))))])
# hazards with the same MTTF
a$scale*gamma(1+1/a$shape)
cnew<-c
cnew$MeanTime<-a$MeanTime
cnew$scale<-cnew$MeanTime/gamma(1+1/c$shape)
h3<-a$shape*((1/a$scale)^a$shape)*fortheX^(a$shape-1)
h4<-c$shape*((1/cnew$scale)^c$shape)*fortheX^(c$shape-1)
plot(fortheX,h3,type="l",xlab="Days",ylab="Failure Rate",main="Instantaneous Failure Rate")
lines(fortheX,h4,col="royalblue")
legend("bottomright",legend=c("New Part","Repaired Part"),fill=c("black","royalblue"))
abline(v=fortheX[which(abs(h3-h4)==(min(abs(h3-h4))))])
text(x=-5500+fortheX[which(abs(h3-h4)==(min(abs(h3-h4))))],y=0.000022,label=paste(round(fortheX[which(abs(h3-h4)==(min(abs(h3-h4))))],-2),"days\n at cross"))
abline(v=c(a$MeanTime,c$MeanTime),col=c("black","royalblue"))
text(x=4500+a$MeanTime,y=0.000031,label=paste(round(a$MeanTime,-2),"\n mean days"))
#text(x=-4500+c$MeanTime,y=0.000024,label=paste(round(cnew$MeanTime,-2),"\n mean days"),col="royalblue")

5315013787824
2590010986751
2590011964716 # 1.9
3040011028207 # 1.6
1240014750276 # 1.5
5330013170093 # 1.8
5340013632705 # 2.5
5895013177618 # 2.3
2530015319542 # 1.6

# histogram of tank birthdays
hist(freq=TRUE,prodYear$IN.SERVICE,xlab="Year",main="M1A1 First Year of Service",
     breaks=c(as.Date("1991-01-01"),as.Date("1992-01-01"),as.Date("1993-01-01"),as.Date("1994-01-01"),as.Date("1995-01-01"),
              as.Date("1996-01-01"),as.Date("1997-01-01"),as.Date("1998-01-01"),as.Date("1999-01-01"),as.Date("2000-01-01"),
              as.Date("2001-01-01"),as.Date("2002-01-01"),as.Date("2003-01-01"),as.Date("2004-01-01"),as.Date("2005-01-01"),
              as.Date("2006-01-01"),as.Date("2007-01-01"),as.Date("2008-01-01"),as.Date("2009-01-01"),
              as.Date("2010-01-01"),as.Date("2011-01-01"),as.Date("2012-01-01"),as.Date("2013-01-01"),
              as.Date("2014-01-01"),as.Date("2015-01-01"))) # 700 x 280
hist(freq=TRUE,totalsSub$LstSttDt,breaks=c(as.POSIXct("1991-01-01"),as.POSIXct("1992-01-01"),as.POSIXct("1993-01-01"),as.POSIXct("1994-01-01"),as.POSIXct("1995-01-01"),
                                 as.POSIXct("1996-01-01"),as.POSIXct("1997-01-01"),as.POSIXct("1998-01-01"),as.POSIXct("1999-01-01"),as.POSIXct("2000-01-01"),
                                 as.POSIXct("2001-01-01"),as.POSIXct("2002-01-01"),as.POSIXct("2003-01-01"),as.POSIXct("2004-01-01"),as.POSIXct("2005-01-01"),
                                 as.POSIXct("2006-01-01"),as.POSIXct("2007-01-01"),as.POSIXct("2008-01-01"),as.POSIXct("2009-01-01"),
                                 as.POSIXct("2010-01-01"),as.POSIXct("2011-01-01"),as.POSIXct("2012-01-01"),as.POSIXct("2013-01-01"),
                                 as.POSIXct("2014-01-01"),as.POSIXct("2015-01-01")),
     main="Last ERO Start Date Per Platform")
hist(as.numeric(totalsSub$LstSttDt-totalsSub$INSERVICE)/365,xlab="Years",main="M1A1 Years in Service")

############################################ OCTOBER redo for 1 part at a time
# 2006 failure times
# enginetotals<-totals[totals$NSN %in% 2835015482910,]
# actually, start over
# find the failures assuming start date of 2006, then build totals and merge
#enginesSNnsnminOrdered5<-SNnsnminOrdered5[SNnsnminOrdered5$NSN %in% 2835015482910,] # go read on line 270ish - this DF has all the failures, including 2nd and 3rd, etc.
enginerawEros<-rawEros[rawEros$NSN %in% 2835015482910,]
enginerawEros$IN.SERVICE<-as.Date('2007-02-16')

system.time(enginesSNnsnminOrdered5<-ddply(enginerawEros,c("SerialNumber","NSN"),first5Ordered)) #### FOR SOME REASON THIS IS RETURNING ALL THE DATES AS NOT NA EVEN THOUGH SOME ARE
# apply(head(enginesSNnsnminOrdered5),FUN=is.na,MARGIN=1) # test to see the missing dates are actually NA
# FSC NSN and group
enginesSNnsnminOrdered5$FSC<-substr(enginesSNnsnminOrdered5$NSN,1,4) # add fsc
enginesSNnsnminOrdered5$Grp<-substr(enginesSNnsnminOrdered5$NSN,1,2) # add group

# makes ugly INF and NANs - make the Infs NA, NaN are treated as NA (not vice versa)
enginesSNnsnminOrdered5$diff2[which(enginesSNnsnminOrdered5$diff2==Inf)]<-NA
enginesSNnsnminOrdered5$diff3[which(enginesSNnsnminOrdered5$diff3==Inf)]<-NA
enginesSNnsnminOrdered5$diff4[which(enginesSNnsnminOrdered5$diff4==Inf)]<-NA
enginesSNnsnminOrdered5$diff5[which(enginesSNnsnminOrdered5$diff5==Inf)]<-NA
nrow(enginesSNnsnminOrdered5) # 112
# only 22% of 2nd removals are non-zero at this point
sum(!is.na(enginesSNnsnminOrdered5$diff2))/length(is.na(enginesSNnsnminOrdered5$diff2))
# zero out the negative or zero removal times
enginesSNnsnminOrdered5$diff2[which(enginesSNnsnminOrdered5$diff2<=0)]<-NA
enginesSNnsnminOrdered5$diff3[which(enginesSNnsnminOrdered5$diff3<=0)]<-NA
enginesSNnsnminOrdered5$diff4[which(enginesSNnsnminOrdered5$diff4<=0)]<-NA
enginesSNnsnminOrdered5$diff5[which(enginesSNnsnminOrdered5$diff5<=0)]<-NA
# now only 21% of 2nd removals are non-zero
sum(!is.na(enginesSNnsnminOrdered5$diff2))/length(is.na(enginesSNnsnminOrdered5$diff2))

partnumbers<-data.frame("NSN"=as.character(2835015482910),stringsAsFactors = FALSE)
serialnumbers<-data.frame("SerialNumber"=unique(rawEros$SerialNumber)) # this is a trimmed down list already
system.time(enginetotals<-merge(x=partnumbers,y=serialnumbers,by=NULL))
system.time(enginetotals<-join(x=enginetotals,y=enginesSNnsnminOrdered5,by=c("SerialNumber","NSN"),type="full"))
# bring over in service date
enginetotals<-join(x=enginetotals,y=prodYear[,c("SerialNumber","IN.SERVICE")],by="SerialNumber",type="full")
colnames(enginetotals)[length(enginetotals)]<-"INSERVICE" # Rstats
tail(enginetotals)
enginetotals<-enginetotals[-nrow(enginetotals),]# some weird NA row at the end
# bring over max date this serial number ordered a part
enginetotals<-join(x=enginetotals,y=SNmaxStart,by="SerialNumber",type="full")
                    ### DECISION POINT
# treat all platforms as starting their lives on 1 Jan 2006 or 16 Feb 2007 (niin assignment date)
enginetotals$INSERVICE<-as.Date('2007-02-16')
enginetotals<-enginetotals[enginetotals$LstSttDt>as.Date('2007-02-16'),] # kill the platforms we saw last before 2006
# or just look at platforms that were born after 1 Jan 2005
#enginetotals<-enginetotals[enginetotals$INSERVICE>as.Date('2006-01-01'),] # kill the platforms that were born before 2006
  
## suspension data
enginetotals[which(is.na(enginetotals$diff1)),"diff1"]<-as.numeric(enginetotals[which(is.na(enginetotals$diff1)),"LstSttDt"]-enginetotals[which(is.na(enginetotals$diff1)),"INSERVICE"])
# need the other differences ; need only one suspension per row
system.time(enginetotals[which(is.na(as.character(enginetotals$ScdSttDt)) & !is.na(as.character(enginetotals$FstSttDt))),"diff2"]<-
              as.numeric(enginetotals[which(is.na(as.character(enginetotals$ScdSttDt)) & !is.na(as.character(enginetotals$FstSttDt))),"LstSttDt"]-
                           enginetotals[which(is.na(as.character(enginetotals$ScdSttDt)) & !is.na(as.character(enginetotals$FstSttDt))),"FstSttDt"]))
# I really need to subtract the previous END date instead of the previous START date, but I can't get that working right now!
system.time(enginetotals[which(is.na(as.character(enginetotals$ThdSttDt)) & !is.na(as.character(enginetotals$ScdSttDt))),"diff3"]<-
              as.numeric(enginetotals[which(is.na(as.character(enginetotals$ThdSttDt)) & !is.na(as.character(enginetotals$ScdSttDt))),"LstSttDt"]-
                           enginetotals[which(is.na(as.character(enginetotals$ThdSttDt)) & !is.na(as.character(enginetotals$ScdSttDt))),"ScdSttDt"]))
system.time(enginetotals[which(is.na(as.character(enginetotals$FurSttDt)) & !is.na(as.character(enginetotals$ThdSttDt))),"diff4"]<-
              as.numeric(enginetotals[which(is.na(as.character(enginetotals$FurSttDt)) & !is.na(as.character(enginetotals$ThdSttDt))),"LstSttDt"]-
                           enginetotals[which(is.na(as.character(enginetotals$FurSttDt)) & !is.na(as.character(enginetotals$ThdSttDt))),"ThdSttDt"]))
system.time(enginetotals[which(is.na(as.character(enginetotals$FvhSttDt)) & !is.na(as.character(enginetotals$FurSttDt))),"diff5"]<-
              as.numeric(enginetotals[which(is.na(as.character(enginetotals$FvhSttDt)) & !is.na(as.character(enginetotals$FurSttDt))),"LstSttDt"]-
                           enginetotals[which(is.na(as.character(enginetotals$FvhSttDt)) & !is.na(as.character(enginetotals$FurSttDt))),"FurSttDt"]))

## need to remove a few zeros (again) b/c of suspension times
length(which(enginetotals$diff1<=0));enginetotals$diff1[which(enginetotals$diff1<=0)]<-NA
length(which(enginetotals$diff2<=0));enginetotals$diff2[which(enginetotals$diff2<=0)]<-NA
length(which(enginetotals$diff3<=0));enginetotals$diff3[which(enginetotals$diff3<=0)]<-NA
length(which(enginetotals$diff4<=0));enginetotals$diff4[which(enginetotals$diff4<=0)]<-NA
length(which(enginetotals$diff5<=0));enginetotals$diff5[which(enginetotals$diff5<=0)]<-NA

source("C:/Users/tbaer/Desktop/m1a1/randCWwork/m1functions.R")
system.time(leftandright0<-CensUncensm1(enginetotals,1))
system.time(zweib1<-fitdistcens(leftandright0,distr="weibull"))
system.time(leftandright1<-CensUncensm1(enginetotals,3))
system.time(zweib2<-fitdistcens(leftandright1,distr="weibull"))
# see line 935

