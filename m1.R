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

require(plyr)
require(dplyr)
require(hexbin)
require(MASS)
require(fitdistrplus)
require(RODBC)
`%notin%` <- function(x,y) !(x %in% y) 

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
plot(sort(table(rawEros$SerialNumber),decreasing=TRUE))
head(sort(table(rawEros$SerialNumber),decreasing=TRUE),1)/nrow(rawEros) # proportion of data with serial number = 0
plot(sort(table(rawEros$Unit),decreasing=TRUE))
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
rawEros$FSC<-round(rawEros$NSN,-9)/1000000000
rawEros$Group<-round(rawEros$NSN,-11)/100000000000
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
  FvhSttDt<-as.Date(FvhSttDt, origin = '1899-12-30')
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
SNnsnminOrdered5$FSC<-round(SNnsnminOrdered5$NSN,-9)/1000000000 # add fsc
SNnsnminOrdered5$Grp<-round(SNnsnminOrdered5$NSN,-11)/100000000000 # add group
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
sum(is.na(rawEros$LeadTime))
# take out the negative ones lead times
sum(rawEros$LeadTime<0,na.rm=TRUE)/nrow(rawEros) # only 277 of these, 0.16%
rawEros[which(rawEros$LeadTime<0),"LeadTime"]<-NA
hist(as.numeric(rawEros[rawEros$FSC==5120,"LeadTime"]))

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
totals$FSC<-round(totals$NSN,-9)/1000000000 # add fsc
totals$Grp<-round(totals$NSN,-11)/100000000000 # add group
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
system.time(a<-CensUncensm1(totals[totals$NSN %in% 1005000565572,])) # 5310009388387
system.time(zweib<-fitdistcens(a,distr="weibull")) # sometimes singular - not enough failures
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
a<-rownames(table(SNnsnminOrdered5$NSN))[which(table(SNnsnminOrdered5$NSN)>150)] # 200 works with no NAs, so does 50 (has a NaN error?). 20 does not
datasubset<-totals[totals$NSN %in% a,]

source("C:/Users/tbaer/Desktop/m1a1/randCWwork/m1functions.R")
system.time(weibs<-gatherDistFitsm1(datasubset,"NSN",unbug=FALSE,modkm=FALSE,ontwth=2))
system.time(weibs<-gatherDistFitsm1(totals,"NSN",unbug=FALSE,modkm=FALSE,ontwth=1))
weibs[,"shape"]<-as.numeric(as.character(weibs[,"shape"]))
head(weibs[order(weibs$Events),])
hist(totals[totals$NSN %in% 2540012553347 & !is.na(totals$FstSttDt),"diff1"])
head(weibs[order(runif(nrow(weibs))),],10) # ten random ones
hist(weibs$shape)


# collect them
system.time(weibsAN<-gatherDistFitsm1(totals,"NSN",unbug=FALSE,modkm=FALSE,ontwth=1)) # 377 seconds
system.time(weibsAF<-gatherDistFitsm1(totals,"FSC",unbug=FALSE,modkm=FALSE,ontwth=1)) # 80 seconds
system.time(weibsAG<-gatherDistFitsm1(totals,"Grp",unbug=FALSE,modkm=FALSE,ontwth=1)) # 76 seconds
system.time(weibsBN<-gatherDistFitsm1(totals,"NSN",unbug=FALSE,modkm=FALSE,ontwth=2)) # 51 seconds - many few data points b/c so few 2nd removals
system.time(weibsBF<-gatherDistFitsm1(totals,"FSC",unbug=FALSE,modkm=FALSE,ontwth=2)) # 8  seconds
system.time(weibsBG<-gatherDistFitsm1(totals,"Grp",unbug=FALSE,modkm=FALSE,ontwth=2)) # 5  seconds
system.time(weibsCN<-gatherDistFitsm1(totals,"NSN",unbug=FALSE,modkm=FALSE,ontwth=3)) # 263 seconds
system.time(weibsCF<-gatherDistFitsm1(totals,"FSC",unbug=FALSE,modkm=FALSE,ontwth=3)) # 14 seconds
system.time(weibsCG<-gatherDistFitsm1(totals,"Grp",unbug=FALSE,modkm=FALSE,ontwth=3)) # 7 seconds

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
weibsCN<-read.csv("weibsCN.csv",colClasses=c(NSN="character"))
weibsBN<-read.csv("weibsBN.csv",colClasses=c(NSN="character"))
weibsAN<-read.csv("weibsAN.csv",colClasses=c(NSN="character"))


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
weibs2use$FSC<-round(as.numeric(weibs2use$NSN),-9)/1000000000
weibs2use$Grp<-round(as.numeric(weibs2use$NSN),-11)/100000000000
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
CNgC<-weibsCN[weibsCN$Events>ctf,"NSN"]; length(ANgC)/nrow(weibsAN)
CFgC<-weibsCF[weibsCF$Events>ctf,"FSC"]; length(AFgC)/nrow(weibsAF)
CGgC<-weibsCG[weibsCG$Events>ctf,"Grp"]; length(AGgC)/nrow(weibsAG)
# start with all NSNs, then left join onto it - doesn't work, have to stich together pieces
weibs2use<-data.frame("NSN"=unique(weibsCN$NSN),"FSC"=0,"Grp"=0,
                      "wfm"=factor("Grp",levels=c("NSN","FSC","Grp","All")),stringsAsFactors=FALSE)
weibs2use$FSC<-round(as.numeric(weibs2use$NSN),-9)/1000000000
weibs2use$Grp<-round(as.numeric(weibs2use$NSN),-11)/100000000000
# NSNs that exceed cutoff
NSNs2use<-weibs2use[weibs2use$NSN %in% ANgC,"NSN"] # NSNs I want to use
weibs2useNSN<-inner_join(weibs2use,select(weibsCN[weibsCN$NSN %in% NSNs2use,],NSN,shape,scale,MeanTime,Events,Censored),by="NSN")
weibs2useNSN$wfm<-"NSN"
FSCs2use<-CFgC#unique(weibs2use[weibs2use$FSC %in% AFgC,"FSC"]) # FSCs I may want to use
#weibs2useFSC<-inner_join(weibs2use[weibs2use$NSN %notin% weibs2useNSN$NSN,],select(weibsAF[weibsAF$FSC %in% FSCs2use,],FSC,shape,scale,MeanTime,Events,Censored),by="FSC") # this isn't working
weibs2useFSC<-merge(weibs2use[weibs2use$NSN %notin% weibs2useNSN$NSN,],select(weibsCF[weibsCF$FSC %in% FSCs2use,],FSC,shape,scale,MeanTime,Events,Censored),by="FSC")
weibs2useFSC$wfm<-"FSC"
Grps2use<-CGgC
weibs2useGrp<-merge(weibs2use[weibs2use$NSN %notin% weibs2useNSN$NSN & weibs2use$NSN %notin% weibs2useFSC$NSN,]
                    ,select(weibsCG[weibsAG$Grp %in% Grps2use,],Grp,shape,scale,MeanTime,Events,Censored),by="Grp")
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
# now the m1a1 models in model
nsnstofind<-read.csv("m1a1nsnsinmodel0908.csv",col.names="NSN")
nsnstofind$FSC<-round(nsnstofind$NSN,-9)/1000000000
nsnstofind$Grp<-round(nsnstofind$NSN,-11)/100000000000
nsnstofind$InSOE<-"Yes"
nsnstofind$NSN<-as.character(nsnstofind$NSN) # make sure join column is the same structure
weibs2use<-nsnstofind
# bring over everything that matches - A
## A
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
nsnstofind$NSN<-as.numeric(nsnstofind$NSN)
weibs2use<-nsnstofind
nsnstofindCmatch<-inner_join(weibs2use,select(weibs2useTC,NSN,shape,scale,MeanTime,Events,Censored,wfm),by="NSN")
FSCs2use<-weibsCF[weibsCF$Events>ctf,"FSC"]
weibs2useFSC<-merge(weibs2use[weibs2use$NSN %notin% weibs2useNSN$NSN,],select(weibsCF[weibsCF$FSC %in% FSCs2use,],FSC,shape,scale,MeanTime,Events,Censored),by="FSC")
weibs2useFSC$wfm<-"FSC"
weibs2useFSC$InSOE<-"No"
Grps2use<-weibsCG[weibsCG$Events>ctf,"Grp"]
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
weibs2useTCmodelNSN<-weibs2useT # totals for 1st removal - for saving
write.csv(weibs2useTCmodelNSN,"modelNSNweibuls30failscutoffTwoToFive.csv",row.names=FALSE)
#####################################


########## compare drop in scale
weibsN<-join(select(weibsAN,NSN,scale,Events,Censored),select(weibsBN,NSN,scale,Events,Censored),by="NSN")
weibsN<-join(weibsN,select(weibsCN,NSN,scale,Events,Censored),by="NSN")
colnames(weibsN)<-c("NSN","scale1","Events1","Censored1","scale2","Events2","Censored2","scale3","Events3","Censored3")
weibsNpos<-weibsN[which(!is.na(weibsN$scale1) & !is.na(weibsN$scale2) & !is.na(weibsN$scale3)),]
weibsNg10<-weibsNpos[which(weibsNpos$Events1>=10 & weibsNpos$Events2>10 & weibsNpos$Events3>10),]
mean(weibsNg10$scale1/weibsNg10$scale2);mean(weibsNg10$scale1/weibsNg10$scale3)
weibsNg30<-weibsNpos[which(weibsNpos$Events1>=30 & weibsNpos$Events2>30 & weibsNpos$Events3>30),]
mean(weibsNg30$scale1/weibsNg30$scale2);mean(weibsNg30$scale1/weibsNg30$scale3)
hist(weibsNg30$scale1/weibsNg30$scale3)

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
dist1<-rnorm(n*n1,0.9358265,2.18359422683716E-02) # where 10 is number of platforms deployed
dist2<-rnorm(n*n2,0.9001246,2.82451796531677E-02)
dist3<-rnorm(n*n3,0.9030827,1.50580132007599E-02)
dist4<-rnorm(n*n4,0.9725072,2.09958839416504E-02)
dist5<-rnorm(n*n5,0.8932196,2.93550491333008E-02)
dist6<-rnorm(n*n6,0.9136784,0.036519193649292)
dist7<-rnorm(n*n7,0.8910694,7.13001203536987E-02)
(totalD<-n1+n2+n3+n4+n5+n6+n7)
(totMu<-mean(dist1)*n1/totalD+mean(dist2)*n2/totalD+mean(dist3)*n3/totalD+mean(dist4)*n4/totalD+
   mean(dist5)*n5/totalD+mean(dist6)*n6/totalD+mean(dist7)*n7/totalD) # 0.913
(totVar<-sum((c(dist1,dist2,dist3,dist4,dist5,dist6,dist7)-totMu)^2)/(totalD*n)) # 0.00112, empirical variance
# variance estimate:  
# assuming mixture of normal theory : #Sum([mixprob]*[var]+[mixprob]*([availability]-[expavail])^2) AS rowvarmix # then maybe average this (/4)
sum(n1/totalD*var(dist1)+n1/totalD*(mean(dist1)-totMu)^2+n2/totalD*var(dist2)+n2/totalD*(mean(dist2)-totMu)^2+
      n3/totalD*var(dist3)+n3/totalD*(mean(dist3)-totMu)^2+n4/totalD*var(dist4)+n4/totalD*(mean(dist4)-totMu)^2+
      n5/totalD*var(dist5)+n5/totalD*(mean(dist5)-totMu)^2+n6/totalD*var(dist6)+n6/totalD*(mean(dist6)-totMu)^2+
      n7/totalD*var(dist7)+n7/totalD*(mean(dist7)-totMu)^2) # 0.00112 0.1% avail
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

timetime<-data.frame("n"=c(10,100,500,1000,5000,10000,50000),"time"=0)
set.seed(102)
n=50000;
allhist<-data.frame("hist"=seq(1:n),"avail"=0,"var"=0)
timetime[7,2]<-system.time(for (ii in seq(1:n)){
  n<-1
  dist1<-rnorm(n*n1,0.9358265,2.18359422683716E-02) # where 10 is number of platforms deployed
  dist2<-rnorm(n*n2,0.9001246,2.82451796531677E-02)
  dist3<-rnorm(n*n3,0.9030827,1.50580132007599E-02)
  dist4<-rnorm(n*n4,0.9725072,2.09958839416504E-02)
  dist5<-rnorm(n*n5,0.8932196,2.93550491333008E-02)
  dist6<-rnorm(n*n6,0.9136784,0.036519193649292)
  dist7<-rnorm(n*n7,0.8910694,7.13001203536987E-02)
  allhist[ii,2]<-mean(c(dist1,dist2,dist3,dist4,dist5,dist6,dist7)) # stores average availability of 1 history
  allhist[ii,3]<-var(c(dist1,dist2,dist3,dist4,dist5,dist6,dist7))
})[1]
tenkhist<-a # tenk took 768 seconds, 13 minutes
fiftykhist<-allhist # took 3781 seconds, 63 minutes 
hist(fiftykhist[,2],col=rgb(0,1,1,1/3));hist(tenkhist[,2],add=T,col=rgb(1,0,0,1/3))
abline(v=mean(fiftykhist[,2]),col=rgb(0,1,1));mean(fiftykhist[,2])
abline(v=mean(tenkhist[,2]),col=rgb(1,0,0));mean(tenkhist[,2])
var(fiftykhist[,2]);var(tenkhist[,2]) # used for prediction interval
sd(fiftykhist[,2])*qt(0.975,49999)*sqrt(1+1/50000);sd(tenkhist[,2])*qt(0.975,9999)*sqrt(1+1/10000) # for predints widths
sd(fiftykhist[,2])*qt(0.975,49999)/sqrt(50000);sd(tenkhist[,2])*qt(0.975,9999)/sqrt(10000) # for confint widths
var(fiftykhist[,2])/50000^2;var(tenkhist[,2])/10000^2 # seems unlikely for confints

#
mean(allhist[,2]) # history average
var(allhist[,2]) # variance of averages (variance across histories)
mean(allhist[,3]) # history average variance (average of within histories)

# calculate variance empirically
# first try using equal mixing probabilities
totalD<-7 # number of distributions put together
(totMu<-mean(allhist[,2]))
sum(1/totalD*sqrt(2.18359422683716E-02)+1/totalD*(0.9358265-totMu)^2+1/totalD*sqrt(2.82451796531677E-02)+1/totalD*(0.9001246-totMu)^2+
      1/totalD*sqrt(1.50580132007599E-02)+1/totalD*(0.9030827-totMu)^2+1/totalD*sqrt(2.09958839416504E-02)+1/totalD*(0.9725072-totMu)^2+
      1/totalD*sqrt(2.93550491333008E-02)+1/totalD*(0.8932196-totMu)^2+1/totalD*sqrt(0.036519193649292)+1/totalD*(0.9136784-totMu)^2+
      1/totalD*sqrt(7.13001203536987E-02)+1/totalD*(0.8910694-totMu)^2) # ?
sum(1/totalD*sqrt(2.18359422683716E-02)+1/totalD*(0.9358265-totMu)^2+1/totalD*sqrt(2.82451796531677E-02)+1/totalD*(0.9001246-totMu)^2+
      1/totalD*sqrt(1.50580132007599E-02)+1/totalD*(0.9030827-totMu)^2+1/totalD*sqrt(2.09958839416504E-02)+1/totalD*(0.9725072-totMu)^2+
      1/totalD*sqrt(2.93550491333008E-02)+1/totalD*(0.8932196-totMu)^2+1/totalD*sqrt(0.036519193649292)+1/totalD*(0.9136784-totMu)^2+
      1/totalD*sqrt(7.13001203536987E-02)+1/totalD*(0.8910694-totMu)^2)/7^2 # ?
sum(1/totalD*sqrt(2.18359422683716E-02)+1/totalD*(0.9358265-totMu)^2+1/totalD*sqrt(2.82451796531677E-02)+1/totalD*(0.9001246-totMu)^2+
      1/totalD*sqrt(1.50580132007599E-02)+1/totalD*(0.9030827-totMu)^2+1/totalD*sqrt(2.09958839416504E-02)+1/totalD*(0.9725072-totMu)^2+
      1/totalD*sqrt(2.93550491333008E-02)+1/totalD*(0.8932196-totMu)^2+1/totalD*sqrt(0.036519193649292)+1/totalD*(0.9136784-totMu)^2+
      1/totalD*sqrt(7.13001203536987E-02)+1/totalD*(0.8910694-totMu)^2)/50000^2 # ?

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


source("C:/Users/tbaer/Desktop/m1a1/randCWwork/m1functions.R")
system.time(theM1ShipTimes<-gatherDistFitsm1(rawEros,distrib="lognormal",classTypes=c("NSN","Apps")))

# getting error, try w/o apps?
subsetoferos<-erosForLT[1:20000,]
system.time(theM1ShipTimes<-gatherDistFitsm1(subsetoferos,distrib="lognormal",unbug=FALSE))#classTypes=c("NSN","Apps")))


fitanlt<-function(df){
  leadtimes<-df[!is.na(df$LeadTime),"LeadTime"]
  oneleadtime<-fitdistr(leadtimes,densfun="lognormal")
  out<-cbind(t(data.frame(oneleadtime[1])),t(data.frame(oneleadtime[2])))
  out<-as.data.frame(out)
  out[1,5]<-exp(oneleadtime[[1]][1]) # median time b/w order and receive
  out[1,6]<-exp(oneleadtime[[1]][1]+(oneleadtime[[1]][2])^2/2) # mean
  out[1,7]<-oneleadtime$n # events
  colnames(out)<-c("meanlog","sdlog","meanlogSE","sdlogSE","MedianTime","MeanTime","Events") 
  (out)
}

system.time(theM1ShipTimesSplitByApps<-ddply(erosForLT,c("NSN","Apps"),fitanlt))
system.time(theM1ShipTimes<-ddply(erosForLT,c("NSN"),fitanlt))

write.csv(theM1ShipTimesSplitByApps,"theM1ShipTimesSplitByApps.csv",row.names=FALSE)
write.csv(theM1ShipTimes,"theM1ShipTimes.csv",row.names=FALSE)

abc<-arrange(summarise(group_by(rawEros,NSN,Apps),n()),NSN)
def<-arrange(summarise(group_by(abc,NSN,n())),NSN)
sum(def[,2]>1);def[which(def[,2]>1),]
