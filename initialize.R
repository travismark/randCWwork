###  2013-12-08
# initialize the AAV fleet

# assume:  40 aavs have been to depot each of the past years
# the age of every component is at most 1 year just after an IROAN is complete
# max age of any part is 14 years because fleet size is 520 (520/40 = 13 + 1 = 14 years)

# read in the data
setwd("C:\\Users\\tbaer\\Desktop\\aav\\models\\12.8 weekend tests\\newinit")
oai<-read.csv("oaiinitial.csv",colClasses=c("factor","numeric","numeric","factor",
                                            "factor","numeric","numeric","numeric"))
mttfcon<-read.csv("mttfconseq.csv",colClasses=c("factor","numeric","numeric"))

# set completed repairs to zero initially
oai$CR<-0

dim(oai[oai$ObjectType==900262001,])
mttfcon[mttfcon$ObjectType==900262001,]

## For both operating locations, CONUS and Training
#### For all max ages, 2-14
###### Sample lifetimes

# for both operating locations:
for(ii in seq(from=10001,to=10002)){
  # for all max ages, 2-14
  oaibase<-oai[oai$SRAN==ii,]  # pass just the subset of OAI at that base
  for(jj in 2:14){
    parents<-sample(unique(oaibase$ParentNum))  # randomly sort the platforms here
    # the number of platforms in each base are not divisible by 13, so must hardcode a few things
    if(ii=10001) { # if it's CONUS, the last two groups will have one more than the others
      if(jj<13){
        oaisubset<-oaibase[((jj-2)*floor(length(parents)/13)+1):((jj-1)*floor(length(parents)/13)))]
      }
      if(jj=13){
        oaisubset<-oaibase[((jj-2)*floor(length(parents)/13)+1):((jj-2)*floor(length(parents)/13)+31))]
      }
      if(jj=14){
        oaisubset<-oaibase[((jj-2)*floor(length(parents)/13)+2):((jj-2)*floor(length(parents)/13)+32))]
      }
    }
    if(ii=10002) { # if it's Training, the last two groups will have one fewer than the others}
      if(jj<13){
        oaisubset<-oaibase[(392+((jj-2)*ceiling(length(parents)/13)+1)):(392+((jj-1)*ceiling(length(parents)/13)))]
      }
      if(jj=13){
        oaisubset<-oaibase[(392+((jj-2)*ceiling(length(parents)/13)+1)):(392+((jj-2)*ceiling(length(parents)/13)+9))]
      }
      if(jj=14){
        oaisubset<-oaibase[(392+((jj-2)*ceiling(length(parents)/13))):(392+((jj-2)*ceiling(length(parents)/13)+8))]
      }
    }
  }
}

oaibase<-oai[oai$SRAN==10001,]
b<-sample(oaibase$ParentNum)

initOne <- function(base,parents,oaisubset){
  # initializes OAI for these parents at this base
}


## initialize one part
# 11 max years (base 10001)
hoursremaining<-11*125
# shape of 1.2, scale of 0.199098224 hours (turret)
mean(rweibull(100000,shape=1.2,scale=0.199098224))
qplot(rweibull(100000,shape=1.2,scale=0.199098224))

for