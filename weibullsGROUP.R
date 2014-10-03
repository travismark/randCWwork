####  Group weibulls together
# Winter 2012/2013

#option in the stat test's testonesubset()

# start with PN
length(unique(tsh$PN))
for(ii in 2:(4-1)){print((dim(combn(4,ii))[2]))}

### this leaves out a few groups to compare:
##1 stattests eliminates all rows without a weibull fit b/c of too few data
##2 will not group combinations across classifiers, like removal intervals 2 & 3 with Ft. Rucker & SWA

# test a loop - make sure if something passes the first statement it won't also check the third statement
b<-7  # order of brackets matters
if(b>8) {  print ("gt8")
  } else if (b==7) {print ("nothing")
  } else {print("one")}


### preallocate memory for dataframes with something like
data.frame(col1=numeric(5))

remv_small2<-remv_small[remv_small[,"WUC"]==remv_small[1:2,"WUC"],]

someweibulls<-tshweibulls[tshweibulls$adjRepInt==0 & tshweibulls$Loc=="ALL" & tshweibulls$PN!="ALL" & 
    tshweibulls$Platform=="ALL" & tshweibulls$LastRepair=="ALL",]
someweibulls<-droplevels(someweibulls)
remv_set<-tsh[tsh$adjRepInt==0,]
for (nn in seq_len(dim(combn(length(someweibulls[,1]),3))[2])) { # for each column (specific combination of rows)
  test<-rbind(test,testOneMany(rows=combn(length(someweibulls[,1]),3)[,nn],someweibulls,"PN",remv_set))
}

# find ones with over x events
shapetest<-tshweibulls[tshweibulls[,"Events"]>3000,7]
scaletest<-tshweibulls[tshweibulls[,"Events"]>3000,8]
shsctest<-data.frame(shape=shapetest,scale=scaletest)
disttest<-dist(shsctest,method="euclidean")
clusttest<-hclust(disttest)
plot(clusttest)


myplclust <- function( hclust, lab=hclust$labels, lab.col=rep(1,length(hclust$labels)), hang=0.1,...){
  ## modifiction of plclust for plotting hclust objects *in colour*!
  ## Copyright Eva KF Chan 2009
  ## Arguments:
  ##    hclust:    hclust object
  ##    lab:        a character vector of labels of the leaves of the tree
  ##    lab.col:    colour for the labels; NA=default device foreground colour
  ##    hang:     as in hclust & plclust
  ## Side effect:
  ##    A display of hierarchical cluster with coloured leaf labels.
  y <- rep(hclust$height,2); x <- as.numeric(hclust$merge)
  y <- y[which(x<0)]; x <- x[which(x<0)]; x <- abs(x)
  y <- y[order(x)]; x <- x[order(x)]
  plot( hclust, labels=FALSE, hang=hang, ... )
  text( x=x, y=y[hclust$order]-(max(hclust$height)*hang),
        labels=lab[hclust$order], col=lab.col[hclust$order], 
        srt=90, adj=c(1,0.5), xpd=NA, ... )
}