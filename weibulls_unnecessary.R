# Define a function to calculate all the necessary defaults
AddDefaultWeibulls <- function(weibulls_full)  {
  # initialize data frames to compile the weibulls
  weibs<-data.frame() # start with empty data frames
  weibs2<-data.frame() # to be binded together at the end
  weibs3<-data.frame()
  
  # include the necessary "ALL" classifications
  #for(ii in 1:8) {
  for(ii in 3:3) { 
    if(ii==1) { #ALL ALL ALL (1)
      for(jj in 1:length(levels(weibulls_full$WUC))) # For each WUC type:
      {input<-subset(weibulls_full,WUC==levels(weibulls_full$WUC)[jj])
       newdf<-CensUncens(input)
       newweibsfit<-fitdistcens(newdf,"weibull")
       out<-cbind(t(data.frame(newweibsfit[1])),t(data.frame(newweibsfit[2])))
       out<-as.data.frame(out)
       newweibs<-data.frame(levels(weibulls_full$WUC)[jj],"ALL","ALL","ALL",0,0,0,0)
       colnames(newweibs)<-c("WUC","Loc","PN","adjRepInt","shape","scale","shapeSE","scaleSE") 
       newweibs[1,5:8]<-out
       # Append one row per WUC
       #initialize if necessary
       if(length(weibs)==0) { # if it's the first WUC:
         weibs<-newweibs
         #set the factor levels
         levels(weibs[,1])<-c(levels(weibulls_full$WUC))
         levels(weibs[,2])<-c("ALL",levels(weibulls_full$Loc))
         levels(weibs[,3])<-c("ALL",levels(weibulls_full$PN))
         levels(weibs[,4])<-c("ALL",names(table(weibulls_full$adjRepInt)))
       } else { #other wucs.  assign the new weibulls to the next row; levels already set
         weibs[length(weibs[,1])+1,1:8]<-newweibs
       }  # endif: the first WUC type
      } # end that specific WUC  
    } #end ALL ALL ALL (1)
    
    if (ii==2) { # Specific Loc, ALL PN, ALL adjRepInt (2)
      for(jj in 1:length(levels(weibulls_full$WUC))) # For each WUC type:
      {
        for(kk in 1:length(levels(weibulls_full$Loc))) # For each Loc type:
        {
          #subset for that wuc, then for that location type
          input<-subset(weibulls_full,WUC==levels(weibulls_full$WUC)[jj]) 
          input<-subset(input,Loc==levels(input$Loc)[kk]) #one of each location for each WUC
          newdf<-CensUncens(input) # get the Time to Failure data for the next function
          newweibsfit<-fitdistcens(newdf,"weibull") # fit the weibull 
          out<-cbind(t(data.frame(newweibsfit[1])),t(data.frame(newweibsfit[2])))
          out<-as.data.frame(out) # get the pertinent info from the fitting
          newweibs<-data.frame(levels(weibulls_full$WUC)[jj],levels(weibulls_full$Loc)[kk],"ALL","ALL",0,0,0,0)
          colnames(newweibs)<-c("WUC","Loc","PN","adjRepInt","shape","scale","shapeSE","scaleSE") 
          newweibs[1,5:8]<-out
          # Append one row per WUC
          #initialize if necessary
          if(length(weibs2)==0) { # if it's the first WUC:
            weibs2<-newweibs
            #save the factor levels
            tempfact1<-weibs2[1,2]
            #set the factor levels
            levels(weibs2[,1])<-c(levels(weibulls_full$WUC))
            levels(weibs2[,2])<-c("ALL",levels(weibulls_full$Loc))
            weibs2[1,2]<-tempfact1 #previous command overwrote the 1st row's Loc factor/value. this re-overwrites it
            levels(weibs2[,3])<-c("ALL",levels(weibulls_full$PN))
            levels(weibs2[,4])<-c("ALL",names(table(weibulls_full$adjRepInt)))
          } else { #other wucs/locations.  assign the new weibulls to the next row; levels already set
            weibs2[length(weibs2[,1])+1,1:8]<-newweibs
          } # endif: the first WUC/Loc type   
        } # end for each Loc
      } # end for each WUC
    } # end Specific Loc, ALL PN, ALL adjRepInt (2)
    
    if (ii==3) { # Specific PN, ALL Loc, ALL adjRepInt (3)
      for(jj in 1:length(levels(weibulls_full$WUC))) # For each WUC type:
      {
        for(nn in 1:length(levels(weibulls_full$PN))) # For each PN type:
        {
          #subset for that wuc, then for that PN
          input<-subset(weibulls_full,WUC==levels(weibulls_full$WUC)[jj])
          input<-droplevels(input)
          input<-subset(input,PN==levels(input$PN)[nn]) #one of each PN for each WUC
          input<-droplevels(input)
          #newdf<-CensUncens(input) # get the Time to Failure data for the next function
          out<-getWeibullsFromDF(input)
          print("one")
          if (length(out==0)) {break #not enough data to build a weibull: skip it
          } else { 
            #newweibsfit<-fitdistcens(newdf,"weibull") # fit the weibull 
            #out<-cbind(t(data.frame(newweibsfit[1])),t(data.frame(newweibsfit[2])))
            #out<-as.data.frame(out) # get the pertinent info from the fitting
            newweibs<-data.frame(levels(weibulls_full$WUC)[jj],"ALL",levels(input$PN)[1],"ALL",0,0,0,0)
            colnames(newweibs)<-c("WUC","Loc","PN","adjRepInt","shape","scale","shapeSE","scaleSE") 
            newweibs[1,5:8]<-out
            # Append one row per WUC
            #initialize if necessary
            print(length(weibs3))
            if(length(weibs3)==0) { # if it's the first WUC:
              weibs3<-newweibs
              #save the factor levels
              tempfact1<-weibs2[1,3]
              #set the factor levels
              levels(weibs3[,1])<-c(levels(weibulls_full$WUC))
              levels(weibs3[,2])<-c("ALL",levels(weibulls_full$Loc))
              levels(weibs3[,3])<-c("ALL",levels(weibulls_full$PN))
              weibs3[1,3]<-tempfact1 #previous command overwrote the 1st row's PN factor/value. this re-overwrites it
              levels(weibs3[,4])<-c("ALL",names(table(weibulls_full$adjRepInt)))
            } else { #other wucs/locations.  assign the new weibulls to the next row; levels already set
              weibs3[length(weibs3[,1])+1,1:8]<-newweibs
            }  # endif: the first WUC/Loc type  
          } # endif: too little data
        } # end for each PN
      } # end for each WUC
    } # end Specific PN, ALL Loc, ALL adjRepInt (3)
    
  } # end all the necessary defaults (1-8)
  (rbind(weibs,weibs2,weibs3)) #output the weibulls
} # default weibulls function complete

# ALL ALL ALL
alls<-AddDefaultWeibulls(weibulls_initial)