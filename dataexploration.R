#### Exploratory Graphics
weibulls_initial<-read.csv("newTOW_LOC.csv",header=TRUE)

boxplot(weibulls_initial$SumOfTOW ~ weibulls_initial$WUC,varwidth=TRUE,
        main="Removal TOW by Part",ylab="Time on Wing at Removal",xlab="WUC")

boxplot(weibulls_initial$SumOfTOW ~ weibulls_initial$Loc,varwidth=TRUE,
        main="Removal TOW by Location",ylab="Time on Wing at Removal",xlab="Location")

boxplot(weibulls_initial[weibulls_initial$WUC=="06A",]$SumOfTOW ~ 
        weibulls_initial[weibulls_initial$WUC=="06A",]$Loc,varwidth=TRUE,
        main="MXSN Removal TOW by Location",ylab="Time on Wing at Removal",xlab="Location")
