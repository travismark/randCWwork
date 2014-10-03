# Note Vn is the asymptotic covariance matrix, so it's the
# Consistent estimator divided by n. For true Wald tests
# based on numerical MLEs, just use the inverse of the Hessian.
WaldTest = function(L,thetahat,Vn,h=0) {# H0: L theta = h
     WaldTest <- numeric(3)
     names(WaldTest) <- c("W","df","p-value")
     if (length(thetahat)!=1){
     W <- t(L%*%thetahat-h) %*% solve(L%*%Vn%*%t(L)) %*%
       (L%*%thetahat-h); r <- dim(L)[1]
     } else {W <- (L*thetahat-h) * (1/(L*Vn)) * (L*thetahat-h); r <- 1}
     W <- as.numeric(W)
     print(W)
     pval <- 1-pchisq(W,r)
     WaldTest[1] <- W; WaldTest[2] <- r; WaldTest[3] <- pval
     WaldTest
} # End function WaldTest

LL = 1
thetahat=1.204782
subdf<-weibulls_initial[weibulls_initial$WUC=="06A" & weibulls_initial$PN=="7-311310001-41" ,]
subdf<-droplevels(subdf)
censsubdf<-CensUncens(subdf)
weibsubdf2<-fitdistcens2(censsubdf,"weibull")
thetahat <- weibsubdf[[1]][1]
kov <- solve(weibsubdf2$hess)
kov1param<-(kov[1,1])
WaldTest(LL,thetahat,kov1param,1)

thetahat<-weibsubdf[[1]]
subdf<-weibulls_initial[weibulls_initial$WUC=="06A" & weibulls_initial$PN=="7-311310001-43" ,]


