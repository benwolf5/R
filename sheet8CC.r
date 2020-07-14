
### Question 8.1


 library(npmlreg)
 data(hosp)

 hosp
 ?hosp

# a)
fit1 <- lm(duration~age+temp1, data=hosp) 

par(mfrow=c(2,2))
plot(fit1)
    # seems trumpet shape at first glance, but questionable.
    # the scale-location plot indicates rather homoscedasticity, apart from one outlier (case 7).    
    # case 7 is also influential.
    # normality ok with some deviation in the tails (but small data set, so hard to tell)
plot(hosp$age, fit1$res)
plot(hosp$temp1, fit1$res)
plot(fit1$res)
    # looks okay.

# b)

# (i) 
 library(MASS)
 boxcox(fit1 )
    # lambda close to 0, approx -0.2 
    # 95% CI: [ -0.8, 0.4]  does not contain 1, so transformation needed

# (ii)
    # Decide on lambda =0
    # y=log(duation)

# (iii) 
 fit2 <- lm(log(duration)~age+temp1, data=hosp)  
 par(mfrow=c(2,2))
 plot(fit2)

 plot(hosp$age, fit2$res)
 plot(hosp$temp1, fit2$res)
 plot(fit2$res)
 boxcox(fit2)
      ## looks all good.



#### Question 8.2

# a) Pervious code from rugby experiment (Michaelmas term)

rugby<- read.table("http://www.maths.dur.ac.uk/~fwwz94/data/rugby.csv",  sep=",", header=FALSE)
colnames(rugby)<-c("length","width") 
dim(rugby)
#[1] 39  2

par(mfrow=c(1,1))
plot(rugby)
m <- colMeans(rugby)
m
points(m[1],m[2], pch="+", col=2, cex=2)

par(mfrow=c(2,1))
hist(rugby$length)  
hist(rugby$width)  
qqnorm(rugby$length)
qqline(rugby$length)
qqnorm(rugby$width)
qqline(rugby$width)
# strong indication that both variable are normal (apart from extreme values and a 'heaping' effect)
# Does this imply bivariate normal? 

Sigma <-var(rugby)
Sigma

d <- mahalanobis(rugby, m, Sigma)
which(d>   qchisq(0.95,2))
par(mfrow=c(1,1))
plot(rugby)
points(rugby[c(2,5),], col=2)


dim(rugby)
# 39 2
plot(qchisq( (1:39-0.5)/39,2),sort(d))
abline(a=0,b=1)

# Compare to marginal distributions
par(mfrow=c(2,2))
qqnorm(rugby$length)
qqline(rugby$length)
qqnorm(rugby$width)
qqline(rugby$width)
plot(qchisq( (1:39-0.5)/39,2),sort(d))
abline(a=0,b=1)


# b, c)   This is the function as produced in the lecture, but with 2 replaced by 3  

plot2d.pca<- function(Z, add.proj=FALSE, ...){

  n <- dim(Z)[1]
  plot(Z,...)
  eigen.var  <- eigen(var(Z))

  gamma1 <- eigen.var[[2]][,1]
  lambda1<- eigen.var[[1]][1]
  pcline1a   <- colMeans(Z) -  3*sqrt(lambda1)* gamma1
  pcline1b   <- colMeans(Z) +  3*sqrt(lambda1)* gamma1 #
  segments(pcline1a[1], pcline1a[2], pcline1b[1], pcline1b[2], col=2, lwd=2)

  if (add.proj){
     t1  <- t(gamma1)%*%(t(Z)-colMeans(Z))
     for (i in 1:n){
         segments(Z[i,1], Z[i,2], colMeans(Z)[1]+ t1[i]*gamma1[1],
           colMeans(Z)[2]+t1[i]*gamma1[2] , col=2, lty=2)
     }
  }  
}


plot2d.pca(rugby)
plot2d.pca(rugby, add.proj=TRUE)


# d)

plot2d.2pca<- function(Z, plot.second=FALSE, add.proj=FALSE, ...){
    
  n<-dim(Z)[1]
  plot(Z,...)
  eigen.var  <- eigen(var(Z))
  
  gamma1 <- eigen.var[[2]][,1]
  lambda1<- eigen.var[[1]][1]
  pcline1a   <- colMeans(Z) -  2*sqrt(lambda1)* gamma1
  pcline1b   <- colMeans(Z) +  2*sqrt(lambda1)* gamma1 #
  segments(pcline1a[1], pcline1a[2], pcline1b[1], pcline1b[2], col=2, lwd=2)
  
  
  if (plot.second==TRUE){
    
    gamma2 <-  eigen.var[[2]][,2]
    lambda2<- eigen.var[[1]][2]
    pcline2a   <- colMeans(Z) - 2*sqrt(lambda2) * gamma2 
    pcline2b   <- colMeans(Z) + 2*sqrt(lambda2) * gamma2 
    segments(pcline2a[1],pcline2a[2], pcline2b[1],pcline2b[2], col=3, lty=4, lwd=2)
  }
  
  if (add.proj==TRUE){
    t1  <- t(gamma1)%*%(t(Z)-colMeans(Z))
    for (i in 1:n){
      segments(Z[i,1], Z[i,2], colMeans(Z)[1]+ t1[i]*gamma1[1],
               colMeans(Z)[2]+t1[i]*gamma1[2] , col=2, lty=2)
    }
  }
  
}


plot2d.2pca(rugby, plot.second=TRUE)
