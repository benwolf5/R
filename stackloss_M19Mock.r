


################################################################################
# 3.3CC: Stackloss
################################################################################

# Load the data
data(stackloss)
stackloss
pairs(stackloss)
?stackloss

### (1)

# a)
 fit1 <- lm(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc., data = stackloss, x=TRUE)

 summary(fit1)
 # Q7
 # Air.Flow      0.7156     0.1349   5.307  5.8e-05 ***

 # Create a data frame containing the predictors for the new point.
 x0 = data.frame(Air.Flow = 50, Water.Temp = 21, Acid.Conc. = 85)

 # Compute prediction
 # Q8
 predict(fit1, newdata = x0)
 #      1 
 # 10.13293 

 # We can also do this by hand. 
 betahat = coef(fit1)
 #[1] 3.243364
 x0=c(1,50,21,85)
 y0hat   = t(x0) %*% betahat
 y0hat
 # 10.13293

# b)

 fit2 <- lm(log(stack.loss) ~ Air.Flow * Water.Temp + I(Air.Flow^2),  data = stackloss)
 # or
 fit2 <- lm(log(stack.loss) ~ Air.Flow + Water.Temp + I(Air.Flow*Water.Temp)+ I(Air.Flow^2),  data = stackloss)

 y0hat<-predict(fit2, newdata = x0)
 # Q9
 exp(y0hat)                                      
 #       1 
 # 7.880239 
 
 # x0=c(1,50,21,50*21,50^2)
 # y0hat   = t(x0) %*% fit2$coef
 # exp(y0hat)

 
  # Q10
  summary(fit2)
  # Residual standard error: 0.1314 on 16 degrees of freedom
  
 # Q11
 # compared to
 summary(fit1)
 # Residual standard error: 3.243 on 17 degrees of freedom
 # 
 # The df's are given by n-p, so 21-4=17 for fit1 and 21-5 for fit2.
 # The smaller standard error in fit2 is mainly due to the different scale of the responses 
 # (i.e. log(stack.loss) instead of stack.loss); the two standard errors are not actually comparable.


### (2)
# a)

 # Q 12
 x <- runif(100,0,3)
 fx<-  -1+3*x

 ahat <- rep(0,500)
 bhat <- rep(0,500)
 shat <- rep(0,500)
 for (j in 1:500){
   y  <-  fx + rnorm(100,0,0.5)
   fit<- lm(y~x)
   shat[j]<-summary(fit)$sigma
   ahat[j]<-fit$coef[1]
   bhat[j]<-fit$coef[2]
 }
par(mfrow=c(2,1))
plot(ahat,bhat)
hist(shat)

mean(ahat)

mean(bhat)

mean(shat)

# b)

 # Q 13
 # df = n-p= 98

 # Q14 
  # we have  shat^2*(n-p)/sigma^2 ~ chi^2(n-p) and so
  schi<- shat^2*(100-2)/0.5^2
  hist(schi)
  plot(qchisq( (1:500-0.5)/500,98),sort(schi))
  abline(0,1,lty=2)
 # very straight line -- yes this confirms the theory!
  

### (3)
# a)

 m     <- colMeans(stackloss)
 Sigma <- cov(stackloss)
 d <- mahalanobis(stackloss, m, Sigma)

 # Qu 15
 which(d>   qchisq(0.95,4))
 #  21

 # Qu 16
 par(mfrow=c(1,1))
 plot(qchisq( (1:21-0.5)/21,4),sort(d))
 abline(a=0,b=1)

# (i)  We know that, if a given q-variate data set is multivariate normal, then the (squared) Mahalanobis distances to the mean follow a chi-squared distribution with q degrees of freedom.  Hence, we can plot the ordered and squared Mahalanobis distances (d_i) against the (i-0.5)/21 
  # quantiles of a chi^2(4) distribution. If MVN holds, then we should observe a straight line
# (ii) In this case, we see a slight wave-pattern, with the d_i initially above the line, then below, meaning that the data points close to the mean "are  further away" and those far from the mean "are closer" to the overall mean than one would expect under the chi-squared property..
# (iii)From item (ii)  one does see some violation of multivariate normaility.  Still, this violation is not massive and the sample size is small, so that multivariate normality can not be be rejected outright [different interpretations allowed as long as argued coherently.] 
# (iv) The outlier test is only valid for data which are multivariate normal. So, if one takes the view in (iii) that the data are not MVN, then the outlier test loses its basic assumption and can therefore not be trusted.  If one argues in (iii) that MVN is just justifiable [I would do this] 
  # then the outlier test is trustable;  1 out of 21 outliers is also what one would expect at the 5% level of significance, under MVN.  Either way,  given our analysis in part (ii), it seems very unlikely that  we have missed further outliers.  
 

 # Q 17
 # This is just a possible solution; simpler implementations are possible
 plotcontour<-function(Z, grid.size=31, s.mult=4) { 
   m <- colMeans(Z)
   Sigma<- var(Z)
   d<- dim(Z)[2]
   if (d!=2){break("wrong matrix dimension")} 
   grid<- matrix(0,grid.size,2) 
   colsd<-rep(0,d)
   for (j in 1:2){
     colsd[j]<-sd(Z[,j])
     grid[,j]<- seq(m[j]-s.mult* colsd[j] ,m[j]+s.mult*colsd[j] , length=grid.size)
   }

  dens <- matrix(0,grid.size,grid.size)

  for (i in 1:grid.size){
      for (j in 1:grid.size){
     dens[i,j]<- 1/(2*pi*sqrt(det(Sigma)))*exp(-1/2* c(grid[i,1]-m[1], grid[j,2]-m[2])%*%solve(Sigma)%*%c(grid[i,1]-m[1], grid[j,2]-m[2])) 
      }
  }
  contour(grid[,1], grid[,2],dens)
  points(Z[,1], Z[,2])
  return(max(dens))
  }

 par(mfrow=c(1,1))
 plotcontour(stackloss[,1:2], grid.size=51)

 # Second example, just to check:
  plotcontour(scallops[,c("long","lat")])

 # Q18
 par(mfrow=c(4,4), cex=0.5)

    for (i in 1:4){
        for (j in 1:4){
            if (i==j){hist(stackloss[,i], main="", xlab="", ylab="")  } else {
                           plotcontour(stackloss[,c(j,i)])
                           }
            }
       }     


 
 
###################
# Q6

# e.g.
b<-c(1,12,5,6)

t(b)%*%b
#  206

sum(diag(b %*% t(b)))
# 206

sum(b^2)
#[1] 206

det(diag(b^2))
#[1] 129600
