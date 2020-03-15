#read in data
data()
pairs()

lm(y~x1+x2+x3,data=,x=TRUE)

#prediction

x0 = data.frame(x1=,x2=,x3=)
predict(fit1, newdata = x0)
predict(fit, newdata=x0, interval="prediction")
predict(fit, newdata=x0, interval="confidence")

#OR by hand
betahat = coef(fit1)
x0=c(1,x1,x2,x3)
y0hat<-as.numeric(x0%*%fit$coef)
y0hat+c(-1,1)*qt(0.975, df)*s*sqrt(as.numeric(x0%*%summary(fit)$cov.unscaled%*%x0)) #confidence interval
y0hat+c(-1,1)*qt(0.975, 43)*s*sqrt(as.numeric(1+x0%*%summary(fit)$cov.unscaled%*%x0)) #prediction interval

#Linear Model with relationships between variables
lm(log(stack.loss) ~ Air.Flow + Water.Temp + I(Air.Flow*Water.Temp)+ I(Air.Flow^2),  data = stackloss)

mean()
var()

#Simulation example
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

#Check chi-squared of shat
schi<- shat^2*(n-p)/sigma^2
hist(schi)
plot(qchisq( (1:500-0.5)/500,n-p),sort(schi))
abline(0,1,lty=2) #straight line for true

#Mahalanobis distance and Outlier
m     <- colMeans(data)
Sigma <- cov(data)
d <- mahalanobis(data, m, Sigma)
which(d>   qchisq(0.95,number of betahats))

#plotcontour function
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
#pairs
par(mfrow=c(4,4), cex=0.5)

for (i in 1:4){
  for (j in 1:4){
    if (i==j){hist(stackloss[,i], main="", xlab="", ylab="")  } else {
      plotcontour(stackloss[,c(j,i)])
    }
  }
}  

#Confidence interval 
confint(object,parameter)
#t statistic
qt(0.975, df)
#omit data points
newair <- na.omit(airquality)
#identify data points which you have pressed on plot
identify(x)
#qq plots
qqplot()
qqline()
#standard deviation function
sdev<-function(res,p)
{
  n<-length(res)
  s2<-sum(res^2)/(n-p)
  return(sqrt(s2))
} 
#inverse matrix
solve()
#transpose matrix
t()
