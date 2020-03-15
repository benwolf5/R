library(durham)
data(stackloss)
stackloss
pairs(stackloss)
?stackloss
X<-stack.x
Y<-stack.loss
betahat<-solve(t(X)%*%X)  %*% t(X) %*%Y
fit1<-lm(stack.loss~Air.Flow+Water.Temp+Acid.Conc., data=stackloss)
fit1
summary(fit1)
y0<-0.7156*50+1.2953*21-0.1521*85-39.9197
y0
logstack<-log(stackloss$stack.loss)
fit2<-lm(logstack~Air.Flow+Water.Temp+I(Air.Flow*Water.Temp)+I(Air.Flow^2),data=stackloss)
summary(fit2) #remember log
logy1<-0.2369988*50-0.3663181*21+0.0071278*21*50-0.0028668*(50^2)-2.4100826
logy1
exp(logy1)
newx<-data.frame(Air.Flow=50,Water.Temp=21,Acid.Conc.=85)
predict(fit1,newdata=newx,se.fit=TRUE)
predict(fit2,newdata=newx,se.fit=TRUE)

#Part 2
x<-runif(100,0,3)
fx<--1+3*x
ahat<-rep(0,500)
bhat<-rep(0,500)
shat<-rep(0,500)
for (j in 1:500){
  y<-fx+rnorm(100,0,0.5)
  fit<-lm(y~x)
  ahat[j]<-fit$coefficients[1]
  bhat[j]<-fit$coefficients[2]
  RSS<-sum(fit$residuals^2)
  df<-fit$df.residual
  shat[j]<-sqrt(RSS/df)
}
mean(ahat)
mean(bhat)
mean(shat)

hat<-cbind(ahat,bhat)
betahat<-colMeans(hat)
varhat<-var(hat)
dhat<-mahalanobis(hat,betahat,varhat)
plot(qchisq( (1:500-0.5)/500,2),sort(dhat))
abline(a=0,b=1)

#part 3
m<-colMeans(stackloss)
Sigma<-var(stackloss)
d<-mahalanobis(stackloss,m,Sigma)
which(d>qchisq(0.95,4))
sorted<-sort(d)
plot(qchisq((1:21-0.5)/21,4),sorted)
abline(a=0,b=1)


#b)
plotcontour<-function(Z,min_x,max_x,min_y,max_y){
  m<-colMeans(Z)
  Sigma<-var(Z)
  dens<-matrix(0,51,51)
  x<-seq(min_x,max_x,length=51)
  y<-seq(min_y,max_y,length=51)
  for (i in 1:51){
    for (j in 1:51){
      dens[i,j]<- 1/(2*pi*sqrt(det(Sigma)))*exp(-1/2* c(x[i]-m[1], y[j]-m[2])%*%solve(Sigma)%*%c(x[i]-m[1], y[j]-m[2]))
    }
  }
  contour(x,y,dens)
  points(Z)
  return(max(dens))
}
plotcontour(stackloss[,1:2],40,90,12,30)

#c)
par(mfrow=c(4,4))
plot(1, type="n", axes=F, xlab="Air Flow", ylab="Air Flow")
plotcontour(stackloss[,1:2],40,90,10,30) #1:2
plotcontour(stackloss[,c(1,3)],40,90,70,100) #1:3
plotcontour(stackloss[,c(1,4)],40,90,0,45) #1:4
plotcontour(stackloss[,c(2,1)],10,30,40,90) #2:1
plot(1, type="n", axes=F, xlab="Water Temp", ylab="Water Temp")
plotcontour(stackloss[,c(2,3)],10,30,70,100) #2:3
plotcontour(stackloss[,c(2,4)],10,30,0,45) #2:4
plotcontour(stackloss[,c(3,1)],70,100,40,90) #3:1
plotcontour(stackloss[,c(3,2)],70,100,10,30) #3:2
plot(1, type="n", axes=F, xlab="Acid Conc.", ylab="Acid Conc.")
plotcontour(stackloss[,c(3,4)],70,100,0,45) #3:4
plotcontour(stackloss[,c(4,1)],0,45,40,90) #4:1
plotcontour(stackloss[,c(4,2)],0,45,10,30) #4:2
plotcontour(stackloss[,c(4,3)],0,45,70,100) #4:3
plot(1, type="n", axes=F, xlab="Stack.loss", ylab="Stack.loss")

