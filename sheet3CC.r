
################################################################################
# 3.1
################################################################################

################################################################################
# a)

# Load the data.
library(durham)
data(engine)
?engine

# Carry out tasks.
  dim(engine)
  #  [1] 46  3
  names(engine)
 #[1] "CO"  "HC"  "NOX"
 
  X<-  as.matrix(cbind(rep(1,46), engine[,1:2]) )
  Y<-  engine[,3]
  
  betahat<-solve(t(X)%*%X)  %*% t(X) %*%Y
  betahat

  fit<-lm(NOX~CO+HC, data=engine)
  fit




######################################################
## 3.2
#####################################################


# a)
 data(airquality)
 airquality
   # lots of missing values (NA) 

# b)
 par(mfrow=c(2,3))
 hist(airquality$Ozone)
 hist(log(airquality$Ozone))
 hist((airquality$Ozone)^2)
 hist((airquality$Ozone)^3)
 hist((airquality$Ozone)^{1/2})
 hist((airquality$Ozone)^{1/3})
    # Most of the distributions are rather skew
    # That one for k=1/3 looks best (most normal) 

# c)
 names(airquality)
 newair <- na.omit(airquality)
 newair$oz <- (newair$Ozone)^{1/3}
 pairs(newair)


# d)
 air.lm <- lm(oz~  Solar.R + Wind + Temp, data= newair)
 air.lm
 # Coefficients:
 # (Intercept)      Solar.R         Wind         Temp  
 #  -0.298945     0.002206    -0.075967     0.050058  

# e)
 par(mfrow=c(1,2))                                  
 plot(air.lm$res)  # there is no pattern and the variance appears constant -- so this confirms (A1) and  (A2)
qqnorm(air.lm$res) # follows a straight line, so confirms (A3)
# If you want to add the QQ line (good practice):
qqline(air.lm$res)


 ## In the second term, you will discuss in greater depth how to analyze residual plots 
 

# f)

sdev<-function(res,p)
{
    n<-length(res)
    s2<-sum(res^2)/(n-p)
    return(sqrt(s2))
}    

sdev(air.lm$res, 4)

summary(air.lm)$sigma

 
