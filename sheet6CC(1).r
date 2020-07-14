

### Question CC6.1

library(durham)
data(missile)
missile$Temperature<- factor(missile$Temperature, levels=c("Low", "Medium", "High"))


# a)
missile.lm<- lm(Battery.Life~Temperature*Material, data=missile)
summary(missile.lm)
# F-statistic:    11 on 8 and 27 DF,  p-value: 9.426e-07
# clearly, the predictors explain jointly a significant proportion of the variation

# b) 
anova(missile.lm)
# At the 5% level, we do need the interaction terms. Hence, Temperature*Material is the simplest appropriate model. 
anova(lm(Battery.Life~Material*Temperature, data=missile))
# does not depend on order of inclusion (balanced factorial design)

# c)
missile.material     <-lm(Battery.Life~Material,data=missile)
missile.materialtemp <-lm(Battery.Life~Material+Temperature,data=missile)
anova(missile.material,missile.materialtemp)
#1     33 66963                                  
#2     31 27845  2     39119 21.776 1.239e-06 ***

# Clearly, the Temperature term needs to be included.

# d)

anova(missile.lm)
sse <- 18231
ssr <- 39119+10864+9614 
sst <- sse+ ssr  
rsquared <- ssr/sst
rsquared
#  0.7657527
summary(missile.lm)
# Multiple R-squared: 0.7652,     Adjusted R-squared: 0.6956 



### Question CC6.2

# a)
data(cement, package="MASS")
cement=cement[,c(5,1,2,3,4)]
names(cement)= c("heat", "aluminate", "tri.silicate", "ferite", "di.silicate")
cement

# b)
cement.fit.full = lm(heat~ aluminate + tri.silicate + ferite + di.silicate, data = cement)

s<- summary(cement.fit.full)$sigma

# c) 
anova(cement.fit.full)

anova( lm(heat~ ferite + di.silicate+aluminate + tri.silicate, data=cement))

anova( lm(heat~ di.silicate+aluminate + tri.silicate+ ferite, data=cement))

# We see that (of course) the SS and significances do change when changing the order of inclusion; however for all orders of inclusion it is the case that 2 or 3  predictors are sufficient to explain the variation in the response. It also appears that aluminate + tri.silicate appear to explain generally more variation than the other two compounds


# d)

CI<-function(model, var){
  pI <- length(model$coef)
  n  <- length(model$residuals)
  RSSI <- sum((model$residuals)^2)
  ci <- RSSI/var+2*pI-n
  return(ci)
}


# e)

# for instance,

cement.fit.1.3 = lm(heat~  aluminate+ferite,   data = cement)
cement.fit.1.3 = lm(heat~.,   data = cement[,c(1,2,4)])
CI(cement.fit.j,s^2)
# [1] 198.0947 (this agrees with Table 3.5)

cement.fit.1 = lm(heat~  aluminate,   data = cement)
cement.fit.1 = lm(heat~.,   data = cement[,c(1,2)])
CI(cement.fit.1,s^2)
# [1] 202.5488    (this agrees with Table 3.5)

# f)
# As a first simplification, it would be useful to define the set of all possible subsets (that is, the `power set' of the predictors) and then to write a loop which computes CI for each element of this power set (This is in in practice a bit more tricky then it looks).  An even better strategy would be to discard models automatically which appear implausible or unlikely to be optimal, given the information collected so far. This will lead into stepwise methods (Sec 3.9.3)






