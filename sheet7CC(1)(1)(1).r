
munich <- read.table("http://www.maths.dur.ac.uk/~dma0je/Data/munich.dat")
munich$age<- 1999-munich$year
munich$location<-as.factor(munich$location)

     
## 1
# 1a )
   mfit1<- lm(rentsqm~ age+area+location+kitchen+cheating+bath+area:bath, data=munich)
   summary(mfit1)  
   anova(mfit1)
   # All of age, area, location and kitchen explain significant proportions of the variance in the response. Conditional on the inclusion of these, cheating and bath turn out to be uniinformative. However, interestingly, the interaction term area:bath contributes significant information again.
   var<-summary(mfit1)$sigma^2
   # [1]  4.423246

 
# 1b)
 # Backward selection
 step(mfit1, scale=var)

 # Forward selection
 mfit0<- lm(rentsqm~ 1, data=munich)
 step(mfit0, scale =var, scope = list(lower =mfit0, upper = mfit1),  direction = "forward") 

 mfit<- lm(formula = rentsqm ~ age + area + kitchen + location + cheating,  data = munich)

## 2)
# 2a)


 par(mfrow=c(2,2))
 plot(mfit$fitted, mfit$res)
 plot(munich$age, mfit$res)
 qqnorm(rstandard(mfit))
 qqline(rstandard(mfit))
 plot(rstandard(mfit),lm.influence(mfit)$h)


# The first residual plot indicates a slight trumpet shape, which would mean a slight violation of homoscedasticity.
# The second residual plot is well behaved -- no patterns are visible.
# Attainment of normaility of (studentized) residuals is excellent.
# The  last plot indicates that there is a small number of values for which hi and |ri| are large.  These would be the most influential values in this data set.


# 2b 

 E.distance<-function(model){
   n <- length(model$res)
   p <- length(model$coef)
   s <- summary(model)$sigma
   ei <- model$res
   hi <- lm.influence(model)$h
   Ei <-  n/(p*s)*abs(ei)*(exp(hi)-1)
   return(Ei)
 }


# 2c
 mfit.E <- E.distance(mfit)
 mfit.D <- cooks.distance(mfit)

 par(mfrow=c(2,2))
 plot(mfit.D)
 identify(mfit.D)
 plot(mfit.E)
 identify(mfit.E)
 plot(mfit.E,mfit.D)

 # the plots clearly single out the same influential cases (18, 106), and the relationship
 # between the two measures is quite linear, so one may say that the two criteria
 # provide approximately equivalent information 
### 2 marks


# 2d)
  # Clearly, the rule 0f thumb criterion is 2*2=4.

which(E.distance(mfit)>4)
 # 18  26  43  88 106 139 161 
 # 18  26  43  88 106 139 161 

which(cooks.distance(mfit)>1/2)
# named integer(0)

# While for both criteria the "most" influential cases are the same,
# they disagree on the classification of "actually" influential cases.
# Criterion E_i provides quite a lot (probably too many!) 
# On the other hand, it is known that Cooks distances are a bit too conservative in deteting
# influential observations. Further tuning of the rule of thumb in either case appears necessary.
# (For Cook's distances, this is some literature which has attempted this!) 


################################
