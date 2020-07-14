### Question CC5.1

# a)

# Our previous code for the insects data
insects0 =
  c(
    10,7,20,14,14,12,10,23,17,20,14,13,
    11,17,21,11,16,14,17,17,19,21,7,13,
    0,1,7,2,3,1,2,1,3,0,1,4,
    3,5,12,6,4,3,5,5,5,5,2,4,
    3,5,3,5,3,6,1,1,3,2,6,4,
    11,9,15,22,15,16,13,10,26,26,24,13
  )

insects= data.frame("spray"= c(rep("A",12), rep("B",12), rep("C",12),rep("D",12), rep("E",12), rep("F",12)), "insects" = insects0)
insects$spray = as.factor(insects$spray)

# Linear model
fit1.insects <- lm(insects ~ spray, data= insects)
summary(fit1.insects)
model.matrix(fit1.insects)
# Clearly, spray type A is the reference category

# b)

# If we want to use category F as reference category, we have to do the following:

insects$spray2<- relevel(insects$spray, ref = "F")

fit2.insects<- lm(insects ~ spray2, data= insects)
summary(fit2.insects)
model.matrix(fit2.insects)

# c)
predict(fit1.insects, newdata= data.frame(spray="C"), interval="prediction")
#      fit       lwr     upr
#1 2.083333 -6.066732 10.2334
predict(fit2.insects, newdata= data.frame(spray2="C"), interval="prediction")
#     fit       lwr     upr
#1 2.083333 -6.066732 10.2334
# The predictions are the same, as it should be (the coding does not change the properties of the model).
# The prediction intervals are rather too wide to be actually useful, and extend into the negative range which is implausible since the data are counts.  (The larger variability especially for spray types A and F  has led to a relatively large overall standard error of about 4, which has inflated the width of the prediction intervals.)

# d)
### this piece of code is not exam relevant

options("contrasts") # checks the current coding scheme. Default  "contr.teactment" which means dummy coding

options(contrasts=c("contr.sum", contrasts=TRUE)) # changing coding scheme to effect coding
options("contrasts") # check what has changed

fit3.insects<- lm(insects ~ spray, data= insects) # fit model
summary(fit3.insects)
model.matrix(fit3.insects)
predict(fit3.insects, newdata= data.frame(spray="C"), interval="prediction")

options(contrasts=c("contr.treatment", contrasts=TRUE))
  # brings us back to the old coding scheme......


### Question CC5.2

library(durham)
data(missile)
missile$Temperature<- factor(missile$Temperature, levels=c("Low", "Medium", "High"))


# a)

# 3x3 factorial design, complete and balanced.

# b)
missile.lm<- lm(Battery.Life~Temperature*Material, data=missile)
summary(missile.lm)
# t_1^A = t_1^B=0,  t_11^AB=0,  t_1j^AB= t_j1^AB=0, j=2,3.


# c)
par(mfrow=c(1,1))
interaction.plot(missile$Temperature, missile$Material, missile$Battery.Life)

  # We see clearly in the plot the negative effect of medium and high temperatures, corresponding to
                                        # a negative sign in the coefficients for TemperatureMedium  and TemperatureHigh. We further see
                                        # a mostly postive effect of materials 2 and 3 as compared to the reference category (1), reflected in positive
                                        # signs for the indicators Material2 and Material3. Finally, the kinks for medium temperatures hint at some
                                        # interaction, visbile in the significant Material3:TemperatureMedium interaction term.  (That is, the strength of the effect that material3 has on the lifetime depends on the temperature, and is particularly strong for medium temperature).

# d)

new<-data.frame(Temperature=c("Low","Low","High"), Material=c("1", "2", "1"))

# Confidence interval
predict(missile.lm, newdata=new, interval="confidence", level=0.99)
#     fit       lwr      upr
#1 134.75  98.75210 170.7479
#2 155.75 119.75210 191.7479
#3  57.50  21.50210  93.4979

# Prediction interval
predict(missile.lm, newdata=new, interval="prediction", level=0.99)
#      fit       lwr      upr
#1 134.75  54.25624 215.2438
#2 155.75  75.25624 236.2438
#3  57.50 -22.99376 137.9938

# For "Low", "2" by hand:
x0<- c(1,   0,0,1, 0,0,0,0, 0)

y0hat<-t(x0)%*%missile.lm$coef
y0hat
 # 155.75
 
 s<- summary(missile.lm)$sigma
 s
 # 25.98486
 tcrit<-    qt(0.995,   36 -9)
 tcrit  
# 2.770683
  
 # CI
 y0hat +c(-1,1)* s*tcrit*sqrt(t(x0)%*%summary(missile.lm)$cov.unscaled%*%x0)
 # [1]   119.7521 191.7479    

# PI
y0hat +c(-1,1)* s*tcrit*sqrt(1+ t(x0)%*%summary(missile.lm)$cov.unscaled%*%x0)
#   75.25624 236.24376

