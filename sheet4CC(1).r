library(durham)
data(engine)
fit<-lm(NOX~CO+HC, data=engine)
fit

# a)
 beta<- fit$coef
 s<-summary(fit)$sigma
 Sigma<- s^2*summary(fit)$cov.unscaled

 #            (Intercept)            CO          HC
 # (Intercept)  0.063601963  0.0040631790 -0.16893795
 # CO           0.004063179  0.0005361271 -0.01509929
 # HC          -0.168937953 -0.0150992864  0.52403670

 # SE(hatbeta1):
 sqrt(0.0005361271)
 # [1] 0.02315442

 # SE(hatbeta2):
 sqrt(0.52403670)
 # [1] 0.7239038

 summary(fit)
 # correct.

# b)
 beta[2] +c(-1,1)*0.02315442 *qt(0.975, 43)
 # -0.13546191 -0.04207123
 beta[3] +c(-1,1)*0.7239038 *qt(0.975, 43)
 # -0.568613  2.351169

confint(fit,2)
 # [1] -0.13546190 -0.04207124
confint(fit,3)
 # [1] -0.568613  2.351169

# c)
 summary(fit)
 # via p-value:   p=0.000407<0.05, so reject H0
 # via test statistic
    t = abs(-0.08877/0.02315)
    # 3.834557
    qt(0.975, 43)
    # 2.016692 < 3.834557, so reject H0
 # via CI: [-0.13546191, -0.04207123]  does not contain 0 so reject H0

# d)
 # via test statistic
   t = abs((1-0.89128)/ 0.72390)
   # 0.1501865 < 2.016692 so do not reject H0
 # via CI: [-0.568613,  2.351169]  does contain 1 so do not reject H0. 

# e)
test.s<-function(lmobject, sigma0){
    n<-length(lmobject$res)
    p<-length(lmobject$coef)
    s<-summary(lmobject)$sigma
    V= s^2/sigma0^2*(n-p)
    pvalue<- 1-pchisq(V, n-p)
    return(pvalue)
}


summary(fit)
test.s(fit,0.30)
 # [1] 0.04300668
 # H0 is (just) rejected, so we conclude sigma>0.3


## f) prediction
 # From a light-duty engine, one measures 12.2 g carbon monoxide and 0.4g hydrocarbons emitted per mile of usage. Use your fitted model to predict the nitrogen oxide emitted per mile (g). Obtain a 95% confidence interval for the expected emission, as well as a 95% prediction interval for the actual emission. 

predict(fit, newdata=data.frame("CO"=12.2, "HC"=0.4))
 #        1 
 # 0.8171029 

predict(fit, newdata=data.frame("CO"=12.2, "HC"=0.4), interval="confidence")
 #       fit       lwr      upr
 #1 0.8171029 0.3947171 1.239489

predict(fit, newdata=data.frame("CO"=12.2, "HC"=0.4), interval="prediction")
 # fit         lwr      upr
 #1 0.8171029 -0.01373135 1.647937

# alternatively:
x0<-c(1, 12.2,0.4)
y0hat<-as.numeric(x0%*%fit$coef)
y0hat
 #[1,] 0.8171029

#CI
y0hat+c(-1,1)*qt(0.975, 43)*s*sqrt(as.numeric(x0%*%summary(fit)$cov.unscaled%*%x0))
 #[1] 0.3947171 1.2394888

#PI
y0hat+c(-1,1)*qt(0.975, 43)*s*sqrt(as.numeric(1+x0%*%summary(fit)$cov.unscaled%*%x0))
 # [1] -0.01373135  1.64793720
