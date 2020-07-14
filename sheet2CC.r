

# CC 2.1

#
scallops <- read.table("http://www.maths.dur.ac.uk/stats/courses/statsIII/data/scallops.dat", header=TRUE)
scallops <- read.table("T:/MATHS/DMA0JE/scallops.dat", header=TRUE)
m <- colMeans(scallops[,c("long","lat")])
Sigma<- var(scallops[,c("long","lat")])
Sigma

x<- seq(-74,-71, length=31)
y<- seq(38, 41, length=31)

dens <- matrix(0,31,31)

for (i in 1:31){
  for (j in 1:31){
   dens[i,j]<- 1/(2*pi*sqrt(det(Sigma)))*exp(-1/2* c(x[i]-m[1], y[j]-m[2])%*%solve(Sigma)%*%c(x[i]-m[1], y[j]-m[2])) 
 }
}


par(mfrow=c(1,1))
contour(x, y, dens)
points(scallops$long, scallops$lat)
  # The BNV seems not to be perfectly adequate as it cannot account for (and gets biased by) the small branch in the NW.
  # Relative to the fitted distribution, some points in the NW and the SE appear to be outliers, but
  # due to the reason above one has to be careful with this interpretation.



# CC 2.2
library(durham)
data(engine)
?engine

# or
# engine<-read.table("http://www.maths.dur.ac.uk/stats/courses/statsIII/data/engine.dat", header=TRUE)


# a)
M    <- colMeans(engine)
S    <- var(engine)

round(M, digits=2)
#  CO   HC  NOX 
# 7.96 0.55 1.33 

round(S, digits=2)
#       CO    HC   NOX
# CO  27.67  0.80 -1.75
# HC   0.80  0.03 -0.05
# NOX -1.75 -0.05  0.23

# b)
pairs(engine)

plot(engine$CO, engine$HC)
identify(engine$CO, engine$HC)

plot(engine$CO, engine$NOX)
identify(engine$CO, engine$NOX)

plot(engine$HC, engine$NOX)
identify(engine$HC, engine$NOX)

    # Observations 34,45, and 39  appear to be possible outliers


 # c)
d <- mahalanobis(engine, M, S)
d[5]
# 2.385444 

 eng5 <- as.numeric(engine[5,])
(eng5-M)%*%solve(S)%*%(eng5-M)
   ## Note: eng5-m is a vector (not a matrix), and so R will try to align its orientation by itself so that the matrix multiplication works.
#        [,1]
#[1,] 2.385444

# d)
# 5% of significance
which(d>   qchisq(0.95,3))
   # 30 34 35 39 
   #  Observation 30 might be surprising.

# 2.5% of significance
which(d>   qchisq(0.975,3))
   # 34 35 39

# e)

#  We assumed in d) that the data are tri-variate normal.  We check this through a chi^-quantile plot for the Mahalanobis distances

dim(engine)
# 46  3
plot(qchisq( (1:46-0.5)/46,3),sort(d)) # 

abline(a=0,b=1)
  # clearly some deviation from the straight line,
  # especially in the central part (observations are too small)
  # and the boundary (observations are too large ==> outliers).
  # the d_i  have also some discrete (stepwise) character.


# f)

 # After Bonferroni correction:
which(d>   qchisq(1-0.05/46,3))

# none of the observations are identified as outliers
