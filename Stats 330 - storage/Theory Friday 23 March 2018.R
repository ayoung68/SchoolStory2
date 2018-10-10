# Silverman's Motorcycle Data
# accel = head acceleration (g)
# times = time after impact (milliseconds)
library(MASS)
head(mcycle)

# plot dat (notice the nonlinear effect)
plot(accel~times, data=mcycle)

# polynomial approximation
out1 <- lm(accel~times+I(times^2) + I(times^3)  + I(times^4) + I(times^5),
           data=mcycle, x=TRUE)

# judge the fit from the plot
x.star<-seq(0,60,length=100)
yhat1 <- predict(out1,newdata=data.frame(times=x.star))
lines(x.star,yhat1, col = "red3")

# judge the fit from the prediction performance
# median absolute prediction error
median(abs(predict(out1)))

library(car)
#vf(count1)

#(~out2$x)[,2]+out $F

plot(~times+I(times^2) + I(times^3)  + I(times^4) + I(times^5),
   data=mcycle)

# instead of computing the basic function expansion ourselves,
# use one that is orthonormal
out2 <- lm(accel~poly(times, 5), data = mcycle, x = TRUE)

head(~out2$x)         
plot(~out2$x[,2]+out2$x[,3]+out2$x[4]+out2$x[,5]+out2$x[,6])
  

#[,2]

# judge fit from plot                                     
plot(accel~times,data=mcycle)
x.star<-seq(0,60,length=100)
yhat2<-predict(out2,newdata=data.frame(times=x.star))
lines(x.star,yhat2,col="red")

# judge the fit from the prediction performance
# median absolute prediciton error
median(abs(predict(out2)))
          
# fitcubicsplines
library(splines)

out3 <- lm(accel~ns(times,5), data = mcycle,x=TRUE)

# judge fit from plot                                     
plot(accel~times,data=mcycle)
x.star<-seq(0,60,length=100)
yhat3<-predict(out3,newdata=data.frame(times=x.star))
lines(x.star,yhat2,col="red")