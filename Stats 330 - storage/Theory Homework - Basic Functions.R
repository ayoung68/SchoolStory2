# Theory Homework: Basis Function Expansion
# 30 March 2019

Seymour <- read.table(header = TRUE, text = '
                    Yield Handlines
                    4     3
                    1     2
                    8     5
                    3     3
                    3     1
                    5     4
                    6     5
                    3     2
                    2     1
                    3     4
                    ')

plot(Yield~Handlines, data=Seymour)

cor(Seymour$Handlines, Seymour$Yield)

# Yield = f(Handlines) + ,  ∼ N(0, σ2)
plot(~Handlines+I(Handlines^2) + I(Handlines^3),
     data=Seymour)

x <- matrix(c(1,   3,   9,   
            1,   2,   4,
            1,   5,   25,
            1,   3,   9,
            1,   1,   1,
            1,   4,   16,
            1,   5,   25,
            1,   2,   4,
            1,   1,   1,
            1,   4,   16
            ), ncol = 3, byrow = TRUE)

y <- matrix(c(4,
              1,
              8,
              3,
              3,
              5,
              6, 
              3,
              2,
              3
              ), ncol = 1, byrow = TRUE)

# Compute B^ = (X'X)^(-1)X'Y
out.xy <- lm(formula = y1~x1)
summary(out.xy)
plot(out.xy)
library(MASS)

# Compute s^2 = (1/(n-3))*(Y-XB^)'*(Y-XB^)
1/(10-3)
(y%%xout.xy)^-1 * (y%%xout.xy)

# Test the Null: Ho: No curvature effect
# Alternative: Ha: Curvature effect exists
out2 <- lm(Yield~poly(Handlines^5), data =  Seymour, x = TRUE)
plot(out2)
summary(out2)
# With a p-value of 0.000765 and a t-statistic of 5.529, we reject the null and
# there is an effect from curvature on the plot.

# Grimshaw and Burwell data
NFL <- read.csv("http://grimshawville.byu.edu/NFLinSLC.txt", header = TRUE)

# Model Audience = f(TotalPoints) + ,  ∼ N(0, σ2)
NFL.out <- plot(Audience~TotalPoints, data = NFL)

# Approximate f with a 6th order polynomial 
out1 <- lm(Audience~TotalPoints+I(TotalPoints^2) + I(TotalPoints^3) + I(TotalPoints^4) + I(TotalPoints^5) + I(TotalPoints^6),
           data =NFL, x = TRUE)

# judge the fit from the plot
x.star<-seq(0,60,length=100)
yhat1 <- predict(out1,newdata=data.frame(TotalPoints=x.star))
lines(x.star, yhat1, col = "red3")

# judge the fit from the prediction performance
# median absolute prediction error
median(abs(predict(out1)))


# fitcubicsplines
library(splines)

out2 <- lm(Audience~ns(TotalPoints,5), data = NFL,x=TRUE)

# judge fit from plot                                     
plot(Audience~TotalPoints, data = NFL)
x.star<-seq(0,60,length=100)
yhat2 <- predict(out2,newdata=data.frame(TotalPoints=x.star))
lines(x.star, yhat2, col = "aquamarine1")

# median absolute prediction error
median(abs(predict(out2)))
