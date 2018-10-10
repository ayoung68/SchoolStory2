diamonds <- read.table(header = FALSE, "https://www.amstat.org/publications/jse/v9n2/4Cdata.txt")

names(diamonds)<-c("Carat", "Color", "Clarity", "Cert", "Price")
                  
plot(Price~Carat, data = diamonds,
     xlab = "Price (in Singapore$)",
     ylab = "Carat (grams)")

#Price(diamonds)<-lnPrice

#Carat(diamonds)<-lnCarat

diamonds$lnPrice <- log(diamonds$Price)

diamonds$lnCarat <- log(diamonds$Carat)
                                                     
plot(lnPrice~lnCarat, data = diamonds,
     xlab = "Price (in Singapore$)",
     ylab = "Carat (grams)")

# Based on the scatterplot, the log function appears better for linear model assumptions.
# The original scatterplot does not have a very constant variance and is less linear. 
# The ln(Carat) and ln(Price) plot appears more linear and has more constant variance.

# The response variable is the log price of a diamond, measured in Singapore $. The explanatory
# variable is the log carat weight.

# The model is a simpler linear regression model.

#Fit Model/ Estimate the Model Parameters

out.diamonds<-lm(Price~Carat, data=diamonds)

# table of estimates and std errors
summary(out.diamonds)

# The value B^1 = 1.53726 represents that for a 1 percent increase in carat size,
# we estimate the price of the diamond will increase by 1.54%.

# model: ln(Price) = beta0 + beta1 log(Carat) + epsilon, epsilon~N(0,sigma2)
out.diamonds<-lm(log(Price)~log(Carat),data=diamonds)
# careful interpreting coefficients!
summary(out.diamonds)

anova(out.diamonds)

confint(out.diamonds)

#95% CI (11146.085, 12051.68)
# If the experiment were performed many times, the interval (11146.085, 12051.68) will contain the mean price 
# of diamond 95% of the time.


# for a jewelry store owner who wants to be convinced model is closely
# estimated
# graphic of uncertainty with estimated model.
qplot(log(Carat), log(Price), data=diamonds,
      geom = "smooth", formula+y~x, method = "lm", se = TRUE,
      xlab = "Price (in Singapore$)",
      ylab = "Carat (grams)")
                                                    
# predict for a 1 carat diamond
predict(out.diamonds, newdata=data.frame(Carat=1), interval="prediction")
# in price
exp( predict(out.diamonds, newdata=data.frame(Carat=1), interval="prediction") )

#graphic to demonstrate prediction performance
plot.df<-cbind(diamonds,predict(out.diamonds,interval="prediction"))
ggplot(plot.df,
       aes(Price,Carat))+
  xlab("Price (in Singapore$)") +
  ylab("Carat (grams)") +
  geom_point() +
  geom_line(aes(y=fit),color="royalblue")+
  geom_line(aes(y=lwr),color="red", linetype="dashed")+
  geom_line(aes(y=upr),color="red", linetype="dashed") 

# The Real R^2(not the fake R^2)
1-((diamonds$Price - exp(predict(out.diamonds)))^2)/(diamonds$Price - mean(diamonds$Price)^2) 

# r^2 = 0.9574
# The model predicts Price very well. With an R^2 value close to 1, the model is very accurate in 
# predicting Price.

# summary statistics of absolute prediction error
summary( abs( diamonds$Price - exp( predict(out.diamonds))) )

# The model predicts really well when the value falls around the median, in which the price is 
# 602.57 ((untransformed)Singapore $). The model predicts poorly when the 
# diamond size is much larger, and the price is much greater, such as the maximum value, when
# an overestimate of 6658.77, meaning the mistake would be very costly. The model could be improved 
# in this instance by gathering more data relative to the larger diamond size. This would give a more 
# accurate model prediction.

Par(mfrow=c(1,2))
plot(diamonds)
plot (out.diamonds)
Par(mfrow=c(1,1))

# Research Task and Data Features that Match Analysis Strengths:
# One strength of the analysis is the prediction interval. It predicts that the most popular
# one carat diamond would be purchased for around 9,207.31 (Singapore $). Another strength is
# the ability to do a log transformation of the data. It gives a more linear and understandable 
# result. It also gives a good estimation of uncertainty for the store manager.

# Analysis Weaknesses:
# One weakness of the analysis was the inaccuracy in estimating the prices for large diamonds.

# CHALLENGE: 