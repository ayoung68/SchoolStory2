species<-read.csv(header = TRUE, text = '
                Parent, Offspring
                 0.21, 0.1726
                 0.20, 0.1707
                 0.19, 0.1637
                 0.18, 0.1640
                 0.17, 0.1613
                 0.16, 0.1617
                 0.15, 0.1598')

#EDA
cor(species$Parent, species$Offspring)

plot(Offspring~Parent, data = species,
    xlab = "Diameter of Parent Pea",
    ylab = "Diameter of Offspring Pea")

library(ggplot2)
qplot(Parent, Offspring, data=species,
      geom="point",
      xlab="Diameter of Parent Pea",
      ylab="Diameter of Offspring Pea")


# The correlation coefficient, 0.9249, shows there is a strong association between the diameter 
# of the offspring pea and the diameter of parent pea. The data is very linear.

# The scatterplot confirms there is a strong association between the diameters of the offspring and
# parent peas.

# The response variable is the diameter of the offspring sweet peas. The explanatory variable is the 
# diameter of the parent pea.
    
# model: offspring = beta0 + beta1 parent + epsilon, epsilon~N(0,sigma2)
# Assumptions: The assumptions are met since the data appear very linear, with few outliers. There is a 
# strong relationship between the diameter of the parent and the offspring peas. There is no observed
# megaphone pattern.

#Fit Model/ Estimate the Model Parameters

out.species<-lm(Offspring~Parent, data=species)

# table of estimates and std errors
summary(out.species)

# pvalue for Ho is in summary
# 95% CI for beta1
confint(out.species)

# The value 0.21 for beta1 represents the estimated slope of the model. This is the change in y over the 
# change in x. For a one inch increase in parent sweet pea diameter, we expect an increase of 
# 0.21 inches in its offspring sweet pea diameter.

# The null hypothesis(Ho) is that there is no statistically significant inheritance effect in sweet pea
# parent and offspring diameters. The alternative hypothesis (Ha) is that there is a statistically significant inheritance 
# effect in sweet pea parent and offspring diameters.
# The t-test statistic value is 5.438. The reported p-value is 0.002852. 
# With a p-value of 0.002852, we reject the null and claim there is a statistically significant 
# inheritance effect in sweet pea parent and offspring diameters. In other terms, there is a very 
# strong relationship between the diameter of parent and offspring peas. 

# ANOVA
# The null hypothesis(H0) is that there is no difference between the diameters of parent pea and offspring pea.
# The alternative hypothesis(Ha) is that a difference exists between the diameters of parent peas 
# and offspring peas.
# The ANOVA F-test statistic is 29.58. The p-value is 0.002852.
# With a p-value of 0.002852 and an F-statstic of 29.58, there is a significant relationship between
# the diameter of parent and offspring peas.  
# In other terms, the F-statistic shows there is evidence of a strong relationship between the diameter 
# of parent and offspring peas.

# The 95% Confidence Interval
# According to the interval, in repeated sampling we are 95% confident that the interval (0.1107402, 0.309258) 
# will contain the true mean (B1) diameter of parent peas.
# In other terms, if we did this experiment many times, the interval (0.1107042, 0.3092598) will
# contain the mean value of parent pea diameter 95% of the time.
  
# create a graphic showing estimated line and uncertainty
qplot(Parent, Offspring, data=species,
      geom="smooth", formula+y~x, method = "lm", se = TRUE,
      xlab="Diameter of Parent Pea",
      ylab="Diameter of Offspring Pea")

# 95% CI of mean of all offspring diameter with Parent = 0.20
predict(out.species,newdata=data.frame(Parent=0.20), interval = "confidence")

# If we are interested in prediction...

#95% PI of a new offspring diameter with Parent = .18.
predict(out.species,newdata=data.frame(Parent=0.18), interval = "prediction")

library(ggplot2)

#graphic to demonstrate prediction performance
plot.df<-cbind(species,predict(out.species,interval="prediction"))
ggplot(plot.df,
       aes(Parent,Offspring))+
  xlab("Diameter of Parent Pea") +
  ylab("Diameter of Offspring Pea") +
  geom_point() +
  geom_line(aes(y=fit),color="royalblue")+
  geom_line(aes(y=lwr),color="red", linetype="dashed")+
  geom_line(aes(y=upr),color="red", linetype="dashed")

# With an R^2 error of 0.8265, the model predicts Offspring pea diameter with more than 80% 
# accuracy. The model is considered very accurate.

# Research Task and Data Features that Match Analysis Strengths:
# The plot adequately shows the standard error. The confidence interval predicts a fitted value
# of 0.1690286, with a lower limit of 0.1662211 and an upper limit of 0.178361. The prediction
# interval fits a value of 0.1648286, with an upper limit of 0.1704436 and a lower limit of
# 0.1592136. The analysis also gives a good "idea of uncertainty." The 95% CI shows that the
# true line is somewhere in that interval, which is shaded on the graph.

# One weakness of the analysis is that a very small amount of data was sampled and analyzed.
# If more data were available, the results could be more conclusive.
