# LOL Skills

# Data: 2017 Worlds
source('http://grimshawville.byu.edu/eSports2017.R')


# Create train and test
dim(all.players) #3881 observations
set.seed(2319)(LOL.test$Time)
hist(LOL.train$Time)
train.ind<-sample(3881,3000)
LOL.train<-all.players[train.ind,]
LOL.test<-all.players[-train.ind,]

#ticket.test<-ticket.model[-train.rows,]

#confirm they are similar
summary(LOL.train)
summary(LOL.test)

hist(LOL.test$Time)

# EDA
hist(Kills, Win, data = LOL.train) 

# Table of Summary Statistics

# Plots of explanatory variable different for Win/Loss

# The explanatory variables are the number of kills, deaths, assists, and time of game.

# Model:

# response variable is Win = 1 if player's team wins

# logit(Win=1) = beta0 + beta1 Kills + beta2 Deaths + beta3 Assists + beta 4 Time

out.LOL <- glm(Win~Kills + Deaths + Assists + Time, data = LOL.train, family = "binomial")

summary(out.LOL)

# 95% CI on beta
confint(out.LOL)

# What does B^1 represent?
# There is a statistically significant effect of kills on winning. With a one increase in total kills, we 
# expect a 44% increase in odds of winning, holding all else constant. (95% CI, 38%, 50%)

# There is a statistically significant effect of time on winning. With a 1 minute increase in
# playing time, the odds of winning are expected to decrease 8%, holding all else constant. (95%, 8, 10).

# 95% CI on exp(beta)
exp(confint(out.LOL))[-1,]

# What does exp(B^1) represent?
# There is a statistically significant effect of kills on winning after adjusting for deaths, assists, and time. 
# With a one increase in kills, we estimate an increase in odds of winning by 55%, holding all else constant (95% CI, 47%, 65%). 

# There is a statistically significant effect of deaths on winning after adjusting for kills, assists, and time.
# With a one increase in deaths, we estimate a decrease in winning of 55%, holding all else constant. (95% CI, 52%, 59%).

# There is a statistically significant effect of assists on winning after adjusting for kills, deaths, and time.
# With a one increase in assists, we estimate an increase in odds of winning of 67%, holding all else constant. (95% CI, 60%, 75%).  

# There is a statistically significant effect of time on winning after adjusting for kills, deaths, and assists.
# For each additional minute played, we estimate a decrease of odds in winning of 8%, holding all else constant. (95% CI, 8%, 10%).

# demonstrate effect of Kills on P(Win) ... holding all else at median values
x.star<-data.frame(Kills=seq(0,10,length=100), Deaths=2,Assists=6,Time=34)
plot(x.star$Kills,predict(out.LOL,newdata=x.star,type="response"), 
     type = 'l',
     ylim=c(0,1),
     xlab="Kills",
     ylab="P(Win), all other factors at median")

x.star.Deaths<-data.frame(Deaths=seq(2,10,length=100), Kills=2,Assists=6,Time=34)
plot(x.star.Deaths$Deaths,predict(out.LOL,newdata=x.star.Deaths,type="response"), 
     type = 'l',
     ylim=c(0,1),
     xlab="Deaths",
     ylab="P(Win), all other factors at median")

x.star.Assists<-data.frame(Assists=seq(3,13,length=100), Deaths=2,Kills=2,Time=34)
plot(x.star.Assists$Assists,predict(out.LOL,newdata=x.star.Assists,type="response"), 
     type = 'l',
     ylim=c(0,1),
     xlab="Deaths",
     ylab="P(Win), all other factors at median")

x.star.Time<-data.frame(Time=seq(20,60,length=100), Deaths=2,Kills=2,Assists=6)
plot(x.star.Time$Time,predict(out.LOL,newdata=x.star.Time,type="response"), 
     type = 'l',
     ylim=c(0,1),
     xlab="Time",
     ylab="P(Win), all other factors at median")

# Z-test: Kills
summary(out.LOL)

# Null: Ho: There is no effect of a player's kills on winning.
# Alternative: Ha: There is an effect of a player's kills on winning.

# Z-value: 14.726
# P-value: P < 0.0003
# Based on the z-value and p-value, we reject the null hypothesis and claim that there is
# a significant effect of kills on winning. In other words, according to the test, the number 
# of kills has a significant effect on one's winning percentage. 

# Z-test: Deaths

# Null: Ho: There is no effect of a player's deaths on winning.
# Alternative: Ha: There is an effect of a player's deaths on winning.

# Z-value: -19.388
# P-value: P < 0.0003
# Based on the z-value and p-value, we reject the null hypothesis and claim that there is
# a significant effect of deaths on winning. In other words, according to the test, the number 
# of deaths has a significant effect on one's winning percentage. 

# Z-test: Assists

# Null: Ho: There is no effect of a player's assists on winning.
# Alternative: Ha: There is an effect of a player's assists on winning.

# Z-value: 22.957
# P-value: P < 0.0003
# Based on the z-value and p-value, we reject the null hypothesis and claim that there is
# a significant effect of assists on winning. In other words, according to the test, the number 
# of assists has a significant effect on one's winning percentage. 

# Z-test: Time

# Null:Ho: There is no effect of game time on winning.
# Alternative: Ha: There is an effect of game time on winning.

# Z-value: -10.437
# P-value: P < 0.0003
# Based on the z-value and p-value, we reject the null hypothesis and claim that there is
# a significant effect of game time on winning. In other words, according to the test, the duration
# of each game has a significant effect on one's winning percentage. 

# According to the data, there is an advantage of playing aggressively, since a shorter game time, more
# kills, more assists, and fewer deaths each contribute to the odds of winning.

# Estimate for Faker and Ambition
# Faker
  predict(out.LOL,newdata=data.frame(Kills=2,Deaths=3,Assists=8,Time=40), type = "response")
faker.logit <- predict(out.LOL, newdata=data.frame(Kills=2,Deaths=3,Assists=8,Time=40), type="link", se.fit = TRUE)
logit.L.faker <- faker.logit$fit - 1.96*faker.logit$se.fit
logit.U.faker <- faker.logit$fit + 1.96*faker.logit$se.fit
faker.phat.L <-1/(1-exp(-logit.L.faker))
faker.phat.U <-1/(1+exp(-logit.L.faker))
faker.phat.L
faker.phat.U 

# Ambition
predict(out.LOL,newdata=data.frame(Kills=2,Deaths=2,Assists=14,Time=40), type = "response")
ambition.logit <- predict(out.LOL, newdata=data.frame(Kills=2,Deaths=2,Assists=14,Time=40), type="link", se.fit = TRUE)
logit.L.ambition <- ambition.logit$fit - 1.96*ambition.logit$se.fit
logit.U.ambition <- ambition.logit$fit + 1.96*ambition.logit$se.fit
ambition.phat.L <-1/(1-exp(-logit.L.ambition))
ambition.phat.U <-1/(1+exp(-logit.L.ambition))
ambition.phat.L
ambition.phat.U

# Compute ROC and AUC
# Classification
# Choosing 0.5 as cutoff
yhat.train <- as.numeric(predict(out.LOL, type = "response") > 0.5)
table(yhat.train, LOL.train$Win)
# sensitivity
1742/(171+1742)
# Specificity
877/(877+210)

# Choosing 0.4 as cutoff
yhat.train <- as.numeric(predict(out.LOL, type = "response") > 0.4)
table(yhat.train, LOL.train$Win)

# ROC curve
library(ROCR)
train.pred <- prediction(predict(out.LOL, type = "response"), LOL.train$Win)
train.perf<-performance(train.pred,
                        measure="tpr", x.measure = "fpr")
plot(train.perf,xlab="1-specificity", ylab="sensitivity",main="ROC curve")

# AUC
performance(train.pred, measure = "auc")

# Test dataset
# Choosing 0.5 as cutoff
yhat.test <- as.numeric(predict(out.LOL, newdata = LOL.test, type = "response") > 0.5)
table(yhat.test, LOL.test$Win)
# Sensitivity
496/(496+62)
# Specificity
273/(273+50)

# Compare to test (out of sample validation)
# ROC curve
test.pred<-prediction(predict(out.LOL,newdata=LOL.test,type="response"), LOL.test$Win)
test.perf<-performance(test.pred,
                        measure="tpr", x.measure = "fpr")

plot(test.perf, col = "blue", add=TRUE)

# The prediction is worse for the test dataset since its sensitivity is lower. This means
# that we were less likely to pick the winner when the player won, rather than what happened 
# in the train dataset, where the dataset was more accurate in predicting the winner when the 
# player actually won.

abline(0,1,col="gray")

# AUC line
performance(test.pred, measure = "auc")

# Research task and data features:
# One feature of the data was the ROC curve. This helped to assess the classification of the 
# prediction of the test and train datasets. The high ROC value for both the train (.911) and
# test (.889) datasets for sensitivity showed that both datasets were very effective in estimating 
# the winner when the player actually won. Additionally, the specificity values (.810, train) and 
# (.845, test) showed how accurate the estimates were in predicting the loser when the player actually 
# lost. These tests were best done using R, in which thousands of cutoff points were plotted and calculated,
# which gives a quicker and more accurate model of the ROC curve.

# Analysis weaknesses:
# The weakness in the analysis was that the expression of the data in terms of log odds
# made it difficult to be understood by non-statisticians. This can be undone by untransforming the
# data, though it may alter the plot of the data, requiring explanation.

