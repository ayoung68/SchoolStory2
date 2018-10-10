#Response variable 
#The response variable is how much over the speed limit a car went, according to the police.
#This is a quantitative variable.

#Explanatory variables: Following-too-closely, driving without a license, driving with expired license, failing to stop at a stop sign. 

#Going over the speed limit on city roads has the largest effect on predicting speeding. 

#EDA:  
hist(speed.last$speed)
tail(speed.last)

summary(speed.last$speed)

lapply(speed.last,class)
#Response variable is speed

#Create RF
library(randomForest)

#estimate model
out.speed<-randomForest(x=speed.train[,-18],y=speed.train$speed,
                        xtest = speed.test[,-18] , ytest = speed.test$speed,
                        replace=TRUE, #bootstrap sample
                        keep.forest=TRUE,
                        ntree =50,mtry=5,nodesize=25)

#Three most important exp
#predict speeding ticket
predict(out.speed, newdata = new.obs)
#According to the model, you would get a ticket if you had gone more than 14.11402 mph over the speed limit.

#find residual of MSE
out.speed

#Train MSE
sqrt(49.08094)

#test ME
sqrt(50.25)

#Model interpretability, how often were used explanatory variables
importance(out.speed)
varImpPlot(out.speed)




#The new.obs model shows that a speed of 9 mph over the speed limit would lead to a 
#speeding ticket.

#Research task and data features that mask analysis strengths.
#MSER, or SD, shows 7.006 (train) and 7.089 (test) and 8.31% variance explained (test). (+-)2 MSE show 95% accuracy of prediction error.

#Analysis weaknesses
#One analysis weakness is that overfit is possible.

#Challenge
#A similar question with a quantitative response variable that could be asked using the data
#at what time of day are you most likely to be caught and given a ticket? The quantitative 
#response variable is the hour when the drivers are pulled over.