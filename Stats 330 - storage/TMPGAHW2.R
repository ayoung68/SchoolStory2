#Homework: Tree Models
#Accessed 29 Jan 2018

#1. The response variable is the prize money. It is a quantitative variable.
#The quantitative explanatory variables are the average driving distance, driving accuracy, greens in regulation, putting
#average, birdie conversion, sand saves, scrambling, bounce back, and putts per round. 
#There are no categorical explanatory variables.
pga <- read.csv("http://www.stat.tamu.edu/~sheather/book/docs/datasets/pgatour2006.csv")

pganew<-pga[-178,c(-1:-2)]


# SRS without replacement of goods
#all.good<-subset(ticket.last,Ticket=="FALSE")
#n.good<-dim(all.good)[1]
#set.seed(12)
#rows.good<-sample(n.good,n.bad)
#sample.good<-all
# Combine
#ticket.model<-rbind(all.bad,sample.good)

set.seed(12)
train.rows<-sample(195, 150)
pganew.train<-pganew[train.rows,]
pganew.test<-pganew[-train.rows,]
#After observing the training and test datasets, it appears the datasets are similar.
#The medians, minimum, and maximum values are very similar.

summary(pganew.train$PrizeMoney)
summary(pganew.test$PrizeMoney)
library(randomForest)

out.pga<-randomForest(x=pganew.train[, -1], y=pganew.train$PrizeMoney,
                         xtest = pganew.test[,-1], ytest = pganew.test$PrizeMoney, 
                         ntree = 200,
                         replace = TRUE, nodesize = 5, keep.forest = TRUE,
                         mtry=3)

out.pga

sqrt(1611387021)

sqrt(1446050607)
#The randomForest model was fitted on the data successfully. The training RMSE is 40,142.09
#and the test RMSE is 38026.97.


new.obs<-pga[178,-1:-2]

predict(out.pga, newdata= new.obs)
#After the prediction, a golfer with Tiger Woods' 2006 skills would earn $170,226.3 in prize money.

importance(out.pga)
varImpPlot(out.pga)
#Based on the RF model, the three most important explanatory variables in the estimation
#are BirdieConversion, Greens in Regulation, and BounceBack percentage.