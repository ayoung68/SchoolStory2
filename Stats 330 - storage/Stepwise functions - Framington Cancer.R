# Describe where the .csv file is located
fram<- read.csv("https://raw.githubusercontent.com/tykiww/projectpage/master/datasets/Model%20Selection/frmgham2.csv", header = TRUE)
# subset to participants that didn't have CHD
fram<-subset(fram,PERIOD==1 & PREVCHD==0)
# subset to risk factors under study
fram<-fram[,c(2:14,30)]

# Filter out missing observations
fram <- na.omit(fram)

# Create interpretable versions of all coded explanatory variables
fram$SEX <- factor(fram$SEX)
fram$SEX <- relevel(fram$SEX, ref = "1")

fram$CURSMOKE <- factor(fram$CURSMOKE)
fram$CURSMOKE <- relevel(fram$CURSMOKE, ref = "0")

fram$BPMEDS <- factor(fram$BPMEDS)
fram$BPMEDS <- relevel(fram$BPMEDS, ref = "0")

fram$DIABETES<- factor(fram$DIABETES)
fram$DIABETES <- relevel(fram$DIABETES, ref = "0")

fram$CVD<- factor(fram$CVD)
fram$CVD <- relevel(fram$CVD, ref = "0")

# Create train and test datasets
dim(fram) #3658 observations
set.seed(330)
train.ind<-sample(3000,1658)
fram.train<-fram[train.ind,]
fram.test<-fram[-train.ind,]



summary(fram.train)
summary(fram.test)


fram$SEX <- gsub(1, "M", fram$SEX)
fram$SEX <- gsub(2, "F", fram$SEX)

head(fram)

fram$CURSMOKE <- gsub(0, "No", fram$CURSMOKE)
fram$CURSMOKE <- gsub(1, "Yes", fram$CURSMOKE)

fram$BPMEDS <-gsub(0, "No", fram$BPMEDS)
fram$BPMEDS <- gsub(1, "Yes", fram$BPMEDS)
   
fram$DIABETES <-gsub(0, "No", fram$DIABETES)
fram$DIABETES <- gsub(1, "Yes", fram$DIABETES)

fram$educ <- gsub(1, "<HS", fram$educ)
fram$educ <- gsub(2, "HS", fram$educ)
fram$educ <-gsub(3, "some college", fram$educ)
fram$educ <- gsub(4, "college", fram$educ)

# Find the best predicting logistic regression model
# 'best' means subset of all possible explanatory variables

min.model<-glm(CVD ~ +1, data=fram.train, family=binomial)
biggest <- formula ( glm(CVD ~ .,data=fram.train, family=binomial) )

# Forward
out.CVD <-step(min.model, direction="forward", scope=biggest)

# Backward
out.2.CVD <- step(glm(CVD ~ .,data=fram.train,family=binomial)) 

# Stepwise
out3.CVD <- step(min.model, direction="both", scope = biggest)

summary(out.2.CVD)

# Use AIC which combines prediction performance and penalty
# backward elimination

# stepwise
min.model <- glm(CVD~ +1, data=fram.train, family = binomial)
biggest <- formula( glm(CVD ~ ., data = fram.train, family = binomial))

fram.out3 <- step(min.model, direction= "both", scope = biggest)
summary(fram.out3)

confint(fram.out3)
# As you age one year, we estimate an increase of 0.05 in the log odds of CVD holding all else constant (95% CI 0.044, 0.077)

# For a one unit change in total cholesterol, we estimate an increase of 0.004 in the log odds of CVD, holding all else constant (95% CI .003, .009).

# Women are 6.1% more likely to develop CVD than men are, holding all else constant. (95% CI 0.044, 0.077)
library(ROCR)
# Training data
train.pred <- prediction(predict(fram.out3, type = "response"), fram.train$CVD)
train.perf <- performance(train.pred, measure = "tpr", x.measure = "fpr")
plot(train.perf, xlab = "1-specificity", ylab = "sensitivity", main = "ROC Curve")
# Test data
test.pred<- prediction(predict(fram.out3, newdata = fram.test, type = "response"),
                       fram.test$CVD)
test.perf <- performance(test.pred,measure="tpr",x.measure="fpr")
plot(test.perf, add=TRUE,col="royalblue")

# The first explanatory variable that was dropped in the linear regression model was sysbp. 

# There are seven statistically significant variables in the stepwise regression model. They are
# diastolic blood pressure, cigarette packs per day. These are not statistically significant 
# because their AIC level showed they were not significant. These are the questions the doctor
# is not likely to ask first, since they are things that cannot be changed immediately or with
# one action.

# Data features: The ability to choose between the stepwise, backward, and forward directions allowed
# for much more accurate regression estimation.

# Weakness: There were many lurking variables that can have a confounding effect on each of these
# data values, specifically those of past personal and familial health history.