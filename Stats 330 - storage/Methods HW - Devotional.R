# Methods HW: LDS Temples
# 6 April 2018

# Data: "http://grimshawville.byu.edu/MormonTVaud.dat

temples <- read.csv(header = TRUE, text = "
State, Viewed, Temples
Alabama, 0, 1 
Alaska, 0, 1
Arizona, 0, 6
Arkansas, 0, 0
California, 1, 7
Colorado, 1, 2
Connecticut, 0, 1  
Delaware, 1, 0
Florida, 1, 2
Georgia, 1, 1
Hawaii, 1, 1
Idaho, 1, 5
Illinois, 1, 2
Indiana, 1, 1
Iowa, 0, 0
Kansas, 0, 0
Kentucky, 0, 1
Louisiana, 0, 1
Maine, 0, 0
Maryland, 1, 1
Massachusetts, 0, 1
Michigan, 0, 1
Minnesota, 0, 0
Mississippi, 0, 0
Missouri, 1, 0
Montana, 0, 1
Nebraska, 0, 0
Nevada, 0, 2
NewHampshire, 0, 0
NewJersey, 0, 0
NewMexico, 1, 1
NewYork, 1, 2
NorthCarolina, 1, 1
NorthDakota, 0, 0
Ohio, 1, 1
Oklahoma, 0, 1
Oregon, 1, 2
Pennsylvania, 1, 1
RhodeIsland, 0, 0
SouthCarolina, 0, 1
SouthDakota, 0, 0
Tennessee, 1, 2
Texas, 1, 4
Utah, 1, 17
Vermont, 0, 0
Virginia, 1, 1
Washington, 1, 3
WestVirginia, 0, 0
Wisconsin, 0, 0
Wyoming, 1, 1
")

# Fit the model: log(viewed) = (Bo1 + Bo2 Temples)
out.temples <- glm(Viewed~Temples, data = temples, family = "binomial")
summary(out.temples)

# 95% Confidence Interval
confint(out.temples)

# Bo: 0.8070
# Interpretation

# There is a statistically significant effect of Temples on Christmas Devotional viewership. For each additional Temple
# in a given state, we expect the number of viewers of the Christmas Devotional to decrease by 19.3%, holding all else constant. (95% CI: -76.1%, 62.1%)

# Test Ho: The number of Temples has no effect on the number of viewers who watch the Christmas Devotional.
# a = 0.05

summary(out.temples)
# P-value: 0.0226, t-statistic: 2.280
# With the given p-value and test statistic, we reject the null hypothesis and claim that the number of Temples
# has a significant effect on the number of viewers of the Christmas Devotional. This shows that the number of Temples
# in a state has a large effect on the number viewers of the annual devotional. More specifically, for each additional
# Temple, it is expected that the number of viewers in the state will decrease. This likely means that more non-members
# will watch the Christmas Devotional in areas where there are fewer Temples. This is intuitive, since areas with more 
# non-members than members typically have fewer Temples.

# Create ROC curve
library(ROCR)
temples.pred <- prediction(predict(out.temples, type = "response"), temples$Viewed)
temples.perf <- performance(temples.pred, measure="tpr",x.measur="fpr")
plot(temples.perf,xlab="1-specificity", ylab = "sensitivity", main = "ROC Curve")
abline(0, 1, col="cadetblue")

# AUC
performance(temples.pred, measure = "auc")

# The AUC shows that the prediction given for the future decrease in viewers
# will be accurate 80.515% of the time.

temples.sub <- temples[c(33),]

newdata <- mydata[c(1,5:10)]
temples.sub

confint(temples.sub)
