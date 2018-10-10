# Sheather Ch 5 #1
# 7 March 2018

# Bill Collection: Predict LATE from BILL

# Data
# The first 48 observations in the data set are residential accounts
# and the second 48 are commercial accounts.
overdue <- read.table(header = TRUE, "http://www.stat.tamu.edu/~sheather/book/docs/datasets/overdue.txt"
                      )
# create type
overdue$type<-c(rep("residential", 48),rep("commercial", 48))

# comparisons to residential
overdue$type <- factor(overdue$type)
overdue$type <- relevel(overdue$type, "residential")

# EDA 
plot(LATE~BILL, data=overdue, pch=19)
points(LATE~BILL, data=subset(overdue, type=="commercial"), pch=19, col = "red")

# MODEL
# because we see different slopes for residential and commercial, the model
# will include an interaction
out.overdue<-lm(LATE ~ BILL + type + type:BILL, data = overdue, x = TRUE)

# Test Ho: no difference between residential and commercial accounts
reduced.overdue<-lm(LATE~BILL, data = overdue, x = TRUE)
anova(reduced.overdue, out.overdue)

# Conclusion:
# With an overall p-score of 2.2e-16, we conclude that there is a statistically significant
# difference between type of payment and the amount overdue the bill is. The data show that 
# those with residential addresses pay bills significantly later than those with commercial
# addresses. This was represented by the effect equation represented in the multiple linear 
# regression model.

# Analysis Strengths:
# One strength of the analysis was an accurate interaction plot. Due to the correct model,
# we observed the interaction between residential and commercial buildings. Additionally, we 
# saw an F-value as part of the ANOVA test of 2281.7, providing further evidence of a strong 
# relationship between the two variables.

# Analysis Weaknesses:
# The analysis showed a weakness in computing data for a study of parallel lines. In 
# this case, there was a strong interaction between the two variables. The analysis also
# didn't transform the data in any way.
