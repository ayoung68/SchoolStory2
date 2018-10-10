#Homework
# Simple Linear Regression
# Accessed 12 Feb. 2018

# Chapter 2
# #1(c)
playbill <- read.csv(header = TRUE, "http://www.stat.tamu.edu/~sheather/book/docs/datasets/playbill.csv" )

out.playbill <- lm(CurrentWeek~LastWeek, data = playbill)

# conf int of gross box office 
confint(out.playbill)

# predict off of a $400000 week last week
predict(out.playbill, newdata=data.frame(LastWeek=400000), interval="prediction")

# #2, (a)
indicators <- read.table(header = TRUE, "http://www.stat.tamu.edu/~sheather/book/docs/datasets/indicators.txt")

out.indicators <- lm(PriceChange~LoanPaymentsOverdue, data = indicators)

# conf int for slope of regression model
confint(out.indicators)

# (b)
# conf int E(Y|X=4)
predict(out.indicators, newdata=data.frame(LoanPaymentsOverdue=4), interval="confidence")

# #3 (a)
invoices <- read.table(header = TRUE, "http://www.stat.tamu.edu/~sheather/book/docs/datasets/invoices.txt")

out.invoices <- lm(Time~Invoices, data = invoices)

# 95% conf int for start-up time
confint(out.invoices)

# (c)
# 95% PI for time taken to process 130 invoices
predict(out.invoices, newdata=data.frame(Invoices=130), interval="prediction")

# Chapter 3
# Part A

AdRevenue <- read.csv(header = TRUE, "http://www.stat.tamu.edu/~sheather/book/docs/datasets/AdRevenue.csv")

out.AdRevenue <- lm(AdRevenue~Circulation, data = AdRevenue)

plot(AdRevenue~Circulation, data = AdRevenue,
     xlab = "Circulation",
     ylab = "Gross Adv. Avg. Revenue/Pg.")

AdRevenue$lnAdRevenue <- log(AdRevenue$AdRevenue)

AdRevenue$lnCirculation <- log(AdRevenue$Circulation)

# Transformed simple linear regression model 
plot(lnAdRevenue~lnCirculation, data = AdRevenue,
     xlab = "Circulation",
     ylab = "Gross Adv. Avg. Revenue/Pg.")

# 95% prediction interval with 0.5 million people
predict(out.AdRevenue, newdata=data.frame(Circulation = 0.5), interval="prediction")

# 95% prediction interval with 20 million people
predict(out.AdRevenue, newdata=data.frame(Circulation = 20), interval="prediction")
