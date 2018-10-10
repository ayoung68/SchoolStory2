avalanche <- read.csv("reco-download-dataavalancheGood.txt", header=TRUE)
snow <- read.csv("~/Stats 330/New folder/otheravalanche.csv", header=TRUE)

avalanche$Date <- as.Date(avalanche$Date, "%m/%d/%Y") 
avalanche$MonthYr <- format(as.Date(avalanche$Date), "%Y-%m")  

snow$DATE <- as.Date(paste(snow$DATE,"-01",sep="")) 
snow$DATE <- as.Date(snow$DATE, "%Y-%m-%d") 
snow$MonthYr <- format(as.Date(snow$DATE), "%Y-%m") 

month.count <- aggregate(ï..Nid ~ MonthYr, FUN=length, data=avalanche)
snow$avalanche.count <- 0
snow$avalanche.count[snow$MonthYr %in% month.count$MonthYr] <- month.count$ï..Nid[month.count$MonthYr %in% snow$MonthYr]

snow <- snow[grepl("-01", snow$MonthYr) |
               grepl("-02", snow$MonthYr) |
               grepl("-03", snow$MonthYr) |
               grepl("-12", snow$MonthYr), c(3:8)]

names(snow) <- c("date",  "dt32", "snowfall", "tmin", "monthyr", "av.count")



december <- snow[grepl("-12", snow$monthyr),] 
summary(december)

# EDA
plot(av.count~snowfall,data=snow)
# This contradicts the normality assumption of regression because the data aren't normally 
# distributed; there exists some collinearity issues based on the many values close to the x-axis.
# Due to collinearity, an exponential model should be fit on the avalanche data.

plot(av.count~dt32,data=snow)
# For avalanches I would recommend a log transformation in order to balance 
# the slight curvature. This would reduce the multiplicative effect.

plot(av.count~tmin,data=snow)

# The response variable is the number of avalanches. The explanatory variables are the amount of snowfall (SNOW),
# days when the minimum temperature dropped below 32 degrees (DT32), and the minimum temperature on those
# days(TMIN).

# Model
# Y-Poisson(exp(beta0 + beta1 SNOW + Beta 2 TMIN + beta3 dt32))
out.avalanche<-glm(av.count~snowfall~tmin~dt32, data = snow, family = "poisson")
summary(out.avalanche)

# for interpretation
exp(coeff(out.avalanche)[-1])

# The value B^1 represents that for a one inch increase in snowfall, we expect a ** percent decrease in the mean
# number of monthly avalanches occurring, holding all else constant. (95% CI: **, **)

# The value exp(B^1) represents that for a one inch increase in snowfall, we expect the mean number of avalanches to decrease by 
# by 2.4 percent, holding all else constant (95% CI: 1.7%, 3.0%)

# There is a statistically significant effect of snowfall on the monthly mean # of avalanches. For an additional 1 inch of monthly cumulative snowfall, we estimate the mean number of avalanches will decrease by 2.4%
# holding all else constant. (95% CI, **, **) This may be surprising; snowfall adding up leads to more protection from avlanches, whereas spontaneous snow storms
# lead to excess buildup.

## There is a statistically significant effect of temperature on the monthly mean # of avalanches. For an additional 1 degree decrease in temperature, we estimate the mean number of avalanches will decrease by **%
# holding all else constant. (95% CI, **, **) 

# There is a statistically significant effect of minimum temperature on the monthly mean # of avalanches. For an additional 1 inch of monthly cumulative snowfall, we estimate the mean number of avalanches will decrease by %
# holding all else constant. (95% CI, **, **)

# 95% CI
exp(confint(out.avalanche[-1,]))

# Test Ho: Temperature has no effect on monthly mean # of avalanches
red.avalanche <- glm(Avalanches~snow~tmin~dt32, data = snow, family = "poisson")
anova(red.avalanche, out.avalanche, test = "Chisq")

# P-Value: .1094
# According to the chi-square test result, we fail to reject the null and claim that temperature 
# does not have a significant effect on the number of avalanches.

# Test Ho: Snowfall has no effect on monthly mean # of avalanches

# Z-test
# P-Value: p < .0001 
# According to the p-value and the z-test result, we reject the null and claim that snowfall has a statistically 
# significant effect on the number of avalanches.


# Predict mean monthly avalanches for the "typical December"
summary(subset[december, my.month==12])
predict(out.avalanche, newdata=data.frame(snow=12.2, tmin=22.9, dt32=28.0), type = "response")

# Research Task and data features that match analysis strengths:
# One strength of the data was the amount of data available. With so much data, the regression study was more effective. 
# Additionally, the confidence interval gave an accurate estimate of how the additional snowfall would lead to a 
# decrease in the number of avalanches. Being able to explain with an additional sentence why the data
# portrayed this characteristic helped a non-statistician understand what the analysis showed.

# Analysis weaknesses: 
# One potential weakness of the data was that there was no monthly or weekly measurement of snowfall. We only
# looked at daily accumulations. Additionally, there was no specification of summer months vs. winter months. 
# If there was a clarification, it would be easier to understand why the data behaved the way it did in the 
# traditional snow season.