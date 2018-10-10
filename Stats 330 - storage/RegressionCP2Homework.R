# Regression
# Scottish Hill Races

# Accessed 21 February 2018
# Data: "http://www.statsci.org//data/general/hills.txt"

hills <- read.table(header = TRUE, "http://www.statsci.org//data/general/hills.txt")

# EDA: Explore scatterplots of Time and the explanatory variables, using the identify 
# function to find races with unusual observations.
plot(Time~Distance, data = hills,
     xlab = "Race Distance",
     ylab = "Time")

identify(hills$Distance, hills$Time, hills$Race)

hills[c(7, 11, 17, 33, 35),]
library(knitr)
kable(hills[c(7, 11, 17, 33, 35),])

plot(Time~Climb, data = hills,
     xlab = "Hill Climb",
     ylab = "Time")

identify(hills$Climb, hills$Time, hills$Race)

hills[c(7, 11, 31, 33, 35),]

kable(hills[c(7, 11, 31, 33, 35),])

# Analysis
# Compute Regression Diagnostics

# Fit model using all obs.
out.hills<-lm(Time~Distance+Climb, data = hills)

# Compute leverage for Ben Nevis race
leverage.hills<-lm.influence(out.hills)$hat
subset(leverage.hills,hills$race=="BenNevis")

# leverage cutoffs that show how influential: 2*(p+1)/n 
2*(2+1)/35


# Compute Cook's distance for Moffat Chase race
cd.hills<-cooks.distance (out.hills)
subset(cd.hills,hills$Race=="MoffatChase")

# Cook's Distance cutoffs that show how influential 4/(n-(p+1))
4/(35-(2+1))



par(mfrow=c(1,2))
plot(hills$Distance,hills$Time)
points(hills$Distance[35],hills$Time[35],col="red",pch=19)
plot(hills$Climb,hills$Time)
points(hills$Climb[35],hills$Time[35],col="red",pch=19)
par(mfrow=c(1,1))

# compute R-standardized residual for Cairn Table race
R.hills <-rstudent(out.hills)
subset(R.hills,hills$Race=="CairnTable")

par(mfrow=c(1,2))
plot(hills$Distance,hills$Time)
points(hills$Distance[35],hills$Time[35],col="red",pch=19)
plot(hills$Climb,hills$Time)
points(hills$Climb[35],hills$Time[35],col="red",pch=19)
par(mfrow=c(1,1))

# Validate the normality assumption 
hist(R.hills)

# Accordlity test is 0.03657. With a P-value only slightly less than 
# the 0.05 value used for siing to the histogram of the residuals, there is no evidence of the data violating 
# the normality assumption, due to few influential observations.

# Compute the K-S normality test
ks.test(R.hills, "pnorm")

# The p-value for the normagnificance, we do not reject the null and assume the data show
# a normal distribution of residuals; the data is considered normal.

# Kildcon Hill time
subset(R.hills, hills$Race== "KildconHill")
2*(1-pnorm(subset(R.hills, hills$Race=="KildconHill")))

# With a p-value 0.8371985, I would not consider the observation at Kildcon Hill an
# outlier. Its p-value much greater than 0.05 shows that it appears to be a normal 
# observation.

# Moffat Chase hill
# Leverage cutoff 2*(p+1)/n
lev <- 2*(2+1)/35
# Cook's distance cutoff 4/(n-(p+1))
codis <- 4/(35-(2+1))

subset(leverage.hills,hills$Race=="MoffatChase") >= lev
subset(cd.hills,hills$Race=="MoffatChase") >= codis

#Influential to observation - plot
par(mfrow=c(1,2))
plot(x=hills$Distance, y=hills$Time)
points(hills$Distance[35], hills$Time[35], col= "red", pch=19)

plot(x=hills$Climb, y=hills$Time)
points(hills$Climb[35], hills$Time[35], col= "red", pch=19)
par(mfrow=c(1,1))

# The race at Moffat Chase hill is an influential observation. Since its leverage
# value is greater than the cutoff, the value is strong and influential in a 
# good way in determining the record time. The plot shows that both the distance and
# climb are influential to the remaining observations and their time. The codistance value 
# shows no strong influence towards the observations.

# Lairig Ghru hill

subset(leverage.hills,hills$Race=="LairigGhru") >= lev
subset(cd.hills,hills$Race=="LairigGhru") >= codis

#Influential to observation - plot
par(mfrow=c(1,2))
plot(x=hills$Distance, y=hills$Time)
points(hills$Distance[11], hills$Time[11], col= "red", pch=19)

plot(x=hills$Climb, y=hills$Time)
points(hills$Climb[11], hills$Time[11], col= "red", pch=19)
par(mfrow=c(1,1))

# The race at Lairig Ghru is an influential observation. This is due to the leverage 
# value, which is higher than the leverage cutoff, as well as the codistance value, which 
# is above the codistance cutoff value. The plot also shows the distance to be an influential 
# value, and the climb to be an outlier in relation to the record. Because of this, the race
# at Lairig Ghru is influential in a bad way toward the data.

# Cow Hill race
subset(R.hills, hills$Race== "CowHill")
2*(1-pnorm(subset(R.hills, hills$Race=="CowHill")))

# With  a p-value of 0.7529833, the observation at Cow Hill is not considered an outlier.
# At a significance level of 0.05, the value is not significant as an outlier.

# Knock Hill: Rumor or data?
subset(leverage.hills,hills$Race=="KnockHill") >= lev
subset(cd.hills,hills$Race=="KnockHill") >= codis

#Influential to observation - plot
par(mfrow=c(1,2))
plot(x=hills$Distance, y=hills$Time)
points(hills$Distance[18], hills$Time[18], col= "red", pch=19)

plot(x=hills$Climb, y=hills$Time)
points(hills$Climb[18], hills$Time[18], col= "red", pch=19)
par(mfrow=c(1,1))

# Based on the investigation, the Knock Hill race record is based on data. The leverage value was
# lower than the cutoff value. However, due to the codistance value, which was greater than the 
# codistance cutoff, the value is a good influence on the data. The value is credible and is very 
# feasible, rather than being a rumor.
