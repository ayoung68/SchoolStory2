# Sheather Ch. 6 #5
# 9 March 2018

pga <- read.csv("http://www.stat.tamu.edu/~sheather/book/docs/datasets/pgatour2006.csv")

pganew<-pga[-178,c(-2,-4,-11)]

# Transform the y-variable using log transformation
pganew$lnPrizeMoney <- log(pganew$PrizeMoney)

cor(pganew$lnPrizeMoney, pganew$DrivingAccuracy)
plot(lnPrizeMoney~DrivingAccuracy, data=pganew,
     xlab = "Driving Accuracy",
     ylab = "lnPrize Money")
identify(y=pganew$lnPrizeMoney,x= pganew$DrivingAccuracy, labels = pganew$Name)

# Possible outliers
pganew[c(28, 61, 92),]
library(knitr)
kable(pganew[c(28, 61, 92),])

cor(pganew$lnPrizeMoney, pganew$GIR)
plot(lnPrizeMoney~GIR, data=pganew,
     xlab = "GIR",
     ylab = "lnPrize Money")
identify(y=pganew$lnPrizeMoney,x= pganew$GIR, labels = pganew$Name)
pganew[c(1, 83),]

cor(pganew$lnPrizeMoney, pganew$PuttingAverage)
plot(lnPrizeMoney~PuttingAverage, data=pganew,
     xlab = "Putting Average",
     ylab = "lnPrize Money")
identify(y=pganew$lnPrizeMoney,x= pganew$PuttingAverage, labels = pganew$Name)
pganew[c(45, 184),]

cor(pganew$lnPrizeMoney, pganew$BirdieConversion)
plot(lnPrizeMoney~BirdieConversion, data=pganew,
     xlab = "Birdie Conversion",
     ylab = "lnPrize Money")
identify(y=pganew$lnPrizeMoney,x= pganew$BirdieConversion, labels = pganew$Name)
pganew[c(40, 45, 142),]

cor(pganew$lnPrizeMoney, pganew$SandSaves)
plot(lnPrizeMoney~SandSaves, data=pganew,
     xlab = "Sand Saves",
     ylab = "lnPrize Money")
identify(y=pganew$lnPrizeMoney,x= pganew$SandSaves, labels = pganew$Name)
pganew[c(36, 142),]

cor(pganew$lnPrizeMoney, pganew$Scrambling)
plot(lnPrizeMoney~Scrambling, data=pganew,
     xlab = "Scrambling",
     ylab = "Prize Money")
identify(y=pganew$lnPrizeMoney,x= pganew$Scrambling, labels = pganew$Name)
pganew[c(36, 40),]

cor(pganew$lnPrizeMoney, pganew$PuttsPerRound)
plot(lnPrizeMoney~PuttsPerRound, data=pganew,
     xlab = "Putts Per Round",
     ylab = "lnPrize Money")
identify(y=pganew$lnPrizeMoney,x= pganew$PuttsPerRound, labels = pganew$Name)

# Fit the model
out.pganew <- lm(lnPrizeMoney~DrivingAccuracy+GIR+PuttingAverage+BirdieConversion+SandSaves+Scrambling+PuttsPerRound, data = pganew, x=TRUE, y=TRUE)

summary(out.pganew)

# Leverage cutoff 2*(p+1)/n
lev <- 2*(8+1)/196
# Cook's Distance cutoffs that show how influential 4/(n-(p+1))
codis <- 4/(196-(8+1))

# Compute Cook's distance for Phil Mickelson
cd.pganew<-cooks.distance (out.pganew)
subset(cd.pganew,pganew$Name=="Phil Mickelson") >= codis

# Compute Leverage for Phil Mickelson
leverage.pganew<-lm.influence(out.pganew)$hat
subset(leverage.pganew,pganew$Name=="Phil Mickelson") >= lev

subset(leverage.pganew,pganew$Name=="Geoff Ogilvy") <= lev
pga[lev<leverage.pganew,]
pga[codis<cd.pganew,]

R.pganew <-rstudent(out.pganew)

hist(R.pganew)

ks.test(R.pganew, "pnorm")
#P-value of 0.309 = normal histogram data

new.obs<-pganew[178,-1:-2]

predict(out.pganew, newdata= new.obs)

kable(pganew[c(63),])
