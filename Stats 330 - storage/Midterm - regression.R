tv <- read.table(header = TRUE, text= '
            Year TVaud Tigers AtBreak DNP Bullpen
            2003  200.9 1 0.272 6 11
            2004  235.6 2 0.483 1 10
            2005  436.6 1 0.488 4 10
            2006  290.9 3 0.670 7 10
            2007  305.1 5 0.605 2 10  
            2008  223.3 1 0.500 4 11
            2009  302.1 5 0.552 19  13
            2010  260.8 3 0.558 10  16  
            2011  201.9 5 0.533 15  13
            2012  296.2 3 0.512 6 9  
            2013  374.7 6 0.553 8 14
            2014  279.9 4 0.582 17  12  
            2015  190.4 4 0.500 8 12
            2016  122.1 1 0.517 11  14
            2017  111.2 2 0.448 7  11')

summary(tv)

# The Tigers All-Stars in last year's game were Michael Fulmer (pitcher) and Justin Upton (outfielder). 

# Compute correlation coefficients and SDs
cor(tv$TVaud, tv$Tigers)
cor(tv$TVaud, tv$AtBreak)
cor(tv$TVaud, tv$DNP)
cor(tv$TVaud, tv$Bullpen)

sd(tv$Tigers)
sd(tv$AtBreak)
sd(tv$DNP)
sd(tv$Bullpen)

#Fit the model
out.tv <- lm(TVaud~Tigers+AtBreak+DNP+Bullpen, data = tv)

# Compute leverage for Year 2010
leverage.tv<-lm.influence(out.tv)$hat
subset(leverage.tv,tv$Year=="2010")

# leverage cutoffs that show how influential: 2*(p+1)/n 
2*(4+1)/15

# Compute Cook's Distance for year 2003
cd.tv<-cooks.distance (out.tv)
subset(cd.tv,tv$Year=="2003")

# Cook's Distance cutoffs that show how influential 4/(n-(p+1))
4/(15-(4+1))

# Compute R-standardized residual for year 2013
R.tv <-rstudent(out.tv)
subset(R.tv,tv$Year=="2013")

# compute R-standardized residual for year 2005
subset(R.tv,tv$Year=="2005")

#VIF
library(car)
vif(out.tv)

tv1 <- tv[-3,]

dim(tv1)

names(tv1)

out.tv2 <- lm(TVaud~Tigers+AtBreak+DNP+Bullpen, data = tv1)

summary(out.tv2)

confint(out.tv2)

#test Ho:  
reduced.tv1 <- lm(TVaud~Tigers+AtBreak, data=tv1)
anova(reduced.tv1, out.tv2)
# Test-statistic: 0.5636
# P-value: 0.588

predict(out.tv2, newdata=data.frame(Tigers=5, AtBreak=0.600,DNP=7,Bullpen=11), 
        interval = "confidence")
predict(out.tv2, newdata=data.frame(Tigers=1, AtBreak=0.400,DNP=7, Bullpen=11), 
        interval = "confidence")


summary(out.tv2)
