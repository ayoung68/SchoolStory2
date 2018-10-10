mlb.url <- "http://espn.go.com/mlb/standings/_/season/2017"
mlb.webpage <- htmlParse(mlb.url)

# install.packages ("XML")
library(XML)
mlb.url <- "http://espn.go.com/mlb/standings/_/season/2017"
mlb.webpage <- htmlParse(mlb.url)
mlb <- readHTMLTable(mlb.webpage, header = FALSE, which = 1, 
                     skip.rows = c(1:21, 27, 33), colClasses = rep("numeric",11))
mlb <-mlb[,c(1,9)]
names(mlb)<- c("wins", "run.diff")

mlb$division <- c(rep("East",5),rep("Central",5),rep("West",5))

tapply <- mlb[standings]

east <- mlb[mlb$division =="East",]
summary (east$wins)
summary (east$run.diff)

# parallel lines                      
# to show the 3 models that are estimated (one for each factor level)
# scatterplot
plot(mlb$run.diff, mlb$wins, xlab = "Run Differential", ylab = "Wins", pch=19)
points(mlb$run.diff[mlb$division=="East"], mlb$wins[mlb$division=="East"],
       col="coral2", pch=19)
points(mlb$run.diff[mlb$division=="Central"], mlb$wins[mlb$division=="Central"],
       col="dodgerblue", pch=19)
points(mlb$run.diff[mlb$division=="West"], mlb$wins[mlb$division=="West"],
       col="black", pch=19)
legend("topleft", legend = c("East", "Central", "West"),
       col=c("coral2", "dodgerblue", "black"),
       pch=c(rep(19,3)))

# Model
mlb$division<-factor(mlb$division)

# fit the model
out1.mlb<-lm(wins~division + run.diff, data=mlb, x=TRUE, y=TRUE)

# Specify that East is the level we want to compare against
mlb$division<-relevel(mlb$division, "East")

# Why isn't there a a column for divisionCentral?
# There is no column for divisionCentral because we are comparing both Central and West
# against East. East is a reference point, so Central and West are being compared against 
# that division.

# What does the value of the estimated regression coefficient for run.diff represent in this
# analysis?
# The estimated regression coefficient for run.diff represents the number of wins that 
# would result for a given team if its run differential increased by a given number. For example, 
# in the AL West division, one win would result if the run differential for a given team 
# increased by 0.082029. 

out2.mlb <-lm(wins~division + run.diff, data=mlb, x = TRUE, y = TRUE)

summary(out2.mlb)

# Why is interpreting the effect of division complicated?
# The interpretation of the division effect is complicated since each division is measured in 
# relation to another division, meaning there is no baseline measurement. Since division 
# has three levels that are actually three models, each regression coefficient is with 
# regards to a different model.

#AL East
abline(81.096918, 0.082029, col="orange")
# AL Central
abline(81.096918-0.168041, 0.08202942, col = "black")
# AL West
abline(81.096918+0.138265, 0.08202942, col = "red")

# ANOVA: test Ho: no division effect
reduced2.mlb<-lm(wins~run.diff, data = mlb)
anova(reduced2.mlb, out2.mlb)

# With an F-test statistic of 0.0082 and a p-value of 0.9919, we observed that there was no
# statistically significant difference in the average number of wins due to division after
# run differential was calculated. This leads to believe that there is no "grass is greener 
# on the other side" effect.

# Research Task and Data Features that Match Analysis Strengths:
# One feature of the data that matched the analysis strengths was the minimal difference 
# how parallel the lines were as a result of the regression coefficient models done for
# each team. This gave evidence of how there was no significant effect of number of wins
# due to division. 

# Analysis Weaknesses:
# One analysis weakness was the uncertainty that comes from using three different divisions 
# as the baseline for obtaining measurements, rather than using a baseline measurement. This 
# could be represented as a collective standard based on past data, which could be pulled from
# past data.

# CHALLENGE:
# Research Task:
# Is the Eastern conference in the NBA really an easier conference than the Western conference?
# Data: http://www.espn.com/nba/standings/_/season/2017(2016-2017 NBA regular season standings)