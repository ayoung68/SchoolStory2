# Stepwise Regression: MLB All-Star
# 9 April 2018

allstar <- read.table("http://grimshawville.byu.edu/MLBbostonASG.txt", header = TRUE)
allstar[1:5]

# # forward selection because we have more explanatory variables than rows of observations.
min.model <- lm(aud/10^3~1, data = allstar)
biggest.model <- formula(lm(aud/10^3~1, data = allstar))
out.asg <- step(min.model,direction = "forward",scope=biggest.model, steps=3)

# proposed model
out1.asg <- lm(aud/10^3~ BOS.total+bullpen, data = allstar)
summary(out1.asg)

# The first explanatory variable added in the regression model was the total number of Boston players on the All-Star game
# roster prior to the All-Star break. The lurking variables behind this effect are the team's actual record
# during the regular season (wins and losses), the number of wins by more than one run, and the team's
# record within their division, the AL East.

library(car)
avPlots(out1.asg)

# The effects associated with the negative regression coefficients are the decrease in attention
# to the team when a certain variable is measured. For example, as the number of bullpen players 
# are added to the team, the number of viewers decreases. This is because the bullpen players are 
# rarely used and considered less exciting for fans to watch. 

# To make last year's All-Star game more interesting to the Boston audience, they could have
# selected fewer bullpen players to be on the roster and added more position players.

out2.asg <- lm(aud/10^3~ BOS.total+WAS.total+bullpen+TEX.first+DET.first, data = allstar)
summary(out2.asg)

# The five new variables being fit are BOS.total, WAS.total, bullpen, TEX.first, and DET.first. The new R^2 is
# 0.931. Some clues that this is overfit are that some of the variables may not seem as significant as we look at
# them. This may be present in the variables with a regression estimate that is closer to 0, being determined 
# by us. 

# Backward elimination is unfeasible for this data since we cannot return and recognize
# the strong regression coefficients by eliminating one variable. This is because each of these categories
# cannot be changed, since they are all a part of the All-Star game format. Additionally, the number
# of teams can't be reduced or changed, since the format and makeup of the MLB is already set in place.

# Research Task and data features that match analysis strengths:
# One feature of the data that matched the analysis strength was the utilization of forward
# selection to locate variables that were best for regression. This was helpful because it 
# enabled us to not utilize unimportant variables within the model. Additionally,
# the AV plots helped to recognize the importance of the variables relative to Boston itself.

# Analysis Weaknesses: One analysis weakness was the presence of several lurking variables, such
# as the success of the team (Boston) or the success of other teams in comparison to Boston. These 
# are hidden at an initial view the data, yet show the many ways the data can have unexplained characteristics.
# Forward selection can also require unnecessary searching if the variable is unable to be found by simple searching.

