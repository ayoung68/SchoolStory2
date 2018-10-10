allstar <- read.table("http://grimshawville.byu.edu/MLBbostonASG.txt", header = TRUE)
allstar[1:5]

# # forward selection because we have more explanatory variables than rows of observations.
min.model <-lm(aud/10^3~+1, data = allstar)
biggest.model <-formula(lm((aud/10^3~+1 .~ data = allstar)))
out.asg <- step(min.model,direction = "forward",scope=biggest.model, steps=3)

# proposed model
out1.asg <- lm(aud/10^3~ BOS,total+ bullpen, data = allstar)
summary(out1.asg)

library(car)
avPlots(out.1asg)

# added variable plot?
(aud/10^3~+1, data = allstar)
(aud/10^3~+1, data = allstar)
