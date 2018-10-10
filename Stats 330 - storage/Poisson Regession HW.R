explosions <- read.table(header = TRUE, text = '
                        BadRings Temp
                         0 66
                         1 70
                         0 69
                         0 68
                         0 67
                         0 72
                         0 73
                         0 70
                         1 57
                         1 63
                         1 70
                         0 78
                         0 67
                         3 53
                         0 67
                         0 75
                         0 70
                         0 81
                         0 76
                         0 79
                         2 75
                         0 76
                         1 58
                         ')

# Model: B0: O-ringfailures + Bo1: Temperature)
out.explosions <- glm(BadRings~Temp, data=explosions, family = "poisson")

summary(out.explosions)

confint(out.explosions)

# There is a statistically significant effect of temperature on O-ring failures. For a one degree drop in temperature (degrees Fahrenheit),
# we estimate a 12.1% increase in the odds of an O-ring failure and a subsequent shuttle explosion. (95% CI 4%, 21%)

summary(out.explosions)
# Ho: Temperature has no effect on O-ring failures
# Ha: Temperature has an effect on O-ring failures
# alpha = 0.05

# With a p-value of 0.00322 and a t-statistic of -3.108, we reject the null hypothesis and claim there is an 
# effect of temperature on O-ring failures. According to the data, a decrease in temperature will increase the odds
# of having O-ring failures. This confirms the notion that when the outside temperature drops, the O-rings 
# are less resilient and more likely to fail. This contributes to the likelihood of a disaster happening at 
# launch of a space shuttle in cold weather.

# Estimate for # of O-ring failures

predict(out.explosions,newdata=data.frame(Temp=31), type = "link")
launch.logit <- predict(out.explosions, newdata=data.frame(Temp=31), type="link", se.fit = TRUE)
logit.L.launch <- launch.logit$fit - 1.96*launch.logit$se.fit
logit.U.launch <- launch.logit$fit + 1.96*launch.logit$se.fit
launch.phat.L <-1/(1-exp(-logit.L.launch))
launch.phat.U <-1/(1+exp(-logit.L.launch))
launch.phat.L
launch.phat.U 
