critiques <- read.csv("http://grimshawville.byu.edu/bestpicture.txt", header = FALSE)
colnames(critiques)<- c("Movie Name", "Winner", "Tomatometer", "MPAARating")

# Fit the Model
# Model: # (Winner=1) = beta0 + beta1 Tomatometer + beta 2 MPAA Rating  
out.critiques.set <- glm(Winner~Tomatometer + MPAARating, data = critiques, family = "binomial")

out.critiques.set$MPAARating<- factor(out.critiques.set$MPAARating) 

#critiques$MPAARating <- ifelse(critiques$ =="R", "PG-13", "PG", "G", 3, 2, 1, 0)
#examp$pass <- ifelse(examp$ExamP =="Passed",1,0)

# Regression coefficients and standard errors
summary(out.critiques.set)

confint(out.critiques.set)

exp(confint(out.critiques.set))
# Bo
# Critic review for winning the Academy Award
# There is not a significant effect of Tomatometer critic ratings on winning the Academy Award for Best Picture. 
# For a one percent increase in positive ratings from the critics, we estimate odds 
# of winning an Academy Award to increase by 0.1%, holding all else constant. (95% CI, -0.00131, 0.01401)

# What is the difference between R and PG
summary(out.critiques.set)

sum(0.231981-0.050808)
# According to the test, an R-rated movie is 18% more likely to win an Academy Award for Best Picture than
# a PG-rated movie. In other words, according to the results, an R-rated movie's odds of winning an Academy
# Award for Best Picture are 18.1% greater than the odds of a PG-rated movie.

# Hypothesis test

# Null: Ho: MPAA Rating has no effect on winning the Academy Award.
# Alternative: Ha: MPAA Rating has a significant effect on winning the Academy Award. 

summary(out.critiques.set)
