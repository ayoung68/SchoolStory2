examp <- read.csv("http://grimshawville.byu.edu/ExamP.csv") 
  
low_gpa <- examp[examp$GPA < 3.5,]

high_gpa <- examp[examp$GPA > 3.5,]

prop.table(table(low_gpa$ExamP))

prop.table(table(high_gpa$ExamP))
rbind(prop.table(table(low_gpa$ExamP)), prop.table(table(high_gpa$ExamP)))

boxplot(GPA~ExamP, data = examp, xlab = "ExamP", ylab = "GPA")

# define response variable
examp$pass <- ifelse(examp$ExamP =="Passed",1,0)

# The response variable is whether the student received a pass or failing grade on the exam.
# The explanatory variable is the student's GPA.

# Model
# log(pass|gpa)_/P(no pass|gpa)])=beta0 +beta1GPA
out.examp <- glm(pass~GPA, data = examp, family = "binomial")

# Parameter estimates and standard errors
summary(out.examp)

# What does value B^1(2.256) represent? What is a better quantity to describe?
# The value 2.256 represents that holding all else constant, the student's odds of passing actuarial 
# exam P will increase by 2.256 for every one unit increase in GPA. In other words, it means 
# for a one unit increase in GPA, we estimate that the odds of passing actuarial exam P will
# increase by ten times.

# Does GPA have a significant effect on passing?

# Z-test (Wald)
summary(out.examp)

# Ho: There is no relationship between GPA and passing the actuarial exam P.
# Ha: There is a relationship between GPA and passing the actuarial exam P.

# P-value: 0.0329
# z-test statistic: 2.133
# Conclusion: With a p-value of 0.0329 and a z-test statistic of 2.133, we reject the null and
# claim that GPA has a significant effect on passing. In other words, according to the test,
# a person's GPA prior to taking actuarial exam P has a strong effect on the odds of the
# person passing the exam.

# LRT X^2
reduced.examp <- glm(pass~ + 1, data = examp, family = "binomial")
anova(reduced.examp, out.examp, test = "Chisq")

# Ho: There is no relationship between a student's GPA and his or her ability to pass actuarial exam P. 
# Ha: There is a relationship between a student's GPA and his or her ability to pass actuarial exam P.

# P-value: 0.01055
# LRT X^2 value: 6.5393
# Conclusion: Due to the P-value of 0.01055 and the LRT X^2 value of 6.5393, We reject the null and claim 
# that a student's GPA score has a significant effect on his or her odds of passing actuarial exam P. In 
# other words, based on the test, a student's GPA score has a strong effect on the student's odds of passing 
# actuarial exam P.

# 95%  CI on betaw
confint(out.examp)
# In repeated sampling, we are confident that the interval (0.4630731, 4.716415) will contain our prediction 
# (the true mean effect of GPA on the ability to pass actuarial exam P) 95% of the time. We are 
# confident that our prediction will fall in the interval 95% of the time.

# Predict the probability of passing Exam P for two students.
preds <- predict(out.examp, newdata = data.frame (GPA=c(3.25, 3.85)), type = "response")
names(preds)<- c("3.25", "3.85")
preds

# create graphic: plot of passing 
# data
plot(pass ~ GPA, data = examp, xlim = c(0,4))

# curve of probability
xstar<-seq(0,4, length = 100)
phat <- predict(out.examp,newdata=data.frame(GPA = xstar), type = "response")
lines(xstar, phat, col = "red")

#95% Confidence intervals
logit.hat <- predict(out.examp,newdata=data.frame(GPA = xstar), type = "link",se.fit=TRUE)
t <- qnorm(0.975)
logit.L <- logit.hat$fit-t*logit.hat$se.fit
logit.U <- logit.hat$fit+t*logit.hat$se.fit
phat.L<-1/(1+exp(-logit.L))
phat.U<-1/(1-exp(-logit.U))
lines(xstar,phat.L,lty=2,col="gray")
lines(xstar,phat.U,lty=2,col="green")
# Create the ROC curve

# Compute AUC and explain how it measures prediction performance

# Research Task and Data Features that Match Analysis Strengths
# One feature of the data that matched the analysis strengths were the Wald and LRT X^2 tests. Each of these
# gave evidence that a given student's GPA had a strong effect on his or her probability of passing actuarial
# exam P. Another strength of the data was the final graphic, which showed the increasing probability of passing the 
# exam as each of the students' GPAs increased.

# Analysis Weaknesses:
# One weakness of the analysis was that it didn't give any residual estimates to show how far off 
# of the actual prediction the data were.