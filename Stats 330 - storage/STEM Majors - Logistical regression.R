STEM <- read.csv(source("http://grimshawville.byu.edu/STEMgetdata.R"))

# EDA: 
# Side-by-side boxplots
boxplot(nST~y, data = STEM)
boxplot(new.teach~y, data = STEM)
boxplot(new.stu~y, data = STEM)

# Contingency tables
table.prevCalc <- table(STEM$prevCalc, STEM$y)
table.newMJ <- table(STEM$newMJ, STEM$y)
table.gender <- table(STEM$gender, STEM$y)

# Analysis

# The response variable is whether or not the student dropped the class. The explanatory variables are
# prevCalc (calculus experience), nST (percentile of standardized test), newMJ (intended major),
# new.teach (instructor quality), new.stu(student-centered practices), and gender.

# declare categorical variables as R type factor
STEM$gender <- factor(STEM$gender)
STEM$gender <- relevel(STEM$gender, ref = "l")
STEM$prevCalc <- factor(STEM$prevCalc)
STEM$prevCalc <- relevel(STEM$prevCalc, ref = "m")
STEM$newMJ <- factor(STEM$newMJ)
STEM$newMJ <- relevel(STEM$newMJ, ref = "l")

#MODEL
# logit(y=l)=logit(Switchers)=beta0 + beta1nsT +beta2new.teach + beta3new.Stu + gender_i + prevCalc_+newMJ_k
out.stem <- glm(~nST+new.teach + new.stu + gender + prevCalc + newMJ, data = STEM, family = "binomial")
summary(out.stem)

# To interpret gender difference, compute transformed coeff
exp(coef(out.stem)) [-1]

# create a graphic showing the difference between women & men
# men

x.star<-data.frame(gender="1", nST=seq(2,99, length=100),
                   new.teach=6, new.stu=6, prevCalc="1", newMJ="1")

plot(x.star$nST, predict*out.STEM, newdata=x.star, type = "response"),
                  type="l", ylim=(0, 0.25),
                  ylab = "P(Swith from Calc Seq)",xlab="Percentile of Standardized Test"
# women
x.star<-data.frame(gender="2", nST=seq(2,99, length=100),
                   new.teach=6, new.stu=6, prevCalc="1", newMJ="1")

lines(x.star$nST, predict*out.STEM, newdata=x.star, type = "response"),
  col="red"
ylab = "P(Switch from Calc Seq)",xlab="Percentile of Standardized Test"

# differene between men and women, holding all else constant
# 95% CI
expt(confint(out.STEM)[-1,])

# x-test
summary(out.STEM)

# X^2 test
red1.STEM <- glm(~nsT+new.Teach + new.stu + gender + prevCalc + newMJ, data = STEM, family = "binomial")
anova(red1,out.stem, test ="Chisq")
summary(out.stem)

# Is there an effect due to calculus preparation?
# X^2 test
red2.STEM <- glm(y~nST+new.teach+new.stu+gender+newMJ, data=STEM)
anova(red2.STEM, out.STEM,test="Chisq")

# If we focus on using the model to identify students at risk of switching
# ROC Curve
library(ROCR)
STEM.pred <- prediction(predict(out.STEM, type = "response"), STEM$y)
STEM.perf <- performance(STEM.pred, measure="tpr",x.measur="fpr")
plot(STEM.perf,xlab="1-specificity", ylab = "sensitivity", main = "ROC Curve")
abline(0, 1, col="gray")

# AUC
performance(STEM.pred, measure = "auc")
