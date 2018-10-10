  source('http://grimshawville.byu.edu/Tennis2017.R')
  tennis
  
  tennis$Best.of <- factor(tennis$Best.of)
  tennis$Best.of <- relevel(tennis$Best.of, ref = "3")
  
  
  out.tennis <- glm(DNF ~ Gender + Tournament + Round + Best.of + Surface + WRank + LRank, data = tennis, family = "binomial")
  summary(out.tennis)
  
  # To interpret gender difference, compute transformed coeff
  (1/(1+exp(-(coef(out.tennis)))[-1][1]))
  
  confint(out.tennis)
  
  # Difference between women and men on the odds of DNF
  # Based on the results, we estimate that women are 48% more likely than men to 
  # record a DNF at ATP tennis events, holding all else constant.
  
  (1/(1+exp(-(coef(out.tennis)))[-1][2]))
  # Difference between GrandSlam and Masters on the odds of DNF
  # Based on the results, we estimate that those who play in the Masters Tournament are 68% more likely
  # to record a DNF than those who play in Grand Slam tournaments, holding all else constant.
  
  (1/(1+exp(-(coef(out.tennis)))[-1][9]))
  # Difference between Best of 5 and Best of 3 matches on the odds of DNF
  # Based on the results, we estimate that those who play in Best of 5 matches are 75% more likely to
  # record a DNF than men, holding all else constant.
  
  predict(out.tennis, newdata = data.frame(Gender = "M", Tournament = "GrandSlam",
                                           Round = "1st Round", Best.of = "5", Surface = "Hard",
                                           WRank = 50, LRank = 500), type = "response")
  
  tennis.logit <- predict(out.tennis, newdata = data.frame(Gender = "M", Tournament = "GrandSlam",
                                                           Round = "1st Round", Best.of = "5", Surface = "Hard",WRank = 50, LRank = 500), 
                          type = "link", se.fit = TRUE)
  logit.L <- tennis.logit$fit - 1.96*tennis.logit$se.fit
  logit.U <- tennis.logit$fit + 1.96*tennis.logit$se.fit
  tennis.phat.L <- 1/(1+exp(-logit.L))
  tennis.phat.U <- 1/(1+exp(-logit.U))
  
  tennis.phat.L
  tennis.phat.U
  
  # Test Ho: Round of Match has no effect on DNF at alpha = 0.05
  red1.tennis <- glm(DNF ~ Gender + Tournament + Best.of + Surface + WRank + LRank, data = tennis, family = "binomial")
  anova(red1.tennis,out.tennis, test ="Chisq")
  summary(out.tennis)
  
  # The results are not statistically significant. With a test statistic of 3.376 and a p-value of 0.7604, we claim
  # that round of match has no effect on recording a DNF at ATP tournaments. This shows that the positioning in 
  # the duration of the tournament is not significant. A player is no more significant to record a DNF in a later round
  # than in an earlier round.
  
  # Test Ho: Surface has no effect on DNF at alpha = 0.05
  red2.tennis <- glm(DNF ~ Gender + Tournament + Best.of + Round + WRank + LRank, data = tennis, family = "binomial")
  anova(red2.tennis,out.tennis, test ="Chisq")
  
  # The results are not statistically significant. With a test statistic of 2.4672 and a p-value of 0.2912,
  # we claim that the playing surface has no significant effect of recording a DNF at ATP tournaments. There
  # is no evidence to claim that any surface is more likely than another to affect a player's odds
  # of recording a DNF. While many players have their preferred surface (clay, grass, or hard-court),
  # the evidence shows that there is no effect of each of these on dropping out of a tournament.  
  
  (1/(1+exp(-(coef(out.tennis)))[-1][12]))
  confint(out.tennis)
  # Interpretation of Winner's Rank effect on DNF
  # The WRank coefficient (0.501) reveals that for a one rank increase in a player's ranking, we expect
  # a 50% increase in odds of recording a DNF at ATP tennis tournaments. This reveals that ranking has a large
  # effect on the odds of recording a DNF at a tournament (95% CI 0.00077, 0.00330). A higher-ranked, more skilled
  # player has greater odds of recording a DNF than a lower-ranked player.
  
  # Construct the ROC curve to demonstrate prediction performance.
  
  library(ROCR)
  
  tennis.pred <- prediction(predict(out.tennis, type = "response"), tennis$DNF)
  tennis.perf <- performance(tennis.pred, measure="tpr",x.measure="fpr")
  plot(tennis.perf,xlab="1-specificity", ylab = "sensitivity", main = "ROC Curve")
  abline(0, 1, col="gray")
  
  # AUC
  performance(tennis.pred, measure = "auc")
  
  # The AUC curve measures prediction performance by showing how uncertain the model is 
  # in predicting the variables' contributions to recording a DNF. The value .636 shows
  # the model will be inaccurate 37% of the time.