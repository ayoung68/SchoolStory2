pbinom(4, 10, .3)
dbinom(4, 10, .3)
dbinom(6, 10, .7)
sum(dbinom(2:4, 10, .3))
pbinom(2, 10, .3, lower.tail = F)
sum(dbinom(0:1, 10, .7))
sum(dbinom(3:5, 10, .3))

sum(dbinom(1:9, 20, .75))
sum(dbinom(2:3, 3, .06))

pbinom(10, 15, .75, lower.tail = F)

#Poissant distribution
ppois(q=8, lambda=5)
dpois(8, lambda=5)
ppois(8, 5, lower.tail = F)
(dpois(9, 5)) - sum(dpois(6, 5))
  sum(dpois(5:8, 5))
sum(dpois(6:7, 5))
ppois(q=10, lambda = 22)
dpois(20, 22)
