

hypergeom <- function(x,M,N,n){
  choose(M,x)*choose(N-M,n-x)/choose(N,n)
}

(hypergeom(0, 12, 20, 8))+(hypergeom(1, 12, 20, 8))+(hypergeom(2, 12, 20, 8))

(hypergeom(2, 12, 20, 8))+(hypergeom(3, 12, 20, 8))+(hypergeom(4, 12, 20, 8))+(hypergeom(5, 12, 20, 8))+(hypergeom(6, 12, 20, 8))

sum(dhyper(0:1, 12, 20, 8))
sum(dhyper(2:6, 12, 20, 8))

sum(dnbinom(139:181 - 40, 40, 0.25))

dhyper(10, 15, 35, 30)
sum(dhyper(10:30, 15, 35, 30))
sum(dhyper(10:20, 15, 35, 20))

sum(0.3798188+0.013952)
nbinom(0:4, 1, 0.20)

dnbinom(3-1, 1, .409)
sum(dnbinom(1:4-1, 1, .409))



ppois(q=10, lambda = 20)
1-ppois(q=20, lambda = 20)
ppois(q = 9, lambda = 10)

# #77) part c
sum(ppois(q=28, lambda = 10) - (ppois(q = 11, lambda = 10)))
sum(ppois(q=19, lambda = 10) - (ppois(q = 11, lambda = 10))) 

sumpois(0:21, lambda = 10))-sum(ppois(0:11, lambda = 10))

(pbinom(1, 6, .10, lower.tail = F))

