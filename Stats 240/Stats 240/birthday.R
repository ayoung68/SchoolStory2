birthday <- function(n, nReps = 10000) {
  vec <- rep(0,nReps)
  for (i in 1: nReps) {
    samp <- sample(1:365, n, replace = TRUE)
    vec[1] <- (length(unique(samp)) !=n)
  }
  mc.est <- mean(vec)
  mc.ci <- mc.est + c(-1, 1)*qnorm(0.975)*sqrt((mc.est*(1-mc.est))/nReps)
  out <- c(mc.est, mc.ci)
  names(out) <- c("estimate", "lower", "upper")
  out
}

birthday(70, 10000)

birthday.exact <- function(n){
  1 - (factorial(n)*choose(365, n))/(365^n)
}

birthday.exact(70)

n <- seq(0,70)
exact <- sapply(n, birthday.exact)
estimate <- sapply(n, birthday)
plot(n,exact,col = "red", type = "l", xlab = "N", ylab = "Probability")
lines(n, estimate[1,])
lines(n, estimate[2,], lty = 2)
lines(n, estimate[3,], lty = 2)



