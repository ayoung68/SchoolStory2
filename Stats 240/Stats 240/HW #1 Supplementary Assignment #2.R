# Prior distribution  
curve(dbeta(x, 8, 2), lty=2, add=T)

# Posterior distribution  
curve(dbeta(x, 246, 40), xlab="p", ylab="dist of p")

# Posterior mean
246/(246+40)

# Maximum likelihood estimate 
238/276
