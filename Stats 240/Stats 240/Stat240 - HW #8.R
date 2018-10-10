#evaluate P(x-3) = b(x=3; n=10, p=0.25)
dbinom(x=3, size=10, prob=0.25)
dbinom(3, 10, 0.25)

#P(x=7)
dbinom(7, 10, 0.25)


dbinom(0:10, 10, 25)

#Plot Bin (10, 0, 25) pmf
plot(0:10, dbinom(0:10, 10, 0.25))

