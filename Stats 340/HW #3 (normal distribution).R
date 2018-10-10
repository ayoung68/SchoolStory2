# 42(a)
pnorm(15, 15, 1.25)

# (b)
pnorm(17.5, 15, 1.25)

# (c)
pnorm(10, 15, 1.25, lower.tail = FALSE)

# (d)
pnorm(18, 15, 1.25) - pnorm(14, 15, 1.25)

# (e)
pnorm(18, 15, 1.25)


# 44
# (b)
pnorm(4499.5, 3500, 600) - pnorm(3000.5, 3500, 600)
pnorm(4500, 3500, 600) - pnorm(3000, 3500, 600)

# (c)
pnorm(2500, 3500, 600)

# (d)
pnorm(6000, 3500, 600, lower.tail = FALSE)

# (e)
pnorm(4274, 3500, 600)

# (f)
pnorm(5.511557, 7.716179, 1.32277)

# 46
#(a) 
pnorm(20, 30, 7.8)
pnorm(19.5, 30, 7.8)

# (b)
qnorm(0.75, 30, 7.8)

# (c)
qnorm(0.15, 30, 7.8)

# (d)
qnorm(0.90, 30, 7.8)
qnorm(0.10, 30, 7.8)

#48
#(a)
pnorm(10, 8.8, 2.8, lower.tail = FALSE)
pnorm(10.01, 8.8, 2.8, lower.tail = FALSE)

# (b)
pnorm(20.01, 8.8, 2.8, lower.tail = FALSE)

# (c)
pnorm(10, 8.8, 2.8) - pnorm(5, 8.8, 2.8)

# 54
#(b) 
pnorm(65, 58.22, 11.65) - pnorm(50, 58.22, 11.65)

# (c)
pnorm(70.01, 58.22, 11.65, lower.tail = FALSE)

# 62
# (a)
pbinom(20,25,0.5) - pbinom(14,25,0.5)

pbinom(20,25,0.6) - pbinom(14,25,0.6)

pbinom(20,25,0.8) - pbinom(14,25,0.8)

# (b)
pbinom(15,25,0.5) 
pbinom(15,25,0.6) 
pbinom(15,25,0.8) 

# (c)
pbinom(20,25,0.5, lower.tail = FALSE) 
pbinom(20,25,0.6, lower.tail = FALSE) 
pbinom(20,25,0.8, lower.tail = FALSE) 

# 64
pbinom(370,500,0.7) - pbinom(320, 500, 0.7)

pbinom(324,500,0.7)
pbinom(314,500,0.7)
 
