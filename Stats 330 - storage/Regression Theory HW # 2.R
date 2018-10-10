# Adam Young
# Dr. Grimshaw

# Regression: Theory HW #2
# 2 March 2018

# Matrix Y
y <- matrix(c(304.6,
              303.3,
              467.1,
              422.8,
              391.6,
              403.8, 
              373.8,
              263.5, 
              226.8, 
              183.9, 
              208.4, 
              208.2,
              168.8,
              245.4,
              160.0), ncol=1, byrow = TRUE)

x <- matrix(c(1, 3, 1, 5, 1, 3,
               1, 3, 1, 8, 1, 1,
               1, 6, 3, 3, 1, 2,
               1, 4, 1, 4, 1, 5,
               1, 5, 1, 3, 1, 1,
               1, 7, 1, 3, 3, 1,
               1, 6, 1, 3, 5, 2, 
               1, 6, 1, 8, 5, 3,
               1, 5, 1, 8, 3, 2,
               1, 1, 3, 4, 2, 1,
               1, 3, 5, 2, 2, 4,
               1, 2, 3, 3, 1, 3,
               1, 1, 4, 3, 2, 3,
               1, 6, 5, 3, 1, 5,
               1, 2, 1, 5, 2, 2), ncol=6, byrow = TRUE)
# TVaud = Bo + B1BOS + B2BAL + B3NYY + B4TB + B5TOR + e, e ~ N(0, sigma^2)

# Compute se(βˆ2)


# First, compute B(^)= (X'X)^(-1)*X'Y
solve(t(x) %*% x) %*% t(x) %*% y
out.xy <- lm(formula = y~-1+x)

# Compute se(βˆ2)
summary(out.xy)

# Test Ho : β3 = 0.
summary(out.xy)
# With a p-value of .07616 and a t-statistic of -2.003, we fail to reject the null hypothesis and
# claim that there is no relationship between the number of players the New York Yankees send to the
# All-Star game and the number of viewers in Boston who tune in to watch the game. 

   

z <- matrix(c(1, 1, 5, 1, 3,
              1, 1, 8, 1, 1,
              1, 3, 3, 1, 2,
              1, 1, 4, 1, 5,
              1, 1, 3, 1, 1,
              1, 1, 3, 3, 1,
              1, 1, 3, 5, 2, 
              1, 1, 8, 5, 3,
              1, 1, 8, 3, 2,
              1, 3, 4, 2, 1,
              1, 5, 2, 2, 4,
              1, 3, 3, 1, 3,
              1, 4, 3, 2, 3,
              1, 5, 3, 1, 5,
              1, 1, 5, 2, 2), ncol=6, byrow = TRUE)




# Model: Y = XB + E



# Compute B(^)= (X'X)^(-1)*X'Y

y1 <- matrix(c(9,
               8,
               9,
               10,
               7,
               5,
               7,
               6), ncol=1, byrow = TRUE)

x1 <- matrix (c(0, 0, 0, 1,
                0, 1, 0, 0,
                0, 0, 1, 0,
                0, 0, 0, 1,
                0, 1, 0, 0,
                1, 0, 0, 0, 
                0, 0, 1, 0,
                1, 0, 0, 0), ncol=4, byrow = TRUE)

# Compute B(^)= (X'X)^(-1)*X'Y
out.x1y1 <- lm(formula = y1~x1)
summary(out.x1y1)


# Test Ho : β3 = 0
# 
