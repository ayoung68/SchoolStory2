# Matrix Y
Y <- matrix(c(304.6,
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

X <- matrix (c(1, 3, 1, 5, 1, 3,
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

solve(t(X) %*% x) %*% t(X) %*% Y
full.beta.hat <- solve(t(X)%*%X)%*%t(x)%*%Y
# Compute B(^)= (X'X)^(-1)*X'Y
out.XY <- lm(formula = Y~-1+X)
summary(out.XY)


solve(se(full.data.beta))



# Boston ASG Viewership

# Test Ho: "far away teams" have no effect on BOS viewership

#     Ho: TB and BOS have no effect
#     Ha: beta_4 = beta_5 = 0

X <- matrix (c(1, 3, 1, 5,
               1, 3, 1, 8,
               1, 6, 3, 3,
               1, 4, 1, 4,
               1, 5, 1, 3,
               1, 7, 1, 3,
               1, 6, 1, 3, 
               1, 6, 1, 8,
               1, 5, 1, 8,
               1, 1, 3, 4,
               1, 3, 5, 2,
               1, 2, 3, 3,
               1, 1, 4, 3,
               1, 6, 5, 3,
               1, 2, 1, 5), ncol=6, byrow = TRUE)

# Full Model

full.beta.hat <- solve(t(X)%*%X)%*%t(X)%*%Y
reduced.beta.hat <-((t(Xr)%*%Xr)%*%t(Xr)%*%Y)

full.sse <-t(Y-X%*%full.beta.hat)%*%(Y-Xr %*% reduced.beta.hat)

full.def.error<-dim(Xr)[1] - dim(Xr)[2]

reduced.sse <-t(Y-Xr %*%reduced.beta.hat)%*%(Y-Xr %*% reduced.beta.hat)

reduced.def.error<-dim(Xr)[1] - dim(Xr)[2]

solve(t(X) %*% X) %*% t(X) %*% y

anova(out.XY)
