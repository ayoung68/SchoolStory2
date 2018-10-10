B <- 10000
points <- matrix(runif(2*B,-1,1),nrow=B,ncol=2)
in.circle <- apply(points,1,function(p) p[1]^2 + p[2]^2 <= 1)
plot(points)
xy <- sapply(seq(0,2*pi,length=1000), function(a) {
  c(cos(a),sin(a))
})
lines(t(xy),col="red",lwd=10)

# Recall the that area of a circle is pi*radius^2
areaOfSquare <- 2*2
areaOfCircle <- areaOfSquare * mean(in.circle)          # Estimate of pi*r^2 = pi
est + c(-1,1)*qt(1-0.05/2,B-1)*sqrt(var(4*in.circle)B) # CI on pi
