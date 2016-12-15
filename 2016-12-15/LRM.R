x <- read.table("./exData/ex2x.dat")
y <- read.table("./exData/ex2y.dat")

m = length(y)
x = cbind(rep(1, m), x)
names(x) = c('e','x')
exact_theta = solve(t(as.matrix(x)) %*% as.matrix(x)) %*% t(as.matrix(x)) %*% as.matrix(y)
plot(x$x, y$V1, xlab = 'Age in years', ylab = 'Height in meters')
lines(x$x, as.matrix(x) %*% exact_theta)


#initialize Jvals to 100x100 matrix of 0's
J_vals = matrix(0, nrow = 100, ncol = 100)
theta0_vals = seq(-3, 3, length=100)
theta1_vals = seq(-3, 3, length=100)
alpha = 0.7
for( i in c(1:length(theta0_vals)) )
{
  for( j in c(1:length(theta1_vals)) )
  {
    t = cbind(theta0_vals[i], theta1_vals[j])
    J_vals[i,j] = sum((rowSums(t * x) - y)^2) / (2*m)
  }
}
persp(theta0_vals, theta1_vals, J_vals, xlab = 'theta_0', ylab = 'theta_1')