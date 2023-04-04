# 4. done 
library(nloptr)
set.seed(100)

N <- 100000
K <- 10

# 4.

# Generate X matrix
X <- matrix(rnorm(N*K, mean = 0, sd = 1), ncol = K)
X[,1] <- rep(1, N)
# check matrix 
str(X)
# Generate eps vector
eps <- rnorm(N, mean = 0, sd = 0.5)

# Generate beta vector
beta <- c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)

# Generate Y vector
Y <- X %*% beta + eps

# 5.

# Compute OLS estimate of beta
b_ols <- solve(t(X) %*% X) %*% t(X) %*% Y

# Compare with true value of beta
print(b_ols)
#             [,1]
#[1,]  1.5005793
#[2,] -0.9956182
#[3,] -0.2486498
#[4,]  0.7471903
#[5,]  3.5017669
#[6,] -1.9994365
#[7,]  0.5011339
#[8,]  0.9987400
#[9,]  1.2528300
#[10,]  1.9993846

print(beta)

# [1]  1.50 -1.00 -0.25  0.75  3.50 -2.00  0.50  1.00
# [9]  1.25  2.00

# The OLS estimate of beta and the true beta are very close to the same. 

# 6.

# Set the learning rate (step size)
learning_rate <- 0.0000003

# Initialize beta_hat to zero
beta_hat <- matrix(rep(0, K), ncol = 1)

# Set the maximum number of iterations
max_iter <- 1000

# Perform gradient descent
for (i in 1:max_iter) {
  # Compute the gradient vector
  gradient <- t(X) %*% (X %*% beta_hat - Y)
  
  # Update beta_hat
  beta_hat <- beta_hat - learning_rate * gradient
}

# Print the OLS estimate of beta using gradient descent
print(beta_hat)
# [,1]
# [1,]  1.5005793
# [2,] -0.9956182
# [3,] -0.2486498
# [4,]  0.7471903
# [5,]  3.5017669
# [6,] -1.9994365
# [7,]  0.5011339
# [8,]  0.9987400
# [9,]  1.2528300
# [10,]  1.9993846
print(beta)
# [1]  1.50 -1.00 -0.25  0.75  3.50 -2.00  0.50  1.00
# [9]  1.25  2.00
# Close to the same as well. 

# 7.Compute ˆbOLS using nloptr’s L-BFGS algorithm. 
# Define objective function

eval_f <- function(b, X, Y) {
  sum((Y - X %*% b)^2)
}

# Define gradient function
eval_grad_f <- function(b, X, Y) {
  -2/N * t(X) %*% (Y - X %*% b)
}

# Set bounds and initial value
lb <- rep(-Inf, K)
ub <- rep(Inf, K)
x0 <- rep(0, K)

# Define list of options for nloptr
opts <- list("algorithm"="NLOPT_LD_LBFGS", "xtol_rel"=1.0e-10)

# Run optimization using nloptr
res <- nloptr(x0=x0, eval_f=eval_f, eval_grad_f=eval_grad_f, X=X, Y=Y, opts=opts)
res
# Call:

# nloptr(x0 = x0, eval_f = eval_f, eval_grad_f = eval_grad_f, opts = opts, 
#       X = X, Y = Y)

# Minimization using NLopt version 2.7.1 

# NLopt solver status: 1 ( NLOPT_SUCCESS: Generic success 
#                         return value. )

# Number of Iterations....: 7 
# Termination conditions:  xtol_rel: 1e-10 
# Number of inequality constraints:  0 
# Number of equality constraints:    0 
# Optimal value of objective function:  24953.283521585 
# Optimal value of controls: 1.500579 -0.9956182 -0.2486498 0.7471903 3.501767 
# -1.999436 0.5011339 0.99874 1.25283 1.999385


# Do it again using the Nelder-Mead
# algorithm. 
# Define objective function
eval_f <- function(b, X, Y) {
  sum((Y - X %*% b)^2)
}

# Set bounds and initial value
lb <- rep(-Inf, K)
ub <- rep(Inf, K)
x0 <- rep(0, K)

# Define list of options for nloptr
opts <- list("algorithm"="NLOPT_LN_NELDERMEAD", "xtol_rel"=1.0e-10)

# Run optimization using nloptr
res2 <- nloptr(x0=x0, eval_f=eval_f, X=X, Y=Y, lb=lb, ub=ub, opts=opts)
res2
# Extract solution from res object
b_gd <- res2$solution
b_gd
# [1]  0.9767063 -0.9680386 -0.1361704  1.1277353
# [5]  3.2668671 -2.1447235  0.5663044  1.0422382
# [9]  1.4966523  2.3011765

# Do your answers differ? Yes, they are different values close but not very close at all, off by .25-.5. 

# 8.
library(nloptr)
# set up a stepsize
alpha <- 0.003
# set up a number of iteration
iter <- 500
# define the gradient 
gradient <- function (theta ,Y,X) {
grad <- as.vector(rep(0,length(theta)))
beta <- theta[1:(length(theta)-1)]
sig <- theta[length(theta)]
grad[1:(length(theta)-1)] <- -t(X)%*%(Y - X%*% beta)/(sig^2)
grad[length(theta)] <- dim(X)[1]/sig - crossprod(Y-X%*% beta)/(sig^3)
return ( grad )
}
# randomly initialize a value to x
set.seed(100)
x <- floor(runif(1)*10)
# create a vector to contain all xs for all steps
x.All <- vector("numeric",iter)
# gradient descent method to find the minimum(for loop)
for(i in 1:iter){
  x <- x - alpha*gradient(x)
  #store x values
  x.All[i] <- x
  #print 
  print(x)
}
# print result and plot all xs for every iteration
print(paste("The minimum of f(x) is ", x, sep = ""))
#[1] "The minimum of f(x) is 3"

# 9.  
# linear regression model
lm <- lm(Y ~ X - 1)

# Print estimated coefficients
print(coef(lm))

library(modelsummary)

# Generate regression table and export to .tex file
reg_table <- list(lm)
modelsummary(reg_table, output = "latex", file = "PS8_Fraser.tex")
reg_table

# The estimated coefficients are very close to the true values of beta. This is an indication that
# the linear regression model is able to recover the underlying relationship between X and Y. The small 
# differences are likely due to the presence of noise in the data. 

