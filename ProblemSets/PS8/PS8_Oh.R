library(nloptr)
library(dplyr)
library(kableExtra)
library(modelsummary)
library(tibble)

set.seed(100)

# Set up the data
N = 100000
k = 10
X = matrix(rnorm(N*k), nrow=N, ncol=k)
X[,1] = 1
eps = rnorm(N, mean=0, sd=sqrt(0.25))
beta = c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)
T_beta <- beta
Y = X %*% beta + eps

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Q5. OLS estimate matrix calculation
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::
beta_hat = solve(t(X) %*% X) %*% t(X) %*% Y
# Compare the result to the original beta
df <- data_frame(
  beta_hat = beta_hat,
  beta = T_beta,
  diff = beta_hat - beta
)

kbl(df, booktabs = T, format = "latex", digits = 6)


#::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Q6. Gradient descent
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# set up a stepsize
alpha <- 0.0000003
# set up a number of iterations
maxiter <- 500000
# Define the objective function
objfun <- function(beta, y, X) {
  return(sum((Y-X%*%beta)^2))
}
# Gradient of the objective function
gradient <- function(beta,y,X) {
  return( as.vector(-2*t(X)%*%(Y-X%*%beta)) )
}
# Initial values
beta <- runif(dim(X)[2])
# create a vector to contain all beta's for all steps
beta.All <- matrix("numeric",length(beta),maxiter)
# gradient descent method to find the minimum
iter  <- 1
beta0 <- 0*beta
while (norm(as.matrix(beta0)-as.matrix(beta))>1e-8) {
  beta0 <- beta
  beta <- beta0 - alpha*gradient(beta0,Y,X)
  beta.All[,iter] <- beta
  if (iter%%10000==0) {
    print(beta)
  }
  iter <- iter+1
}
# print result and plot all xs for every iteration
print(iter)
print(paste("The minimum of f(beta,y,X) is ", beta, sep = ""))
gd_beta <- beta

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Q7. L-BFGS and Nelder-Mead
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# L-BFGS
## initial values
beta0 <- runif(dim(X)[2]) #start at uniform random numbers equal to number of coefficients
## Algorithm parameters
options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e3)
## Optimize
result <- nloptr( x0=beta0,eval_f=objfun,eval_grad_f=gradient,opts=options,y=Y,X=X)
print(result)

LBFGS <- result$solution

# Nelder-Mead
## initial values
beta0 <- runif(dim(X)[2]) 
## Algorithm parameters
options <- list("algorithm"="NLOPT_LN_NELDERMEAD","xtol_rel"=1.0e-6,"maxeval"=1e3)
## Optimize
result <- nloptr( x0=beta0,eval_f=objfun,eval_grad_f=gradient,opts=options,y=Y,X=X)
print(result)

Nelder_Mead <- result$solution

df <- tibble(
  L_BFGS = LBFGS,
  Nelder_Mead = Nelder_Mead,
  diff = LBFGS - Nelder_Mead
)
kbl(df, booktabs = T, format = "latex", digits = 3)

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Q8. MLE by L-BFGS
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## Our objective function
objfun  <- function(theta,y,X) {
  # need to slice our parameter vector into beta and sigma components
  beta    <- theta[1:(length(theta)-1)]
  sig     <- theta[length(theta)]
  # write objective function as *negative* log likelihood (since NLOPT minimizes)
  loglike <- -sum( -.5*(log(2*pi*(sig^2)) + ((Y-X%*%beta)/sig)^2) ) 
  return (loglike)
}
# Gradient vector
gradient <- function(theta,y,X) {
  grad <- as.vector(rep(0, length(theta)))
  beta <- theta[1:(length(theta)-1)]
  sig  <- theta[length(theta)]
  grad[1:(length(theta)-1)] <- -t(X)%*%(Y-X%*%beta)/(sig^2)
  grad[length(theta)] <- dim(X)[1]/sig - crossprod(Y-X%*%beta)/(sig^3)
  return (grad)
}

## initial values
theta0 <- runif(dim(X)[2]+1) #start at uniform random numbers equal to number of coefficients

## Algorithm parameters
options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e4)

## Optimize
result <- nloptr( x0=theta0,eval_f=objfun,eval_grad_f=gradient,opts=options,y=Y,X=X)
print(result)
betahat  <- result$solution[1:(length(result$solution)-1)]
sigmahat <- result$solution[length(result$solution)]

# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Q9. Easy way to do OLS
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::
result <- lm(Y~X-1)
print(result$coefficients)

modelsummary(result, title = 'Regression Result', output = "latex")

# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Compare all results
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::
df <- data.frame(
  TRUE_ = T_beta,
  OLS = beta_hat,
  Gradient_Descent = gd_beta,
  L_BFGS = LBFGS,
  Nelder_Mead = Nelder_Mead,
  MLE = betahat,
  Easy_way = result$coefficients
)

df <- rbind(df, c(0, mean((beta_hat-T_beta)^2)*1000, mean((gd_beta-T_beta)^2)*1000, mean((LBFGS - T_beta)^2)*1000, mean((Nelder_Mead - T_beta)^2)*1000, mean((betahat - T_beta)^2)*1000, mean((T_beta - result$coefficients)^2)*1000))
row.names(df) <- c("beta1", "beta2", "beta3", "beta4", "beta5", "beta6", "beta7", "beta8", "beta9", "beta10", "Variance")

kbl(df, caption = "Comparison of the results", booktabs = T, format = "latex") %>%
  kable_styling(latex_options = c("striped", "hold_position"))
