#Q1,a
C <- 1 / (2 - 5/exp(1))

f <- function(x) {
  C * x^2 * exp(-x)
}
K <- f(1)

rejection_sample <- function(n) {
  samples <- numeric(0)
  
  while (length(samples) < n) {
    
    x <- runif(1, 0, 1)
    u <- runif(1)
    
    if (u <= f(x) / K) {
      samples <- c(samples, x)
    }
  }
  
  return(samples)
}

samples <- rejection_sample(5000)

hist(samples,
     probability = TRUE,
     breaks = 30,
     main = "Rejection Sampling",
     xlab = "x")

curve(f(x), 0, 1, add = TRUE, lwd = 2)

#Q2 a
N <- 100000

x <- runif(N, 0, 1)

I_hat <- mean(exp(exp(x)))

I_hat

#b
N <- 100000

x <- rnorm(N, mean = 0, sd = 1)

I_hat <- mean(sqrt(2*pi) * exp(-x^2 / 2))

I_hat
#c
N <- 100000

x <- runif(N, 0, 1)
y <- runif(N, 0, 1)

I_hat <- mean(exp((x + y)^2))

I_hat
#d
N <- 100000
x <- rexp(N, 1)
y <- rexp(N, 1)

mean(y <= x)

#Q3
# Define h(x)
h <- function(x) {
  (cos(50*x) + sin(20*x))^2
}

# Sample sizes
N_values <- seq(100, 20000, by = 100)

# Create empty vectors
estimates <- numeric(length(N_values))
sd_estimates <- numeric(length(N_values))

# Repeat for each N
for (i in 1:length(N_values)) {
  
  N <- N_values[i]
  
  # Generate N Uniform(0,1) samples
  x <- runif(N, 0, 1)
  
  # Calculate h(x)
  hx <- h(x)
  
  # Monte Carlo estimate
  estimates[i] <- mean(hx)
  
  # Estimated standard deviation of the MC estimator
  sd_estimates[i] <- sd(hx) / sqrt(N)
}


plot(N_values, estimates,
     type = "l",
     xlab = "Sample size N",
     ylab = "Monte Carlo estimate")

lines(N_values,
      estimates + sd_estimates,
      lty = 2)

lines(N_values,
      estimates - sd_estimates,
      lty = 2)

abline(h = 0.965, lty = 3)

#4b
# Posterior 1
f1 <- function(theta) {
  dbeta(theta, 26, 76)
}

mode1 <- (26 - 1) / (26 + 76 - 2)

K1 <- f1(mode1)

rejection1 <- function(n) {
  
  samples <- numeric(0)
  attempts <- 0
  
  while (length(samples) < n) {
    
    # sample theta from g(theta) = Uniform(0,1)
    theta <- runif(1, 0, 1)
    
    # sample U
    u <- runif(1)
    
    attempts <- attempts + 1
    
    # g(theta) = 1
    if (u <= f1(theta) / K1) {
      samples <- c(samples, theta)
    }
  }
  
  acceptance_rate <- n / attempts
  
  return(list(samples = samples,
              acceptance_rate = acceptance_rate))
}


f2 <- function(theta) {
  dbeta(theta, 27.24, 75.96)
}

mode2 <- (27.24 - 1) / (27.24 + 75.96 - 2)

K2 <- f2(mode2)

rejection2 <- function(n) {
  
  samples <- numeric(0)
  attempts <- 0
  
  while (length(samples) < n) {
    
    theta <- runif(1, 0, 1)
    u <- runif(1)
    
    attempts <- attempts + 1
    
    if (u <= f2(theta) / K2) {
      samples <- c(samples, theta)
    }
  }
  
  acceptance_rate <- n / attempts
  
  return(list(samples = samples,
              acceptance_rate = acceptance_rate))
}


result1 <- rejection1(5000)
result2 <- rejection2(5000)

result1$acceptance_rate
result2$acceptance_rate
