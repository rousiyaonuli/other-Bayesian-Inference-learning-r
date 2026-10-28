#Q1
mysimulated <- function(n) {
  U <- runif(n)
  X <- sqrt(2*U + 1/4) - 1/2
  return(X)
}

sample_data <- mysimulated(10000)

hist(sample_data, probability = TRUE)

theoretical <- function(t) {
  return(t + 1/2)
}

curve(theoretical, from = 0, to = 1, add = TRUE)


#Q2'
mysimulated2 <- function(n) {
  U <- runif(n)
  X <- ifelse(U < 0.5,
              1/2 * log(2*U),
              -1/2 * log(2 - 2*U))
  return(X)
}

sample_data2 <- mysimulated2(10000)

hist(sample_data2, probability = TRUE, ylim = c(0, 1.1))

theoreticalpdf <- function(t) {
  density <- ifelse(
    t < 0,
    exp(2*t),
    exp(-2*t))
  return(density)
}

curve(theoreticalpdf, add = TRUE)

#Q3
mysimulated3 <- function(n,lambda) {
  U <- runif(n)
  x <- -log(1-U) / lambda
  return(x)
}

sample_data3 <- mysimulated3(10000,2)

hist(sample_data3, probability = TRUE)

theoreticalexppdf <- function(t) {
  density <- 2*exp(-2*t)
  return(density)
}


curve(theoreticalexppdf,  add= TRUE)


#Q4 K，这题我设为5000次，k次exp distribution加起来 follow Gamma distribution

mysimulated3 <- function(n,lambda) {
  U <- runif(n)
  x <- -log(1-U) / lambda
  return(x)
}

sample_data4 <- replicate(
  5000,
  sum(mysimulated3(10, 2))
)

# Histogram
hist(sample_data4, probability = TRUE,breaks = 30)

# Theoretical Gamma(10,2) density
theoreticalgammapdf <- function(t) {
  density <- dgamma(t, shape = 10, rate = 2)
  return(density)
}
curve(theoreticalgammapdf, add = TRUE)






