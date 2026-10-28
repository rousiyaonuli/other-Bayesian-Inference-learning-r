#Q2 a
tuberculosis <- read.table("tuberculosis.txt")
names(tuberculosis) <- c("alpha", "c", "rho", "mu") 
#names(tuberculosis)=tub这个 data frame里的column name
head(tuberculosis)

hist(tuberculosis$alpha) #从tub里拿出alpha这一列
hist(tuberculosis$c)
hist(tuberculosis$rho)
hist(tuberculosis$mu)
#给这4个parameter分别生成historgram

pairs(tuberculosis) 
#“把这个tub data frame 里面所有 columns 两两配对，给我画 scatterplot matrix

#Q2 b 这题就是把sample放入这个phi里，然后看phi的值在哪里动荡，是否不靠近1
delta <- 0.52
tau <- 0.52
epsilon_S <- 0.52
epsilon_R <- 0.202
phi <- (1 - tuberculosis$c) *
  (1/tau + 1/(delta + epsilon_R)) /
  (1/tau + 1/(delta + epsilon_S + tuberculosis$rho))
hist(phi)
mean(phi<1) #phi<1多，true多，1多，mean大.phi<1少，true少，1少，mean小
#得到0.02，mean小，说明phi<1的很少

#2c 求rho的95%cl 0.025~0.975的区域就是0.95
CI_rho <- quantile(tuberculosis$rho, c(0.025,0.975))
CI_rho
CI_rho[2] - CI_rho[1] #CL_rho[1]= lower endpoint CL_rho[2]=higher endpoint



#2d 5000*0.95=4750
rho_sorted <- sort(tuberculosis$rho)#从小到大排

L <- numeric(250)
U <- numeric(250)
width <- numeric(250)

for (i in 1:250) {
  L[i] <- rho_sorted[i]
  U[i] <- rho_sorted[i + 4750]
  width[i] <- U[i] - L[i]
}

which.min(width)#min(width) → 最小的 width 是多少 which.min(width) → 最小的 width 在第几个位置

best <- which.min(width)
L[best]
U[best]
width[best]

#3a
l <- 1
d <- 2
N <- 1000 #数据是随意取的

x <- runif(N, 0, d/2) #random N number~ uniform(0,d/2)
theta <- runif(N, 0, pi/2)

hit <- x <= (l/2) * sin(theta) #所有不等号（比较符号）都会出来true/false

p_hat <- mean(hit) #mean里true=1，false=0
pi_hat <- 2*l/(d*p_hat)

p_hat
pi_hat

p_hat_N <- cumsum(hit) / (1:N) #前N次至今hit的数量/N=>得到1000个p hat


pi_hat_N <- 2*l / (d*p_hat_N)

plot(1:N, pi_hat_N, type = "l",
     xlab = "N",
     ylab = "Estimated pi")
#x=1~N，y=pi hat N,
#observation： N越大 simulation 越稳定。

#3c
N <- 1000
nrep <- 1000
d <- 1
rho <- 0.5
l <- rho * d

pi_estimates <- numeric(nrep)
for (i in 1:nrep) {
  
  x <- runif(N, 0, d/2)
  theta <- runif(N, 0, pi/2)
  
  hit <- x <= (l/2) * sin(theta)
  
  p_hat <- mean(hit)
  
  pi_estimates[i] <- 2*l/(d*p_hat)
}
sd(pi_estimates)

N <- 1000
nrep <- 1000
d <- 1

rho_values <- seq(0.1, 1, by = 0.1)

sd_values <- numeric(length(rho_values))

for (j in 1:length(rho_values)) {
  
  rho <- rho_values[j]
  l <- rho * d
  
  pi_estimates <- numeric(nrep)
  
  for (i in 1:nrep) {
    
    x <- runif(N, 0, d/2)
    theta <- runif(N, 0, pi/2)
    
    hit <- x <= (l/2) * sin(theta)
    
    p_hat <- mean(hit)
    
    pi_estimates[i] <- 2*l/(d*p_hat)
  }
  
  sd_values[j] <- sd(pi_estimates)
}

rho_values
sd_values

plot(rho_values, sd_values,
     type = "b",
     xlab = "rho",
     ylab = "SD of estimated pi")
