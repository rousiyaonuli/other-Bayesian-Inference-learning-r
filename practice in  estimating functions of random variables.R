# Q1 a
n=65; sumx=24890
alpha=1; beta=0.01
pmean=(alpha+sumx)/(beta+n)
#qgmma 里的scale和rate是公式化的
L=qgamma(0.025,shape = alpha+sumx,rate = beta+n) #(note specification as scale and rate)
U=qgamma(0.975,shape = alpha+sumx,rate = beta+n)
cat("Posterior mean: ",pmean," \n95% central CL: (",L,",",U,")")
 



# Q1 b
N <- 500
theta_sample <- rgamma(N,shape = 24891,rate = 65.01) # rgamma = random generiate数字 from gamma distribution
L_b <- quantile(theta_sample, 0.025)
U_b <- quantile(theta_sample, 0.975)
CL <- cat(L_b,U_b)
#到这里完成了一组，这一组有500个按照gamma distribution生成的随机数字。并根据这些数字生成了cl
nrep <- 500
L_MC <- numeric(nrep) #numeric=准备空位
U_MC <- numeric(nrep)
for (i in 1:nrep) {
  
  theta_sample <- rgamma(N,shape = alpha + sumx,rate = beta + n)
  
  CI <- quantile(theta_sample, c(0.025, 0.975))
  
  L_MC[i] <- CI[1]
  U_MC[i] <- CI[2]
}
#到这里把上半部分重复了500次，并得到L,U的各500个数据
hist(L_MC)
points(L, 0) #Superimpose a point corresponding to the true interval endpoint,也就是a中的理论值
hist(U_MC)
points(U, 0) #point(U,0)->x-cooridinate is u,y-cooridinate is 0
cat("L interval variability (range):",
    max(L_MC)-min(L_MC), "\n")
cat("U interval variability (range):",
    max(U_MC)-min(U_MC), "\n")
#把L，U的各500组数据用histgram plot出，可以看到L，U这500次数据的分布，并且和理论值作比较。
#observation：500 次 Monte Carlo 得到的 endpoint estimates 分布在 theoretical endpoint 附近。
N2 <- 80000
L_MC2 <- numeric(nrep) #numeric=准备空位
U_MC2 <- numeric(nrep)
for (i in 1:nrep) {
  
  theta_sample <- rgamma(N2,shape = alpha + sumx,rate = beta + n)
  
  L_MC2[i] <- quantile(theta_sample, 0.025)
  U_MC2[i] <- quantile(theta_sample, 0.975)
}
hist(L_MC2)
points(L, 0) 

hist(U_MC2)
points(U, 0)

cat("L2 interval variability (range):",
    max(L_MC2)-min(L_MC2), "\n")
cat("U2 interval variability (range):",
    max(U_MC2)-min(U_MC2), "\n")
#现在取5000个数据
#很窄 → 每次 estimate 都差不多 → 更 precise
#很宽 → 每次 estimate 差很多 → less precise
#N越大→每一次quantile估计越稳定→500次结果更集中→max−min通常更小

#Q1 c
#路线 A：Monte Carlo,通过实验找规律：取一大堆theta->得到一大堆y->近似π(y∣x)
N <- 5000

theta_sample <- rgamma(N,shape = alpha + sumx,rate = beta + n)
y_sample <- rpois(N, lambda = theta_sample) #rpois=random generation from Poisson distribution
#lambda=泊松分布（Poisson Distribution）里的 λ
hist(y_sample, probability = TRUE)

?dnbinom # density/PMF of Negative Binomial
y_values <- 300:470 #Y=300,Y=301...Y=400
exact_probability <- dnbinom(y_values,size = alpha + sumx,prob = (beta + n)/(beta + n + 1))
#P(Y=300),P(Y=301)...P(Y=400)且follow NegBin PMF
points(y_values, exact_probability) #points() 只是往已经存在的图上加东西,和之前通过实验得到的Monte Carlo做对比
# observation：Monte Carlo≈Negative Binomial exact result

#路线 B：Exact mathematical result
#题目已经告诉你：y∣x∼NegBin(a,p)所以我们可以直接用 Negative Binomial 的 PMF。



