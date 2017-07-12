

# Normal Distribution Simulations
norm.dist = function( n ) {
  m= 1000
  sd.est = c(); iqr.est = c(); mean.est = c(); mr.est =c(); median.est =c();
  for(i in 1:m) {
    x = rnorm(n)
    iqr.est[i] = IQR(x)
    mr.est[i] = (max(x)+min(x))/2
    mean.est [i] = mean(x)
    median.est[i] = median(x)
  }
  cat ("MEANS: ", "Mean =", mean(mean.est),"Median = ", mean(median.est), "Midrange =", mean(mr.est), "IQR = ", mean(iqr.est))
  cat (" STANDARD DEVIATIONS: ", "Mean =", sd(mean.est),"Median = ", sd(median.est), "Midrange =", sd(mr.est), "IQR = ", sd(iqr.est))
  cat (" MEDIANS: ", "Mean =", median(mean.est),"Median = ", median(median.est), "Midrange =", median(mr.est), "IQR = ", median(iqr.est))
  
  data.frame(mean.est, median.est, iqr.est, mr.est)
  
}

par (mfrow = c(1,1))
norm.10 = norm.dist(10)
attach(norm.10)
boxplot(mean.est, median.est, mr.est, iqr.est, names = c("Mean", "Median", "Midrange", "IQR"), ylab = "Estimate", main = "
        Sampling Distributions of N(0,1) Mean, Median., Midrange, and IQR with n=10")

plot(density(mean.est), xlim=range(-1:3), xlab = "Mean", yaxs =
       "i", ylim= range(0:2.5), main = "Sampling Distribution of  X ~ N(0,1), n=10")

lines(density(median.est), lty = 2)
lines(density(iqr.est), lty = 3)
lines(density(mr.est), lty = 4)
legend(2.0, .85, c("Median","IQR","Midrange", "Mean"), lty = c(2, 3, 4, 1))

pairs (norm.10, main = "Pair Plot of N(0,1) with n=10")
detach(norm.10)

par (mfrow = c(1,1))
norm.30 = norm.dist(30)
attach(norm.30)
boxplot(mean.est, median.est, mr.est, iqr.est, names = c("Mean", "Median", "Midrange", "IQR"), ylab = "Estimate", main = "
        Sampling Distributions of N(0,1) Mean, Std.Dev., Midrange, and IQR with n=30")

plot(density(mean.est), xlim=range(-1:3), xlab = "Mean", yaxs =
       "i", ylim= range(0:3), main = "Sampling Distribution of  X ~ N(0,1) with n=30")

lines(density(median.est), lty = 2)
lines(density(iqr.est), lty = 3)
lines(density(mr.est), lty = 4)
legend(2.0, 1.75, c("Median","IQR","Midrange", "Mean"), lty = c(2, 3, 4, 1))

pairs (norm.30, main = "Pair Plot of N(0,1) with n=30")
detach(norm.30)

par (mfrow = c(1,1))
norm.100 = norm.dist(100)
attach(norm.100)
boxplot(mean.est, median.est, mr.est, iqr.est, names = c("Mean", "Median", "Midrange", "IQR"), ylab = "Estimate", main = "
        Sampling Distributions of N(0,1) Mean, Median, Midrange, and IQR with N=100")

plot(density(mean.est), xlim=range(-1:3), xlab = "Mean", yaxs =
       "i",ylim= range(0:6), main = "Sampling Distribution of  X ~ N(0,1) with n=100")

lines(density(median.est), lty = 2)
lines(density(iqr.est), lty = 3)
lines(density(mr.est), lty = 4)
legend(2.0, 3, c("Median","IQR","Midrange", "Mean"), lty = c(2, 3, 4, 1))

pairs (norm.100, main = "Pair Plot of  X ~ N(0,1) with n=100")
detach(norm.100)

#Uniform Distribution Simulations
unif.dist = function( n ) {
  m= 1000
  sd.est = c(); iqr.est = c(); mean.est = c(); mr.est =c(); median.est =c();
  for(i in 1:m) {
    x = runif(n, 0, 1)
    iqr.est[i] = IQR(x)
    mr.est[i] = (max(x)+min(x))/2
    mean.est [i] = mean(x)
    median.est[i] = median(x)
  }
  cat ("MEANS: ", "Mean =", mean(mean.est),"Median = ", mean(median.est), "Midrange =", mean(mr.est), "IQR = ", mean(iqr.est))
  cat (" STANDARD DEVIATIONS: ", "Mean =", sd(mean.est),"Median = ", sd(median.est), "Midrange =", sd(mr.est), "IQR = ", sd(iqr.est))
  cat (" MEDIANS: ", "Mean =", median(mean.est),"Median = ", median(median.est), "Midrange =", median(mr.est), "IQR = ", median(iqr.est))
  
  data.frame(mean.est, median.est, iqr.est, mr.est)
}

par (mfrow = c(1,1))
n=10
unif.10 = unif.dist(n)
attach(unif.10)
boxplot(mean.est, median.est, mr.est, iqr.est, names = c("Mean", "Median", "Midrange", "IQR"), ylab = "Estimate", main = "
        Sampling Distributions of Mean, Median, Midrange, and IQR n=10")

plot(density(mean.est), xlim=range(0:1.5), xlab = "Mean", yaxs =
       "i", ylim= range(0:n), main = "Sampling Distribution of  X ~ Unif(0,1) n=10")

lines(density(median.est), lty = 2)
lines(density(iqr.est), lty = 3)
lines(density(mr.est), lty = 4)
legend(.7, (n-2), c("Median","IQR","Midrange", "Mean"), lty = c(2, 3, 4, 1))

pairs (unif.10, main = "Pair Plot of of  X ~ Unif(0,1) n=10")
detach(unif.10)

par (mfrow = c(1,1))
n=30
unif.30 = unif.dist(n)
attach(unif.30)
boxplot(mean.est, median.est, mr.est, iqr.est, names = c("Mean", "Median", "Midrange", "IQR"), ylab = "Estimate", main = "
        Sampling Distributions of Mean, Median., Midrange, and IQR n=30")

plot(density(mean.est), xlim=range(0:1.5), xlab = "Mean", yaxs =
       "i", ylim= range(0:n), main = "Sampling Distribution of  X ~ Unif(0,1) n=30")

lines(density(median.est), lty = 2)
lines(density(iqr.est), lty = 3)
lines(density(mr.est), lty = 4)
legend(.7, (n-2), c("Median","IQR","Midrange", "Mean"), lty = c(2, 3, 4, 1))

pairs (unif.30, main = "Pair Plot of of  X ~ Unif(0,1) n=30")
detach(unif.30)

par (mfrow = c(1,1))
n=100
unif.100 = unif.dist(n)
attach(unif.100)
boxplot(mean.est, median.est, mr.est, iqr.est, names = c("Mean", "Median", "Midrange", "IQR"), ylab = "Estimate", main = "
        Sampling Distributions of Mean, Median, Midrange, and IQR n=100")

plot(density(mean.est), xlim=range(0:1.5), xlab = "Mean", yaxs =
       "i", ylim= range(0:80), main = "Sampling Distribution of  X ~ Unif(0,1) n=100")

lines(density(median.est), lty = 2)
lines(density(iqr.est), lty = 3)
lines(density(mr.est), lty = 4)
legend(.7, 76, c("Median","IQR","Midrange", "Mean"), lty = c(2, 3, 4, 1))

pairs (unif.100, main = "Pair Plot of of  X ~ Unif(0,1) n=100")
detach(unif.100)

#Exponential Distribution Simulations

exp.dist = function( n ) {
  m= 1000
  sd.est = c(); iqr.est = c(); mean.est = c(); mr.est =c(); median.est =c();
  for(i in 1:m) {
    x = rexp(n, 1)
    iqr.est[i] = IQR(x)
    mr.est[i] = (max(x)+min(x))/2
    mean.est [i] = mean(x)
    median.est[i] = median(x)
  }
  cat ("MEANS: ", "Mean =", mean(mean.est),"Median = ", mean(median.est), "Midrange =", mean(mr.est), "IQR = ", mean(iqr.est))
  cat (" STANDARD DEVIATIONS: ", "Mean =", sd(mean.est),"Median = ", sd(median.est), "Midrange =", sd(mr.est), "IQR = ", sd(iqr.est))
  cat (" MEDIANS: ", "Mean =", median(mean.est),"Median = ", median(median.est), "Midrange =", median(mr.est), "IQR = ", median(iqr.est))
  data.frame(mean.est, median.est, iqr.est, mr.est)
}

par (mfrow = c(1,1))
n=10
exp.10 = exp.dist(n)
attach(exp.10)
boxplot(mean.est, median.est, mr.est, iqr.est, names = c("Mean", "Median", "Midrange", "IQR"), ylab = "Estimate", main = "
        Sampling Distributions of Mean, Median., Midrange, and IQR n =10")

plot(density(mean.est), xlim=range(0:2), xlab = "Mean", yaxs =
       "i", ylim= range(0:n), main = "Sampling Distribution of  X ~ Exp(1) n=10")

lines(density(median.est), lty = 2)
lines(density(iqr.est), lty = 3)
lines(density(mr.est), lty = 4)
legend(.7, (n-2), c("Median","IQR","Midrange", "Mean"), lty = c(2, 3, 4, 1))

pairs (exp.10, main ="Pair Plot of of  X ~ Exp(1) n=10")
detach(exp.10)

par (mfrow = c(1,1))
n=30
exp.30 = exp.dist(n)
attach(exp.30)
boxplot(mean.est, median.est, mr.est, iqr.est, names = c("Mean", "Median", "Midrange", "IQR"), ylab = "Estimate", main = "
        Sampling Distributions of Mean, Median., Midrange, and IQR n =30")

plot(density(mean.est), xlim=range(0:4), xlab = "Mean", yaxs =
       "i", ylim= range(0:10), main = "Sampling Distribution of  X ~ Exp(1) n =30 ")

lines(density(median.est), lty = 2)
lines(density(iqr.est), lty = 3)
lines(density(mr.est), lty = 4)
legend(.7, 8, c("Median","IQR","Midrange", "Mean"), lty = c(2, 3, 4, 1))

pairs (exp.30, main ="Pair Plot of of  X ~ Exp(1) n=30")
detach(exp.30)

par (mfrow = c(1,1))
n=100
exp.100 = exp.dist(n)
attach(exp.100)
boxplot(mean.est, median.est, mr.est, iqr.est, names = c("Mean", "Median", "Midrange", "IQR"), ylab = "Estimate", main = "
        Sampling Distributions of Mean, Median, Midrange, and IQR n=100")

plot(density(mean.est), xlim=range(0:4), xlab = "Mean", yaxs =
       "i", ylim= range(0:10), main = "Sampling Distribution of  X ~ Exp(1) n=100")

lines(density(median.est), lty = 2)
lines(density(iqr.est), lty = 3)
lines(density(mr.est), lty = 4)
legend(.7, 8, c("Median","IQR","Midrange", "Mean"), lty = c(2, 3, 4, 1))

pairs (exp.100, main ="Pair Plot of of  X ~ Exp(1) n=100")
detach(exp.100)
