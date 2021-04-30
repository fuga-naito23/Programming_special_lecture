n = 20
out_mean = numeric(10000)
set.seed(1234)

for (i in 1:10000) {
  m = rnorm(n=n, mean = 120, sd = 10)
  out_mean[i] = m
}
hist(out_mean,col='orange',xlim=c(60,180)) 
sd(out_mean)

n = 200
for (i in 1:10000) {
  m = rnorm(n=n, mean = 120, sd = 10)
  out_mean[i] = m
}

hist(out_mean,col='orange',xlim=c(60,180)) 
sd(out_mean)
