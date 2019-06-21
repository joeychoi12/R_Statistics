# Chi Square 분포
n <- 1000
df <- 3
ch.2.mean <- rep(NA, n)
ch.4.mean <- rep(NA, n)
ch.16.mean <- rep(NA, n)
ch.64.mean <- rep(NA, n)

# 표본 크기별로 1000번의 표본추출로 표본평균을 구함
for(i in 1:n) {
  ch.2.mean[i] <- mean(rchisq(2, df=df))
  ch.4.mean[i] <- mean(rchisq(4, df=df))
  ch.16.mean[i] <- mean(rchisq(16, df=df))
  ch.64.mean[i] <- mean(rchisq(64, df=df))
}

# 표본평균들의 분포에서 평균과 표준편차
options(digits=4)
c(mean(ch.2.mean), sd(ch.2.mean))
c(mean(ch.4.mean), sd(ch.4.mean))
c(mean(ch.16.mean), sd(ch.16.mean))
c(mean(ch.64.mean), sd(ch.64.mean))

# chisq(df=3)의 평균과 표준편차
m <- df
s <- sqrt(2 * df)

par(mfrow=c(2,2), oma = c(0, 0, 2, 0))
hist(ch.2.mean, prob=T, main="표본 크기 : 2", 
     ylab="", xlab="", col="orange", border="red")
x1 <- seq(min(ch.2.mean), max(ch.2.mean), length=1000)
y1 <- dnorm(x=x1, mean=m, sd=s/sqrt(2))
lines(x1, y1, lty=2, lwd=2, col="blue")

hist(ch.4.mean, prob=T, main="표본 크기 : 4", 
     ylab="", xlab="", col="orange", border="red")
x2 <- seq(min(ch.4.mean), max(ch.4.mean), length=1000)
y2 <- dnorm(x=x2, mean=m, sd=s/sqrt(4))
lines(x2, y2, lty=2, lwd=2, col="blue")

hist(ch.16.mean, prob=T, main="표본 크기 : 16", 
     ylab="", xlab="", col="orange", border="red")
x3 <- seq(min(ch.16.mean), max(ch.16.mean), length=1000)
y3 <- dnorm(x=x3, mean=m, sd=s/sqrt(16))
lines(x3, y3, lty=2, lwd=2, col="blue")

hist(ch.64.mean, prob=T, main="표본 크기 : 64", 
     ylab="", xlab="", col="orange", border="red")
x4 <- seq(min(ch.64.mean), max(ch.64.mean), length=1000)
y4 <- dnorm(x=x4, mean=m, sd=s/sqrt(64))
lines(x4, y4, lty=2, lwd=2, col="blue")

mtext("Chi Square 표본평균 분포(df=3인 경우)", outer = TRUE, cex = 1.2)

# T 분포
n <- 1000
df <- 3
t.2.mean <- rep(NA, n)
t.4.mean <- rep(NA, n)
t.8.mean <- rep(NA, n)
t.16.mean <- rep(NA, n)

# 표본 크기별로 1000번의 표본추출로 표본평균을 구함
for(i in 1:n) {
  t.2.mean[i] <- mean(rt(2, df=df))
  t.4.mean[i] <- mean(rt(4, df=df))
  t.8.mean[i] <- mean(rt(8, df=df))
  t.16.mean[i] <- mean(rt(16, df=df))
}

# 표본평균들의 분포에서 평균과 표준편차
options(digits=4)
c(mean(t.2.mean), sd(t.2.mean))
c(mean(t.4.mean), sd(t.4.mean))
c(mean(t.8.mean), sd(t.8.mean))
c(mean(t.16.mean), sd(t.16.mean))

# t(df=3)의 평균과 표준편차
m <- 0
s <- sqrt(df / (df - 2))

par(mfrow=c(2,2), oma = c(0, 0, 2, 0))
hist(t.2.mean, prob=T, main="표본 크기 : 2", 
     ylab="", xlab="", col="orange", border="red")
x1 <- seq(min(t.2.mean), max(t.2.mean), length=1000)
y1 <- dnorm(x=x1, mean=m, sd=s/sqrt(2))
lines(x1, y1, lty=2, lwd=2, col="blue")

hist(t.4.mean, prob=T, main="표본 크기 : 4", 
     ylab="", xlab="", col="orange", border="red")
x2 <- seq(min(t.4.mean), max(t.4.mean), length=1000)
y2 <- dnorm(x=x2, mean=m, sd=s/sqrt(4))
lines(x2, y2, lty=2, lwd=2, col="blue")

hist(t.8.mean, prob=T, main="표본 크기 : 8", 
     ylab="", xlab="", col="orange", border="red")
x3 <- seq(min(t.8.mean), max(t.8.mean), length=1000)
y3 <- dnorm(x=x3, mean=m, sd=s/sqrt(8))
lines(x3, y3, lty=2, lwd=2, col="blue")

hist(t.16.mean, prob=T, main="표본 크기 : 16", 
     xlim=c(-2,2), ylim=c(0,0.9), 
     ylab="", xlab="", col="orange", border="red")
x4 <- seq(min(t.16.mean), max(t.16.mean), length=1000)
y4 <- dnorm(x=x4, mean=m, sd=s/sqrt(16))
lines(x4, y4, lty=2, lwd=2, col="blue")

mtext("T 표본평균 분포(df=3인 경우)", outer = TRUE, cex = 1.2)

# F 분포
n <- 1000
df1 <- 3; df2 <- 5
f.4.mean <- rep(NA, n)
f.16.mean <- rep(NA, n)
f.64.mean <- rep(NA, n)
f.256.mean <- rep(NA, n)

# 표본 크기별로 1000번의 표본추출로 표본평균을 구함
for(i in 1:n) {
  f.4.mean[i] <- mean(rf(4, df1=df1, df2=df2))
  f.16.mean[i] <- mean(rf(16, df1=df1, df2=df2))
  f.64.mean[i] <- mean(rf(64, df1=df1, df2=df2))
  f.256.mean[i] <- mean(rf(256, df1=df1, df2=df2))
}

# 표본평균들의 분포에서 평균과 표준편차
options(digits=4)
c(mean(f.4.mean), sd(f.4.mean))
c(mean(f.16.mean), sd(f.16.mean))
c(mean(f.64.mean), sd(f.64.mean))
c(mean(f.256.mean), sd(f.256.mean))

# f(df1=3, df2=5)의 평균과 표준편차
m <- df2 / (df2 - 2)
s <- sqrt(2 * df2^2 * (df1+df2-2) /(df1 * (df2-2)^2 * (df2-4)))

par(mfrow=c(2,2), oma = c(0, 0, 2, 0))
hist(f.4.mean, prob=T, main="표본 크기 : 4", 
     xlim=c(0,10),
     ylab="", xlab="", col="orange", border="red")
x1 <- seq(min(f.4.mean), max(f.4.mean), length=1000)
y1 <- dnorm(x=x1, mean=m, sd=s/sqrt(4))
lines(x1, y1, lty=2, lwd=2, col="blue")

hist(f.16.mean, prob=T, main="표본 크기 : 16", 
     xlim=c(0,6),
     ylab="", xlab="", col="orange", border="red")
x2 <- seq(min(f.16.mean), max(f.16.mean), length=1000)
y2 <- dnorm(x=x2, mean=m, sd=s/sqrt(16))
lines(x2, y2, lty=2, lwd=2, col="blue")

hist(f.64.mean, prob=T, main="표본 크기 : 64", 
     xlim=c(0.4,4),
     ylab="", xlab="", col="orange", border="red")
x3 <- seq(min(f.64.mean), max(f.64.mean), length=1000)
y3 <- dnorm(x=x3, mean=m, sd=s/sqrt(64))
lines(x3, y3, lty=2, lwd=2, col="blue")

hist(f.256.mean, prob=T, main="표본 크기 : 256", 
     xlim=c(0.8,3),
     ylab="", xlab="", col="orange", border="red")
x4 <- seq(min(f.256.mean), max(f.256.mean), length=1000)
y4 <- dnorm(x=x4, mean=m, sd=s/sqrt(256))
lines(x4, y4, lty=2, lwd=2, col="blue")

mtext("F 표본평균 분포(df1=3, df5=5인 경우)", outer = TRUE, cex = 1.2)

# 포아송(Poisson) 분포
n <- 1000
lambda <- 3
p.4.mean <- rep(NA, n)
p.16.mean <- rep(NA, n)
p.64.mean <- rep(NA, n)
p.256.mean <- rep(NA, n)

# 표본 크기별로 1000번의 표본추출로 표본평균을 구함
for(i in 1:n) {
  p.4.mean[i] <- mean(rpois(4, lambda=lambda))
  p.16.mean[i] <- mean(rpois(16, lambda=lambda))
  p.64.mean[i] <- mean(rpois(64, lambda=lambda))
  p.256.mean[i] <- mean(rpois(256, lambda=lambda))
}

# 표본평균들의 분포에서 평균과 표준편차
options(digits=4)
c(mean(p.4.mean), sd(p.4.mean))
c(mean(p.16.mean), sd(p.16.mean))
c(mean(p.64.mean), sd(p.64.mean))
c(mean(p.256.mean), sd(p.256.mean))

# pois(lambda=3)의 평균과 표준편차
m <- lambda
s <- sqrt(lambda)

par(mfrow=c(2,2), oma = c(0, 0, 2, 0))
hist(p.4.mean, prob=T, main="표본 크기 : 4", 
     #xlim=c(0,10),
     ylab="", xlab="", col="orange", border="red")
x1 <- seq(min(p.4.mean), max(p.4.mean), length=1000)
y1 <- dnorm(x=x1, mean=m, sd=s/sqrt(4))
lines(x1, y1, lty=2, lwd=2, col="blue")

hist(p.16.mean, prob=T, main="표본 크기 : 16", 
     #xlim=c(0,6),
     ylab="", xlab="", col="orange", border="red")
x2 <- seq(min(p.16.mean), max(p.16.mean), length=1000)
y2 <- dnorm(x=x2, mean=m, sd=s/sqrt(16))
lines(x2, y2, lty=2, lwd=2, col="blue")

hist(p.64.mean, prob=T, main="표본 크기 : 64", 
     #xlim=c(0.4,4),
     ylab="", xlab="", col="orange", border="red")
x3 <- seq(min(p.64.mean), max(p.64.mean), length=1000)
y3 <- dnorm(x=x3, mean=m, sd=s/sqrt(64))
lines(x3, y3, lty=2, lwd=2, col="blue")

hist(p.256.mean, prob=T, main="표본 크기 : 256", 
     #xlim=c(0.8,3),
     ylab="", xlab="", col="orange", border="red")
x4 <- seq(min(p.256.mean), max(p.256.mean), length=1000)
y4 <- dnorm(x=x4, mean=m, sd=s/sqrt(256))
lines(x4, y4, lty=2, lwd=2, col="blue")

mtext("포아송 표본평균 분포(lambda=3인 경우)", outer = TRUE, cex = 1.2)

par(mfrow=c(1,1), oma = c(0, 0, 0, 0))