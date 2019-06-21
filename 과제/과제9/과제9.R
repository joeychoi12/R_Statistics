#Chi square 분포 
n <- 1000
df <- 3
chi.2.mean <- rep(NA,n)
chi.4.mean <- rep(NA,n)
chi.16.mean <- rep(NA,n)
chi.64.mean <- rep(NA ,n)
chi.256.mean <- rep(NA, n)

#표본 크기별로 1000번의 표본 추출로 표본 평균을 구함
for(i in 1:n) {
  chi.2.mean[i] <- mean(rchisq(2, df=df))
  chi.4.mean[i] <- mean(rchisq(4, df=df))
  chi.16.mean[i] <- mean(rchisq(16,df=df))
  chi.64.mean[i] <- mean(rchisq(64, df=df))
  chi.256.mean[i] <- mean(rchisq(256, df=df))
}

#표본 평균들의 분포에서 평균과 표준편차 
options(digits =4)
c(mean(ch.2.mean), sd(chi.2.mean))
c(mean(ch.4.mean), sd(chi.4.mean))
c(mean(ch.16.mean),sd(chi.16.mean))
c(mean(ch.64.mean),sd(chi.64.mean))

#chisq(df=3)의 평균과 표준 편차
m <- df
s <- sqrt(2 *df)

hist(chi.2.mean,prob=T, main = "표본 크기: 2", 
     ylab="",xlab="",col="orange")
x1 <- seq(min(chi.2.mean), max(chi.2.mean), length=1000)
y1 <- dnorm(x=x1, mean=m, sd= s/sqrt(2))
lines(x1, y1, lty=2, lwd=2,col="red")

par(mfrow=c(2,2))


hist(chi.4.mean,prob=T, main = "표본 크기: 4", 
     ylab="",xlab="",col="orange")
x2 <- seq(min(chi.4.mean), max(chi.4.mean), length=1000)
y2 <- dnorm(x=x2, mean=m, sd= s/sqrt(2))
lines(x1, y1, lty=2, lwd=2,col="green")

hist(chi.16.mean,prob=T, main = "표본 크기: 16", 
     ylab="",xlab="",col="orange")
x3 <- seq(min(chi.16.mean), max(chi.16.mean), length=1000)
y3 <- dnorm(x=x3, mean=m, sd= s/sqrt(16))
lines(x1, y1, lty=2, lwd=2,col="blue")

hist(chi.64.mean,prob=T, main = "표본 크기: 64", 
     ylab="",xlab="",col="orange")
x4 <- seq(min(chi.64.mean), max(chi.64.mean), length=1000)
y4 <- dnorm(x=x3, mean=m, sd= s/sqrt(64))
lines(x1, y1, lty=2, lwd=2,col="pink")

hist(chi.256.mean,prob=T, main = "표본 크기: 256", 
      ylab="",xlab="",col="orange")
x5 <- seq(min(chi.256.mean), max(chi.256.mean), length=1000)
y5 <- dnorm(x=x3, mean=m, sd= s/sqrt(256))
lines(x5, y5, lty=2, lwd=2,col="yellow")


#T distribution
n <- 1000
df <- 3
t.2.mean <- rep(NA,n)
t.4.mean <-rep(NA,n)
t.8.mean <- rep(NA,n)
t.16.mean <- rep(NA,n)


#표본 크기별로 1000번의 표본 추출로 표본 평균을 구함
for(i in 1:n) {
  t.2.mean[i] <- mean(rt(2, df=df))
  t.4.mean[i] <- mean(rt(4, df=df))
  t.8.mean[i] <- mean(rt(8,df=df))
  t.16.mean[i] <- mean(rt(16, df=df))
}

#표본 평균들의 분포에서 평균과 표준 편차 
c(mean(t.2.mean), sd(t.2.mean))
c(mean(t.4.mean), sd(t.4.mean))
c(mean(t.8.mean), sd(t.8.mean))
c(mean(t.16.mean), sd(t.16.mean))

#t (df = 3)의 평균과 표준 편차 
m <-0
s <- sqrt(df/(df - 2))

par(mfrow = c(2,2))
hist(t.2.mean, prob = T, main = "표본 크기: 2",
     ylab="",xlab="",col="orange")
x1 <- seq(min(t.2.mean), max(t.2.mean), length = 1000)
y2 <- dnorm(x=x1, mean=m, sd=s/sqrt(2))
lines(x1,y1,lty = 2,lwd=2,col = "red")

hist(t.4.mean, prob = T, main = "표본 크기: 4",
     ylab="",xlab="",col="orange")
x1 <- seq(min(t.4.mean), max(t.4.mean), length = 1000)
y2 <- dnorm(x=x1, mean=m, sd=s/sqrt(4))
lines(x1,y1,lty = 2,lwd=2,col = "orange")

hist(t.8.mean, prob = T, main = "표본 크기: 8",
     ylab="",xlab="",col="orange")
x1 <- seq(min(t.8.mean), max(t.8.mean), length = 1000)
y2 <- dnorm(x=x1, mean=m, sd=s/sqrt(8))
lines(x1,y1,lty = 2,lwd=2,col = "yellow")

hist(t.16.mean, prob = T, main = "표본 크기: 16",
     ylab="",xlab="",col="orange")
x1 <- seq(min(t.16.mean), max(t.16.mean), length = 1000)
y2 <- dnorm(x=x1, mean=m, sd=s/sqrt(16))
lines(x1,y1,lty = 2,lwd=2,col = "green")

# F distribution 
n <- 100
df1 <- 3
df2 <- 5
f.4.mean <- rep(NA,n)
f.16.mean <- rep(NA,n)
f.64.mean <- rep(NA,n)
f.256.mean <- rep(NA,n)

for (i in 1:n) {
  f.4.mean[i] <- mean(rf(4, df1 = df1, df2=df2))
  f.16.mean[i] <- mean(rf(16, df1 = df1, df2=df2))
  f.64.mean[i] <- mean(rf(64, df1 = df1, df2=df2))
  f.256.mean[i] <- mean(rf(256, df1 = df1, df2=df2))
}

#f(df1=3,df2=5)의 평균과 표준편차 
m <- df2/(df2-2)
s <- sqrt(2*df2^2 * (df1 + df2-2) / (df1 * (df2-2)^2 * (df2-4)))

par(mfrow=c(2,2))
hist(f.4.mean,prob=T, main = "표본 크기: 4", xlim = c(0,10), ylab="",xlab="")
x1 <- seq(min(f.4.mean), max(f.4.mean),length = 1000)
y1 <- dnorm(x=x1,mean=m,sd=s/sqrt(4))
lines(x1,y1,lt=2,lwd=2,col="red")


hist(f.16.mean,prob=T, main = "표본 크기: 16", xlim = c(0,8), ylab="",xlab="")
x1 <- seq(min(f.16.mean), max(f.16.mean),length = 1000)
y1 <- dnorm(x=x1,mean=m,sd=s/sqrt(16))
lines(x1,y1,lt=2,lwd=2,col="red")

hist(f.64.mean,prob=T, main = "표본 크기: 64", xlim = c(0,4), ylab="",xlab="")
x1 <- seq(min(f.64.mean), max(f.64.mean),length = 1000)
y1 <- dnorm(x=x1,mean=m,sd=s/sqrt(64))
lines(x1,y1,lt=2,lwd=2,col="red")

hist(f.256.mean,prob=T, main = "표본 크기: 256", xlim = c(0,3), ylab="",xlab="")
x1 <- seq(min(f.256.mean), max(f.256.mean),length = 1000)
y1 <- dnorm(x=x1,mean=m,sd=s/sqrt(256))
lines(x1,y1,lt=2,lwd=2,col="red")
