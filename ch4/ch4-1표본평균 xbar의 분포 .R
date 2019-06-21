#4장 표본 분포 Sample Distribution 
#4-1. 표본평균 xbar의 분포 
#Var(X) = E(X^2) - E(x)^2
m10 <- rep(NA,1000)
m40 <- rep(NA,1000)

#Using set.seed to get the same value
set.seed(1)
rbinom(10, 30, 0.5)
set.seed(1)
rbinom(10, 30, 0.5)

set.seed(9)
for(i in 1:1000) {
  m10[i] <-mean(rnorm(10))
  m40[i] <-mean(rnorm(40))
}

#표본 평균과 표준 편차 구하기 
options(digits = 4)
c(mean(m10), sd(m10))
c(mean(m40), sd(m40))


hist(m10,xlim = c(-1.5,1.5), main="", xlab="x",ylab="",
     col = "cyan", border="blue")
hist(m40,xlim = c(-1.5,1.5), main="", xlab="x",ylab="",
     col = "cyan", border="blue")

dev.off()
par(mfrow=c(1,2))
par(mar=c(1,1,1,1)
hist(m10,xlim = c(-1.5,1.5), main="", xlab="x",ylab="",
     col = "cyan", border="blue")
hist(m40,xlim = c(-1.5,1.5), main="", xlab="x",ylab="",
     col = "cyan", border="blue")
