#5장. 추정(Estimation)
#5-1 점 추정 
#예제 5-2 유효성 모의 실험 
#21bar의 기대값을 구하기 위한 함수 

options(digits = 3)
set.seed(1)
mean.seq <- function(x) {
  n <- length(x)
  sum <- 0
  n2 <- 0 
  for (i in 1:n) {
    newx <- i * x[i]
    sum <- sum + newx
    n2 <- n2 + i 
  }
  return(sum/n2)
}

y1 <- rep(NA,1000)
y2 <- rep(NA,1000)
for(i in 1:1000) {
  smp <- rnorm(3)
  y1[i] <- mean(smp)
  y2[i] <- mean.seq(smp)
}

dev.off()
par("mar")
par(mar=c(1,1,1,1))
n1 <- length(y1[(y1 > -0.1) & (y1 < 0.1)])
n2 <- length(y1[(y2 > -0.1) & (y2 < 0.1)])
data.frame(mean= mean(y1), var=var(y1), n=n1)
data.frame(mean= mean(y2), var=var(y2), n=n2)

par(mfrow =c( 1,2))
hist(y1, probability = T,xlim=c(-2,2), ylim=c(0,0.65),
     main = "(x1 + x2+x3)/3", xlab="",col="orange",border="red")
hist(y2, probability = T, xlim=c(-2,2), ylim = c(0,0.65),
     main="(1*x1+2*x2+6*x3)/6",xlab="", col="orange", border = "red")
dev.off()     
par(mfrow = c(1,1))
#예제 5-1 유효성
x <- seq(-3,3,by =0.01)
y <- dnorm(x)
y.1 <- dnorm(x, sd=sqrt(1/3))
y.2 <- dnorm(x, sd=sqrt(7/18))
pnorm(0.1, sd=sqrt(1/3)) - pnrom(-.01, sd=sqrt(1/3))
pnorm(0.1,sd = sqrt(7/18)) - pnorm(-0.1, sd = sqrt(7/18))
plot(x, y, col = "yellow", lwd=3, ylim = c(0,0.9))
lines(x,y.1,col = "red", lwd=3)
lines(x,y.2, col = "green",lty=2,lwd=3)
axis(1)

#5-3 모비율의 추정 
library(prob)
n <- 3 
smps.all <- rolldie(n)
str(smps.all)
head(smps.all, n=3)

is.even <- function(x) return(!x%%2)
var.p <- function(x) {
  return(sum ((x-mean(x))^2 / length(x)))
}
p.even <- function(x, s.size =3) {
  return(sum(is.even(x))/s.size)
}

phat <- apply(smps.all, 1, p.even)

mean(phat)
(p.p <- 0.5)
var.p(phat)
(p.p*(1-p.p)/3)
sqrt(var.p(phat))   



#예제 5-4 모평균에 대한 95% 신뢰구간 
set.seed(9)
n <- 10
x <- 1:100
y <- seq(-3,3,by = 0.01)

smps <- matrix(rnorm(n * length(x)), ncol = n)

xbar <- apply(smps, 1,mean)
se <- 1/sqrt(10)
alpha <- 0.05
z <- qnorm(1 - alpha/2)
ll <- xbar - z* se
ul <- xbar + z* se 


#예제 5-5 모평균에 대한 95% 신뢰구간(모분산을 모를때)
ci.t <- function(x, alpha = 0.05) {
  n <- length(smp)
  m <- mean(x)
  s <- sd(x)
  t <- qt(1-(alpha/2), df=n-1)
  ll <- m-t*(s/sqrt(n))
  ul <- m+t*(s/sqrt(n))
  ci <- c(1-alpha,ll,m,ul)
  names(ci) <- c("Confidence Level","Lower Limit","Mean","Upper Limit")
  return(ci)
}

smp <- c(520,498,481,512,515,542,520,518,527,524)
ci.t(smp)
ci.t(smp,0.1) 