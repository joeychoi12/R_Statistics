#과제 10 R 연습문제 6-7
#6 모비율 추정 

#1
#어느 대학교에서 대중교통을 이용하여 등교하는 학생의 비율을 알아보기 위하여 이 학교 학생 중 n 명을 임의 추출하여 조사한 결과 50%의 학생이 대중교통을 이용하여 등교하는것으로 나타났다. 이 결과를 이용하여 이 대학교 전체 학생 중에서 대중교통을 이용하여 등교하는 학생의 비율 p에 대한 신뢰도 95%의 신뢰 구간을 구하시오.

phat <- 0.5
alpha <- 0.05
z <- qnorm(1-alpha/2)
ll <- phat - z*(phat*(1-phat)/n)
ul <- phat + z*(phat*(1-phat)/n)
(c.i <-c(ll,ul))
#-1.96 <= Z <= 1.96

#2 어느 음식점에서 새로운 메뉴를 개발하여 이 메뉴에 대한 선호도를 조사하기로 하였다.
#고객 100명을 임의추출하여 이 메뉴에 대한 반응을 조사하였더니 이들 중 4/5가
#선호한다고 하였다. 전체 고객의 새로운 메뉴에 대한 선호도를 p라 할 때, 모비율 p에
#대한 신뢰도 95%의 신뢰구간을 구하시오.
n <- 100 
phat <- 4/5
alpha <- 0.05
sd <- sqrt(phat*(1-phat))
se <- sd/sqrt(n)
z <- qnorm(1 - alpha/2)
ll <- phat - z*se
ul <- phat + z*se
ul;ll;
options(digits = 3)
(paste(ll, " <= p  <= " , ul))

#3
#우리나라 성인 남성의 흡연율을 조사한다고 한다. 이에 성인 남자 1,000명을 무작위로
#뽑아 흡연 여부를 조사하였더니, 430명이 흡연을 하고 있었다. 이때 흡연율(모비율)에
#대한 90% 신뢰구간을 추정하시오.
n <- 1000
phat <- 430/1000
alpha <- 0.1
sd <- sqrt(phat*(1-phat))
se <- sd/sqrt(n)
z <- qnorm(1 - alpha/2)
options(digits = 3)
ll <- phat - z*se
ul <- phat + z*se
ul;ll;
options(digits = 3)
(paste(ll, " <= p  <= " , ul))


#7. 1-Sample T 테스트
#1
#A회사의 건전지의 수명시간이 1000시간 일 때, 무작위로 뽑은 10개의 건전지에 대한
# 수명은 다음과 같다. 
#980, 1008, 968, 1032, 1012, 996, 1021, 1002, 996, 1017
# 샘플이 모집단과 같다고 할 수 있는가?

time <- c(980, 1008, 968, 1032, 1012, 996, 1021, 1002, 996, 1017)
xbar <- mean(time)
s<- sd(time)
n <- length(time)
h0 <- 1000
(t.t <- (xbar - h0) / (s /sqrt(n)))


alpha <- 0.05
(c.u <-qt(1-alpha/2, df =n-1))
(p.value <- 1- pt(t.t,df=n-1))

t.test(time, mu=1000, alternative="two.sided")
#p value = 0.6 
#fail to reject null hypothesis 
#샘플이 모집단과 같다고 할수 있다. 

#2어떤 반의 학생들의 수학 평균성적은 55점이었다. 0교시 수업을 시행하고 나서 학생들의 
#시험 성적은 다음과 같다.
#58, 49, 39, 99, 32, 88, 62, 30, 55, 65, 44, 55, 57, 53, 88, 42, 39
#0교시 수업을 시행한 후, 학생들의 성적은 올랐다고 할 수 있는가?

scores <- c(58, 49, 39, 99, 32, 88, 62, 30, 55, 65, 44, 55, 57, 53, 88, 42, 39)
xbar <- mean(scores)
s <- sd(scores)
n <- length(scores)
h0 <- 55
(t.t <- (xbar - h0) / (s/sqrt(n)))

alpha <- 0.05
(c.u <- qt(1-alpha, df = n-1))
(p.value <- 1-pt(t.t,df=n-1))

t.test(scores, mu = 55, alternative = "greater")
#p value = 0.405 therefore fail to reject null hypothesis 
#올랐다고 할수 없다. 

#도표 작성: 
par(mar = c(0,1,1,1))
x <- seq(-3,3, by=0.001)
y <- dt(x, df=n-1)
plot(x,y,type="l", axes=F,ylim=c(-0.02,0.38),
     main="", xlab="t", ylab="")
abline(h=0)

polygon(c(c.u, x[x>c.u], 3), c(0,y[x>c.u], 0), col=2)
text(c.u, -0.02, expression(t[0.05]==1.74))
text(1.8,0.2, expression(t[0.05]== 1.74))
arrows(1.8,0.18,1.8,0.09, length = 0.05)

polygon(c(t.t, x[x>t.t], 3), c(0, y[x>t.t],0), density = 20, angle = 45)
text(t.t,-0.02, paste("t=", round(t.t,3)), pos=4)
text(2.65, 0.1,expression(plain(P)(T>2.2333) == 0.0196), cex=0.8)
arrows(2.7,0.08, 2.5,0.03, length=0.05)

#2006년 조사에 의하면 한국인의 1인 1일 평균 알코올 섭취량이 8.1g 이다. 
#2008년 무작위로 뽑은 알코올 섭취량은 다음과 같다.
#15.50, 11.21, 12.67, 8.87, 12.15, 9.88, 2.06, 14.50, 0, 4.97
#평균 알코올 섭취량이 달라졌다고 할 수 있는가?
alcohol <- c(15.50, 11.21, 12.67, 8.87, 12.15, 9.88, 2.06, 14.50, 0, 4.97)
xbar <- mean(alcohol)
n <- length(alcohol)
s <- sd(alcohol)
h0 <- 8.1
(t.t <- (xbar - h0) / (s/sqrt(n)))

alpha <- 0.05
(c.u <- qt(1-alpha/2, df = n-1))
(p.value <- 1-pt(t.t,df=n-1))
  
t.test(alcohol,mu=8.1)

#p value = 0.5 cannot reject null hypothesis
#평균 알코올 섭취량이 달라졌다고 할수 없다.

#도표 작성: 
par(mar = c(0,1,1,1))
x <- seq(-3,3, by=0.001)
y <- dt(x, df=n-1)
plot(x,y,type="l", axes=F,ylim=c(-0.02,0.38),
     main="", xlab="t", ylab="")
abline(h=0)

polygon(c(c.u, x[x>c.u], 3), c(0,y[x>c.u], 0), col=2)
text(c.u, -0.02, expression(t[0.05]==1.74))
text(1.8,0.2, expression(t[0.05]== 1.74))
arrows(1.8,0.18,1.8,0.09, length = 0.05)

polygon(c(t.t, x[x>t.t], 3), c(0, y[x>t.t],0), density = 20, angle = 45)
text(t.t,-0.02, paste("t=", round(t.t,3)), pos=4)
text(2.65, 0.1,expression(plain(P)(T>2.2333) == 0.0196), cex=0.8)
arrows(2.7,0.08, 2.5,0.03, length=0.05)

