#1
#1) 시행횟수가 6 성공확률이 1/3인 이항분포 성공 횟수:3 일 확률 

p <- 1/3
n <- 6
x <- 0:n

dbinom(3, size = n, prob = p)
#ANS: 0.219

#2) 평균이 170 표준편차 = 6인 정규분포 상위 20%의 키 범위 
mu <-170
sigma <- 6
qnorm(0.8, mean = mu, sd = sigma)

#3) 자유도 =3 카이제곱분포에 누적확률이 95%일때의 값 
qchisq(0.95,3)

#4) 자유도 =2 인 t 분포에서 누적확률이 0.975일 떄의 값
qt(0.975,2)

#5) 표준정규분포에서 확률 변수의 값이 1일 때의 누적 확률 
pnorm(1)

#2 다음 문항이 베르누이 시행인지 반단하시오 

#3
library(dplyr)
str(iris)
setosa.sepal.leng <- iris %>% 
  filter(Species == "setosa") %>%
  select(Sepal.Length)
n <- nrow(setosa.sepal.leng)
mean <- mean(setosa.sepal.leng$Sepal.Length)
alpha <- 0.05
sd <- sd(setosa.sepal.leng$Sepal.Length)
se <- sd/sqrt(n)
z <- qnorm(1 - alpha/2)
ll <- mean - z*se
ul <- mean + z*se
ll;ul;
options(digits = 3)
(paste(ll, " <= p  <= " , ul))

#4 
#1) 자유투를 10번 던져서 9번 이상 성공할 확률
dbinom(9,10,prob = 0.7)
#Ans
#12.1% 

#2) 자유투를 10번 던져서 5번 이상 8번이하의 성공 확률 
a <- pbinom(8,10,prob = 0.7)
b <- pbinom(5,10,prob=0.7)
#ans
a -b

#5 
alcohol <- c(16.90,13.21,15.67,9.87,13.15,9.98,3.56,14.50,8.12,6.97)
xbar <- mean(alcohol)
s <- sd(alcohol)
n <- length(alcohol)
h0 <- 8.1
(t.t <- (xbar - h0) / (s/(sqrt(n))))

alpha <- 0.05
(c.u <-qt(1-alpha/2, df =n-1))
(p.value <- 1- pt(t.t,df=n-1))

t.test(alcohol, mu=8.1, alternative="two.sided")
# P Value가 0.05로써 영가설을 기각.
# 평균 알코올 섭취량이 달라 졌다고 통계적으로 유의 


#6
rangenorm <- function(from,to, mean,sd) {
  a <- pnorm(to, mean=mean,sd=sd) - pnorm(from, mean=mean, sd= sd)
  return(a)  
}
rangenorm(-1.96,1.96,0,1)

#7 mpg
#1) subcompact 자동차와 midsize 자동차의 도시 연비 
str(mpg)
subcompact <- mpg %>% 
  filter(class %in% c("subcompact","midsize")) %>%
  select(cty, class)
           
View(subcompact) 


var.test(drive$cty ~ drive$drv)
#p-value = 0.1979
#unable to reject null hypothesis 

t.test(drive$cty ~ drive$drv, paired = F, var.equal = T, conf.level = 0.95)
#p-value = 0.00004826
#p-value lower than 0.05 
#reject null hypothesis 
#driving forward and backward has a significant difference in cty mpg) 


#2) 일반 휘발유(r)와 고급 휘발유(p)의 고속도로 연비 
mpg$fl


#8 
a <- c(322,109,99,29)
mendal <- c(9,3,3,1)
chisq.test(a,mendal)

### Q9 R 내장 데이터인 women 을 이용하여 다음을 구하시오 
#키 와 몸무게의 곡선 회귀분석을 통한 회귀식 


