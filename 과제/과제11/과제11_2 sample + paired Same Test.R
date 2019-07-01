#과제 11
#8-9 
#8 2 Sample T 테스트 

#2 Sanoke Test:
#1) Separate 2 data
#2) 정규성 검정 (Shapiro.test, qqnorm, qqline)
#3) 분산이 같은지 (var.test())
library(PairedData)
library(psych)
library(tidyr)
library(reshape2)

#mtcars 데이터셋에서 자동차 기어 종류(am: 오토/수`동)에 따른 mpg의 차이가 
#통계적으로 유의한지 t-test를 통해 확인해 보시오.

View(mtcars)
#등분 산성 TEST
var.test(mpg ~ am, data = mtcars)
var.test(mtcars[mtcars$am==1,1], mtcars[mtcars$am==0,1])
mtcars[mtcars$am == 1,1]
View(mtcars[mtcars$am])
# p-value = 0.06691 등분산성 만족 

#T Test
t.test(mtcars[mtcars$am ==0,1] , mtcars[mtcars$am == 1,1] ,paired = FALSE, 
       var.equal=TRUE, conf.level = 0.95)
#Auto Mean: 17.14 Manual Mean: 24.39
#p-value = 0.000285
# 차이가 있다 통계적으로 유의



#2 MASS 패키지에 내장된 Cars93 데이터프레임에 대해서 생산국(Origin)이 
#USA vs. non-USA 2개의 group 에 대해서 차 가격(Price)의 평균이 차이가 있는지를 검정해보시오.

str(Cars93)
head(Cars93)
usa.price <- Cars93[Cars93$Origin == "USA","Price"]
usa.price

nonus.price <- Cars93[Cars93$Origin == "non-USA", "Price"]
#등분 산성 TEST 
var.test(Cars93[Cars93$Origin == "USA","Price"], Cars93[Cars93$Origin == "non-USA","Price"])
# p-value = 0.01387 
# 표본 분산의 동일성 만족하지 못함 
# 분산이 서로 다르다는 대립가설 채택
t.test(usa.price, nonus.price, paired = F, var.equal = F, conf.level =0.95)

var.test(Price ~ Origin, data = Cars93)
#p-value = 0.3428
# 차이가 없다 통계적으로 유의 

# 3 mpg 데이터셋에서 다음을 검정해 보시오.
# 1) subcompact 자동차와 midsize 자동차의 고속도로 연비
str(mpg)
a <- mpg[mpg$class == "subcompact", "hwy" ]
table(mpg$class)
var.test(mpg[mpg$class == "subcompact", "hwy" ], mpg[mpg$class == "midsize", "hwy" ])
a <- mpg %>% 
  filter(class == "subcompact") %>%
  select(hwy)
b <- mpg %>%
  filter(class == "midsize") %>%
  select(hwy)

var1 <- mpg %>% 
  filter(class == c("subcompact", "midsize")) %>%
  select(hwy,class)
var.test(var1$hwy ~ var1$class)
# p-value = 0.0004447 
# reject null hypothesis 
t.test(var1$hwy ~ var1$class, paired = F, var.equal = F, conf.level = 0.95)
# p-value = 0.7171 
# fail to reject null hypothesis
# subcompact car and midsize hwy mpg has significant difference


# 2) 일반 휘발유(r)와 고급 휘발유(p)의 도시 연비
table(mpg$fl)
var.test(mpg[mpg$fl =="r", "cty"], mpg[mpg$fl == "p", "cty"])
premiumVregular <- mpg%>% 
  filter(fl == c("p","r")) %>%
  select(cty, fl)
View(premiumVregular)
var.test(premiumVregular$cty ~ premiumVregular$fl)
#p-value = 0.04918
#p value is less than 0.05 therefore cannot accept null hypothesis
t.test(premiumVregular$cty ~ premiumVregular$fl, paired = F, var.equal = F, conf.level = 0.95)
#p-value = 0.6691
#therefore cannot reject null hypothesis 
#regular fuel and premium fuel does not make a significant difference in cty mpg

# 3) subcompact 자동차의 전륜구동(f)이냐 후륜구동(r)이냐에 따른 도시 연비
table(mpg$drv)
drive <- mpg %>%
  filter(drv == c("f","r")) %>%
  select(cty, drv)

var.test(drive$cty ~ drive$drv)
#p-value = 0.1979
#unable to reject null hypothesis 

t.test(drive$cty ~ drive$drv, paired = F, var.equal = T, conf.level = 0.95)
#p-value = 0.00004826
#p-value lower than 0.05 
#reject null hypothesis 
#driving forward and backward has a significant difference in cty mpg) 


#9 Paired Sample T 테스트 
#1
#새로운 당뇨병 치료제를 개발한 제약사에서는 치료에 지대한 영향을 주는 외부요인을 
#통제하기 위해 10명의 당뇨병 환자를 선별하여 1달 동안 '위약(placebo)'을 투여한 기간의 
#혈당 수치(Xi)와 '신약(new medicine)'을 투여한 1달 기간 동안의 혈당 수치(Yi)를 측정하여 짝을 이루어 혈당 차이를 유의수준 5%에서 비교하시오.

placebo <- c(51.4,52.0,45.5,54.5,52.3,50.9,52.7,50.3,53.8,53.1)
medicine <- c(50.1,51.5,45.9,53.1,51.8,50.3,52.0,49.9,52.5,53.0)
diff <- placebo - medicine
diff
n <- length(placebo)
n
mean.diff <- mean(diff)
mean.diff
s <- sd(diff)
t.t <- mean.diff/(s/sqrt(n))
alpha <- 0.05
qt(alpha, df=9)
pt(t.t,df=9)

t.test(placebo,medicine,
       alternative = "greater", paired = TRUE, 
       conf.level = 0.95)
#p-value = 0.003105 
#able to reject null hypothesis
#치료제는 효과 있음


#2 
#두 종류의 신발 밑창의 원재료가 닳는 정도가 차이가 있는지를 검정하기 위해서 10명의
#소년에게 한쪽은 A라는 원재료로 만든 신발을 신기고, 다른 한쪽은 B라는 원재료로 만든
#신발을 신긴 후에, 일정 기간이 지난후에 신발을 수거하여 10명의 각 소년의 왼쪽 신발
#밑창의 닳은 정도와 오른쪽 신발 밑창의 닳은 정도의 차이를 비교하여 두 종류 원재료의 재질이 다른지를 검정하시오.

a <- c(13.2,8.2,10.9,14.3,10.7,6.6,9.5,10.8,8.8,13.3)
b <- c(14.0,8.8,11.2,14.2,11.8,6.4,9.8,11.3,9.3,13.6)
diff <- a-b
diff
n <- length(a)
m <- mean(diff)
s <- sd(diff)
t.t <- m/(s/sqrt(n))
alpha <- 0.05
qt(alpha, df = n-1)
pt(t.t,df=n-1)

t.test(a,b, alternative = "two.sided", paired = T,
       conf.level = 0.95)

#p-value = 0.008539 
#reject null hypothesis 
#차이가 있다. 

#  10. 일원 분산분석(One way ANOVA)
lake1 <- c(5, 7, 6, 8, 6, 7, 8, 8, 6, 10)
lake2 <- c(6, 8, 9, 11, 13, 12, 10, 8, 9, 10)
lake3 <- c(14, 25, 26, 18, 19, 22, 21, 16, 20, 30)
lake <- as.data.frame(cbind(lake1,lake2,lake3))
lake_melt <- melt(lake, variable.name = "lake", value.name = "ppm")
lake_melt

#분석을 위한 통계량 계산과 오차제곱합 구하기 
l1.mean <- mean(lake1)
l2.mean <- mean(lake2)
l3.mean <- mean(lake3)

sse.1 <- sum((lake1 - l1.mean)^2)
sse.2 <- sum((lake2 - l2.mean)^2)
sse.3 <- sum((lake3 - l3.mean)^2)

(sse <- sse.1 + sse.2 + sse.3)
(dfe <- (length(lake1)-1) + (length(lake2)-1) + (length(lake3) -1))

#처리 제곱합 구하기
all.lake <- c(lake1,lake2,lake3)
l.mean <- mean(all.lake)
(sst.1 <- length(lake1) * sum(l1.mean - l.mean)^2)
(sst.2 <- length(lake2) * sum(l2.mean - l.mean)^2)
(sst.3 <- length(lake3) * sum(l3.mean - l.mean)^2)

sst <- sst.1 + sst.2 + sst.3
sst

#전체 제곱합과 분해된 제곱합의 합 구하기
(tsq <- sum( (all.lake - l.mean)^2))
(ss <- sst+sse)

#검정 통계량 구하기 
mst <- sst/dft
mse <- sse/dfe
(f.t <- mst/mse)

#기각역을 위한 임계값 구하기 
alpha <- 0.05
(tol <- qf(1-alpha, 2,27))
(p.value <- 1- pf(f.t,2,27))

ow <- lm(ppm~lake, data= lake_melt)
anova(ow)

# p value = 2.411e-10 로써 유의 수준 0.05 보다 낮기떄문에 영가설을 기각함으로 3개의 호수의 산소량이 같다고 할수 없다 


#10 -2 
a <- c(15.5, 14.3, 16.3, 13.5, 15.7, 16.4, 14.7)
b <- c(14.7, 16.3, 15.5, 15.2, 16.3, 13.5, 15.4)
c <- c(15.5, 13.2, 16.5, 15.7, 15.3, 15.2, 14.8)
veggies <- data.frame(a,b,c)
veggies
veggies <- melt(veggies, variable.name = "Vegetable", value.name = "Price")

ow <- lm(Price~Vegetable, data = veggies)
anova(ow)

# p value = 0.9824 유의수준 0.05 보다 높기 때문에 영가설 기각을 실패함으로써 채소 3개의 가격이 같다고 할 수 있다. 


#11 - 1
n <- 80
pTrue <- 0.85
pFalse <- 0.15
alpha <- 0.05
#검정통계량
evTrue <- n * pTrue
evFalse <- n * pFalse
"검정통계량"
t.s <- (((64-evTrue)^2) /evTrue + ((16-evFalse)^2)/evFalse)
"p-value"
1 - pchisq(t.s, df=1)

#fail to reject null hypothesis 
# 유의수준 0.05보다 높기 떄문에 영가설을 기각 15%를 따른 다고 볼수 있다. 

#11-2 
s.more <- c(23,31,13)
s.less <- c(21,48,23)
s.none <- c(63,159,119)

smokeDrink <- data.frame(row.names = c("반병 이상", "반병 이하", "못마심"),  s.more, s.less, s.none)
smokeDrink$Sum <- smokeDrink$s.more + smokeDrink$s.less + smokeDrink$s.none 
newRow <- data.frame(row.names = "계", sum(smokeDrink$s.more), sum(smokeDrink$s.less), sum(smokeDrink$s.none))
#newRow

smokeDrink <- rbind(smokeDrink,newRow)
names(newRow) <- names(smokeDrink)
smokeDrink

tab <- apply(smokeDrink,c(1,2),sum)
#tab
round(prop.table(tab,margin =2) ,1)
s.n <- margin.table(tab, margin =1)
d.n <- margin.table(tab, margin= 2)

(s.p <- s.n/margin.table(tab))
(d.p <- d.n/margin.table(tab))

(expected <- margin.table(tab) * (s.p %*% t(d.p)))
addmargins(expected)
#chi-squared statistics
o.e <- (tab - expected)^2 / expected
addmargins(o.e)




chisq.t <- sum(o.e) #검정 통계량 
chisq.t
qchisq(chisq.t, df=2) #p value 


chisq.test(tab)


nullstring <- "
d.more <- c(23,21,63)
d.less <- c(31,48,159)
d.none <- c(13,23,119)
data.frame(  d.more, d.less, d.none)" 

