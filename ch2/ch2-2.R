#ch2 section 02  모수와 통계량
setwd("D:/workspace/R_Statistics/ch2")
#라니카페
rainicafe <- read.csv("cafedata.csv", stringsAsFactors = F)
str(rainicafe)
head(rainicafe)
summary(rainicafe)
dim(rainicafe)
rainicafe$Coffees
rainicafe$Coffees <- as.numeric(rainicafe$Coffees)
sort(rainicafe$Coffees)
sort(rainicafe$Coffees)[1] #최소값 
sort(rainicafe$Coffees, decreasing = T)[1] #최대값
min(rainicafe$Coffees, na.rm = T)
max(rainicafe$Coffees, na.rm = T)

#최빈값 mode
stem(rainicafe$Coffees) #최빈값
hist(rainicafe$Coffees)

#커피 주문량의 평균 
rc <- rainicafe$Coffees
weight <- 1 / (length(rc) -1)
sum(rc * weight, na.rm = T)
mean(rc,na.rm = T)

rc[rc== max(rc, na.rm = T)] <- 480
mean(rc,na.rm = T )

length(rc)
median.idx <- (1+length(rc)-1) /2
sort(rc)[median.idx]
median(rc, na.rm = T)
 
#Standard Deviation  표준 편차
height <- c(164,166,168,170,172,174,176)
height.m <- mean(height)
height.dev <- height - height.m
sum(height.dev)
height.dev
height.dev2 <- (height.dev)^2
sum(height.dev2)
sum((height.dev)^2)
variance <- sum(height.dev2) / length(height) #분산 
standard_deviation <- sqrt(variance) #표준편차
standard_deviation

mean(height)
var(height)
sd(height)

#사분위수 구하기 Quantiles 
quantile(rc, na.rm = T)
qs <- quantile(rc, na.rm = T)
#3분위수 - 1분위수 
#IQR Inter Quantile Range 사분위수 범위 
IQR(rc, na.rm = T)
qs[4]-qs[2]
bp <- boxplot(rc, main = "커피 판매량에 대한 상자도표",axes=T)$stats
summary(rc)
boxplot(rc, main = "커피 판매량에 대한 상자도표",axes=T)$stats

#이상치 (Outlier)
boxplot(cars)
summary(cars)
qs <- quantile(cars$dist)
Q <- quantile(cars$dist)
iqr <- qs[4] - qs[2]
ll <- Q[2] - 1.5 * IQR(cars$dist)
ul <- Q[4] + 1.5 * IQR(cars$dist)
upperLimit <- qs[4] + 1.5 * iqr
lowerLimit <- qs[2] - 1.5 * iqr
lowerLimit;upperLimit

cars$dist 