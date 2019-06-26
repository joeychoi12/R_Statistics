#7장 여러 모집단의 평균 비교 검정
#7-1. 모집단 두개인 경우 
#정규성 테스트
setwd("D:/workspace/R_Statistics/ch7/data")
data <- read.table("D:/workspace/R_Statistics/ch7/data/chapter7.txt",header = T)
data
data2 <- data.frame(gender = c(1,1,1,1), weight = c(3350,3380, 3800, 3900))
data <- rbind(data,data2)
boy <- subset(data, gender ==1)
girl <- subset(data, gender ==2)

var.test(data$weight ~ data$gender)

#정규성 테스트
shapiro.test(boy$weight)
shapiro.test(girl$weight)
qqnorm(boy$weight)
qqline(boy$weight)
qqnorm(girl$weight)
qqline(girl$weight)

iriss <- subset(iris, Species == 'setosa')
shapiro.test(iriss$Sepal.Length)
qqnorm(iriss$Sepal.Length)
qqline(iriss$Sepal.Length)
shapiro.test(iriss$Petal.Width)
qqnorm(iriss$Petal.Width)
qqline(iriss$Petal.Width)

#등분산성 테스트 
var.test(data$weight ~ data$gender)

#2-sample T test
t.test(data$weight ~ data$gender , mu = 0, alternative = "less",
       var.equal=TRUE )

#예제 식용 부진증 치료요법의 효과 검정 
install.packages("PairedData")
library(PairedData)
data("Anorexia")
data <- Anorexia
str(data)

install.packages('psych')
library(psych)
summary(data)
describe(data)

n <- length*data$Prior - data$Post
m <- mean(dat$Prior - data$Post)
s <- sd (data$Prior - data$Post)
t.t <- m/(s/sqrt(n))
alpha <- 0.05
qt(alpha, df=16)
pt(t.t,df=16)  #검정통계량으로부터 구한 유의 확률 

t.test(data$Prior, data$Post, paired = T,alternative = "less")
