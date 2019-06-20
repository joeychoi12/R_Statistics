#2장 
setwd('D:/workspace/R_Statistics/ch2')
data <- read.csv("ch2.csv", header=F, na.strings=c("."))
str(data)
data$V1 <- factor( data$V1 , levels = c(1,2), 
                   labels = c("남자","여자"))
data$V3 <- factor(data$V3, levels=1:14,
                  labels = c("가구주", "가구주의 배우자", '자녀',
                             "자녀의 배우자", "가구주의 부모","배우자의 부모 ", 
                             "손자녀, 그 배우자", "중손자녀,그 배우자",
                             "조부모", "형제자매, 그 배우자", "형제자매의 자녀, 그 배우자" ,
                            "부모의 형제자매, 그 배우자" ,
                             "기타 친인척" , "그외같이사는사람"))
data$V4 <- factor( data$V4, levels = 1:8, 
                   labels = c("안 받았음", "초등학교", "중학교", 
                              "고등학교", "대학-4년제 미만", "대학-4년제 이상",
                              "석사과정","박사과정"))
str(data)
save.image("data.rda")
data

plot(data)
ggplot(data,)

#1 그래프 
library(ggplot2)
cars

par(mfrow=c(1,2))
plot(cars$speed, cars$dist, main = "속도와 제동거리",
     xlab = "속도(mph)", ylab = "제동거리 (ft)", pch = 1, col = "red")
plot(jitter(cars$speed), jitter(cars$dist), main = "속도와 제동거리",
     xlab = "속도(mph)", ylab = "제동거리 (ft)", pch = 1, col = "red")
par(mfrow=c(1,2))
p1 <- ggplot(cars, aes(speed,dist)) +
  geom_point()

p2 <- ggplot(cars, aes(speed,dist)) +
  geom_jitter()
library(gridExtra)
grid.arrange(p1,p2,nrow = 1)

Nile
plot(Nile, main= "Nile강의 연도별 유량 변화",
     xlab = "연도", ylab = "유량")
plot(Nile, type = 'p', main = "Nile 강의 연도별 유량 변화",
     xlab = "연도", ylab = "유량")
plot.ts(Nile)
p1 <- plot(Nile)

str(Nile)
df_Nile <- as.data.frame(Nile)
df_Nile <- data.frame(Y= as.matrix(Nile), date=time(Nile))

p2 <- ggplot(df_Nile, aes(date,Y)) + 
  geom_line() +  
  scale_y_continuous(expand = c(0,0)) 
grid.arrange(p1,p2, nrow = 1)

df_nile <- as.data.frame(Nile)
year <- c(1871:1970)
df_nile$year <- year
ggplot(df_nile, aes(x=year,y=x)) + 
  geom_line()

#막대 그래프
head(data);tail(data)
tableV5 <- table(data$V5)
tableV5
barplot(tableV5, main="출생아(남자)별 빈도",
        xlab = "출생아수", ylab = "빈도", col = "Green")

#히스토그램 
hist(data$V2, main = "연령별 분포",xlab = "연령", ylab = "빈도")
hist(data$V2, breaks = c(seq(0,90,10)), right = F,
     main = "연령별 분포",xlab = "연령", ylab = "빈도")
hist(data$V2, probability = T, breaks = c(seq(0,90,10)), right = F,
     main = "연령별 분포",xlab = "연령", ylab = "빈도")


#학력에 따른 성별 인원수
data
library(scales)
ggplot(data, aes(V4,V2,fill=V1)) + 
  geom_col() + scale_y_continuous(labels = comma)
#scale_y_continuous <- changes scientific notation into commabase using label = comma
tableV1.V4 <- table(data$V1, data$V4)
tableV1.V4
barplot(tableV1.V4,legend.text = T, col = c("blue","red"),
        main="학력에 따른 성별 인원수")

#원 도표 PIE CHART 
load("data.rda")
table.V4 <- table(data$V4)
table.V4
pie(table.V4, main = "학력수준별 비중")
