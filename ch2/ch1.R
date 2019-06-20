seq(-3,3,length.out = 60)
seq(-3,3,length.out = 61)
x <- seq(-3,3,0.1)
x[1]
x[c(1,11,31)]

#자료구조: factor
#factor(x = character(), levels, labels = levels, ordered = FALSE)
#x: factor 로 만들 벡터
#levels: 주어진 데이터 중 factor의 각 값(수준) 으로 할 값을 벡터 형태로 지정

x <- 1:5
factor(x, levels =c(1:4))
class(x)
#label
factor(x, levels =c(1:4), labels = c('a','b','c','d'))
#순서 
factor(x, levels =c(1:4), labels = c('a','b','c','d'), ordered = TRUE)
week <- 1:7
weekend <- factor(week, levels = c(1:7), labels = c('월','화','수','목','금','토','일'))
class(weekend)
class(week)

#자료구조: 데이터 프레임
name <- c("철수",'영희','길동')
age <- c(21,20,31)
gender <- factor(c("M","F","M"))
character <- data.frame(name,age,gender)
str(character)
person <- character
person

character[1,]
character[,2]
character[,3]
character[1,3]
