#과제 12 
#1 
x <- c(150,160,170,180,190)
y <- c(176,179,182,181,185)
height <- data.frame(x,y)
names(height) <- c("Dad", "Son")
cor_height <- lm(Son ~ Dad, data = height)
cor_height
par(mfrow=c(2,2))
plot(cor_height)
summary(cor_height)


library(psych)
pairs.panels(height[names(height)])

pre <- predict(cor_height,newdata = height )
pre <- as.data.frame(pre)
predict(cor_height, )
test <- as.data.frame(165)
data <- c(150,160,170,180,190)
test2 <- data.frame("Dad" = data)
names(test) <- "Dad"
pre.ans <- predict(cor_height,test)
pre.test <- predict(cor_height,test2)
#예측치
cor(pre.test,y)

#2 
x <- c(100,200,300,400,500)
y <- c(30,70,85,140,197)
cc <- data.frame(Income = x, Usage = y)
cor_cc <- lm(Usage ~ Income, data = cc)
par(mfrow = c(2,2))
plot(cor_cc)
summary(cor_cc)
test <- data.frame(Income = 250)
pre.ans <- predict(cor_cc,test)
pre.ans

#3 mtcars 데이터셋에서 배기량(disp)에 따른 마력(hp)의 회귀식을 구하시오.
str(mtcars)
head(mtcars)
# 종속 변수 
y <- mtcars$hp  
# 독립 변수 
x <- mtcars$disp

df <- data.frame(disp = x, hp = y)
result.lm <- lm(hp ~ disp, data= df)
result.lm2 <- lm(hp ~ ., data = mtcars)
result.lm3 <- lm(hp ~ disp + wt + carb, data = mtcars)
summary(result.lm2)
par(mfrow=c(2,2))
plot(result.lm)
plot(result.lm2)
plot(result.lm3)

par(mfrow=c(1,1))
step(result.lm2, direction = 'backward')

subsets1 <- regsubsets(hp~., data = mtcars,
                      method = 'seqrep', nbest=4)
subsets2 <- regsubsets(hp~., data= mtcars,
                      method='exhaustive',nbest=4)
plot(subsets1)
plot(subsets2)

final.lm <- lm(hp ~ disp + wt + carb, data = mtcars)
summary(final.lm)


#4 MASS 패키지를 설치하고, 이 패키지 안에 있는 Boston 데이터셋을 이용하여 Boston 인근의 집값을 결정하는 다중회귀 모델을 만드시오.
library(MASS)
str(Boston)
head(Boston)
View(Boston)

data <- Boston
set.seed(1234)
pairs(data)
pairs.panels(data)

library(dplyr)
pairs(data %>% dplyr::sample_n(min(1000, nrow(data))),
      lower.panel=function(x,y){ points(x,y); abline(0, 1, col='red')})

data_lm_full <- lm(medv ~ ., data= data)
summary(data_lm_full)
step(result.lm2, direction = 'backward')
data_step <- step(data_lm_full, direction = "backward")
data_step1 <- stepAIC(data_lm_full, scope = list(upper = ~.^2, lower ~ 1))

data_lm_adjusted <- lm(medv ~ crim + zn + chas + nox + rm + dis + rad + tax + ptratio + 
  black + lstat, data=data)
data_lm_adjusted1 <- lm(formula = medv ~ crim + zn + indus + chas + nox + rm + age + 
                         dis + rad + tax + ptratio + black + lstat + rm:lstat + rad:lstat + 
                         dis:rad + black:lstat + crim:chas + chas:nox + chas:rm + 
                         chas:ptratio + rm:ptratio + age:black + zn:dis + crim:rm + 
                         crim:lstat + chas:lstat + rm:age + age:lstat + rm:black + 
                         crim:black + crim:dis + zn:lstat + zn:tax + dis:lstat + tax:ptratio + 
                         nox:rad + crim:zn + nox:age + indus:nox + age:rad + age:ptratio + 
                         age:dis + chas:black + age:tax + dis:tax + crim:rad + indus:rm + 
                         rm:tax + tax:lstat + dis:black + crim:tax, data = data)
summary(data_lm_adjusted)
summary(data_lm_adjusted1)
length(coef(data_step))
length(coef(data_step1))


#모형 평가 
set.seed(1212)
n <- nrow(data)
idx <- 1:n
training_idx <- sample(idx,n*.60)
idx <- setdiff(idx, training_idx)
validate_idx <- sample(idx, n*.20)
test_idx <- setdiff(idx, validate_idx)
training <- data[training_idx,]
validation <- data[validate_idx,]
test <- data[test_idx,]

y_obs <- validation$medv
yhat_lm <- predict(data_lm_full, newdata = validation)
yhat_lm_2 <- predict(data_lm_adjusted, newdata = validation)
yhat_lm_3 <- predict(data_lm_adjusted1, newdata = validation)
install.packages("Metrics")
library(Metrics)
rmse(y_obs,yhat_lm)
rmse(y_obs,yhat_lm_2)
rmse(y_obs,yhat_lm_3)
