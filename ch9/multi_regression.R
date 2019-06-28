#다중회귀분석 
state.x77
head(state.x77)
states <- as.data.frame(state.x77[,c("Murder","Population","Illiteracy","Income","Frost")])

fit <- lm(Murder~Population + Illiteracy + Income+ Frost, data=states)
summary(fit)
par(mfrow=c(2,2))
plot(fit)
par(mfrow=c(1,1))


fit1 <- lm(Murder~ ., data=states)
summary(fit1)

fit2 <- lm(Murder ~ Population + Illiteracy, data=states)
summary(fit2)

#AIC(Akaike Information Criterion)
AIC(fit1, fit2) #값이 적을수록 좋은 모델

# Backward stepwise regression, Forward stepwise regression
step(fit1, direction = 'backward')

fit3 <- lm(Murder ~1, data=states)
step(fit3, direction='forward',
     scope =~ Population + Illiteracy+Income +Frost)
step(fit3, direction = 'forward', scope=list(upper=fit1,lower=fit3))

#Backward
# 모든 변수부터 AIC 값이 min 일때 

#Forward
#1 상수항 부터 AIC 값이 min 일때 


install.packages('leaps')
library(leaps)
subsets <- regsubsets(Murder~., data = states,
                      method = 'seqrep', nbest=4)
subsets <- regsubsets(Murder~., data=states,
                      method='exhaustive',nbest=2)
subsets <- regsubsets(Murder~., data=states,
                      method='exhaustive',nbest=10)

summary(subsets)
plot(subsets)
