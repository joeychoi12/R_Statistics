#9-2 회귀 분석
#예제 9-2 아버지와 아들 키 자료로 부터 회귀계수 추정 
plot(hf$Height,hf$Father)
hf$Gender <- factor(hf$Gender, levels=c("M","F"))
hf.son <- subset(hf, Gender == "M")
hf.son <- hf.son[c("Father","Height")]

mean.x <- mean(hf.son$Father)
mean.y <- mean(hf.son$Height)

sxy <- sum((hf.son$Father - mean.x)* (hf.son$Height - mean.y))
sxx <- sum(hf.son$Father  - mean.x)^2

( b1 <- sxy / sxx )
( b0 <- mean.y - b1 * mean.x )


# lm() 함수 이용 
out <- lm(formula = Height ~ Father, data=hf.son)
summary(out)


par(mfrow=c(2,2))
plot(out)
#좋은 선형 모델 
#정규성 - 두번쨰 그림
#독립성
#선형성 첫번째 그림
#등분산성 - 세번째 그림 