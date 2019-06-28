# 예제 9-1 아버지와 아들 의 공분산과 상관 계수 
hf <- read.table("http://www.randomservices.org/random/data/Galton.txt", header = T, stringsAsFactors = FALSE)
str(hf)
hf$Gender <- factor(hf$Gender, levels = c("M","F"))
hf.son <- subset(hf, Gender = "M")
hf.son <- hf.son[c("Father", "Height")]
str(hf.son)

f.mean <- mean(hf.son$Father)
s.mean <- mean(hf.son$Height)
cov.num <- sum((hf.son$Father - f.mean) * (hf.son$Height - s.mean))
(cov.xy <- cov.num / (nrow(hf.son) - 1))
# R 함수를 이용한 공분산 (표본)
cov(hf.son$Father, hf.son$Height)

(r.xy <- cov.xy/ (sd(hf.son$Father) * sd(hf.son$Height)))

#R 을 이요한 상관 계수 
cor(hf.son$Father, hf.son$Height)



hf <- read.csv("http://www.math.uah.edu/stat/data/Galton.csv", header=T, stringsAsFactors = FALSE)
hf$Gender <- factor(hf$Gender, levels=c("M", "F"))
hf.son <- subset(hf, Gender=="M")
hf.son <- hf.son[c("Father", "Height")]

f.mean <- mean(hf.son$Father)
s.mean <- mean(hf.son$Height)
cov.num <- sum( (hf.son$Father-f.mean) * (hf.son$Height - s.mean) )
(cov.xy <- cov.num / (nrow(hf.son) - 1))
cov(hf.son$Father, hf.son$Height) 

(r.xy <- cov.xy / (sd(hf.son$Father) * sd(hf.son$Height)))
cor(hf.son$Father, hf.son$Height)











