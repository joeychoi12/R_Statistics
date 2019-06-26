#6-2 모비율 검정: 야구공의 불량률 검정
tmp <- read.table("restitution.txt", header =T)
rel <- ifelse(tmp$rst < 0.4134 | tmp$rst > 0.4374, 1, 0)

n <- length(rel)
nos <- sum(rel)
sp <- nos/n
hp <- 0.1
(z <- (sp - hp) / sqrt( (hp * (1-hp) ) /n))

alpha <- 0.05
(c.u <- qnorm(1-alpha))
(p.value <- 1- pnorm(z))

prop.test(nos,n,p=0.1, alternative = "greater", correct=F)

# 도표 출력: 
par(mar=c(0,1,1,1))
x <- seq(-3,3, by = 0.001)
y <- dnorm(x) 

