#Chapter 8 범주형 자료분석 
#값으로부터 직접 계산을 할수 없는 질적자료의 경우 

#1 Sample T  And Sample Prop
#2 sample T
#Paired T
#ANOVA
#등분산성, 정규성 

#예제 1. 멘델의 법칙
x <- c(315,101,108,32)
chisq.test(x,p=c(9,3,3,1)/16)
#p-value = 0.9254
ychi <- dchisq(x,3)
alpha <- 0.05
tol <- qchisq(0.95, df=3)

par(mar=c(0,1,1,1))
curve(x,ychi,type = "l", axes=F, ylim=c(-0.03,0.05),xlab="",ylab="")
chisq.plot
curve(dchisq(x,3),col = "black", main= "",xlim=c(0,8))
abline(h=0)

x <- seq(0, 15, by = 0.01)
dc <- dchisq(x, df=3)

alpha <- 0.05
tol <- qchisq(0.95,df=3)

par(mar=c(0,1,1,1))
plot(x, dc, type = "l", axes=F, ylim=c(-0.03,0.25), xlab="",ylab="")
abline(h=0)
tol.g <- round(tol,2)
polygon(c(tol.g, x[x>tol.g],15), c(0,dc[x>tol.g],0), col="yellow")
text(0,-0.03,"0", cex=0.8)
text(tol, -0.03, expression(chi[0.05]^{2}==2.14), cex=0.8)

tol2 <- qchisq(1-0.9254, df=3)
tol2.g <-round(tol2, 2)     
polygon(c(tol2.g, x[x>tol2.g],15), c(0,dc[x>tol2.g],0),col = "red", density=20, angle=305)
text(0,-0.03, "0", cex=0.8)
text(tol2, -0.03, expression(chi[0.9254]^{2} ==0.47), cex=0.8)

#2 동질성 검정과 독립성 검정
#예제-2 연령대별 SNS 이용률의 동질성 검정 
sns.c <- read.csv("./data/snsbyage.csv", header=T, stringsAsFactors=FALSE)
str( sns.c )
head(sns.c)

sns.c <- transform(sns.c, age.c = 
                     factor(age, levels=c(1, 2, 3), 
                            labels=c("20대", "30대", "40대")))

sns.c <- transform(sns.c, service.c = 
                     factor(service, levels=c("F", "T", "K", "C", "E"), 
                            ordered=TRUE))
head(sns.c)
c.tab <- table(sns.c$age.c, sns.c$service.c)

(a.n <- margin.table(c.tab, margin=1))
(s.n <- margin.table(c.tab, margin=2))
(s.p <- s.n / margin.table(c.tab))
(expected <- a.n %*% t(s.p))

(o.e <- c.tab-expected)
(t.t <- sum(  (o.e)^2 / expected ))


qchisq(0.95, df=8)

1-pchisq(t.t, df=8) #p-value

chisq.test(c.tab)
names <-chisq.test(c.tab) 

addmargins(chisq.test(c.tab)$expected)
result$expected
str(result)
result$p.value

#독립성 검정
#에제-3 성별에 따른 대학원 입학 여부의 독립성 검정 
data("UCBAdmissions")
UCBAdmissions
ucba.tab <- apply(UCBAdmissions, c(1,2), sum)
ucba.tab


round(prop.table(ucba.tab,margin =2) *100,1)
#독립성 검정
a.n <- margin.table(ucba.tab, margin =1)
g.n <- margin.table(ucba.tab, margin= 2)

(a.p <- a.n/margin.table(ucba.tab))
(g.p <- g.n/margin.table(ucba.tab))

(expected <- margin.table(ucba.tab) * (a.p %*% t(g.p)))
1 - pchisq(112.250, df = 1)

#chi-squared statistics
o.e <- (ucba.tab - expected)^2 / expected
addmargins(o.e)


chi sq.t <- sum(o.e) #검정 통계량 
chisq.t
qchisq(chisq.t, df=1) #p value 

chisq.test(ucba.tab)

##continu