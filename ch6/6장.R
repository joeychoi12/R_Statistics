#6장 가설검정
#6-1 가설 검정 
setwd("D:/workspace/R_Statistics/ch6/data")
#6장 준비 
data <- read.csv("2010_6차_직접측정 데이터.csv")
str(data)
View(tmp)
View(data)

tmp <- subset(data, data$나이 == 7)
height.p <- tmp$X104.키

set.seed(9)
height <- height.p[sample(length(height.p), 15)]
height
pt(0.727, 14)

mean(height)
sd(height)
t.test(height, mu = 1220)


#단일 모집단의 편균검정 
data <- read.table("http://www.amstat.org/publications/jse/datasets/babyboom.dat.txt", header =F)
str(data)
names(data)<- c("time", "gender", "weight", "minutes")
tmp <- subset(data, gender ==1)
weight <- tmp[[3]]

barx <- mean(weight)
s<- sd(weight)
n <- length(weight)
h0 <- 2800
(t.t <- (barx - h0) / (s /sqrt(n)))

alpha <- 0.05
(c.u <-qt(1-alpha, df =n-1))
(p.value <- 1- pt(t.t,df=n-1))

t.test(weight, mu=2800, alternative="greater")

#도표 작성: 그림 6-8
par(mar = c(0,1,1,1))
x <- seq(-3,3, by=0.001)
y <- dt(x, df=n-1)
plot(x,y,type="l", axes=F,ylim=c(-0.02,0.38),
     main="", xlab="t", ylab="")
abline(h=0)

polygon(c(c.u, x[x>c.u], 3), c(0,y[x>c.u], 0), col=2)
text(c.u, -0.02, expression(t[0.05]==1.74))
text(1.8,0.2, expression(t[0.05]== 1.74))
arrows(1.8,0.18,1.8,0.09, length = 0.05)

polygon(c(t.t, x[x>t.t], 3), c(0, y[x>t.t],0), density = 20, angle = 45)
text(t.t,-0.02, paste("t=", round(t.t,3)), pos=4)
text(2.65, 0.1,expression(plain(P)(T>2.2333) == 0.0196), cex=0.8)
arrows(2.7,0.08, 2.5,0.03, length=0.05)
