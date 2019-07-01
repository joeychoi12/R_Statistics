#Chi squared distribution X^2분포
#degree of freedom 자유도 
#chisq() <- chisquared function  

par(mar=c(1,1,1,1))
dev.off()
rchisq <- rchisq(n = 3, df=2)
hist(rchisq)
x4 <- seq(min(rchisq),max(rchisq),length = 100)
y4 <- dchisq(x = x4, df = 2)
lines(x4,y4,lty=2,lwd=2,col="orange")

hist(b.64.mean, prob = T, xlim= c(0.3, 1.7), main="표본 크기: 64",
     col = "orange", border="red")
x4<- seq(min(b.64.mean), max(b.64.mean),length =1000)
y4 <- dnorm(x=x4, mean=1, sd=sqrt(0.9)/sqrt(64))
lines(x4,y4,lty=2,lwd=2,col="orange")

plot(sort(rchisq, decreasing = T),type = "l")


#chi squared
#자유도 1 
set.seed(123) 
x1 <- rchisq(100, 1) 
hist(x1, prob=TRUE,main= "df: 1 ", col="orange",border = "black") 
curve( dchisq(x, df=2),lty=2,lwd=2, col='red', add=TRUE) 

#자유도 3
set.seed(123) 
x3 <- rchisq(100, 3) 
hist(x3, prob=TRUE,main= "df: 3 ", col="orange",border = "black") 
curve( dchisq(x, df=3),lty=2,lwd=2, col='red', add=TRUE) 


#자유도 5 
set.seed(123) 
x5 <- rchisq(100, 5) 
hist(x5, prob=TRUE,main= "df: 5 ", col="orange",border = "black") 
curve( dchisq(x, df=5),lty=2,lwd=2, col='red', add=TRUE) 


#자유도 20
dev.off()
set.seed(123) 
x10 <- rchisq(100, 20) 
hist(x10, prob=TRUE,main= "df: 20 ", col="orange",border = "black") 
curve( dchisq(x, df=20),lty=2,lwd=2, col='red', add=TRUE) 


#t-distribution
#자유도 1
for(i in 1:n) {
  t.2.mean[i] <- mean(df(2, size = t, prob = p ))
  b.4.mean[i] <- mean(df(4, size = t, prob = p ))
  b.32.mean[i] <- mean(df(32, size = t, prob = p ))
  b.64.mean[i] <- mean(df(64, size = t, prob = p))
}

rt1 <- rt(50, df=1)
hist(rt1, breaks=20)


rt2 <- rt(50, df=2)
rt5 <- rt(50, df=5)
rt30 <- rt(50, df=30)



t.values.1 <- seq(-4,4,.1)
plot(x = t.values, y=dt(t.values,df=1),type = "l",lty = "dotted",ylim = c(0,.4), xlab = "t", ylab = "f(t)") 
lines(x= t.values, y= dt(t.values,df=2),type = "l",lty = "dotted",col = "blue")
lines(x = t.values, y = dt(t.values, df= 8),type = "l", col= "red")
lines(x = t.values, y = dt(t.values, df= 30),type = "l", col = "green")
legend(1,0.3,legend=c("df: 1", "df: 2", "df: 8", "df: 30"),
       col = c("black", "blue", "red", "green"), lty = c(2,2,1,1), cex = 0.75, bty = "n",
       seg.len=1)


plot(x= t.values.2, y= dt(t.values.2,df=2),type = "l",lty = "dotted",ylim = c(0,.4), xlab = "t", ylab = "f(t)")

#F distribution 

#calculating df 
df(1.2,df1=10,df2=20)

x <- rf(1000, df1 = 3, df2 =5)
hist(x, freq = F,
     xlim=c(0,7),
     ylim=c(0,1),
     xlab='',
     main = " "
     )
curve(df(x, df1 =3, df2 = 5), from= 0, to = 5, n = 5000, col = "red", lwd = 2, add=T)


x2 <- rf(1000,df= 3, df2 = 20)
hist(x2, freq = F,
     xlim=c(0,5),
     ylim=c(0,0.8),
     xlab='',
     main = " "
)
curve(df(x, df1 =3, df2 = 20), from= 0, to = 5, n = 5000, col = "red", lwd = 2, add=T)

x3 <- rf(1000,df1 = 10, df2 = 5)
hist(x3, freq=F,
     xlim=c(0,8),
     ylim=c(0,1),
     xlab='',
     main = ""
     )
curve(df(x, df1 = 10, df2 = 5), from= 0, to = 8, n = 5000, col = "red", lwd = 2, add=T)

x4 <- rf(1000,df1 = 10, df2 = 20)
hist(x4, freq=F,
     xlim=c(0,8),
     ylim=c(0,1),
     xlab='',
     main = ""
)
curve(df(x, df1 = 10, df2 = 20), from= 0, to = 8, n = 5000, col = "red", lwd = 2, add=T)

dev.off()

curve(df(x, df1 = 10, df2 = 20), from= 0, to = 3, n = 5000, col = "red", lwd = 2, add=T)
curve(df(x, df1 = 10, df2 = 5), from= 0, to = 8, n = 5000, col = "blue", lwd = 2, add=T)
curve(df(x, df1 =3, df2 = 20), from= 0, to = 5, n = 5000, col = "green", lwd = 2, add=T)
curve(df(x, df1 =3, df2 = 5), from= 0, to = 5, n = 5000, col = "yellow", lwd = 2, add=T)










