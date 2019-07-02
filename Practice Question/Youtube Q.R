#Produce plots of some explanatory variables 
plot(Ozone~Solar.R, data = airquality)
plot(Ozone~Wind, airquality)

coplot(Ozone~Solar.R|Wind,panel=panel.smooth,airquality)

model2 <- lm(Ozone~Solar.R*Wind, airquality)
plot(model2)
summary(model2)
termplot(model2)
summary(airquality$Solar.R)
summary(airquality$Wind)

Solar1 <- mean(airquality$Solar.R, na.rm = T)
Solar2 <- 100
Solar3 <- 300n

predict(model2, data.frame(Solar.R=100, Wind=10))
predict(model2, data.frame(Solar.R=Solar1, Wind=10))
?airquality
p1 <- predict(model2, data.frame(Solar.R=Solar1,Wind=1:20))
p2 <- predict(model2, data.frame(Solar.R=Solar2,Wind=1:20))
p3 <- predict(model2, data.frame(Solar.R=Solar3,Wind=1:20))

p1
p2
p3

plot(Ozone~Wind,airquality)
lines(1:20,p1)
lines(1:20,p2)
lines(1:20,p3)

wind1 <- coef(model1)[1]+coef(model1)[2]*19
wind2 <- coef(model1)[1]+coef(model1)[2]*20
coef(model1)[1]

Sys.setlocale()