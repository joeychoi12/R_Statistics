data <- read.csv("http://stats.idre.ucla.edu/stat/data/binary.csv")
str(data)
head(data)
View(data)

data$rank <- as.factor(data$rank)
str(data)

train <-data[1:200,]
test <- data[201:400,]
model <- glm(admit~gre + gpa + rank, data=data, family = "binomial")
summary(model)
 
