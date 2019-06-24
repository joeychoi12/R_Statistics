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
