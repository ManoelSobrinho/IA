library(ggplot2)
library(dplyr)
library(class)
library(e1071)

dados = read.csv("emotions.arff", header = FALSE, sep = ",", comment.char = "@")

dados1 = dados[,1:72]

a <- dados[,73]
b <- dados[,74]
c <- dados[,75]
d <- dados[,76]
e <- dados[,77]
f <- dados[,78] 

regressao <- lm(a~b+c+d+e+f)

summary(regressao)

regressao1 <- lm(a~c+d+e+f)

summary(regressao1)

regressao2 <- lm(a~c+d+e)

summary(regressao2)

regressao2$fitted.values

dados2 <- group_by(dados1,regressao$fitted.values)

dados3 <- scale(dados2)

base_treinamento = dados3[1:465,]
base_teste = dados3[466:593,]

svmfit <- svm(Species ~., data = base_treinamento, kernel = "linear", cost = .1, scale = FALSE)