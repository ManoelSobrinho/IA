library(dplyr)
library(class)

# dados = sample(read.csv("emotions.arff", header = FALSE, sep = ",", comment.char = "@"), , replace = FALSE)

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

previsao = knn(base_treinamento[,-73], base_teste[,-73], cl = base_treinamento[,73], k = 20)

matriz_confusao = table(base_teste[,73], previsao)
matriz_confusao
