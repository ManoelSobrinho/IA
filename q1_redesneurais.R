library(h2o)

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

dados2 <- group_by(dados1,regressao2$fitted.values)

dados3 <- scale(dados2)

base_treinamento = dados3[1:465,]
base_teste = dados3[466:593,]

h2o.init(nthreads = -1)

base_treinamento <- as.h2o(base_treinamento)
base_teste <- as.h2o(base_teste)

classificador = h2o.deeplearning(y = 'regressao2$fitted.values',
                                 training_frame = base_treinamento,
                                 activation = 'Rectifier',
                                 hidden = c(30,30),
                                 epochs = 100)
                                 
plot(classificador)

previsoes <- h2o.predict(classificador, newdata = as.h2o(base_teste[,-73]))

previsoes <- (previsoes>0.5)
previsoes <- as.vector(previsoes)

matriz_confusao  <- table(base_teste[,73], previsoes)