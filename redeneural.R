data = sample(read.csv("emotions.arff", header = FALSE, sep = ",", comment.char = "@"), , replace = FALSE)

base_treinamento = data[1:465,]
base_teste = data[466:593,]

library(h2o)

h2o.init(nthreads = -1)

base_treinamento = as.h2o(base_treinamento)
base_teste = as.h2o(base_teste)

classificador = h2o.deeplearning(y = data[,73:78],
                                 training_frame = base_treinamento,
                                 activation = "Rectfier",
                                 hidden = c(30,30),
                                 epochs = 100)
                                 
classificador
plot(classificador)

previsoes = h2o.predict(classificador, base_teste[data[,73:78]])

previsoes
previsoes = as.vector(previsoes)

library(miscTools)

h2o.shutdown()

