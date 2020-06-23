library(cluster)

data = read.csv("C:\\Users\\Manoel Cleonaldo\\Downloads\\Live.csv", header = TRUE, sep = ",", comment.char = "@")

# View(data)

data1 <- data[,-1]
# View(data1)
data2 <- data1[,-1]
# View(data2)
data3 <- data2[,-1]
# View(data3)
data4 <- data3[,1:9]
# View(data4)

dados <- scale(data4)

# View(dados)

dados_hc <- hclust(d = dist(dados, method = 'euclidean'), method = 'ward.D')

plot(dados_hc)

previsao <- cutree(dados_hc, 3)
previsao

plot(dados, col=previsao)

clusplot(dados, previsao, color = TRUE, lines = FALSE, labels = 4)
