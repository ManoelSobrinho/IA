library(FactoMineR)
library(factoextra)
library(cluster)
library(xlsx)

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

fviz_nbclust(dados, kmeans, method = "gap_stat")

dados_kmeans <- kmeans(dados, 7)

fviz_cluster(dados_kmeans, data = dados)