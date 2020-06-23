# Países estudados: China, Itália, EUA, Espanha, França, Coréia do Sul, Alemanha, Reino Unido, Brasil

# Densidade demográfica
# 144.28 206.57 33.48 97.61 104.64 515.62 225.36 267.25 24.52	

# Tamanho populacional
# 1,384,689,024 62,246,672 329,256,480 49,331,076 67,364,360 51,418,096 80,457,736 65,105,248 208,846,896

# Média da idade da população
# 37,4 45,5 38,1 42,7 41,4 41,8 47 40,5 32,6

# Taças mundiais
# 0 4 0 1 2 0 4 1 5

# IDH
# 0,758 0,883 0,920 0,893 0,891 0,877 0,939 0,920 0,761

# Quantidade total de casos
# 81054, 59139, 32356, 28603, 16018, 8897, 24852, 5683, 1209

dados <- matrix(c(81054, 144.28, 1384689024, 37.4, 0.758,
59139, 206.57, 62246672, 45.5, 0.883,
32356, 33.48, 329256480, 38.1, 0.920,
28603, 97.61, 49331076, 42.7, 0.893,
16018, 104.64, 67364360, 41.4, 0.891,
8897, 515.62, 51418096, 41.8, 0.877,
24852, 225.36, 80457736, 47, 0.939,
5683, 267.25, 65105248, 40.5, 0.920,
1209, 24.52, 208846896, 32.6, 0.761),nrow=9,ncol=5,byrow=TRUE)

y <- dados[,1]
densDem <- dados[,2]
tamPop <- dados[,3]
mediaIdade <- dados[,4]
idh <- dados[,5]

ajuste <- lm(y ~ densDem + tamPop + mediaIdade + idh, family=binomial())
summary(ajuste)

ajuste1 <- lm(y ~ densDem + tamPop + mediaIdade, family=binomial())
summary(ajuste1)

ajuste2 <- lm(y ~ tamPop + mediaIdade, family=binomial())
summary(ajuste2)

ajuste3 <- lm(y ~ densDem, family=binomial())
summary(ajuste3)


