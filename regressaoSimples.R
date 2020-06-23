library(ggplot2)

# Estudo no Brasil

# Variável resposta
Y <- c(1,1,1,2,2,2,2,3,8,13,19,25,30,34,69,78,98,121,200,234,291,428,621,904,1128) 

# Variável explicativa
X <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25)    

dados <- data.frame(Y,X)

modelo.regressao <- lm(Y ~ X, data= dados)

summary(modelo.regressao)

plot (Y ~ X,pch=16 ,data = dados)

# Esta função ajusta a reta do modelo aos dados
# abline(modelo.regressao,col="red") 

ggplot(data=dados,aes(y=Y,x=X))+geom_point()+geom_smooth(method="lm")

# Diagnóstico completo dos resíduos

par(mfrow=c(2,2))
plot(modelo.regressao) 

#############################################################################

# Estudo na China

# Variável resposta
Z <- c(571,830,1287,1975,2744,4515,5974,7711,9692,11791,14380,17205,20440,24324,28018,31161,34546,37198,40171,42638,44653,58761,63851,66492,68500,70548,72436,74185,74576,75465,76288,76936,77150,77658,78064,78497,78824,79251,79824,80026,80151,80270,80409,80552,80651,80695,80735,80754,80778,80793,80813,80824,80844,80860,80881,80894,80928,80967,81008,81054) 

# Variável explicativa
W <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60)    

dados1 <- data.frame(Z,W)

modelo.regressao1 <- lm(Z ~ W, data= dados1)

summary(modelo.regressao1)

plot (Z ~ W,pch=16 ,data = dados1)

# Esta função ajusta a reta do modelo aos dados
# abline(modelo.regressao,col="red") 

ggplot(data=dados1,aes(y=Z,x=W))+geom_point()+geom_smooth(method="lm")

# Diagnóstico completo dos resíduos

par(mfrow=c(2,2))
plot(modelo.regressao1)

#############################################################################

# Estudo na Itália

# Variável resposta
A <- c(3,3,3,3,3,4,21,79,157,229,323,470,655,889,1128,1701,2036,2502,3089,3858,4636,5883,7375,9172,10149,12462,15113,17660,21157,24747,27980,31506,35713,41035,47021,53578) 

# Variável explicativa
B <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36)    

dados2 <- data.frame(A,B)

modelo.regressao2 <- lm(A ~ B, data= dados2)

summary(modelo.regressao2)

plot (A ~ B,pch=16 ,data = dados2)

# Esta função ajusta a reta do modelo aos dados
# abline(modelo.regressao,col="red") 

ggplot(data=dados2,aes(y=A,x=B))+geom_point()+geom_smooth(method="lm")

# Diagnóstico completo dos resíduos

par(mfrow=c(2,2))
plot(modelo.regressao2)

#############################################################################

# Estudo na Espanha

# Variável resposta
O <- c(2,2,2,2,2,2,2,2,2,3,9,13,25,33,58,84,120,165,228,282,401,525,674,1231,1695,2277,3146,5232,6391,7988) 

# Variável explicativa
P <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)    

dados5 <- data.frame(O,P)

modelo.regressao5 <- lm(O ~ P, data= dados5)

summary(modelo.regressao5)

plot (O ~ P,pch=16 ,data = dados5)

# Esta função ajusta a reta do modelo aos dados
# abline(modelo.regressao,col="red") 

ggplot(data=dados5,aes(y=O,x=P))+geom_point()+geom_smooth(method="lm")

# Diagnóstico completo dos resíduos

par(mfrow=c(2,2))
plot(modelo.regressao5)

#############################################################################

# Estudo no EUA

# Variável resposta
J <- c(15,15,15,15,15,15,35,35,35,53,57,60,60,63,68,75,100,124,158,221,319,435,541,704,994,1301,1697,2247,2943,3680) 

# Variável explicativa
K <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)    

dados4 <- data.frame(J,K)

modelo.regressao4 <- lm(J ~ K, data= dados4)

summary(modelo.regressao4)

plot (J ~ K,pch=16 ,data = dados4)

# Esta função ajusta a reta do modelo aos dados
# abline(modelo.regressao,col="red") 

ggplot(data=dados4,aes(y=J,x=K))+geom_point()+geom_smooth(method="lm")

# Diagnóstico completo dos resíduos

par(mfrow=c(2,2))
plot(modelo.regressao4)

#############################################################################

# Multiplicadores Espanha
# 1.343, 1.381, 1.663, 1.221, 1.249

U <- c(1,1,1,2,2,2,2,3,8,13,19,25,30,34,69,78,98,121,200,234,291,428,621,904,1128,1514,2090,3475,4242,5298) 

R <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)

dados7 <- data.frame(U,R)

modelo.regressao7 <- lm(U ~ R, data= dados7)

summary(modelo.regressao7)

plot (U ~ R,pch=16 ,data = dados7)

# Esta função ajusta a reta do modelo aos dados
# abline(modelo.regressao,col="red") 

ggplot(data=dados7,aes(y=U,x=R))+geom_point()+geom_smooth(method="lm")

# Diagnóstico completo dos resíduos

par(mfrow=c(2,2))
plot(modelo.regressao7)

#############################################################################

# Multiplicadores EUA
# 1.308, 1.304, 1.324, 1.311, 1.250

M <- c(1,1,1,2,2,2,2,3,8,13,19,25,30,34,69,78,98,121,200,234,291,428,621,904,1128,1475,1923,2546,3337,4171) 

N <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)

dados3 <- data.frame(M,N)

modelo.regressao3 <- lm(M ~ N, data= dados3)

summary(modelo.regressao3)

plot (M ~ N,pch=16 ,data = dados3)

# Esta função ajusta a reta do modelo aos dados
# abline(modelo.regressao,col="red") 

ggplot(data=dados3,aes(y=M,x=N))+geom_point()+geom_smooth(method="lm")

# Diagnóstico completo dos resíduos

par(mfrow=c(2,2))
plot(modelo.regressao3)

plot(modelo.regressao3,modeloregressao7)

