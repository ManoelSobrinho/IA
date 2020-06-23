
# Os dados usados foram omitidos a pedido de quem forneceu eles (De Bustamante Simas)

dados <- matrix(c(...),nrow=28,ncol=7,byrow=TRUE)
 
y=dados[,1]
idade=dados[,2]
sexo=dados[,3]
superior=dados[,4]
solteiro=dados[,5]
alcool=dados[,6]
trabalha=dados[,7]

# Ajustes feitos

ajuste=glm(y ~ idade + sexo + superior + solteiro + alcool + trabalha, family=binomial())
summary(ajuste)

ajuste2=glm(y ~ idade + sexo + superior + alcool + trabalha, family=binomial())
summary(ajuste2)

ajuste3=glm(y ~ idade + sexo + alcool + trabalha, family=binomial())
summary(ajuste3)

ajuste4=glm(y ~ idade + sexo + trabalha, family=binomial())
summary(ajuste4)

ajuste5=glm(y ~ idade + trabalha, family=binomial())
summary(ajuste5)

par(mfrow=c(2,2))

plot(ajuste4)

# Cálculo das Probabilidades

st=(exp(-4.39272 +0.17138*30-4.32985 ))/(1+exp(-4.39272+0.17138*30 -4.32985 ))

snt=(exp(-4.39272+0.17138*30))/(1+exp(-4.39272+0.17138*30 ))

# Vetor do ajuste5

ajuste5$fitted.values

# A análise ROC foi feita no software RGUI utilizando as bibliotecas Rcmdr e RcmdrPlugin.ROC que é manual.
