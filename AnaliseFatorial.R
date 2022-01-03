#Passo 1: Carregar os pacotes que serao usados
library('readxl')
library('MVA')
library('MASS')
library('fpp')
library('lattice')
library('dplyr')
library('rstatix')
library('DescTools')
library('emmeans')
library('ggplot2')
library('psych')
library('GPArotation')


setwd("C:/Users/55119/Desktop/4° SEMESTRE/MQAAE")#Especifica um diretório padrão desejado
getwd()#Checa o diretório

#Passo 2: Carregar o banco de dados
dados <- read_excel("BD COM TAXA PETRÓLEO - TRABALHO 5.xlsx",sheet= "Planilha1")#Importando um arquivo excel do diretório
View(dados)#Para visualizar

#Passo 3: Pressupostos
qqnorm(dados$Acoes, col="red",xlab = "eixo x", ylab = "eixo y",main = "Acoes")
qqline(dados$Acoes, col= "black")

#Normalidade
shapiro.test(dados$Acoes)
shapiro.test(dados$Cambio)
shapiro.test(dados$IPCA)
shapiro.test(dados$Petroleo)
shapiro.test(dados$DI)

ks.test(dados$Acoes,"pnorm",mean(dados$Acoes),sd(dados$Acoes))
ks.test(dados$DI,"pnorm",mean(dados$DI),sd(dados$DI))
ks.test(dados$Cambio,"pnorm",mean(dados$Cambio),sd(dados$Cambio))
ks.test(dados$Petroleo,"pnorm",mean(dados$Petroleo),sd(dados$Petroleo))
ks.test(dados$IPCA,"pnorm",mean(dados$IPCA),sd(dados$IPCA))


#Teste de Homocedasticidade
fligner.test(dados$Acoes~dados$IPCA)
fligner.test(dados$Acoes~dados$Cambio)
fligner.test(dados$Acoes~dados$DI)
fligner.test(dados$Acoes~dados$Petroleo)


#Passo 4: KMO
KMO(dados)
?KMO

#Passo 5: Número de fatores
parallel <- fa.parallel(dados, fm ='minres', fa = 'fa')

#Passo 6:Estimando o modelo com dois fatores, sem rotação
fit <- fa(dados, 2, rotation="none")
print(fit)

#Estimando o modelo, com dois fatores, com rotação ortogonal varimax
fit.v <- fa(dados, 2, rotation="varimax")
print(fit.v)

#Outra forma de fazer 
fit <- factanal(dados, 2, rotation="varimax")
print(fit)

nfactors(dados, 8, rotate="nome")

#Visualizar só os scores
print(fit$loadings,cutoff = 0.2)
print(fit.v$loadings,cutoff = 0.2)


scores <- fit.v$scores
scores

load <- fit.v$loadings[,1:2]

#Gráfico do modelo com os dois fatores
load <- fit.v$loadings[,1:2]
plot(load, type="n")
text(load, labels=names(dados), cex=.7)