#Passo 1: Carregar os pacotes que serao usados
library('readxl')
library('MVA')
library('MASS')
library('fpp')
library('tidyverse')
library('caret')
library('lattice')


if(!require(dplyr)) install.packages("dplyr")
library(dplyr)
if(!require(car)) install.packages("car")
library(car)
if(!require(rstatix)) install.packages("rstatix")
library(rstatix)
if(!require(DescTools)) install.packages("DescTools")
library(DescTools)
if(!require(emmeans)) install.packages("emmeans")
library(emmeans)
if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)
if(!require(psych)) install.packages("psych")
library(psych)


setwd("C:/Users/55119/Desktop/4° SEMESTRE/MQAAE")#Especifica um diretório padrão desejado
getwd()#Checa o diretório

#Passo 2: Carregar o banco de dados
dados <- read_excel("BD COM TAXA PETRÓLEO - TRABALHO 4.xlsx",sheet= "Planilha1")#Importando um arquivo excel do diretório
View(dados)#Para visualizar

#pairs(dados[,1:5])

KMO(dados)
?KMO




#Passo 3: Verificando os pressupostos 
#Normalidade - Shapiro por grupos: 
dados %>% group_by(Acontecimentos) %>%
  shapiro_test(Acoes)

#Passo 4: Presenca de outliers
boxplot(dados$Acoes~dados$Acontecimentos)

dados %>% group_by(Acontecimentos) %>%
  identify_outliers(Acoes)

#Passo 5: Testando a homogeneidade
bartlett.test(dados$Acoes ~ dados$Acontecimentos)

#Passo 6: Estimando a função discriminante linear
d.l <- lda(Acontecimentos ~ Cambio + DI + IPCA + Petroleo, data = dados)
plot(d.l)
print(d.l)

#Passo 7: Predicao
p.l <- d.l %>% predict(dados)

head(p.l$posterior,18)

head(p.l$x,18)

#Passo 8: Analise da qualidade do modelo (por resubtituicao)
l <- mean(p.l$class == dados$Acontecimentos)
print(l)

table(dados$Acontecimentos, p.l$class, dnn=c("Real", "Classificado"))

table(dados$Acontecimentos, p.l$class, dnn=c("Real", "Classificação")) %>% 
  prop.table(1) %>% round(3)

#Passo 9: Matriz de confusao
predito <- p.l$class
observado <- as.factor(dados$Acontecimentos)
confusionMatrix(predito,observado)
