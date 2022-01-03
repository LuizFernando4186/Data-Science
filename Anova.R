
#Carregar os pacotes que serao usados
library('readxl')
library('MVA')
library('MASS')
library('fpp')
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
dados <- read_excel("DADOS TRANSFORMADOS E NORMALIZADO.xlsx",sheet= "Planilha1")#Importando um arquivo excel do diretório
View(dados)#Para visualizar
glimpse(dados)


#Passo 3: Verificando os pressupostos 
#Normalidade - Shapiro por grupos: 
dados %>% group_by(Ano) %>%
shapiro_test(Acoes)

#Passo 4: Verificao da presenca de Outliers por grupo
boxplot(dados$Acoes~dados$Ano)

dados %>% group_by(Ano) %>%
  identify_outliers(Acoes)

#Passo 5: Verificando a homogeineidade de variancia - teste de Levene (pacote car)
leveneTest(Acoes ~ Ano,dados, center=mean)#diz que o centro e a mediana

#Passo 6: Realizacao da ANOVA
anova_BV <- aov(Acoes ~ Ano, dados)
summary(anova_BV)


#Passo 7: Analise post-hoc - pacote DescTools

#Uso do Duncan
PostHocTest(anova_BV, method = "duncan", conf.level = 0.95)

#Uso do TukeyHSD
PostHocTest(anova_BV, method = "hsd", conf.level = 0.95)

#Uso do Bonferroni
PostHocTest(anova_BV, method = "bonf", conf.level = 0.95)

#Passo 8: Analise descritiva dos dados

describeBy(dados$Acoes, group = dados$Ano)

