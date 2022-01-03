library('readxl')
library('MVA')
library('gmodels')
library('ggplot2')

setwd("C:/Users/55119/Desktop/4° SEMESTRE/MQAAE")#Especifica um diretório padrão desejado
getwd()#Checa o diretório

#Passo 1: Carregar os pacotes que serão usados

if(!require(pacman)) install.packages("pacman")
library(pacman)

pacman::p_load(dplyr, car, rstatix, lmtest, ggpubr, 
               QuantPsyc,psych, scatterplot3d)

#Passo 2: Carregar o banco de dados
dados <- read_excel("DADOS TRANSFORMADOS E NORMALIZADO.xlsx",sheet= "Planilha1")#Importando um arquivo excel do diretório
View(dados)#Para visualizar
glimpse(dados)

#Verifica a correlação entre as variáveis
cor(dados$Acoes,dados$Cambio)


#Passo 3: Construção do modelo:

mod1 <- lm(Acoes ~ Cambio, dados)
mod2 <- lm(Acoes ~ DI, dados)
mod3 <- lm(Acoes ~ IPCA, dados)
mod4 <- lm(Acoes ~ IPCA + Cambio, dados)
mod5 <- lm(Acoes ~ Cambio + DI, dados)
mod6 <- lm(Acoes ~ IPCA + DI, dados)
mod7 <- lm(Acoes ~ IPCA + DI + Cambio, dados)


#Passo 4: Análise gráfica
par(mfrow=c(2,2))
plot(mod1)

#Normalidade dos resíduos
shapiro.test(mod$residuals)


#Outliers nos resíduos:
summary(rstandard(mod))#Avaliar os resíduos padronizados

#Independência dos resíduos(Durbin-watson)
durbinWatsonTest(mod)

#Homocedasticidade (Breusch-Pagan)
bptest(mod)

#Ausência de Multicolinearidade
pairs.panels(dados)#r > 0.9 (ou 0.8)
vif(mod)#VIF > 10

#Passo 5: Análise do modelo
summary(mod1)#H1 tem impacto

#Coeficiente padronizado
lm.beta(mod1)#Quem está mais associado
lm.beta(mod2)#Quem está mais associado
lm.beta(mod3)#Quem está mais associado
lm.beta(mod4)#Quem está mais associado
lm.beta(mod5)#Quem está mais associado
lm.beta(mod6)#Quem está mais associado



#Obtenção do IC 95% para os coeficientes
confint(mod)

#AIC e BIC - Comparação entre quaisquer modelos
AIC(mod1,mod2,mod3,mod4,mod5,mod6,mod7) #quanto menor melhor explica
BIC(mod1,mod2,mod3,mod4,mod5,mod6,mod7)


#anova(mod, mod2)
#O melhor é aquele com menor valor de RSS 


#Gráfico de dispersão
graph <- scatterplot3d(dados$Acoes ~ dados$Cambio + dados$IPCA,
                  pch=16, angle=30, color="steelblue", box=FALSE,
                  xlab = "Cambio", ylab = "IPCA", zlab = "Ibovespa")

graph$plane3d(mod, col="black", draw_polygon=TRUE)




plot(y = dados$Acoes,
     x = dados$Cambio, main = 'Relação entre Ibovespa e Cambio',
     xlab = 'Cambio', ylab = 'Ibovespa', col = 'green')

grid()

regressao <- lm(Acoes ~ Cambio,dados)
abline(a = 31571.5, b = -12634.6)#Coloca o intercepto e o coeficiente linear


residuos <- function(regressao) {
  par(mfrow=c(2,2))
  plot(regressao)
}

dispersao <- function(regressao, x, y, xlab, ylab, title, color = "blue") {
  par(mfrow=c(1,1))
  plot(y = ibov,
       x = dados[, index], main = paste('Ibovespa X ', name),
       xlab = name, ylab = 'Ibovespa', col = colors[i])
  
  cil <- confint(regressao)[,2]
  abline(a = cil[1], b = cil[2])
}

dispersao3d <- function(regressao, dados, xlab, ylab, zlab) {
  par(mfrow=c(1,1))
  plot(mod1)
  graph <- scatterplot3d(dados, pch=16, angle=30, color="steelblue", box=FALSE,
                         xlab = xlab, ylab = ylab, zlab = zlab)
  graph$plane3d(regressao, col="black", draw_polygon=TRUE)
}


colors <- c("blue", "red", "green") # cores
ibov <- dados$Acoes # variavel de analise 
dep <- dados[, 3:5] # variaveis dependentes
names <-  colnames(dep) # nomes das colunas

for (i in seq_along(names)) {
  name <- names[i]
  index <- i + 2
  
  # Modelo
  regressao <- lm(ibov ~ dados[, index], data = dados)
  
  print(summary(rstandard(regressao))) 
  print(summary(regressao))
  
  dispersao(regressao, dados[, index], ibov, name, "Ibovespa", paste('Ibovespa X ', name), colors[i])
  residuos(regressao)
}


# regressao multipla

# IPCA + DI
regressao <- lm(Acoes ~ IPCA + DI, dados)
dispersao3d(regressao = regressao, dados = dados$Acoes ~ dados$IPCA + dados$DI, xlab="IPCA", ylab="DI", zlab="Ibovespa")
residuos(regressao)

print(summary(rstandard(regressao))) 
print(summary(regressao))

# Cambio + DI
regressao <- lm(Acoes ~ Cambio + DI, dados)
dispersao3d(regressao = regressao, dados = dados$Acoes ~ dados$Cambio + dados$DI, xlab="Cambio", ylab="DI", zlab="Ibovespa")
residuos(regressao)

print(summary(rstandard(regressao))) 
print(summary(regressao))

# Cambio + IPCA
regressao <- lm(Acoes ~ Cambio + IPCA, dados)
dispersao3d(regressao = regressao, dados = dados$Acoes ~ dados$Cambio + dados$IPCA, xlab="Cambio", ylab="IPCA", zlab="Ibovespa")
residuos(regressao)

print(summary(rstandard(regressao))) 
print(summary(regressao))


