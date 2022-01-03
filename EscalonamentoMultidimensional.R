#Passo 1: Bibliotecas
if(!require(dplyr)) install.packages("vegan")
library(vegan)
library('readxl')
require('ggplot2')



setwd("C:/Users/55119/Desktop/4° SEMESTRE/MQAAE")#Especifica um diretório padrão desejado
getwd()#Checa o diretório

#Passo 2: Carregar o banco de dados
dados <- read_excel("BD COM TAXA PETRÓLEO - TRABALHO 4.xlsx",sheet= "Planilha1")#Importando um arquivo excel do diretório
View(dados)#Para visualizar

data_1 <- dados[,1]
data_2 <- dados[,2:6]


#Passo 3: Distancia de bray
NMDS_bray <- metaMDS(data_2,k=2, distance = "bray")

co=c("red","blue","yellow","darkgrey")
shape=c(18,16,17,20)
plot(NMDS_bray$points, col=co[data_1$Acontecimentos], pch = shape[data_1$Acontecimentos],
     cex=1.2, main = "Mercado Financeiro", xlab ="Axis 1", ylab ="Axis 2")



envfit(NMDS_bray,fact)

#Passo 4: Score de ajustamento, precisa ser menor que 0.3
NMDS_bray$stress

#Grafico 
plot(NMDS_bray)

#Passo 5: Grupos
grupo <- data_1
grupo.nivel <- levels(grupo)
grupo


#Passo 6: Scores
escores.nmds <- scores(NMDS_bray)
escores.nmds

#Passo 7: Plot
ordiplot(escores.nmds, type = "t")
points(escores.nmds[grupo == "Crise_Mundial",], pch =(18), cex=2, col="black")
points(escores.nmds[grupo == "Internacional",], pch =(19), cex=2, col="darkgrey")
points(escores.nmds[grupo == "Copa_Do_Mundo",], pch =(20), cex=2, col="yellow")
points(escores.nmds[grupo == "Crise_Política",], pch =(17), cex=2, col="red")


