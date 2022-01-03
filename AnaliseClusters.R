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
library("factoextra")
library("NbClust")


setwd("C:/Users/55119/Desktop/4° SEMESTRE/MQAAE")#Especifica um diretório padrão desejado
getwd()#Checa o diretório

#Passo 2: Carregar o banco de dados
dados <- read_excel("BD COM TAXA PETRÓLEO - TRABALHO 6.xlsx",sheet= "Planilha1")#Importando um arquivo excel do diretório
View(dados)#Para visualizar

#1°Analise das variaveis e dos objetos a serem agrupados
fact <- data.frame(dados[,2:6], row.names = dados$Acontecimentos)
View(fact)

#Calculando a distancia de Mahalonobis
#Detectando variaveis atipicas 
p.cov <- var(scale(fact))
p.cov <- var(fact)
p.mean <- apply(fact, 2, mean)
p.mah <- mahalanobis(fact,p.mean,p.cov)
View(p.mah)

#Variancia 
apply(fact,2,var)

#2° Selecao de criterio de agrupamento
d.eucl <- dist(fact, method = "euclidean")

#Visualizando
round(as.matrix(d.eucl)[424:432,424:432], 1)

#3° Selecao do algoritmo de agrupamento
# Hierarquico x nao hierarquico 

res.hc <- hclust(d = d.eucl, method = "average")

#Calculando a matriz cofenetica
res.coph <- cophenetic(res.hc)
#Correlacao entre a distancia cofenetica e distancia original
cor(d.eucl, res.coph)

#4° Definicao do numero de agrupamento

help(fviz_dend)

#Obtendo o dendograma
fviz_dend(res.hc, cex = 0.5)


nb <- NbClust(fact, distance = "euclidean", min.nc = 2, 
              max.nc = 10, method = "average", index = "all")

#Obtendo os indicadores
fviz_nbclust(nb)


#5° Interpretacao e validacao dos agrupamentos
#Obtendo os agrupamentos
g <- cutree(res.hc, k=4)
#Numero de membros em cada agrupamento
table(g)

#Obtendo o nome dos membros no agrupamento 1
rownames(fact)[g == 1]
rownames(fact)[g == 3]
rownames(fact)[g == 4]

#Calculando a media em cada grupo
clust.centroid = function(i, dat, g){ 
  ind = (g == i)
  colMeans(dat[ind,])
  }

sapply(unique(g), clust.centroid, fact, g)
mat <- t(as.matrix(sapply(unique(g), clust.centroid, fact, g)))

#Analise de dois estagios
fviz_nbclust(fact, kmeans, method = "wss")+
  geom_vline(xintercept= 4, linetype = 2)


set.seed(123)
km.res <- kmeans(fact, 4, nstart = 25)
print(km.res)

k <- cbind(fact, Grupos=km.res$cluster)
k


