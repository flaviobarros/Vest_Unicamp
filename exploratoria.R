## Carregando pacotes
library(plyr)
library(psych)
library(ggplot2) 
library(reshape2)

## Lendo o conjunto de dados
fase1 <- read.delim(file='V2000_Fase1.txt')
fase2 <- read.delim(file='V2000_Fase2.txt')
social1 <- read.delim('V2000_QSEPR.txt')
social2 <- read.delim('V2000_QSETT.txt')

## Tratamento da fase1
fase1$ENEM2 <- as.numeric(gsub(fase1$ENEM2, pattern=',', replacement='.'))
fase1 <- fase1[fase1$PRESF1==1,]
drops <- c('INSCRI2', 'ANO', 'PRESF1', 'REDCORR')
fase1 <- fase1[,!(names(fase1) %in% drops)]
fase1$Q13 <- as.numeric(fase1$Q13)

rownames(fase1) <- NULL

## Tratamento da fase2
ind <- grepl(pattern='PRE*', names(fase2))
pre <- names(fase2)[ind]
fase2$APTD <- as.factor(fase2$APTD)
fase2$AREA <- as.factor(fase2$AREA)
fase2$CURSO <- as.factor(fase2$CURSO)
fase2$GRUPO <- as.factor(fase2$GRUPO)

## Refazendo os histogramas
hist(fase1$ENEM2)
hist(fase1$RED)
hist(fase1$Q13)
hist(fase1$FASE1)

## Sumário
describe(fase1[,-1])

## Correlações
cor(fase1[,14:17], use='complete.obs')
qplot(x=Var1, y=Var2, data=melt(cor(fase1[,14:17], use='complete.obs')), fill=value, geom="tile")

## Cálculo dos acumulados nas disciplinas
matematica <- fase2[,grepl(pattern='MATE*', names(fase2))] 
portugues <- fase2[,grepl(pattern='PORT*', names(fase2))]
biologia <- fase2[,grepl(pattern='BIOL*', names(fase2))]
quimica <- fase2[,grepl(pattern='QUIM*', names(fase2))]
historia <- fase2[,grepl(pattern='HIST*', names(fase2))]
fisica <- fase2[,grepl(pattern='FISI*', names(fase2))]
geografia <- fase2[,grepl(pattern='GEOG*', names(fase2))]

## Soma das notas
fase2$matsum <- rowSums(matematica[,-1], na.rm=T)
fase2$portsum <- rowSums(portugues[,-1], na.rm=T)
fase2$biosum <- rowSums(biologia[,-1], na.rm=T)
fase2$quimsum <- rowSums(quimica[,-1], na.rm=T)
fase2$histsum <- rowSums(historia[,-1], na.rm=T)
fase2$fisisum <- rowSums(fisica[,-1], na.rm=T)
fase2$geosum <- rowSums(geografia[,-1], na.rm=T)

## Correlação entre as somas de notas
cor(fase2[,119:125])
qplot(x=Var1, y=Var2, data=melt(cor(fase2[,119:125])), fill=value, geom="tile")

## Sumário da fase2
describe(fase2[,119:125])

## Preparação de social 1
social1$PAAIS <- as.factor(social1$PAAIS)

social1$ISENTOS <- as.factor(social1$ISENTOS)
levels(social1$ISENTOS) <- c('não isentos', 'isentos')

social1$APROVF2 <- as.factor(social1$APROVF2)
levels(social1$APROVF2) <- c('reprovado', 'aprovado')

social1$MATR <- as.factor(social1$MATR)
levels(social1$MATR) <- c('não matriculado', 'matriculado')
