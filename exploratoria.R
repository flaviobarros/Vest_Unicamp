## Carregando pacotes
library(plyr)
library(psych)
library(ggplot2) 
library(reshape2)
library(extRemes)
library(AER)

## Funções necessárias
convert.magic <- function(obj, type){
  FUN1 <- switch(type,
                 character = as.character,
                 numeric = as.numeric,
                 factor = as.factor)
  out <- lapply(obj, FUN1)
  as.data.frame(out)
}

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


## Base sem "NA"
fase1.zero <- fase1
fase1.zero[is.na(fase1.zero)] <- 0
fase2.zero <- fase2
fase2.zero[is.na(fase2.zero)] <- 0

social1.zero <- social1
social1.zero[is.na(social1.zero)] <- 0

## Na base social2 o número de inscrição é concatenado com "2000", assim, para padronizar com as outras bases,
INSCRI3 <- social1.zero$INSCRI2 - 20000000000
social1.zero <- cbind(INSCRI3,social1.zero)

social2.zero <- social2
social2.zero[is.na(social2.zero)] <- 0

#################################################################

## Estatísticas descritivas ##
cor(social1.zero)
cor(social2.zero)
cor(fase1[,14:17], use='complete.obs')

summary(fase1.zero$Q13)
summary(social1.zero)
summary(social2.zero)

as.data.frame(table(social2.zero$P1))
as.data.frame(table(social2.zero$P2))
as.data.frame(table(social2.zero$P3))
as.data.frame(table(social2.zero$P4))
as.data.frame(table(social2.zero$P5))
as.data.frame(table(social2.zero$P6))
as.data.frame(table(social2.zero$P7))
as.data.frame(table(social2.zero$P8))
as.data.frame(table(social2.zero$P9))
as.data.frame(table(social2.zero$P10))
as.data.frame(table(social2.zero$P11))
as.data.frame(table(social2.zero$P12))
as.data.frame(table(social2.zero$P13))
as.data.frame(table(social2.zero$P14))
as.data.frame(table(social2.zero$P15))
as.data.frame(table(social2.zero$P16))
as.data.frame(table(social2.zero$P17))
as.data.frame(table(social2.zero$P18))
as.data.frame(table(social2.zero$P19))
as.data.frame(table(social2.zero$P20))
as.data.frame(table(social2.zero$P21))
as.data.frame(table(social2.zero$P22))
as.data.frame(table(social2.zero$P23))
as.data.frame(table(social2.zero$P24))
as.data.frame(table(social2.zero$P25))
as.data.frame(table(social2.zero$P26))
as.data.frame(table(social2.zero$P27))
as.data.frame(table(social2.zero$P28))
as.data.frame(table(social2.zero$P29))
as.data.frame(table(social2.zero$P30))
as.data.frame(table(social2.zero$P31))
as.data.frame(table(social2.zero$P32))
as.data.frame(table(social2.zero$P33))
as.data.frame(table(social2.zero$P34))
as.data.frame(table(social2.zero$P35))
as.data.frame(table(social2.zero$P36))
as.data.frame(table(social2.zero$P37))
as.data.frame(table(social2.zero$P38))
as.data.frame(table(social2.zero$P39))
as.data.frame(table(social2.zero$P40))
as.data.frame(table(social2.zero$P41))
as.data.frame(table(social2.zero$P42))
as.data.frame(table(social2.zero$P43))
as.data.frame(table(social2.zero$P44))
#observamos que há muitas variáveis com grandes quantidades de respostas em branco, e categorias com quantidades pequenas de amostra. Devemos tomar cuidado ao inserir no modelo.
#Combinaremos a variável "Renda" com "Número de pessoas que vivem da renda" pois imaginamos que a interação renda x pessoas, ao invés de utilizar somente a renda, possa trazer mais informações sobre a situação financeira
#A interação P38 e P39 com o curso escolhido (ou a grande área do curso como Humanas e Exatas) possa ser uma variável interessante, pois o efeito de uma pessoa que lê filosofia ou poesia para quem vai prestar Letras ou Estudos Literários pode ser diferente de para quem irá prestar Matemática ou Estatística
#Acreditamos que a interação Sexo x Nível de instrução do pai e Sexo x Nível de instrução da mãe possa ser relevante, pois é comum o filho se espelhar no pai, e a filha se espelhar na mãe, ou alguma relação de sexo com pai vs mãe. 

#A quantidade da amostra da Fase1 é diferente das do social1 e social2, provavelmente por que o questionário socio-econômico é realizado antes da primeira fase, e na fase1 não deve conter informação de candidato que não foi realizar a prova. 2,5% dos candidatos faltarem ao vestibular parece plausível. Utilizaremos os dados somente dos candidatos em comum nas duas bases.
seletor <- social2.zero$INSCRI %in% fase1.zero$INSCRI
social2.zero2 <- social2.zero[seletor,]
seletor <- social1.zero$INSCRI3 %in% fase1.zero$INSCRI
social1.zero2 <- social1.zero[seletor,]
length(social1.zero2[,1])
length(social2.zero2[,1])

#Selecionamos 17 variáveis que pareciam ter bom comportamento, e que esperamos que tenha efeito sobre a nota da primeira fase. As variáveis são as seguintes:
# Sexo (P1)
# Idade (P2)
# Estado Civil (P3)
# Localização da Residência (P4)
# Universidades prestadas (P5)
# Tipo de escola, primeiro grau (P6)
# Tipo de escola, segundo rgau (P7)
# Se realizou cursinho (P11)
# Já cursou ensino superior (P17)
# Renda (P25)
# Número de pessoas na família que vivem da renda indicada (P26)
# Nível de instrução do pai (P31)
# Nível de instrução da mãe (P32)
# Exerce atividade remunerada (P33)
# O que lê (P38)
# Revista que assina (P39)
# Lê jornal (P40)

### criando tabela somente com as variáveis que iremos avaliar
variaveis.1 <- cbind(fase1.zero[,c('Q13')],social1.zero2[,c('OPC1')],social2.zero2[,c('P1','P2','P3','P4','P5','P6','P7','P8','P11','P16','P17','P25','P26','P31','P32','P33','P38','P39','P40')])
colnames(variaveis.1) <- c('Q13','OPC1','P1','P2','P3','P4','P5','P6','P7','P8','P11','P16','P17','P25','P26','P31','P32','P33','P38','P39','P40')

variaveis.1$OPC1 <- as.factor(variaveis.1$OPC1)
variaveis.1$P1 <- as.factor(variaveis.1$P1)
variaveis.1$P2 <- as.factor(variaveis.1$P2)
variaveis.1$P3 <- as.factor(variaveis.1$P3)
variaveis.1$P4 <- as.factor(variaveis.1$P4)
variaveis.1$P5 <- as.factor(variaveis.1$P5)
variaveis.1$P6 <- as.factor(variaveis.1$P6)
variaveis.1$P7 <- as.factor(variaveis.1$P7)
variaveis.1$P11 <- as.factor(variaveis.1$P11)
variaveis.1$P17 <- as.factor(variaveis.1$P17)
variaveis.1$P25 <- as.factor(variaveis.1$P25)
variaveis.1$P26 <- as.factor(variaveis.1$P26)
variaveis.1$P31 <- as.factor(variaveis.1$P31)
variaveis.1$P32 <- as.factor(variaveis.1$P32)
variaveis.1$P33 <- as.factor(variaveis.1$P33)
variaveis.1$P38 <- as.factor(variaveis.1$P38)
variaveis.1$P39 <- as.factor(variaveis.1$P39)
variaveis.1$P40 <- as.factor(variaveis.1$P40)

### Verificar correlação entre as variáveis
cor(variaveis.1)

attach(variaveis.1)
boxplot(Q13~OPC1,xlab="Primeira Opção",ylab="NotaFase1")
boxplot(Q13~P1,xlab="Sexo",ylab="NotaFase1")
boxplot(Q13~P2,xlab="Idade",ylab="NotaFase1")
boxplot(Q13~P3,xlab="Estado Civil",ylab="NotaFase1")
boxplot(Q13~P4,xlab="Localização da Residência",ylab="NotaFase1")
boxplot(Q13~P5,xlab="Universidades Prestadas",ylab="NotaFase1")
boxplot(Q13~P6,xlab="Tipo de Escola (1o Grau)",ylab="NotaFase1")
boxplot(Q13~P7,xlab="Tipo de Escola (2o Grau)",ylab="NotaFase1")
boxplot(Q13~P11,xlab="Realizou Cursinho",ylab="NotaFase1")
boxplot(Q13~P17,xlab="Cursou ensino superior",ylab="NotaFase1")
boxplot(Q13~P25,xlab="Renda",ylab="NotaFase1")
boxplot(Q13~P26,xlab="Número de dependentes da renda",ylab="NotaFase1")
boxplot(Q13~P31,xlab="Nível de Instrução do Pai",ylab="NotaFase1")
boxplot(Q13~P32,xlab="Nível de Instrução da Mãe",ylab="NotaFase1")
boxplot(Q13~P33,xlab="Exerce Atividade Remunerada",ylab="NotaFase1")
boxplot(Q13~P38,xlab="O que lê",ylab="NotaFase1")
boxplot(Q13~P39,xlab="Revista que assina",ylab="NotaFase1")
boxplot(Q13~P40,xlab="Lê Jornal",ylab="NotaFase1")

boxplot(Q13~P25*P26,xlab="Renda",ylab="NotaFase1") #Verificar o efeito da interação Renda x Qtd de pessoas que vivem com a renda indicada

#É possível ver que a interação Opção x O que lê faz sentido ao comparar o boxplot do Q13~P38, onde P38 é "o que lê" para o curso de estatística e para o curso de filosofia
boxplot(Q13[variaveis.1$OPC1==30]~P38[variaveis.1$OPC1==30],main="Curso = Filosofia",xlab="O que le",ylab="NotaFase1")
boxplot(Q13[variaveis.1$OPC1==2]~P38[variaveis.1$OPC1==2],main="Curso = Estatistica",xlab="O que le",ylab="NotaFase1")

### Para verificar se pode haver problema com multicolinearidade, calculamos o número de condições da matriz de variáveis. Como o número de condições é em torno de 65, e, segundo o artigo http://www.scielo.br/scielo.php?pid=S0103-84782005000200015&script=sci_arttext, na prática podemos interpretar que número de condição menor que 100, consideramos que não há problema com multicolinearidade
kappa(variavel.1)

#### Ajuste Regressão
ajuste.1 <- lm(Q13 ~ OPC1+P1+P2+P3+P4+P5+P6+P7+P11+P17+P25+P26+P25*P26+P31+P32+P33+P38+P39+P40+social1.zero2$ISENTOS)
vcov1 <- vcovHC(ajuste.1, type=c('HC1'))
coeftest(ajuste.1, vcov=vcov1)
summary(ajuste.1)
ajuste.2 <- lm(Q13 ~ OPC1+P1+P2+P4+P5+P6+P7+P11+P17+P25+P26+P25*P26+P31+P32+P33+P38+OPC1*P38+P40)
vcov2 <- vcovHC(ajuste.2, type=c('HC1'))
coeftest(ajuste.2, vcov=vcov2)
ajuste.3 <- lm(Q13 ~ OPC1+P1+P2+P4+P5+P6+P7+P11+P17+P25+P26+P25*P26+P31+P2*P31+P32+P2*P32+P33+P38+OPC1*P38+P40)
vcov3 <- vcovHC(ajuste.3, type=c('HC1'))
coeftest(ajuste.3, vcov=vcov3)
### as interações que propomos foram estatísticamente significativos.
ajuste.4 <- lm(Q13 ~ OPC1 +  social2.zero2$P1 +  social2.zero2$P2 +  social2.zero2$P3 +  social2.zero2$P4 +  social2.zero2$P5 +  social2.zero2$P6 +  social2.zero2$P7 +  social2.zero2$P8 +  social2.zero2$P9 +  social2.zero2$P10 +  social2.zero2$P11 +  social2.zero2$P12 +  social2.zero2$P13 +  social2.zero2$P14 +  social2.zero2$P15 +  social2.zero2$P16 +  social2.zero2$P17 +  social2.zero2$P18 +  social2.zero2$P19 +  social2.zero2$P20 +  social2.zero2$P21 +  social2.zero2$P22 +  social2.zero2$P23 +  social2.zero2$P24 +  social2.zero2$P25 +  social2.zero2$P26 +  social2.zero2$P27 +  social2.zero2$P28 +  social2.zero2$P29 +  social2.zero2$P30 +  social2.zero2$P31 +  social2.zero2$P32 +  social2.zero2$P33 +  social2.zero2$P34 +  social2.zero2$P35 +  social2.zero2$P36 +  social2.zero2$P37 +  social2.zero2$P38 +  social2.zero2$P39 +  social2.zero2$P40 +  social2.zero2$P41 +  social2.zero2$P42 +  social2.zero2$P43 +  social2.zero2$P44)
vcov4 <- vcovHC(ajuste.4, type=c('HC1'))
coeftest(ajuste.4, vcov=vcov4)
ajuste.5 <- lm(Q13 ~ OPC1 +  social2.zero2$P1 +  social2.zero2$P2 +  social2.zero2$P3 +  social2.zero2$P4 +  social2.zero2$P5 +  social2.zero2$P6 +  social2.zero2$P7 + social2.zero2$P9 +  social2.zero2$P10 +  social2.zero2$P11 +  social2.zero2$P12 +  social2.zero2$P13 +  social2.zero2$P14 +  social2.zero2$P15 +  social2.zero2$P16 +  social2.zero2$P17 +  social2.zero2$P18 +  social2.zero2$P19 +  social2.zero2$P20 +  social2.zero2$P21 +  social2.zero2$P22 +  social2.zero2$P25 +  social2.zero2$P26 +  social2.zero2$P29 +  social2.zero2$P30 +  social2.zero2$P31 +  social2.zero2$P32 +  social2.zero2$P33 +  social2.zero2$P35 +  social2.zero2$P36 +  social2.zero2$P37 +  social2.zero2$P38 +  social2.zero2$P39 +  social2.zero2$P40 +  social2.zero2$P41 +  social2.zero2$P42 +  social2.zero2$P44)
vcov5 <- vcovHC(ajuste.5, type=c('HC1'))
coeftest(ajuste.5, vcov=vcov4)


## Ajustes
#### Todas as variaveis
ajuste.1 <- lm(Q13 ~ P1+P3+P7+P25+P32+P33, data=variaveis.1)
vcov1 <- vcovHC(ajuste.1, type=c('HC1'))
coeftest(ajuste.1, vcov=vcov1)
summary(ajuste.1)

## Somente renda
ajuste.2 <- lm(Q13 ~ P33, data=variaveis.1)
vcov2 <- vcovHC(ajuste.2, type=c('HC1'))
coeftest(ajuste.2, vcov=vcov2)


ajuste.3 <- lm(Q13 ~ P1+P33, data=variaveis.1)
vcov3 <- vcovHC(ajuste.3, type=c('HC1'))
coeftest(ajuste.3, vcov=vcov3)


### as interações que propomos foram estatísticamente significativos.
ajuste.4 <- lm(Q13 ~ P1 + P33 + P1*P33, data=variaveis.1)
vcov4 <- vcovHC(ajuste.4, type=c('HC1'))
coeftest(ajuste.4, vcov=vcov4)


ajuste.5 <- lm(Q13 ~ P1 + P33 + P32, data=variaveis.1) 
vcov5 <- vcovHC(ajuste.5, type=c('HC1'))
coeftest(ajuste.5, vcov=vcov5)

AIC_BIC <- data.frame(AIC=rep(0,5), BIC=rep(0,5))
AIC_BIC$AIC <- c(AIC(ajuste.1), AIC(ajuste.2), AIC(ajuste.3), AIC(ajuste.4), AIC(ajuste.5))
AIC_BIC$BIC <- c(BIC(ajuste.1), BIC(ajuste.2), BIC(ajuste.3), BIC(ajuste.4), BIC(ajuste.5))
rownames(AIC_BIC) <- c('Ajuste1', 'Ajuste2', 'Ajuste3', 'Ajuste4', 'Ajuste5')

plot(ajuste.1$fitted.values,ajuste.1$residuals)
plot(ajuste.2$fitted.values,ajuste.2$residuals)
plot(ajuste.3$fitted.values,ajuste.3$residuals)
plot(ajuste.4$fitted.values,ajuste.4$residuals)
plot(ajuste.5$fitted.values,ajuste.5$residuals)

par(mfrow  = c(2,3))
qqnorm(ajuste.1$residuals, main='Ajuste 1')
qqline(ajuste.1$residuals,col=2)

qqnorm(ajuste.2$residuals, main='Ajuste 2')
qqline(ajuste.2$residuals,col=2)

qqnorm(ajuste.3$residuals, main='Ajuste 3')
qqline(ajuste.3$residuals,col=2)

qqnorm(ajuste.4$residuals, main='Ajuste 4')
qqline(ajuste.4$residuals,col=2)

qqnorm(ajuste.5$residuals, main='Ajuste 5')
qqline(ajuste.5$residuals,col=2)

acf(ajuste.1$residuals)
acf(ajuste.2$residuals)
acf(ajuste.3$residuals)

# Se adicionarmos a variável ENEM conseguimos aumentar o R2 ajustado de 0.22 para 0.38. O que faz sentido pois ENEM também é uma prova para avaliar a capacidade do aluno. Entretanto não iremos utilizar esta variável neste estudo pois o intúito é avaliar os efeitos das características sócio-econômicas na nota do vestibular
## Best subsets
library(leaps)

## Selecionando somente as variáveis mais escolhidas anteriormente
unicamp <- subset(variaveis.1, select = c('P1', 'P3', 'P7', 'P25', 'P32', 'P33', 'Q13'))
rownames(unicamp) <- NULL
unicamp <- sapply(unicamp, function(x) {
  
  ifelse(x==0, NA,x)
})

## Seleciona somente casos completos
unicamp <- data.frame(unicamp[complete.cases(unicamp),])
unicamp[, c('P1', 'P3', 'P7', 'P25', 'P32', 'P33')] <- convert.magic(unicamp[, c('P1', 'P3', 'P7', 'P25', 'P32', 'P33')], "factor")

## Criando melhor subconjunto com os dados fornecidos
regfit.full <- regsubsets(Q13 ~ ., data = unicamp, nvmax = 30)
reg.summary <- summary(regfit.full)
names(reg.summary)

## Gráficos com sumários
plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp")
which.min(reg.summary$cp)
points(10, reg.summary$cp[10], pch = 20, col = "red")
plot(regfit.full, scale = "Cp")
coef(regfit.full, 6)
