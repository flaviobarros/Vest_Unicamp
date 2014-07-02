#---------------------------------------------------------------#
# Adaptado dos programas construídos pelo Prof. Gilberto Paula
# disponíveis nos site :http://www.ime.usp.br/~giapaula/textoregressao.htm
#---------------------------------------------------------------#
#
X <- model.matrix(~P1+P3+P7+P25+P32+P33, data=uni)
QRx = qr(X) #obtain QRD
Q = qr.Q(QRx) #extract Q matrix
H <- Q %*% t(Q) #hat matrix 
h <- diag(H)
lms <- summary(fit.model)
s <- lms$sigma
r <- resid(lms)
ts <- r/(s*sqrt(1-h))
di <- (1/p)*(h/(1-h))*(ts^2)
si <- lm.influence(fit.model)$sigma
tsi <- r/(si*sqrt(1-h))
a <- max(tsi)
b <- min(tsi)
par(mfrow=c(2,2))
plot(tsi,xlab="Indice", ylab="Resíduo Studentizado",
     ylim=c(b-1,a+1), pch=16)
abline(2,0,lty=2)
abline(-2,0,lty=2)
abline(0,0,lty=2)
#identify(tsi, n=1)
#title(sub="(c)")
#
plot(fitted(fit.model),tsi,xlab="Valores Ajustados", 
     ylab="Residuo Studentizado", ylim=c(b-1,a+1), pch=16)
abline(2,0,lty=2)
abline(-2,0,lty=2)
abline(0,0,lty=2)
#title(sub="(d)")
#identify(fitted(fit.model),tsi, n=1)
boxplot(tsi,ylab="Residuo studentizado")
X <- model.matrix(~P1+P3+P7+P25+P32+P33, data=uni)
QRx = qr(X) #obtain QRD
Q = qr.Q(QRx) #extract Q matrix
H <- Q %*% t(Q) #hat matrix 
h <- diag(H)
si <- lm.influence(fit.model)$sigma
r <- resid(fit.model)
tsi <- r/(si*sqrt(1-h))
#
ident <- diag(n)
epsilon <- matrix(0,n,100)
e <- matrix(0,n,100)
e1 <- numeric(n)
e2 <- numeric(n)
#
for(i in 1:100){
  epsilon[,i] <- rnorm(n,0,1)
  e[,i] <- (ident - H)%*%epsilon[,i]
  u <- diag(ident - H)
  e[,i] <- e[,i]/sqrt(u)
  e[,i] <- sort(e[,i]) }
#
for(i in 1:n){
  eo <- sort(e[i,])
  e1[i] <- (eo[2]+eo[3])/2
  e2[i] <- (eo[97]+eo[98])/2 }
#
med <- apply(e,1,mean)
faixa <- range(tsi,e1,e2)
#
#par(pty="s")
qqnorm(tsi,xlab="Percentis da N(0,1)",
       ylab="Residuo Studentizado", ylim=faixa, pch=16,main="")
par(new=T)
qqnorm(e1,axes=F,xlab="",ylab="",type="l",ylim=faixa,lty=1,main="")
par(new=T)
qqnorm(e2,axes=F,xlab="",ylab="", type="l",ylim=faixa,lty=1,main="")
par(new=T)
qqnorm(med,axes=F,xlab="",ylab="",type="l",ylim=faixa,lty=2,main="")
#---------------------------------------------------------------#
