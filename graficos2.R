#---------------------------------------------------------------#
# Adaptado dos programas construídos pelo Prof. Gilberto Paula
# disponíveis nos site :http://www.ime.usp.br/~giapaula/textoregressao.htm
#---------------------------------------------------------------#
#

graficos2 <- function(fit.model) {
  
  par(mfrow=c(1,2))
  boxplot(tsi,ylab="Residuo studentizado")
  tsi <- studres(fit.model)
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
  
  
}
