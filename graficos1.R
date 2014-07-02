#---------------------------------------------------------------#
# Adaptado dos programas construídos pelo Prof. Gilberto Paula
# disponíveis nos site :http://www.ime.usp.br/~giapaula/textoregressao.htm
#---------------------------------------------------------------#
#
# Obtém resíduos studentizado do modelo

graficos <- function(fit.model) {
  
  
  tsi <- studres(fit.model)
  a <- max(tsi)
  b <- min(tsi)
  
  
  ## Faz o plot do resíduo studentizado pelo índice
  df <- data.frame(y = tsi,x=1:length(tsi))
  g1 <- ggplot(df,aes(x=x,y=y)) + 
    geom_point(alpha = 0.3) +
    xlab("Indice") +
    ylab("Resíduo Studentizado") +
    ylim(b-1,a+1) +
    geom_abline(intercept=2, slope=0, size=1, col='red') +
    geom_abline(intercept=-2, slope=0, size=1, col='red')
  
  ## Faz o plot do resíduo studentizado pelo valor ajustado
  df <- data.frame(x = fitted(fit.model),y=tsi)
  g2 <- ggplot(df,aes(x=x,y=y)) + 
    geom_point(alpha = 0.3) +
    xlab("Valores Ajustados") +
    ylab("Resíduo Studentizado") +
    ylim(c(b-1,a+1)) +
    geom_abline(intercept=2, slope=0, size=1, col='red') +
    geom_abline(intercept=-2, slope=0, size=1, col='red')
 
  ## Boxplot do resíduo studentizado
  #g3 <- ggplot(df, aes(y=y,x=x)) + geom_boxplot()
  
  grid.arrange(g1, g2)
  
}
  
  
