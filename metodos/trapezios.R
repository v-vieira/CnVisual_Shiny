trapezios<- function(env_funcao,env_interv_integra,env_divisoes,env_pintar,env_linvt,env_indices){
  #== Valores de entrada
  f<-env_funcao
  interentr<-env_interv_integra
  div<-as.numeric(env_divisoes) # Quantidade de divisões
  
  #============== pegar os valores separados em x =======
  interaux <- as.list(strsplit(interentr," ")[[1]])
  limitx <- c()
  for(i in 1:2){
    limitx[i] <- as.numeric(interaux[i])
  }
  
  #criando uma nova string: func <- function(x){ string da entrada}
  func <- paste("func <- function(x){",f,"}")
  func2 <- paste("func2 <- expression(",f,")")
  
  # transformando o texto salvo na variavel ftext em uma expressao
  eval(parse(text=func))
  eval(parse(text=func2))
  
  ##== Funcoes derivadas
  DevFunc2 <- function(x){eval(D(D(func2,"x"),"x"))}
  DevFunc3 <- function(x){eval(D(D(D(func2,"x"),"x"),"x"))}
  
  #=== funcao com o calculo da soma do metodo
  trapezoid <- function(fun, a, b, n) {
    h <- (b-a)/n
    x <- seq(a, b, by=h)
    y <- func(x)
    s <- h * (y[1]/2 + sum(y[2:n]) + y[n+1]/2)
    return(s)
  }
  
  ##= Variavel com a soma do metodo
  soma <- abs(trapezoid(func, limitx[1], limitx[2], div + 1))
  
  ##= Vetores com os valores de x
  pointx <- c()
  pointy <- c()
  pointx[1] <- limitx[1]
  pointy[1] <- func(pointx[1])
  h <- ((limitx[2] - limitx[1])/div)
  for(i in 2:(div+1)){
    pointx[i] <- (pointx[i-1] + h)
    pointy[i] <- func(pointx[i])
  }
  
  ##= limites do plot
  xmin <- limitx[1]
  xmax <- limitx[2]
  
  ##=== Calculo do erro
  #
  
  #= Achar os pontos criticos
  j<- 1
  crit <- c()
  for(i in seq(from=xmin, to=xmax, by=0.01)){
    if(((DevFunc3(i))>-0.1)&&(DevFunc3(i)<0.1)){
      crit[j] <- DevFunc2(i)
      j <- j + 1
    }
  }
  
  #= Calcular a derivada segunda dos extremos e seu maior valor de todas as derivadas segundas
  #
  Dev2min <- DevFunc2(xmin)
  Dev2max <- DevFunc2(xmax)
  
  if(Dev2min>Dev2max){
    valmax <- Dev2min
  }
  else {
    valmax <-Dev2max
  }
  if(j>1){
    for(i in 1:(j-1)){
      if(valmax < crit[i]){
        valmax <- crit[i]
      }
    }
  }
  
  ##= Calculo do erro
  Errotrap <- valmax*(((xmax-xmin)*(h^2))/(12))
  
  #vetor de zeros para o plot
  z_k <- rep(0, (div+1))
  
  #==== Plot do metodo
  #
  # visible(gg) <- TRUE #a area grafica gg que passara a receber os plots
  p <- ggplot(data = data.frame(x = 0,y=0), mapping = aes(x = x,y=y)) + geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + xlim(xmin,xmax) + xlab("Eixo x") + ylab("Eixo y")
  p <- p + stat_function(fun = func, col = "red")
  #plot(func, xlim=c(xmin, xmax), col = "red", xlab="Eixo x", ylab="Eixo y")#Plot da f(x)
  #abline(h=0, lty=2)
  #abline(v=0, lty=2)
  plot_vector <<- list(p)
  #animacao
  #Sys.sleep(speed/4)
  
  for(i in 1:length(pointx)){
    p <- p + geom_point(x=pointx[i],y= pointy[i], col="blue", pch = 1) # Plot dos pontos x e f_x
  }
  
#  index <-c(0:(div)) #vetor com os indices para o plot
  
  
  if(env_indices){ #Caso seja marcado os indices dos pontos no checkbox
    for(i in 1:length(pointx)){
      p <- p + annotate("text", label=toString(i),pointx[i],pointy[i], col="blue")
    }
  }
  if(env_linvt){ #Caso seja marcado as retas verticais no checkbox
    for(i in 1:length(pointx)){
      p <- p + geom_segment(x=pointx[i],xend=pointx[i],y=0,yend= pointy[i], col = "gray48",linetype='dashed')
    }
  }
  plot_vector[[2]] <<- p
  print(pointx)
  print(pointy)
  for (i in 1:(div+1)) #fazer as retas dos trapezios
  {
    #Sys.sleep(speed/4)
    if(i!=(div+1)){  #If para que i chegue apenas ate div e nao quebre o codigo
      #TODO: ERRO AQUI
      plot_vector[[i+2]] <<- plot_vector[[i+1]] + geom_segment(x=pointx[i],xend=pointx[i+1],y=pointy[i],yend=pointy[i+1])
      p <- plot_vector[i+2]
      print("====")
      print(i)
    }
  }
  
  #========= Pintar a area ==============
  if(env_pintar){
    for(i in 1:div){
      # xini <- pointx[i] #=== x inicial ===
      # xfin <- pointx[i+1] #==== x final
      # cord.x <- c(xini,xini,xfin,xfin)
      # cord.y <- c(0,pointy[i],pointy[i+1],0)
      shape <- data.frame(x=c(pointx[i],pointx[i],pointx[i+1],pointx[i+1]),y=c(0,pointy[i],pointy[i+1],0)) 
      p <- p + geom_polygon(data=shape,fill='skyblue')
      # polygon(cord.x,cord.y, col="skyblue", border = "skyblue")
    }
    
    for (i in 1:(div+1))
    {
      if(i!=(div+1)){  #If para que i chegue apenas ate div e nao quebre o codigo
         p <- p + geom_segment(x=pointx[i],xend=pointx[i+1],y=pointy[i],yend=pointy[i+1])
      }
    }
    
    if(env_linvt){
      for(i in 1:length(pointx)){
        p <- p + geom_segment(x=pointx,xend=pointx,y=z_k,yend=pointy, col = "gray48",linetype='dashed')
      }
    }
  }
  plot_vector[[div+3]] <<- p
  #Resultados a serem mostrados ao usuario
  value_output <<- list()
  value_output[[1]] <<-  paste0("Valor da integração pelo metodo: ",soma," | O erro do metodo: ",Errotrap)
}