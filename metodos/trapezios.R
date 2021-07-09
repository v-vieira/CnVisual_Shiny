trapezios<- function(env_funcao,env_interv_integra,env_divisoes,env_pintar,env_linvt,env_indices){
  ### vetor de erro
  error_vector <<-c()
  
  ### Valores de entrada
  f<-env_funcao
  interentr<-env_interv_integra
  div<- abs(as.numeric(env_divisoes)) # Quantidade de divisões
  
  interaux <- as.list(strsplit(interentr," ")[[1]])
  limitx <- c()
  for(i in 1:2){
    limitx[i] <- as.numeric(interaux[i])
  }
  limitx <- sort(limitx)
  
  ### Erro caso a quantidade de divisões seja 0
  if(div==0){
    error_vector <<- c(error_vector,'Numero de divisões deve ser maior ou igual a 1')
  }
  
  else if(is.null(error_vector)){
    func <- paste("func <- function(x){",f,"}")
    func_replot <- paste("func_replot <- function(x){",f,"+0.1-0.1}")
    func2 <- paste("func2 <- expression(",f,")")
    
    eval(parse(text=func))
    eval(parse(text=func2))
    eval(parse(text=func_replot))
    
    DevFunc2 <- function(x){eval(D(D(func2,"x"),"x"))}
    DevFunc3 <- function(x){eval(D(D(D(func2,"x"),"x"),"x"))}
    
    trapezoid <- function(fun, a, b, n) {
      h <- (b-a)/n
      x <- seq(a, b, by=h)
      y <- func(x)
      s <- h * (y[1]/2 + sum(y[2:n]) + y[n+1]/2)
      return(s)
    }
    
    soma <- abs(trapezoid(func, limitx[1], limitx[2], div + 1))
    
    pointx <- c()
    pointy <- c()
    pointx[1] <- limitx[1]
    pointy[1] <- func(pointx[1])
    h <- ((limitx[2] - limitx[1])/div)
    for(i in 2:(div+1)){
      pointx[i] <- (pointx[i-1] + h)
      pointy[i] <- func(pointx[i])
    }
    
    xmin <- limitx[1]
    xmax <- limitx[2]
    
    j<- 1
    crit <- c()
    for(i in seq(from=xmin, to=xmax, by=0.01)){
      if(((DevFunc3(i))>-0.1)&&(DevFunc3(i)<0.1)){
        crit[j] <- DevFunc2(i)
        j <- j + 1
      }
    }
    
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
    #TODO: Verificar se esse erro está correto
    Errotrap <- valmax*(((xmax-xmin)*(h^2))/(12))
    
    z_k <- rep(0, (div+1))
    
    p1 <- ggplot(data = data.frame(x = 0,y=0), mapping = aes(x = x,y=y)) + geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + xlim(xmin,xmax) + xlab("Eixo x") + ylab("Eixo y")
    p <- p1 + stat_function(fun = func, col = "red")
    
    plot_vector <<- list(p)
    
    for(i in 1:length(pointx)){
      p <- p + geom_point(x=pointx[i],y= pointy[i], col="blue", pch = 1)
      p1 <- p1 + geom_point(x=pointx[i],y= pointy[i], col="blue", pch = 1)
    }
    
    if(env_indices){
      for(i in 1:length(pointx)){
        p <- p + annotate("text", label=toString(i),pointx[i],pointy[i], col="blue")
        p1 <- p1 + annotate("text", label=toString(i),pointx[i],pointy[i], col="blue")
      }
    }
    if(env_linvt){
      for(i in 1:length(pointx)){
        p <- p + geom_segment(x=pointx[i],xend=pointx[i],y=0,yend= pointy[i], col = "gray48",linetype='dashed')
        p1 <- p1 + geom_segment(x=pointx[i],xend=pointx[i],y=0,yend= pointy[i], col = "gray48",linetype='dashed')
      }
    }
    
    plot_vector[[2]] <<- p
    
    for (i in 1:(div+1)){
      if(i!=(div+1)){
        plot_vector[[i+2]] <<- plot_vector[[i+1]] + geom_segment(x=pointx[i],xend=pointx[i+1],y=pointy[i],yend=pointy[i+1])
        p <- plot_vector[[i+2]]
        p1 <- p1 + geom_segment(x=pointx[i],xend=pointx[i+1],y=pointy[i],yend=pointy[i+1])
      }
    }
    
    if(env_pintar){
      for(i in 1:div){
        shape <- data.frame(x=c(pointx[i],pointx[i],pointx[i+1],pointx[i+1]),y=c(0,pointy[i],pointy[i+1],0)) 
        p <- p + geom_polygon(data=shape,fill='skyblue',alpha=0.7)
      }
      
      for (i in 1:(div+1)){
        if(i!=(div+1)){ 
          p <- p + geom_segment(x=pointx[i],xend=pointx[i+1],y=pointy[i],yend=pointy[i+1])
        }
      }
      
      if(env_linvt){
        for(i in 1:length(pointx)){
          p <- p + geom_segment(x=pointx[i],xend=pointx[i],y=0,yend=pointy[i], col = "gray48",linetype='dashed')
        }
      }
    }
    #TODO: Replotar a curva por cima
    plot_vector[[div+3]] <<- p + stat_function(fun = func_replot, col = "red")
    
    value_output <<- list()
    value_output[[1]] <<-  paste0("Valor da integração pelo metodo: ",soma," | O erro do metodo: ",Errotrap)
  }
}