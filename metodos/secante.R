secante<- function(env_funcao,env_pontos,env_iteracoes,env_decimais,env_indices,env_linvt,env_linsec){
  ### vetor de erro
  error_vector <<-c()
  
  ### Valores de entrada
  f<-env_funcao
  initical_point <-env_pontos
  
  stpcont <-as.numeric(env_iteracoes)
  if(is.na(stpcont)){stpcont<-9999}
  
  s<-as.numeric(env_decimais)
  stp <- 10^(-s)
  
  valxaux <- as.list(strsplit(initical_point," ")[[1]])
  contval <- length(valxaux) 
  x0 <-as.numeric(valxaux[1])
  x1 <- as.numeric(valxaux[2])
  
  func <- paste("func <- function(x){",f,"}") # Criando string de entrada
  eval(parse(text=func))# Transformando o texto salvo na variavel ftext em uma expressao
  
  f_x0 <- func(x0)
  f_x1 <- func(x1)
  
  # Criando vetores para o plot
  x_k <- c()
  fx_k <- c()
  
  #Preenchendo os vetores
  x_k[1] <- x0
  x_k[2] <- x1
  fx_k[1] <- f_x0
  fx_k[2] <- f_x1
  
  # contador de indices para while
  cont <- 2
  
  whileaux <- -1 #teste para fazer ate chegar ao erro desejado
  
  ### Erro caso o ponto 'a' dado ja satisfaz o criterio de parada
  if(abs(f_x0)<stp){ 
    error_vector <<- c(error_vector,'O ponto \'a\' já satisfaz o critério de parada')
  }
  ### Erro caso o ponto 'b' dado ja satisfaz o criterio de parada
  if(abs(f_x1)<stp){ 
    error_vector <<- c(error_vector,'O ponto \'b\' já satisfaz o critério de parada')
  }
  ### Erro para garantir que os extremos tem sinais diferentes
  if((f_x0*f_x1)>0){ 
    error_vector <<- c(error_vector,'f(a)*f(b) > 0; Portanto, não é possível garantir que há uma raiz neste intervalo')
  }
  
  #===Comeco do metodo em si
  #
  else if(is.null(error_vector)){
    while(whileaux == -1){
      cont <- cont + 1
      x_k[cont] <- (x_k[cont-1] - ((fx_k[cont-1])*((x_k[cont-1] - x_k[cont-2])/(fx_k[cont-1] - fx_k[cont-2]))))
      fx_k[cont] <- func(x_k[cont])
      
      Errosec <-((x_k[cont]-x_k[cont-1])/(x_k[cont]))
      if(abs(Errosec)<stp){whileaux <- 1}
      if(cont>stpcont){whileaux <- 1}
      if(abs(fx_k[cont])<stp){whileaux <- 1}
    }
    
    p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + xlim(x0-1,x1+1) + xlab("Eixo x") + ylab("Eixo y")
    p <- p + stat_function(fun = func, col = "red")
    
    plot_vector <<- list(p)
    
    if(env_indices){
      p <- p + annotate("text",label="0",x=x_k[1],y=3,col="blue")
    }
    p <- p + geom_point(x=x_k[1],y= 0, col="blue", pch = 1)
    plot_vector[[2]] <<- p
    
    if(env_linvt){
      p <- p + geom_segment(x=x_k[1],xend=x_k[1],y=0,yend=fx_k[1],col= "azure4",linetype="dashed")
    }
    plot_vector[[3]] <<- p + geom_point(x=x_k[1],y=fx_k[1],col="green",pch=1)
    
    for (i in 2:(cont)){
      k <- 4 + (i-2)*5
      plot_vector[[k]] <<- plot_vector[[k-1]] + geom_point(x=x_k[i],y=0, col="blue", pch = 1)
      
      if(env_linvt){
        plot_vector[[k+1]] <<- plot_vector[[k]] + geom_segment(x=x_k[i],xend=x_k[i],y=0,yend=fx_k[i], col= "azure4", lty=2)
      }
      
      plot_vector[[k+2]] <<- plot_vector[[k+1]] + geom_point(x=x_k[i],y=fx_k[i], col="green", pch=1)
      
      if(env_indices){
        plot_vector[[k+3]] <<- plot_vector[[k+2]] + annotate("text",label=i,x=x_k[i],y=3,col="blue")
      }
      
      if(env_linsec){
        if((fx_k[i-1]*fx_k[i])<0)
          p <- geom_segment(x=x_k[i-1],xend=x_k[i],y=fx_k[i-1],yend=fx_k[i],col = "yellow")
        else
          p <- geom_segment(x=x_k[i-1],xend=x_k[i+1],y=fx_k[i-1],yend=0,col="yellow")
        plot_vector[[k+4]] <<- plot_vector[[k+3]] + p
      }
      value_output <<- list()
      value_output[[1]] <<-paste("Aproximações: ",paste0(x_k, collapse =" | "))
    }
  }
}