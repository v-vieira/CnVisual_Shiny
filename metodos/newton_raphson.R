newtonraphson<- function(env_funcao,env_x0,env_intervalo,env_decimais,env_iteracoes,g_indices,g_lv,g_ltg){
  ### vetor de erro
  error_vector <<-c()
  
  #== Valores de entrada
  f<-env_funcao 
  x0<-as.numeric(env_x0)
  pointsentr<-env_intervalo  
  
  s<-as.numeric(env_decimais)
  stp <- 10^(-s)
  stpint <- as.numeric(env_iteracoes)
  if(is.na(stpint)) stpint <- 999 #numero ilimitado de itera??es
  
  
  #=== pegar os valores separados em x
  #
  valxaux <- as.list(strsplit(pointsentr," ")[[1]])
  contval <- length(valxaux) # contador da quantidade de valores de entrada
  #intervalo
  a0 <-as.numeric(valxaux[1])
  b0 <- as.numeric(valxaux[2])
  
  
  # Criando strings de entrada
  func <- paste("func <- function(x){",f,"}")
  func2 <- paste("func2 <- expression(",f,")")
  
  # Transformando os textos salvos na variavel ftext em uma expressao
  eval(parse(text=func))
  eval(parse(text=func2))
  
  ##=== Criando a funcao derivada primeira da funcao de entrada
  DevFunc <- function(x){eval(D(func2,"x"))}
  f_x0 <- func(x0)     # valor da F(x0)
  Dev_x0 <-DevFunc(x0)   #valor da F'(x0)
  
  ### Erro caso a derivada seja igual a zero
  #TODO: Avaliar se não tem que ser tendendo a zero.
  if(Dev_x0 == 0){
    error_vector <<- c(error_vector,'A derivada do ponto \'x0\' é igual a 0 ')
  }
  else{
    # Criando vetores para o plot
    x_k <- c()
    fx_k <- c()
    
    # Preenchendo os vetores
    x_k[1] <- x0
    x_k[2] <- x0 - f_x0/Dev_x0
    fx_k[1] <- f_x0
    fx_k[2] <- func(x_k[2])
  }
  
  # contador de indices para while
  cont <- 2
  
  # Erro caso o ponto dado ja satisfaz o criterio de parada
  if(abs(func(x0))<stp){
    error_vector <<- c(error_vector,'O ponto \'x0\' já satisfaz o critério de parada')
  }
  else if(is.null(error_vector)){
    whilevar <- -1
    while(whilevar == -1){
      cont <- cont + 1
      #TODO: implementar fora de garantir que não faça caso DevFunc(x_k[cont-1] seja igual a zero (ou tenda -> validar)
      x_k[cont] <- (x_k[cont-1] - ((func(x_k[cont-1]))/(DevFunc(x_k[cont-1]))))
      fx_k[cont] <- func(x_k[cont])
      
      if(cont>=stpint){whilevar <-1}
      if(abs(x_k[cont]-x_k[cont-1])<stp){whilevar <- 1}
      if(abs(fx_k[cont])<stp){whilevar <- 1}
    }
    p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + xlim(a0,b0)
    p <- p + stat_function(fun=func, col = "red")+ geom_vline(xintercept = 0) + geom_hline(yintercept = 0)
    plot_vector <<- list(p)
    
    #TODO: Implementar k opções gráficas
    for (i in 1:cont){
      k <- 2+ 4*(i-1)
      index <-c(0:(cont-1))
      
      p <- plot_vector[[k-1]] + geom_point(x=x_k[i],y=0,col="blue",shape=1)
      
      if(g_indices){
        p <- p + annotate("text",label=toString(i),x=x_k[i],y=3,col="blue")
      }
      
      plot_vector[[k]] <<- p
      
      if(g_lv){
        plot_vector[[k+1]] <<- plot_vector[[k]] + geom_segment(x=x_k[i],xend=x_k[i],y=0,yend=fx_k[i],col="darkgray",linetype="dashed")
      }
      
      plot_vector[[k+2]]<<- plot_vector[[k+1]] + geom_point(x=x_k[i],y=fx_k[i], col="green", shape=1)
      
      if(g_ltg){
        plot_vector[[k+3]] <<- plot_vector[[k+2]] + geom_segment( x= x_k[i],xend=x_k[i+1],y=fx_k[i],yend =0,col = "black")
      }
    }
    value_output <<- list()
    value_output[[1]] <<-  paste("Aproximacoes: ",paste0(x_k, collapse =" | "))
  }
}