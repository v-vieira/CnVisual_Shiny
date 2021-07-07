library(ggplot2)
library(plotly)
falsa <- function(env_funcao,env_pontos,env_decimais,env_iteracoes,env_indices,env_linsc){
  ### vetor de erro
  error_vector <<-c()
  
  ### Valores de entrada
  f<-env_funcao
  pointsentr<-env_pontos
  s<-as.numeric(env_decimais)
  stp <- 10^(-s)
  stpint <- as.numeric(env_iteracoes)
  if(is.na(stpint)) stpint <- 999
  
  ### pegar os valores separados em x
  valxaux <- as.list(strsplit(pointsentr," ")[[1]])
  contval <- length(valxaux)
  a0 <-as.numeric(valxaux[1])
  b0 <- as.numeric(valxaux[2])
  
  func <- paste("func <- function(x){",f,"}")
  eval(parse(text=func))
  
  fa <- func(a0)
  fb <- func(b0)
  
  # Vetores para o plot
  a_k <- c()
  b_k <- c()
  fa_k <- c()
  fb_k <- c()
  m_k <- c()
  
  # Contador de indices para while
  cont <- 1
  
  ### Garantindo que os pre-requisitos estao sendo seguidos exibindo janela de erro
  # Erro caso o extremo inferior ja satisfaca o criterio de parada
  if(abs(fa)<stp){
    error_vector <<- c(error_vector,'O ponto \'a\' já satisfaz o critério de parada')
  }
  
  ### Erro caso o extremo superior ja satisfaca o criterio de parada
  if(abs(fb)<stp){ 
    error_vector <<- c(error_vector,'O ponto \'b\' já satisfaz o critério de parada')
  }
  
  ### Garantir que o metodo só seja feito caso tenha um numero ímpar de raizes no intervalo dado
  if((fa*fb)>0){
    error_vector <<- c(error_vector,'f(a)*f(b) > 0; Portanto, não é possível garantir que há uma raiz neste intervalo')
  }
  
  else if(is.null(error_vector)){
    whilevar <- -1
    while(whilevar == -1){
      ### Atribuicao dos valores aos vetores
      a_k[cont] <- a0
      b_k[cont] <- b0
      fa_k[cont] <- func(a0)
      fb_k[cont] <- func(b0)
      m_k[cont] <- (a_k[cont]*fb_k[cont] - b_k[cont]*fa_k[cont])/(fb_k[cont] - fa_k[cont])
      
      # Definir qual sera o proximo a e b
      if(fa_k[cont]*func(m_k[cont])<0){
        b0 <- m_k[cont]
      }
      else {
        a0 <- m_k[cont]
      }
      
      if((cont>=2)&&(abs(b_k[cont]-a_k[cont])<stp)){whilevar <- 1}
      if(abs(func(m_k[cont]))<stp){whilevar<-1}
      if((cont)>=stpint){whilevar <- 1}
      cont<- cont + 1
    }
    cont <- cont -1
    
    y_min <- optimize(func,interval = c(a_k[1],b_k[1]))
    y_min <- y_min$objective
    y_max <- optimize(func,interval = c(a_k[1],b_k[1]),maximum = TRUE)
    y_max <- y_max$objective
    absalt <- abs(y_max-y_min)
    absalt <- abs(y_max-y_min)
    
    if(abs(y_min) <= 0.1*(absalt)) y_min<- -0.1*(absalt)
    if(abs(y_max) <= 0.1*(absalt)) y_max<- 0.1*(absalt)
    
    
    p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + xlim(a_k[1] - 1, b_k[1] + 1) + ylim(y_min,y_max) + xlab("Eixo x") + ylab("Eixo y")
    p <- p + stat_function(fun = func, col = "red")
    plot_vector <<- list(p)
    
    plot_vector[[2]] <<- plot_vector[[1]] + geom_point(x=a_k[1],y=0, col="blue", pch = 1) + geom_point(x=b_k[1],y= 0, col="blue", pch = 1)+annotate("text",label="a",x=a_k[1],y=3)+annotate("text",label="b",x=b_k[1],y=3)
    
    # Animacao
    #TODO: Implementar k opções gráficas
    for (i in 1:cont){
      k <- 3 + 3*(i-1) 
      
      p <- plot_vector[[k-1]] + geom_point(x = m_k[i], y = 0, col="blue", pch = 1)
      
      if(env_indices){
        index <-c(0:(i-1))
        p <- p + annotate("text", label = toString(i),x= m_k[i],y=3 )
      }
      plot_vector[[k]] <<- p
      
      plot_vector[[k+1]] <<- plot_vector[[k]] + geom_segment(x=a_k[i],xend=a_k[i],y=0,yend=fa_k[i],col= "azure4",linetype="dashed")
      
      if(env_linsc){
        plot_vector[[k+2]] <<- plot_vector[[k+1]] + geom_segment(x=a_k[i],xend=b_k[i],y=fa_k[i],yend=fb_k[i], col="yellow") #lwd = 1.2
      }
    }
    value_output <<- list()
    value_output[[1]] <<- paste("Aproximações: ",paste0(m_k, collapse =" | "))
  }
}