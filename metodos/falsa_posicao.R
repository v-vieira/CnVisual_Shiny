falsa <- function(env_funcao,env_pontos_1,env_pontos_2,env_decimais,env_iteracoes,g_indices,g_sc){
  ### vetor de erro
  error_vector <<-c()
  
  ### Valores de entrada
  f<-env_funcao
  
  s<-round(abs(as.numeric(env_decimais)))
  if(s==0||s>5){s<-5}
  stp <- 10^(-s)
  
  stpint <- round(abs(as.numeric(env_iteracoes)))
  if(stpint==0 || stpint>15){stpint <- 15}
  
  ### pegar os valores separados em x
  input_points <- c(env_pontos_1,env_pontos_2)
  a0 <- min(env_pontos_1)
  b0 <- max(env_pontos_2)
  
  func <- paste("func <- function(x){",f,"}")
  eval(parse(text=func))
  
  fa <- func(a0)
  fb <- func(b0)
  
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
    # Vetores para o plot
    a_k <- c()
    b_k <- c()
    fa_k <- c()
    fb_k <- c()
    m_k <- c()
    # Contador de indices para while
    cont <- 1
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
    
    ### PLOT
    h_x <- abs(b_k[1]-a_k[1])*0.05
    
    p <- ggplot(data = data.frame(x = 0,y = 0), mapping = aes(x = x,y = y)) + geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + xlim(a_k[1]-h_x, b_k[1]+h_x) + xlab("Eixo x") + ylab("Eixo y")
    p <- p + stat_function(fun = func, col = "red")
    plot_vector <<- list(p)
    
    plot_vector[[2]] <<- plot_vector[[1]] + geom_point(x=a_k[1],y=0, col="blue", pch = 1) + geom_point(x=b_k[1],y= 0, col="blue", pch = 1)+annotate("text",label="a0",x=a_k[1],y=0)+annotate("text",label="b0",x=b_k[1],y=0)
    
    # Animacao
    for (i in 1:cont){
      
      plot_vector[[length(plot_vector)+1]] <<- plot_vector[[length(plot_vector)]] + geom_segment(x=a_k[i],xend=a_k[i],y=0,yend=fa_k[i],col= "azure4",linetype="dashed")+geom_segment(x=b_k[i],xend=b_k[i],y=0,yend=fb_k[i],col= "azure4",linetype="dashed")
      
      plot_vector[[length(plot_vector)+1]] <<- plot_vector[[length(plot_vector)]] + geom_point(x=a_k[i],y=fa_k[i],col='grey')+geom_point(x=b_k[i],y=fb_k[i],col='grey')
      
      if(g_sc){
        plot_vector[[length(plot_vector)+1]] <<- plot_vector[[length(plot_vector)]] + geom_segment(x=a_k[i],xend=b_k[i],y=fa_k[i],yend=fb_k[i], col="yellow")
      }
      
      p <- plot_vector[[length(plot_vector)]] + geom_point(x = m_k[i], y = 0, col="blue", pch = 1)
      
      if(g_indices){
        p <- p + annotate("text", label = toString(i-1),x= m_k[i],y=0)
      }
      
      plot_vector[[length(plot_vector)+1]] <<- p
      
    }
    value_output <<- list()
    value_output[[1]] <<- paste("Aproximações: ",paste0(m_k, collapse =" | "))
  }
}