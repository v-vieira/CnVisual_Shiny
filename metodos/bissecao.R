bissection <- function(env_funcao,env_pontos_1,env_pontos_2,env_decimais,env_iteracoes,g_indices,g_lh){
  ### vetor de erro
  error_vector <<-c()
  
  ### Valores de entrada
  f<-env_funcao
  
  s<-round(abs(env_decimais))
  if(s==0||s>5){s<-5}
  stp <- 10^(-s)
  
  stpint <- round(abs(env_iteracoes))
  if(stpint==0 || stpint>15){stpint <- 15}
  
  # Intervalo
  input_points <- c(env_pontos_1,env_pontos_2)
  a0 <- min(env_pontos_1)
  b0 <- max(env_pontos_2)
  
  func <- paste("func <- function(x){",f,"}") # Criando string de entrada
  eval(parse(text=func)) # Transformando o texto salvo na variavel ftext em uma expressao
  
  fa <- func(a0)
  fb <- func(b0)

  ### Garantindo que os pre-requisitos estao sendo seguidos exibindo janela de erro
  ### Erro caso o extremo inferior ja satisfaca o criterio de parada
  if(abs(fa)<stp){
    error_vector <<- c(error_vector,'O ponto \'a0\' já satisfaz o critério de parada')
  }
  
  ### Erro caso o extremo superior ja satisfaca o criterio de parada
  if(abs(fb)<stp){ 
    error_vector <<- c(error_vector,'O ponto \'b0\' já satisfaz o critério de parada')
  }
  
  ### Garantir que o metodo só seja feito caso tenha um numero ímpar de raizes no intervalo dado
  if((fa*fb)>0){
    error_vector <<- c(error_vector,'f(a0)*f(b0) > 0; Portanto, não é possível garantir que há uma raiz neste intervalo')
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
      m_k[cont] <- (a_k[cont]+b_k[cont])/2
      # Definir qual sera o proximo a e b
      if(fa_k[cont]*func(m_k[cont])<0){
        b0 <- m_k[cont]
      }
      else {
        a0 <- m_k[cont]
      }
      # Parar o metodo pelo while
      if((cont)>=stpint){whilevar <-1}
      if(abs(b0-a0)<stp){whilevar <- 1}
      if(abs(func(m_k[cont]))<stp){whilevar <- 1}
      if(whilevar== -1) cont<- cont + 1 # Aumentar o indice do contador
    }
    
    ### PLOT
    #Pegar os valores maximos e minimos da funcao e forcar um valor minimo e maximo para o plot
    y_min <- 0
    y_max <- 0
    
    if(abs(b_k[1]-a_k[1])<=1000){by_for = 0.05}
    else{by_for = 0.001}
    
    for (x in seq(a_k[1],b_k[1],by=by_for)){
      if (func(x) < y_min){
        y_min <- func(x)
      }
      if(func(x) > y_max){
        y_max <- func(x)
      }
    }
    
    absalt <- abs(y_max-y_min) #Altura total do plot
    
    if(abs(y_min) < absalt*0.05){
      y_min <- -absalt*0.1
    }
    if(abs(y_max) < absalt*0.05){
      y_max <- absalt*0.1
    }
    
    absalt <- abs(y_max-y_min)
    h_x <- abs(b_k[1]-a_k[1])*0.05
    h_ind <- absalt*0.04
    
    p <- ggplot(data = data.frame(x = 0,y=0), mapping = aes(x = x,y=y)) + geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + xlim(a_k[1]-h_x,b_k[1]+h_x) + ylim(y_min,y_max)
    p <- p + stat_function(fun = func, col = "red")
    # Salvar o plot na lista
    plot_vector <<- list(p)
    
    ### Plot dos pontos a e b sobre o eixo x
    plot_vector[[2]] <<- p + geom_point(x=a_k[1],y=0, col="blue", pch = 1) + geom_point(x=b_k[1],y= 0, col="blue", pch = 1)+annotate("text",label="a0",x=a_k[1],y=h_ind)+annotate("text",label="b0",x=b_k[1],y=h_ind)
    
    ### Salvar o plot de cada iteração
    for (i in 1:cont){
      # Linha horizontal
      if(g_lh){
        inter_x <-c(a_k[i],b_k[i])
        plot_vector[[length(plot_vector)+1]] <<- plot_vector[[length(plot_vector)]] + geom_segment(x=inter_x[1], xend=inter_x[2], y=y_min, yend=y_min,size=i + (i-1)*0.3,col="green4") +geom_point(x=inter_x[1],y=y_min,shape=21,bg="green4")+geom_point(x=inter_x[2],y=y_min,shape=21,bg="green4")
      }
      # Indices dos pontos
      if(g_indices){
        plot_vector[[length(plot_vector)+1]] <<- plot_vector[[length(plot_vector)]] + geom_point(x=m_k[i],y=0, col="blue", shape = 1)+annotate("text",label=toString(i-1),x=m_k[i],y=h_ind)
      }
      else{
        # Plot dos pontos m_k sobre o eixo x
        plot_vector[[length(plot_vector)+1]] <<- plot_vector[[length(plot_vector)]] + geom_point(x=m_k[i],y=0, col="blue", shape = 1)
      }
    }
    value_output <<- list()
    value_output[[1]] <<- paste("Aproximações: ",paste0(m_k, collapse =" | "))
  }
}
