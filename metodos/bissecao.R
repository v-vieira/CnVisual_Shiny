bissection <- function(env_funcao,env_pontos,env_decimais,env_iteracoes,g_indices,g_lh){
  ### vetor de erro
  error_vector <<-c()
  
  ### Valores de entrada
  f<-env_funcao
  pointsentr<-env_pontos
  s<-as.numeric(env_decimais)
  stp <- 10^(-s)
  stpint <- as.numeric(env_iteracoes)
  if(stpint==0) stpint <- 999
  
  
  ### pegar os valores separados em x
  valxaux <- as.list(strsplit(pointsentr," ")[[1]])
  contval <- length(valxaux)
  # Intervalo
  a0 <-as.numeric(valxaux[1])
  b0 <- as.numeric(valxaux[2])
  
  
  func <- paste("func <- function(x){",f,"}") # Criando string de entrada
  eval(parse(text=func)) # Transformando o texto salvo na variavel ftext em uma expressao
  
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
  ### Erro caso o extremo inferior ja satisfaca o criterio de parada
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
    
    #= Colocar os valores de entrada de volta em a0 e b0
    a0 <- as.numeric(valxaux[1])
    b0 <- as.numeric(valxaux[2])
    
    ### PLOT
    #Pegar os valores maximos e minimos da funcao e forcar um valor minimo e maximo para o plot
    y_min <- optimize(func,interval = c(a_k[1]- 1,b_k[1]+ 1)) #y_min pega o valor que da o minimo em x e o valor em y
    y_min <- y_min$objective #y_min agora pega apenas o valor em y
    y_max <- optimize(func,interval = c(a_k[1]- 1,b_k[1]+ 1),maximum = TRUE)
    y_max <- y_max$objective
    absalt <- abs(y_max-y_min) #Altura total do plot
    
    # Definir uma altura minima nos extremos superiores e inferiores
    if(abs(y_min) <= 0.1*(absalt)) y_min<- -0.1*(absalt)
    if(abs(y_max) <= 0.1*(absalt)) y_max<- 0.1*(absalt)
    inter_y <- c(y_min,y_min)
    
    p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) 
    p <- p + stat_function(fun = func, col = "red") + geom_vline(xintercept = 0) + geom_hline(yintercept = 0)
    # Salvar o plot na lista
    plot_vector <<- list(p)
    
    ### Plot dos pontos a e b sobre o eixo x
    plot_vector[[2]] <<- p + geom_point(x=a_k[1],y=0, col="blue", pch = 1) + geom_point(x=b_k[1],y= 0, col="blue", pch = 1)+annotate("text",label="a",x=a_k[1],y=3)+annotate("text",label="b",x=b_k[1],y=3)
    
    ### Salvar o plot de cada iteração
    #TODO: Implementar k opções gráficas
    for (i in 1:cont){
      # Linha horizontal
      if(g_lh){ 
        k <- 3+2*(i-1)
        inter_x <-c(a_k[i],b_k[i])
        plot_vector[[k]] <<- plot_vector[[k-1]] + geom_segment(x=inter_x[1], xend=inter_x[2], y=inter_y[1]-1, yend=inter_y[2]-1,size=i + (i-1)*0.3,col="green4") +geom_point(x=inter_x[1],y=inter_y[1]-1,shape=21,bg="green4")+geom_point(x=inter_x[2],y=inter_y[2]-1,shape=21,bg="green4")
      }
      else{
        k <- i +2
      }
      # Indices dos pontos
      if(g_indices){
        plot_vector[[k+1]] <<- plot_vector[[k]] + geom_point(x=m_k[i],y=0, col="blue", shape = 1)+annotate("text",label=toString(i),x=m_k[i],y=3)
      }
      else{
        # Plot dos pontos m_k sobre o eixo x
        plot_vector[[k+1]] <<- plot_vector[[k]] + geom_point(x=m_k[i],y=0, col="blue", shape = 1)
      }
    }
    value_output <<- list()
    value_output[[1]] <<- paste("Aproximações: ",paste0(m_k, collapse =" | "))
  }
}
