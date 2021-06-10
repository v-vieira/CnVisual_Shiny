library(ggplot2)
library(plotly)
falsa <- function(env_funcao,env_pontos,env_decimais,env_iteracoes,env_points,env_linsc)
{
  # Valores de entrada
  f<-env_funcao
  pointsentr<-env_pontos
  s<-as.numeric(env_decimais)
  stp <- 10^(-s)
  stpint <- as.numeric(env_iteracoes)
  if(is.na(stpint)) stpint <- 999 #numero ilimitado de itera??es
  speed = 1
  
  #=== pegar os valores separados em x
  #
  valxaux <- as.list(strsplit(pointsentr," ")[[1]])
  contval <- length(valxaux) # contador da quantidade de valores de entrada
  #intervalo
  a0 <-as.numeric(valxaux[1])
  b0 <- as.numeric(valxaux[2])
  
  func <- paste("func <- function(x){",f,"}")  # Criando string de entrada
  eval(parse(text=func))  # Transformando o texto salvo na variavel ftext em uma expressao
  
  fa <- func(a0)     # valor da F(a)
  fb <- func(b0)     # Valor da F(b)
  
  # Vetores para o plot
  a_k <- c()
  b_k <- c()
  fa_k <- c()
  fb_k <- c()
  m_k <- c()
  
  # Contador de indices para while
  cont <- 1
  
  #=== Garantindo que os pre-requisitos estao sendo seguidos exibindo janela de erro
  #TODO: Implementar erro abaixo
  # if(abs(fa)<stp) # Erro caso o extremo inferior ja satisfaca o criterio de parada
  # {
  #   error.FAMINOR <- gwindow("Erro", width = 15)
  #   error.FAgt <- ggroup(horizontal = FALSE, container = error.FAMINOR)
  #   error.FAgb <- ggroup(horizontal = FALSE, container = error.FAMINOR)
  #   error.FAlabel <- glabel("O extremo inferior do intervalo ja satisfaz o criterio de parada", container=error.FAgt)
  #   exit.FA <-function(h,...){dispose(error.FAMINOR)}
  #   gbutton("Ok", cont= error.FAgb, handler = exit.FA)
  #   stop
  # }
  
  #TODO: Implementar erro abaixo
  # if(abs(fb)<stp){ #Erro caso o extremo superior ja satisfaca o criterio de parada
  #   
  #   error.FBMINOR <- gwindow("Erro", width = 15)
  #   error.FBgt <- ggroup(horizontal = FALSE, container = error.FBMINOR)
  #   error.FBgb <- ggroup(horizontal = FALSE, container = error.FBMINOR)
  #   error.FBlabel <- glabel("O extremo superior do intervalo ja satisfaz o criterio de parada", container=error.FBgt)
  #   exit.FB <-function(h,...){dispose(error.FBMINOR)}
  #   gbutton("Ok", cont= error.FBgb, handler = exit.FB)
  #   stop
  # }
  
  
  #===Comeco do metodo em si
  #
  if((fa*fb)<0) # If para garantir que o metodo so seja feito caso tenha um numero impar de raizes no intervalo dado
  {
    whilevar <- -1
    while(whilevar == -1) # Garantir que seja feito somente ate que o criterio do erro nao seja satisfeito
    {
      #==Atribuicao dos valores aos vetores
      a_k[cont] <- a0
      b_k[cont] <- b0
      fa_k[cont] <- func(a0)
      fb_k[cont] <- func(b0)
      m_k[cont] <- (a_k[cont]*fb_k[cont] - b_k[cont]*fa_k[cont])/(fb_k[cont] - fa_k[cont]) #Atribuicao do ponto medio
      
      
      #== Definir qual sera o proximo a e b
      if(fa_k[cont]*func(m_k[cont])<0){
        b0 <- m_k[cont]
      }
      else {
        a0 <- m_k[cont]
      }
      
      if((cont>=2)&&(abs(b_k[cont]-a_k[cont])<stp)){whilevar <- 1}
      if(abs(func(m_k[cont]))<stp){whilevar<-1}
      if((cont)>=stpint){whilevar <- 1}
      cont<- cont + 1 # Aumentar o indice do contador
    }
    cont <- cont -1
    
    #==== Plot do metodo
    #
    
    y_min <- optimize(func,interval = c(a_k[1],b_k[1])) #y_min pega o valor que da o minimo em x e o valor em y
    y_min <- y_min$objective #y_min agora pega apenas o valor em y
    y_max <- optimize(func,interval = c(a_k[1],b_k[1]),maximum = TRUE)
    y_max <- y_max$objective
    absalt <- abs(y_max-y_min) #Altura total do plot
    absalt <- abs(y_max-y_min) #Altura total do plot
    
    # Definir uma altura minima nos extremos superiores e inferiores
    if(abs(y_min) <= 0.1*(absalt)) y_min<- -0.1*(absalt)
    if(abs(y_max) <= 0.1*(absalt)) y_max<- 0.1*(absalt)
    
    #visible(gg) <- TRUE #Agora a area grafica gg que ira receber o plot
    p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + xlim(a_k[1] - 1, b_k[1] + 1) + ylim(y_min,y_max) + xlab("Eixo x") + ylab("Eixo y")
    p <- p + stat_function(fun = func, col = "red") #plot da f(x)
    # abline(h=0, lty=2)
    # abline(v=0, lty=2)
    plot_vector <<- list(p)
    
    
    # z_k <- rep(0, cont) # Vetor de zeros do tamanho do vetor m_k
    
    #= Plot dos pontos a e b sobre o eixo x
    # points(a_k[1], 0, col="blue", pch = 1)
    # text(a_k[1],0,"a",cex=0.65, pos=3, col="blue")
    # points(b_k[1], 0, col="blue", pch = 1)
    # text(b_k[1],0,"b",cex=0.65, pos=3, col="blue")
    
    
    plot_vector[[2]] <<- plot_vector[[1]] + geom_point(x=a_k[1],y=0, col="blue", pch = 1) + geom_point(x=b_k[1],y= 0, col="blue", pch = 1)+annotate("text",label="a",x=a_k[1],y=3)+annotate("text",label="b",x=b_k[1],y=3)
    
    
    # Animacao
    #
    for (i in 1:cont) #Para cada iteracao
    {
      #Sys.sleep(speed/3)
      k <- 3 + 3*(i-1) 
      
      p <- plot_vector[[k-1]] + geom_point(x = m_k[i], y = 0, col="blue", pch = 1)# Plot dos pontos m_k sobre o eixo x
      
      if(env_points){#Indices dos pontos
        index <-c(0:(i-1))
        p <- p + annotate("text", label = toString(i),x= m_k[i],y=3 )
      }
      plot_vector[[k]] <<- p
      # Sys.sleep(speed/3)
      
      plot_vector[[k+1]] <<- plot_vector[[k]] + geom_segment(x=a_k[i],xend=a_k[i],y=0,yend=fa_k[i],col= "azure4",linetype="dashed")
      
      #segments(,0,a_k[i],, col= "azure4", lty=2)
      if(env_linsc){
        #Sys.sleep(speed/3)
        plot_vector[[k+2]] <<- plot_vector[[k+1]] + geom_segment(x=a_k[i],xend=b_k[i],y=fa_k[i],yend=fb_k[i], col="yellow") #lwd = 1.2
      }
    }
    
    
    #=== Plot do zoom
    #
    # dx <- (b0-a0)/10
    # visible(gg2) <- TRUE #agora a area grafica gg2 que ira receber o plot
    # par(mar=rep(0, 4)) #margem
    # plot(func, xlim=c(m_k[cont] - dx,m_k[cont] + dx), col = "red", xlab="Eixo x", ylab="Eixo y") #plot da funcao
    # abline(h=0, lty=2)
    # abline(v=0, lty=2)
    # 
    # points(m_k[1:cont], z_k[1:cont], col="blue", pch = 1) # Plot dos pontos m_k sobre o eixo x
    # index <-c(0:cont)
    # if(svalue(pont)){
    #   text(m_k[1:cont],z_k[1:cont],index,cex=0.65, pos=3, col="blue")
    # }
    # segments(a_k[1:cont],rep(0,cont),a_k[1:cont],fa_k[1:cont], col= "azure4", lty=2)
    # if(svalue(linsc)){
    #   segments(a_k[1:cont],fa_k[1:cont],b_k[1:cont],fb_k[1:cont], col="yellow", lwd = 1.2)
    # }
    # 
    #Resultados a serem mostrados ao usuario
    value_output <<- list()
    value_output[[1]] <<- paste("Aproximacoes: ",paste0(m_k, collapse =" | "))
    #insert(mk_output,valuetextm)
  }
  
  #== Erro para caso tenha um numero par de raizes
  #TODO: Implementar erro abaixo
  # else{
  #   error.NoNegative <- gwindow("Erro",width = 10)
  #   error.NNgt <- ggroup(horizontal = FALSE, container = error.NoNegative)
  #   error.NNgb <- ggroup(horizontal = FALSE, container = error.NoNegative)
  #   error.NNlabel <- glabel("No intervalo dado a funcao nao tem raiz ou tem um numero par de raizes. Escolha outro interlavo", container=error.NNgt)
  #   exit.NN <-function(h,...){dispose(error.NoNegative)}
  #   gbutton("Ok", cont= error.NNgb, handler = exit.NN)
  #   
  # }
}