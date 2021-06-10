newtonraphson<- function(env_funcao,env_x0,env_intervalo,env_decimais,env_iteracoes,g_indices,g_lv,g_ltg)
{
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
  #Dev_x1 <- DevFunc((x0 - ((f_x0)/(Dev_x0))))  #valor da F'(x1)
  #TODO: Criar janela erro abaixo
  if(FALSE){
  # if(Dev_x0 == 0){ #= Erro caso a derivada fa funÇão em x0 seja zero
    # #== Criacao da janela de erro
    # error.x0 <- gwindow("Erro")
    # error.x0gt <- ggroup(horizontal = FALSE, container = error.x0)
    # error.x0gb <- ggroup(horizontal = FALSE, container = error.x0)
    # error.x0label <- glabel("a derivada da funcao tem valor zero no ponto x0 dado como chute inicial", container=error.x0gt)
    # exit.x0 <-function(h,...){dispose(error.x0)}
    # gbutton("Ok", cont= error.x0gb, handler = exit.x0)
    # stop
  }
  
  # Criando vetores para o plot
  x_k <- c()
  fx_k <- c()
  
  # Preenchendo os vetores
  x_k[1] <- x0
  x_k[2] <- x0 - f_x0/Dev_x0 #primeira iteracao para x1
  fx_k[1] <- f_x0
  fx_k[2] <- func(x_k[2]) #f(x1)
  
  # contador de indices para while
  cont <- 2
  
  #=== Garantindo que os pre-requisitos estao sendo seguidos exibindo janela de erro
  #TODO: IMPLEMENTAR ERRO ABAIXO
  #if(abs(func(x0))<stp){ #= Erro caso o ponto dado ja satisfaz o criterio de parada
  if(FALSE){  
    # #== Criacao da janela de erro
    # error.x0 <- gwindow("Erro")
    # error.x0gt <- ggroup(horizontal = FALSE, container = error.x0)
    # error.x0gb <- ggroup(horizontal = FALSE, container = error.x0)
    # error.x0label <- glabel("O ponto inicial dado ja satisfaz o criterio do erro minimo.", container=error.x0gt)
    # exit.x0 <-function(h,...){dispose(error.x0)}
    # gbutton("Ok", cont= error.x0gb, handler = exit.x0)
    # stop #Parar o c?digo se a janela for criada
  }
  else{ #Continuar o codigo...
    #===Comeco do metodo em si
    whilevar <- -1
    while(whilevar == -1) #= fazer iteracoes ate que o criterio seja atingido
    {
      cont <- cont + 1
      x_k[cont] <- (x_k[cont-1] - ((func(x_k[cont-1]))/(DevFunc(x_k[cont-1]))))
      fx_k[cont] <- func(x_k[cont])
      
      #Parar o m?todo pelo while
      if(cont>=stpint){whilevar <-1}
      if(abs(x_k[cont]-x_k[cont-1])<stp){whilevar <- 1}
      if(abs(fx_k[cont])<stp){whilevar <- 1}
    }
  }
  #==== Plot do metodo
  #
  
  #visible(gg) <- TRUE #= area grafica gg recebe o plot
  #plot(func, xlim = c(a0, b0), col = "red", xlab="Eixo x", ylab="Eixo y", container= gg) #= plot da curva
  #abline(h=0, lty=2)
  #abline(v=0, lty=2)
  p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + xlim(a0,b0)
  p <- p + stat_function(fun=func, col = "red")+ geom_vline(xintercept = 0) + geom_hline(yintercept = 0)
  plot_vector <<- list(p)
  
  z_k <- rep(0, cont) ## Vetor de zeros do tamanho do vetor m_k
  
  #== Animacao
  #TODO: IMPLEMENTAR O K PARA TODAS AS CONFIGURAÇÕES GRAFICAS
  for (i in 1:cont) #Para cada iteracao
  {
    k <- 2+ 4*(i-1)
    ##Plot dos pontos
    #============
    #Sys.sleep(speed/4)
    #points(x_k[1:i], z_k[1:i], col="blue", pch = 1) # Plot dos pontos x_k sobre o eixo x
    index <-c(0:(cont-1))
    
    p <- plot_vector[[k-1]] + geom_point(x=x_k[i],y=0,col="blue",shape=1)
    if(g_indices){ #Caso seja marcado os indicies dos pontos no checkbox
      #text(x_k[i],z_k[i],  index[i], cex=0.65, pos=3, col="blue")
      p <- p + annotate("text",label=toString(index[i]),x=x_k[i],y=3,col="blue")
    }
    plot_vector[[k]] <<- p
    
    if(g_lv){ #Caso seja marcado as linhas verticais no checkbox
      #==========
      #Sys.sleep(speed/4)
      #segments(x_k[i],0,x_k[i],fx_k[i], col= "azure4", lty=2)
      plot_vector[[k+1]] <<- plot_vector[[k]] + geom_segment(x=x_k[i],xend=x_k[i],y=0,yend=fx_k[i],col="darkgray",linetype="dashed")
    }
    ##Plot dos x_k na funcao
    #==============
    #Sys.sleep(speed/4)
    plot_vector[[k+2]]<<- plot_vector[[k+1]] + geom_point(x=x_k[i],y=fx_k[i], col="green", shape=1)
    if(g_ltg){ #Caso seja marcado as linhs tangentes no checkbox
      #=============
      #Sys.sleep(speed/4)
      plot_vector[[k+3]] <<- plot_vector[[k+2]] + geom_segment( x= x_k[i],xend=x_k[i+1],y=fx_k[i],yend =0,col = "black")
    }
  }
  
  # #=== Plot do zoom
  # #
  # dx <- (b0-a0)/10
  # visible(gg2) <- TRUE #agora a area grafica gg2 que ira receber o plot
  # par(mar=rep(0, 4)) #margem
  # plot(func, xlim=c(x_k[cont] - dx,x_k[cont] + dx), col = "red", xlab="Eixo x", ylab="Eixo y", container= gg) #plot da funcao
  # abline(h=0, lty=2)
  # abline(v=0, lty=2)
  # 
  # points(x_k[1:cont], z_k[1:cont], col="blue", pch = 1, cer = 5) # Plot dos pontos m_k sobre o eixo x
  # index <-c(0:cont)
  # if(svalue(pont)){
  #   text(x_k[1:cont],z_k[1:cont],index,cex=0.65, pos=3, col="blue")
  # }
  
  # if(svalue(lintg)){ #Caso seja marcado as linhas tangente no checkbox
  
  #  for(i in 1:cont-1)
  #   segments(x_k[i],fx_k[i],x_k[i+1],0.0,col = "black")
  #}
  #if(svalue(linvt)){ #Caso seja marcado as linhas verticais no checkbox
  #while(i<cont){
  # for(i in 1:cont-1)
  #  segments(x_k[i],0.0,x_k[i],fx_k[i], col= "azure4", lty=2)
  #}
  #Resultados a serem mostrados ao usuario
  value_output <<- list()
  value_output[[1]] <<-  paste("Aproximacoes: ",paste0(x_k, collapse =" | "))
  #insert(xk_output,valuetextm)
}