secante<- function(env_funcao,env_pontos,env_iteracoes,env_decimais,env_indices,env_linvt,env_linsec) #Fun??o principal (que faz o metodo)
{
  #== Valores de entrada
  f<-env_funcao
  initical_point <-env_pontos
  
  stpcont <-as.numeric(env_iteracoes)
  if(is.na(stpcont)){stpcont<-9999}
  
  s<-as.numeric(env_decimais)
  stp <- 10^(-s)
  
  
  
  #=== pegar os valores separados em x
  #
    valxaux <- as.list(strsplit(initical_point," ")[[1]])
  contval <- length(valxaux) # contador da quantidade de valores de entrada
  
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
  
  #TODO: Implementar erro abaixo
  #   if(((f_x0)*(f_x1))>0){ # Erro para garantir que os extremos tem sinais diferentes
  #== Criacao da janela de erro
  #    error.x0 <- gwindow("Erro", height=100, parent=c(250,100))
  #    error.x0gt <- ggroup(horizontal = FALSE, container = error.x0)
  #    error.x0label <- glabel("O intervalo ou nao tem raiz ou tem um numero par de raizes [ f(a)*f(b)<0 ]
  #      por favor, selecione um intervalo melhor.", container=error.x0gt)
  #   exit.x0 <-function(h,...){dispose(error.x0)}
  #   gbutton("Ok", cont= error.x0gt, handler = exit.x0)
  # }
  
  #===Comeco do metodo em si
  #
  #else{
  while(whileaux == -1) # Garantir que seja ate o criterio ser atingido
  {
    cont <- cont + 1
    x_k[cont] <- (x_k[cont-1] - ((fx_k[cont-1])*((x_k[cont-1] - x_k[cont-2])/(fx_k[cont-1] - fx_k[cont-2]))))
    fx_k[cont] <- func(x_k[cont])
    
    Errosec <-((x_k[cont]-x_k[cont-1])/(x_k[cont]))
    ##= Mudar o test quando atingir o erro desejado
    if(abs(Errosec)<stp){whileaux <- 1}
    if(cont>stpcont){whileaux <- 1}
    if(abs(fx_k[cont])<stp){whileaux <- 1}
  }
  
  #==== Plot do metodo
  
  #visible(gg) <- TRUE #Agora a area grafica gg que ira receber o plot
  

  p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + xlim(x0-1,x1+1) + xlab("Eixo x") + ylab("Eixo y")
  p <- p + stat_function(fun = func, col = "red") #= plot da curva
  
  plot_vector <<- list(p)
  
  # abline(h=0, lty=2)
  # abline(v=0, lty=2)
  
  # z_k <- rep(0, cont) #= vetor de zeros
  
  #== Animacao
  #
  index <-c(0:(cont-1)) #indices
  if(env_indices)
    p <- p + annotate("text",label="0",x=x_k[1],y=3,col="blue")
  
  p <- p + geom_point(x=x_k[1],y= 0, col="blue", pch = 1) # Plot dos pontos x_k sobre o eixo x
  plot_vector[[2]] <<- p
  
  #Sys.sleep(speed/4)
  if(env_linvt) #linhas verticais
    p <- p + geom_segment(x=x_k[1],xend=x_k[1],y=0,yend=fx_k[1],col= "azure4",linetype="dashed")
  
  #Sys.sleep(speed/4)
  plot_vector[[3]] <<- p + geom_point(x=x_k[1],y=fx_k[1],col="green",pch=1) #Plot dos x_k na funcao
  
  for (i in 2:(cont))
  {
    k <- 4 + (i-2)*5
    #Sys.sleep(speed/4)
    plot_vector[[k]] <<- plot_vector[[k-1]] + geom_point(x=x_k[i],y=0, col="blue", pch = 1) # Plot dos pontos x_k sobre o eixo x
    print(x_k[i])
    
    if(env_linvt){ #linhas verticais
      #Sys.sleep(speed/4)
      plot_vector[[k+1]] <<- plot_vector[[k]] + geom_segment(x=x_k[i],xend=x_k[i],y=0,yend=fx_k[i], col= "azure4", lty=2)
    }
    
    #Sys.sleep(speed/4)
    plot_vector[[k+2]] <<- plot_vector[[k+1]] + geom_point(x=x_k[i],y=fx_k[i], col="green", pch=1) #Plot dos x_k na funcao
    
    #Sys.sleep(speed/4)
    if(env_indices){ #indices dos pontos
      plot_vector[[k+3]] <<- plot_vector[[k+2]] + annotate("text",label=index[i],x=x_k[i],y=3,col="blue")
    }
    
    if(env_linsec){ #linhas secantes
      #Sys.sleep(speed/4)
      if((fx_k[i-1]*fx_k[i])<0)
        p <- geom_segment(x=x_k[i-1],xend=x_k[i],y=fx_k[i-1],yend=fx_k[i],col = "yellow")
      else
        p <- geom_segment(x=x_k[i-1],xend=x_k[i+1],y=fx_k[i-1],yend=0,col="yellow")
      plot_vector[[k+4]] <<- plot_vector[[k+3]] + p
    }
  }
  
  
  #=== Plot do zoom
  # dx <- (b0-a0)/10
  # visible(gg2) <- TRUE #agora a area grafica gg2 que ira receber o plot
  # par(mar=rep(0, 4)) #margem
  # plot(func, xlim=c(x_k[cont]-dx, x_k[cont]+dx), col = "red", xlab="", ylab="") #plot da funcao
  # abline(h=0, lty=2)
  # abline(v=0, lty=2)
  # 
  # points(x_k[1:cont], z_k[1:cont], col="blue", pch = 1) # Plot dos pontos m_k sobre o eixo x
  # 
  # if(svalue(pont)){ #Caso seja marcado os pontos no checkbox
  #   text(x_k,z_k,  index, cex=0.65, pos=3, col="blue")
  # }
  # if(svalue(linsc)){ #Caso seja marcado as linhas tangente no checkbox
  #   for(i in (1:cont)){
  #     if(i<cont){
  #       if((fx_k[i]*fx_k[i+1])<0){
  #         segments(x_k[i],fx_k[i],x_k[i+1],fx_k[i+1],col = "yellow")
  #       }
  #       else{
  #         segments(x_k[i],fx_k[i],x_k[i+2],0,col="yellow")
  #       }
  #     }
  #   }
  # }
  
  # if(svalue(linvt)){ #linhas verticais
  #   for(i in 1:cont){
  #     segments(x_k[i],0,x_k[i],fx_k[i], col= "azure4", lty=2)
  #   }
  # }
  
  #Resultados a serem mostrados ao usuario
  value_output <<- list()
  value_output[[1]] <<-paste0(x_k, collapse =" | ")
  #}
  
  
  
}