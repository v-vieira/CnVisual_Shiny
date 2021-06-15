inter_pontos<- function(env_ponto_aprox,env_pontos_x,env_pontos_y,env_indices,env_linvt)
{
   #== Valores de entrada
  valaprm <- as.numeric(env_ponto_aprox)
  # interentrx <- #env_pontos_x
  # interentry <- #env_pontos_y
  xentr <- env_pontos_x
  yentr <- env_pontos_y
  
  #=== pegar os valores separados em x
  #
  valxaux <- as.list(strsplit(xentr," ")[[1]])
  contval <- length(valxaux) # contador da quantidade de valores de entrada, tanto x quanto para y
  valx <- c()
  for(i in 1:contval){
    valx[i] <- as.numeric(valxaux[i])
  }
  
  #=== pegar os valores separados em y
  #
  valyaux <- as.list(strsplit(yentr," ")[[1]])
  valy <- c()
  for(i in 1:contval){
    valy[i] <- as.numeric(valyaux[i])
  }
  
  #=== Pegas os extremos em x
  # #
  # interauxx <- as.list(strsplit(interentrx," ")[[1]])
  # plotintervalx <- c()
  # for(i in 1:2){
  #   plotintervalx[i] <- as.numeric(interauxx[i])
  # }
  # 
  # #=== Pegas os extremos em y
  # #
  # interauxy <- as.list(strsplit(interentry," ")[[1]])
  # plotintervaly <- c()
  # for(i in 1:2){
  #   plotintervaly[i] <- as.numeric(interauxy[i])
  # }
  
  
  ##=== Criar o polinomio de lagrange
  #
  lagrange <- rep(1,contval) # encher o vetor de 1, elemento neutro na multipl.
  for(j in 1:contval){ #======== for que termina o grau do polinomio
    for(i in 1:contval){ #======= for para fazer cada L
      if(i!=j){ #===== If para n?o acontecer de de i==j
        lagrange[j] <- paste0(lagrange[j], "*", "(x -",valx[i],")/(", valx[j]-valx[i],")")
      }
    }
  }
  
  ##=== Juntar os yi com os li e montar o polinomio
  polinomio <- 0
  for(i in 1:contval){
    polinomio <- paste(polinomio, "+",valy[i],"*",lagrange[i])
  }
  
  
  f_text <- paste("PolLagrange<- function(x){",polinomio,"}")# Criando string de entrada
  eval(parse(text=f_text))# Transformando o texto salvo na variavel ftext em uma expressao
  
  
  Valaprmy <- PolLagrange(valaprm)# Valor no polinomio do ponto a ser aproximado
  
  #==== Plot do metodo
  #
  #=Pontos maximos e minimos
  # x_min <- plotintervalx[1]
  # x_max <- plotintervalx[2]
  # y_min <- plotintervaly[1]
  # y_max <- plotintervaly[2]
  
  # visible(gg) <- TRUE #Especificar que a area grafica gg recebe o grafico
  p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + xlim(min(valx), max(valx)) + ylim(min(valy), max(valy)) + xlab("Eixo x") + ylab("Eixo y")
  
  for(i in 1:contval){
    p <- p + geom_point(x=valx[i],y=valy[i],col='blue',pch=1)
  }
  # plot(valx, valy, xlim= c(x_min,x_max),ylim= c(y_min,y_max),  xlab ="Eixo x", ylab="Eixo y",col="blue", pch=1) #== plot dos pontos
  #abline(h=0, lty=2, col ="azure2")
  #abline(v=0, lty=2, col ="azure2")
  
  # abline(h=0, lty=2)
  # abline(v=0, lty=2)
  
  if(env_indices){ #Caso seja marcado os indicies dos pontos no checkbox
    for(i in 1:contval){
      p <- p + annotate("text",label=toString(i),x=valx[i],y=valy[i],col='blue')
    }
  }
  
  if(env_linvt){
    for(i in 1:contval){
      p <- p + geom_segment(x=valx[i],xend=valx[i],y=0,yend=valy[i],col="azure4",linetype="dashed")
    }
  }
  
  plot_vector <<- list(p)
  
  ##= Plot do polinomio de lagrange
  #TODO: Implementar animação
  # if(svalue(anim)){# Caso a opcao de animacao esteja marcada
  #   kmax <- 100
  #   for(k in 1:kmax){ #desenhar o polinomio com o tempo
  #     Sys.sleep(1/8)
  #     l <- (k-1)/(kmax -1)
  #     x0 <- x_min
  #     xk <- (1-l)*x_min + l*x_max
  #     
  #     curve(PolLagrange,xlim= c(x0, xk), col = "red", add = TRUE) #plot do polinomio
  #   }
  # }
  
  #else{ #sem animacao
    plot_vector[[2]] <<- plot_vector[[1]] + stat_function(fun = PolLagrange, col = "red") #plot do polinomio
  #}
  
   
  ##==Plot do ponto aproximado
  #Sys.sleep(2/3)
    
  p <- plot_vector[[2]] + geom_segment(x=valaprm,xend=valaprm,y=0,yend=Valaprmy, col="azure4", linetype="dashed") + geom_point(x=valaprm,y=Valaprmy,col="chartreuse4",pch=9)
  
  # points(valaprm, Valaprmy, col="chartreuse4", pch=9, cex=2)
  if(env_indices){ #Caso seja marcado os indicies dos pontos no checkbox
    p <- p + annotate("text",label="Valaprmy",valaprm,Valaprmy, col="chartreuse4")
  }
 
  plot_vector[[3]] <<- p
  #Resultados a serem mostrados ao usuario
  value_output <<- list()
  value_output[[1]] <<- paste0("O valor aproximado pela interpolacao e: ",Valaprmy)
  
}