inter_funcao <- function(env_ponto_aprox,env_pontos_x,env_funcao,env_indices,env_linvt){
  
  # interentrx <- #env_pontos_x
  # interentry <- #env_pontos_y
  valaprm <- as.numeric(env_ponto_aprox)
  xentr <- env_pontos_x
  f<-env_funcao 
  
  func <- paste("func <- function(x){",f,"}")# Criando string de entrada
  eval(parse(text=func))# Transformando o texto salvo na variavel ftext em uma expressao
  
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
  valy<-c()
  for(i in 1:contval){
    valy[i] <- func(valx[i])
  }
  
  #=== Pegas os extremos em x
  #
  # interauxx <- as.list(strsplit(interentrx," ")[[1]])
  # plotintervalx <- c()
  # for(i in 1:2){
  #   plotintervalx[i] <- as.numeric(interauxx[i])
  # }
  
  #=== Pegas os extremos em y
  #
  # interauxy <- as.list(strsplit(interentry," ")[[1]])
  # plotintervaly <- c()
  # for(i in 1:2){
  #   plotintervaly[i] <- as.numeric(interauxy[i])
  # }
  # 
  
  ##=== Criar o polinomio de lagrange
  #
  lagrange <- rep(1,contval) # encher o vetor de 1, elemento neutro na multipl.
  for(j in 1:contval){ # for que determina o grau do polinomio
    for(i in 1:contval){ # for para fazer cada L
      if(i!=j){ # If para n?o acontecer de de i==j
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
  
  Valaprmy <- PolLagrange(valaprm) # Valor no polinomio do ponto a ser aproximado
  valrealy <- func(valaprm) # Valor na funcao do ponto a ser aproximado
  
  #==== Plot do metodo
  #
  
  #=Pontos maximos e minimos
  x_min <- min(valx)-1
  x_max <- max(valx)+1
  y_min <- min(valy)-1
  y_max <- max(valy)+1
  
  # visible(gg) <- TRUE #Agora a area grafica gg que ira receber o plot
  
  p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + xlim(x_min,x_max) + ylim(y_min, y_max) + xlab("Eixo x") + ylab("Eixo y")
  p <- p + stat_function(fun = func, col = "black")
  
  plot_vector <<- list (p)
  
  for(i in 1:contval){  #Plot dos pontos na f(x)
    p <- p + geom_point(x=valx[i],y=valy[i],col='blue',pch=1)
  }
  
  # plot(func, xlim=c(x_min,x_max), ylim= c(y_min,y_max),xlab=("Eixo x"),ylab=("Eixo y"), col="black")  #Plot da f(x)
  # abline(h=0, lty=2)
  # abline(v=0, lty=2)
  
  # Sys.sleep(1/3)
  # points(valx,valy, col="blue",pch=1)  
  
  if(env_indices){ #Caso seja marcado os indicies dos pontos no checkbox
    index <-c(0:(contval-1)) #= Indicies dos pontos
    for(i in 1:contval){  #Plot dos pontos na f(x)
      p <- p + annotate("text",label= index[i],x=valx[i],y=valy[i],col='blue')
    }
  }
  if(env_linvt){
    for (i in 1:(contval)){ #Caso seja marcado as linhas verticais no checkbox
      p <- p + geom_segment(x=valx[i],xend=valx[i],y=0,yend=valy[i], col= "azure4", linetype='dashed')
    }
  }
  
  plot_vector[[2]] <<- p
  ##= Plot do polinomio de lagrange
  
  # if(svalue(anim)){# Caso a opcao de animacao esteja marcada
  #   kmax <- 100
  #   for(k in 1:kmax){ #desenhar o polinomio com o tempo
  #     Sys.sleep(1/8)
  #     l <- (k-1)/(kmax -1)
  #     x0 <- x_min
  #     xk <- (1-l)*x_min + l*x_max
  #     
  #     curve(Pollagrange, xlim= c(x0, xk), col = "red", xlab="eixo x", ylab="eixo y", add = TRUE) #= plot do polinonmio
  #   }
  # }
  # else{ #sem animacao
    plot_vector[[3]] <<- plot_vector[[2]] + stat_function(fun = PolLagrange, col = "red") #= plot do polinonmio
  # }
  
  ##==Plot do ponto aproximado
  # Sys.sleep(1/2)
  #=  valor no polinomio
  p <- plot_vector[[3]] + geom_point(x=valaprm, y= Valaprmy, col="chartreuse4", pch=9) + annotate("text",label=toString(Valaprmy),x=valaprm,y=Valaprmy, col="chartreuse4")
  # Sys.sleep(1/2)
  #= valor na f(x)
  p <- p + geom_point(x=valaprm,y= valrealy, col="chartreuse4", pch=9) + annotate("text", label = toString(valrealy),x=valaprm,y=valrealy, col="chartreuse4")
  # Sys.sleep(1/2)
  plot_vector[[4]] <<- p + geom_segment(x=valaprm,xend=valaprm,y=Valaprmy,yend= valrealy, col = "chartreuse4")
  
  
  
  
  # Plot do zoom
  #
  # if(Valaprmy < valrealy){  # fazer com que a diferen?a fique centralizada
  #   yplot_min <- Valaprmy
  #   yplot_max <- valrealy
  # }
  # else{
  #   yplot_min <- valrealy
  #   yplot_max <- Valaprmy
  # }
  
  # visible(gg22) <- TRUE #agora a area grafica gg2 que ira receber o plot
  # par(mar = rep(2,4)) #margem
  # 
  # plot(Pollagrange, xlim= c(valaprm - 0.5, valaprm + 0.5), ylim= c(yplot_min - 0.5,yplot_max + 0.5), col="red")#plot do polinomio
  # points(valaprm, Valaprmy, add = TRUE, col = "chartreuse4",  pch = 9) #= plot do ponto aproximado no polinimio
  # points(valaprm, valrealy, add = TRUE, col = "chartreuse4",  pch = 9) #= plot do ponto aproximado na f(c)
  # segments(valaprm, valrealy, valaprm, Valaprmy, col = "chartreuse4", add=TRUE) #= segmento de diferen?a entre os 2 pontos
  # curve(func, col = "black", xlab="", ylab="", add= TRUE) #= plot funcao dada
  # 
  # if(svalue(pont)){ #Caso seja marcado os pontos no checkbox
  #   text(valx,valy,  index, cex=0.65, pos=3, col="blue")
  # }
  
  #Resultados a serem mostrados ao usuario
  valerro <- (abs(Valaprmy - valrealy)) # Calculo do erro absoluto
  value_output <<- list()
  value_output[[1]] <<- paste0("O valor achado pelo método é: ",Valaprmy, " | O valor absoluto do erro é: ",valerro)
}