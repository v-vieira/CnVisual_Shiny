simpson<- function(env_funcao,env_interv_integra,env_divisoes,env_linvt,env_indices,env_pintar)
{
  #== Valores de entrada
  
  f<-env_funcao
  interentr<-env_interv_integra
  div<-as.numeric(env_divisoes)
  
  #============== pegar os valores separados em x =======
  interaux <- as.list(strsplit(interentr," ")[[1]])
  limitx <- c()
  for(i in 1:2){
    limitx[i] <- as.numeric(interaux[i])
  }
  
  # Criando strings de entrada
  func <- paste("func <- function(x){",f,"}")
  func2 <- paste("func2 <- expression(",f,")")
  
  # transformando o texto salvo em uma express?es
  eval(parse(text=func))
  eval(parse(text=func2))
  
  #=============== Funcoes derivadas ===============
  DevFunc4 <- function(x){eval(D(D(D(D(func2,"x"),"x"),"x"),"x"))}
  DevFunc5 <- function(x){eval(D(D(D(D(D(func2,"x"),"x"),"x"),"x"),"x"))}
  
  # Vetores para o plot
  pointx <- c()
  pointy <- c()
  
  #TODO: Implementar msg de erro abaixo
  # if((div%%2)!=0) # Erro para caso o numero de intervalos seja impar
  # {
  #   error.INTERIMP <- gwindow("Erro", width = 15)
  #   error.IIgt <- ggroup(horizontal = FALSE, container = error.INTERIMP)
  #   error.IIgb <- ggroup(horizontal = FALSE, container = error.INTERIMP)
  #   error.FAlabel <- glabel("O numero de intervalos e impar, favor forneca um numero par.", container=error.IIgt)
  #   exit.II <-function(h,...){dispose(error.INTERIMP)}
  #   gbutton("Ok", cont= error.IIgb, handler = exit.II)
  #   stop
  # }
  
  #else{
    #===Comeco do metodo em si
    #
    simpson <- function(fun, a, b, n) {
      h <- (b-a)/n
      x <- seq(a, b, by=h)
      y <- fun(x)
      s <- (h/3) * ( y[1] + y[n] + 4*(sum(y[2*(1:(n/2))+1])) + 2*(sum(y[2*(1:((n/2)-1))])) )
      return(s)
    }
    
    soma <- simpson(func, limitx[1], limitx[2], div)
    
    # ====Preenchendo os vetores para o plot
    pointx[1] <- limitx[1]
    pointy[1] <- func(pointx[1])
    h <- ((limitx[2] - limitx[1])/div)
    for(i in 2:(div+1)){
      pointx[i] <- (pointx[i-1] + h)
      pointy[i] <- func(pointx[i])
    }
    
    ##=== Plotar a funcao dentro do intervalo
    
    xmin <- limitx[1]
    xmax <- limitx[2]
    
    ##======== Calcular o erro ======
    ##
    j<- 1 #j ? uma variavel para encher o vetor critico
    crit <- c() #=== que ser? preenchido
    for(i in seq(from=xmin, to=xmax, by=0.01)){ ##=== Achar todos os pontos cr?ticos
      if(((DevFunc5(i))>-0.1)&&(DevFunc5(i)<0.1)){
        crit[j] <- DevFunc4(i)  #=== Encher o vetor crit quando encontrado
        j <- j + 1
      }
    }
    
    Dev4min <- DevFunc4(xmin) #Valor da f''(x) quando xmin
    Dev4max <- DevFunc4(xmax) #Valor da f''(x) quando xmax
    
    if(Dev4min>Dev4max){ #Achar qual e maior dos valores da f''(x) nos extremos
      valmax <- Dev4min
    }
    else {   #Achar qual e maior dos valores da f''(x) nos extremos
      valmax <-Dev4max
    }
    if(j>1){
      for(i in 1:(j-1)){ #Avaliar entre o valor dos extremos e os criticos da f''(x)
        if(valmax < crit[i]){
          valmax <- crit[i]
        }
      }
    }
    Errosimp<- - valmax*(((xmax-xmin)*(h^4))/(180)) #Erro propriamente
    
    #==== Plot do metodo
    #
    #visible(gg) <- TRUE
    p <- ggplot(data = data.frame(x = 0,y=0), mapping = aes(x = x,y=y)) + geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + xlim(xmin,xmax) + xlab("Eixo x") + ylab("Eixo y")
    p <- p + stat_function(fun = func, col = "red")
    
    plot_vector <<- list(p)
    # plot(func, xlim=c(xmin,xmax), col = "red", xlab="eixo x", ylab="eixo y")
    # abline(h=0, lty=2)
    # abline(v=0, lty=2)
    
    # z_k <- rep(0, (div+1)) #Vetor de zeros
    index <-c(0:(div-1)) #Indice
    
    for (i in 1:(div+1))
    {
      
      if(env_linvt){
        p <- p + geom_segment(x=pointx[i],xend=pointx[i],y=0,yend=pointy[i],col='azure3',linetype='dashed')
        #segments( pointx[i], pointy[i], lty=2 , col = "azure3")
      }
      p <- p + geom_point(x=pointx[i],y=pointy[i],col = 'blue',pch=1)
      
      if(env_indices){ #Caso seja marcado os indicies dos pontos no checkbox
        p <- p + annotate("text",label=toString(i),x=pointx[i],y=pointy[i],col='blue')
        #text(pointx[i],pointy[i],  index[i], cex=0.65, pos=3, col="blue")
      }
      
    }
    
    plot_vector[[2]] <<- p
    #Vetor para o plot das parabolas
    Fpoli<- c()
    
    #============== Plot das parabolas ================#
    j <- 3
    for(i in seq(from=1, to=(div-1), by=2)){
      #Sys.sleep(speed)
      Amatr <- array(c((pointx[i])^2,(pointx[i+1])^2,(pointx[i+2])^2,pointx[i],pointx[i+1],pointx[i+2],1,1,1),c(3,3))#Matrix com pontos
      Ypon <- c(pointy[i],pointy[i+1],pointy[i+2]) #=== Vetor com os valores em y
      Ainver <- solve(Amatr) #=== Matriz inversa para resolu??o
      ABCMatr <- Ainver %*% Ypon #=== matriz com os valores A, B e C, do polinomio que esta sendo feito
      Fpoli[i] <- paste0(ABCMatr[1],"*x^2+",ABCMatr[2],"*x+",ABCMatr[3]) #==== Juntando o valor da matriz inversa com o A B e C
      polifun <- paste("polifun <- function(x){",Fpoli[i],"}")
      eval(parse(text=polifun))
      p <- p + stat_function(fun = polifun,xlim=c(pointx[i]-h*0.15,pointx[i+2]+h*0.15))
      plot_vector[[j]] <<- p
      j <- j + 1
      #curve(polifun , (), (),type="l", add=TRUE)
    }
    
    #TODO: Implementar pintar a área
    #========= Pintar a area ============#
    # if(svalue(pint)){
    #   #Sys.sleep(speed)
    #   for(i in seq(from=1, to=(div-1), by=2)){
    #     polifun <- paste("polifun <- function(x){",Fpoli[i],"}")
    #     eval(parse(text=polifun))
    #     xini <- pointx[i] #=== x inicial ===
    #     xfin <- pointx[i+2] #==== x final
    #     cord.x <- c(xini,seq(xini,xfin,0.01),xfin)
    #     cord.y <- c(0,polifun(seq(xini,xfin,0.01)),0)
    #     polygon(cord.x,cord.y, col="skyblue", border = "skyblue")
    #     curve(polifun , (pointx[i]-h*0.15), (pointx[i+2]+h*0.15),type="l", add=TRUE)
    #   }
    # }
    #Plot da funcao de novo, para ficar a cima dos outros plots
    # curve(func, xmin -1, xmax +1, col = "red", xlab="eixo x", ylab="eixo y", lwd=2, add=TRUE)
    # abline(h=0, lty=2)
    
    #Resultados a serem mostrados ao usuario
    value_output <<- list()
    value_output[[1]] <<- paste0("Valor da integração pelo metodo: ",soma," | O erro do metodo: ",Errosimp)
    
    
  #}
}