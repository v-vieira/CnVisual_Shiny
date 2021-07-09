simpson<- function(env_funcao,env_interv_integra,env_divisoes,env_linvt,env_indices,env_pintar){
  ### vetor de erro
  error_vector <<-c()
  
  ### Valores de entrada
  f<-env_funcao
  interentr<-env_interv_integra
  div<- abs(as.numeric(env_divisoes))
  
  interaux <- as.list(strsplit(interentr," ")[[1]])
  limitx <- c()
  for(i in 1:2){
    limitx[i] <- as.numeric(interaux[i])
  }
  limitx <- sort(limitx)
  
  func <- paste("func <- function(x){",f,"}")
  func2 <- paste("func2 <- expression(",f,")")
  
  eval(parse(text=func))
  eval(parse(text=func2))
  
  DevFunc4 <- function(x){eval(D(D(D(D(func2,"x"),"x"),"x"),"x"))}
  DevFunc5 <- function(x){eval(D(D(D(D(D(func2,"x"),"x"),"x"),"x"),"x"))}
  
  pointx <- c()
  pointy <- c()
  
  ### Erro caso a quantidade de divisões seja 0
  if(div==0){
    error_vector <<- c(error_vector,'Por favor, escolha o número de divisões')
  }
  ### Erro caso o numero de intervalos seja impar
  else if((div%%2)!=0){
    error_vector <<- c(error_vector,'O número de intervalos deve ser par')
  }
  
  else if(is.null(error_vector)){
    simpson <- function(fun, a, b, n) {
      h <- (b-a)/n
      x <- seq(a, b, by=h)
      y <- fun(x)
      s <- (h/3) * ( y[1] + y[n] + 4*(sum(y[2*(1:(n/2))+1])) + 2*(sum(y[2*(1:((n/2)-1))])) )
      return(s)
    }
    
    soma <- simpson(func, limitx[1], limitx[2], div)
    
    pointx[1] <- limitx[1]
    pointy[1] <- func(pointx[1])
    h <- ((limitx[2] - limitx[1])/div)
    for(i in 2:(div+1)){
      pointx[i] <- (pointx[i-1] + h)
      pointy[i] <- func(pointx[i])
    }
    
    xmin <- limitx[1]
    xmax <- limitx[2]
    
    j<- 1
    crit <- c()
    for(i in seq(from=xmin, to=xmax, by=0.01)){
      if(((DevFunc5(i))>-0.1)&&(DevFunc5(i)<0.1)){
        crit[j] <- DevFunc4(i)
        j <- j + 1
      }
    }
    
    Dev4min <- DevFunc4(xmin)
    Dev4max <- DevFunc4(xmax)
    
    if(Dev4min>Dev4max){
      valmax <- Dev4min
    }
    else {
      valmax <-Dev4max
    }
    if(j>1){
      for(i in 1:(j-1)){
        if(valmax < crit[i]){
          valmax <- crit[i]
        }
      }
    }
    ### Erro
    #TODO: Conferir erro
    Errosimp<- - valmax*(((xmax-xmin)*(h^4))/(180))
    
    p <- ggplot(data = data.frame(x = 0,y=0), mapping = aes(x = x,y=y)) + geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + xlim(xmin,xmax) + xlab("Eixo x") + ylab("Eixo y")
    p <- p + stat_function(fun = func, col = "red")
    
    plot_vector <<- list(p)
    
    for (i in 1:(div+1)){
      ### Caso seja marcado os indicies dos pontos no checkbox
      if(env_linvt){
        p <- p + geom_segment(x=pointx[i],xend=pointx[i],y=0,yend=pointy[i],col='azure3',linetype='dashed')
      }
      p <- p + geom_point(x=pointx[i],y=pointy[i],col = 'blue',pch=1)
      
      ### Caso seja marcado os indicies dos pontos no checkbox
      if(env_indices){ 
        p <- p + annotate("text",label=toString(i),x=pointx[i],y=pointy[i],col='blue')
      }
      
    }
    
    plot_vector[[2]] <<- p
    Fpoli<- c()
    
    j <- 3
    area_vector <- list()
    p_area <- p
    for(i in seq(from=1, to=(div-1), by=2)){
      Amatr <- array(c((pointx[i])^2,(pointx[i+1])^2,(pointx[i+2])^2,pointx[i],pointx[i+1],pointx[i+2],1,1,1),c(3,3))
      Ypon <- c(pointy[i],pointy[i+1],pointy[i+2])
      Ainver <- solve(Amatr)
      ABCMatr <- Ainver %*% Ypon
      Fpoli[i] <- paste0(ABCMatr[1],"*x^2+",ABCMatr[2],"*x+",ABCMatr[3])
      polifun <- paste("polifun <- function(x){",Fpoli[i],"}")
      eval(parse(text=polifun))
      p <- p + stat_function(fun = polifun,xlim=c(pointx[i]-h*0.15,pointx[i+2]+h*0.15))
      p_area <- p_area + 
                stat_function(fun=polifun,xlim=c(pointx[i],pointx[i+2]),geom='area',alpha=0.7,fill='skyblue') +
                stat_function(fun = polifun,xlim=c(pointx[i]-h*0.15,pointx[i+2]+h*0.15)) 
      
      plot_vector[[j]] <<- p
      j <- j + 1
    }
    
    #TODO: Implementar pintar a área
    if(env_pintar){
      plot_vector[[j]] <<- p_area + stat_function(fun = func, col = "red")
    }
    #Plot da funcao de novo, para ficar a cima dos outros plots
    # curve(func, xmin -1, xmax +1, col = "red", xlab="eixo x", ylab="eixo y", lwd=2, add=TRUE)
    # abline(h=0, lty=2)
    
    ### Resultados
    value_output <<- list()
    value_output[[1]] <<- paste0("Valor da integração pelo metodo: ",soma," | O erro do metodo: ",Errosimp)
  }
}