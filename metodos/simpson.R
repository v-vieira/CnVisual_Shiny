simpson<- function(env_funcao,env_interv_integra_a,env_interv_integra_b,env_divisoes,g_lv,g_indices,g_pintar){
  ### vetor de erro
  error_vector <<-c()
  
  ### Valores de entrada
  f<-env_funcao
  m <- round(abs(env_divisoes))
    
  limitx <- c(env_interv_integra_a,env_interv_integra_b)
  limitx <- sort(limitx)
  
  ### Erro caso a quantidade de pontos seja 0
  if(m==0){
    error_vector <<- c(error_vector,'O número de pontos deve ser maior ou igual a 2')
  }
  ### Erro caso o numero de intervalos seja impar
  else if((m%%2)!=0){
    error_vector <<- c(error_vector,'O número de intervalos deve ser par')
  }
  #TODO: Corrigir soma
  else if(is.null(error_vector)){
    func <- paste("func <- function(x){",f,"}")
    func2 <- paste("func2 <- expression(",f,")")
    func_replot <- paste("func_replot <- function(x){",f,"+0.1-0.1}")
    
    eval(parse(text=func))
    eval(parse(text=func2))
    eval(parse(text=func_replot))
    
    DevFunc4 <- function(x){eval(D(D(D(D(func2,"x"),"x"),"x"),"x"))}
    
    pointx <- c()
    pointy <- c()
    
    simpson <- function(fun, a, b, n) {
      h <- abs((b-a))/(m)
      x <- seq(a, b, by=h)
      y <- func(x)
      if(m==2){
        s <- (h/3)*( y[1] + y[m+1] + 4*(sum(y[seq(2,m,by=2)])) )
      }
      else{
        s <- (h/3)*( y[1] + y[m+1] + 4*(sum(y[seq(2,m,by=2)])) + 2*(sum(y[seq(3,m-1,by=2)])) )
      }
      return(s)
    }
    
    soma <- simpson(func, limitx[1], limitx[2], m)
    
    xmin <- limitx[1]
    xmax <- limitx[2]
    
    pointx[1] <- limitx[1]
    pointy[1] <- func(pointx[1])
    h <- (abs(xmax - xmin)/m)
    for(i in 2:(m+1)){
      pointx[i] <- (pointx[i-1] + h)
      pointy[i] <- func(pointx[i])
    }
    
    ### Erro
    M4 <- 0
    
    if(abs(xmax-xmin)<=1000){by_for = 0.05}
    else{by_for = 0.001}
    for (x in seq(xmin,xmax,by=by_for)){
      if(abs(DevFunc4(x)) > M4){
        M4 <- abs(DevFunc4(x))
      }
    }
    
    Errosimp <- M4*(((xmax-xmin)*(h^4))/(180))
    
    ### Plot
    h_x <- abs(xmax - xmin)*0.05
    
    p <- ggplot(data = data.frame(x = 0,y=0), mapping = aes(x = x,y=y)) + geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + xlim(xmin-h_x,xmax+h_x)+ xlab("Eixo x") + ylab("Eixo y")
    p <- p + stat_function(fun = func, col = "red")
    
    plot_vector <<- list(p)
    
    for (i in 1:(m+1)){
      ### Caso seja marcado os indicies dos pontos no checkbox
      if(g_lv){
        p <- p + geom_segment(x=pointx[i],xend=pointx[i],y=0,yend=pointy[i],col='azure3',linetype='dashed')
      }
      p <- p + geom_point(x=pointx[i],y=pointy[i],col = 'blue',pch=1)
      
      ### Caso seja marcado os indicies dos pontos no checkbox
      if(g_indices){ 
        p <- p + annotate("text",label=toString(i-1),x=pointx[i],y=pointy[i],col='blue')
      }
      
    }
    
    plot_vector[[length(plot_vector)+1]] <<- p
    Fpoli<- c()
    
    area_vector <- list()
    p_area <- p
    for(i in seq(from=1, to=(m-1), by=2)){
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
      
      plot_vector[[length(plot_vector)+1]] <<- p
    }
    
    if(g_pintar){
      plot_vector[[length(plot_vector)+1]] <<- p_area + stat_function(fun = func_replot, col = "red")
    }
    
    ### Resultados
    value_output <<- list()
    value_output[[1]] <<- paste0("Valor da integração pelo metodo: ",soma," | Erro do metodo <= ",Errosimp)
  }
}