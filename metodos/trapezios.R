trapezios<- function(env_funcao,env_interv_integra_a,env_interv_integra_b,env_divisoes,g_pintar,g_lv,g_indices){
  ### vetor de erro
  error_vector <<-c()
  
  ### Valores de entrada
  f<-env_funcao
  m<- round(abs(env_divisoes))
  
  limitx <- c(env_interv_integra_a,env_interv_integra_b)
  limitx <- sort(limitx)
  
  ### Erro caso a quantidade de divisões seja 0
  if(m==0){
    error_vector <<- c(error_vector,'Numero de divisões deve ser maior ou igual a 1')
  }
  else if(is.null(error_vector)){
    func <- paste("func <- function(x){",f,"}")
    func_replot <- paste("func_replot <- function(x){",f,"+0.1-0.1}")
    func2 <- paste("func2 <- expression(",f,")")
    
    eval(parse(text=func))
    eval(parse(text=func2))
    eval(parse(text=func_replot))
    
    DevFunc2 <- function(x){eval(D(D(func2,"x"),"x"))}
    
    trapezoid <- function(fun, a, b, n) {
      h <- abs((b-a))/n
      x <- seq(a, b, by=h)
      y <- fun(x)
      s <- h * (y[1]/2 + sum(y[2:n]) + y[n+1]/2)
      return(s)
    }
    
    soma <- trapezoid(fun=func, a=limitx[1], b=limitx[2], n=m + 1)
    pointx <- c()
    pointy <- c()
    pointx[1] <- limitx[1]
    pointy[1] <- func(pointx[1])
    h <- ((limitx[2] - limitx[1])/m)
    for(i in 2:(m+1)){
      pointx[i] <- (pointx[i-1] + h)
      pointy[i] <- func(pointx[i])
    }
    
    x_min <- limitx[1]
    x_max <- limitx[2]
    
    ### Erro
    
    M2 <- 0
    
    if(abs(x_max-x_min)<=1000){by_for = 0.05}
    else{by_for = 0.001}
    for (x in seq(x_min,x_max,by=by_for)){
      if(abs(DevFunc2(x)) > M2){
        M2 <- abs(DevFunc2(x))
      }
    }
    
    Errotrap <- M2*(h^2)*(x_max-x_min)/12
    
    ### Plot
    y_min <- 0
    y_max <- 0
    
    if(abs(x_max-x_min)<=1000){by_for = 0.05}
    else{by_for = 0.001}
    
    for (x in seq(x_min,x_max,by=by_for)){
      if (func(x) < y_min){
        y_min <- func(x)
      }
      if(func(x) > y_max){
        y_max <- func(x)
      }
    }
    
    h_ind <- abs(y_max-y_min)*0.04
    h_x <- abs(x_max - x_min)*0.05
    
    p <- ggplot(data = data.frame(x = 0,y=0), mapping = aes(x = x,y=y)) + geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + xlim(x_min-h_x,x_max+h_x) + xlab("Eixo x") + ylab("Eixo y")
    p <- p + stat_function(fun = func, col = "red")
    
    plot_vector <<- list(p)
    
    for(i in 1:length(pointx)){
      p <- p + geom_point(x=pointx[i],y= pointy[i], col="blue", pch = 1)
    }
    
    if(g_indices){
      for(i in 1:length(pointx)){
        p <- p + annotate("text", label=toString(i-1),pointx[i],pointy[i]+h_ind, col="blue")
      }
    }
    if(g_lv){
      for(i in 1:length(pointx)){
        p <- p + geom_segment(x=pointx[i],xend=pointx[i],y=0,yend= pointy[i], col = "gray48",linetype='dashed')
      }
    }
    
    plot_vector[[length(plot_vector)+1]] <<- p
    
    for (i in 1:(m+1)){
      if(i!=(m+1)){
        plot_vector[[length(plot_vector)+1]] <<- plot_vector[[length(plot_vector)]] + geom_segment(x=pointx[i],xend=pointx[i+1],y=pointy[i],yend=pointy[i+1])
        p <- plot_vector[[length(plot_vector)]]
      }
    }
    
    if(g_pintar){
      for(i in 1:m){
        shape <- data.frame(x=c(pointx[i],pointx[i],pointx[i+1],pointx[i+1]),y=c(0,pointy[i],pointy[i+1],0)) 
        p <- p + geom_polygon(data=shape,fill='skyblue',alpha=0.7)
      }
      
      for (i in 1:(m+1)){
        if(i!=(m+1)){ 
          p <- p + geom_segment(x=pointx[i],xend=pointx[i+1],y=pointy[i],yend=pointy[i+1])
        }
      }
      
      if(g_lv){
        for(i in 1:length(pointx)){
          p <- p + geom_segment(x=pointx[i],xend=pointx[i],y=0,yend=pointy[i], col = "gray48",linetype='dashed')
        }
      }
    }
    
    plot_vector[[length(plot_vector)+1]] <<- p + stat_function(fun = func_replot, col = "red")
    
    value_output <<- list()
    value_output[[1]] <<-  paste0("Valor da integração pelo metodo: ",soma," | Erro do metodo <= ",Errotrap)
  }
}