inter_pontos<- function(env_ponto_aprox,env_pontos_x,env_pontos_y,g_indices,g_lv){
  ### vetor de erro
  error_vector <<-c()
  warning_vector <<- c()
  
  ### Valores de entrada
  valaprm <- as.numeric(env_ponto_aprox)
  xentr <- env_pontos_x
  yentr <- env_pontos_y
  
  valxaux <- as.list(strsplit(xentr," ")[[1]])
  contvalx <- length(valxaux)
  valx <- c()
  for(i in 1:contvalx){
    valx[i] <- as.numeric(valxaux[i])
  }
  if(!all(sort(valx)==valx)){
    warning_vector <<- c(warning_vector,'Os pontos em x não estão em ordem crescente. Verifique se o plot está conforme o esperado;')
  }
  
  valyaux <- as.list(strsplit(yentr," ")[[1]])
  contvaly <- length(valyaux)
  valy <- c()
  for(i in 1:contvaly){
    valy[i] <- as.numeric(valyaux[i])
  }
  ### Erros
  if(contvalx!=contvaly){
    error_vector <<- c(error_vector,'A quantidade de pontos em x e y não são iguais')
  }
  if(is.null(error_vector)){
    tryCatch({
      contval <- contvalx
      
      ### Criação dos elementos Lagrange
      lagrange <- rep(1,contval)
      for(j in 1:contval){
        for(i in 1:contval){
          if(i!=j){lagrange[j] <- paste0(lagrange[j], "*", "(x -",valx[i],")/(", valx[j]-valx[i],")")}
        }
      }
      
      polinomio <- 0
      for(i in 1:contval){
        polinomio <- paste(polinomio, "+",valy[i],"*",lagrange[i])
      }
      
      ### Criação da função polinomio
      f_text <- paste("PolLagrange<- function(x){",polinomio,"}")
      eval(parse(text=f_text))
      
      # valor a ser aproximado
      Valaprmy <- PolLagrange(valaprm)
      
      x_min <- min(valx)
      x_max <- max(valx)
      if(valaprm<x_min){x_min <- valaprm}
      else if (valaprm>x_max){x_max <- valaprm}
      
      y_min <- 0
      y_max <- 0
      
      if(abs(x_max-x_min)<=1000){by_for = 0.05}
      else{by_for = 0.001}
      
      tryCatch({
        for (x in seq(x_min,x_max,by=by_for)){
          if (PolLagrange(x) < y_min){
            y_min <- PolLagrange(x)
          }
          if(PolLagrange(x) > y_max){
            y_max <- PolLagrange(x)
          }
        }
      },
      warning = function(w){
        if(is.null(error_vector)){
          error_vector <<- c(error_vector,'Não é possível calcular o ponto na função;')
        }
        return(NULL)
      },
      error = function(e){
        if(is.null(error_vector)){
          error_vector <<- c(error_vector,'Não é possível calcular o ponto na função;')
        }
        return(NULL)
      })
      
      h_ind <- abs(y_max-y_min)*0.04
      h_x <- abs(x_max-x_min)*0.05
      
      p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + xlim(x_min-h_x, x_max+h_x) + xlab("Eixo x") + ylab("Eixo y")
      
      # Plot dos pontos
      for(i in 1:contval){
        p <- p + geom_point(x=valx[i],y=valy[i],col='blue',pch=1)
      }
      
      if(g_lv){
        for(i in 1:contval){
          p <- p + geom_segment(x=valx[i],xend=valx[i],y=0,yend=valy[i],col="azure4",linetype="dashed")
        }
      }
      ### Plot dos indices
      if(g_indices){
        for(i in 1:contval){
          p <- p + annotate("text",label=toString(i),x=valx[i],y=valy[i]+h_ind,col='blue')
        }
      }
      
      plot_vector <<- list(p)
      
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
      
      ### Plot do polinomio
      plot_vector[[2]] <<- plot_vector[[1]] + stat_function(fun = PolLagrange, col = "red")
      
      ### Plot do ponto aproximado
      p <- plot_vector[[2]] + geom_point(x=valaprm,y=Valaprmy,col="chartreuse4",pch=9)
      
      if(g_lv){
        p <- p + geom_segment(x=valaprm,xend=valaprm,y=0,yend=Valaprmy, col="azure4", linetype="dashed")
      }
      # if(g_indices){
      #   p <- p + annotate("text",label=Valaprmy,valaprm,Valaprmy, col="chartreuse4")
      # }
      
      plot_vector[[3]] <<- p
      
      ### Resultados
      value_output <<- list()
      value_output[[1]] <<- paste0("O valor aproximado pela interpolacao e: ",Valaprmy)
      
      
    },
    warning = function(w){
      if(is.null(error_vector)){
        error_vector <<- c(error_vector,'Erro desconhecido;')
      }
      return(NULL)
    },
    error = function(e){
      if(is.null(error_vector)){
        error_vector <<- c(error_vector,'Erro desconhecido;')
      }
      return(NULL)
    })
  }
}