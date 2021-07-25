newtonraphson<- function(env_funcao,env_x0,env_decimais,env_iteracoes,g_indices,g_lv,g_ltg){
  ### vetor de erro
  error_vector <<-c()
  warning_vector <<- c()
  
  #== Valores de entrada
  f<-env_funcao 
  x0<-env_x0
  
  s<-round(abs(env_decimais))
  if(env_decimais<0){
    warning_vector <<- c(warning_vector,'O número de casas decimais não pode ser negativo, foi utilizado o normal;')
  }
  if(round(env_decimais)!=env_decimais){
    warning_vector <<- c(warning_vector,'O número de casas decimais não pode decimal, foi arredondado;')
  }
  if(s==0){
    s<-5
    warning_vector <<- c(warning_vector,'O número de casas decimais dado é 0, foi considerado o limite da ferramenta: 5;')
  }
  else if(s>5){
    s<-5
    warning_vector <<- c(warning_vector,'O número de casas decimais dado é maior que 5, foi considerado o limite da ferramenta: 5;')
  }
  stp <- 10^(-s)
  
  stpint <- round(abs(env_iteracoes))
  if(env_iteracoes<0){
    warning_vector <<- c(warning_vector,'O máximo de iterações não pode ser negativo, foi utilizado o normal;')
  }
  if(round(env_iteracoes)!=env_iteracoes){
    warning_vector <<- c(warning_vector,'O máximo de iterações não pode decimal, foi arredondado;')
  }
  if(stpint==0){
    stpint <- 15
    warning_vector <<- c(warning_vector,'O máximo de iterações dado é 0, foi considerado o limite da ferramenta: 15;')
  }
  else if(stpint>15){
    stpint <- 15
    warning_vector <<- c(warning_vector,'O máximo de iterações dado é maior que 15, foi considerado o limite da ferramenta: 15;')
  }
  
  tryCatch({
    # Criando strings de entrada
    func <- paste("func <- function(x){",f,"}")
    func2 <- paste("func2 <- expression(",f,")")
    
    # Transformando os textos salvos na variavel ftext em uma expressao
    eval(parse(text=func))
    eval(parse(text=func2))
    
    ##=== Criando a funcao derivada primeira da funcao de entrada
    tryCatch({
      DevFunc <- function(x){eval(D(func2,"x"))}
    },
    warning = function(w){
      if(is.null(error_vector)){
        error_vector <<- c(error_vector,'Não foi possível calcular a derivada da função;')
      }
      return(NULL)
    },
    error = function(e){
      if(is.null(error_vector)){
        error_vector <<- c(error_vector,'Não foi possível calcular a derivada da função;')
      }
      return(NULL)
    })
    
    tryCatch({
      f_x0 <- func(x0)     # valor da F(x0)
      Dev_x0 <-DevFunc(x0)   #valor da F'(x0)
    },
    warning = function(w){
      if(is.null(error_vector)){
        error_vector <<- c(error_vector,'Não foi possível calcular o ponto na função ou derivada;')
      }
      return(NULL)
    },
    error = function(e){
      if(is.null(error_vector)){
        error_vector <<- c(error_vector,'Não foi possível calcular o ponto na função ou derivada;')
      }
      return(NULL)
    })
    
    ### Erro caso a derivada seja igual a zero
    #TODO: Avaliar se não tem que ser tendendo a zero.
    if(Dev_x0 == 0){
      error_vector <<- c(error_vector,'A derivada do ponto \'x0\' é igual a 0 ')
    }
    
    ### Garantindo que os pre-requisitos estao sendo seguidos exibindo janela de erro
    # Erro caso o ponto dado ja satisfaz o criterio de parada
    if(abs(func(x0))<stp){
      error_vector <<- c(error_vector,'O ponto \'x0\' já satisfaz o critério de parada')
    }
    else if(is.null(error_vector)){
      
      # Criando vetores para o plot
      x_k <- c()
      fx_k <- c()
      # Preenchendo os vetores
      a<-1/0
      x_k[1] <- x0
      x_k[2] <- x0 - f_x0/Dev_x0
      fx_k[1] <- f_x0
      fx_k[2] <- func(x_k[2])
      
      # contador de indices para while
      cont <- 2
      whilevar <- -1
      
      tryCatch({
        while(whilevar == -1){
          cont <- cont + 1
          x_k[cont] <- (x_k[cont-1] - ((func(x_k[cont-1]))/(DevFunc(x_k[cont-1]))))
          fx_k[cont] <- func(x_k[cont])
          
          if(cont>=stpint){whilevar <- 1}
          if(abs(x_k[cont]-x_k[cont-1])<stp){whilevar <- 1}
          if(abs(fx_k[cont])<stp){whilevar <- 1}
        }
      },
      warning = function(w){
        if(is.null(error_vector)){
          error_vector <<- c(error_vector,'Não foi possível calcular os pontos na função ou a derivada é zero;')
        }
        return(NULL)
      },
      error = function(e){
        if(is.null(error_vector)){
          error_vector <<- c(error_vector,'Não foi possível calcular os pontos na função ou a derivada é zero;')
        }
        return(NULL)
      })
      
      ### PLOT
      x_min <- min(x_k)
      x_max <- max(x_k)
      y_min <- 0
      y_max <- 0
      
      if(abs(x_max-x_min)<=1000){by_for = 0.05}
      else{by_for = 0.001}
      
      tryCatch({
        for (x in seq(x_min,x_max,by=by_for)){
          if (func(x) < y_min){
            y_min <- func(x)
          }
          if(func(x) > y_max){
            y_max <- func(x)
          }
        }
      },
      warning = function(w){
        if(is.null(error_vector)){
          error_vector <<- c(error_vector,'Não foi possível calcular o ponto na função;')
        }
        return(NULL)
      },
      error = function(e){
        if(is.null(error_vector)){
          error_vector <<- c(error_vector,'Não foi possível calcular o ponto na função;')
        }
        return(NULL)
      })
      
      h_ind <- abs(y_max-y_min)*0.04
      h_x <- abs(x_max-x_min)*0.05
      
      p <- ggplot(data = data.frame(x = 0,y = 0), mapping = aes(x = x, y = y)) + xlim(x_min-h_x,x_max+h_x)
      p <- p + stat_function(fun=func, col = "red")+ geom_vline(xintercept = 0) + geom_hline(yintercept = 0)
      plot_vector <<- list(p)
      
      for (i in 1:cont){
        
        p <- plot_vector[[length(plot_vector)]] + geom_point(x=x_k[i],y=0,col="blue",shape=1)
        
        if(g_indices){
          if(i==1){
            p <- p + annotate("text",label='x0',x=x_k[i],y=h_ind,col="blue")
          }
          else{
            p <- p + annotate("text",label=toString(i-1),x=x_k[i],y=h_ind,col="blue")
          }
        }
        
        plot_vector[[length(plot_vector)+1]] <<- p
        
        if(g_lv){
          plot_vector[[length(plot_vector)+1]] <<- plot_vector[[length(plot_vector)]] + geom_segment(x=x_k[i],xend=x_k[i],y=0,yend=fx_k[i],col="darkgray",linetype="dashed")
        }
        
        plot_vector[[length(plot_vector)+1]]<<- plot_vector[[length(plot_vector)]] + geom_point(x=x_k[i],y=fx_k[i], col="green", shape=1)
        
        if(g_ltg){
          plot_vector[[length(plot_vector)+1]] <<- plot_vector[[length(plot_vector)]] + geom_segment( x= x_k[i],xend=x_k[i+1],y=fx_k[i],yend =0,col = "black")
        }
      }
      value_output <<- list()
      value_output[[1]] <<-  paste("Aproximacoes: ",paste0(x_k, collapse =" | "))
      
      
    }
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