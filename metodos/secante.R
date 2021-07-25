secante <- function(env_funcao, env_pontos_1, env_pontos_2, env_iteracoes, env_decimais, g_indices, g_lv, g_sc) {
  ### vetor de erro
  error_vector <<- c()
  warning_vector <<- c()
  
  ### Valores de entrada
  f <- env_funcao
  
  s <- round(abs(env_decimais))
  if (env_decimais < 0) {
    warning_vector <<- c(warning_vector, "O número de casas decimais não pode ser negativo, foi utilizado o normal;")
  }
  if (round(env_decimais) != env_decimais) {
    warning_vector <<- c(warning_vector, "O número de casas decimais não pode decimal, foi arredondado;")
  }
  if (s == 0) {
    s <- 5
    warning_vector <<- c(warning_vector, "O número de casas decimais dado é 0, foi considerado o limite da ferramenta: 5;")
  }
  else if (s > 5) {
    s <- 5
    warning_vector <<- c(warning_vector, "O número de casas decimais dado é maior que 5, foi considerado o limite da ferramenta: 5;")
  }
  stp <- 10^ (- s)
  
  stpint <- round(abs(env_iteracoes))
  if (env_iteracoes < 0) {
    warning_vector <<- c(warning_vector, "O máximo de iterações não pode ser negativo, foi utilizado o normal;")
  }
  if (round(env_iteracoes) != env_iteracoes) {
    warning_vector <<- c(warning_vector, "O máximo de iterações não pode decimal, foi arredondado;")
  }
  if (stpint == 0) {
    stpint <- 15
    warning_vector <<- c(warning_vector, "O máximo de iterações dado é 0, foi considerado o limite da ferramenta: 15;")
  }
  else if (stpint > 15) {
    stpint <- 15
    warning_vector <<- c(warning_vector, "O máximo de iterações dado é maior que 15, foi considerado o limite da ferramenta: 15;")
  }
  
  # Intervalo
  input_points <- c(env_pontos_1, env_pontos_2)
  x0 <- min(input_points)
  x1 <- max(input_points)
  
  if (x0 != env_pontos_1) {
    warning_vector <<- c(warning_vector, "Foi dado que x0 < x1, portanto, os pontos foram invertidos para que x0 > x1")
  }
  tryCatch({
    func <- paste("func <- function(x) {", f, "}") # Criando string de entrada
    eval(parse(text = func))# Transformando o texto salvo na variavel ftext em uma expressao
    
    tryCatch({
      f_x0 <- func(x0)
      f_x1 <- func(x1)
    },
    warning = function(w) {
      error_vector <<- c(error_vector, "Não foi possível calcular o ponto na função;")
      return(NULL)
    },
    error = function(e) {
      error_vector <<- c(error_vector, "Não foi possível calcular o ponto na função;")
      return(NULL)
    })
    
    ### Erro caso o ponto x0 dado ja satisfaz o criterio de parada
    if (abs(f_x0) < stp) {
      error_vector <<- c(error_vector, "O ponto \"x0\" já satisfaz o critério de parada")
    }
    ### Erro caso o ponto "b" dado ja satisfaz o criterio de parada
    if (abs(f_x1) < stp) {
      error_vector <<- c(error_vector, "O ponto \"x1\" já satisfaz o critério de parada")
    }
    ### Erro caso x1 - x0 já satisfaça o critério de parada
    else if (abs(x0 - x1) < stp) {
      error_vector <<- c(error_vector, "|x0-x1| já satisfaz o critério de parada")
    }
    
    else if (is.null(error_vector)) {
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
      
      whileaux <- - 1 #teste para fazer ate chegar ao erro desejado
      
      tryCatch({
        while (whileaux == -1) {
          cont <- cont + 1
          x_k[cont] <- (x_k[cont - 1] - ((fx_k[cont - 1]) * ((x_k[cont - 1] - x_k[cont - 2]) / (fx_k[cont - 1] - fx_k[cont - 2]))))
          fx_k[cont] <- func(x_k[cont])
          
          errosec <- ((x_k[cont] - x_k[cont - 1]) / (x_k[cont]))
          if (abs(errosec * x_k[cont]) < stp) {
            whileaux <- 1
          }
          if (cont>stpint + 1) {
            whileaux <- 1
          }
          if (abs(fx_k[cont])<stp) {
            whileaux <- 1
          }
        }
      },
      warning = function(w) {
        if (is.null(error_vector)) {
          error_vector <<- c(error_vector, "Erro desconhecido;")
        }
        return(NULL)
      },
      error = function(e) {
        if (is.null(error_vector)) {
          error_vector <<- c(error_vector, "Erro desconhecido;")
        }
        return(NULL)
      })
      
      x_min <- min(x_k)
      x_max <- max(x_k)
      y_min <- 0
      y_max <- 0
      
      if (abs(x_max - x_min) <= 1000) {
        by_for <- 0.05
      }
      else{
        by_for <- 0.001
      }
      
      tryCatch({
        for (x in seq(x_min, x_max, by = by_for)) {
          if (func(x) < y_min) {
            y_min <- func(x)
          }
          if (func(x) > y_max) {
            y_max <- func(x)
          }
        }
      },
      warning = function(w) {
        if (is.null(error_vector)) {
          error_vector <<- c(error_vector, "Não foi possível calcular o ponto na função;")
        }
        return(NULL)
      },
      error = function(e) {
        if (is.null(error_vector)) {
          error_vector <<- c(error_vector, "Não foi possível calcular o ponto na função;")
        }
        return(NULL)
      })
      
      h_ind <- abs(y_max - y_min) * 0.04
      h_x <- abs(x_max - x_min) * 0.05
      
      p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
        xlim(x_min - h_x, x_max + h_x) + xlab("Eixo x") + ylab("Eixo y")
      p <- p + stat_function(fun = func, col = "red")
      
      plot_vector <<- list(p)
      
      if (g_indices) {
        p <- p + annotate("text", label = "0", x = x_k[1], y = h_ind, col = "blue")
      }
      p <- p + geom_point(x = x_k[1], y = 0, col= "blue", pch = 1)
      plot_vector[[2]] <<- p
      
      if (g_lv) {
        p <- p + geom_segment(x = x_k[1], xend = x_k[1], y = 0, yend = fx_k[1], col = "azure4", linetype ="dashed")
      }
      plot_vector[[3]] <<- p + geom_point(x = x_k[1], y = fx_k[1], col = "green", pch = 1)
      
      for (i in 2:(cont)) {
        plot_vector[[length(plot_vector)+1]] <<- plot_vector[[length(plot_vector)]] + geom_point(x = x_k[i], y = 0, col = "blue", pch = 1)
        
        if (g_lv) {
          plot_vector[[length(plot_vector)+1]] <<- plot_vector[[length(plot_vector)]] +
            geom_segment(x = x_k[i], xend = x_k[i], y = 0, yend = fx_k[i], col = "azure4", lty = 2)
        }
        
        p <-  plot_vector[[length(plot_vector)]] + geom_point(x = x_k[i], y = fx_k[i], col = "green", pch = 1)
        if (g_indices) {
          p <- p + annotate("text", label = i - 1, x = x_k[i], y = h_ind, col = "blue")
        }
        
        plot_vector[[length(plot_vector) + 1]] <<- p
        if (g_sc && i != cont) {
          if ((fx_k[i - 1] * fx_k[i]) < 0)
            p <- geom_segment(x = x_k[i - 1], xend = x_k[i], y = fx_k[i - 1], yend = fx_k[i], col = "yellow")
          else{
            if (abs(fx_k[i - 1]) < abs(fx_k[i])) {
              p <- geom_segment(x = x_k[i], xend = x_k[i + 1], y = fx_k[i], yend = 0, col = "yellow")
            }
            else{
              p <- geom_segment(x = x_k[i - 1], xend = x_k[i + 1], y = fx_k[i - 1], yend = 0, col = "yellow")
            }
          }
          plot_vector[[length(plot_vector) + 1]] <<- plot_vector[[length(plot_vector)]] + p
        }
      }
      value_output <<- list()
      value_output[[1]] <<- paste("Aproximações: ", paste0(x_k, collapse = " | "))
    }
  },
  warning = function(w) {
    if (is.null(error_vector)) {
      error_vector <<- c(error_vector, "Erro desconhecido;")
    }
    return(NULL)
  },
  error = function(e) {
    if (is.null(error_vector)) {
      error_vector <<- c(error_vector, "Erro desconhecido;")
    }
    return(NULL)
  })
}
