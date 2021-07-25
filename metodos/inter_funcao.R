inter_funcao <- function(env_ponto_aprox, env_pontos_x, env_funcao, g_indices, g_lv) {
  ### vetor de erro
  error_vector <<- c()
  warning_vector <<- c()
  
  ### Valores de entrada
  valaprm <- env_ponto_aprox
  xentr <- env_pontos_x
  f <- env_funcao
  
  func <- paste("func <- function(x) {", f, "}")
  eval(parse(text = func))
  
  valxaux <- as.list(strsplit(xentr, " ")[[1]])
  contval <- length(valxaux)
  valx <- c()
  for (i in 1:contval) {
    valx[i] <- as.numeric(valxaux[i])
  }
  if (!all(sort(valx) == valx)) {
    warning_vector <<- c(warning_vector, "Os pontos em x não estão em ordem crescente. Verifique se o plot está conforme o esperado;")
  }
  
  tryCatch({
    valy <- c()
    for (i in 1:contval) {
      valy[i] <- func(valx[i])
    }
  },
  warning = function(w) {
    error_vector <<- c(error_vector, "Não é possível calcular o ponto na função;")
    return(NULL)
  },
  error = function(e) {
    error_vector <<- c(error_vector, "Não é possível calcular o ponto na função;")
    return(NULL)
  })
  
  if (is.null(error_vector)) {
    tryCatch({
      ### Criação dos elementos Lagrange
      lagrange <- rep(1, contval) # encher o vetor de 1, elemento neutro na multipl.
      for (j in 1:contval) { # for que determina o grau do polinomio
        for (i in 1:contval) { # for para fazer cada L
          if (i != j) { # If para n?o acontecer de de i==j
            lagrange[j] <- paste0(lagrange[j], "*", "(x -", valx[i], ")/(", valx[j] - valx[i], ")")
          }
        }
      }
      
      ### Criação da função polinomio
      polinomio <- 0
      for (i in 1:contval) {
        polinomio <- paste(polinomio, "+", valy[i], "*", lagrange[i])
      }
      
      f_text <- paste("pol_lagrange<- function(x) {", polinomio, "}")# Criando string de entrada
      eval(parse(text = f_text))# Transformando o texto salvo na variavel ftext em uma expressao
      
      valaprmy <- pol_lagrange(valaprm) # Valor no polinomio do ponto a ser aproximado
      valrealy <- func(valaprm) # Valor na funcao do ponto a ser aproximado
      
      #=Pontos maximos e minimos
      x_min <- min(valx)
      x_max <- max(valx)
      if (valaprm < x_min) {
        x_min <- valaprm
      }
      else if (valaprm > x_max) {
        x_max <- valaprm
      }
      
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
          if (pol_lagrange(x) < y_min) {
            y_min <- pol_lagrange(x)
          }
          if (func(x) < y_min) {
            y_min <- func(x)
          }
          if (pol_lagrange(x) > y_max) {
            y_max <- pol_lagrange(x)
          }
          if (func(x) > y_max) {
            y_max <- func(x)
          }
        }
      },
      warning = function(w) {
        error_vector <<- c(error_vector, "Não é possível calcular o ponto na função;")
        return(NULL)
      },
      error = function(e) {
        error_vector <<- c(error_vector, "Não é possível calcular o ponto na função;")
        return(NULL)
      })
      
      h_ind <- abs(y_max - y_min) * 0.04
      h_x <- abs(x_max - x_min) * 0.05
      
      p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + geom_vline(xintercept = 0) +
        geom_hline(yintercept = 0) + xlim(x_min - h_x, x_max + h_x) + xlab("Eixo x") + ylab("Eixo y")
      p <- p + stat_function(fun = func, col = "black")
      
      plot_vector <<- list(p)
      
      for (i in 1:contval) {  #Plot dos pontos na f(x)
        p <- p + geom_point(x = valx[i], y = valy[i], col = "blue", pch = 1)
      }
      
      ### Plot das linhas verticais
      if (g_lv) {
        for (i in 1:(contval)) {
          p <- p + geom_segment(x = valx[i], xend = valx[i], y = 0, yend = valy[i], col = "azure4", linetype = "dashed")
        }
      }
      ### Plot dos indices
      if (g_indices) {
        for (i in 1:contval) {
          p <- p + annotate("text", label = toString(i), x = valx[i], y = valy[i] + h_ind, col = "blue")
        }
      }
      
      plot_vector[[2]] <<- p
      
      ### Plot do polinomio
      plot_vector[[3]] <<- plot_vector[[2]] + stat_function(fun = pol_lagrange, col = "red")
      
      ### Plot do ponto aproximado no polinomio
      p <- plot_vector[[3]] + geom_point(x = valaprm, y = valaprmy, col = "chartreuse4", pch = 9)
      ### Plot do ponto aproximado na função
      p <- p + geom_point(x = valaprm, y = valrealy, col = "chartreuse4", pch = 9)
      
      plot_vector[[4]] <<- p + geom_segment(x = valaprm, xend = valaprm, y = valaprmy, yend = valrealy, col = "chartreuse4")
      
      ### Resultados
      valerro <- (abs(valaprmy - valrealy))
      value_output <<- list()
      value_output[[1]] <<- paste0("O valor achado pelo método é: ", valaprmy, " | O erro absoluto no ponto é: ", valerro)
      
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
}
