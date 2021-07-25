taylor <- function(env_funcao, env_ponto_input, env_ponto_aprox, env_graus, g_offset) {
  ### vetor de erro
  error_vector <<- c()
  warning_vector <<- c()
  
  ### Valores de entrada
  f <- env_funcao
  valapr <- env_ponto_input
  valenv <- env_ponto_aprox
  
  offset <- abs(g_offset)
  if (g_offset < 0) {
    warning_vector <<- c(warning_vector, "O offset não pode ser negativo, foi utilizado o normal;")
  }
  else if (offset == 0) {
    warning_vector <<- append(error_vector, "O valor de offset deve ser diferente de 0, foi utilizado 1;")
  }
  else{
    tryCatch({
      x_min <- valenv - offset
      x_max <- valenv + offset
      
      # Criando strings de entrada
      func <- paste("func <- function(x) {", f, "}")
      func2 <- paste("func2 <- expression(", f, ")")
      
      # Transformando o texto em uma expressao
      eval(parse(text = func))
      eval(parse(text = func2))
      
      ### Funções derivadas
      tryCatch({
        dev_func1 <- function(x) {
          eval(D(func2, "x"))
        }
        dev_func2 <- function(x) {
          eval(D(D(func2, "x"), "x"))
        }
        dev_func3 <- function(x) {
          eval(D(D(D(func2, "x"), "x"), "x"))
        }
        dev_func4 <- function(x) {
          eval(D(D(D(D(func2, "x"), "x"), "x"), "x"))
        }
        dev_func5 <- function(x) {
          eval(D(D(D(D(D(func2, "x"), "x"), "x"), "x"), "x"))
        }
      },
      warning = function(w) {
        if (is.null(error_vector)) {
          error_vector <<- c(error_vector, "Não foi possível calcular alguma derivada da função;")
        }
        return(NULL)
      },
      error = function(e) {
        if (is.null(error_vector)) {
          error_vector <<- c(error_vector, "Não foi possível calcular alguma derivada da função;")
        }
        return(NULL)
      })
      
      ### valores utilizados na f^n
      fnval <- c()
      tryCatch({
        fnval[1] <- dev_func1(valapr)
        fnval[2] <- dev_func2(valapr)
        fnval[3] <- dev_func3(valapr)
        fnval[4] <- dev_func4(valapr)
        fnval[5] <- dev_func5(valapr)
      },
      warning = function(w) {
        if (is.null(error_vector)) {
          error_vector <<- c(error_vector, "Não foi possível calcular a derivada no ponto dado;")
        }
        return(NULL)
      },
      error = function(e) {
        if (is.null(error_vector)) {
          error_vector <<- c(error_vector, "Não foi possível calcular a derivada no ponto dado;")
        }
        return(NULL)
      })
      
      # Vetor que recebera cada polinomio
      fnpol <- c()
      for (i in 1:5) {
        if (i != 1) {
          fnpol[i] <- paste0(fnpol[i - 1], "+(", (fnval[i] / (factorial(i))), ")*((x-", valapr, ")^", i, ")")
        }
        else{
          fnpol[i] <- paste0(func(valapr), "+(", (fnval[i] / (factorial(i))), ")*((x-", valapr, ")^", i, ")")
        }
      }
      
      ### Plot da função
      p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
        xlim(x_min, x_max) + xlab("Eixo x") + ylab("Eixo y")
      p <- p + stat_function(fun = func, col = "black")
      
      plot_vector <<- list(p)
      
      ### Plot dos polinomios de taylor
      # Caso o grau nao seja marcado começar como nulo
      resulpol1 <- ""
      resulpol2 <- ""
      resulpol3 <- ""
      resulpol4 <- ""
      resulpol5 <- ""
      i <- 2
      if (1 %in% env_graus) {
        fpoli <- fnpol[1]
        poli1 <- paste("poli1 <-function(x) {", fpoli, "}")
        eval(parse(text = poli1))
        p <- p + stat_function(fun = poli1, col = "chartreuse3")
        p <- p + geom_point(x = valenv, y = poli1(valenv), col = "blue", pch = 1)
        plot_vector[[i]] <<- p
        resulpol1 <- paste0("\n", "No grau 1: ", poli1(valenv))
        i <- i + 1
      }
      if (2 %in% env_graus) {
        fpoli <- fnpol[2]
        poli2 <- paste("poli2 <-function(x) {", fpoli, "}")
        eval(parse(text = poli2))
        p <- p + stat_function(fun = poli2, col = "aquamarine3")
        p <- p + geom_point(x = valenv, y = poli2(valenv), col = "blue", pch = 1)
        plot_vector[[i]] <<- p
        resulpol2 <- paste0("\n", "No grau 2: ", poli2(valenv))
        i <- i + 1
      }
      if (3 %in% env_graus) {
        fpoli <- fnpol[3]
        poli3 <- paste("poli3 <-function(x) {", fpoli, "}")
        eval(parse(text = poli3))
        p <- p + stat_function(fun = poli3, col = "coral3")
        p <- p + geom_point(x = valenv, y = poli3(valenv), col = "blue", pch = 1)
        plot_vector[[i]] <<- p
        resulpol3 <- paste0("\n", "No grau 3: ", poli3(valenv))
        i <- i + 1
      }
      if (4 %in% env_graus) {
        fpoli <- fnpol[4]
        poli4 <- paste("poli4 <-function(x) {", fpoli, "}")
        eval(parse(text = poli4))
        p <- p + stat_function(fun = poli4, col = "deeppink2")
        p <- p + geom_point(x = valenv, y = poli4(valenv), col = "blue", pch = 1)
        plot_vector[[i]] <<- p
        resulpol4 <- paste0("\n", "No grau 4: ", poli4(valenv))
        i <- i + 1
      }
      if (5 %in% env_graus) {
        fpoli <- fnpol[5]
        poli5 <- paste("poli5 <-function(x) {", fpoli, "}")
        eval(parse(text = poli5))
        p <- p + stat_function(fun = poli5, col = "midnightblue")
        p <- p + geom_point(x = valenv, y = poli5(valenv), col = "blue", pch = 1)
        plot_vector[[i]] <<- p
        resulpol5 <- paste0("\n", "No grau 5: ", poli5(valenv))
        i <- i + 1
      }
      
      ### Plot do ponto na função
      resul <- func(valenv)
      plot_vector[[i]] <<- plot_vector[[i - 1]] + geom_point(x = valenv, y = resul, pch = 3)
      
      ### Resultadoa
      value_output <<- list()
      value_output[[1]] <<- paste0("O valor na função: ", resul, resulpol1, resulpol2, resulpol3, resulpol4, resulpol5)
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
