inter_funcao <- function(env_ponto_aprox,env_pontos_x,env_funcao,g_indices,g_lv){
  ### vetor de erro
  error_vector <<-c()
  
  ### Valores de entrada
  valaprm <- as.numeric(env_ponto_aprox)
  xentr <- env_pontos_x
  f<-env_funcao 
  
  func <- paste("func <- function(x){",f,"}")
  eval(parse(text=func))
  
  valxaux <- as.list(strsplit(xentr," ")[[1]])
  contval <- length(valxaux)
  valx <- c()
  for(i in 1:contval){
    valx[i] <- as.numeric(valxaux[i])
  }
  
  valy<-c()
  for(i in 1:contval){
    valy[i] <- func(valx[i])
  }
  
  if(contval<2){
    error_vector <<- c(error_vector,'Foi passado apenas um ponto')
  }
  else if(is.null(error_vector)){
    ### Criação dos elementos Lagrange
    lagrange <- rep(1,contval) # encher o vetor de 1, elemento neutro na multipl.
    for(j in 1:contval){ # for que determina o grau do polinomio
      for(i in 1:contval){ # for para fazer cada L
        if(i!=j){ # If para n?o acontecer de de i==j
          lagrange[j] <- paste0(lagrange[j], "*", "(x -",valx[i],")/(", valx[j]-valx[i],")")
        }
      }
    }
    
    ### Criação da função polinomio
    polinomio <- 0
    for(i in 1:contval){
      polinomio <- paste(polinomio, "+",valy[i],"*",lagrange[i])
    }
    
    f_text <- paste("PolLagrange<- function(x){",polinomio,"}")# Criando string de entrada
    eval(parse(text=f_text))# Transformando o texto salvo na variavel ftext em uma expressao
    
    Valaprmy <- PolLagrange(valaprm) # Valor no polinomio do ponto a ser aproximado
    valrealy <- func(valaprm) # Valor na funcao do ponto a ser aproximado
    
    #=Pontos maximos e minimos
    x_min <- min(valx)
    x_max <- max(valx)
    
    h_x <- abs(x_max-x_min)*0.05
    
    p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + xlim(x_min-h_x,x_max+h_x) + xlab("Eixo x") + ylab("Eixo y")
    p <- p + stat_function(fun = func, col = "black")
    
    plot_vector <<- list (p)
    
    for(i in 1:contval){  #Plot dos pontos na f(x)
      p <- p + geom_point(x=valx[i],y=valy[i],col='blue',pch=1)
    }
    
    ### Plot dos indices
    if(g_indices){
      for(i in 1:contval){
        p <- p + annotate("text",label= toString(i),x=valx[i],y=valy[i],col='blue')
      }
    }
    ### Plot das linhas verticais
    if(g_lv){
      for (i in 1:(contval)){
        p <- p + geom_segment(x=valx[i],xend=valx[i],y=0,yend=valy[i], col= "azure4", linetype='dashed')
      }
    }
    
    plot_vector[[2]] <<- p
    
    ### Plot do polinomio
    plot_vector[[3]] <<- plot_vector[[2]] + stat_function(fun = PolLagrange, col = "red")
    
    ### Plot do ponto aproximado no polinomio
    p <- plot_vector[[3]] + geom_point(x=valaprm, y= Valaprmy, col="chartreuse4", pch=9) 
    ### Plot do ponto aproximado na função
    p <- p + geom_point(x=valaprm,y= valrealy, col="chartreuse4", pch=9) 
    
    # if(g_indices){
    #   p <- p + annotate("text",label=toString(Valaprmy),x=valaprm,y=Valaprmy, col="chartreuse4")+ annotate("text", label = toString(valrealy),x=valaprm,y=valrealy, col="chartreuse4")
    # }
    
    plot_vector[[4]] <<- p + geom_segment(x=valaprm,xend=valaprm,y=Valaprmy,yend= valrealy, col = "chartreuse4")
    
    ### Resultados
    valerro <- (abs(Valaprmy - valrealy))
    value_output <<- list()
    value_output[[1]] <<- paste0("O valor achado pelo método é: ",Valaprmy, " | O valor absoluto do erro é: ",valerro)
  }
}