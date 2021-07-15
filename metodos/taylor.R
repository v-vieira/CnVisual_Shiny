taylor<- function(env_funcao,env_ponto_input,env_ponto_aprox,env_graus,g_offset){
  ### vetor de erro
  error_vector <<-c()
  
  ### Valores de entrada
  f<-env_funcao
  valapr<-as.numeric(env_ponto_input)
  valenv <- as.numeric(env_ponto_aprox)
  
  offset <- abs(g_offset)
  if(offset==0){
    error_vector <<- append(error_vector,'O valor de offset deve ser diferente de 0')
  }
  
  else{
    x_min = valenv-offset
    x_max = valenv+offset
    
    
    # Criando strings de entrada
    func <- paste("func <- function(x){",f,"}")
    func2 <- paste("func2 <- expression(",f,")")
    
    # Transformando o texto em uma expressao
    eval(parse(text=func))
    eval(parse(text=func2))
    
    ### Funções derivadas
    DevFunc1 <- function(x){eval(D(func2,"x"))}
    DevFunc2 <- function(x){eval(D(D(func2,"x"),"x"))}
    DevFunc3 <- function(x){eval(D(D(D(func2,"x"),"x"),"x"))}
    DevFunc4 <- function(x){eval(D(D(D(D(func2,"x"),"x"),"x"),"x"))}
    DevFunc5 <- function(x){eval(D(D(D(D(D(func2,"x"),"x"),"x"),"x"),"x"))}
    
    
    ### valores utilizados na f^n
    fnval <- c()
    fnval[1] <- DevFunc1(valapr)
    fnval[2] <- DevFunc2(valapr)
    fnval[3] <- DevFunc3(valapr)
    fnval[4] <- DevFunc4(valapr)
    fnval[5] <- DevFunc5(valapr)
    
    # Vetor que recebera cada polinomio
    fnpol <- c()
    for(i in 1:5){
      if(i!=1){
        fnpol[i] <- paste0(fnpol[i-1],"+(",(fnval[i]/(factorial(i))),")*((x-",valapr,")^",i,")")
      }
      else{
        fnpol[i] <- paste0(func(valapr),"+(",(fnval[i]/(factorial(i))),")*((x-",valapr,")^",i,")")
      }
    }
    
    ### Plot da função
    p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +xlim(x_min,x_max)+ xlab("Eixo x") + ylab("Eixo y")
    p <- p + stat_function(fun = func, col = "black")
    
    plot_vector <<- list(p)
    #TODO: Colocar legenda dos graus
    #legend(limitx[2] +(limitx[2]-limitx[1])*0.06, func(limitx[2]), c("Grau 1", "Grau 2", "Grau 3", "Grau 4", "Grau 5"),col = c("chartreuse3", "aquamarine3", "coral3", "deeppink2","midnightblue"),cex = 0.8,lwd = 1, lty = 1)
    
    ### Plot dos polinomios de taylor
    
    # Caso o grau nao seja marcado começar como nulo
    resulpol1 <- ""
    resulpol2 <- ""
    resulpol3 <- ""
    resulpol4 <- ""
    resulpol5 <- ""
    i = 2
    if(1 %in% env_graus){
      Fpoli <- fnpol[1]
      poli1 <- paste("poli1 <-function(x){",Fpoli,"}")
      eval(parse(text=poli1))
      p <- p + stat_function(fun=poli1, col= "chartreuse3")
      p <- p + geom_point(x=valenv,y=poli1(valenv),col='blue',pch=1)
      plot_vector[[i]] <<- p
      resulpol1 <- paste0("\n","No grau 1: ",poli1(valenv))
      i <- i + 1
    }
    if(2 %in% env_graus){
      Fpoli <- fnpol[2]
      poli2 <- paste("poli2 <-function(x){",Fpoli,"}")
      eval(parse(text=poli2))
      p <- p + stat_function(fun=poli2, col= "aquamarine3")
      p <- p + geom_point(x=valenv,y=poli2(valenv),col='blue',pch=1)
      plot_vector[[i]] <<- p
      resulpol2 <- paste0("\n","No grau 2: ",poli2(valenv))
      i <- i + 1
    }
    if(3 %in% env_graus){
      Fpoli <- fnpol[3]
      poli3 <- paste("poli3 <-function(x){",Fpoli,"}")
      eval(parse(text=poli3))
      p <- p + stat_function(fun=poli3, col= "coral3")
      p <- p + geom_point(x=valenv,y=poli3(valenv),col='blue',pch=1)
      plot_vector[[i]] <<- p
      resulpol3 <- paste0("\n","No grau 3: ",poli3(valenv))
      i <- i + 1
    }
    if(4 %in% env_graus){
      Fpoli <- fnpol[4]
      poli4 <- paste("poli4 <-function(x){",Fpoli,"}")
      eval(parse(text=poli4))
      p <- p + stat_function(fun=poli4, col= "deeppink2")
      p <- p + geom_point(x=valenv,y=poli4(valenv),col='blue',pch=1)
      plot_vector[[i]] <<- p
      resulpol4 <- paste0("\n","No grau 4: ",poli4(valenv))
      i <- i + 1
    }
    if(5 %in% env_graus){
      Fpoli <- fnpol[5]
      poli5 <- paste("poli5 <-function(x){",Fpoli,"}")
      eval(parse(text=poli5))
      p <- p + stat_function(fun=poli5, col= "midnightblue")
      p <- p + geom_point(x=valenv,y=poli5(valenv),col='blue',pch=1)
      plot_vector[[i]] <<- p
      resulpol5 <- paste0("\n","No grau 5: ",poli5(valenv))
      i <- i + 1
    }
    
    ### Plot do ponto na função
    resul <- func(valenv)
    plot_vector[[i]] <<- plot_vector[[i-1]] + geom_point(x=valenv,y=resul,pch=3) 
    
    ### Resultadoa
    value_output <<- list()
    value_output[[1]] <<- paste0("O valor na funcao: ",resul,resulpol1,resulpol2,resulpol3,resulpol4,resulpol5)
  }
}