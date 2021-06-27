taylor<- function(env_funcao,env_ponto_input,env_ponto_aprox,env_int_plot,env_graus){
  #== Valores de entrada
  f<-env_funcao
  valapr<-as.numeric(env_ponto_input)
  valenv <- as.numeric(env_ponto_aprox)
  limix <- env_int_plot
  
  #============== pegar os valores separados em x =======
  interaux <- as.list(strsplit(limix," ")[[1]])
  limitx <- c()
  for(i in 1:2){
    limitx[i] <- as.numeric(interaux[i])
  }
  
  x_min = min(limitx)
  x_max = max(limitx)
  
  
  # Criando strings de entrada
  func <- paste("func <- function(x){",f,"}")
  func2 <- paste("func2 <- expression(",f,")")
  
  # Transformando o texto em uma expressao
  eval(parse(text=func))
  eval(parse(text=func2))
  
  #=============== Funcoes derivadas===============
  DevFunc1 <- function(x){eval(D(func2,"x"))}
  DevFunc2 <- function(x){eval(D(D(func2,"x"),"x"))}
  DevFunc3 <- function(x){eval(D(D(D(func2,"x"),"x"),"x"))}
  DevFunc4 <- function(x){eval(D(D(D(D(func2,"x"),"x"),"x"),"x"))}
  DevFunc5 <- function(x){eval(D(D(D(D(D(func2,"x"),"x"),"x"),"x"),"x"))}
  
  
  #Vetor com o valores que serao uzados de f^n(a) que serao uzados no polinomio
  fnval <- c()
  fnval[1] <- DevFunc1(valapr)
  fnval[2] <- DevFunc2(valapr)
  fnval[3] <- DevFunc3(valapr)
  fnval[4] <- DevFunc4(valapr)
  fnval[5] <- DevFunc5(valapr)
  
  #Vetor que recebera cada polinomio
  fnpol <- c()
  for(i in 1:5){
    if(i!=1){
      fnpol[i] <- paste0(fnpol[i-1],"+(",(fnval[i]/(factorial(i))),")*((x-",valapr,")^",i,")")
    }
    else{
      fnpol[i] <- paste0(func(valapr),"+(",(fnval[i]/(factorial(i))),")*((x-",valapr,")^",i,")")
    }
  }
  
  #==== Plot do metodo
  #
  #visible(gg) <- TRUE #Agora a area grafica gg que ira receber o plot
  #par(xpd = T, mar = c(0,0,0.5,5)) #= margem
  #plot(func, limitx[1],limitx[2], col = "red", lwd= 3,xaxt='n',yaxt='n',ann=FALSE,bty='n') #Plot da f(x)
  p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +xlim(x_min,x_max)+ xlab("Eixo x") + ylab("Eixo y")
  p <- p + stat_function(fun = func, col = "black")
  
  plot_vector <<- list(p)
  #TODO: Colocar legenda
  #legend(limitx[2] +(limitx[2]-limitx[1])*0.06, func(limitx[2]), c("Grau 1", "Grau 2", "Grau 3", "Grau 4", "Grau 5"),col = c("chartreuse3", "aquamarine3", "coral3", "deeppink2","midnightblue"),cex = 0.8,lwd = 1, lty = 1)
  #abline(h=0, lty=2)
  #abline(v=0, lty=2)
  
  #======= Plot dos polinomios de taylor
  #
  #==== Caso o grau nao seja marcado come?ar como nulo
  resulpol1 <- ""
  resulpol2 <- ""
  resulpol3 <- ""
  resulpol4 <- ""
  resulpol5 <- ""
  #==============
  i = 2
  if(1 %in% env_graus){
    #===== Plot
    Fpoli <- fnpol[1]
    poli1 <- paste("poli1 <-function(x){",Fpoli,"}")
    eval(parse(text=poli1))
    p <- p + stat_function(fun=poli1, col= "chartreuse3")
    #===== Plot do ponto no polinomio
    #points(valenv, poli1(valenv), col="blue", pch = 1)
    p <- p + geom_point(x=valenv,y=poli1(valenv),col='blue',pch=1)
    plot_vector[[i]] <<- p
    #===== Colocar o resultado na janela
    resulpol1 <- paste0("\n","No grau 1: ",poli1(valenv))
    i <- i + 1
  }
  if(2 %in% env_graus){
    # Sys.sleep(speed)
    Fpoli <- fnpol[2]
    poli2 <- paste("poli2 <-function(x){",Fpoli,"}")
    eval(parse(text=poli2))
    #curve(poli2, type="l", add=TRUE, col ="aquamarine3")
    p <- p + stat_function(fun=poli2, col= "aquamarine3")
    #points(valenv, poli2(valenv), col="blue", pch = 1)
    p <- p + geom_point(x=valenv,y=poli2(valenv),col='blue',pch=1)
    plot_vector[[i]] <<- p
    resulpol2 <- paste0("\n","No grau 2: ",poli2(valenv))
    i <- i + 1
  }
  if(3 %in% env_graus){
    # Sys.sleep(speed)
    Fpoli <- fnpol[3]
    poli3 <- paste("poli3 <-function(x){",Fpoli,"}")
    eval(parse(text=poli3))
    #curve(poli3, type="l", add=TRUE, col="coral3")
    p <- p + stat_function(fun=poli3, col= "coral3")
    #points(valenv, poli3(valenv), col="blue", pch = 1)
    p <- p + geom_point(x=valenv,y=poli3(valenv),col='blue',pch=1)
    plot_vector[[i]] <<- p
    resulpol3 <- paste0("\n","No grau 3: ",poli3(valenv))
    i <- i + 1
  }
  if(4 %in% env_graus){
    # Sys.sleep(speed)
    Fpoli <- fnpol[4]
    poli4 <- paste("poli4 <-function(x){",Fpoli,"}")
    eval(parse(text=poli4))
    #curve(poli4, type="l", add=TRUE, col="deeppink2")
    p <- p + stat_function(fun=poli4, col= "deeppink2")
    #points(valenv, poli4(valenv), col="blue", pch = 1)
    p <- p + geom_point(x=valenv,y=poli4(valenv),col='blue',pch=1)
    plot_vector[[i]] <<- p
    resulpol4 <- paste0("\n","No grau 4: ",poli4(valenv))
    i <- i + 1
  }
  if(5 %in% env_graus){
    #Sys.sleep(speed)
    Fpoli <- fnpol[5]
    poli5 <- paste("poli5 <-function(x){",Fpoli,"}")
    eval(parse(text=poli5))
    #curve(poli5, type="l", add=TRUE, col="midnightblue")
    p <- p + stat_function(fun=poli5, col= "midnightblue")
    #points(valenv, poli5(valenv), col="blue", pch = 1)
    p <- p + geom_point(x=valenv,y=poli5(valenv),col='blue',pch=1)
    plot_vector[[i]] <<- p
    resulpol5 <- paste0("\n","No grau 5: ",poli5(valenv))
    i <- i + 1
  }
  
  #==== Guardar o resultado na variavel e plotar
  resul <- func(valenv)
  plot_vector[[i]] <<- plot_vector[[i-1]] + geom_point(x=valenv,y=resul,pch=3) 
  #points(valenv, resul, pch=3)
  
  #Resultados a serem mostrados ao usuario
  value_output <<- list()
  value_output[[1]] <<- paste0("\n","O valor na funcao: ",resul,resulpol1,resulpol2,resulpol3,resulpol4,resulpol5)
  
}