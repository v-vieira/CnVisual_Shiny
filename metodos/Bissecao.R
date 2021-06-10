#valuetextm <- "Aproximacoes obtidas"

#valores de entrada
#' env_funcao = Função de entrada | texto
#' env_pontos = | texto
#' env_decimais = Numero de casa de decimais de precisação | texto
#' env_iteracões = Numero maximom de iterações | texto
#' env_veloc_animm = Velocidade de animação | texto
#' g_indices = indice dos graficos | Logico
#' g_lv = plot das linhas verticais | Logico
#' 

#==== Funcao principal (que faz o metodo)
bissection <- function(env_funcao,env_pontos,env_decimais,env_iteracoes,g_indices,g_lh)
{
  #= [[1]] do plot
  
  
  #== Valores de entrada
  f<-env_funcao
  pointsentr<-env_pontos
  s<-as.numeric(env_decimais)
  stp <- 10^(-s)
  stpint <- as.numeric(env_iteracoes)
  if(stpint==0) stpint <- 999 #numero ilimitado de iteracoes
  
  
  #=== pegar os valores separados em x
  #
  valxaux <- as.list(strsplit(pointsentr," ")[[1]])
  contval <- length(valxaux) # contador da quantidade de valores de entrada
  #intervalo
  a0 <-as.numeric(valxaux[1])
  b0 <- as.numeric(valxaux[2])
  
  
  func <- paste("func <- function(x){",f,"}")# Criando string de entrada
  eval(parse(text=func))# Transformando o texto salvo na variavel ftext em uma expressao
  
  fa <- func(a0)
  fb <- func(b0)
  
  # Vetores para o plot
  a_k <- c()
  b_k <- c()
  fa_k <- c()
  fb_k <- c()
  m_k <- c()
  
  # Contador de indices para while
  cont <- 1
  
  # TODO: Implementar essa msg de erro
  #=== Garantindo que os pre-requisitos estao sendo seguidos exibindo janela de erro
  #
  # if(abs(fa)<stp){ # Erro caso o extremo inferior ja satisfaca o criterio de parada
  #   #== Criacao da janela de erro
  #   error.FAMINOR <- gwindow("Erro", width = 15)
  #   error.FAgt <- ggroup(horizontal = FALSE, container = error.FAMINOR)
  #   error.FAgb <- ggroup(horizontal = FALSE, container = error.FAMINOR)
  #   error.FAlabel <- glabel("O extremo inferior do intervalo ja satisfaz o criterio de parada", container=error.FAgt)
  #   exit.FA <-function(h,...){dispose(error.FAMINOR)}
  #   gbutton("Ok", cont= error.FAgb, handler = exit.FA)
  #   stop #Parar o codigo se a janela for criada
  # }
  
  # TODO: Implementar essa msg de erro
  # if(abs(fb)<stp){ #Erro caso o extremo superior ja satisfaca o criterio de parada
  #   #== Criacao da janela de erro
  #   error.FBMINOR <- gwindow("Erro", width = 15)
  #   error.FBgt <- ggroup(horizontal = FALSE, container = error.FBMINOR)
  #   error.FBgb <- ggroup(horizontal = FALSE, container = error.FBMINOR)
  #   error.FBlabel <- glabel("O extremo superior do intervalo ja satisfaz o criterio de parada", container=error.FBgt)
  #   exit.FB <-function(h,...){dispose(error.FBMINOR)}
  #   gbutton("Ok", cont= error.FBgb, handler = exit.FB)
  #   stop #Parar o codigo se a janela for criada
  # }
  
  #===Comeco do metodo em si
  #
  if((fa*fb)<0) # Garantir que o metodo so seja feito caso tenha um numero impar de raizes no intervalo dado
  {
    whilevar <- -1
    while(whilevar == -1) # Garantir que seja feito somente ate que o criterio do erro nao seja satisfeito
    {
      #==Atribuicao dos valores aos vetores
      a_k[cont] <- a0
      b_k[cont] <- b0
      fa_k[cont] <- func(a0)
      fb_k[cont] <- func(b0)
      m_k[cont] <- (a_k[cont]+b_k[cont])/2 #Atribuicao do ponto medio
      
      
      #== Definir qual sera o proximo a e b
      if(fa_k[cont]*func(m_k[cont])<0){
        b0 <- m_k[cont]
      }
      else {
        a0 <- m_k[cont]
      }
      
      #Parar o metodo pelo while
      if((cont)>=stpint){whilevar <-1}
      if(abs(b0-a0)<stp){whilevar <- 1}
      if(abs(func(m_k[cont]))<stp){whilevar <- 1}
      if(whilevar== -1) cont<- cont + 1 # Aumentar o indice do contador
    }
    
    #= Colocar os valores de entrada de volta em a0 e b0
    
    a0 <- as.numeric(valxaux[1])
    b0 <- as.numeric(valxaux[2])
    #==== Plot do metodo
    #
    
    
    #Pegar os valores maximos e minimos da funcao e forcar um valor minimo e maximo para o plot
    y_min <- optimize(func,interval = c(a_k[1]- 1,b_k[1]+ 1)) #y_min pega o valor que da o minimo em x e o valor em y
    y_min <- y_min$objective #y_min agora pega apenas o valor em y
    y_max <- optimize(func,interval = c(a_k[1]- 1,b_k[1]+ 1),maximum = TRUE)
    y_max <- y_max$objective
    absalt <- abs(y_max-y_min) #Altura total do plot
    
    # Definir uma altura minima nos extremos superiores e inferiores
    if(abs(y_min) <= 0.1*(absalt)) y_min<- -0.1*(absalt)
    if(abs(y_max) <= 0.1*(absalt)) y_max<- 0.1*(absalt)
    inter_y <- c(y_min,y_min) #vetor para da altura para o plot das linhas horizontais
    
    #visible(gg) <- TRUE #Agora a area grafica gg que ira receber o plot
    p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + xlim(a_k[1]- 1,b_k[1]+ 1)
    p <- p + stat_function(fun = func, col = "red") + geom_vline(xintercept = 0) + geom_hline(yintercept = 0) #Plot da f(x)
    #abline(h=0, lty=2)
    #abline(v=0, lty=2)
    plot_vector <<- list(p)
    
    
    z_k <- rep(0, cont) # Vetor de zeros do tamanho do vetor m_k
    
    #= Plot dos pontos a e b sobre o eixo x
    plot_vector[[2]] <<- p + geom_point(x=a_k[1],y=0, col="blue", pch = 1) + geom_point(x=b_k[1],y= 0, col="blue", pch = 1)+annotate("text",label="a",x=a_k[1],y=3)+annotate("text",label="b",x=b_k[1],y=3)
    #text(a_k[1],0,"a",cex=0.65, pos=3, col="blue")
    #text(b_k[1],0,"b",cex=0.65, pos=3, col="blue")
    
    #== Animacao
    #
    for (i in 1:cont) #Para cada iteracao
    {
      if(g_lh){ #linha horizontal
        k <- 3+2*(i-1)
        #Sys.sleep(speed/2) #tempo
        inter_x <-c(a_k[i],b_k[i])
        #lines(inter_x, inter_y, col="green4", lwd = i + (i-1)*1.2)
        #points(inter_x, inter_y,bg ="green4", col="black", pch = 21)
        
        #todo: COlocar bg nos pontos
        plot_vector[[k]] <<- plot_vector[[k-1]] + geom_segment(x=inter_x[1], xend=inter_x[2], y=inter_y[1]-1, yend=inter_y[2]-1,size=i + (i-1)*0.3,col="green4") +geom_point(x=inter_x[1],y=inter_y[1]-1,shape=21,bg="green4")+geom_point(x=inter_x[2],y=inter_y[2]-1,shape=21,bg="green4")
        #--------------------------------------------------------------
      }
      else{
        k <- i +2
      }
      
      #Sys.sleep(speed/2) #tempo
      
      

      if(g_indices){ #Indices dos pontos
        # Plot dos pontos m_k sobre o eixo x
        #TODO: Implementar texto dos indices e somar na linha abaixo
        plot_vector[[k+1]] <<- plot_vector[[k]] + geom_point(x=m_k[i],y=z_k[i], col="blue", shape = 1)+annotate("text",label=toString(i),x=m_k[i],y=3)
        #index <-c(0:(i-1))
        #text(m_k[1:i],z_k[1:i],index,cex=0.65, pos=3, col="blue")
      }
      else{
        # Plot dos pontos m_k sobre o eixo x
        plot_vector[[k+1]] <<- plot_vector[[k]] + geom_point(x=m_k[i],y=z_k[i], col="blue", shape = 1)
      }
    }
    
    # TODO: Implementar zoom
    # #=== Plot do zoom
    # #
    # x_max<- -999
    # x_min<- 999
    # print(cont)
    # #pegar apenas 4 ultimas iteracoes, ou apenas a que houver
    # if(cont>3){
    #   iz <- cont-3
    #   index <-c((cont-4):(cont-1))
    #   
    #   for (i in iz:(cont-1)) #averiguar ponto maximo e minimo com mais de 3 pontos
    #   {
    #     if(m_k[i] > x_max) x_max <- m_k[i]
    #     if(m_k[i] < x_min) x_min <- m_k[i]
    #   }
    #   
    # }
    # else {
    #   if(cont==1){
    #     iz<- 1
    #     index <-c(0)
    #     
    #     #colocar como pontos maximos os pontos de entrada
    #     x_max <- b_k[1]
    #     x_min <- a_k[1]
    #   }
    #   else{
    #     iz<- 1
    #     index <-c(0:(cont-1))
    #     
    #     for (i in iz:(cont-1)) #averiguar ponto maximo e minimo entre 1 e 3 pontos
    #     {
    #       if(m_k[i] > x_max) x_max <- m_k[i]
    #       if(m_k[i] < x_min) x_min <- m_k[i]
    #     }
    #     
    #   }
    # }
    # # TODO melhorar 
    # 
    # #Pegar os valores maximos e minimos da funcao e forcar um valor minimo e maximo para o plot
    # y_min <- optimize(func,interval = c(x_min,x_max)) #y_min pega o valor que da o minimo em x e o valor em y
    # y_min <- y_min$objective #y_min agora pega apenas o valor em y
    # y_max <- optimize(func,interval = c(x_min,x_max),maximum = TRUE)
    # y_max <- y_max$objective
    # 
    # #garantir um valor minimo de 0.25 casa decimal
    # if(y_min> -0.1) y_min <- -0.25
    # if(y_max<0.1) y_max <- 0.25
    # 
    # visible(gg2) <- TRUE #agora a area grafica gg2 que ira receber o plot
    # par(mar=rep(0, 4)) #margem
    # plot(func, xlim=c(x_min,x_max),ylim=c(y_min,y_max), col = "red", xlab="Eixo x", ylab="Eixo y", container= gg) #plot da funcao
    # abline(h=0, lty=2)
    # abline(v=0, lty=2)
    # 
    # points(m_k[iz:cont], z_k[iz:cont], col="blue", pch = 1, cer = 5) # Plot dos pontos m_k sobre o eixo x
    # 
    # if(g_indices){
    #   text(m_k[iz:cont],z_k[iz:cont],index,cex=0.65, pos=3, col="blue")
    # }
    
    #Resultados a serem mostrados ao usuario
    
    #dispose(mk_output)
    
    
  }
  # TODO: Implementar essa msg de erro
  
  # #== Erro para caso tenha um numero par de raizes
  # else{
  #   error.NoNegative <- gwindow("Erro",width = 10)
  #   error.NNgt <- ggroup(horizontal = FALSE, container = error.NoNegative)
  #   error.NNgb <- ggroup(horizontal = FALSE, container = error.NoNegative)
  #   error.NNlabel <- glabel("No intervalo dado a funcao nao tem raiz, tem um numero par de raizes, ou a raiz e um ponto critico da funcao. Escolha outro interlavo", container=error.NNgt)
  #   exit.NN <-function(h,...){dispose(error.NoNegative)}
  #   gbutton("Ok", cont= error.NNgb, handler = exit.NN)
  # 
  # }
  value_output <<- list()
  value_output[[1]] <<- paste("Aproximações: ",paste0(m_k, collapse =" | "))
}


#===========================================================================
#INTERFACE
# winbissection <- gwindow("Metodo da Bissecao") #Criacao da janela
# 
# ##= Criacao dos grupos
# Groupbuttons <- ggroup(container = winbissection, horizontal=FALSE)
# Groupgraphic <- ggroup(container = winbissection, horizontal=FALSE)
# 
# ##= Cricao dos frames
# buttonsFrame <- gframe("Dados de Entrada", container = Groupbuttons, horizontal = FALSE)
# gbutton("Desenhe", cont= Groupbuttons, handler = bissection)
# valueframe <- gframe("Resultados", container = Groupbuttons, hozizontal = TRUE)
# mk_output <- gtext("", container=valueframe, expand = TRUE)
# 
# ##= Criacao das opcoes graficas
# checkboxframe <- gframe("Opcoes Graficas", container =Groupgraphic, horizontal = TRUE)
# glabel("Selecione antes do Plot", container= checkboxframe)
# g_indices <- gcheckbox("Indices de x", checked = FALSE, cont =checkboxframe)
# g_lv <- gcheckbox("Linha auxiliar", checked = TRUE, cont= checkboxframe, expand = TRUE)
# 
# ##= Criacao das area do zoom
# zoomGraphFrame <- gframe("Zoom do grafico principal", container = Groupbuttons, horizontal = FALSE)
# gg2<-ggraphics(container = zoomGraphFrame,  width = 220, height = 220)
# 
# ##= Criacao da area do plot principal
# mainGrapghFrame <- gframe("Grafico Principal", container = Groupgraphic)
# gg<-ggraphics(container = mainGrapghFrame, width = 500, height = 500)
# 
# ##= area de entrada dos dados
# functionframe <- gframe("Funcao", container = buttonsFrame, horizontal = TRUE)
# env_funcao<-gedit("",width = 50, cont = functionframe, initial.msg = "ex: 2*x + exp(x) - sin(x) + log(x)")
# 
# intervalframe <- gframe("Intervalo", container = buttonsFrame, horizontal = TRUE)
# glabel("Intervalo em x:", container = intervalframe)
# env_pontos<-gedit("", width = 20,cont = intervalframe, initial.msg = "Separados por espaco")
# 
# stopframe <- gframe("Precisao desejada", container= buttonsFrame, horizontal=TRUE)
# glabel("No de casas decimais:", container = stopframe)
# env_decimais<-gedit("",width = 5, cont = stopframe, initial.msg = "ex.: 1 para 0,1")
# glabel("No de iteracoes:", container = stopframe)
# env_iteracoes<-gedit("",width = 5, cont = stopframe, initial.msg = "ex.: 0 p/ ilimitado")
# 
# sppedframe <- gframe("Velocidade da animacao", container= buttonsFrame, horizontal=TRUE, expand = TRUE)
# glabel("Tempo em segundos:", container = sppedframe)
# env_veloc_anim<-gedit("",width = 35, cont = sppedframe, initial.msg = "Intervalo de tempo entre as iteracoes")
# 
# ##= criacao do botao de saida
# exit_func<-function(h,...){dispose(winbissection)}
# gbutton("SAIR", cont= Groupbuttons, handler = exit_func)
# 
# ##= while utilizado na construcao da animacao
# while(isExtant(winbissection)) Sys.sleep(1)
# 
# ##= Mudar o icone da janela
# dir <- dirname(sys.frame(1)$ofile)
# icon_dir <-paste0(dir, "/icon.png")
# img <- gdkPixbufNewFromFile(icon_dir)
# getToolkitWidget(winbissection)$setIcon(img$retval)

