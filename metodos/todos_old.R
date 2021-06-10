############################################################################################
#' Metodo da Bissecao:
#' Ilustra as iteracoes feitas pelo metodo da bissecao, que obtem aproximacoes para as raizes de uma dada funcao real.
#'
#' @param Funcao A equacao que descreve a funcao para a qual se deseja encontrar as raizes. Ex: exp(x) - x^2 + sqrt(x + 2)
#' @param Intervalo_x Intervalo contento a raiz da funcao, separado por espaco, exemplo: -5 6
#' @param N_casas_decimais Numero de casas decimais correspondente a precisao desejada
#' @param Iteracoes Numero maximo de iteracoes
#' @param Tempo tempo de exibição de cada iteracao
#' @param OG_Indices Determina se os indices das aproximacoes obtidas em cada iteracao serao exibidos ou nao
#' @param OG_Linha_Axiliar Determina se a linha com o intervalo da iteracao sera exibida ou nao
BISSECAO <- function()
{
#== Mensagem inicial na area de resultados
valuetextm <- "Aproximacoes obtidas"


##================================================================
##========== FUNCOES

#==== Funcao principal (que faz o metodo)
bissection <- function(h,...)
{
  #== Valores de entrada
  f<-svalue(env_function)
  pointsentr<-svalue(env_pts)
  s<-as.numeric(svalue(env_stop))
  stp <- 10^(-s)
  stpint <- as.numeric(svalue(env_inter))
  if(is.na(stpint)) stpint <- 999 #numero ilimitado de iteracoes
  speed<-as.numeric(svalue(env_speed))


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

  #=== Garantindo que os pre-requisitos estao sendo seguidos exibindo janela de erro
  #
  if(abs(fa)<stp){ # Erro caso o extremo inferior ja satisfaca o criterio de parada
    #== Criacao da janela de erro
    error.FAMINOR <- gwindow("Erro", width = 15)
    error.FAgt <- ggroup(horizontal = FALSE, container = error.FAMINOR)
    error.FAgb <- ggroup(horizontal = FALSE, container = error.FAMINOR)
    error.FAlabel <- glabel("O extremo inferior do intervalo ja satisfaz o criterio de parada", container=error.FAgt)
    exit.FA <-function(h,...){dispose(error.FAMINOR)}
    gbutton("Ok", cont= error.FAgb, handler = exit.FA)
    stop #Parar o codigo se a janela for criada
  }


  if(abs(fb)<stp){ #Erro caso o extremo superior ja satisfaca o criterio de parada
    #== Criacao da janela de erro
    error.FBMINOR <- gwindow("Erro", width = 15)
    error.FBgt <- ggroup(horizontal = FALSE, container = error.FBMINOR)
    error.FBgb <- ggroup(horizontal = FALSE, container = error.FBMINOR)
    error.FBlabel <- glabel("O extremo superior do intervalo ja satisfaz o criterio de parada", container=error.FBgt)
    exit.FB <-function(h,...){dispose(error.FBMINOR)}
    gbutton("Ok", cont= error.FBgb, handler = exit.FB)
    stop #Parar o codigo se a janela for criada
  }

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

    visible(gg) <- TRUE #Agora a area grafica gg que ira receber o plot
    plot(func,xlim=c(a_k[1]- 1,b_k[1]+ 1), col = "red", xlab="Eixo x", ylab="Eixo y", container= gg) #Plot da f(x)
    abline(h=0, lty=2)
    abline(v=0, lty=2)

    z_k <- rep(0, cont) # Vetor de zeros do tamanho do vetor m_k

    #= Plot dos pontos a e b sobre o eixo x
    points(a_k[1], 0, col="blue", pch = 1, cer = 5)
    text(a_k[1],0,"a",cex=0.65, pos=3, col="blue")
    points(b_k[1], 0, col="blue", pch = 1, cer = 5)
    text(b_k[1],0,"b",cex=0.65, pos=3, col="blue")

    #== Animacao
    #
    for (i in 1:cont) #Para cada iteracao
    {
      if(svalue(linhz)){ #linha horizontal
        Sys.sleep(speed/2) #tempo
        inter_x <-c(a_k[i],b_k[i])
        lines(inter_x, inter_y, col="green4", lwd = i + (i-1)*1.2)
        points(inter_x, inter_y,bg ="green4", col="black", pch = 21)
        #--------------------------------------------------------------
      }

      Sys.sleep(speed/2) #tempo
      points(m_k[1:i], z_k[1:i], col="blue", pch = 1, cer = 5) # Plot dos pontos m_k sobre o eixo x

      #
      if(svalue(pont)){ #Indices dos pontos
        index <-c(0:(i-1))
        text(m_k[1:i],z_k[1:i],index,cex=0.65, pos=3, col="blue")
      }
    }


    #=== Plot do zoom
    dx <- (b0-a0)/10
    visible(gg2) <- TRUE #agora a area grafica gg2 que ira receber o plot
    par(mar=rep(0, 4)) #margem
    plot(func, xlim=c(m_k[cont] - dx,m_k[cont] + dx), col = "red", xlab="Eixo x", ylab="Eixo y", container= gg) #plot da funcao
    abline(h=0, lty=2)
    abline(v=0, lty=2)

    points(m_k[1:cont], z_k[1:cont], col="blue", pch = 1, cer = 5) # Plot dos pontos m_k sobre o eixo x
    index <-c(0:cont)
    if(svalue(pont)){
      text(m_k[1:cont],z_k[1:cont],index,cex=0.65, pos=3, col="blue")
    }

    #==Resultados a serem mostrados ao usuario
    valuetextm <- paste("Aproximacoes: ",paste0(m_k, collapse =" | "))
    insert(mk_output,valuetextm)
  }

  #== Erro para caso tenha um numero par de raizes
  else{
    error.NoNegative <- gwindow("Erro",width = 10)
    error.NNgt <- ggroup(horizontal = FALSE, container = error.NoNegative)
    error.NNgb <- ggroup(horizontal = FALSE, container = error.NoNegative)
    error.NNlabel <- glabel("No intervalo dado a funcao nao tem raiz, tem um numero par de raizes, ou a raiz e um ponto critico da funcao. Escolha outro interlavo", container=error.NNgt)
    exit.NN <-function(h,...){dispose(error.NoNegative)}
    gbutton("Ok", cont= error.NNgb, handler = exit.NN)

  }

}


#===========================================================================
#INTERFACE
winbissection <- gwindow("Metodo da Bissecao") #Criacao da janela

##= Criacao dos grupos
Groupbuttons <- ggroup(container = winbissection, horizontal=FALSE)
Groupgraphic <- ggroup(container = winbissection, horizontal=FALSE)

##= Cricao dos frames
buttonsFrame <- gframe("Dados de Entrada", container = Groupbuttons, horizontal = FALSE)
gbutton("Desenhe", cont= Groupbuttons, handler = bissection)
valueframe <- gframe("Resultados", container = Groupbuttons, hozizontal = TRUE)
mk_output <- gtext("", container=valueframe, expand = TRUE)

##= Criacao das opcoes graficas
checkboxframe <- gframe("Opcoes Graficas", container =Groupgraphic)
glabel("Selecione antes do Plot", container= checkboxframe)
pont <- gcheckbox("Indices de x", checked = FALSE, cont =checkboxframe)
linhz <- gcheckbox("Linha auxiliar", checked = TRUE, cont= checkboxframe, expand = TRUE)

##= Criacao das area do zoom
zoomGraphFrame <- gframe("Zoom do grafico principal", container = Groupbuttons)
gg2<-ggraphics(container = zoomGraphFrame,  width = 220, height = 220)

##= Criacao da area do plot principal
mainGrapghFrame <- gframe("Grafico Principal", container = Groupgraphic)
gg<-ggraphics(container = mainGrapghFrame, width = 500, height = 500)

##= area de entrada dos dados
functionframe <- gframe("Funcao", container = buttonsFrame)
env_function<-gedit("",width = 50, cont = functionframe, initial.msg = "ex: 2*x + exp(x) - sin(x) + log(x)")

intervalframe <- gframe("Intervalo", container = buttonsFrame)
glabel("Intervalo em x:", container = intervalframe)
env_pts<-gedit("", width = 20,cont = intervalframe, initial.msg = "Separados por espaco")

stopframe <- gframe("Precisao desejada", container= buttonsFrame )
glabel("No de casas decimais:", container = stopframe)
env_stop<-gedit("",width = 5, cont = stopframe, initial.msg = "ex.: 1 para 0,1")
glabel("No de iteracoes:", container = stopframe)
env_inter<-gedit("",width = 5, cont = stopframe, initial.msg = "ex.: 0 p/ ilimitado")

sppedframe <- gframe("Velocidade da animacao", container= buttonsFrame, expand = TRUE)
glabel("Tempo em segundos:", container = sppedframe)
env_speed<-gedit("",width = 35, cont = sppedframe, initial.msg = "Intervalo de tempo entre as iteracoes")

##= criacao do botao de saida
exit_func<-function(h,...){dispose(winbissection)}
gbutton("SAIR", cont= Groupbuttons, handler = exit_func)

##= while utilizado na construcao da animacao
while(isExtant(winbissection)) Sys.sleep(1)

##= Mudar o icone da janela
#dir <- dirname(sys.frame(1)$ofile)
#icon_dir <-paste0(dir, "/icon.png")
#img <- gdkPixbufNewFromFile(icon_dir)
#getToolkitWidget(winbissection)$setIcon(img$retval)

}


###########################################################################################
#' Método da Falsa Posicao:
#' Ilustra as iteracoes feitas pelo metodo da falsa posicao, que obtem aproximacoes para as raizes de uma dada funcao real.
#'
#' @param Funcao A equacao que descreve a funcao para a qual se deseja encontrar as raizes. Ex: exp(x) - x^2 + sqrt(x + 2)
#' @param Intervalo_x Intervalo contento a raiz da funcao, separado por espaco, exemplo: -5 6
#' @param N_Casas Numero de casas decimais correspondente a precisao desejada
#' @param N_Iteracoes Numero maximo de iteracoes
#' @param Tempo Tempo de exibicao de cada iteracao
#' @param OG_Indices Determina se os indices das aproximacoes obtidas em cada iteracao serao exibidos ou não
#' @param OG_Linha_Secante Deternina se as retas secantes ao grafico serao exibidas ou não
FALSAPOSICAO <-function()
{
  #== Mensagem inicial na area de resultados
  valuetextm <- "Aproximacoes obtidas"

  ##================================================================
  ##========== FUNCOES

  #==== Funcao principal (que faz o metodo)
  falsa <- function(h,...)
  {
    # Valores de entrada
    f<-svalue(env_function)
    pointsentr<-svalue(env_pts)
    s<-as.numeric(svalue(env_stop))
    stp <- 10^(-s)
    stpint <- as.numeric(svalue(env_inter))
    if(is.na(stpint)) stpint <- 999 #numero ilimitado de itera??es
    speed<-as.numeric(svalue(env_speed))

    #=== pegar os valores separados em x
    #
    valxaux <- as.list(strsplit(pointsentr," ")[[1]])
    contval <- length(valxaux) # contador da quantidade de valores de entrada
    #intervalo
    a0 <-as.numeric(valxaux[1])
    b0 <- as.numeric(valxaux[2])

    func <- paste("func <- function(x){",f,"}")  # Criando string de entrada
    eval(parse(text=func))  # Transformando o texto salvo na variavel ftext em uma expressao

    fa <- func(a0)     # valor da F(a)
    fb <- func(b0)     # Valor da F(b)

    # Vetores para o plot
    a_k <- c()
    b_k <- c()
    fa_k <- c()
    fb_k <- c()
    m_k <- c()

    # Contador de indices para while
    cont <- 1

    #=== Garantindo que os pre-requisitos estao sendo seguidos exibindo janela de erro
    #
    if(abs(fa)<stp) # Erro caso o extremo inferior ja satisfaca o criterio de parada
    {
      error.FAMINOR <- gwindow("Erro", width = 15)
      error.FAgt <- ggroup(horizontal = FALSE, container = error.FAMINOR)
      error.FAgb <- ggroup(horizontal = FALSE, container = error.FAMINOR)
      error.FAlabel <- glabel("O extremo inferior do intervalo ja satisfaz o criterio de parada", container=error.FAgt)
      exit.FA <-function(h,...){dispose(error.FAMINOR)}
      gbutton("Ok", cont= error.FAgb, handler = exit.FA)
      stop
    }

    if(abs(fb)<stp){ #Erro caso o extremo superior ja satisfaca o criterio de parada

      error.FBMINOR <- gwindow("Erro", width = 15)
      error.FBgt <- ggroup(horizontal = FALSE, container = error.FBMINOR)
      error.FBgb <- ggroup(horizontal = FALSE, container = error.FBMINOR)
      error.FBlabel <- glabel("O extremo superior do intervalo ja satisfaz o criterio de parada", container=error.FBgt)
      exit.FB <-function(h,...){dispose(error.FBMINOR)}
      gbutton("Ok", cont= error.FBgb, handler = exit.FB)
      stop
    }


    #===Comeco do metodo em si
    #
    if((fa*fb)<0) # If para garantir que o metodo so seja feito caso tenha um numero impar de raizes no intervalo dado
    {
      whilevar <- -1
      while(whilevar == -1) # Garantir que seja feito somente ate que o criterio do erro nao seja satisfeito
      {
        #==Atribuicao dos valores aos vetores
        a_k[cont] <- a0
        b_k[cont] <- b0
        fa_k[cont] <- func(a0)
        fb_k[cont] <- func(b0)
        m_k[cont] <- (a_k[cont]*fb_k[cont] - b_k[cont]*fa_k[cont])/(fb_k[cont] - fa_k[cont]) #Atribuicao do ponto medio


        #== Definir qual sera o proximo a e b
        if(fa_k[cont]*func(m_k[cont])<0){
          b0 <- m_k[cont]
        }
        else {
          a0 <- m_k[cont]
        }

        if((cont>=2)&&(abs(b_k[cont]-a_k[cont])<stp)){whilevar <- 1}
        if(abs(func(m_k[cont]))<stp){whilevar<-1}
        if((cont)>=stpint){whilevar <- 1}
        cont<- cont + 1 # Aumentar o indice do contador
      }
      cont <- cont -1

      #==== Plot do metodo
      #

      y_min <- optimize(func,interval = c(a_k[1],b_k[1])) #y_min pega o valor que da o minimo em x e o valor em y
      y_min <- y_min$objective #y_min agora pega apenas o valor em y
      y_max <- optimize(func,interval = c(a_k[1],b_k[1]),maximum = TRUE)
      y_max <- y_max$objective
      absalt <- abs(y_max-y_min) #Altura total do plot
      absalt <- abs(y_max-y_min) #Altura total do plot

      # Definir uma altura minima nos extremos superiores e inferiores
      if(abs(y_min) <= 0.1*(absalt)) y_min<- -0.1*(absalt)
      if(abs(y_max) <= 0.1*(absalt)) y_max<- 0.1*(absalt)

      visible(gg) <- TRUE #Agora a area grafica gg que ira receber o plot
      plot(func, xlim=c(a_k[1] - 1, b_k[1] + 1),ylim=c(y_min,y_max), col = "red", xlab="Eixo x", ylab="Eixo y", container= gg) #plot da f(x)
      abline(h=0, lty=2)
      abline(v=0, lty=2)

      z_k <- rep(0, cont) # Vetor de zeros do tamanho do vetor m_k

      #= Plot dos pontos a e b sobre o eixo x
      points(a_k[1], 0, col="blue", pch = 1, cer = 5)
      text(a_k[1],0,"a",cex=0.65, pos=3, col="blue")
      points(b_k[1], 0, col="blue", pch = 1, cer = 5)
      text(b_k[1],0,"b",cex=0.65, pos=3, col="blue")



      # Animacao
      #
      for (i in 1:cont) #Para cada iteracao
      {
        Sys.sleep(speed/3)
        points(m_k[1:i], z_k[1:i], col="blue", pch = 1, cer = 5)# Plot dos pontos m_k sobre o eixo x
        if(svalue(pont)){#Indices dos pontos
          index <-c(0:(i-1))
          text(m_k[1:i],z_k[1:i],index,cex=0.65, pos=3, col="blue")
        }
        Sys.sleep(speed/3)
        segments(a_k[i],0,a_k[i],fa_k[i], col= "azure4", lty=2)
        if(svalue(linsc)){
          Sys.sleep(speed/3)
          segments(a_k[i],fa_k[i],b_k[i],fb_k[i], col="yellow", lwd = 1.2)
        }
      }


      #=== Plot do zoom
      #
      dx <- (b0-a0)/10
      visible(gg2) <- TRUE #agora a area grafica gg2 que ira receber o plot
      par(mar=rep(0, 4)) #margem
      plot(func, xlim=c(m_k[cont] - dx,m_k[cont] + dx), col = "red", xlab="Eixo x", ylab="Eixo y", container= gg) #plot da funcao
      abline(h=0, lty=2)
      abline(v=0, lty=2)

      points(m_k[1:cont], z_k[1:cont], col="blue", pch = 1, cer = 5) # Plot dos pontos m_k sobre o eixo x
      index <-c(0:cont)
      if(svalue(pont)){
        text(m_k[1:cont],z_k[1:cont],index,cex=0.65, pos=3, col="blue")
      }
      segments(a_k[1:cont],rep(0,cont),a_k[1:cont],fa_k[1:cont], col= "azure4", lty=2)
      if(svalue(linsc)){
        segments(a_k[1:cont],fa_k[1:cont],b_k[1:cont],fb_k[1:cont], col="yellow", lwd = 1.2)
      }

      #Resultados a serem mostrados ao usuario
      valuetextm <- paste("Aproximacoes: ",paste0(m_k, collapse =" | "))
      insert(mk_output,valuetextm)
    }

    #== Erro para caso tenha um numero par de raizes
    else{
      error.NoNegative <- gwindow("Erro",width = 10)
      error.NNgt <- ggroup(horizontal = FALSE, container = error.NoNegative)
      error.NNgb <- ggroup(horizontal = FALSE, container = error.NoNegative)
      error.NNlabel <- glabel("No intervalo dado a funcao nao tem raiz ou tem um numero par de raizes. Escolha outro interlavo", container=error.NNgt)
      exit.NN <-function(h,...){dispose(error.NoNegative)}
      gbutton("Ok", cont= error.NNgb, handler = exit.NN)

    }
  }


  #===========================================================================
  #INTERFACE
  winfalse <- gwindow("Metodo da Falsa Posicao") #Janela Principal

  ##= Criacao dos grupos
  Groupbuttons <- ggroup(container = winfalse, horizontal=FALSE)
  Groupgraphic <- ggroup(container = winfalse, horizontal=FALSE)

  ##= Criacao dos frames
  buttonsFrame <- gframe("Dados de Entrada", container = Groupbuttons, horizontal = FALSE) #
  gbutton("Desenhe", cont= Groupbuttons, handler = falsa)
  valueframe <- gframe("Resultados", container = Groupbuttons, hozizontal = TRUE)
  mk_output <- gtext("", container=valueframe, expand = TRUE)

  ##= Criacao das opcoes graficas
  checkboxframe <- gframe("Opcoes", container =Groupgraphic, horizontal = TRUE)
  glabel("Selecione antes do Plot", container= checkboxframe)
  pont <- gcheckbox("Indices de x", checked = FALSE, cont =checkboxframe)
  linsc <- gcheckbox("Linha secante", checked = TRUE, cont= checkboxframe, expand = TRUE)

  ##= Criacao da area do zoom
  zoomGraphFrame <- gframe("Zoom do grafico principal", container = Groupbuttons, horizontal = FALSE)
  gg2<-ggraphics(container = zoomGraphFrame,  width = 220, height = 220)

  ##= Criacao da area do plot principal
  mainGrapghFrame <- gframe("Grafico Principal", container = Groupgraphic)
  gg<-ggraphics(container = mainGrapghFrame, width = 503, height = 503)

  ##= Area de entrada dos dados
  functionframe <- gframe("Funcao", container = buttonsFrame, horizontal = TRUE)
  env_function<-gedit("",width = 50, cont = functionframe, initial.msg = "ex: 2*x + exp(x) - sin(x) + log(x)")

  intervalframe <- gframe("Intervalo", container = buttonsFrame, horizontal = TRUE)
  glabel("Intervalo em x:", container = intervalframe)
  env_pts<-gedit("", width = 21,cont = intervalframe, initial.msg = "Separados por espaco")

  stopframe <- gframe("Precisao desejada", container= buttonsFrame, horizontal=TRUE)
  glabel("No de casas decimais:", container = stopframe)
  env_stop<-gedit("",width = 7, cont = stopframe, initial.msg = "ex.: 1 para 0,1")
  glabel("No de iteracoes:", container = stopframe)
  env_inter<-gedit("",width = 7, cont = stopframe, initial.msg = "ex.: 0 p/ ilimitado")

  sppedframe <- gframe("Velocidade da animacao", container= buttonsFrame, horizontal=TRUE)
  glabel("Tempo em segundos:", container = sppedframe)
  env_speed<-gedit("",width = 35, cont = sppedframe, initial.msg = "Intervalo de tempo entre as iteracoes")

  ##= Criacao do botao de saida
  exit_func<-function(h,...){dispose(winfalse)}
  gbutton("SAIR", cont= Groupbuttons, handler = exit_func)

  ##= while utilizado na construcao da animacao
  while(isExtant(winfalse)){Sys.sleep(1)}

  ##= Mudar o icone da janela
  # dir <- dirname(sys.frame(1)$ofile)
  # icon_dir <-paste0(dir, "/icon.png")
  # img <- gdkPixbufNewFromFile(icon_dir)
  # getToolkitWidget(winfalse)$setIcon(img$retval)


}
###########################################################################################
#' Método Newton-Rapson:
#' Ilustra as iteracoes feitas pelo metodo  Newton_Raphson, que obtem aproximacoes para as raizes de uma dada funcao real.
#'
#' @param Funcao A equacao que descreve a funcao para a qual se deseja encontrar as raizes. Ex: exp(x) - x^2 + sqrt(x + 2)
#' @param Pto_inicial "chute inicial"
#' @param Intervalo_x Define o intervalo no eixo x de visualizacao da funcao dada, separados por espaco e na ordem crescente
#' @param N_Casas Numero de casas decimais correspondente a precisao desejada
#' @param N_Iteracoes Numero maximo de iteracoes
#' @param Tempo Tempo de exibicao de cada iteracao
#' @param OG_Indices Determina se os indices das aproximacoes obtidas em cada iteracao serao exibidos ou não
#' @param OG_Linha_Tangante Determina se as retas tangentes ao grafico, utilizadas pelo metodo serao exibidas ou não
#' @param OG_Linha_vertical Determina se as linhas verticais ligando o ponto da funcao no eixo x serao exibidas ou nao
NEWTONRAPSON <-function()
{
  #== Mensagem inicial na area de resultados
  valuetextm <- "Aproximacoes obtidas"

  ##================================================================
  ##========== FUNCOES

  #==== Funcao principal (que faz o metodo)
  newtonraphson<- function(h,...)
  {
    #== Valores de entrada
    pointsentr<-svalue(env_pts)
    f<-svalue(env_function)
    x0<-as.numeric(svalue(env_x0))
    s<-as.numeric(svalue(env_stop))
    stp <- 10^(-s)
    stpint <- as.numeric(svalue(env_inter))
    if(is.na(stpint)) stpint <- 999 #numero ilimitado de itera??es
    speed<-as.numeric(svalue(env_speed))


    #=== pegar os valores separados em x
    #
    valxaux <- as.list(strsplit(pointsentr," ")[[1]])
    contval <- length(valxaux) # contador da quantidade de valores de entrada
    #intervalo
    a0 <-as.numeric(valxaux[1])
    b0 <- as.numeric(valxaux[2])


    # Criando strings de entrada
    func <- paste("func <- function(x){",f,"}")
    func2 <- paste("func2 <- expression(",f,")")

    # Transformando os textos salvos na variavel ftext em uma expressao
    eval(parse(text=func))
    eval(parse(text=func2))

    ##=== Criando a funcao derivada primeira da funcao de entrada
    DevFunc <- function(x){eval(D(func2,"x"))}
    f_x0 <- func(x0)     # valor da F(x0)
    Dev_x0 <-DevFunc(x0)   #valor da F'(x0)
    #Dev_x1 <- DevFunc((x0 - ((f_x0)/(Dev_x0))))  #valor da F'(x1)
    if(Dev_x0 == 0){ #= Erro caso a derivada fa funÇão em x0 seja zero
        #== Criacao da janela de erro
        error.x0 <- gwindow("Erro")
        error.x0gt <- ggroup(horizontal = FALSE, container = error.x0)
        error.x0gb <- ggroup(horizontal = FALSE, container = error.x0)
        error.x0label <- glabel("a derivada da funcao tem valor zero no ponto x0 dado como chute inicial", container=error.x0gt)
        exit.x0 <-function(h,...){dispose(error.x0)}
        gbutton("Ok", cont= error.x0gb, handler = exit.x0)
        stop
      }

    # Criando vetores para o plot
    x_k <- c()
    fx_k <- c()

    # Preenchendo os vetores
    x_k[1] <- x0
    x_k[2] <- x0 - f_x0/Dev_x0 #primeira iteracao para x1
    fx_k[1] <- f_x0
    fx_k[2] <- func(x_k[2]) #f(x1)

    # contador de indices para while
    cont <- 2

    #=== Garantindo que os pre-requisitos estao sendo seguidos exibindo janela de erro
    #
    if(abs(func(x0))<stp){ #= Erro caso o ponto dado ja satisfaz o criterio de parada
      #== Criacao da janela de erro
      error.x0 <- gwindow("Erro")
      error.x0gt <- ggroup(horizontal = FALSE, container = error.x0)
      error.x0gb <- ggroup(horizontal = FALSE, container = error.x0)
      error.x0label <- glabel("O ponto inicial dado ja satisfaz o criterio do erro minimo.", container=error.x0gt)
      exit.x0 <-function(h,...){dispose(error.x0)}
      gbutton("Ok", cont= error.x0gb, handler = exit.x0)
      stop #Parar o c?digo se a janela for criada
    }
    else{ #Continuar o codigo...
      #===Comeco do metodo em si
      whilevar <- -1
      while(whilevar == -1) #= fazer iteracoes ate que o criterio seja atingido
      {
        cont <- cont + 1
        x_k[cont] <- (x_k[cont-1] - ((func(x_k[cont-1]))/(DevFunc(x_k[cont-1]))))
        fx_k[cont] <- func(x_k[cont])

        #Parar o m?todo pelo while
        if(cont>=stpint){whilevar <-1}
        if(abs(x_k[cont]-x_k[cont-1])<stp){whilevar <- 1}
        if(abs(fx_k[cont])<stp){whilevar <- 1}
      }
    }
    #==== Plot do metodo
    #

    visible(gg) <- TRUE #= area grafica gg recebe o plot
    plot(func, xlim = c(a0, b0), col = "red", xlab="Eixo x", ylab="Eixo y", container= gg) #= plot da curva
    abline(h=0, lty=2)
    abline(v=0, lty=2)
    z_k <- rep(0, cont) ## Vetor de zeros do tamanho do vetor m_k

    #== Animacao
    #
    for (i in 1:cont) #Para cada iteracao
    {
      ##Plot dos pontos
      Sys.sleep(speed/4)
      points(x_k[1:i], z_k[1:i], col="blue", pch = 1) # Plot dos pontos x_k sobre o eixo x
      index <-c(0:(cont-1))

      if(svalue(pont)){ #Caso seja marcado os indicies dos pontos no checkbox
        text(x_k[i],z_k[i],  index[i], cex=0.65, pos=3, col="blue")
      }
      if(svalue(linvt)){ #Caso seja marcado as linhas verticais no checkbox
        Sys.sleep(speed/4)
        segments(x_k[i],0,x_k[i],fx_k[i], col= "azure4", lty=2)
      }
      ##Plot dos x_k na funcao
      Sys.sleep(speed/4)
      points(x_k[1:i],fx_k[1:i], col="green", pch=1)
      if(svalue(lintg)){ #Caso seja marcado as linhs tangentes no checkbox
        Sys.sleep(speed/4)
        segments( x_k[i],fx_k[i],x_k[i+1],0,col = "black")
      }
    }

    #=== Plot do zoom
    #
    dx <- (b0-a0)/10
    visible(gg2) <- TRUE #agora a area grafica gg2 que ira receber o plot
    par(mar=rep(0, 4)) #margem
    plot(func, xlim=c(x_k[cont] - dx,x_k[cont] + dx), col = "red", xlab="Eixo x", ylab="Eixo y", container= gg) #plot da funcao
    abline(h=0, lty=2)
    abline(v=0, lty=2)

    points(x_k[1:cont], z_k[1:cont], col="blue", pch = 1, cer = 5) # Plot dos pontos m_k sobre o eixo x
    index <-c(0:cont)
    if(svalue(pont)){
      text(x_k[1:cont],z_k[1:cont],index,cex=0.65, pos=3, col="blue")
    }

   # if(svalue(lintg)){ #Caso seja marcado as linhas tangente no checkbox

    #  for(i in 1:cont-1)
     #   segments(x_k[i],fx_k[i],x_k[i+1],0.0,col = "black")
    #}
    #if(svalue(linvt)){ #Caso seja marcado as linhas verticais no checkbox
      #while(i<cont){
     # for(i in 1:cont-1)
      #  segments(x_k[i],0.0,x_k[i],fx_k[i], col= "azure4", lty=2)
    #}
    #Resultados a serem mostrados ao usuario
    valuetextm <- paste("Aproximacoes: ",paste0(x_k, collapse =" | "))
    insert(xk_output,valuetextm)
  }

  #===========================================================================
  #INTERFACE
  winnewton <- gwindow("Metodo de Newton-raphson") #Criacao da janela

  ##= Criacao dos grupos
  Groupbuttons <- ggroup(container = winnewton, horizontal=FALSE)
  Groupgraphic <- ggroup(container = winnewton, horizontal=FALSE)

  ##= Cricao dos frames
  buttonsFrame <- gframe("Dados de Entrada", container = Groupbuttons, horizontal = FALSE)
  gbutton("Desenhe", cont= Groupbuttons, handler = newtonraphson)
  valueframe <- gframe("Resultados", container = Groupbuttons, hozizontal = TRUE)
  xk_output <- gtext("", container=valueframe, expand = TRUE)

  ##= Criacao das opcoes graficas
  checkboxframe <- gframe("Opcoes Graficas", container =Groupgraphic, horizontal = TRUE)
  pont <- gcheckbox("Indices dos pontos", checked = FALSE, cont =checkboxframe)
  lintg <- gcheckbox("Linhas tangentes", checked = TRUE, cont = checkboxframe)
  linvt <- gcheckbox("Linhas verticais", checked = TRUE, cont= checkboxframe)

  ##= Criacao das area do zoom
  zoomGraphFrame <- gframe("Zoom do grafico principal", container = Groupbuttons, horizontal = FALSE)
  gg2<-ggraphics(container = zoomGraphFrame,  width = 220, height = 220)

  ##= Criacao da area do plot principal
  mainGrapghFrame <- gframe("Grafico Principal", container = Groupgraphic)
  gg<-ggraphics(container = mainGrapghFrame, width = 545, height = 545)

  ##= area de entrada dos dados
  functionframe <- gframe("Funcao", container = buttonsFrame, horizontal = TRUE)
  env_function<-gedit("",width = 50, cont = functionframe, initial.msg = "ex: 2*x + exp(x) - sin(x) + log(x)")

  intervalframe <- gframe("Chute inicial", container = buttonsFrame, horizontal = TRUE)
  glabel("Ponto de inicio:", container = intervalframe)
  env_x0<-gedit("", width = 14,cont = intervalframe, initial.msg = "x0")

  interframe <- gframe("Intervalo do plot", container = buttonsFrame, horizontal = TRUE)
  glabel("Intervalo em x:", container = interframe)
  env_pts<-gedit("", width = 14,cont = interframe, initial.msg = "Separado por espaco")

  stopframe <- gframe("Precisao desejada", container= buttonsFrame, horizontal=TRUE)
  glabel("No de casas decimais:", container = stopframe)
  env_stop<-gedit("",width = 7, cont = stopframe, initial.msg = "ex.: 1 para 0,1")
  glabel("No de iteracoes:", container = stopframe)
  env_inter<-gedit("",width = 7, cont = stopframe, initial.msg = "ex.: 0 p/ ilimitado")

  speedframe <- gframe("Velocidade da animacao", container= buttonsFrame, horizontal=TRUE)
  glabel("Tempo em segundos:", container = speedframe)
  env_speed<-gedit("",width = 35, cont = speedframe, initial.msg = "Intervalo de tempo entre as iteracoes")

  ##= Botao de saida
  exit_func<-function(h,...){dispose(winnewton)}
  gbutton("SAIR", cont= Groupbuttons, handler = exit_func)

  ##= while utilizado na construcao da animacao
  while(isExtant(winnewton)) Sys.sleep(1)

  ##= Mudar o icone da janela
#  dir <- dirname(sys.frame(1)$ofile)
#  icon_dir <-paste0(dir, "/icon.png")
#  img <- gdkPixbufNewFromFile(icon_dir)
#  getToolkitWidget(winbissection)$setIcon(img$retval)

}
###########################################################################################
#' Aproximacao pelo metodo das secantes:
#' Ilustra as iteracoes feitas pelo metodo  Método da Secante, que obtem aproximacoes para as raizes de uma dada funcao real.
#' @param Funcao A equacao que descreve a funcao para a qual se deseja encontrar as raizes. Ex: exp(x) - x^2 + sqrt(x + 2)
#' @param Intervalo_x Intervalo contento a raiz da funcao, separado por espaco, exemplo: -5 6
#' @param N_Casas Numero de casas decimais correspondente a precisao desejada
#' @param N_Iteracoes Define o numero maximo de iteracao, 0 (zero) para limitar apenas pelas casas decimais
#' @param Tempo Tempo de exibicao de cada iteracao
#' @param OG_Indices Determina se os indices das aproximacoes obtidas em cada iteracao serao exibidos ou não
#' @param OG_Linha_secante  Determina se as retas secantes ao grafico, utilizadas pelo metodo serao exibidas ou não
#' @param OG_Linha_vertical Determina se as linhas verticais ligando o ponto da funcao no eixo x serao exibidas ou nao
SECANTE <-function()
{
  #== Mensagem inicial na area de resultados
  valuetextm <- "Aproximacoes obtidas"

  ##================================================================
  ##========== FUNCOES


  #==== Funcao principal (que faz o metodo)
  secante<- function(h,...) #Fun??o principal (que faz o metodo)
  {
    #== Valores de entrada
    f<-svalue(env_function)
    pointsentr<-svalue(env_pts)
    initical_point <-svalue(env_init)
    speed<-as.numeric(svalue(env_speed))

    stpcont <-as.numeric(svalue(env_stpcont))
    if(is.na(stpcont)){stpcont<-9999}

    s<-as.numeric(svalue(env_stop))
    stp <- 10^(-s)


    #=== pegar os valores separados em x
    #
    valxaux <- as.list(strsplit(pointsentr," ")[[1]])
    contval <- length(valxaux) # contador da quantidade de valores de entrada
    #intervalo
    a0 <-as.numeric(valxaux[1])
    b0 <- as.numeric(valxaux[2])

    valxaux <- as.list(strsplit(initical_point," ")[[1]])
    contval <- length(valxaux) # contador da quantidade de valores de entrada

    x0 <-as.numeric(valxaux[1])
    x1 <- as.numeric(valxaux[2])



    func <- paste("func <- function(x){",f,"}") # Criando string de entrada
    eval(parse(text=func))# Transformando o texto salvo na variavel ftext em uma expressao

    f_x0 <- func(x0)
    f_x1 <- func(x1)

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


    whileaux <- -1 #teste para fazer ate chegar ao erro desejado

 #   if(((f_x0)*(f_x1))>0){ # Erro para garantir que os extremos tem sinais diferentes
      #== Criacao da janela de erro
  #    error.x0 <- gwindow("Erro", height=100, parent=c(250,100))
  #    error.x0gt <- ggroup(horizontal = FALSE, container = error.x0)
  #    error.x0label <- glabel("O intervalo ou nao tem raiz ou tem um numero par de raizes [ f(a)*f(b)<0 ]
                        #      por favor, selecione um intervalo melhor.", container=error.x0gt)
   #   exit.x0 <-function(h,...){dispose(error.x0)}
   #   gbutton("Ok", cont= error.x0gt, handler = exit.x0)
   # }

    #===Comeco do metodo em si
    #
    #else{
      while(whileaux == -1) # Garantir que seja ate o criterio ser atingido
      {
        cont <- cont + 1
        x_k[cont] <- (x_k[cont-1] - ((fx_k[cont-1])*((x_k[cont-1] - x_k[cont-2])/(fx_k[cont-1] - fx_k[cont-2]))))
        fx_k[cont] <- func(x_k[cont])

        Errosec <-((x_k[cont]-x_k[cont-1])/(x_k[cont]))
        ##= Mudar o test quando atingir o erro desejado
        if(abs(Errosec)<stp){whileaux <- 1}
        if(cont>stpcont){whileaux <- 1}
        if(abs(fx_k[cont])<stp){whileaux <- 1}
      }

      #==== Plot do metodo

      visible(gg) <- TRUE #Agora a area grafica gg que ira receber o plot

      plot(func,xlim = c(a0, b0), col = "red", xlab="Eixo x", ylab="Eixo y") #= plot da curva
      abline(h=0, lty=2)
      abline(v=0, lty=2)

      z_k <- rep(0, cont) #= vetor de zeros

      #== Animacao
      #
      index <-c(0:(cont-1)) #indices
      if(svalue(pont))
      text(x_k[1],z_k[1],  index[1], cex=0.65, pos=3, col="blue")
      points(x_k[1], z_k[1], col="blue", pch = 1) # Plot dos pontos x_k sobre o eixo x

      Sys.sleep(speed/4)
      if(svalue(linvt)) #linhas verticais
      segments(x_k[1],0,x_k[1],fx_k[1], col= "azure4", lty=2)
      Sys.sleep(speed/4)
      points(x_k[1],fx_k[1], col="green", pch=1) #Plot dos x_k na funcao

      for (i in 2:(cont))
      {
        Sys.sleep(speed/4)
        points(x_k[1:i], z_k[1:i], col="blue", pch = 1) # Plot dos pontos x_k sobre o eixo x

        if(svalue(linvt)){ #linhas verticais
          Sys.sleep(speed/4)
          segments(x_k[i],0,x_k[i],fx_k[i], col= "azure4", lty=2)
        }

        Sys.sleep(speed/4)
        points(x_k[2:i],fx_k[2:i], col="green", pch=1) #Plot dos x_k na funcao

        Sys.sleep(speed/4)
        if(svalue(pont)){ #indices dos pontos
          text(x_k[i],z_k[i],  index[i], cex=0.65, pos=3, col="blue")
        }

        if(svalue(linsc)){ #linhas secantes
          Sys.sleep(speed/4)
           if((fx_k[i-1]*fx_k[i])<0)
              segments(x_k[i-1],fx_k[i-1],x_k[i],fx_k[i],col = "yellow")
            else
              segments(x_k[i-1],fx_k[i-1],x_k[i+1],0.0,col="yellow")
        }
      }


      #=== Plot do zoom
      dx <- (b0-a0)/10
      visible(gg2) <- TRUE #agora a area grafica gg2 que ira receber o plot
      par(mar=rep(0, 4)) #margem
      plot(func, xlim=c(x_k[cont]-dx, x_k[cont]+dx), col = "red", xlab="", ylab="") #plot da funcao
      abline(h=0, lty=2)
      abline(v=0, lty=2)

      points(x_k[1:cont], z_k[1:cont], col="blue", pch = 1) # Plot dos pontos m_k sobre o eixo x

      if(svalue(pont)){ #Caso seja marcado os pontos no checkbox
        text(x_k,z_k,  index, cex=0.65, pos=3, col="blue")
      }
      if(svalue(linsc)){ #Caso seja marcado as linhas tangente no checkbox
        for(i in (1:cont)){
          if(i<cont){
            if((fx_k[i]*fx_k[i+1])<0){
              segments(x_k[i],fx_k[i],x_k[i+1],fx_k[i+1],col = "yellow")
            }
            else{
              segments(x_k[i],fx_k[i],x_k[i+2],0,col="yellow")
            }
          }
        }
      }

      if(svalue(linvt)){ #linhas verticais
        for(i in 1:cont){
          segments(x_k[i],0,x_k[i],fx_k[i], col= "azure4", lty=2)
        }
      }

      #Resultados a serem mostrados ao usuario
      valuetextm <- paste0(x_k, collapse =" | ")
      insert(xk_output,valuetextm)
    #}



  }

  #===========================================================================
  #INTERFACE
  winsecante <- gwindow("Metodo das Secantes") #= janela principal

  ##= Criacao dos grupos
  Groupbuttons <- ggroup(container = winsecante, horizontal=FALSE)
  Groupgraphic <- ggroup(container = winsecante, horizontal=FALSE)

  ##= Cricao dos frames
  buttonsFrame <- gframe("Dados de Entrada", container = Groupbuttons, horizontal = FALSE)
  gbutton("Desenhe", cont= Groupbuttons, handler = secante)
  valueframe <- gframe("Resultados", container = Groupbuttons, hozizontal = TRUE)
  xk_output <- gtext("", container=valueframe, expand = TRUE)

  ##= Criacao das opcoes graficas
  checkboxframe <- gframe("Opcoes", container =Groupgraphic, horizontal = TRUE)
  glabel("Selecione antes do Plot", container= checkboxframe)
  pont <- gcheckbox("Indices de x", checked = FALSE, cont =checkboxframe)
  linsc <- gcheckbox("Linhas Secantes", checked = TRUE, cont = checkboxframe)
  linvt <- gcheckbox("Linhas verticais", checked = TRUE, cont= checkboxframe)

  ##= Criacao das area do zoom
  zoomGraphFrame <- gframe("Zoom do grafico principal", container = Groupbuttons, horizontal = FALSE)
  gg2<-ggraphics(container = zoomGraphFrame,  width = 220, height = 220)

  ##= Criacao da area do plot principal
  mainGrapghFrame <- gframe("Grafico Principal", container = Groupgraphic)
  gg<-ggraphics(container = mainGrapghFrame, width = 503, height = 503)

  ##= area de entrada dos dados
  functionframe <- gframe("Funcao", container = buttonsFrame, horizontal = TRUE)
  env_function<-gedit("",width = 50, cont = functionframe, initial.msg = "ex: 2*x + exp(x) - sin(x) + log(x)")

  intervalframe <- gframe("Intervalo", container = buttonsFrame, horizontal = TRUE)
  glabel("Intervalo em x:", container = intervalframe)
  env_pts<-gedit("", width = 14,cont = intervalframe, initial.msg = "Separados por espaco")

  intervalframe <- gframe("Chutes Inicial", container = buttonsFrame, horizontal = TRUE)
  glabel("x0 e x1:", container = intervalframe)
  env_init<-gedit("", width = 14,cont = intervalframe, initial.msg = "Separados por espaco")



  stopframe <- gframe("Precisao desejada", container= buttonsFrame, horizontal=TRUE)
  glabel("Decimais:", container = stopframe)
  env_stop<-gedit("",width = 10, cont = stopframe, initial.msg = "ex.: 1 para 0,1")
  glabel("Iteracoes:", container = stopframe)
  env_stpcont <- gedit("", width = 10, cont = stopframe, initial.msg = "Caso nao tenha digite 0")

  speedframe <- gframe("Velocidade da animacao", container= buttonsFrame, horizontal=TRUE)
  glabel("Tempo em segundos:", container = speedframe)
  env_speed<-gedit("",width = 35, cont = speedframe, initial.msg = "Intervalo de tempo entre as iteracoes")

  ##= criacao do botao de saida
  exit_func<-function(h,...){dispose(winsecante)}
  gbutton("SAIR", cont= Groupbuttons, handler = exit_func)

  ##= while utilizado na construcao da animacao
  while(isExtant(winsecante)) Sys.sleep(1)

  ##= Mudar o icone da janela
 # dir <- dirname(sys.frame(1)$ofile)
 # icon_dir <-paste0(dir, "/icon.png")
 # img <- gdkPixbufNewFromFile(icon_dir)
 # getToolkitWidget(winsecante)$setIcon(img$retval)
}
##########################################################################################

############################################################################################
#' Metodo dos Trapezios:
#' Ilustra as iteracoes feitas pelo metodo dos trapezios, metodo que aproxima o valor da integral de uma função em um dado intervalo.
#'
#' @param Funcao A equacao que descreve a funcao para a qual se deseja calcular a integral
#' @param Intervalo Intervalo onde sera feita a aproximacao da integral, separado por espaco, exemplo: -3 9.87
#' @param Numero_de_Intervalos Define em quantas vezes o intervalo dado sera dividido
#' @param Tempo Regula quanto tempo durara cada iteracao
#' @param OG_Indices Determina se os indices dos pontos serao exibidos
#' @param OG_Pintar_Area Determina se a area abaixo das linhas formadas pelo método devem ser pintadas ou nao
#' @param OG_Linhas_Verticais Determina se serao plotadas as linhas verticais associados a cada ponto
TRAPEZIOS <-function()
{
  #== Mensagem inicial na area de resultados
  valuetextm <- "Aproximacoes obtidas"

  ##================================================================
  ##========== FUNCOES


  #==== Funcao principal (que faz o metodo)
  integral<- function(h,...)
  {
    #== Valores de entrada
    f<-svalue(env_function)
    interentr<-svalue(env_entr)
    div<-as.numeric(svalue(env_div))
    speed<-as.numeric(svalue(env_speed))

    #============== pegar os valores separados em x =======
    interaux <- as.list(strsplit(interentr," ")[[1]]) # contador da quantidade de valores de entrada, serve tanto para o x quanto para y
    limitx <- c()
    for(i in 1:2){
      limitx[i] <- as.numeric(interaux[i])
    }

    #criando uma nova string: func <- function(x){ string da entrada}
    func <- paste("func <- function(x){",f,"}")
    func2 <- paste("func2 <- expression(",f,")")

    # transformando o texto salvo na variavel ftext em uma expressao
    eval(parse(text=func))
    eval(parse(text=func2))

    ##== Funcoes derivadas
    DevFunc2 <- function(x){eval(D(D(func2,"x"),"x"))}
    DevFunc3 <- function(x){eval(D(D(D(func2,"x"),"x"),"x"))}

    #=== funcao com o calculo da soma do metodo
    trapezoid <- function(fun, a, b, n) {
      h <- (b-a)/n
      x <- seq(a, b, by=h)
      y <- func(x)
      s <- h * (y[1]/2 + sum(y[2:n]) + y[n+1]/2)
      return(s)
    }

    ##= Variavel com a soma do metodo
    soma <- abs(trapezoid(func, limitx[1], limitx[2], div + 1))

    ##= Vetores com os valores de x
    pointx <- c()
    pointy <- c()
    pointx[1] <- limitx[1]
    pointy[1] <- func(pointx[1])
    h <- ((limitx[2] - limitx[1])/div)
    for(i in 2:(div+1)){
      pointx[i] <- (pointx[i-1] + h)
      pointy[i] <- func(pointx[i])
    }

    ##= limites do plot
    xmin <- limitx[1]
    xmax <- limitx[2]

    ##=== Calculo do erro
    #

    #= Achar os pontos criticos
    j<- 1
    crit <- c()
    for(i in seq(from=xmin, to=xmax, by=0.01)){
      if(((DevFunc3(i))>-0.1)&&(DevFunc3(i)<0.1)){
        crit[j] <- DevFunc2(i)
        j <- j + 1
      }
    }

    #= Calcular a derivada segunda dos extremos e seu maior valor de todas as derivadas segundas
    #
    Dev2min <- DevFunc2(xmin)
    Dev2max <- DevFunc2(xmax)

    if(Dev2min>Dev2max){
      valmax <- Dev2min
    }
    else {
      valmax <-Dev2max
    }
    if(j>1){
      for(i in 1:(j-1)){
        if(valmax < crit[i]){
          valmax <- crit[i]
        }
      }
    }

    ##= Calculo do erro
    Errotrap <- valmax*(((xmax-xmin)*(h^2))/(12))

    #vetor de zeros para o plot
    z_k <- rep(0, (div+1))

    #==== Plot do metodo
    #
    visible(gg) <- TRUE #a area grafica gg que passara a receber os plots
    plot(func, xlim=c(xmin, xmax), col = "red", xlab="Eixo x", ylab="Eixo y")#Plot da f(x)
    abline(h=0, lty=2)
    abline(v=0, lty=2)

    #animacao
    Sys.sleep(speed/4)

    points(pointx, pointy, col="blue", pch = 1) # Plot dos pontos x e f_x
    index <-c(0:(div)) #vetor com os indices para o plot

    if(svalue(pont)){ #Caso seja marcado os indices dos pontos no checkbox
      text(pointx,pointy,  index, cex=0.65, pos=3, col="blue")
    }
    if(svalue(linvt)){ #Caso seja marcado as retas verticais no checkbox
      segments(pointx, z_k, pointx, pointy, lty=2 , col = "gray48")
    }

    for (i in 1:(div+1)) #fazer as retas dos trapezios
    {
      Sys.sleep(speed/4)
      if(i!=(div+1)){  #If para que i chegue apenas ate div e nao quebre o codigo
        segments(pointx[i],pointy[i],pointx[i+1],pointy[i+1])
      }
    }


    #========= Pintar a area ==============
    if(svalue(pint)){
      for(i in 1:div){
        xini <- pointx[i] #=== x inicial ===
        xfin <- pointx[i+1] #==== x final
        cord.x <- c(xini,xini,xfin,xfin)
        cord.y <- c(0,pointy[i],pointy[i+1],0)
        polygon(cord.x,cord.y, col="skyblue", border = "skyblue")
      }

      for (i in 1:(div+1))
      {
        if(i!=(div+1)){  #If para que i chegue apenas ate div e nao quebre o codigo
          segments(pointx[i],pointy[i],pointx[i+1],pointy[i+1])
        }
      }

      if(svalue(linvt)){
        segments(pointx, z_k, pointx, pointy, lty=2 , col = "gray48")
      }

      #Resultados a serem mostrados ao usuario
      valuetextm <- paste0("\n valor da soma pelo metodo: ",soma,"\n", "O erro do metodo: ",Errotrap)
      dispose(xk_output)
      insert(xk_output,valuetextm)

    }
  }

  #===========================================================================
  #INTERFACE
  wintrapezio <- gwindow("Metodo dos Trapezios") #Criacao da janela

  ##= Criacao dos grupos
  Groupbuttons <- ggroup(container = wintrapezio, horizontal=FALSE)
  Groupgraphic <- ggroup(container = wintrapezio, horizontal=FALSE)

  ##= Cricao dos frames
  buttonsFrame <- gframe("Dados de Entrada", container = Groupbuttons, horizontal = FALSE)
  gbutton("Desenhe", cont= Groupbuttons, handler = integral)
  valueframe <- gframe("Resultados", container = Groupbuttons, hozizontal = TRUE, expand = TRUE)
  xk_output <- gtext("", container=valueframe, expand = TRUE, width = 300)

  ##= Criacao das opcoes graficas
  checkboxframe <- gframe("Opcoes", container =Groupgraphic, horizontal = TRUE)
  glabel("Selecione antes do Plot", container= checkboxframe)
  pont <- gcheckbox("Indices de x", checked = FALSE, cont =checkboxframe)
  pint <- gcheckbox("Pintar Area", checked = TRUE, cont = checkboxframe)
  linvt <- gcheckbox("Linhas verticais", checked = TRUE, cont= checkboxframe)

  ##= Criacao da area do plot principal
  mainGrapghFrame <- gframe("Grafico Principal", container = Groupgraphic)
  gg<-ggraphics(container = mainGrapghFrame, width = 500,height = 500)

  ##= area de entrada dos dados
  functionframe <- gframe("Funcao", container = buttonsFrame, horizontal = TRUE)
  env_function<-gedit("", cont = functionframe, initial.msg = "ex: 2*x + exp(x) - sin(x) + log(x)", expand = TRUE)

  intervalframe <- gframe("Intervalo", container = buttonsFrame, horizontal = TRUE)
  glabel("Limites em x:", container = intervalframe)
  env_entr<-gedit("",cont = intervalframe, initial.msg = "ex.: -2 3", expand = TRUE)

  stopframe <- gframe("Divisoes", container= buttonsFrame, horizontal=TRUE)
  glabel("Numero de intervalos:", container = stopframe)
  env_div<-gedit("", cont = stopframe, initial.msg = "ex.: 5", expand = TRUE)

  speedframe <- gframe("Velocidade da animacao", container= buttonsFrame, horizontal=TRUE)
  glabel("Tempo em segundos:", container = speedframe)
  env_speed<-gedit("", cont = speedframe, initial.msg = "Intervalo de tempo entre as iteracoes", expand = TRUE)

  ##= criacao do botao de saida
  exit_func<-function(h,...){dispose(wintrapezio)}
  gbutton("SAIR", cont= Groupbuttons, handler = exit_func)

  ##= while utilizado na construcao da animacao
  while(isExtant(wintrapezio)) Sys.sleep(1)

  ##= Mudar o icone da janela
 # dir <- dirname(sys.frame(1)$ofile)
  #icon_dir <-paste0(dir, "/icon.png")
 # img <- gdkPixbufNewFromFile(icon_dir)
 # getToolkitWidget(winbissection)$setIcon(img$retval)
}
###########################################################################################
#' Metodo de Simpson:
#' Ilustra as iteracoes feitas pelo metodo dos Simpson, que aproxima o valor da integral de uma funcao em um dado intervalo.
#'
#' @param Funcao A equacao que descreve a funcao para a qual se deseja calcular a integral
#' @param Intervalo Intervalo onde sera feita a aproximacao da integral, separado por espaco, exemplo: -3 9
#' @param Numero_de_Intervalos Define em quantas vezes o intervalo dado sera dividido
#' @param Tempo Regula quanto tempo durara cada iteracao
#' @param OG_Indices Determina se os indices dos pontos serao exibidos
#' @param OG_Pintar_Area Determina se a area abaixo das curvas formadas pelo metodo devem ser pintadas ou nao
#' @param OG_Linhas_Verticais Determina se serao plotadas as linhas verticais associados a cada ponto
SIMPSON <-function()
{
  #== Mensagem inicial na area de resultados
  valuetextm <- "Aproximacoes obtidas"

  ##================================================================
  ##========== FUNCOES

  #==== Funcao principal (que faz o metodo)
  integral<- function(h,...)
  {
    #== Valores de entrada
    f<-svalue(env_function)
    interentr<-svalue(env_entr)
    div<-as.numeric(svalue(env_div))
    speed<-as.numeric(svalue(env_speed))

    #============== pegar os valores separados em x =======
    interaux <- as.list(strsplit(interentr," ")[[1]])
    limitx <- c()
    for(i in 1:2){
      limitx[i] <- as.numeric(interaux[i])
    }

    # Criando strings de entrada
    func <- paste("func <- function(x){",f,"}")
    func2 <- paste("func2 <- expression(",f,")")

    # transformando o texto salvo em uma express?es
    eval(parse(text=func))
    eval(parse(text=func2))

    #=============== Funcoes derivadas ===============
    DevFunc4 <- function(x){eval(D(D(D(D(func2,"x"),"x"),"x"),"x"))}
    DevFunc5 <- function(x){eval(D(D(D(D(D(func2,"x"),"x"),"x"),"x"),"x"))}

    # Vetores para o plot
    pointx <- c()
    pointy <- c()


    if((div%%2)!=0) # Erro para caso o numero de intervalos seja impar
    {
      error.INTERIMP <- gwindow("Erro", width = 15)
      error.IIgt <- ggroup(horizontal = FALSE, container = error.INTERIMP)
      error.IIgb <- ggroup(horizontal = FALSE, container = error.INTERIMP)
      error.FAlabel <- glabel("O numero de intervalos e impar, favor forneca um numero par.", container=error.IIgt)
      exit.II <-function(h,...){dispose(error.INTERIMP)}
      gbutton("Ok", cont= error.IIgb, handler = exit.II)
      stop
    }

    else{
      #===Comeco do metodo em si
      #
      simpson <- function(fun, a, b, n) {
        h <- (b-a)/n
        x <- seq(a, b, by=h)
        y <- fun(x)
        s <- (h/3) * ( y[1] + y[n] + 4*(sum(y[2*(1:(n/2))+1])) + 2*(sum(y[2*(1:((n/2)-1))])) )
        return(s)
      }

      soma <- simpson(func, limitx[1], limitx[2], div)

      # ====Preenchendo os vetores para o plot
      pointx[1] <- limitx[1]
      pointy[1] <- func(pointx[1])
      h <- ((limitx[2] - limitx[1])/div)
      for(i in 2:(div+1)){
        pointx[i] <- (pointx[i-1] + h)
        pointy[i] <- func(pointx[i])
      }

      ##=== Plotar a funcao dentro do intervalo

      xmin <- limitx[1]
      xmax <- limitx[2]

      ##======== Calcular o erro ======
      ##
      j<- 1 #j ? uma variavel para encher o vetor critico
      crit <- c() #=== que ser? preenchido
      for(i in seq(from=xmin, to=xmax, by=0.01)){ ##=== Achar todos os pontos cr?ticos
        if(((DevFunc5(i))>-0.1)&&(DevFunc5(i)<0.1)){
          crit[j] <- DevFunc4(i)  #=== Encher o vetor crit quando encontrado
          j <- j + 1
        }
      }

      Dev4min <- DevFunc4(xmin) #Valor da f''(x) quando xmin
      Dev4max <- DevFunc4(xmax) #Valor da f''(x) quando xmax

      if(Dev4min>Dev4max){ #Achar qual e maior dos valores da f''(x) nos extremos
        valmax <- Dev4min
      }
      else {   #Achar qual e maior dos valores da f''(x) nos extremos
        valmax <-Dev4max
      }
      if(j>1){
        for(i in 1:(j-1)){ #Avaliar entre o valor dos extremos e os criticos da f''(x)
          if(valmax < crit[i]){
            valmax <- crit[i]
          }
        }
      }
      Errosimp<- - valmax*(((xmax-xmin)*(h^4))/(180)) #Erro propriamente

      #==== Plot do metodo
      #
      visible(gg) <- TRUE
      plot(func, xlim=c(xmin,xmax), col = "red", xlab="eixo x", ylab="eixo y")
      abline(h=0, lty=2)
      abline(v=0, lty=2)

      z_k <- rep(0, (div+1)) #Vetor de zeros
      index <-c(0:(div-1)) #Indice

      for (i in 1:(div+1))
      {

        if(svalue(linvt)){
          segments(pointx[i], z_k[i], pointx[i], pointy[i], lty=2 , col = "azure3")
        }
        points(pointx[i], pointy[i], col="blue", pch = 1) # Plot dos pontos x e f_x
        index <-c(0:(div))

        if(svalue(pont)){ #Caso seja marcado os indicies dos pontos no checkbox
          text(pointx[i],pointy[i],  index[i], cex=0.65, pos=3, col="blue")
        }
      }
      #Vetor para o plot das parabolas
      Fpoli<- c()

      #============== Plot das parabolas ================#
      for(i in seq(from=1, to=(div-1), by=2)){
        Sys.sleep(speed)
        Amatr <- array(c((pointx[i])^2,(pointx[i+1])^2,(pointx[i+2])^2,pointx[i],pointx[i+1],pointx[i+2],1,1,1),c(3,3))#Matrix com pontos
        Ypon <- c(pointy[i],pointy[i+1],pointy[i+2]) #=== Vetor com os valores em y
        Ainver <- solve(Amatr) #=== Matriz inversa para resolu??o
        ABCMatr <- Ainver %*% Ypon #=== matriz com os valores A, B e C, do polinomio que esta sendo feito
        Fpoli[i] <- paste0(ABCMatr[1],"*x^2+",ABCMatr[2],"*x+",ABCMatr[3]) #==== Juntando o valor da matriz inversa com o A B e C
        polifun <- paste("polifun <- function(x){",Fpoli[i],"}")
        eval(parse(text=polifun))
        curve(polifun , (pointx[i]-h*0.15), (pointx[i+2]+h*0.15),type="l", add=TRUE)
      }

      #========= Pintar a area ============#
      if(svalue(pint)){
        Sys.sleep(speed)
        for(i in seq(from=1, to=(div-1), by=2)){
          polifun <- paste("polifun <- function(x){",Fpoli[i],"}")
          eval(parse(text=polifun))
          xini <- pointx[i] #=== x inicial ===
          xfin <- pointx[i+2] #==== x final
          cord.x <- c(xini,seq(xini,xfin,0.01),xfin)
          cord.y <- c(0,polifun(seq(xini,xfin,0.01)),0)
          polygon(cord.x,cord.y, col="skyblue", border = "skyblue")
          curve(polifun , (pointx[i]-h*0.15), (pointx[i+2]+h*0.15),type="l", add=TRUE)
        }
      }

      #Plot da funcao de novo, para ficar a cima dos outros plots
      curve(func, xmin -1, xmax +1, col = "red", xlab="eixo x", ylab="eixo y", lwd=2, add=TRUE)
      abline(h=0, lty=2)

      #Resultados a serem mostrados ao usuario
      valuetextm <- paste0("O resultado da soma do m?todo ?:",soma,"\n","O erro do m?todo ?:", Errosimp,"\n")
      insert(xk_output,valuetextm)

    }
  }

  #===========================================================================
  #INTERFACE
  winsimpson <- gwindow("Metodo de Simpson") #Criacao da janela

  ##= Criacao dos grupos
  Groupbuttons <- ggroup(container = winsimpson, horizontal=FALSE)
  Groupgraphic <- ggroup(container = winsimpson, horizontal=FALSE)

  ##= Cricao dos frames
  buttonsFrame <- gframe("Dados de Entrada", container = Groupbuttons, horizontal = FALSE)
  gbutton("Desenhe", cont= Groupbuttons, handler = integral)
  valueframe <- gframe("Resultados", container = Groupbuttons, hozizontal = TRUE)
  xk_output <- gtext("", container=valueframe, expand = TRUE, width = 300)

  ##= Criacao das opcoes graficas
  checkboxframe <- gframe("Opcoes", container =Groupgraphic, horizontal = TRUE)
  glabel("Selecione antes do Plot", container= checkboxframe)
  pont <- gcheckbox("Indices de x", checked = FALSE, cont =checkboxframe)
  pint <- gcheckbox("Pintar area", checked = TRUE, cont = checkboxframe)
  linvt <- gcheckbox("Linhas verticais", checked = TRUE, cont= checkboxframe)

  ##= Criacao da area do plot principal
  mainGrapghFrame <- gframe("Grafico Principal", container = Groupgraphic)
  gg<-ggraphics(container = mainGrapghFrame, width = 500, height = 500)

  ##= area de entrada dos dados
  functionframe <- gframe("Funcao", container = buttonsFrame, horizontal = TRUE)
  env_function<-gedit("",width = 50, cont = functionframe, initial.msg = "ex: 2*x + exp(x) - sin(x) + log(x)")

  intervalframe <- gframe("Intervalo", container = buttonsFrame, horizontal = TRUE)
  glabel("Limites em x:", container = intervalframe)
  env_entr<-gedit("", width = 14,cont = intervalframe, initial.msg = "ex.: -2 3")

  stopframe <- gframe("Divisoes", container= buttonsFrame, horizontal=TRUE)
  glabel("Numero de intervalos:", container = stopframe)
  env_div<-gedit("",width = 20, cont = stopframe, initial.msg = "ex.: 5")

  speedframe <- gframe("Velocidade da animacao", container= buttonsFrame, horizontal=TRUE)
  glabel("Tempo em segundos:", container = speedframe)
  env_speed<-gedit("",width = 35, cont = speedframe, initial.msg = "Intervalo de tempo entre as iteracoes")

  ##= criacao do botao de saida
  exit_func<-function(h,...){dispose(winsimpson)}
  gbutton("SAIR", cont= Groupbuttons, handler = exit_func)

  ##= while utilizado na construcao da animacao
  while(isExtant(winsimpson)) Sys.sleep(1)

  ##= Mudar o icone da janela
  #dir <- dirname(sys.frame(1)$ofile)
  #icon_dir <-paste0(dir, "/icon.png")
 # img <- gdkPixbufNewFromFile(icon_dir)
  #getToolkitWidget(winbissection)$setIcon(img$retval)
}
###########################################################################################
#' Interpolacao por Lagrange - Pontos:
#' Interpola polinomialmente um conjunto de pontos, e a partir da interpolacao obtida aproxima o valor da funcao em um dado ponto.
#'
#' @param Valores_em_x As coordenadas no eixo x dos pontos utilizados na interpolacao, separados por espaco e na ordem crescente, exemplo.: -9 -3 0.58 8 22
#' @param Valores_em_y As coordenadas no eixo y dos pontos utilizados na interpolacao, separados por espaco, a quantidade de termos deve ser igual a do eixo x
#' @param Pto_aproximado valor de x para o qual a funcao sera aproximada pela polinomio interpolador
#' @param Intervalo_x Define qual o intervalo do plot no eixo x, separados por espaco e na ordem crescente
#' @param OG_Indice Determina se os indices das aproximacoes obtidas em cada iteracao serao exibidos ou não
#' @param OG_Animacao Determina se a funcao interpolada sera visualizada com animacao ou sera plotada de uma so vez
#' @param OG_Linhas_Verticais  Determina se as linhas verticais ligando o ponto da funcao no eixo x serao exibidas ou nao
INTERPOLACAOPONTOS <-function()
{
  #== Mensagem inicial na area de resultados
  valuetextm <- "Aproximacoes obtidas"

  ##================================================================
  ##========== FUNCOES

  #==== Funcao principal (que faz o metodo)
  polinomial<- function(h,...)
  {
    #== Valores de entrada
    valaprm <- as.numeric(svalue(env_aprm))
    interentrx <- svalue(env_intervalx)
    interentry <- svalue(env_intervaly)
    xentr <- svalue(env_points_x)
    yentr <- svalue(env_points_y)

    #=== pegar os valores separados em x
    #
    valxaux <- as.list(strsplit(xentr," ")[[1]])
    contval <- length(valxaux) # contador da quantidade de valores de entrada, tanto x quanto para y
    valx <- c()
    for(i in 1:contval){
      valx[i] <- as.numeric(valxaux[i])
    }

    #=== pegar os valores separados em y
    #
    valyaux <- as.list(strsplit(yentr," ")[[1]])
    valy <- c()
    for(i in 1:contval){
      valy[i] <- as.numeric(valyaux[i])
    }

    #=== Pegas os extremos em x
    #
    interauxx <- as.list(strsplit(interentrx," ")[[1]])
    plotintervalx <- c()
    for(i in 1:2){
      plotintervalx[i] <- as.numeric(interauxx[i])
    }

    #=== Pegas os extremos em y
    #
    interauxy <- as.list(strsplit(interentry," ")[[1]])
    plotintervaly <- c()
    for(i in 1:2){
      plotintervaly[i] <- as.numeric(interauxy[i])
    }


    ##=== Criar o polinomio de lagrange
    #
    lagrange <- rep(1,contval) # encher o vetor de 1, elemento neutro na multipl.
    for(j in 1:contval){ #======== for que termina o grau do polinomio
      for(i in 1:contval){ #======= for para fazer cada L
        if(i!=j){ #===== If para n?o acontecer de de i==j
          lagrange[j] <- paste0(lagrange[j], "*", "(x -",valx[i],")/(", valx[j]-valx[i],")")
        }
      }
    }

    ##=== Juntar os yi com os li e montar o polinomio
    polinomio <- 0
    for(i in 1:contval){
      polinomio <- paste(polinomio, "+",valy[i],"*",lagrange[i])
    }


    f_text <- paste("PolLagrange<- function(x){",polinomio,"}")# Criando string de entrada
    eval(parse(text=f_text))# Transformando o texto salvo na variavel ftext em uma expressao


    Valaprmy <- PolLagrange(valaprm)# Valor no polinomio do ponto a ser aproximado

    #==== Plot do metodo
    #
    #=Pontos maximos e minimos
    x_min <- plotintervalx[1]
    x_max <- plotintervalx[2]
    y_min <- plotintervaly[1]
    y_max <- plotintervaly[2]

    visible(gg) <- TRUE #Especificar que a area grafica gg recebe o grafico
    plot(valx, valy, xlim= c(x_min,x_max),ylim= c(y_min,y_max),  xlab ="Eixo x", ylab="Eixo y",col="blue", pch=1) #== plot dos pontos
    #abline(h=0, lty=2, col ="azure2")
    #abline(v=0, lty=2, col ="azure2")

    abline(h=0, lty=2)
    abline(v=0, lty=2)

    index <-c(0:(contval-1)) #= Indicies dos pontos
    if(svalue(pont)){ #Caso seja marcado os indicies dos pontos no checkbox
      text(valx,valy, index, cex=0.65, pos=3, col="blue")
    }

    for (i in 1:(contval))
    {
      if(svalue(linvt)){ #Caso seja marcado as linhas verticais no checkbox
        segments(valx[i],0,valx[i],valy[i], col= "azure4", lty=2)
      }
    }


    ##= Plot do polinomio de lagrange

    if(svalue(anim)){# Caso a opcao de animacao esteja marcada
      kmax <- 100
      for(k in 1:kmax){ #desenhar o polinomio com o tempo
        Sys.sleep(1/8)
        l <- (k-1)/(kmax -1)
        x0 <- x_min
        xk <- (1-l)*x_min + l*x_max

        curve(PolLagrange,xlim= c(x0, xk), col = "red", add = TRUE) #plot do polinomio
      }
    }

    else{ #sem animacao
      curve(PolLagrange,xlim= c(plotintervalx[1], plotintervalx[2]), col = "red", xlab="eixo x", ylab="eixo y", add = TRUE) #plot do polinomio
    }


    ##==Plot do ponto aproximado
    Sys.sleep(2/3)
    segments(valaprm,0,valaprm,Valaprmy, col="azure4", lty = 2)
    points(valaprm, Valaprmy, col="chartreuse4", pch=9, cex=2)
    if(svalue(pont)){ #Caso seja marcado os indicies dos pontos no checkbox
      text(valaprm,Valaprmy, Valaprmy, cex=0.65, pos=3, col="chartreuse4")
    }

    #Resultados a serem mostrados ao usuario
    valuetextm <- paste0("O valor aproximado pela interpolacao e: ",Valaprmy,"\n")
    insert(xk_output,valuetextm)

  }


  #===========================================================================
  #INTERFACE
  winpolinomial <- gwindow("Interpolacao por Lagrange - Pontos ") #Criacao da janela

  ##= Criacao dos grupos
  Groupbuttons <- ggroup(container = winpolinomial, horizontal=FALSE)
  Groupgraphic <- ggroup(container = winpolinomial, horizontal=FALSE)

  ##= Criacao dos frames
  buttonsFrame <- gframe("Dados de Entrada", container = Groupbuttons, horizontal = FALSE)
  gbutton("Desenhe", cont= Groupbuttons, handler = polinomial)
  valueframe <- gframe("Resultado", container = Groupbuttons, hozizontal = TRUE, expand = TRUE)
  xk_output <- gtext("", container=valueframe, expand = TRUE, width = 220, height = 100, expand=TRUE)

  ##= Criacao das opcoes graficas
  checkboxframe <- gframe("Opc?es", container =Groupgraphic, horizontal = TRUE)
  glabel("Selecione antes do Plot", container= checkboxframe)
  pont <- gcheckbox("Indices de x", checked = FALSE, cont =checkboxframe)
  anim <- gcheckbox("Animacao da funcao", checked=TRUE, cont=checkboxframe)
  linvt <- gcheckbox("Linhas verticais", checked = TRUE, cont= checkboxframe)


  ##= Criacao da area do plot
  mainGrapghFrame <- gframe("Grafico Principal", container = Groupgraphic)
  gg<-ggraphics(container = mainGrapghFrame, width = 500, height = 500)

  ##= area de entrada dos dados
  pointsframe <- gframe("Pontos", container = buttonsFrame, horizontal = FALSE)
  glabel("Valores em x", container = pointsframe)
  env_points_x<-gedit("",width = 50, cont = pointsframe, initial.msg = "Separados por espaco")
  glabel("Valores em y", container = pointsframe)
  env_points_y<-gedit("", width = 50,cont = pointsframe, initial.msg = "Separados por espaco")

  pointframe <- gframe("Aproximacao", container= buttonsFrame, horizontal=TRUE)
  glabel("Ponto a ser aproximado", container = pointframe)
  env_aprm<-gedit("",width = 15, cont = pointframe, initial.msg = "ex.: 3.25")

  intervalframe <- gframe("Intervalo", container= buttonsFrame, horizontal=TRUE)
  glabel("Intervalo nos eixos: ", container = intervalframe)
  env_intervalx<-gedit("",width = 20, cont = intervalframe, initial.msg = "sobre o eixo x")
  env_intervaly<-gedit("",width = 20, cont = intervalframe, initial.msg = "Sobre o eixo y")

  ##= Criacao do botao de saida
  exit_func<-function(h,...){dispose(winpolinomial)}
  gbutton("SAIR", cont= Groupbuttons, handler = exit_func)

  ##= while utilizado na construcao da animacao
  while(isExtant(winpolinomial)) Sys.sleep(1)

  ##= Mudar o icone da janela
#  dir <- dirname(sys.frame(1)$ofile)
#  icon_dir <-paste0(dir, "/icon.png")
#  img <- gdkPixbufNewFromFile(icon_dir)
 # getToolkitWidget(winpolinomial)$setIcon(img$retval)
}
###########################################################################################
#' Interpolacao por Lagrange - Funcao:
#' Interpola polinomialmente pontos de uma dada funcao, e a partir da interpolacao obtida aproxima o valor da funcao em um dado ponto.
#'
#' @param Funcao A funcao que sera usada para dar os pontos em y e entao ser feita a interpolacao
#' @param Valores_em_x As coordenadas em x dos pontos utilizados na interpolacao, separados por espaco e em ordem crescente, exemplo: -5.58 -1 2.2 8
#' @param Pto_aproximado Escolha de qual ponto quer ser aproximado a partir da aproximacao pela interpolacao
#' @param Intervalo_x Define qual o intervalo do plot no eixo x, separados por espaco e na ordem crescente
#' @param Intervalo_y Define qual o intervalo do plot no eixo y, separados por espaco e na ordem crescente
#' @param OG_Indice Determina se os indices das aproximacoes obtidas em cada iteracao serao exibidos ou não
#' @param OG_Animacao Determina se a funcao interpolada sera visualizada com animacao ou sera plotada de uma so vez
#' @param OG_Linhas_Verticais  Determina se as linhas verticais ligando o ponto da funcao no eixo x serao exibidas ou nao
INTERPOLACAOFUNCAO <-function()
{
  #== Mensagem inicial na area de resultados
  valuetextm <- "Aproximacoes obtidas"

  ##================================================================
  ##========== FUNCOES


  #==== Funcao principal (que faz o metodo)
  polinomial2 <- function(h,...)
  {
    #== Valores de entrada
    valaprm2 <- as.numeric(svalue(env_aprm2))
    interentrx2 <- svalue(env_intervalx2)
    interentry2 <- svalue(env_intervaly2)
    f <- svalue(env_function2)
    pointsentr <- svalue(env_points2)

    func <- paste("func <- function(x){",f,"}")# Criando string de entrada
    eval(parse(text=func))# Transformando o texto salvo na variavel ftext em uma expressao

    #=== pegar os valores separados em x
    #
    valxaux2 <- as.list(strsplit(pointsentr," ")[[1]])
    contval2 <- length(valxaux2) # contador da quantidade de valores de entrada, tanto x quanto para y
    valx2 <- c()
    for(i in 1:contval2){
      valx2[i] <- as.numeric(valxaux2[i])
    }

    #=== pegar os valores separados em y
    #
    valy2<-c()
    for(i in 1:contval2){
      valy2[i] <- func(valx2[i])
    }

    #=== Pegas os extremos em x
    #
    interauxx2 <- as.list(strsplit(interentrx2," ")[[1]])
    plotintervalx2 <- c()
    for(i in 1:2){
      plotintervalx2[i] <- as.numeric(interauxx2[i])
    }

    #=== Pegas os extremos em y
    #
    interauxy2 <- as.list(strsplit(interentry2," ")[[1]])
    plotintervaly2 <- c()
    for(i in 1:2){
      plotintervaly2[i] <- as.numeric(interauxy2[i])
    }


    ##=== Criar o polinomio de lagrange
    #
    lagrange2 <- rep(1,contval2) # encher o vetor de 1, elemento neutro na multipl.
    for(j in 1:contval2){ # for que determina o grau do polinomio
      for(i in 1:contval2){ # for para fazer cada L
        if(i!=j){ # If para n?o acontecer de de i==j
          lagrange2[j] <- paste0(lagrange2[j], "*", "(x -",valx2[i],")/(", valx2[j]-valx2[i],")")
        }
      }
    }

    ##=== Juntar os yi com os li e montar o polinomio
    polinomio2 <- 0
    for(i in 1:contval2){
      polinomio2 <- paste(polinomio2, "+",valy2[i],"*",lagrange2[i])
    }

    f_text <- paste("PolLagrange2<- function(x){",polinomio2,"}")# Criando string de entrada
    eval(parse(text=f_text))# Transformando o texto salvo na variavel ftext em uma expressao

    Valaprmy2 <- PolLagrange2(valaprm2) # Valor no polinomio do ponto a ser aproximado
    valrealy2 <- func(valaprm2) # Valor na funcao do ponto a ser aproximado

    #==== Plot do metodo
    #

    #=Pontos maximos e minimos
    x_min <- plotintervalx2[1]
    x_max <- plotintervalx2[2]
    y_min <- plotintervaly2[1]
    y_max <- plotintervaly2[2]

    visible(gg) <- TRUE #Agora a area grafica gg que ira receber o plot
    plot(func, xlim=c(x_min,x_max), ylim= c(y_min,y_max),xlab=("Eixo x"),ylab=("Eixo y"), col="black")  #Plot da f(x)
    abline(h=0, lty=2)
    abline(v=0, lty=2)

    Sys.sleep(1/3)
    points(valx2,valy2, col="blue",pch=1)  #Plot dos pontos na f(x)

    if(svalue(pont)){ #Caso seja marcado os indicies dos pontos no checkbox
      index <-c(0:(contval2-1)) #= Indicies dos pontos
      text(valx2,valy2, index, cex=0.65, pos=3, col="blue")
    }
    if(svalue(linvt)) {
      for (i in 1:(contval2)){ #Caso seja marcado as linhas verticais no checkbox
        segments(valx2[i],0,valx2[i],valy2[i], col= "azure4", lty=2)
      }
    }


    ##= Plot do polinomio de lagrange

    if(svalue(anim)){# Caso a opcao de animacao esteja marcada
      kmax <- 100
      for(k in 1:kmax){ #desenhar o polinomio com o tempo
        Sys.sleep(1/8)
        l <- (k-1)/(kmax -1)
        x0 <- x_min
        xk <- (1-l)*x_min + l*x_max

        curve(PolLagrange2, xlim= c(x0, xk), col = "red", xlab="eixo x", ylab="eixo y", add = TRUE) #= plot do polinonmio
      }
    }
    else{ #sem animacao
      curve(PolLagrange2, xlim= c(plotintervalx2[1], plotintervalx2[2]), col = "red", xlab="eixo x", ylab="eixo y", add = TRUE) #= plot do polinonmio
    }

    ##==Plot do ponto aproximado
    Sys.sleep(1/2)
    points(valaprm2, Valaprmy2, col="chartreuse4", pch=9) #=  valor no polinomio
    text(valaprm2,Valaprmy2, Valaprmy2, cex=0.65, pos=3, col="chartreuse4")
    Sys.sleep(1/2)
    points(valaprm2, valrealy2, col="chartreuse4", pch=9) #= valor na f(x)
    text(valaprm2,valrealy2, valrealy2, cex=0.65, pos=3, col="chartreuse4")
    Sys.sleep(1/2)
    segments(valaprm2,Valaprmy2, valaprm2, valrealy2, col = "chartreuse4", lty=2)


    # Plot do zoom
    #
    if(Valaprmy2 < valrealy2){  # fazer com que a diferen?a fique centralizada
      yplot_min <- Valaprmy2
      yplot_max <- valrealy2
    }
    else{
      yplot_min <- valrealy2
      yplot_max <- Valaprmy2
    }

    visible(gg22) <- TRUE #agora a area grafica gg2 que ira receber o plot
    par(mar = rep(2,4)) #margem

    plot(PolLagrange2, xlim= c(valaprm2 - 0.5, valaprm2 + 0.5), ylim= c(yplot_min - 0.5,yplot_max + 0.5), col="red")#plot do polinomio
    points(valaprm2, Valaprmy2, add = TRUE, col = "chartreuse4",  pch = 9) #= plot do ponto aproximado no polinimio
    points(valaprm2, valrealy2, add = TRUE, col = "chartreuse4",  pch = 9) #= plot do ponto aproximado na f(c)
    segments(valaprm2, valrealy2, valaprm2, Valaprmy2, col = "chartreuse4", add=TRUE) #= segmento de diferen?a entre os 2 pontos
    curve(func, col = "black", xlab="", ylab="", add= TRUE) #= plot funcao dada

    if(svalue(pont)){ #Caso seja marcado os pontos no checkbox
      text(valx2,valy2,  index, cex=0.65, pos=3, col="blue")
    }

    #Resultados a serem mostrados ao usuario
    valerro <- (abs(Valaprmy2 - valrealy2)) # Calculo do erro absoluto
    valuetextm2 <- paste0("O valor achado pelo m?todo ?: ",Valaprmy2,"\n", "O valor absoluto do erro ?: ",valerro)
    insert(xk_output2,valuetextm2)
  }


  #===========================================================================
  #INTERFACE
  winpolinomial <- gwindow("Interpoladqo por Lagrange - Funcao ") #Criacao da janela

  ##= Criacao dos grupos
  Groupbuttons2 <- ggroup(container = winpolinomial, horizontal=FALSE)
  Groupgraphic <- ggroup(container = winpolinomial, horizontal=FALSE)

  ##= Criacao dos frames
  buttonsFrame <- gframe("Dados de Entrada", container = Groupbuttons2, horizontal = FALSE)
  gbutton("Desenhe", cont= Groupbuttons2, handler = polinomial2)
  valueframe2 <- gframe("Resultados", container = Groupbuttons2, hozizontal = TRUE, expand = TRUE)
  xk_output2 <- gtext("", container=valueframe2, expand = TRUE, width = 220, height = 60, expand = TRUE)

  ##= Criacao das opcoes graficas
  checkboxframe <- gframe("Opcoes", container =Groupgraphic, horizontal = TRUE)
  glabel("Selecione antes do Plot", container= checkboxframe)
  pont <- gcheckbox("Indices de x", checked = FALSE, cont =checkboxframe)
  anim <-gcheckbox("Animacao da funcao", checked=TRUE, cont=checkboxframe)
  linvt <- gcheckbox("Linhas verticais", checked = TRUE, cont= checkboxframe)

  ##= Criacao da area do plot principal
  mainGrapghFrame <- gframe("Grafico Principal", container = Groupgraphic) #= grafico principal
  gg<-ggraphics(container = mainGrapghFrame, width = 500, height = 500) #= area grafia gg (principal)

  ##= Area do zoom
  zoomGraphFrame2 <- gframe("Zoom do grafico principal", container = Groupbuttons2, horizontal = FALSE)
  gg22<-ggraphics(container = zoomGraphFrame2,  width = 220, height = 220)

  ##= area de entrada dos dados
  functionframe2 <- gframe("Funcao", container = buttonsFrame, horizontal = TRUE)
  env_function2<-gedit("",width = 50, cont = functionframe2, initial.msg = "ex: 2*x + exp(x) - sin(x) + log(x)")

  pointsframe2 <- gframe("Pontos em x", container = buttonsFrame, horizontal = TRUE)
  glabel("Valores de x", container = pointsframe2)
  env_points2<-gedit("", width = 45,cont = pointsframe2, initial.msg = "Separados por espaco")

  pointframe2 <- gframe("Aproximacao", container= buttonsFrame, horizontal=TRUE)
  glabel("Ponto a ser aproximado", container = pointframe2)
  env_aprm2<-gedit("",width = 10, cont = pointframe2, initial.msg = "ex.: 4.23")

  intervalframe2 <- gframe("Intervalo", container= buttonsFrame, horizontal=TRUE)
  glabel("Intervalo nos eixos: ", container = intervalframe2)
  env_intervalx2<-gedit("",width = 20, cont = intervalframe2, initial.msg = "sobre o eixo x")
  env_intervaly2<-gedit("",width = 20, cont = intervalframe2, initial.msg = "Sobre o eixo y")

  #== Criacao do botao de saida
  exit_func<-function(h,...){dispose(winpolinomial)} #= funcao de saida
  gbutton("SAIR", cont= Groupbuttons2, handler = exit_func) #= botao de saida

  ##= while utilizado na construcao da animacao
  while(isExtant(winpolinomial)) Sys.sleep(1)

  ##= Mudar o icone da janela
  #dir <- dirname(sys.frame(1)$ofile)
  #icon_dir <-paste0(dir, "/icon.png")
 # img <- gdkPixbufNewFromFile(icon_dir)
  #getToolkitWidget(winpolinomial)$setIcon(img$retval)
}
###########################################################################################
#' Aproximacao por Taylor:
#' Obtem uma aproximação local para dada funcao, através da construcao dos polinomios de Taylor
#'
#' Atraves da derivada de um ponto dado de uma funcao sao interpolados polinomios de grau 1 a 5 para aproximar um outro ponto
#'
#' @param Funcao A que ser utilizada como base
#' @param Pto_usado Ponto que sera utilizado para pegar a derivada e aproximar as interpolacoes
#' @param Pto_aproximado Escolha de qual ponto quer ser aproximado a partir da aproximacao pela interpolacao
#' @param Intervalo_x Define qual o intervalo do plot no eixo x, separados por espaco e na ordem crescente
#' @param Tempo Define o tempo entre cada interpolacao
#' @param OG_Grau Seleciona quais grais serao feitos e plotados
TAYLOR <-function()
{
  #== Mensagem inicial na area de resultados
  valuetextm <- "Aproximacoes obtidas"

  ##================================================================
  ##========== FUNCOES

  #==== Funcao principal (que faz o metodo)
  taylorfun<- function(h,...)
  {
    #== Valores de entrada
    f<-svalue(env_function)
    valapr<-as.numeric(svalue(env_entr))
    valenv <- as.numeric(svalue(env_val))
    limix <- svalue(env_limx)
    speed<-as.numeric(svalue(env_speed))

    #============== pegar os valores separados em x =======
    interaux <- as.list(strsplit(limix," ")[[1]])
    limitx <- c()
    for(i in 1:2){
      limitx[i] <- as.numeric(interaux[i])
    }


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
    visible(gg) <- TRUE #Agora a area grafica gg que ira receber o plot
    par(xpd = T, mar = c(0,0,0.5,5)) #= margem
    plot(func, limitx[1],limitx[2], col = "red", lwd= 3,xaxt='n',yaxt='n',ann=FALSE,bty='n') #Plot da f(x)
    legend(limitx[2] +(limitx[2]-limitx[1])*0.06, func(limitx[2]), c("Grau 1", "Grau 2", "Grau 3", "Grau 4", "Grau 5"),col = c("chartreuse3", "aquamarine3", "coral3", "deeppink2","midnightblue"),cex = 0.8,lwd = 1, lty = 1) #= legenda
    abline(h=0, lty=2)
    abline(v=0, lty=2)

    #======= Plot dos polinomios de taylor
    #
    #==== Caso o grau nao seja marcado come?ar como nulo
    resulpol1 <- ""
    resulpol2 <- ""
    resulpol3 <- ""
    resulpol4 <- ""
    resulpol5 <- ""
    #==============

    if(svalue(grau1)){
      Sys.sleep(speed) #Anima??o
      #===== Plot
      Fpoli <- fnpol[1]
      poli1 <- paste("poli1 <-function(x){",Fpoli,"}")
      eval(parse(text=poli1))
      curve(poli1, type="l", add=TRUE, col= "chartreuse3")
      #===== Plot do ponto no polinomio
      points(valenv, poli1(valenv), col="blue", pch = 1)
      #===== Colocar o resultado na janela
      resulpol1 <- paste0("\n","No grau 1: ",poli1(valenv))
    }
    if(svalue(grau2)){
      Sys.sleep(speed)
      Fpoli <- fnpol[2]
      poli2 <- paste("poli2 <-function(x){",Fpoli,"}")
      eval(parse(text=poli2))
      curve(poli2, type="l", add=TRUE, col ="aquamarine3")
      points(valenv, poli2(valenv), col="blue", pch = 1)
      resulpol2 <- paste0("\n","No grau 2: ",poli2(valenv))
    }
    if(svalue(grau3)){
      Sys.sleep(speed)
      Fpoli <- fnpol[3]
      poli3 <- paste("poli3 <-function(x){",Fpoli,"}")
      eval(parse(text=poli3))
      curve(poli3, type="l", add=TRUE, col="coral3")
      points(valenv, poli3(valenv), col="blue", pch = 1)
      resulpol3 <- paste0("\n","No grau 3: ",poli3(valenv))
    }
    if(svalue(grau4)){
      Sys.sleep(speed)
      Fpoli <- fnpol[4]
      poli4 <- paste("poli4 <-function(x){",Fpoli,"}")
      eval(parse(text=poli4))
      curve(poli4, type="l", add=TRUE, col="deeppink2")
      points(valenv, poli4(valenv), col="blue", pch = 1)
      resulpol4 <- paste0("\n","No grau 4: ",poli4(valenv))
    }
    if(svalue(grau5)){
      Sys.sleep(speed)
      Fpoli <- fnpol[5]
      poli5 <- paste("poli5 <-function(x){",Fpoli,"}")
      eval(parse(text=poli5))
      curve(poli5, type="l", add=TRUE, col="midnightblue")
      points(valenv, poli5(valenv), col="blue", pch = 1)
      resulpol5 <- paste0("\n","No grau 5: ",poli5(valenv))
    }

    #==== Guardar o resultado na variavel e plotar
    resul <- func(valenv)
    points(valenv, resul, pch=3)

    #Resultados a serem mostrados ao usuario
    valuetextm <- paste0("\n","O valor na funcao: ",resul,resulpol1,resulpol2,resulpol3,resulpol4,resulpol5)
    insert(xk_output,valuetextm)

  }

  #===========================================================================
  #INTERFACE
  wintaylor <- gwindow("Aproximacao por Taylor") #Criacao da janela

  ##= Criacao dos grupos
  Groupbuttons <- ggroup(container = wintaylor, horizontal=FALSE)
  Groupgraphic <- ggroup(container = wintaylor, horizontal=FALSE)

  ##= Cricao dos frames
  buttonsFrame <- gframe("Dados de Entrada", container = Groupbuttons, horizontal = FALSE)
  gbutton("Desenhe", cont= Groupbuttons, handler = taylorfun)
  valueframe <- gframe("Resultados", container = Groupbuttons, hozizontal = TRUE, expand = TRUE)
  xk_output <- gtext("", container=valueframe, width = 220, expand=TRUE)

  ##= Criacao das opcoes graficas
  checkboxframe <- gframe("Opcoes", container =Groupgraphic, horizontal = TRUE)
  glabel("Grau do polinomio", container= checkboxframe)
  grau1 <- gcheckbox("Grau 1", checked = TRUE, cont = checkboxframe)
  grau2 <- gcheckbox("Grau 2", checked = TRUE, cont = checkboxframe)
  grau3 <- gcheckbox("Grau 3", checked = TRUE, cont = checkboxframe)
  grau4 <- gcheckbox("Grau 4", checked = FALSE, cont = checkboxframe)
  grau5 <- gcheckbox("Grau 5", checked = FALSE, cont = checkboxframe)

  ##= Criacao da area do plot
  mainGrapghFrame <- gframe("Grafico Principal", container = Groupgraphic)
  gg<-ggraphics(container = mainGrapghFrame, width = 500, height = 500)

  ##= area de entrada dos dados
  functionframe <- gframe("Funcao", container = buttonsFrame, horizontal = TRUE)
  env_function<-gedit("", cont = functionframe, initial.msg = "ex: 2*x + exp(x) - sin(x) + log(x)",expand = TRUE)

  intervalframe <- gframe("Aproximacao", container = buttonsFrame, horizontal = TRUE)
  glabel("Ponto a ser usado:", container = intervalframe)
  env_entr<-gedit("", width = 7,cont = intervalframe, initial.msg = "ex.: 0")
  glabel("Ponto a ser aproximado:", container = intervalframe)
  env_val<-gedit("", width = 7,cont = intervalframe, initial.msg = "ex.: 0.75")

  stopframe <- gframe("Limites", container= buttonsFrame, horizontal=TRUE)
  glabel("Limite em x:", container = stopframe)
  env_limx<-gedit("",width = 15, cont = stopframe, initial.msg = "ex.: -5 5")

  speedframe <- gframe("Velocidade da animacao", container= buttonsFrame, horizontal=TRUE)
  glabel("Tempo em segundos:", container = speedframe)
  env_speed<-gedit("", cont = speedframe, initial.msg = "Intervalo de tempo entre as iteracoes", expand = TRUE)

  ##= criacao do botao de saida
  exit_func<-function(h,...){dispose(wintaylor)}
  gbutton("SAIR", cont= Groupbuttons, handler = exit_func)

  ##= while utilizado na construcao da animacao
  while(isExtant(wintaylor)) Sys.sleep(1)

  ##= Mudar o icone da janela
  #dir <- dirname(sys.frame(1)$ofile)
 # icon_dir <-paste0(dir, "/icon.png")
 # img <- gdkPixbufNewFromFile(icon_dir)
  #getToolkitWidget(winbissection)$setIcon(img$retval)
}

JANELAPRINCIPAL<-function(...)
{

    linkfun <- function(h,...){
        browseURL("http://liscustodio.github.io/CnVisual")
    }

    choose <- c("----------------","Zero de funcoes:", "   Bissecao","   Falsa Posicao", "   Newton-Raphson", "   Secantes",
    "Interpolacao:","Polinomios de Lagrange (Funcao como entrada)","   Polinomios de Lagrange (Pontos como entrada)","   Taylor",
    "Integracao:" ,"   Trapezios","   Simpson")
    open <- function(h,...)
    {
        if((svalue(h$obj))==choose[3]) {BISSECAO()}
        if((svalue(h$obj))==choose[4]) {FALSAPOSICAO()}
        if((svalue(h$obj))==choose[5]) {NEWTONRAPSON()}
        if((svalue(h$obj))==choose[6]) {SECANTE()}
        if((svalue(h$obj))==choose[8]) {INTERPOLACAOFUNCAO()}
        if((svalue(h$obj))==choose[9]) {INTERPOLACAOPONTOS()}
        if((svalue(h$obj))==choose[10]) {TAYLOR()}
        if((svalue(h$obj))==choose[12]) {TRAPEZIOS()}
        if((svalue(h$obj))==choose[13]) {SIMPSON()}
    }

    MainWindow <- gwindow(title = "CN Visual",width = 300, height = 300)
    maingroup <- ggroup(horizontal=FALSE, container=MainWindow)
    checkframe <- gframe("Selecione o metodo", container = maingroup, horizontal = FALSE)
    Metodo <- gcombobox( choose, container= checkframe, handler=open, horizontal = FALSE, height=150)
    bottomframe <- gframe("Atencao", container = maingroup, horizontal = FALSE)
    Texto <- glabel("Por favor, leia o manual antes de utilizar o software \ne verifique se nao ha uma versao mais recente, \n ambos podem ser encontrados no site, \n basta clicar no botao a baixo", container = bottomframe)
    gbutton("Ir para o site", cont= maingroup, handler = linkfun)

    #img <- gdkPixbufNewFromFile("icon.png")
    #getToolkitWidget(MainWindow)$setIcon(img$retval)

    exit_hand<-function(h,...){dispose(MainWindow)}
    gbutton("SAIR", cont= maingroup, handler = exit_hand)
}
