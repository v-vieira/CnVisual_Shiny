library(shiny)
library(shinyjs)
library(plotly)
library(ggplot2)
library(stringr)

server <- function(session,input,output){
  ### Inicia o timer com 1 seg
  clock <- reactiveTimer(1000)
  timer <- reactiveValues(inc=0, timer=clock,started=FALSE)
  ### Variáveis globais, usadas para controle.
  plot_vector <<- NULL
  value_output <<- NULL
  error_vector <<- c()
  
  source("./funcoes/popup.R",encoding = "utf-8")
  source("listas.R",encoding = "utf-8")
  source("./funcoes/test_inputs.R",encoding = "utf-8")
  source("./metodos/Bissecao.R",encoding = "utf-8")
  source("./metodos/newton_raphson.R",encoding = "utf-8")
  source("./metodos/falsa_posicao.R",encoding = "utf-8")
  source("./metodos/secante.R",encoding = "utf-8")
  source("./metodos/inter_funcao.R",encoding = "utf-8")
  source("./metodos/inter_pontos.R",encoding="utf-8")
  source("./metodos/taylor.R",encoding="utf-8")
  source("./metodos/trapezios.R",encoding="utf-8")
  source("./metodos/simpson.R",encoding="utf-8")
  
  ### Mudar os metodos disponíveis de acordo com o tipo
  observeEvent(input$tipo,{
    updateSelectInput(session,
                      inputId = "metodo",
                      choices = metodos_tipos[[input$tipo]]
    )
    shinyjs::showElement(id = input$tipo)
    for (value in nomes_tipos){
      if (value != input$tipo){
        shinyjs::hideElement(id = value)
      }
    }
  })
  ### Mostrar apenas as entradas de acordo com o metodo
  observeEvent(input$metodo,{
    for (value in lista_entradas){
      if (value %in% entrada_metodo[[input$metodo]]){
        shinyjs::showElement(id=value)
      }
      else{
        shinyjs::hideElement(id=value)
      }
    }
  })
  ### Ao clicar no botao, roda o metodo e ativa o timer
  observeEvent(input$button,{
    test_inputs(input$metodo,input)
    if(is.null(error_vector)){
      if (as.numeric(input$input_veloc_anim) < 0.3){
        clock <<- reactiveTimer(300)
        timer <<- reactiveValues(inc=0, timer=clock,started=FALSE)
      }
      else{
        clock <<- reactiveTimer(as.numeric(input$input_veloc_anim)*1000)
        timer <<- reactiveValues(inc=0, timer=clock,started=FALSE)
      }
      
      ### Variáveis globais, usadas para controle.
      plot_vector <<- NULL
      value_output <<- NULL
      error_vector <<- c()
      
      if(input$tipo=="tipo_raiz"){
        if(input$metodo=="Bis"){
          bissection(input$input_funcao,
                     input$input_pontos,
                     input$input_decimais,
                     input$input_iteracoes,
                     TRUE,
                     TRUE)
        }
        else if (input$metodo=="Fal_Pos"){
          falsa(input$input_funcao,
                input$input_pontos,
                input$input_decimais,
                input$input_iteracoes,
                TRUE,
                TRUE)
        }
        else if(input$metodo=="New_Rap"){
          newtonraphson(input$input_funcao,
                        input$input_x0,
                        input$input_intervalo,
                        input$input_decimais,
                        input$input_iteracoes,
                        TRUE,
                        TRUE,
                        TRUE)
        }
        else{
          secante(input$input_funcao,
                  input$input_pontos,
                  input$input_iteracoes,
                  input$input_decimais,
                  TRUE,
                  TRUE,
                  TRUE)
        }
      }
      else if(input$tipo=="tipo_interpo"){
        if(input$metodo=="Pol_fun"){
          inter_funcao(input$input_ponto_aprox,
                       input$input_pontos_x,
                       input$input_funcao,
                       TRUE,
                       TRUE)
        }
        else if(input$metodo=="Pol_pon"){
          inter_pontos(input$input_ponto_aprox,
                       input$input_pontos_x,
                       input$input_pontos_y,
                       TRUE,
                       TRUE)
        }
        else{
          taylor(input$input_funcao,
                 input$input_ponto_input,
                 input$input_ponto_aprox,
                 #TODO: Colocar como input (gráfico)
                 env_int_plot = '-6 6',
                 input$input_graus)
        }
      }
      else{
        if(input$metodo=="Tra"){
          trapezios(input$input_funcao,
                    input$input_interv_integra,
                    input$input_divisoes,
                    TRUE,
                    TRUE,
                    TRUE)
        }
        else{
          simpson(input$input_funcao,
                  input$input_interv_integra,
                  input$input_divisoes,
                  TRUE,
                  TRUE,
                  TRUE)
        }
      }
      if(is.null(error_vector)){
        timer$started <- TRUE
      }
      else{
        timer$started <- FALSE
        popup_erro(error_vector)
      }
    }
    else{
      popup_erro(error_vector)
    }
  })
  
  ### Para cada alteração de valor do timer, plot os valores.
  observe({
    timer$timer()
    if(is.null(error_vector)){
      if(isolate(timer$started)&&isolate(timer$inc<=length(plot_vector)-1)){
        output$plot1<- NULL
        output$plot1<- renderPlotly({
          ggplotly(plot_vector[[timer$inc]])
        })
        timer$inc<-isolate(timer$inc)+1
        if(timer$inc==length(plot_vector)){
          output$text_output <- renderText({value_output[[1]]})
        }
      }
    }
  })
  
  ### controle do timer
  observeEvent(input$s1,{timer$started<-TRUE})
  observeEvent(input$s2,{timer$started<-FALSE})
  observeEvent(input$s3, {
    timer$inc<-1
    timer$started<-TRUE})
  
}