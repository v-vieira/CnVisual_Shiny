server <- function(session, input, output) {
  ### Inicia o timer com 1 seg
  timer <- reactiveTimer(intervalMs = 1000)
  vars_anim <- list(inc = 1)
  ### Variáveis globais, usadas para controle.
  plot_vector <<- NULL
  value_output <<- NULL
  error_vector <<- c()
  warning_vector <<- c()
  
  source("./funcoes/popup.R", encoding = "utf-8")
  source("listas.R", encoding = "utf-8")
  source("./funcoes/test_inputs.R", encoding = "utf-8")
  source("./metodos/bissecao.R", encoding = "utf-8")
  source("./metodos/newton_raphson.R", encoding = "utf-8")
  source("./metodos/falsa_posicao.R", encoding = "utf-8")
  source("./metodos/secante.R", encoding = "utf-8")
  source("./metodos/inter_funcao.R", encoding = "utf-8")
  source("./metodos/inter_pontos.R", encoding = "utf-8")
  source("./metodos/taylor.R", encoding = "utf-8")
  source("./metodos/trapezios.R", encoding = "utf-8")
  source("./metodos/simpson.R", encoding = "utf-8")
  
  # Colocar a image de warning no output
  output$warning_png <- renderImage({
    return(list(
      src = "warning_icon.png",
      filetype = "image/png",
      alt = "warning"
    ))
  }, deleteFile = FALSE)
  
  ### Mudar os metodos disponíveis de acordo com o tipo
  observeEvent(input$tipo, {
    updateSelectInput(session,
                      inputId = "metodo",
                      choices = metodos_tipos[[input$tipo]]
    )
    shinyjs::showElement(id = input$tipo)
    for (value in nomes_tipos) {
      if (value != input$tipo) {
        shinyjs::hideElement(id = value)
      }
    }
  })
  ### Mostrar apenas as entradas de acordo com o metodo
  observeEvent(input$metodo, {
    for (value in lista_entradas) {
      if (value %in% entrada_metodo[[input$metodo]]) {
        shinyjs::showElement(id = value)
      }
      else{
        shinyjs::hideElement(id = value)
      }
    }
    for (value in lista_og) {
      if (value %in% grafico_metodo[[input$metodo]]) {
        shinyjs::showElement(id = value)
      }
      else{
        shinyjs::hideElement(id = value)
      }
    }
  })
  ### Ao clicar no botao, roda o metodo e ativa o timer
  observeEvent(input$button, {
    test_inputs(input$metodo, input)
    if (is.null(error_vector)) {
      shinyjs::hideElement(id = "warning_div")
      if (as.numeric(input$input_veloc_anim) < 0.3) {
        timer <<- reactiveTimer(300)
      }
      else{
        timer <<- reactiveTimer(as.numeric(input$input_veloc_anim) * 1000)
      }
      
      ### Variáveis globais, usadas para controle.
      plot_vector <<- NULL
      value_output <<- NULL
      error_vector <<- c()
      warning_vector <<- c()
      
      if (input$tipo == "tipo_raiz") {
        if (input$metodo == "Bis") {
          bissection(input$input_funcao,
                     input$input_pontos_a,
                     input$input_pontos_b,
                     input$input_decimais,
                     input$input_iteracoes,
                     input$g_indices,
                     input$g_lh)
        }
        else if (input$metodo == "Fal_Pos") {
          falsa(input$input_funcao,
                input$input_pontos_a,
                input$input_pontos_b,
                input$input_decimais,
                input$input_iteracoes,
                input$g_indices,
                input$g_sc)
        }
        else if (input$metodo == "New_Rap") {
          newtonraphson(input$input_funcao,
                        input$input_x0,
                        input$input_decimais,
                        input$input_iteracoes,
                        input$g_indices,
                        input$g_lv,
                        input$g_ltg)
        }
        else{
          secante(input$input_funcao,
                  input$input_pontos_x0,
                  input$input_pontos_x1,
                  input$input_iteracoes,
                  input$input_decimais,
                  input$g_indices,
                  input$g_lv,
                  input$g_sc)
        }
      }
      else if (input$tipo == "tipo_interpo") {
        if (input$metodo == "Pol_fun") {
          inter_funcao(input$input_ponto_aprox,
                       input$input_pontos_x,
                       input$input_funcao,
                       input$g_indices,
                       input$g_lv)
        }
        else if (input$metodo == "Pol_pon") {
          inter_pontos(input$input_ponto_aprox,
                       input$input_pontos_x,
                       input$input_pontos_y,
                       input$g_indices,
                       input$g_lv)
        }
        else{
          taylor(input$input_funcao,
                 input$input_ponto_input,
                 input$input_ponto_aprox,
                 input$input_graus,
                 input$g_offset)
        }
      }
      else{
        if (input$metodo == "Tra") {
          trapezios(input$input_funcao,
                    input$input_integra_a,
                    input$input_integra_b,
                    input$input_divisoes,
                    input$g_pintar,
                    input$g_lv,
                    input$g_indices)
        }
        else{
          simpson(input$input_funcao,
                  input$input_integra_a,
                  input$input_integra_b,
                  input$input_divisoes,
                  input$g_lv,
                  input$g_indices,
                  input$g_pintar)
        }
      }
      if (is.null(error_vector)) {
        if (!is.null(warning_vector)) {
          text_warning <- ""
          for (value in warning_vector) {
            text_warning <- paste(text_warning, "<li>", value, "</li>")
          }
          output$warning_text <- renderUI({
            HTML("<div style =\"text-align:left\"><ul>", text_warning, "</ul></div>")
          })
          shinyjs::showElement(id = "warning_div")
        }
        vars_anim$inc <<- 1
        anim$resume()
        shinyjs::hideElement(id='text_output')
      }
      else{
        anim$suspend()
        popup_erro(error_vector)
      }
    }
    else{
      anim$suspend
      popup_erro(error_vector)
    }
  })
  
  ### Para cada alteração de valor do timer, plot os valores.
  anim <- observe({
    timer()
    if (is.null(error_vector)) {
      if (vars_anim$inc <= length(plot_vector)) {
        aux_plot <- plot_vector[[vars_anim$inc]]
        output$plot1 <- NULL
        output$plot1 <- renderPlotly({ggplotly(aux_plot)})
        vars_anim$inc <<- vars_anim$inc + 1
      }
      else{
        shinyjs::showElement(id='text_output')
        output$text_output <- renderUI({HTML(value_output[[1]])})
        anim$suspend()
      }
    }
  }, suspended = TRUE)
  
  ### controle do timer
  observeEvent(input$playPause, {
    if(isTRUE(anim$.suspended)){
      anim$resume()
    }
    else{
      anim$suspend()
    }
  })
  observeEvent(input$restart, {
    vars_anim$inc <<- 1
    anim$resume()
  })
}
