library(shiny)
library(shinyjs)
library(shinyalert)
library(ggplot2)
library(plotly)
library(stringr)

ui <- fluidPage(
  useShinyjs(),
  useShinyalert(),
  titlePanel("CnVisual"),
  sidebarLayout(
    
    ### *Input () functions
    sidebarPanel(
      "Opções",
      # Seleção do tipo de cálculo
      selectInput(
        inputId = "tipo",
        label = "Tipo",
        choices = c("Raiz da função" = "tipo_raiz",
                    "Interpolação" = "tipo_interpo",
                    "Integração" = "tipo_integra")
      ),
      # Seleção do método
      selectInput(
        inputId = "metodo",
        label = "Método",
        choices = c()
      ),
      
      wellPanel(id = "Entradas",    # Painel com todas as entradas, começam todas escondidas e mostram de acordo com a seleção do metodo
                shinyjs::hidden(textInput(inputId = "input_funcao", label = "Função")),
                shinyjs::hidden(tags$div(id = "input_pontos_ab",
                                         fluidRow(
                                           column(6, numericInput(inputId = "input_pontos_a", label = "a0", step = 0.05, value = NULL)),
                                           column(6, numericInput(inputId = "input_pontos_b", label = "b0", step = 0.05, value = NULL))))),
                shinyjs::hidden(tags$div(id = "input_pontos_sec",
                                         fluidRow(
                                           column(6, numericInput(inputId = "input_pontos_x0", label = "x0", step = 0.05, value = NULL)),
                                           column(6, numericInput(inputId = "input_pontos_x1", label = "x1", step = 0.05, value = NULL))))),
                shinyjs::hidden(numericInput(inputId = "input_x0", label = "x0", step = 0.05, value = NULL)),
                shinyjs::hidden(textInput(inputId = "input_pontos_x", label = "Pontos X")),
                shinyjs::hidden(textInput(inputId = "input_pontos_y", label = "Pontos Y")),
                shinyjs::hidden(numericInput(inputId = "input_ponto_input", label = "Ponto utilizado pelo método", step = 0.05, value = NULL)),
                shinyjs::hidden(numericInput(inputId = "input_ponto_aprox", label = "Ponto a ser aproximado", step = 0.05, value = NULL)),
                shinyjs::hidden(numericInput(inputId = "input_divisoes", label = "nº de divisões", step = 1, min = 2, max = 14, value = 2)),
                shinyjs::hidden(tags$div(id = "input_interv_integra",
                                         tags$b("Intervalo de integração"),
                                         fluidRow(
                                           column(6, numericInput(inputId = "input_integra_a", label = "a", step = 0.05, value = NULL)),
                                           column(6, numericInput(inputId = "input_integra_b", label = "b", step = 0.05, value = NULL))))),
                shinyjs::hidden(checkboxGroupInput(inputId = "input_graus", label = "Graus de integração",
                                                   choices = c("Grau 1" = 1, "Grau 2" = 2, "Grau 3" = 3, "Grau 4" = 4, "Grau 5" = 5))),
                shinyjs::hidden(numericInput(inputId = "input_decimais", label = "Nº casas decimais", step = 1, min = 1, max = 5, value = 1)),
                shinyjs::hidden(numericInput(inputId = "input_iteracoes", label = "Nº iterações", step = 1, min = 2, max = 15, value = 3)),
                shinyjs::hidden(numericInput(inputId = "input_veloc_anim", label = "Velocidade de animação", step = 0.1, min = 0.3, max = 5, value = 0.8))
      ),
      # Botão para rodar
      actionButton(
        inputId = "button",
        label = "Rodar!"
      ),
      actionButton("s1", "Play/Pause"),
      actionButton("s2", "Restart")
    ),
    
    ### *Output() functions
    mainPanel("Plot",
              fluidRow(
                column(10,
                       plotlyOutput(outputId = "plot1"),
                       shinyjs::hidden(tags$div(id = "warning_div", {
                         fluidRow(style = "background-color:#fcf4e9; display: flex; align-items: center; justify-content: center;",
                                  column(2, style = "", img(src = "warning_icon.png")),
                                  column(10, h3(style = "text-align:center", "ATENÇÃO!"), htmlOutput(outputId = "warning_text"))
                         )
                       })),
                       shinyjs::hidden(htmlOutput(outputId = "text_output", class = "verbatim_output"))
                ),
                column(2,
                       tags$div(id = "opcoes_grapicas",
                                tags$b("Opções Gráficas"),
                                fluidRow(checkboxInput(inputId = "g_indices", label = "Índices", value = TRUE),
                                         checkboxInput(inputId = "g_lh", label = "Linha Horizontal", value = TRUE),
                                         checkboxInput(inputId = "g_sc", label = "Linha Secante", value = TRUE),
                                         checkboxInput(inputId = "g_lv", label = "Linha Vertical", value = TRUE),
                                         checkboxInput(inputId = "g_ltg", label = "Linha Tangente", value = TRUE),
                                         checkboxInput(inputId = "g_pintar", label = "Pintar Área Gráfica", value = TRUE),
                                         numericInput(inputId = "g_offset", label = "Offset pt. aprox.", value = 1, min = 0.1, step = 0.1)))
                )
              )
    )
  ),
  tags$style(type = "text/css", "#plot1.recalculating { 
                                    opacity: 1.0; 
                                  }
                                  .verbatim_output {
                                    display: block; padding: 9.5px; margin: 0 0 10px; font-size: 13px; line-height: 1.42857143; color: #333;
                                    word-break: break-all; word-wrap: break-word; background-color: #f5f5f5; border: 1px solid #ccc;
                                    border-radius: 4px; font-family: Menlo,Monaco,Consolas,\"Courier New\",monospace; overflow: auto;
                                    box-sizing: border-box;
                                  }")
)
