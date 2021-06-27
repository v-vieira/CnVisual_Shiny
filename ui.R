library(shiny)
library(shinyjs)
library(plotly)

# Importação dos espaços de inputs
source("entradas.R",encoding = "utf-8")

ui <- fluidPage(
  useShinyjs(),
  titlePanel("CnVisual"),
  sidebarLayout(
  
  ### *Input () functions
    sidebarPanel(
      "Opções",
      selectInput(    # Seleção do tipo de cálculo
        inputId = "tipo",
        label = "Tipo",
        choices = c("Raiz da função"='tipo_raiz',
                    "Interpolação"='tipo_interpo',
                    "Integração" ='tipo_integra')
        ),
      
      selectInput(    # Seleção do método
        inputId = "metodo",
        label = "Método",
        choices = c()
        ),
      
      wellPanel(id="Entradas",    # Painel com todas as entradas, começam todas escondidas e mostram de acordo com a seleção do metodo
        shinyjs::hidden(input_funcao),
        shinyjs::hidden(input_pontos),
        shinyjs::hidden(input_x0),
        shinyjs::hidden(input_intervalo),
        shinyjs::hidden(input_decimais),
        shinyjs::hidden(input_iteracoes),
        shinyjs::hidden(input_veloc_anim),
        shinyjs::hidden(input_pontos_x),
        shinyjs::hidden(input_pontos_y),
        shinyjs::hidden(input_ponto_aprox),
        shinyjs::hidden(input_ponto_input),
        shinyjs::hidden(input_lim_x),
        shinyjs::hidden(input_divisoes),
        shinyjs::hidden(input_interv_integra),
        shinyjs::hidden(input_graus)
        ),
      
      actionButton(    # Botão para rodar
        inputId = 'button',
        label = 'Rodar!'
        ),
      actionButton("s1","start"),
      actionButton("s2","stop"),
      actionButton("s3","restart")
      ),
    
  ### *Output() functions
    mainPanel("Plot",
              plotlyOutput(outputId = "plot1"),
              verbatimTextOutput(outputId = 'text_output')
              )
  ),
  tags$style(type="text/css", "#plot1.recalculating { opacity: 1.0; }")
)
