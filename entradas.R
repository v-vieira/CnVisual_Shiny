input_funcao <- textInput(
  inputId="input_funcao",
  label = "Função"
)

input_pontos_ab <- tags$div(
  id='input_pontos_ab',
  fluidRow(
    column(6,
           numericInput(
             inputId="input_pontos_a",
             label = "a0",
             step = 0.05,
             value = NULL
           )
    ),
    column(6,
           numericInput(
             inputId="input_pontos_b",
             label = "b0",
             step = 0.05,
             value = NULL
           )
    )
  )
)

input_pontos_sec <- tags$div(
  id='input_pontos_sec',
  fluidRow(
    column(6,
           numericInput(
             inputId="input_pontos_x0",
             label = "x0",
             step = 0.05,
             value = NULL
           )
    ),
    column(6,
           numericInput(
             inputId="input_pontos_x1",
             label = "x1",
             step = 0.05,
             value = NULL
           )
    )
  )
)

input_x0 <- numericInput(
  inputId="input_x0",
  label = "x0",
  step = 0.05,
  value = NULL
)

input_decimais <- numericInput(
  inputId="input_decimais",
  label = "Nº casas decimais",
  step = 1,
  min = 1,
  max = 5,
  value = 1
)

input_iteracoes <- numericInput(
  inputId="input_iteracoes",
  label = "Nº iterações",
  step = 1,
  min = 2,
  max = 15,
  value = 3
)

input_pontos_x <- textInput(
  inputId="input_pontos_x",
  label = "Pontos X"
)

input_pontos_y <- textInput(
  inputId="input_pontos_y",
  label = "Pontos Y"
)

input_ponto_aprox <- numericInput(
  inputId="input_ponto_aprox",
  label = "Ponto a ser aproximado",
  step = 0.05,
  value = NULL
)

input_ponto_input <- numericInput(
  inputId="input_ponto_input",
  label = "Ponto utilizado pelo método",
  step = 0.05,
  value = NULL
)

input_divisoes <- numericInput(
  inputId="input_divisoes",
  label = "nº de divisões",
  step = 1,
  min = 2,
  max = 14,
  value = 2
)

input_veloc_anim <- numericInput(
  inputId="input_veloc_anim",
  label = "Velocidade de animação",
  step = 0.1,
  min = 0.3,
  max = 5,
  value = 0.8
)

input_interv_integra <- tags$div(
  id='input_interv_integra',
  tags$b("Intervalo de integração"),
  fluidRow(
    column(6,
           numericInput(
             inputId="input_integra_a",
             label = "a",
             step = 0.05,
             value = NULL
           )
    ),
    column(6,
           numericInput(
             inputId="input_integra_b",
             label = "b",
             step = 0.05,
             value = NULL
           )
    )
  )
)

input_graus <- checkboxGroupInput(
  inputId="input_graus",
  label = "Graus de integração",
  choices = c("Grau 1"=1,
              "Grau 2"=2,
              "Grau 3"=3,
              "Grau 4"=4,
              "Grau 5"=5)
)

opcoes_grapicas <- tags$div(
  id='opcoes_grapicas',
  tags$b("Opções Gráficas"),
  fluidRow(
    checkboxInput(inputId = 'g_indices',
                  label='Índices',
                  value=TRUE),
    checkboxInput(inputId = 'g_lh',
                  label='Linha Horizontal',
                  value=TRUE),
    checkboxInput(inputId = 'g_sc',
                  label='Linha Secante',
                  value=TRUE),
    checkboxInput(inputId = 'g_lv',
                  label='Linha Vertical',
                  value=TRUE),
    checkboxInput(inputId = 'g_ltg',
                  label='Linha Tangente',
                  value=TRUE),
    checkboxInput(inputId = 'g_pintar',
                  label='Pintar Área Gráfica',
                  value=TRUE),
    numericInput(inputId = 'g_offset',
                  label='Offset pt. aprox.',
                  value=1,
                  min = 0.1,
                  step = 0.1)
  )
)