input_funcao <- textInput(
  inputId="input_funcao",
  label = "Função"
)

input_pontos <- textInput(
  inputId="input_pontos",
  label = "Pontos"
)

input_x0 <- numericInput(
  inputId="input_x0",
  label = "x0",
  step = 0.05,
  value = NULL
)

input_intervalo <- textInput(
  inputId="input_intervalo",
  label = "Intervalo"
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
  value = NULL
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
  label = "Ponto utilizado na aproximação",
  step = 0.05,
  value = NULL
)

input_lim_x <- textInput(
  inputId="input_lim_x",
  label = "Limite em x"
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
  value = NULL
)

input_interv_integra <- textInput(
  inputId="input_interv_integra",
  label = "Intervalo de integração"
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