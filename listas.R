# lista com todos os metodos
metodos <- list("Bis","Fal_Pos","New_Rap","Sec","Pol_fun","Pol_pon","Tay","Tra","Sim")

# lista com todas as entradas
lista_entradas <- list("input_funcao",
                       "input_pontos",
                       "input_x0",
                       "input_intervalo",
                       "input_decimais",
                       "input_iteracoes",
                       "input_veloc_anim",
                       # Usados apenas em interpolacao
                       "input_pontos_x",
                       "input_pontos_y",
                       "input_ponto_aprox",
                       "input_ponto_input",
                       "input_lim_x",
                       "input_divisoes",
                       "input_interv_integra")

# lista com os tipos de metodos
nomes_tipos <- list("tipo_raiz","tipo_interpo",'tipo_integra')

# entradas utilizadas em cada metodo
entrada_metodo <- list(
  #'
  #'    RAIZ
  #'
  
  # Bisseção -> OK
  c("input_funcao",
    "input_pontos",
    "input_decimais",
    "input_iteracoes",
    "input_veloc_anim"),
  # Falsa Posição -> OK
  c("input_funcao",
    "input_pontos",
    "input_decimais",
    "input_iteracoes",
    "input_veloc_anim"),
  # Newton Rapson -> OK
  c("input_funcao",
    "input_x0",
    "input_intervalo",
    "input_decimais",
    "input_iteracoes",
    "input_veloc_anim"),
  # Secante -> OK
  c("input_funcao",
    "input_intervalo",
    "input_decimais",
    "input_iteracoes",
    "input_veloc_anim"),
  
  #'
  #'    INTERPOLAÇÃO
  #'
  
  # Pol. de Lagrange por função
  c("input_pontos_x",
    "input_funcao",
    "input_ponto_aprox",
    #TODO Colocar Intervalos de plot abaixo:
    #,
    #"input_intervalo_x",
    #"input_intervalo_y",
    "input_veloc_anim"
    ),
  # Pol. de Lagrange por pontos
  c("input_pontos_x",
    "input_pontos_y",
    "input_ponto_aprox",
    #TODO Colocar Intervalos de plot abaixo:
    #,
    #"input_intervalo_x",
    #"input_intervalo_y",
    "input_veloc_anim"
    ),
  # Taylor
  c("input_funcao",
    "input_ponto_input",
    "input_ponto_aprox",
    "input_lim_x", #TODO Conferir se esse é gráfico ou não
    #TODO Colocar Intervalos de plot abaixo:
    #,
    #"input_intervalo_x",
    "input_veloc_anim"
    ),
  
  #'
  #'    INTEGRAÇÃO
  #'
  
  # Trapézios
  c("input_funcao",
    "input_interv_integra",
    "input_divisoes",
    "input_veloc_anim"),
  # Simpson
  c("input_funcao",
    "input_interv_integra",
    "input_divisoes",
    "input_veloc_anim")
)
names(entrada_metodo) <- metodos

# Opções gráficas de cada método
grafico_metodo <- list(
  # Bisseção
  c("graph_indices",
    "graph_lv"),
  # Falsa Posição
  c(),
  # Newton Rapson
  c(),
  # Secante
  c(),
  # Pol. de Lagrange por função
  c(),
  # Pol. de Lagrange por pontos
  c(),
  # Taylor
  c(),
  # Trapézios
  c(),
  # Simpson
  c()
  
)
names(grafico_metodo) <- metodos

#  tipos de metodos e seus ids
metodos_tipos <- list(
  c("Bisseção"="Bis",
    "Falsa Posição"="Fal_Pos",
    "Newton-Raphson"="New_Rap",
    "Secantes" = "Sec"),
  c("Pol. de Lagrange (função)"="Pol_fun",
    "Pol. de Lagrange (pontos)"="Pol_pon",
    "Taylor"="Tay"),
  c("Trapézios"="Tra",
    "Simpson"="Sim")
)
names(metodos_tipos) <- nomes_tipos
