# lista com todos os metodos
metodos <- list("Bis", "Fal_Pos", "New_Rap", "Sec", "Pol_fun", "Pol_pon", "Tay", "Tra", "Sim")

# lista com todas as entradas
lista_entradas <- list("input_funcao", "input_pontos_ab", "input_pontos_sec",
                       "input_x0", "input_decimais", "input_iteracoes",
                       "input_veloc_anim", "input_pontos_x", "input_pontos_y",
                       "input_ponto_aprox", "input_ponto_input", "input_divisoes",
                       "input_interv_integra", "input_graus", "g_offset")

lista_og <- list("g_indices", "g_lh", "g_sc", "g_lv", "g_ltg", "g_pintar")
# lista com os tipos de metodos
nomes_tipos <- list("tipo_raiz", "tipo_interpo", "tipo_integra")

# entradas utilizadas em cada metodo
entrada_metodo <- list(
  ####    RAIZ

  # Bisseção
  c("input_funcao", "input_pontos_ab", "input_decimais", "input_iteracoes", "input_veloc_anim"),
  # Falsa Posição
  c("input_funcao", "input_pontos_ab", "input_decimais", "input_iteracoes", "input_veloc_anim"),
  # Newton Rapson
  c("input_funcao", "input_x0", "input_decimais", "input_iteracoes", "input_veloc_anim"),
  # Secante
  c("input_funcao", "input_pontos_sec", "input_decimais", "input_iteracoes", "input_veloc_anim"),

  ####    INTERPOLAÇÃO

  # Pol. de Lagrange por função
  c("input_pontos_x", "input_funcao", "input_ponto_aprox", "input_veloc_anim"),
  # Pol. de Lagrange por pontos
  c("input_pontos_x", "input_pontos_y", "input_ponto_aprox", "input_veloc_anim"),
  # Taylor
  c("input_funcao", "input_ponto_input", "input_ponto_aprox", "input_graus", "input_veloc_anim", "g_offset"),

  ####    INTEGRAÇÃO

  # Trapézios
  c("input_funcao", "input_interv_integra", "input_divisoes", "input_veloc_anim"),
  # Simpson
  c("input_funcao", "input_interv_integra", "input_divisoes", "input_veloc_anim")
)
names(entrada_metodo) <- metodos

# Opções gráficas de cada método
grafico_metodo <- list(
  # Bisseção
  c("g_indices", "g_lh"),
  # Falsa Posição
  c("g_indices", "g_sc"),
  # Newton Rapson
  c("g_indices", "g_lv", "g_ltg"),
  # Secante
  c("g_indices", "g_lv", "g_sc"),
  # Pol. de Lagrange por função
  c("g_indices", "g_lv"),
  # Pol. de Lagrange por pontos
  c("g_indices", "g_lv"),
  # Taylor
  c(),
  # Trapézios
  c("g_pintar", "g_lv", "g_indices"),
  # Simpson
  c("g_lv", "g_indices", "g_pintar")

)
names(grafico_metodo) <- metodos

#  tipos de metodos e seus ids
metodos_tipos <- list(
  c("Bisseção" = "Bis", "Falsa Posição" = "Fal_Pos", "Newton-Raphson" = "New_Rap", "Secantes" = "Sec"),
  c("Pol. de Lagrange (função)" = "Pol_fun", "Pol. de Lagrange (pontos)" = "Pol_pon", "Taylor" = "Tay"),
  c("Trapézios" = "Tra", "Simpson" = "Sim")
)
names(metodos_tipos) <- nomes_tipos