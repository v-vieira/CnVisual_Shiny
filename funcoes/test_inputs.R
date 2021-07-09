test_inputs <- function(input_metodo,input){
  error_vector <<- c()
  ### Lista de entradas para seus respectivos testes de input
  # Numericas
  type_num <- c('input_x0','input_decimais','input_iteracoes','input_veloc_anim','input_ponto_aprox','input_ponto_input','input_divisoes')
  # Lista de pontos
  type_list <- c('input_pontos','input_intervalo','input_pontos_x','input_pontos_y','input_lim_x','input_interv_integra')
  # Função
  type_fun <- c('input_funcao')
  
  for(list in entrada_metodo[input_metodo]){
    to_test <- list
  }
  for(metodo in to_test){
    met <- eval(parse(text=paste0('input$',metodo)))
    # Teste dos numericos
    if(metodo %in% type_num){
      if(!is.numeric(met)){
        error_vector <<- append(error_vector,paste('Valor não-numerico em ',metodo))
      }
    }
    # Teste da lista numerica
    else if(metodo %in% type_list){
      valxaux <- as.list(strsplit(met," ")[[1]])
      error <- FALSE
      for(value in valxaux){
        if(is.na(as.numeric(value))){
          error <- TRUE
        }
      }
      if(error){
        error_vector <<- append(error_vector,paste('Erro ao dividir os valores da lista em ',metodo))
      }
    }
    # Teste da função
    else if(metodo %in% type_fun){
      # garantir que não tem complexo, mas deixar sin()
      if(str_detect(str_replace(met,'sin',''),'i') || met==''){
        error_vector <<- append(error_vector,paste('Não foi possível criar a função dada em ',metodo))
      }
      else{
        tryCatch({eval(parse(text=paste('b<-function(x){',met,'}')))},
                 error = function(e){
                   error_vector <<- append(error_vector,paste('Não foi possível criar a função dada em ',metodo))
                 })
      }
    }
  }
}