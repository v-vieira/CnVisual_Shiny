test_inputs <- function(input_metodo,input){
  error_vector <<- c()
  ### Lista de entradas para seus respectivos testes de input
  # Numericas
  type_num <- c('input_x0','input_decimais','input_iteracoes','input_veloc_anim','input_ponto_aprox','input_ponto_input','input_divisoes','g_offset')
  # Lista de pontos str
  type_list_str <- c('input_pontos_x','input_pontos_y')
  # Lista 2 pontos num
  type_list_num <- c('input_pontos_ab','input_pontos_sec','input_interv_integra')
  inputs_list_num <- list(
    c('input_pontos_a','input_pontos_b'),
    c('input_pontos_x0','input_pontos_x1'),
    c('input_integra_a','input_integra_b')
  )
  names(inputs_list_num) <- type_list_num
  # Função
  type_fun <- c('input_funcao')
  
  for(aslist in entrada_metodo[input_metodo]){
    to_test <- aslist
  }
  
  for(the_input in to_test){
    # Teste dos numericos
    if(the_input %in% type_num){
      met <- eval(parse(text=paste0('input$',the_input)))
      if(!is.numeric(met)){
        error_vector <<- append(error_vector,paste('Valor não-numerico em ',the_input))
      }
    }
    # Teste da lista como string
    else if(the_input %in% type_list_str){
      met <- eval(parse(text=paste0('input$',the_input)))
      valxaux <- as.list(strsplit(met," ")[[1]])
      error <- FALSE
      if(length(valxaux)<2){
        error_vector <<- append(error_vector,paste('Numero de pontos insuficiente em',the_input))
      }
      else{
        for(value in valxaux){
          if(is.na(as.numeric(value))){
            error <- TRUE
          }
        }
        if(error){
          error_vector <<- append(error_vector,paste('Erro ao dividir os valores da lista em ',the_input))
        }
      }
    }
    # Teste da lista como numerico
    else if(the_input %in% type_list_num){
      for(toeval in inputs_list_num[the_input]){
        to_eval <- toeval
      }
      val_1 <- eval(parse(text=paste0('input$',to_eval[1])))
      val_2 <- eval(parse(text=paste0('input$',to_eval[2])))
      if((!is.numeric(val_1)) || (!is.numeric(val_2))){
        error_vector <<- append(error_vector,paste('Valor não-numerico em ',the_input))
      }
      else if(val_1==val_2){
        error_vector <<- append(error_vector,paste('O intervalo em ',the_input,' não é válido'))
      }
    }
    # Teste da função
    else if(the_input %in% type_fun){
      met <- eval(parse(text=paste0('input$',the_input)))
      # garantir que não tem complexo, mas deixar sin()
      if(str_detect(str_replace(met,c('sin','sinpi','sinh','asinh','factorial','sign','ceiling','cospi','tanpi','pi'),''),'i') || met==''){
        error_vector <<- append(error_vector,paste('Não foi possível criar a função dada em ',the_input))
      }
      else{
        tryCatch({
          eval(parse(text=paste('a<-function(x){',met,'}')))
          b <- a(10)
          if(is.na(b)){
            error_vector <<- append(error_vector,paste('Não foi possível criar a função dada em ',the_input))
          }
          else if(b==Inf){
            error_vector <<- append(error_vector,paste('Não foi possível criar a função dada em ',the_input))
          }
        },
        error = function(e){
          error_vector <<- append(error_vector,paste('Não foi possível criar a função dada em ',the_input))
        }
        )
      }
    }
  }
}