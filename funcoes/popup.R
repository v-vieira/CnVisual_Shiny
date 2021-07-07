popup_erro<- function(error_vector){
  text_html = ''
  for (value in error_vector){
    text_html = paste(text_html,'<li>',value,'</li>')
  }
  shinyalert(
    title = "ERRO",
    text = paste("<div style ='text-align:left'>
                <div style ='font-weight: bold;'>Atenção, corriga o(s) seguinte(s) erro(s):</div>
                <ul>",text_html,"
                </ul>
                <div> Para mais informações acesse a documentação disponível <a href='https://google.com'>aqui</a> </div>
              </div>"),
    size = "s", 
    closeOnEsc = TRUE,
    closeOnClickOutside = TRUE,
    html = TRUE,
    type = "error",
    showConfirmButton = TRUE,
    showCancelButton = FALSE,
    confirmButtonText = "OK",
    confirmButtonCol = "#AEDEF4",
    timer = 0,
    imageUrl = "",
    animation = TRUE
  )
}
