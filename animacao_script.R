library(shiny)
library(plotly)

ui <- fluidPage(
  sidebarLayout(sidebarPanel("Opções",
                             textOutput("mytext"),
                             actionButton("s1","start"),
                             actionButton("s2","stop"),
                             actionButton("s3","restart")),
                mainPanel("plot",
                          plotlyOutput(outputId  ="grafico")))
)

server <- function(input, output, session) {
  #start tiemr para plot
  timer<-reactiveValues(inc=0, timer=reactiveTimer(1000), started=FALSE)
  
  # inicar o plot vazio
  
  output$plot <- renderPlotly({
    p <- ggplot()
    ggplotly(p)})
  
  #= agregação dos graficos
  fun <- function(x){x^3}
  p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + xlim(-5,5)
  toSave <- list(
    p + stat_function(fun = fun),
    p + stat_function(fun = fun) + geom_point(x=1, y=2),
    p + stat_function(fun = fun) + geom_point(x=1, y=2) + geom_segment(aes(x=-2.5, xend=2.5, y=-100, yend=100)),
    p + stat_function(fun = fun) + geom_point(x=1, y=2) + geom_segment(aes(x=-2.5, xend=2.5, y=-100, yend=100)) + geom_point(x=3, y=4)
  )
  g <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + xlim(-5,5) + stat_function(fun = fun)
  toSave2 <- list(g)
  for (value in 1:5){
    toSave2[[value+1]] <- toSave2[[value]] + geom_point(x=value,y=value)
  }
  
  # Botoes
  observeEvent(input$s1,{timer$started<-TRUE})
  observeEvent(input$s2,{timer$started<-FALSE})
  observeEvent(input$s3, {
    timer$inc<-1
    timer$started<-TRUE})
  
  #= plot animmado
  observe({
    timer$timer()
    output$mytext <- renderText(timer$inc)
    if(isolate(timer$started)&&isolate(timer$inc<=4)){
      output$grafico<- renderPlotly({
        ggplotly(toSave2[[timer$inc]]) # caso não use o plotly, tem que colocar só [] ao inves de [[]]
        })
      timer$inc<-isolate(timer$inc)+1
    }
  })
  
}

shinyApp(ui, server)