library(shiny)
library(memoise)
library(shinycssloaders)


ui <- fluidPage(
    titlePanel("Memoised"),
    fluidRow(
        column(6,
            selectInput("number", "Введите число:", choices = 1:100),
            actionButton("run", "Посчитать"),
            br(),
            br(),
            withSpinner(textOutput("result"))
        )
    )
)

count = memoise(function(int_sec){
    res = 0
    for(i in seq_len(int_sec)){
        Sys.sleep(1)
        print(i)
        res = res + i
    }
    res
})

server <- function(input, output, session) {
    

    output$result = renderText({""})
    output$result = renderText({
        input$run    
        secs = isolate(input$number)
        progress = Progress$new(session, min=1, max=secs)
        on.exit(progress$close())
        progress$set(message = 'Calculation in progress')
        count(secs)
    })
    
    
}

shinyApp(ui, server)