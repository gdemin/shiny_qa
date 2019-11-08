library(shiny)

ui <- fluidPage(
    titlePanel("Пример observeEvent"),
    sidebarLayout(
        sidebarPanel(
            selectInput('numbers', 'Цифры', choices = 1:10),
            actionButton('do1', 'Сделать что-нибудь 1'),
            actionButton('do2', 'Сделать что-нибудь 2')
        ),
        mainPanel(
            textOutput("result"),
            plotOutput("random")
        )
    )
)

server <- function(input, output, session) {
    
    
    observeEvent(input$do1, {
        
        numbers = input$numbers
        input$do2
        output$result = renderText({
            # здесь не должно быть ничего реактивного
            numbers
            
        })
        output$random = renderPlot({
            # здесь не должно быть ничего реактивного
            plot(runif(100), runif(100))
            
        })
    },
    priority = 0,
    ignoreInit = FALSE,
    once = FALSE
    )   
    
    
}

shinyApp(ui = ui, server = server)