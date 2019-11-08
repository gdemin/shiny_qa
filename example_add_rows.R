library(shiny)
library(DT)
shinyApp(
    ui = fluidPage(
        fluidRow(
            column(12, 
                   br(),
                   actionButton("add_row", "Добавить строку"),
                   DTOutput('tbl'),
                   plotOutput('plt')
            )
        )
    ),
    server = function(input, output) {
        N = 5
        set.seed(123)
        my_df = data.frame(x = round(rnorm(N),2), y = round(rnorm(N),2))
        output$tbl = renderDT(
            my_df, 
            options = list(pageLength = 15, info = FALSE, dom = "t"), 
            editable = TRUE, 
            autoHideNavigation  = TRUE,
            server = TRUE
        )
        
        proxy = dataTableProxy('tbl')
        observeEvent(input$tbl_cell_edit, {
            edited_cell = input$tbl_cell_edit
            i = edited_cell$row
            j = edited_cell$col
            v = edited_cell$value
            my_df[i, j] <<- DT::coerceValue(v, my_df[i, j])
            replaceData(proxy, my_df, resetPaging = FALSE) 

            
        })
        output$plt = renderPlot({
            input$tbl_cell_edit
            plot(y ~ x, data = my_df)
        })
        
        observeEvent(input$add_row, {            
            addRow(proxy, data.frame(x = NA, y = NA))
            my_df <<- rbind(my_df, NA)
            # если server = TRUE
            replaceData(proxy, my_df, resetPaging = FALSE)
        })
    }
)