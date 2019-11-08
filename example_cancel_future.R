# Исходные материалы
# http://blog.fellstat.com/?p=407
# https://rstudio.github.io/promises/articles/overview.html

library(shiny)
library(promises)
library(future)
plan(multiprocess)
IDLE = "_" # строка, которая обозначает, что процесс не запущен


ui <- fluidPage(
    titlePanel("Длительная прерываемая обработка в фоне"),
    sidebarLayout(
        sidebarPanel(
            actionButton('run', 'Запустить'),
            actionButton('cancel', 'Отменить'),
            actionButton('do', 'Сделать что-нибудь')
        ),
        mainPanel(
            tableOutput("result"),
            plotOutput("random")
        )
    )
)

server <- function(input, output, session) {
    
    #### код для работы со статусом
    # статус у нас хранится во временном файле. Гарантируется, что для каждого пользователя он будет свой
    status_file <- tempfile()
    
    get_status <- function(){
        if(!file.exists(status_file)) return(IDLE)
        res = readLines(status_file)
        res
    }
    
    set_status <- function(msg = IDLE){
        write(msg, status_file)
    }
    
    is_idle = function(){
        identical(get_status(), IDLE)        
    }
    
    set_idle = function(){
        set_status(IDLE)
    }
    
    ## завершение кода для работы со статусом
    # Удаляем временный файл по завершению сессии
    onStop(function(){
        print(status_file)
        if(file.exists(status_file)) unlink(status_file)
    })
    
    N <- 10 # кол-во циклов
    set_idle()
    progress = NULL # будущий прогресс-бар

    result_val <- reactiveVal() # здесь будет результат
    
    observeEvent(input$run,{
        
        
        if(is_idle()){
            # если процесса нет, то будем его запускать
            # создаем прогресс бар
            progress <<- Progress$new(session, min=1, max=100)
            progress$set(message = "Запускаемся")

        } else {
            # ничего не делаем, если задача уже запущена
            showNotification("Задача уже запущена")
            return(NULL)  
        }
        
        # высвечиваем вместо результата, что мы работаем
        result_val(data.frame(Status="Работаем..."))
        
        
        result <- future({
            print("Работаем...")
            step = N/10 # шаг по 10%
            counter = 0 # счетчик шагов
            set_status(counter*10)
            for(i in 1:N){
                
                # Здесь должна быть полезная деятельность. Имитируем её с помощью задержки
                Sys.sleep(1)
                
                # Проверяем, не пытаются ли прервать процесс
                if(is_idle()){ 
                    print("Останавливаемся...")
                    stop("Прервано пользователем")
                }
                
                # Пишем в файл, на каком мы этапе
                if(i>step*counter) {
                    counter = counter + 1
                    set_status(counter*10)
                    
                }
            }
            
            # Изображаем полученный ценный результат
            quantile(rnorm(1000))
        }) %...>% 
            result_val() # 
        
        # Здесь перехватываем ошибки и поьзовательские прерывания, и сообщаем о них пользователю
        result <- catch(result,
                        function(e){
                            result_val(NULL)
                            print(e$message)
                            showNotification(e$message)
                        })
        
        # После завершения процесса закрываем прогресс-бар и сбрасываем статус, чтобы можно было еще раз запустить
        # выполняется в любом случае, даже если была ошибка
        result <- finally(result,
                          function(){
                              progress$close() # закрываем прогресс-бар
                              set_idle() # сбрасываем статус в файле
                          })
        
        # проверка, что в статусе что-то меняется
        new_status = reactivePoll(1000, session, get_status, get_status)
        
        observe({
            # если в статусе что-то изменилось, то мы отображаем это на прогресс-баре
            if(!is_idle()) {
                curr_status = new_status()
                progress$set(value = as.numeric(curr_status), 
                             message = paste0("Работаем... ", curr_status, "%")
                             )
            }
        })
        
        # Надо вернуть что-нибудь, кроме promise, чтобы приложение осталось отзывчивым
        NULL
    })
    
    output$result <- renderTable({
        # req - чтобы выводилось только, если у нас операция корректно завершилась
        req(result_val())
    })
    
    # Здесь обрабатываем отмену задачи пользователем
    observeEvent(input$cancel,{
        print("Cancel")
        set_idle()
    })
    
    
    # здесь мы изобаржаем маленькую вспомогательную задачу,
    # которую пользователь может делать, пока основая работает в фоне
    output$random <- renderPlot({
        input$do
        plot(rnorm(100), rnorm(100))
    })
    
}


shinyApp(ui = ui, server = server)