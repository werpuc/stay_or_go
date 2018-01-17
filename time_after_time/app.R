library(shiny)
library(ggplot2)

data <- read.csv("time.csv", stringsAsFactors = FALSE, header = TRUE)
data$both <- data$work + data$sport
data$weekday <- !(as.POSIXlt(data$date)$wday == 0 | as.POSIXlt(data$date)$wday == 6)
final_date <- '2018-03-17'
time_difference <- ceiling(as.numeric(difftime(final_date, Sys.Date())))
mean_worktime <- mean(data$work)

ui <- fluidPage(
  titlePanel("Time contribution", windowTitle = "Time after time"),
  sidebarLayout(
    sidebarPanel(
      selectInput("typeInput", "Graph data", choices = c("work", "sport", "both")),
      radioButtons("timeTypeInput", "Time unit", choices = c("Hours", "Pomodoros"), selected = "Pomodoros"),
      br(),
      h3("Basic stats"),
      h5("Mean daily worktime : ", mean_worktime, "pomodoros and that's ", mean_worktime/2, " in hours."),
      br(), 
      h1("Days left:", time_difference)
    ),
    mainPanel(
      plotOutput("plot")
      
    )
  )
)

server <- function(input, output, session) {
  
  output$plot <- renderPlot({
    timeType <- reactive({input$timeTypeInput})
    title <- "Pomodoros done"
    if (timeType() == 'Hours'){
      data$work = data$work/2
      data$sport = data$sport/2
      data$both = data$both/2
      title <- "Hours worked"
    }
    ggplot(data, aes_string(x = "date", y = input$typeInput, color = "weekday")) +
      geom_point() +
      expand_limits(y = 0) +
      labs(title = "Time commitment over days", subtitle = "basic plot") +
      labs(x = "Date") +
      labs(y = title)
      
      # + scale_y_continuous(name = "Pomodoros done", limits=c(0, 16))
  })
}

shinyApp(ui = ui, server = server)