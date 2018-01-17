library(shiny)
library(ggplot2)

data <- read.csv("time.csv", stringsAsFactors = FALSE, header = TRUE)
data$both <- data$work + data$sport


ui <- fluidPage(
  titlePanel("Time contribution", windowTitle = "Time after time"),
  sidebarLayout(
    sidebarPanel(
      selectInput("typeInput", "Graph data", choices = c("work", "sport", "both")),
      radioButtons("timeTypeInput", "Time unit", choices = c("hours", "pomodoros"), selected = "pomodoros")
    ),
    mainPanel(
      plotOutput("plot"),
      br(),
      tableOutput("stats"),
      textOutput("txt")
    )
  )
  
)

server <- function(input, output, session) {
  
  output$plot <- renderPlot({
    timeType <- reactive({input$timeTypeInput})
    if (timeType() == 'hours'){
      data$work = data$work/2
      data$sport = data$sport/2
      data$both = data$both/2
    }
    ggplot(data, aes_string(x = "date", y = input$typeInput)) +
      geom_point() +
      expand_limits(y = 0) 
      # scale_y_continuous(name = "Pomodoros done", limits=c(0, 16))
  })
}

shinyApp(ui = ui, server = server)