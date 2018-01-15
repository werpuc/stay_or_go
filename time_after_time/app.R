library(shiny)
library(ggplot2)

data <- read.csv("time.csv", stringsAsFactors = FALSE, header = TRUE)
data$both <- data$work + data$sport


ui <- fluidPage(
  titlePanel("Time contribution", windowTitle = "Time after time"),
  sidebarLayout(
    sidebarPanel(
      selectInput("type", "type", choices = c("work", "sport", "both"))
    ),
    mainPanel(
      plotOutput("plot"),
      br(),
      tableOutput("stats")
    )
  )
  
)

server <- function(input, output, session) {
  output$plot <- renderPlot({
    ggplot(data, aes_string(x = "date", y = input$type)) +
      geom_point()
  })
}

shinyApp(ui = ui, server = server)