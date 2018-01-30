library(shiny)
library(ggplot2)

setwd("C:/Users/Weronika/stay_or_go/time_after_time")

data <- read.csv("time.csv", stringsAsFactors = FALSE, header = TRUE)
data$both <- data$work + data$sport
data$weekday <- !(as.POSIXlt(data$date)$wday == 0 | as.POSIXlt(data$date)$wday == 6)
final_date <- '2018-03-17'
time_difference <- ceiling(as.numeric(difftime(final_date, Sys.Date())))
mean_worktime <- round(mean(data$work))
shame_days <- as.numeric(count(data[data$work == 0, ]))
good_days <- as.numeric(count(data[data$work != 0, ]))
##


##
ui <- fluidPage(
  titlePanel("Time contribution", windowTitle = "Time after time"),
  sidebarLayout(
    sidebarPanel(
      selectInput("typeInput", "Graph data", choices = c("work", "sport", "both")),
      radioButtons("timeTypeInput", "Time unit", choices = c("Hours", "Pomodoros"), selected = "Pomodoros"),
      br(),
      h3("Basic stats"),
      h5("Mean daily worktime : ", mean_worktime, "pomodoros and that's ", round(mean_worktime/2), " in hours."),
      br(), 
      h1("Days left:", time_difference),
      h5("Since the beginning : "),
      h4("Shame days: ", shame_days),
      h4("Worked days: ", good_days),
      h4("More work then shame? ", good_days > shame_days)
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
      labs(y = title) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
      # + scale_y_continuous(name = "Pomodoros done", limits=c(0, 16))
  })
}

shinyApp(ui = ui, server = server)