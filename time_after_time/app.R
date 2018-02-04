library(shiny)
library(ggplot2)
library(tidyverse)

setwd("C:/Users/Weronika/stay_or_go/time_after_time")

data <- read.csv("time.csv", stringsAsFactors = FALSE, header = TRUE)
data$both <- data$work + data$sport
data$weekday <- !(as.POSIXlt(data$date)$wday == 0 | as.POSIXlt(data$date)$wday == 6)
final_date <- '2018-03-17'
time_difference <- ceiling(as.numeric(difftime(final_date, Sys.Date())))
mean_worktime_pomodoro <- round(mean(data$work))
mean_worktime_hours <- round(mean_worktime_pomodoro/2, 1)
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
      h5("Mean daily worktime : ", mean_worktime_pomodoro, "pomodoros and that's ", mean_worktime_hours, " in hours."),
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
    mean_worktime <- mean_worktime_pomodoro
    y_max <- max(data[input$typeInput])
    if (timeType() == 'Hours'){
      data$work = data$work/2
      data$sport = data$sport/2
      data$both = data$both/2
      title <- "Hours worked"
      mean_worktime <- mean_worktime_hours
    }
    ggplot(data, aes_string(x = "date", y = input$typeInput, color = "weekday")) +
      geom_hline(aes(yintercept = mean_worktime), linetype = 'dotdash', size = 0.5, alpha = 0.3) +
      # scale_y_continuous(breaks = sort(c(seq(0, y_max), mean_worktime))) + 
      # geom_text(aes(0, mean_worktime, label = 'mean', vjust = -1)) +
      geom_point() +
      expand_limits(y = 0) +
      labs(title = "Time commitment over days", subtitle = "basic plot") +
      labs(x = "Date") +
      labs(y = title) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
  })
}

shinyApp(ui = ui, server = server)