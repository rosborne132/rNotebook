library(shiny)
library(ggplot2)
library(rsconnect)

# Define the UI
ui <- fluidPage(
  titlePanel("Basic Shiny App"),

  sidebarLayout(
    sidebarPanel(
      # Input: Select a variable for the x-axis
      selectInput("xvar", "X-axis variable:",
                  choices = names(mtcars)),

      # Input: Select a variable for the y-axis
      selectInput("yvar", "Y-axis variable:",
                  choices = names(mtcars),
                  selected = "mpg"),

      # Input: Numeric input for the number of bins
      numericInput("bins", "Number of bins:", 10, min = 1, max = 50)
    ),

    mainPanel(
      plotOutput("distPlot"),
      tableOutput("dataTable")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  # Reactive expression to create the plot
  output$distPlot <- renderPlot({
    x <- mtcars[[input$xvar]]
    y <- mtcars[[input$yvar]]

    hist(x, breaks = input$bins, col = 'darkgray', border = 'white',
         xlab = input$xvar, main = paste("Histogram of", input$xvar))
  })

  # Reactive expression to create the table
  output$dataTable <- renderTable({
    mtcars[, c(input$xvar, input$yvar)]
  })
}

# Run the application
shinyApp(ui = ui, server = server)
