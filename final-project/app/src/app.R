library(shiny)
library(ggplot2)
library(rsconnect)

# Define the UI
ui <- fluidPage(
  includeCSS("www/style.css"),
  titlePanel("Basic Shiny App with Machine Learning Demo"),

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
      numericInput("bins", "Number of bins:", 10, min = 1, max = 50),

      # Input: Select a variable for the predictor
      selectInput("predictor", "Predictor variable:",
                  choices = names(mtcars)),

      # Input: Select a variable for the response
      selectInput("response", "Response variable:",
                  choices = names(mtcars),
                  selected = "mpg")
    ),

    mainPanel(
      plotOutput("distPlot"),
    #   tableOutput("dataTable"),
      verbatimTextOutput("modelSummary"),
      plotOutput("modelPlot")
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

  # Reactive expression to fit the linear model
  model <- reactive({
    lm(as.formula(paste(input$response, "~", input$predictor)), data = mtcars)
  })

  # Output the model summary
  output$modelSummary <- renderPrint({
    summary(model())
  })

  # Output the model plot
  output$modelPlot <- renderPlot({
    ggplot(mtcars, aes_string(x = input$predictor, y = input$response)) +
      geom_point() +
      geom_smooth(method = "lm", col = "blue") +
      labs(title = paste("Linear Regression of", input$response, "on", input$predictor),
           x = input$predictor, y = input$response)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
