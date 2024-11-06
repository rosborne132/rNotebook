library(shiny)
library(tidyverse)
library(plotly)

fert <- read_csv("https://raw.githubusercontent.com/kitadasmalley/FA2020_DataViz/main/data/gapminderFert.csv")

# Define UI
ui <- fluidPage(

  # Application title
  titlePanel("Gapminder: Simple Shiny Demo"),

  # Sidebar with a slider input the year
  sidebarLayout(
    sidebarPanel(
      sliderInput("year",
                  h3("Select a year:"),
                  min = 1962,
                  max = 2015,
                  value = 1962,
                  sep=""),

      selectInput("country",
                  h4("Compare another country's pathway to the United States:"),
                  choices = unique(fert$Country)[-133], # get all of the choices in the code
                  selected = "Vietnam")
    ),

    # Show a scatter plots
    mainPanel(
      plotlyOutput("gapPlot"),
      plotOutput("pathPlot")
    )
  )
)

# Declare global variables to avoid binding errors
# continent <- NULL

# Define server
server <- function(input, output) {
  output$gapPlot <- renderPlotly({
    # generate plot based on input$year from ui.R
    p <- fert %>%
      filter(year == input$year) %>%
      ggplot(aes(fert, life, size = pop, color = continent)) +
      labs(x = "Fertility Rate", y = "Life expectancy at birth (years)",
           caption = "(Based on data from Hans Rosling - gapminder.com)",
           color = 'Continent',size = "Population (millions)") +
      ylim(30,100) +
      xlim(min(fert$fert), max(fert$fert)) +
      geom_point(aes(text = Country))

    ggplotly(p)
  })

  output$pathPlot <- renderPlot({
    # generate plot based on input$country from ui.R

    thisCountry<-input$country
    fert%>%
      filter(Country %in% c("United States", thisCountry))%>%
      ggplot(aes(fert, life, size = pop, color = Country,
                 alpha=year)) +
      labs(x="Fertility Rate", y = "Life expectancy at birth (years)",
           caption = "(Based on data from Hans Rosling - gapminder.com)",
           color = 'Country',size = "Population (millions)",
           title=paste("Fertility Path: United States vs ", thisCountry, sep="")) +
      ylim(30,100) +
      xlim(min(fert$fert), max(fert$fert))+
      geom_point()


  })
}

# Run the application
shinyApp(ui = ui, server = server)