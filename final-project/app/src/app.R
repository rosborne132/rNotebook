# Load libraries ============================
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(httr)
library(DT)
library(caret)

# Load resources ============================
# TODO: load full data set
url <- "https://github.com/rosborne132/rNotebook/raw/main/final-project/data/Quote_Data_Small.csv.zip"
ap_model <- readRDS("www/models/ap_model.rds")
mpi_model <- readRDS("www/models/mpi_model.rds")

# Download and unzip the CSV file ============
temp <- tempfile()
download.file(url, temp)
unzip(temp, exdir = tempdir())
csv_file <- list.files(tempdir(), pattern = "\\.csv$", full.names = TRUE)
quote_data <- read_csv(
  csv_file,
  show_col_types = FALSE,
  col_select = c(
    "SPECIES",
    "BREED",
    "PETGENDER",
    "PETAGEYEARS",
    "MONTHLYPREMIUMINSTALMENT",
    "ANNUALPREMIUM",
    "PROVIDER",
    "STATICRISK",
    "NEUTERED"
  )
)

# Clean up the data =========================
# Remove the pound sign and convert the MONTHLYPREMIUMINSTALMENT to a
# numeric value
quote_data$MONTHLYPREMIUMINSTALMENT <- as.numeric(
  gsub("£", "", quote_data$MONTHLYPREMIUMINSTALMENT)
)

# Clean up the PROVIDER names
quote_data$PROVIDER <- quote_data$PROVIDER %>%
  gsub("£.*", "", .) %>%  # Remove any text starting with £
  gsub("[0-9]", "", .) %>%
  trimws()

# Clean up the BREED names
quote_data$BREED <- quote_data$BREED %>%
  tolower() %>%
  trimws()

# Define constants ===========================
margin_sm <- 14
source <- "Sources: Snowflake UK Pet Insurance Quotes Data - Examples"

# Define the UI ==============================
ui <- fluidPage(
  includeCSS("www/style.css"),
  navbarPage("BeanSprouts",
    tabPanel("Background",
      includeHTML("www/pages/background.html")
    ),
    tabPanel("Mission",
      includeHTML("www/pages/mission.html")
    ),
    tabPanel("Data",
      sidebarLayout(
        sidebarPanel(
          h3("The Data!"),
          p("Our dataset contains over one million pet insurance quotes from the UK market, collected over two days. Each quote includes key pricing factors like age, breed, and species for cats and dogs. This dataset enables insurers, analysts, and researchers to make data-driven decisions through market insights, trend identification, and pricing optimization. Understanding customer profiles and current insurance costs is essential for successful market entry."),
        ),
        mainPanel(
          h3("UK Pet Insurance Quotes"),
          DTOutput("table")
        )
      )
    ),
    navbarMenu("Findings",
      tabPanel("Plot1",
        sidebarLayout(
          sidebarPanel(
            selectInput(
              "select_gender",
              label = h3("Gender"),
              choices = c("Male", "Female"),
              selected = "Male"
            ),
            selectInput(
              "select_species",
              label = h3("Species"),
              choices = c("Cat", "Dog"),
              selected = "Cat"
            )
          ),
          mainPanel(
            plotOutput("plot1", height = "1200px")
          )
        )
      ),
      tabPanel("Plot2",
        sidebarLayout(
          sidebarPanel(
            selectInput(
              "select_species_1",
              label = h3("Species"),
              choices = c("Cat", "Dog"),
              selected = "Cat"
            ),
            checkboxInput("static_risk", "Static Risk", value = FALSE),
            checkboxInput("neutered", "Neutered", value = FALSE)
          ),
          mainPanel(
            plotOutput("plot2", height = "1200px")
          )
        )
      ),
      tabPanel("Plot3",
        sidebarLayout(
          sidebarPanel(
            selectInput(
              "select_gender_quote",
              label = h3("Gender"),
              choices = c("Male", "Female"),
              selected = "Male"
            ),
          ),
          mainPanel(
            plotOutput("plot3", height = "1200px")
          )
        )
      )
    ),
    tabPanel("Quote Estimator",
      sidebarLayout(
        sidebarPanel(
          h3("Proof of concept: Quoting engine"),
          p("By training models with our collected data, we can simulate multiple pricing scenarios for potential customer quotes. Compared to our competitors, having this model will allow us to quickly learn from and adapt to changes in the market."),
          numericInput("age", "Pet Age", value = 5, min = 0, max = 20),
          selectInput("gender", "Gender", choices = c("Male", "Female")),
          selectInput("species", "Species", choices = c("Cat", "Dog")),
          actionButton("predict", "Predict")
        ),
        mainPanel(
          p(htmlOutput("ap_quote")),
          p(htmlOutput("mpi_quote"))
        )
      )
    ),
    tabPanel("Conclusion",
      includeHTML("www/pages/conclusion.html")
    )
  )
)

# Define the server logic ====================
server <- function(input, output) {
  # Data Table ==============================
  output$table <- renderDT(
    quote_data, options = list(lengthChange = FALSE)
  )

  # Plot 1 ==================================
  data_avg_premium_by_breed <- reactive({
    quote_data %>%
      filter(PETGENDER == input$select_gender, SPECIES == input$select_species) %>%
      group_by(BREED) %>%
      summarize(
        avg_annualpremium = mean(ANNUALPREMIUM, na.rm = TRUE),
        count = n(),
        .groups = 'drop'
      ) %>%
      arrange(desc(count)) %>%
      slice_max(order_by = count, n = 20)
  })

  output$plot1 <- renderPlot({
    ggplot(
      data_avg_premium_by_breed(),
      aes(
        x = avg_annualpremium,
        y = reorder(BREED, avg_annualpremium),
        fill = "skyblue"
      )
    ) +
      geom_bar(stat = "identity", color = "black") +
      scale_x_continuous(labels = scales::label_dollar(prefix = "£")) +
      labs(
        title = "Top 10 Pet Breeds by Average Annual Premium for Each Gender",
        subtitle = "Analyzing the Most Quoted Breeds to Understand Premium Trends",
        x = "Annual Premium (Average)",
        y = "Breed",
        caption = source
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title.x = element_text(
          margin = margin(t = margin_sm, b = margin_sm)
        ),
        axis.title.y = element_text(margin = margin(r = margin_sm)),
        legend.position = "none"
      )
  })

  # Plot 2 ==================================
  data_avg_monthly_installment_by_age <- reactive({
    quote_data %>%
      filter(
        SPECIES == input$select_species_1,
        STATICRISK == input$static_risk,
        NEUTERED == ifelse(input$neutered, "Yes", "No")
      ) %>%
      group_by(PETAGEYEARS, PETGENDER) %>%
      summarize(
        avg_monthly_installment = mean(MONTHLYPREMIUMINSTALMENT, na.rm = TRUE),
        count = n(),
        .groups = 'drop'
      )
  })

  output$plot2 <- renderPlot({
    ggplot(
      data_avg_monthly_installment_by_age(),
      aes(x = PETAGEYEARS, y = avg_monthly_installment, fill = PETGENDER)
    ) +
      geom_col(position = "dodge", color = "black", width = 0.6) +
      labs(
        title = "How Age Affects Monthly Premiums",
        subtitle = "Exploring the Relationship Between Pet Age and Average Monthly Premium Costs",
        x = "Pet Age (Years)",
        y = "Average Monthly Premium Instalment",
        caption = source
      ) +
      theme_minimal() +
      scale_y_continuous(labels = scales::label_dollar(prefix = "£")) +
      theme(
        axis.title.x = element_text(
          margin = margin(t = margin_sm, b = margin_sm)
        ),
        axis.title.y = element_text(margin = margin(r = margin_sm)),
        legend.position = "top"
      )
  })

  # Plot 3 ==================================
  data_by_gender <- reactive({
    quote_data %>%
      filter(PETGENDER ==input$select_gender_quote)
  })

  output$plot3 <- renderPlot({
    ggplot(data_by_gender(), aes(x = PETAGEYEARS)) +
      geom_histogram(binwidth = 1, color = "black", fill = "skyblue") +
      scale_y_continuous(labels = scales::comma) +
      labs(
          title = "Counting Insurance Quotes by Pet Age",
          subtitle = "Understanding the Age Distribution of Pets Receiving Insurance Quotes",
          x = "Pet Age (Years)",
          y = "Number of Quotes",
          caption = source
      ) +
      facet_wrap(~ SPECIES) +
      theme_minimal() +
      theme(
          axis.title.x = element_text(margin = margin(t = margin_sm, b = margin_sm)),
          axis.title.y = element_text(margin = margin(r = margin_sm))
      )
  })

  # Machine Learning Demo ===================
  output$ap_quote <- renderUI({
    paste("Fill out the form to get your quote!")
  })
  observeEvent(input$predict, {
    new_data <- data.frame(
      PETAGEYEARS = input$age,
      PETGENDER = input$gender,
      SPECIES = input$species
    )

    # Make predictions
    ap_prediction <- predict(ap_model, new_data)
    mpi_prediction <- predict(mpi_model, new_data)

    # Update the UI
    output$ap_quote <- renderUI({
      paste("Predicted Annual Premium: £", round(ap_prediction, 2))
    })
    output$mpi_quote <- renderUI({
      paste(
        "Predicted Monthly Premium Installment: £",
        round(mpi_prediction, 2)
      )
    })
  })
}

# Run the application =======================
shinyApp(ui, server)
