library(shiny)
library(ggplot2)
library(plotly)
library(rsconnect)
library(dplyr)
library(readr)
library(httr)
library(DT)
library(caret)
# library(randomForest)

url <- "https://github.com/rosborne132/rNotebook/raw/main/final-project/data/Quote_Data_Small.csv.zip"

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

# Train the model ===========================
set.seed(123)
model_data <- quote_data %>%
  select(ANNUALPREMIUM, PETAGEYEARS, PETGENDER, SPECIES) %>%
  na.omit() %>%
  sample_n(10000)

model_data$PETGENDER <- as.factor(model_data$PETGENDER)
model_data$SPECIES <- as.factor(model_data$SPECIES)

train_control <- trainControl(method = "cv", number = 10)
model <- train(
  ANNUALPREMIUM ~ PETAGEYEARS + PETGENDER + SPECIES,
  data = model_data,
  method = "rf",
  trControl = train_control
)

# Define the UI ==============================
ui <- fluidPage(
  includeCSS("www/style.css"),
  navbarPage("PawSure",
    tabPanel("Background",
      includeHTML("www/background.html")
    ),
    tabPanel("Problem Statement",
      includeHTML("www/problem_statement.html")
    ),
    tabPanel("The Data",
      DTOutput("table")
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
              "select_species_2",
              label = h3("Species"),
              choices = c("Cat", "Dog"),
              selected = "Cat"
            ),
            sliderInput(
              "monthly_premium_slider",
              "Monthly Premium Instalment Range",
              min = 50,
              max = 1500,
              value = 50
            )
          ),
          mainPanel(
            plotOutput("plot3", height = "1200px")
          )
        )
      ),
    ),
    tabPanel("Quote Estimator",
      sidebarLayout(
        sidebarPanel(
          numericInput("age", "Pet Age", value = 5, min = 0, max = 20),
          selectInput("gender", "Gender", choices = c("Male", "Female")),
          selectInput("species", "Species", choices = c("Cat", "Dog")),
          actionButton("predict", "Predict")
        ),
        mainPanel(
          verbatimTextOutput("quoteDemo")
        )
      )
    ),
    tabPanel("Conclusion",
      includeHTML("www/conclusion.html")
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
        count = n()
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
        count = n()
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
  data_by_species <- reactive({
    quote_data %>%
      filter(SPECIES == input$select_species_2)
  })

  max_threshold <- reactive({
    input$monthly_premium_slider
  })

  output$plot3 <- renderPlot({
    ggplot(
      data_by_species(),
      aes(x = MONTHLYPREMIUMINSTALMENT, y = PROVIDER, fill = PROVIDER)
    ) +
      geom_boxplot() +
      scale_x_continuous(
        labels = scales::label_dollar(prefix = "£"),
        limits = c(0, max_threshold())
      ) +
      labs(
        title = "Which Providers Offer the Best Value? Monthly Premium Insights",
        subtitle = "A Comparative Boxplot of Monthly Premium Instalments Across Top   Insurance Providers",
        x = "Monthly Premium Instalment",
        y = "Provider",
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
}

# Run the application =======================
shinyApp(ui, server)
