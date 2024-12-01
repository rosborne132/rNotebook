# Load libraries ============================
library(bslib)
library(caret)
library(dplyr)
library(DT)
library(ggplot2)
library(gridExtra)
library(httr)
library(readr)
library(shiny)

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
margin_sm <- 16
axis_title_size <- 20
axis_text_size <- 14
bar_color_primary <- "#E1EFE0"
bar_color_secondary <- "#DFF1FF"

# Define the UI ==============================
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "minty"),
  includeCSS("www/style.css"),
  navbarPage("BeanSprouts",
    tabPanel("Background",
      includeHTML("www/pages/background.html")
    ),
    tabPanel("Mission",
      includeHTML("www/pages/mission.html")
    ),
    tabPanel("Data",
      verticalLayout(
        wellPanel(
          h3("The Data!"),
          p("Our dataset contains over one million pet insurance quotes from the UK market, collected over two days. Each quote includes key pricing factors like age, breed, and species for cats and dogs. This dataset enables insurers, analysts, and researchers to make data-driven decisions through market insights, trend identification, and pricing optimization. Understanding customer profiles and current insurance costs is essential for successful market entry."),
        ),
        br(),
        h3("UK Pet Insurance Quotes"),
        DTOutput("table")
      )
    ),
    navbarMenu("Findings",
      tabPanel("Annual Premiums by Breed",
        verticalLayout(
          wellPanel(
            h2("Understanding Pet Insurance Pricing Through Average Annual Premiums"),
            p("By analyzing the average annual premiums and the number of quotes for the top breeds, we can identify patterns in customer demand and pricing behavior. The bar chart of average premiums provides insights into which breeds are associated with higher costs, while the total number of quotes by breed highlights the most popular choices among customers. Additionally, the histogram of pet ages for these breeds reveals key demographic trends. Together, these insights support data-driven decisions to optimize pricing strategies, ensuring competitiveness in the market and aligning with our goal of offering affordable and tailored coverage."),
            fluidRow(
              column(6,
                selectInput(
                  "select_gender",
                  label = h3("Gender"),
                  choices = c("Male", "Female"),
                  selected = "Male"
                )
              ),
              column(6,
                selectInput(
                  "select_species",
                  label = h3("Species"),
                  choices = c("Cat", "Dog"),
                  selected = "Cat"
                )
              )
            )
          ),
          br(),
          plotOutput("plot1", height = "1200px")
        )
      ),
      tabPanel("Annual Premiums by Age",
        verticalLayout(
          verticalLayout(
            wellPanel(
              h2("How Age Affects Monthly Premiums"),
              p("By examining the average monthly premiums across different age groups and genders, we can identify patterns that influence pricing decisions. For instance, understanding how premiums vary with age and differ between male and female pets can help tailor insurance plans to align with customer expectations and breed-specific risk factors. These insights enable us to create competitive, data-informed pricing models that resonate with the European market's unique demographics, ensuring BeanSprouts remains a trusted choice for pet owners."),
              selectInput(
                "select_species_1",
                label = h3("Species"),
                choices = c("Cat", "Dog"),
                selected = "Cat"
              ),
              checkboxInput("static_risk", "Static Risk", value = FALSE),
              checkboxInput("neutered", "Neutered", value = FALSE)
            )
          ),
          br(),
          plotOutput("plot2", height = "1200px")
        )
      ),
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
      filter(
        PETGENDER == input$select_gender,
        SPECIES == input$select_species
      ) %>%
      group_by(BREED) %>%
      summarize(
        avg_annualpremium = mean(ANNUALPREMIUM, na.rm = TRUE),
        count = n(),
        .groups = 'drop'
      ) %>%
      arrange(desc(count)) %>%
      slice_max(order_by = count, n = 10)
  })

  output$plot1 <- renderPlot({
    # Take data_avg_premium_by_breed and reorder the BREED
    # based on the avg_annualpremium
    data_avg_premium_by_breed_sorted <- data_avg_premium_by_breed() %>%
      mutate(
        BREED = factor(BREED, levels = BREED[order(avg_annualpremium)]),
        bar_color = ifelse(avg_annualpremium == max(avg_annualpremium), "highest", "rest")
      )

    breed_with_highest_avg_premium <- data_avg_premium_by_breed_sorted %>%
      filter(bar_color == "highest")

    data_total_quote_by_breed <- quote_data %>%
      filter(BREED %in% data_avg_premium_by_breed_sorted$BREED) %>%
      group_by(BREED) %>%
      summarize(
        total_quotes = n()
      ) %>%
      mutate(
        BREED = factor(
          BREED,
          levels = levels(data_avg_premium_by_breed_sorted$BREED)
        ),
        bar_color = ifelse(
          BREED == breed_with_highest_avg_premium$BREED,
          "highest",
          "rest"
        )
      )

    data_total_breed_count <- quote_data %>%
      filter(BREED %in% data_avg_premium_by_breed_sorted$BREED)

    avg_highest_premuim_breed_age <- data_total_breed_count %>%
      filter(BREED == data_avg_premium_by_breed_sorted$BREED) %>%
      summarize(avg_age = mean(PETAGEYEARS, na.rm = TRUE)) %>%
      pull(avg_age)

    data_total_breed_count <- data_total_breed_count %>%
        mutate(bar_fill = ifelse(PETAGEYEARS <= avg_highest_premuim_breed_age, "below_avg", "above_avg"))

    # Plot Average annual premium by breed
    a <- ggplot(
      data_avg_premium_by_breed_sorted,
      aes(
        x = BREED,
        y = avg_annualpremium,
        fill = bar_color
      )
    ) +
      geom_bar(stat = "identity", color = "black") +
      scale_fill_manual(
        values = c("highest" = bar_color_primary, "rest" = bar_color_secondary)
      ) +
      scale_y_continuous(labels = scales::label_dollar(prefix = "£")) +
      labs(
        x = "Breed",
        y = "Annual Premium (Average)"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1,
          size = axis_text_size
        ),
        axis.text.y = element_text(size = axis_text_size),
        axis.title.x = element_text(
          margin = margin(t = margin_sm, b = margin_sm),
          size = axis_title_size
        ),
        axis.title.y = element_text(
          margin = margin(r = margin_sm),
          size = axis_title_size
        ),
        legend.position = "none",
        panel.grid.major.x = element_blank()
      )

    # Plot Total quotes by breed
    b <- ggplot(data_total_quote_by_breed, aes(x = reorder(BREED, total_quotes), y = total_quotes, fill = bar_color)) +
      geom_bar(stat = "identity", color = "black") +
      scale_fill_manual(
        values = c("highest" = bar_color_primary, "rest" = bar_color_secondary)
      ) +
      scale_y_continuous(labels = scales::comma) +
      labs(
        x = "Breed"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1,
          size = axis_text_size
        ),
        axis.text.y = element_text(size = axis_text_size),
        axis.title.x = element_text(
          margin = margin(t = margin_sm, b = margin_sm),
          size = axis_title_size
        ),
        axis.title.y = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "none"
      )

    # Plot Number of quotes by pet age
    c <- ggplot(data_total_breed_count, aes(x = PETAGEYEARS)) +
      geom_histogram(
        binwidth = 1,
        color = "black",
        fill = bar_color_secondary
      ) +
      scale_y_continuous(labels = scales::comma) +
      labs(
        x = "Pet Age (Years)",
        y = "Number of Quotes"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = axis_text_size),
        axis.text.y = element_text(size = axis_text_size),
        axis.title.x = element_text(
          margin = margin(t = margin_sm, b = margin_sm),
          size = axis_title_size
        ),
        axis.title.y = element_text(
          margin = margin(r = margin_sm),
          size = axis_title_size
        ),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
      )

    # Convert ggplot objects to grobs
    a_grob <- ggplotGrob(a)
    b_grob <- ggplotGrob(b)
    c_grob <- ggplotGrob(c)

    # Arrange the plots using grid.arrange
    grid.arrange(
      a_grob,
      arrangeGrob(c_grob, b_grob, ncol = 2, widths = c(1, 2)),
      nrow = 2
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
      scale_fill_manual(values = c("Male" = bar_color_secondary, "Female" = bar_color_primary)) +
      labs(
        x = "Pet Age (Years)",
        y = "Average Monthly Premium Instalment"
      ) +
      theme_minimal() +
      scale_y_continuous(labels = scales::label_dollar(prefix = "£")) +
      theme(
        axis.text.x = element_text(size = axis_text_size),
        axis.text.y = element_text(size = axis_text_size),
        axis.title.x = element_text(
          margin = margin(t = margin_sm, b = margin_sm),
          size = axis_title_size
        ),
        axis.title.y = element_text(
          margin = margin(r = margin_sm),
          size = axis_title_size
        ),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = axis_text_size),
        legend.title = element_blank(),
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
