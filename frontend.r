library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)

# Simulate supplier data
set.seed(123)
supplier_data <- data.frame(
  Supplier = c('NeuroPulse Solutions',
'BrightCode Labs',
'SynapTech',
'QuantumEdge Systems',
'Lumenify Tech',
'GreenWave Innovations',
'Sesa group',
'SolarBloom',
'NatureBridge',
'RegenEra'
),
  Carbon_Emissions_m3 = round(runif(10, 100, 500), 2),
  Energy_Consumption_kWh = round(runif(10, 1000, 5000), 2),
  Water_Usage_L = round(runif(10, 500, 2000), 2),
  Waste_Recycled_rate = round(runif(10, 50, 95), 2),
  Workforce_Diversity_rate = round(runif(10, 30, 70), 2),
  Employee_Turnover_rate = round(runif(10, 5, 20), 2),
  Lost_time_incident_rate = round(runif(10, 0.2, 1.5), 2),
  Number_of_corruption_incidents= round(runif(10, 0, 5), 2),
  Customer_Satisfaction_rate = round(runif(10, 50, 100), 2)
)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Sustainability Scoreboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
    ),
    selectInput("selected_supplier", "Select a Supplier:", choices = c("All", supplier_data$Supplier), selected = "All"),
    sliderInput("w_carbon", "Carbon Emissions", min = 0, max = 1, value = 0.1, step = 0.01, width = "90%"),
    sliderInput("w_energy", "Energy Consumption", min = 0, max = 1, value = 0.1, step = 0.01, width = "90%"),
    sliderInput("w_water", "Water Usage", min = 0, max = 1, value = 0.1, step = 0.01, width = "90%"),
    sliderInput("w_recycle", "Waste Recycled", min = 0, max = 1, value = 0.1, step = 0.01, width = "90%"),
    sliderInput("w_diversity", "Workforce Diversity", min = 0, max = 1, value = 0.1, step = 0.01, width = "90%"),
    sliderInput("w_turnover", "Employee Turnover", min = 0, max = 1, value = 0.1, step = 0.01, width = "90%"),
    sliderInput("w_safety", "Health and Safety", min = 0, max = 1, value = 0.1, step = 0.01, width = "90%"),
    sliderInput("w_corruption", "Anti-Corruption", min = 0, max = 1, value = 0.1, step = 0.01, width = "90%"),
    sliderInput("w_satisfaction", "Customer Satisfaction", min = 0, max = 1, value = 0.1, step = 0.01, width = "90%"),
     h4("Weight Sum Validation"),
      textOutput("weight_sum_alert", inline = TRUE)
  ),
  
  dashboardBody(
    fluidRow(
      box(
        width = 12,
        DTOutput("supplier_table"),
        title = "Supplier ESG Scores",
        status = "primary",
        solidHeader = TRUE
      )
    )
  )
)

# Server
server <- function(input, output, session) {

    observe({
    total_weight <- sum(
      input$w_carbon, input$w_energy, input$w_water, input$w_recycle, input$w_diversity,
      input$w_turnover, input$w_safety, input$w_corruption, input$w_satisfaction
    )
    
    # Alert if the sum exceeds 1
    if (total_weight > 1) {
      shinyalert::shinyalert(
        title = "Weight Error",
        text = paste("The total weight exceeds 1. Current sum:", round(total_weight, 2)),
        type = "error"
      )
    }
  })
  
  # Display total weight sum
  output$weight_sum_alert <- renderText({
    total_weight <- sum(
      input$w_carbon, input$w_energy, input$w_water, input$w_recycle, input$w_diversity,
      input$w_turnover, input$w_safety, input$w_corruption, input$w_satisfaction
    )
    if (total_weight > 1) {
      paste("⚠️ Total weight exceeds 1! (", round(total_weight, 2), ")")
    } else {
      paste("✅ Total weight is valid. (", round(total_weight, 2), ")")
    }
  })
  
  
  # Reactive data processing
  ranked_data <- reactive({
    data <- supplier_data
    
    # Utility functions
    Ut_low <- function(x) (max(x) - x) / (max(x) - min(x))
    Ut_high <- function(x) (x - min(x)) / (max(x) - min(x))
    
    # Normalize and rank data
    data <- data %>%
      mutate(
        Carbon_Emissions_Score = Ut_low(Carbon_Emissions_m3),
        Energy_Consumption_Score = Ut_low(Energy_Consumption_kWh),
        Water_Usage_Score = Ut_low(Water_Usage_L),
        Waste_Recycled_Score = Ut_high(Waste_Recycled_rate),
        Workforce_Diversity_Score = Ut_high(Workforce_Diversity_rate),
        Employee_Turnover_Score = Ut_low(Employee_Turnover_rate),
        Health_and_Safety_Score = Ut_high(Lost_time_incident_rate),
        Anti_Corruption_Score = Ut_low(Number_of_corruption_incidents),
        Customer_Satisfaction_Score = Ut_high(Customer_Satisfaction_rate)
      )
    
    # Apply weights
    data <- data %>%
      mutate(
        ESG_Ranking = Carbon_Emissions_Score * input$w_carbon +
          Energy_Consumption_Score * input$w_energy +
          Water_Usage_Score * input$w_water +
          Waste_Recycled_Score * input$w_recycle +
          Workforce_Diversity_Score * input$w_diversity +
          Employee_Turnover_Score * input$w_turnover +
          Health_and_Safety_Score * input$w_safety +
          Anti_Corruption_Score * input$w_corruption +
          Customer_Satisfaction_Score * input$w_satisfaction)

      data<- data %>% mutate(ESG_Ranking=round(ESG_Ranking,2))

        # Add rank column and sort by ESG score
    data <- data[order(-data$ESG_Ranking), ]
     ##data$Rank <- rank(-data$ESG_Score, ties.method = "min")
    
    # Filter by selected supplier
    if (input$selected_supplier != "All") {
      data <- data[data$Supplier == input$selected_supplier, ]
    }
    
    return(data)
  })
  
  # Output the table
  output$supplier_table <- renderDT({
    data <- ranked_data()
    datatable(data, options = list(pageLength = 10), rownames = FALSE)
    data$Actions <- paste0(
    '<button class="btn btn-warning" onclick="window.open(\'https://www.sesa.it/\', \'_blank\')">CHECK</button>',
    ' <button class="btn btn-success">INVITE</button>',
    ' <button class="btn btn-danger">REMOVE</button>'
  )
    # Select columns to display
  datatable(
    data[, c("Supplier", "Carbon_Emissions_m3", "Energy_Consumption_kWh", "Water_Usage_L", "Waste_Recycled_rate", 
             "Workforce_Diversity_rate", "Employee_Turnover_rate", "Lost_time_incident_rate", "Customer_Satisfaction_rate","Number_of_corruption_incidents", "ESG_Ranking", "Actions")],
    escape = FALSE, options = list(pageLength = 10, autoWidth = TRUE)
  )

  })
}


# Run the application
shinyApp(ui = ui, server = server,options = list(host = '0.0.0.0', port = 8080))


