
# Load necessary library
library(tidyverse)

# Set seed for reproducibility
set.seed(42)

# Number of suppliers
num_suppliers <- 300

# Create a synthetic dataset
supplier_data <- tibble(
  supplier_name = paste("Supplier", 1:num_suppliers),                # Supplier's name (e.g., Supplier 1, Supplier 2, ...)
  electric_consumption = rnorm(num_suppliers, mean = 150, sd = 30),  # Electric consumption in kWh
  products_sold = sample(50:200, num_suppliers, replace = TRUE),     # Products sold to the company
  num_employees = sample(100:500, num_suppliers, replace = TRUE),    # Number of employees
  male_female_ratio = runif(num_suppliers, min = 0.5, max = 1.5),    # Ratio between male and female employees
  num_transports = sample(10:50, num_suppliers, replace = TRUE),     # Number of transports
  delivery_time = rnorm(num_suppliers, mean = 5, sd = 2.5)           # Time for deliveries in days
)

# Add CO2 emissions with correlation to electric consumption
supplier_data <- supplier_data %>%
  mutate(
    co2_emissions = electric_consumption * runif(num_suppliers, min = 1.2, max = 1.5) + rnorm(num_suppliers, mean = 0, sd = 10)
  )

# Add costs of the product from the beginning to the destination with correlation to delivery time
supplier_data <- supplier_data %>%
  mutate(
    cost_from_origin = delivery_time * runif(num_suppliers, min = 800, max = 1200) + rnorm(num_suppliers, mean = 0, sd = 100)
  )

# Introduce correlation between num_employees and male_female_ratio
supplier_data <- supplier_data %>% 
  mutate(
    male_female_ratio = 0.5 + (num_employees / 1000) + rnorm(num_suppliers, mean = 0, sd = 0.1)
  ) 

# Correct the income calculation with consistent usage of runif and rnorm
supplier_data <- supplier_data %>%
  mutate(
    income = (products_sold * (runif(num_suppliers, min=10, max=5000) + rnorm(num_suppliers, 0, 100)))  
              -(num_employees * (runif(num_suppliers, min=8, max=20) + rnorm(num_suppliers, 0, 10)))
  )

# View the first few rows of the dataset
library(bnlearn)
library(ggplot2)

workdf<-data.frame(supplier_data)
workdf$supplier_name<-as.factor(workdf$supplier_name)
workdf$products_sold<-as.numeric(workdf$products_sold)
workdf$num_employees<-as.numeric(workdf$num_employees)
workdf$num_transports<-as.numeric(workdf$num_transports)
workdf$income<-as.numeric(workdf$income)


# white list

wl<-data.frame(from = "supplier_name", to = c("income","num_employees","co2_emissions"))

#  blacklist
bl <- data.frame(from = c("income"), to = c("supplier_name"))

# structure learning
trainig_set<-workdf[1:200,]
#test_set<-workdf[201:300,]
net<-hc(workdf, score = 'bic-cg'
    , whitelist = wl
    ,blacklist = bl
)

# parameter leanrig
net_coef<-bn.fit(net, workdf)

library(shiny)

# Define the UI
ui <- fluidPage(
  titlePanel("Bayesian Network Visualization"),
  sidebarLayout(
    sidebarPanel(
        selectInput("node", "Select Node of Interest:", choices = names(workdf)),
        fileInput("file1", "Upload CSV File for Test Data", accept = ".csv"),
        actionButton("predict_btn", "Show Predictions")
    ),
    mainPanel(
        plotOutput("networkPlot"),
        plotOutput("densityPlot"),
        textOutput("summary")

    )
  )
)

# Define the server logic
server <- function(input, output) {

    print("Server started")

    test_data <- reactive({
    req(input$file1)  # Ensure a file is uploaded
    data<-read.csv(input$file1$datapath)

    # Transform all integer columns to numeric
    data <- data %>% mutate(across(where(is.integer), as.numeric))
    data <- data %>% mutate(across(where(is.character), as.factor))
    

    # Print the data types for debugging
    print(str(data))
    
    return(data)
  })
  
  observeEvent(input$predict_btn, {
    node <- input$node

    test_set <- if (!is.null(input$file1)) {
      test_data()
    } else {
      workdf[201:300, ]  # Default test set
    }
    
    # Perform prediction for the selected node
    if (node %in% names(workdf)) {


      predicted_values <- predict(net_coef, node = node, data = test_set, method = "bayes-lw")
      
      output$densityPlot <- renderPlot({
        plot(density(predicted_values), main = paste("Density Plot for Predicted", node), col = "blue", lwd = 2)
        abline(v = mean(predicted_values), col = 'blue', lwd = 2, lty = 2)
        #lines(density(test_set[[node]]), col = 'red', lwd = 2)
        #abline(v = mean(test_set[[node]]), col = 'red', lwd = 2, lty = 2)
        legend("topright", legend = c("Predicted", "Training"), col = c("blue", "red"), lty = 1, lwd = 2)
      })
      


      output$summary <- renderText({
        paste("Summary statistics for predicted", node, ":\n",
              "Mean:", round(mean(predicted_values), 2), "\n",
              "SD:", round(sd(predicted_values), 2))
      })
    }
  })

       output$networkPlot <- renderPlot({
    graphviz.plot(net, main = "Bayesian Network Structure")  # Plot the network using graphviz.plot
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server,options = list(host = '0.0.0.0', port = 8080))