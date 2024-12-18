
# Load necessary library
library(tidyverse)

# Set seed for reproducibility
set.seed(42)

# Number of suppliers
num_suppliers <- 300

# Create a synthetic dataset
supplier_data <- tibble(
  supplier_name = paste("Supplier", 1:num_suppliers),                # Supplier's name (e.g., Supplier 1, Supplier 2, ...)
  electric_consumption = rnorm(num_suppliers, mean = 150, sd = 100),  # Electric consumption in kWh
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

wl<-data.frame(from = c("supplier_name","supplier_name","supplier_name","supplier_name","supplier_name", "products_sold","electric_consumption", "num_transports", "delivery_time" ),
               to = c("income","num_employees","co2_emissions","num_transports", "electric_consumption","income", "co2_emissions","delivery_time","cost_from_origin"))

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
  titlePanel("SISS Project"),
  sidebarLayout(
    sidebarPanel(
        selectInput("node", "Select Attributes of Interest:", choices = names(workdf)),
        selectInput("multi_nodes", "Select Attributes for Utility Calculation:", choices = names(workdf), multiple = TRUE),
        fileInput("file1", "Upload CSV File for Test Data", accept = ".csv"),
        actionButton("predict_btn", "Show Predictions")
    ),
    mainPanel(
        plotOutput("networkPlot"),
        plotOutput("densityPlot"),
        plotOutput("utilityPieChart"),
        textOutput("utility"),
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
     data$electric_consumption<-as.numeric(data$electric_consumption)


    # Print the data types for debugging
    print(str(data))
    
    return(data)
  })
  
  observeEvent(input$predict_btn, {

    node <- input$node
    multi_nodes <- input$multi_nodes

    test_set <- if (!is.null(input$file1)) {
      test_data()
    } else {
      workdf[201:300, ]  # Default test set
    }
    print(str(test_set))
    # Perform prediction for the selected node
    if (node %in% names(workdf)) {

      predicted_values_plot <- predict(net_coef, node = node, data = test_set, method = "bayes-lw")
      
      output$densityPlot <- renderPlot({
        plot(density(predicted_values_plot), main = paste("Prediction of ", node), col = "blue", lwd = 2)
        abline(v = mean(predicted_values_plot), col = 'blue', lwd = 2, lty = 2)
        #lines(density(test_set[[node]]), col = 'red', lwd = 2)
        #abline(v = mean(test_set[[node]]), col = 'red', lwd = 2, lty = 2)
        legend("topright", legend = c("Predicted"), col = c("blue"), lty = 1, lwd = 2)
      })
       

      output$summary <- renderText({
        paste("Summary statistics for predicted", node, ":\n",
              "Mean:", round(mean(predicted_values_plot), 2), "\n",
              "SD:", round(sd(predicted_values_plot), 2))
      })
    }

    if (length(multi_nodes) > 0) {
      utility_values <- matrix(NA, nrow=length(multi_nodes),ncol=2)
      mycount<-0
      for (n in multi_nodes) {
        mycount<-mycount+1
        if (n %in% names(workdf)) {
          predicted_values <- predict(net_coef, node = n, data = test_set, method = "bayes-lw")
          predicted_values<-as.numeric(predicted_values)

          if (n == "co2_emissions"){
            Ut=function(x){ 
                ut=(max(x)-x)/(max(x)-min(x))
            return(ut) }

          } else{
            Ut=function(x){
              ut=(x-min(x))/(max(x)-min(x))
            return(ut)}}
          
          exp_ut<-mean(Ut(predicted_values))
          utility_values[mycount,2]<-as.numeric(exp_ut)
          utility_values[mycount,1]<-n

          ##for debug
          #print(utility_values)
          #print(is.numeric(utility_values))
          #print(mycount)
          #print(predicted_values)
        }

      }
      expt_val_subut<-round(as.numeric(utility_values[,2]),2)
       print(rep(1, length(multi_nodes)) )
       w<-rep(1, length(multi_nodes)) / length(multi_nodes)

       #print(w)

       w <- w / sum(w)

      #print(length(multi_nodes))
      #print(multi_nodes)
      #print(sum(w))

      expected_utility<- sum(w*expt_val_subut)

      output$utility <- renderText({
        paste("Expected Utility for selected nodes:", round(expected_utility, 2), "\n",
              if (expected_utility > 0.5) {
                "The supplier is considered good based on the utility score."
              } else {
                "The supplier is not considered good based on the utility score."
              })
      })

   output$utilityPieChart <- renderPlot({    
    
    total_utility <- sum(w * expt_val_subut)
    par(mfrow = c(1, 3))  # 1 row, 2 columns
  
   pie((w*expt_val_subut),        
    labels = round(w*expt_val_subut, 2),
        main = "Utility Values for Selected Attributes", 
      col = rainbow(length(expt_val_subut)))

    
       legend('topright',multi_nodes,
      fill=rainbow(length((expt_val_subut)))
)
 
  
   barplot(
    total_utility, 
    names.arg = "Total utility", 
    col = "blue" ,
    main = "Utility Values for Selected Attributes", 
    ylim = c(0, 1),           # Set y-axis limit from 0 to 1 for better visualization
    ylab = "Utility",
    width = 0.2             # Set the bar width
    ,xlim = c(-1 , 1.5)       # Narrow x-axis to avoid too much space
    #, axes = FALSE               # Suppress x-axis to prevent unnecessary long ax
  )
  
  # Add a red horizontal line at 0.5 to represent the threshold
  abline(h = 0.5, col = "red", lwd = 2, lty = 2)  
  
  text(
    x = 0.15,                    # The x-position for the text (aligned with the bar)
    y = total_utility,         # The y-position (on top of the bar)
    labels = round(total_utility, 2),  # Show the rounded total utility
    pos = 3,                  # Position above the bar
    cex = 2,                # Font size
    col = "#1e00ff"           # Text color
  )
  
  # Add a legend explaining the red line
  legend("topright", legend = "Threshold (0.5)", col = "red", lty = 2, lwd = 2)
})
   }



  
  })

       output$networkPlot <- renderPlot({
    graphviz.plot(net, main = "Relational Model")  # Plot the network using graphviz.plot
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server,options = list(host = '0.0.0.0', port = 8080))