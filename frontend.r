library(tidyverse)
library(fmsb)
# Set seed for reproducibility
set.seed(42)

# Number of years for one supplier
num_years <-600 

# Create a synthetic dataset for one supplier over five years
supplier_data <- tibble(
  year = c(rep(2018,100),rep(2019,100),rep(2020,100),rep(2021,100),rep(2022,100),rep(2023,100)),   # Years
 # supplier_name = rep("Supplier A", num_years),             # Single supplier name
  Energy_Consumption_kWh = rnorm(num_years, mean = 150, sd = 10), # Electric consumption in kWh
  Water_Usage_L = round(runif(num_years, 500, 2000), 2),
  Products_sold = sample(50:200, num_years, replace = TRUE), # Products sold per year
  Num_employees = sample(100:200, num_years, replace = TRUE), # Number of employees
  Employee_Turnover_rate = round(runif(num_years, 5, 20), 2),
  Workforce_Diversity_rate = runif(num_years, min = 0.5, max = 1.5), # Ratio of male-to-female employees
  Num_transports = sample(10:30, num_years, replace = TRUE),  # Number of transports
  Delivery_time = rnorm(num_years, mean = 5, sd = 2)        # Delivery time in days
)

# Add CO2 emissions with correlation to electric consumption
supplier_data <- supplier_data %>%
  mutate(
    Carbon_Emissions_m3 = Energy_Consumption_kWh * runif(num_years, min = 1.2, max = 1.5) + rnorm(num_years, mean = 0, sd = 3)
  )

# Add costs of the product from origin to destination with correlation to delivery time
supplier_data <- supplier_data %>%
  mutate(
    Cost_from_origin = Delivery_time * runif(num_years, min = 800, max = 1200) + rnorm(num_years, mean = 0, sd = 50)
  )

# Introduce correlation between num_employees and Workforce_Diversity_rate
supplier_data <- supplier_data %>% 
  mutate(
    Workforce_Diversity_rate = 0.5 + (Num_employees / 1000) + rnorm(num_years, mean = 0, sd = 0.1)
  ) 

# Add income calculation with consistent usage of runif and rnorm
supplier_data <- supplier_data %>%
  mutate(
    Income = (Products_sold * (runif(num_years, min = 10, max = 5000) + rnorm(num_years, 0, 50)))  
              -(Num_employees * (runif(num_years, min = 8, max = 20) + rnorm(num_years, 0, 5)))
  )

# View the dataset
print(supplier_data)

# View the first few rows of the dataset
library(bnlearn)
library(ggplot2)

workdf<-data.frame(supplier_data)
workdf$year<-as.factor(workdf$year)
workdf$Products_sold<-as.numeric(workdf$Products_sold)
workdf$Num_employees<-as.numeric(workdf$Num_employees)
workdf$Num_transports<-as.numeric(workdf$Num_transports)
workdf$Income<-as.numeric(workdf$Income)


# white list

#wl<-data.frame(from = c("year", "products_sold","electric_consumption", "num_transports", "delivery_time" ),
#               to = c("income","num_employees","co2_emissions","num_transports", "electric_consumption","income", "co2_emissions","delivery_time","cost_from_origin"))

#  blacklist
#bl <- data.frame(from = c("income"), to = c("supplier_name"))

# structure learning
trainig_set<-workdf[1:500,]
#test_set<-workdf[201:300,]
net<-hc(workdf, score = 'bic-cg'
    #, whitelist = wl
   # ,blacklist = bl
)

# parameter leanrig
net_coef<-bn.fit(net, workdf)

library(shiny)

# Define the UI
ui <- fluidPage(
  titlePanel("Monitoring Performances"),
  sidebarLayout(
    sidebarPanel(
        selectInput("node", "Select Attributes of Interest:", choices = names(workdf)),
        selectInput("multi_nodes", "Select Attributes for Utility Calculation:", choices = names(workdf), multiple = TRUE),
        uiOutput("weight_inputs"),
        fileInput("file1", "Upload CSV File for Test Data", accept = ".csv"),
        actionButton("predict_btn", "Show Predictions")
    ),
    mainPanel(
       # plotOutput("networkPlot"),
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


    output$weight_inputs <- renderUI({
    lapply(input$multi_nodes, function(multi_nodes) {

        sliderInput(paste0("weight_", multi_nodes), paste0("Weight for ", multi_nodes), 
                    min = 0, max = 1, value = 0.1)

    })
  })

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
      ###workdf[501:600, ]  # Default test set
      workdf
      
    }
    print(str(test_set))
    # Perform prediction for the selected node
    if (node %in% names(workdf)) {

      predicted_values_plot <- predict(net_coef, node = node, data = test_set, method = "bayes-lw")
      
      output$densityPlot <- renderPlot({
         prediction_df <- tibble(
        Index = 1:length(predicted_values_plot),
        Predicted = predicted_values_plot,
        Year= as.character(c(rep(2018,100),rep(2019,100),rep(2020,100),rep(2021,100),rep(2022,100),rep(2023,100)))
      )
      
    ggplot(prediction_df, aes(x = Index, y = Predicted, colour = Year, group=Year)) +
        ##geom_point(size = 3, alpha = 0.8) +  # Use points instead of lines
        geom_line( size = 1) +
        geom_point(size = 2) +
        geom_smooth(method = "lm", se =TRUE, color='black') +
        scale_color_manual(values = c("2018" = "red", "2019" = "blue", "2020" = "green", 
                                       "2021" = "purple", "2022" = "orange", "2023" = "pink")) +  # Set custom colors
        labs(
            title = paste("Predicted Values for", node),
            x = "Index",
            y = "Predicted Values",
            color = "Year"  # Label for the color legend
        ) +
        theme_minimal() +
        theme(
            plot.title = element_text(size = 20, face = "bold"),  # Customize title size
            axis.text = element_text(size = 12),                 # Customize axis label size
            legend.text = element_text(size = 12)                # Customize legend text size
        )

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

      weight_input <- input[[paste0("weight_", node)]]
          weights <- c(weights, ifelse(is.null(weight_input), 1, weight_input)) 

          
      for (n in multi_nodes) {
        mycount<-mycount+1
        if (n %in% names(workdf)) {
          predicted_values <- predict(net_coef, node = n, data = test_set, method = "bayes-lw")
          predicted_values<-as.numeric(predicted_values)

          if (n == "Carbon_Emissions_m3"){
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
       weights<-rep(1, length(multi_nodes)) / length(multi_nodes)

       #print(w)

       weightsw <- weights / sum(weights)

      #print(length(multi_nodes))
      #print(multi_nodes)
      #print(sum(w))

      expected_utility<- sum(weights*expt_val_subut)

      output$utility <- renderText({
        paste("Expected Utility for selected nodes:", round(expected_utility, 2), "\n",
              if (expected_utility > 0.5) {
                "The supplier is considered good based on the utility score."
              } else {
                "The supplier is not considered good based on the utility score."
              })
      })

   output$utilityPieChart <- renderPlot({    
    
    total_utility <- sum(weights * expt_val_subut)

     radar_data <- as.data.frame(t(weights * expt_val_subut))
    colnames(radar_data) <- multi_nodes
    
    # Add maximum and minimum rows to format the radar chart
    radar_data <- rbind(rep(1, length(multi_nodes)), rep(0, length(multi_nodes)), radar_data)
    
    print(radar_data)
    # Radar chart visualization
    par(mfrow = c(1, 2))  # Side-by-side layout for radar chart and barplot
   
    # Plot the radar chart
    library(fmsb)
    radarchart(
        radar_data,
        axistype = 2,
        title = "Utility Values for Selected Attributes",
        pcol = "blue",
        pfcol = rgb(0, 0, 1, alpha = 0.5), # Semi-transparent fill
        plwd = 2,  # Line width
        cglcol = "grey", # Gridline color
        cglty = 1, # Gridline type
        axislabcol = "black", # Axis label color
        caxislabels = seq(0, 1, 0.2), # Axis label intervals
        vlcex = 1.2 # Variable label size
    )
    
    # Add legend for radar chart
    legend("topright", legend = multi_nodes, fill = "blue", cex = 0.8)
  
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

    #   output$networkPlot <- renderPlot({
   # graphviz.plot(net, main = "Relational Model")  # Plot the network using graphviz.plot
  #})
}

# Run the Shiny app
shinyApp(ui = ui, server = server,options = list(host = '0.0.0.0', port = 8080))