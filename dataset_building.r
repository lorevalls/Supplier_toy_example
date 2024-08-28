
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

# add  correlation:

supplier_data <- supplier_data %>% 
mutate(
  male_female_ratio = 0.5 + (num_employees / 1000) + rnorm(num_suppliers, mean = 0, sd = 0.1)

) 

supplier_data <- supplier_data %>%
mutate(
  income= (products_sold * (runif(products_sold, min=10, max=5000)+rnorm(products_sold, 0,100)))- (num_employees * (runif(num_employees, min=8, max=20)+rnorm(num_employees, 0, 10))
  )
)


# View the first few rows of the dataset
print(head(supplier_data))

# Check correlation to ensure it's in place
correlation_results <- supplier_data %>%
  select(electric_consumption, co2_emissions, delivery_time, cost_from_origin) %>%
  cor()

print(correlation_results)



