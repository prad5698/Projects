install.packages("ggplot2")
install.packages("dplyr")
install.packages("readxl")


# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readxl)

library(readxl)

# Load the sample dataset
machining_data <- read_excel("C:\\Users\\Pradyumna\\Desktop\\toolwear_sample_data_R.xlsx")


# Define a function to simulate tool wear based on machining parameters
simulate_tool_wear <- function(feed_rate, spindle_speed, depth_of_cut) {
  # Simulate tool wear using a linear regression model (replace with your own model)
  tool_wear <- 0.2 * feed_rate + 0.1 * spindle_speed + 0.15 * depth_of_cut + rnorm(1, mean = 0, sd = 1)
  return(tool_wear)
}

# Define a threshold for tool wear that triggers maintenance
maintenance_threshold <- 10

# Initialize variables
current_tool_wear <- 0
total_tool_wear <- 0
maintenance_needed <- FALSE

# Simulate machining for each row in the dataset
for (i in 1:nrow(machining_data)) {
  # Extract machining parameters
  feed_rate <- machining_data$Feed_Rate[i]
  spindle_speed <- machining_data$Spindle_Speed[i]
  depth_of_cut <- machining_data$Depth_of_cut[i]
  
  # Simulate tool wear for this operation
  tool_wear <- simulate_tool_wear(feed_rate, spindle_speed, depth_of_cut)
  
  # Update current tool wear and total tool wear
  current_tool_wear <- current_tool_wear + tool_wear
  total_tool_wear <- total_tool_wear + tool_wear
  
  # Check if maintenance is needed
  if (current_tool_wear > maintenance_threshold) {
    cat("Maintenance needed at operation", i, "\n")
    maintenance_needed <- TRUE
    # Reset current_tool_wear after maintenance
    current_tool_wear <- 0
  }
  
  # Update the dataset with the simulated tool wear for this operation (optional)
  machining_data$Tool_Wear[i] <- tool_wear
}

# Print the total tool wear
cat("Total Tool Wear:", total_tool_wear, "\n")

# Print maintenance status
if (maintenance_needed) {
  cat("Maintenance performed.\n")
} else {
  cat("No maintenance needed.\n")
  
}

# Visualize the tool wear over time (optional)
ggplot(data = machining_data, aes(x = 1:nrow(machining_data), y = Tool_Wear)) +
  geom_line() +
  labs(x = "Operation Number", y = "Tool Wear") +
  theme_minimal()

