# Load required libraries
library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)

# Function to fetch F1 circuit data from Ergast API
fetch_circuit_data <- function() {
  endpoint <- "/circuits.json"
  base_url <- "https://ergast.com/api/f1"
  url <- paste0(base_url, endpoint)
  response <- GET(url)
  if (http_error(response)) {
    stop("Error fetching data:", content(response, "text"))
  }
  return(content(response, "parsed")$MRData$CircuitTable$Circuits)
}

# Fetch F1 circuit data
circuit_data <- fetch_circuit_data()

# Extract relevant circuit attributes
circuit_attributes <- lapply(circuit_data, function(circuit) {
  data.frame(
    circuitId = circuit$circuitId,
    circuitName = circuit$circuitName,
    latitude = as.numeric(circuit$Location$lat),
    longitude = as.numeric(circuit$Location$long)
  )
})

# Combine all circuit attributes into one dataframe
circuit_attributes <- do.call(rbind, circuit_attributes)

# Print sample data
head(circuit_attributes)

# Visualize circuit attributes (e.g., location on a map)
# Visualize circuit attributes with improved readability
# Visualize circuit attributes with distinctive colors and a separate legend
# Visualize circuit attributes with distinctive colors and a separate legend
ggplot(circuit_attributes, aes(x = longitude, y = latitude, fill = circuitName)) +
  geom_point(alpha = 0.7, color = "black", size = 3, shape = 21) +  # Customize point appearance
  labs(title = "F1 Circuits Location") +
  theme_minimal() +
  scale_fill_manual(name = "Circuit Name", values = rainbow(length(unique(circuit_attributes$circuitName)))) +  # Assign distinctive colors
  scale_x_continuous(limits = c(-180, 180)) +  # Adjust x-axis scale
  scale_y_continuous(limits = c(-90, 90))      # Adjust y-axis scale