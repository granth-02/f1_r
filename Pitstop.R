# Load required libraries
library(httr)
library(jsonlite)
library(dplyr)
library(purrr)

# Function to fetch race data for a specific season and round
fetch_race_data <- function(season, round) {
  base_url <- paste0("https://ergast.com/api/f1/", season, "/", round)
  endpoint <- "/pitstops.json"
  url <- paste0(base_url, endpoint)
  response <- GET(url)
  if (http_error(response)) {
    stop("Error fetching data:", content(response, "text"))
  }
  return(content(response, "parsed"))
}

# Function to fetch pitstop data for all rounds of a specific season
fetch_season_pitstop_data <- function(season) {
  all_rounds <- 1:20  # Assuming 20 rounds in a season, adjust if needed
  pitstop_times <- map_dbl(all_rounds, function(round) {
    race_data <- fetch_race_data(season, round)
    if (length(race_data$MRData$RaceTable$Races) > 0) {
      pitstops <- race_data$MRData$RaceTable$Races[[1]]$PitStops
      if (length(pitstops) > 0) {
        return(mean(as.numeric(sapply(pitstops, function(pitstop) pitstop$duration))))
      }
    }
    return(NA)  # Return NA if no pitstop data is available for the round
  })
  return(pitstop_times[!is.na(pitstop_times)])  # Remove NA values
}

# Function to calculate average pitstop timing for each season from 2011 to 2023
calculate_season_average_pitstop_timing <- function(seasons) {
  season_averages <- map_dbl(seasons, ~ mean(fetch_season_pitstop_data(.x)))
  return(season_averages)
}

# Calculate average pitstop timing for each season from 2011 to 2023
seasons <- 2011:2023
season_average_pitstop_timings <- calculate_season_average_pitstop_timing(seasons)

# Print the results
for (i in seq_along(seasons)) {
  cat("Average pitstop timing for the", seasons[i], "season:", round(season_average_pitstop_timings[i], 2), "seconds\n")
}

pitstop_data <- data.frame(
  Season = seasons,
  Average_Pitstop_Timing = season_average_pitstop_timings
)

ggplot(pitstop_data, aes(x = Season, y = Average_Pitstop_Timing)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Average Pitstop Timing by Season (2011 - 2023)",
    x = "Season",
    y = "Average Pitstop Timing (seconds)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

# Function to calculate the shortest pitstop time for a specific year and round
calculate_shortest_pitstop_time <- function(year, round) {
  race_data <- fetch_pitstop_data(year, round)
  pitstops <- race_data$MRData$RaceTable$Races[[1]]$PitStops
  if (length(pitstops) > 0) {
    shortest_time <- min(as.numeric(sapply(pitstops, function(pitstop) pitstop$duration)))
    return(shortest_time)
  } else {
    return(NA)  # No pitstop data available for the round
  }
}

# Function to calculate the shortest pitstop time for each round in a year
calculate_shortest_pitstop_times <- function(year) {
  rounds <- 1:10  # Assuming there are 22 rounds in a season
  shortest_times <- sapply(rounds, function(round) calculate_shortest_pitstop_time(year, round))
  return(shortest_times)
}

calculate_shortest_pitstop_time(2023)

# Calculate the shortest pitstop time for each year from 2011 to 2023
years <- 2021:2023
shortest_pitstop_times <- sapply(years, calculate_shortest_pitstop_times)

# Combine the results into a data frame
shortest_pitstop_df <- data.frame(Year = rep(years, each = 22),
                                  Round = rep(1:22, length(years)),
                                  Shortest_Pitstop_Time = unlist(shortest_pitstop_times))

# Calculate the shortest pitstop time for each year
shortest_pitstop_year <- shortest_pitstop_df %>%
  group_by(Year) %>%
  summarise(Shortest_Pitstop_Time = min(Shortest_Pitstop_Time, na.rm = TRUE))

# Create a bar plot for the shortest pitstop time of each year
ggplot(shortest_pitstop_year, aes(x = Year, y = Shortest_Pitstop_Time)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.7) +
  labs(
    title = "Shortest Pitstop Time by Year (2011 - 2023)",
    x = "Year",
    y = "Shortest Pitstop Time (seconds)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels
