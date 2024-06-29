# Load required libraries
library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)

# Function to fetch constructor standings for a specific season
get_constructor_standings <- function(season) {
  base_url <- "https://ergast.com/api/f1"
  url <- paste(base_url, "/", season, "/constructorStandings.json", sep = "")
  response <- httr::GET(url)
  if (http_error(response)) {
    stop("Error fetching data:", content(response, "text"))
  }
  return(content(response, "parsed"))
}

# Function to extract and process constructor standings data
process_constructor_standings <- function(data) {
  constructor_standings <- data$MRData$StandingsTable$StandingsLists[[1]]$ConstructorStandings
  
  # Merge Alpine, Renault, and Lotus F1 into one team name (e.g., Alpine)
  for (i in seq_along(constructor_standings)) {
    if (constructor_standings[[i]]$Constructor$name %in% c("Alpine F1 Team", "Renault", "Lotus F1")) {
      constructor_standings[[i]]$Constructor$name <- "Alpine"
    }
    if (constructor_standings[[i]]$Constructor$name %in% c("AlphaTauri", "Toro Rosso", "RB F1 Team")) {
      constructor_standings[[i]]$Constructor$name <- "RB"
    }
    if (constructor_standings[[i]]$Constructor$name %in% c("Alfa Romeo", "Sauber")) {
      constructor_standings[[i]]$Constructor$name <- "Sauber"
    }
    if (constructor_standings[[i]]$Constructor$name %in% c("Force India", "Aston Martin", "Racing Point")) {
      constructor_standings[[i]]$Constructor$name <- "Aston Martin"
    }
    if (constructor_standings[[i]]$Constructor$name %in% c("Haas F1 Team", "Manor Marussia")) {
      constructor_standings[[i]]$Constructor$name <- "Haas"
    }
    
  }
  
  constructor_df <- data.frame(
    Season = rep(as.integer(data$MRData$StandingsTable$StandingsLists[[1]]$season), length(constructor_standings)),
    Team = sapply(constructor_standings, function(x) x$Constructor$name),
    Points = sapply(constructor_standings, function(x) as.numeric(x$points))
  )
  return(constructor_df)
}

# Initialize an empty data frame to store constructor standings
constructor_standings_data <- data.frame()

# Loop through each season from 2010 to 2024
for (season in 2010:2024) {
  # Fetch constructor standings for the season
  constructor_standings_season <- get_constructor_standings(season)
  
  # Process constructor standings data
  season_constructor_standings <- process_constructor_standings(constructor_standings_season)
  
  # Append constructor standings data to the overall data frame
  constructor_standings_data <- bind_rows(constructor_standings_data, season_constructor_standings)
}

# Filter data for specific teams
teams_to_include <- c("Alpine", "Aston Martin", "Red Bull", "Ferrari", "McLaren", "Williams", "Mercedes", "Haas", "RB", "Sauber")
filtered_data <- constructor_standings_data %>%
  filter(Team %in% teams_to_include)

# Customize colors for each team
# Customize colors for each team
team_colors <- c("Alpine" = "#fa57a9", "Aston Martin" = "darkgreen", "Red Bull" = "navy", 
                 "Ferrari" = "red", "McLaren" = "orange", "Williams" = "#a29dfa", 
                 "Mercedes" = "#02b0b0", "Haas" = "gray", "RB" = "blue", "Sauber" = "brown")

# Plotting the comparison
ggplot(filtered_data, aes(x = Season, y = Points, color = Team)) +
  geom_line() +
  geom_point() +
  labs(title = "Total Points Scored by Each Team in Formula 1 (2010-2024)",
       x = "Season",
       y = "Total Points") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = team_colors)+
  scale_x_continuous(breaks = seq(2010, 2024, by = 1))

# Initialize an empty data frame to store constructor championship counts
championship_counts <- data.frame(Team = character(), Championships = numeric())

# Loop through each season from 1950 to 2023
for (season in 1950:2023) {
  # Fetch constructor standings for the season
  constructor_standings_season <- try(get_constructor_standings(season), silent = TRUE)
  
  # Check if fetching data was successful
  if (inherits(constructor_standings_season, "try-error")) {
    next  # Skip to the next iteration if there was an error fetching the data
  }
  
  # Extract the constructor standings data
  constructor_standings <- constructor_standings_season$MRData$StandingsTable$StandingsLists
  
  # Check if the data structure is empty
  if (length(constructor_standings) == 0) {
    next  # Skip to the next iteration if there are no standings data
  }
  
  # Get the winning team (assuming the first team is the winner)
  winning_team <- constructor_standings[[1]]$ConstructorStandings[[1]]$Constructor$name
  
  # Check if the winning team is already in the championship_counts data frame
  if (winning_team %in% championship_counts$Team) {
    # Increment the count of championships for the winning team
    championship_counts$Championships[championship_counts$Team == winning_team] <- 
      championship_counts$Championships[championship_counts$Team == winning_team] + 1
  } else {
    # Add the winning team to the championship_counts data frame
    championship_counts <- rbind(championship_counts, data.frame(Team = winning_team, Championships = 1))
  }
}

teams_to_include <- c("Benetton", "Brabham-Repco", "Red Bull", "Ferrari", "McLaren", 
                      "Williams", "Mercedes", "Brawn", "BRM", "Cooper-Climax", "Lotus-Climax", "Lotus-Ford", "Vanwall")

filtered_data <- championship_counts %>%
  filter(Team %in% teams_to_include)

# Corrected color vector, from 'my_colors' to 'team_colors'
team_colors <- c("Benetton" = "cyan", "Brabham-Repco" = "purple", "Red Bull" = "navy", 
                 "Ferrari" = "red", "McLaren" = "orange", "Williams" = "#a29dfa", 
                 "Mercedes" = "#02b0b0", "Brawn" = "#32CD32", "BRM" = "pink", "Cooper-Climax" = "brown",
                 "Lotus-Climax" = "gold", "Lotus-Ford" = "gray", "Vanwall" = "#36453a")

# Plotting
ggplot(filtered_data, aes(x = "", y = Championships, fill = Team)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y", start = 0) +
  labs(title = "Number of Constructor Championships by Team", fill = "Team") +
  theme_void() +
  theme(legend.position = "right") +
  geom_text(aes(label = Championships), position = position_stack(vjust = 0.5), color = "white") +
  scale_fill_manual(values = team_colors)  # Using the correct color vector