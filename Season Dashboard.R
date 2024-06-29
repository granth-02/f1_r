library(shiny)
library(ggplot2)
library(dplyr)

# Load required libraries
library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(purrr)

# Define UI
ui <- fluidPage(
  titlePanel("F1 Analysis Dashboard"),
  
  tabsetPanel(
    tabPanel("Constructor Standings", plotOutput("constructor_standings")),
    tabPanel("Driver Standings", plotOutput("driver_standings")),
    tabPanel("Race Results", tableOutput("race_results")),
    tabPanel("Overall Constructor Points", plotOutput("overall_constructor_points")),
    tabPanel("Average Finishing Position", plotOutput("avg_finish_position"))
  )
)

# Define server logic
server <- function(input, output) {
  # Fetch data from Ergast API
  fetch_data <- function(endpoint) {
    base_url <- "https://ergast.com/api/f1/2024"
    url <- paste0(base_url, endpoint)
    response <- GET(url)
    if (http_error(response)) {
      stop("Error fetching data:", content(response, "text"))
    }
    return(content(response, "parsed"))
  }
  
  # Fetch constructor standings data
  constructor_standings <- fetch_data("/constructorStandings.json")
  constructor_data <- constructor_standings$MRData$StandingsTable$StandingsLists[[1]]$ConstructorStandings
  constructor_df <- data.frame(
    Team = sapply(constructor_data, function(x) x$Constructor$name),
    Points = sapply(constructor_data, function(x) as.numeric(x$points))
  )
  
  # Fetch driver standings data
  drivers_standings <- fetch_data("/driverStandings.json")
  drivers_data <- drivers_standings$MRData$StandingsTable$StandingsLists[[1]]$DriverStandings
  drivers_df <- data.frame(
    Driver = sapply(drivers_data, function(x) paste(x$Driver$givenName, x$Driver$familyName, sep = " ")),
    Points = sapply(drivers_data, function(x) as.numeric(x$points))
  )
  
  # Fetch all race results data
  fetch_all_race_results <- function() {
    all_races <- list()
    offset <- 0
    limit <- 100  # Adjust as needed
    
    repeat {
      race_results <- fetch_data(sprintf("/results.json?limit=%d&offset=%d", limit, offset))
      races <- race_results$MRData$RaceTable$Races
      if (length(races) == 0) {
        break  # No more races
      }
      all_races <- c(all_races, races)
      offset <- offset + limit
    }
    
    return(all_races)
  }
  
  # Process race results data
  all_races_data <- fetch_all_race_results()
  race_df <- bind_rows(lapply(all_races_data, function(race) {
    race_location <- race$Circuit$Location$locality
    race_name <- race$raceName
    race_results <- race$Results
    
    bind_rows(lapply(race_results, function(result) {
      driver_name <- paste(result$Driver$givenName, result$Driver$familyName, sep = " ")
      position <- as.numeric(result$position)
      
      data.frame(Race_Location = race_location, Race_Name = race_name, Driver_Name = driver_name, Position = position)
    }))
  }))
  
  # Calculate constructor points scored in each separate race
  constructor_points_race <- lapply(all_races_data, function(race) {
    race_name <- race$raceName
    race_results <- race$Results
    
    constructor_points <- lapply(race_results, function(result) {
      constructor_name <- result$Constructor$name
      points <- as.numeric(result$points)
      data.frame(Race_Name = race_name, Constructor_Name = constructor_name, Points = points)
    })
    
    do.call(rbind, constructor_points)
  })
  
  constructor_points_df <- do.call(rbind, constructor_points_race)
  
  # Calculate overall average points scored by each constructor
  overall_constructor_points <- constructor_points_df %>%
    group_by(Constructor_Name) %>%
    summarise(Total_Points = sum(Points, na.rm = TRUE),
              Average_Points = mean(Points, na.rm = TRUE))
  
  # Calculate average finishing position for each driver
  avg_finish_position <- race_df %>%
    group_by(Driver_Name) %>%
    summarise(Avg_Finish_Position = mean(Position, na.rm = TRUE))
  
  team_colors <- c("Alpine" = "#fa57a9", "Aston Martin" = "darkgreen", "Red Bull" = "navy", 
                   "Ferrari" = "red", "McLaren" = "orange", "Williams" = "#a29dfa", 
                   "Mercedes" = "#02b0b0", "Haas" = "gray", "RB" = "blue", "Sauber" = "brown")
  
  # Output constructor standings
  output$constructor_standings <- renderPlot({
    ggplot(constructor_df, aes(x = reorder(Team, -Points), y = Points, fill = Team)) +
      geom_bar(stat = "identity") +
      labs(title = "Constructor Standings (2024 F1 Season)",
           x = "Team",
           y = "Points") +
      theme_minimal() +
      scale_fill_manual(values = team_colors) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Output driver standings
  output$driver_standings <- renderPlot({
    ggplot(drivers_df, aes(x = reorder(Driver, -Points), y = Points, fill = Driver)) +
      geom_bar(stat = "identity") +
      labs(title = "Driver Standings (2024 F1 Season)",
           x = "Driver",
           y = "Points") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Output race results
  output$race_results <- renderTable({
    race_df
  })
  
  # Output overall constructor points
  output$overall_constructor_points <- renderPlot({
    ggplot(overall_constructor_points, aes(x = Constructor_Name, y = Average_Points, fill = Constructor_Name)) +
      geom_point(shape = 21, color = "black", size = 3) +
      labs(title = "Overall Average Points Scored by Each Constructor",
           x = "Constructor",
           y = "Average Points") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_manual(name = "Constructor", values = team_colors) +
      geom_abline(intercept = mean(overall_constructor_points$Average_Points), slope = 0, color = "darkred", linetype = "dashed")
  })
  
  # Output average finishing position
  output$avg_finish_position <- renderPlot({
    ggplot(avg_finish_position, aes(x = Driver_Name, y = Avg_Finish_Position)) +
      geom_point() +
      labs(title = "Average Finishing Position of Each Driver",
           x = "Driver",
           y = "Average Finishing Position") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
