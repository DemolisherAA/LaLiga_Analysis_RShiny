# Installing required packages if not already installed
necessary_packages <- c("shiny", "shinydashboard", "dplyr", "ggplot2", "lubridate", "readr", "tidyr", "stringr", "plotly", "sf", "leaflet", "ggrepel", "shinyjs")

for (package in necessary_packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package)
  }
}

# Loading required libraries
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(tidyr)
library(stringr)
library(plotly)
library(sf)
library(leaflet)
library(ggrepel)
library(shinyjs)

custom_css <- "
  .main-header .logo {
    height: 75px; 
    position: fixed;
  }
  .main-header .sidebar-toggle {
    height: 75px;
    line-height: 75px;
    position: fixed;
    left: 0;
    padding: 0 30px;
  }
  .sidebar-menu {
    top: 35%;
    position: fixed;
    background-color: black;
  }
  .custom-legend {
    display: flex;
    justify-content: center;
    margin-top: 10px;
  }
  .custom-legend div {
    margin-right: 15px;
    display: flex;
    align-items: center;
  }
  .custom-legend span {
    width: 20px;
    height: 20px;
    display: inline-block;
    margin-right: 5px;
  }
  .legend-wins {
    background-color: lightgreen;
  }
  .legend-losses {
    background-color: tomato;
  }
  .legend-draws {
    background-color: orange;
    border-radius: 50%;
  }
  .legend-winlossdiff {
    border-top: 3px solid blue;
    transform: translateY(50%);
  }
  .legend-goalscored {
    background-color: lightgreen;
  }
  .legend-goalsconceded {
    background-color: tomato;
  }
  .legend-goaldiff {
    border-top: 3px solid blue;
    transform: translateY(50%);
  }
"

# Loading the data
data_laliga <- read_csv("combined_data_laliga.csv", show_col_types = FALSE)

# Creating the Home_Win, Away_Win, and Draw columns
data_laliga <- data_laliga %>%
  mutate(
    Home_Win = if_else(`Home Team Goals Scored` > `Away Team Goals Scored`, 1, 0),
    Away_Win = if_else(`Away Team Goals Scored` > `Home Team Goals Scored`, 1, 0),
    Draw = if_else(`Home Team Goals Scored` == `Away Team Goals Scored`, 1, 0)
  )

# Calculating win/loss ratio including draws for home and away teams
home_stats <- data_laliga %>%
  group_by(`Home Team`) %>%
  summarise(
    Home_Wins = sum(Home_Win, na.rm = TRUE),
    Home_Losses = sum(Away_Win, na.rm = TRUE),
    Home_Draws = sum(Draw, na.rm = TRUE),
    Home_Goals_Scored = sum(`Home Team Goals Scored`, na.rm = TRUE),
    Home_Goals_Conceded = sum(`Away Team Goals Scored`, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  rename(Team = `Home Team`)

away_stats <- data_laliga %>%
  group_by(`Away Team`) %>%
  summarise(
    Away_Wins = sum(Away_Win, na.rm = TRUE),
    Away_Losses = sum(Home_Win, na.rm = TRUE),
    Away_Draws = sum(Draw, na.rm = TRUE),
    Away_Goals_Scored = sum(`Away Team Goals Scored`, na.rm = TRUE),
    Away_Goals_Conceded = sum(`Home Team Goals Scored`, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  rename(Team = `Away Team`)

# Combining home and away stats
total_stats <- full_join(home_stats, away_stats, by = "Team") %>%
  mutate(
    Total_Wins = coalesce(Home_Wins, 0) + coalesce(Away_Wins, 0),
    Total_Losses = coalesce(Home_Losses, 0) + coalesce(Away_Losses, 0),
    Total_Draws = coalesce(Home_Draws, 0) + coalesce(Away_Draws, 0),
    Total_Goals_Scored = coalesce(Home_Goals_Scored, 0) + coalesce(Away_Goals_Scored, 0),
    Total_Goals_Conceded = coalesce(Home_Goals_Conceded, 0) + coalesce(Away_Goals_Conceded, 0),
    Win_Loss_Difference = Total_Wins - Total_Losses,
    Goal_Difference = Total_Goals_Scored - Total_Goals_Conceded
  ) %>%
  select(Team, Total_Wins, Total_Losses, Total_Draws, Total_Goals_Scored, Total_Goals_Conceded, Win_Loss_Difference, Goal_Difference)

# Identifying top 3 teams based on win/loss difference or goal difference
get_top_3_teams <- function(metric) {
  top_3_teams <- total_stats %>%
    arrange(desc(!!sym(metric))) %>%
    slice_head(n = 3) %>%
    mutate(Rank = row_number(), 
           Medal_Color = case_when(
             Rank == 1 ~ "#FFD700",  # Gold Medal
             Rank == 2 ~ "#C0C0C0",  # Silver Medal
             Rank == 3 ~ "#CD7F32"   # Bronze Medal
           ),
           Label = paste0(Team, " (", Rank, ")"))
  return(top_3_teams)
}

# Team logos (URLs)
team_logos <- list(
  `BARCELONA` = "https://upload.wikimedia.org/wikipedia/en/thumb/4/47/FC_Barcelona_%28crest%29.svg/1200px-FC_Barcelona_%28crest%29.svg.png",
  `ATLETICO MADRID` = "https://en.atleticodemadrid.com/system/file3s/84815/large/XtrpSffKkn_ESCUDO_ATM_24_WEB2.jpg?1688128199",
  `REAL MADRID` = "https://upload.wikimedia.org/wikipedia/en/thumb/5/56/Real_Madrid_CF.svg/1200px-Real_Madrid_CF.svg.png"
)



# Add team descriptions to total_stats
team_descriptions <- data.frame(
  Team = c("BARCELONA", "REAL MADRID", "ATLETICO MADRID"),
  Description = c("Barcelona: The top performer in La Liga with the most Wins and most Goals Scored.",
                  "Real Madrid: Consistent Performer throughout with an aggressive attacking strategy.",
                  "Atlético Madrid: Consistent Performer with Strong defensive tactics.")
)

total_stats <- left_join(total_stats, team_descriptions, by = "Team")


# Loading the shapefile
shapefile_path <- "es_10km.shp"
shapefile <- st_read(shapefile_path)

# Reprojecting the shapefile to WGS84 (long-lat)
shapefile <- st_transform(shapefile, crs = 4326)

# Loading the stadiums dataset
stadiums_path <- "LaLiga_Stadiums.csv"
stadiums <- read.csv(stadiums_path)

# Convert the stadiums DataFrame to an sf object
stadiums_sf <- st_as_sf(stadiums, coords = c("Longitude", "Latitude"), crs = 4326)

# Merging stadiums data with total_stats
stadiums_sf <- stadiums_sf %>%
  left_join(total_stats, by = c("Team" = "Team")) %>%
  select(Team, Stadium, everything()) # Ensure Stadium column is included

# Clustering teams based on performance metrics On Percentiles
stadiums_sf <- stadiums_sf %>%
  mutate(
    Goals_Scored_Cluster = case_when(
      Total_Goals_Scored < quantile(Total_Goals_Scored, 0.50) ~ "Below Average",
      Total_Goals_Scored < quantile(Total_Goals_Scored, 0.80) ~ "Average",
      TRUE ~ "Above Average"
    ),
    Wins_Cluster = case_when(
      Total_Wins < quantile(Total_Wins, 0.50) ~ "Below Average",
      Total_Wins < quantile(Total_Wins, 0.80) ~ "Average",
      TRUE ~ "Above Average"
    ),
    Win_Loss_Cluster = case_when(
      Win_Loss_Difference < quantile(Win_Loss_Difference, 0.50) ~ "Below Average",
      Win_Loss_Difference < quantile(Win_Loss_Difference, 0.80) ~ "Average",
      TRUE ~ "Above Average"
    ),
    Goal_Difference_Cluster = case_when(
      Goal_Difference < quantile(Goal_Difference, 0.50) ~ "Below Average",
      Goal_Difference < quantile(Goal_Difference, 0.80) ~ "Average",
      TRUE ~ "Above Average"
    )
  )

# Calculating win/loss ratio including draws for home and away teams by year
home_stats_by_year <- data_laliga %>%
  group_by(`Home Team`, year) %>%
  summarise(
    Home_Wins = sum(Home_Win, na.rm = TRUE),
    Home_Losses = sum(Away_Win, na.rm = TRUE),
    Home_Draws = sum(Draw, na.rm = TRUE),
    Home_Goals_Scored = sum(`Home Team Goals Scored`, na.rm = TRUE),
    Home_Goals_Conceded = sum(`Away Team Goals Scored`, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  rename(Team = `Home Team`)

away_stats_by_year <- data_laliga %>%
  group_by(`Away Team`, year) %>%
  summarise(
    Away_Wins = sum(Away_Win, na.rm = TRUE),
    Away_Losses = sum(Home_Win, na.rm = TRUE),
    Away_Draws = sum(Draw, na.rm = TRUE),
    Away_Goals_Scored = sum(`Away Team Goals Scored`, na.rm = TRUE),
    Away_Goals_Conceded = sum(`Home Team Goals Scored`, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  rename(Team = `Away Team`)

# Combining home and away stats by year
total_stats_by_year <- full_join(home_stats_by_year, away_stats_by_year, by = c("Team", "year")) %>%
  mutate(
    Total_Wins = coalesce(Home_Wins, 0) + coalesce(Away_Wins, 0),
    Total_Losses = coalesce(Home_Losses, 0) + coalesce(Away_Losses, 0),
    Total_Draws = coalesce(Home_Draws, 0) + coalesce(Away_Draws, 0),
    Total_Goals_Scored = coalesce(Home_Goals_Scored, 0) + coalesce(Away_Goals_Scored, 0),
    Total_Goals_Conceded = coalesce(Home_Goals_Conceded, 0) + coalesce(Away_Goals_Conceded, 0),
    Win_Loss_Difference = Total_Wins - Total_Losses,
    Goal_Difference = Total_Goals_Scored - Total_Goals_Conceded
  ) %>%
  select(Team, year, Total_Wins, Total_Losses, Total_Draws, Total_Goals_Scored, Total_Goals_Conceded, Win_Loss_Difference, Goal_Difference)

# Aggregating data for all years
total_stats_all_years <- total_stats_by_year %>%
  group_by(Team) %>%
  summarise(
    Total_Wins = sum(Total_Wins, na.rm = TRUE),
    Total_Losses = sum(Total_Losses, na.rm = TRUE),
    Total_Draws = sum(Total_Draws, na.rm = TRUE),
    Total_Goals_Scored = sum(Total_Goals_Scored, na.rm = TRUE),
    Total_Goals_Conceded = sum(Total_Goals_Conceded, na.rm = TRUE),
    Win_Loss_Difference = sum(Win_Loss_Difference, na.rm = TRUE),
    Goal_Difference = sum(Goal_Difference, na.rm = TRUE)
  ) %>%
  mutate(year = "All Years")

# Converting year to character in total_stats_by_year
total_stats_by_year <- total_stats_by_year %>%
  mutate(year = as.character(year))

# Combining the datasets
total_stats_by_year <- bind_rows(total_stats_by_year, total_stats_all_years)

# Loading the data
data <- read.csv("primera-division.csv_2010_2020.csv")
data$fee_cleaned <- as.numeric(gsub("[€m]", "", data$fee))

# Filtering out rows with NA values in fee_cleaned to ensure clean data for analysis
clean_data <- data %>%
  filter(!is.na(fee_cleaned))

# Summarizing total money spent on incoming players and gained from outgoing players
summary_data <- clean_data %>%
  group_by(year, club_name, transfer_movement) %>%
  summarise(total = sum(fee_cleaned, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = transfer_movement, values_from = total, values_fill = list(total = 0)) %>%
  mutate(Net_Revenue = `out` - `in`)  # Calculate net revenue


# Calculating total spending on incoming players by club
total_spending <- clean_data %>%
  filter(transfer_movement == "in") %>%
  group_by(club_name) %>%
  summarise(Total_Spent = sum(fee_cleaned, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(Total_Spent)) %>%
  slice_max(Total_Spent, n = 3)  # Get top 3 clubs based on spending

# Extracting names of top spending clubs
top_clubs <- total_spending$club_name

# Getting details of top 10 transfers for each of these clubs
top_transfers <- clean_data %>%
  filter(club_name %in% top_clubs) %>%
  arrange(desc(fee_cleaned)) %>%
  group_by(club_name) %>%
  slice_head(n = 10) %>%
  ungroup()

# Loading data outside server to use in UI
players_stats <- read_csv("filtered_players_stats.csv")
players_mapping <- read_csv("player_id_mapping.csv")
season_mapping <- read_csv("season_mapping.csv")

# Renaming "Primera Division" to "LaLiga" in season_mapping
season_mapping <- season_mapping %>% mutate(season_name = gsub("Primera Division", "LaLiga", season_name))

# Filtering out season ID 32501
players_stats <- players_stats %>% filter(season_id != 32501)

# Merging datasets to include player names and season names
full_data <- players_stats %>%
  left_join(players_mapping, by = "player_id") %>%
  left_join(season_mapping, by = "season_id")

# Aggregating goals, assists, and successfulDribbles for each player by season
player_season_stats <- full_data %>%
  group_by(player_id, player_name, season_id, season_name) %>%
  summarise(
    TotalGoals = sum(goals, na.rm = TRUE),
    TotalAssists = sum(assists, na.rm = TRUE),
    TotalSuccessfulDribbles = sum(successfulDribbles, na.rm = TRUE),
    .groups = 'drop'
  )

# Aggregating goals, assists, and successfulDribbles for each player across all seasons
player_total_stats <- full_data %>%
  group_by(player_id, player_name) %>%
  summarise(
    TotalGoals = sum(goals, na.rm = TRUE),
    TotalAssists = sum(assists, na.rm = TRUE),
    TotalSuccessfulDribbles = sum(successfulDribbles, na.rm = TRUE),
    GoalsAssistsSum = sum(goals, na.rm = TRUE) + sum(assists, na.rm = TRUE),
    GoalsDribblesSum = sum(goals, na.rm = TRUE) + sum(successfulDribbles, na.rm = TRUE),
    AssistsDribblesSum = sum(assists, na.rm = TRUE) + sum(successfulDribbles, na.rm = TRUE),
    .groups = 'drop'
  )

# Aggregating passes stats for each player across all seasons
player_passes_stats <- full_data %>%
  group_by(player_id, player_name) %>%
  summarise(
    TotalPasses = sum(totalPasses, na.rm = TRUE),
    KeyPasses = sum(keyPasses, na.rm = TRUE),
    AccuratePasses = sum(accuratePasses, na.rm = TRUE),
    .groups = 'drop'
  )

# Sorting by Total Passes and select the top 20 players for the passes plot
top_20_players <- player_passes_stats %>%
  arrange(desc(TotalPasses)) %>%
  slice(1:20)

# Loading goalkeeper data and preprocess
goalkeeper_stats <- players_stats %>%
  filter(!season_id %in% c(8578, 6559, 32501)) %>%
  left_join(players_mapping, by = "player_id") %>%
  left_join(season_mapping, by = "season_id")

# Aggregating goalkeeper stats across all seasons
goalkeeper_stats_all_seasons <- goalkeeper_stats %>%
  group_by(player_id, player_name) %>%
  summarise(
    TotalSavedShots = sum(savedShotsFromInsideTheBox, na.rm = TRUE) + sum(savedShotsFromOutsideTheBox, na.rm = TRUE),
    InsideBoxSaves = sum(savedShotsFromInsideTheBox, na.rm = TRUE),
    OutsideBoxSaves = sum(savedShotsFromOutsideTheBox, na.rm = TRUE),
    SaveRatio = InsideBoxSaves / TotalSavedShots,
    .groups = 'drop'
  )

# Aggregating goalkeeper stats by season
goalkeeper_stats_by_season <- goalkeeper_stats %>%
  group_by(player_id, player_name, season_name) %>%
  summarise(
    TotalSavedShots = sum(savedShotsFromInsideTheBox, na.rm = TRUE) + sum(savedShotsFromOutsideTheBox, na.rm = TRUE),
    InsideBoxSaves = sum(savedShotsFromInsideTheBox, na.rm = TRUE),
    OutsideBoxSaves = sum(savedShotsFromOutsideTheBox, na.rm = TRUE),
    SaveRatio = InsideBoxSaves / TotalSavedShots,
    .groups = 'drop'
  )

# Getting unique season names
unique_seasons <- unique(player_season_stats$season_name)
unique_seasons <- c("All", unique_seasons)

# UI for the dashboard
ui <- dashboardPage(
  dashboardHeader(
    title = tags$div(
      tags$img(src = "https://1000logos.net/wp-content/uploads/2019/01/Spanish-La-Liga-Logo-2016.png", 
               height = "75px", style = "vertical-align: top;"), 
      tags$span(
        style = "font-size: 25px; text-align: center; vertical-align: bottom; font-weight: bold;",
        "It’s Not Football, It’s La Liga: An Analysis of Team Tactics, Player Progression and Transfer Strategies"
      )
    ),
    titleWidth = "calc(100%)" # Adjust the title width to avoid overlap with the toggle
  ),
  dashboardSidebar(
    tags$style(HTML(custom_css)),
    sidebarMenu(
      menuItem("Introduction", tabName = "introduction", icon = icon("info")),
      menuItem("Teams Performance Dashboard", tabName = "teams_performance", icon = icon("bar-chart")),
      menuItem("Player Performance Dashboard", tabName = "player_performance", icon = icon("line-chart")),
      menuItem("Transfers Dashboard", tabName = "transfers", icon = icon("exchange")),
      menuItem("Conclusion", tabName = "conclusion", icon = icon("flag-checkered"))
    )
  ),
  dashboardBody(
    tabItems(
      # Introduction tab
      tabItem(tabName = "introduction",
              h1("Introduction", style = "text-align: center; font-size: 50px"),
              p("Welcome to the La Liga analysis dashboard. This section will provide an overview of the project, objectives, and key insights.", style = "text-align: center; font-size: 18px"),
              div(style = "text-align: center; font-size: 16px;",
                  actionLink("show_dashboard_description", "Click here for the Project Outline")
              ),
              conditionalPanel(
                condition = "input.show_dashboard_description % 2 == 1",
                p("This Project provides a comprehensive analysis of various aspects of La Liga, including team performance, 
                  player metrics, and transfer market dynamics.", style = "text-align: center; font-size: 16px;"),
                p("Click the Tabs in the Sidebar to view All the Different Tabs of this Project.", style = "text-align: center; font-size: 16px;"),
                p("1. Teams Performance Dashboard: Team Performace over the Years and in Seasons. ", style = "text-align: center; font-size: 16px;"),
                p("2. Player Performance Dashboard: In-Depth Analysis of the Players. ", style = "text-align: center; font-size: 16px;"),
                p("3. Transfers Dashboard: Spending of the Clubs and Details of Top 3 Spenders. ", style = "text-align: center; font-size: 16px;")
              ),
              h3("Objective and Motivation of the Project:", style = "margin-top: 30px; text-align: center"),
              p("The Project on La Liga sports analytics, deep diving into various aspects of one of the world's premier football 
                leagues, known for competitive matches and extraordinary player talent. My interest in football since childhood motivates
                me in exploring La Liga using Data Visualisations. The league's rich history of tactical innovation and its impact on 
                global football culture present me a unique opportunity to apply sports analytics. I would like to review the underlying 
                game trends, player performance metrics, and the Transfer Market to get findings that would be useful in the development 
                of coaching strategies, player development programs, and ways through which the fans' experience can be improved using 
                data. A perspective that will allow the analysis of patterns of play, individual contribution of players, and strategies 
                of the team that gives a full scope of the competitive landscape within La Liga.", style = "text-align: center"),
              h4("An Introductory Plot showing the Performance of the Teams in La Liga (A Broad Perspective of the Project).", style = "text-align: center"),
              div(style = "text-align: center; font-size: 16px;",
                  actionLink("show_instructions", "Click here for the Instructions for the Introductory Plot")
              ),
              conditionalPanel(
                condition = "input.show_instructions % 2 == 1",
                p("Hover Over the Graph to see the Details of the Wins/Losses/Draws or the Goals Scored/Conceeded of the Teams in La Liga. Further, to know about
                      the Top 3 Teams in La Liga, Click on the Gold (Best), Silver(Second Best) and the Bronze(Third Best) circles to get to 
                      know more about the elite clubs.", style = "text-align: center; font-size: 16px;")
              ),
              selectInput("metric", "Select Metric:", choices = c("Win/Loss Difference", "Goal Difference")),
              plotlyOutput("interactive_plot", height = "600px"), # Increase the height of the plot
              uiOutput("custom_legend"),
              h4("Graph Description", style = "text-align: center; margin-top: 20px;"),
              uiOutput("graph_description")
      ),
      # Teams Performance Dashboard tab
      tabItem(tabName = "teams_performance",
              h1("Teams Performance Dashboard", style = "text-align: center; font-size: 50px"),
              p("This section presents various metrics and visualizations related to team performance in La Liga (2014-2020).", style = "text-align: center; font-size: 18px"),
              p("Overall, the Teams Performance Dashboard aims to provide an interactive and intuitive way to analyze the performance of La Liga 
                teams, helping users gain insights into team strengths, weaknesses, and overall competitiveness. The visualizations are designed 
                to be user-friendly and informative, making it easy for users to explore the data and draw meaningful conclusions about team performance 
                in La Liga. There are Plots which show a broad overview of top Teams in La Liga and then allows to compare two Teams in detail.",
                style = "text-align: center; font-size: 16px"),
              fluidRow(
                column(6,
                       div(
                         style = "padding: 10px; border: 1px solid #ccc; border-radius: 4px; background-color: #f9f9f9;",
                         selectInput("map_metric", "Select Metric:", choices = c("Win/Loss Difference", "Goal Difference")),
                         
                         actionLink("show_map_instructions", "Click here for the Instructions for the Teams Map Plot"),
                         conditionalPanel(
                           condition = "input.show_map_instructions % 2 == 1",
                           p("Use the dropdown above to select a metric to display on the map. The scatter plot of that can ve viewed
                             by clicking the link of the scatter plot. To know more details about a Team click on it and it shows 
                             more details. By zooming in to the map, the exact location of the stadium can be seen."
                             , style = "text-align: center; font-size: 16px;")
                         ),
                         br(),
                         br(),
                         actionLink("show_scatterplot", "Click here to Show Scatter Plot of Clustering"),
                         
                         
                         leafletOutput("teams_map", height = "662.5px"),
                         h4("Graph Description", style = "text-align: center; margin-top: 20px;"),
                         uiOutput("map_graph_description")
                       )
                ),
                column(6,
                       div(
                         style = "padding: 10px; border: 1px solid #ccc; border-radius: 4px; background-color: #f9f9f9;",
                         
                         selectInput("metric_team_performance", "Select Metric:", choices = c("", "Total Goals Scored" = "Total_Goals_Scored", "Total Goals Conceded" = "Total_Goals_Conceded", "Goal Difference" = "Goal_Difference", "Total Wins" = "Total_Wins", "Total Losses" = "Total_Losses", "Win/Loss Difference" = "Win_Loss_Difference")),
                         selectInput("year", "Select Year:", choices = c("All Years", sort(unique(total_stats_by_year$year)))),
                         
                         fluidRow(
                           column(6, selectInput("team1", "Select Team 1:", choices = c("", unique(total_stats$Team)))),
                           column(6, selectInput("team2", "Select Team 2:", choices = c("", unique(total_stats$Team))))
                         ),
                         actionLink("show_barplot_instructions", "Click here for the Instructions for the Dual Bar Plot"),
                         conditionalPanel(
                           condition = "input.show_barplot_instructions % 2 == 1",
                           p("Use the dropdowns above to select the metric, the year(season) and the teams you want to compare. Hover over the bars 
                             in the plot to see more details. First select the years. Selecting the year resets the graph."
                             , style = "text-align: center; font-size: 16px;")
                         ),
                         plotlyOutput("dual_bar_graph", height = "600px"),
                         h4("Graph Description", style = "text-align: center; margin-top: 20px;"),
                         p("This plot compares the selected metric between two teams. Use the dropdowns above to select the metric, the year(season) and 
                         the teams to be compared. The available metrics include Total Goals Scored, Total Goals Conceded, Goal Difference,
                         Total Wins, Total Losses, and Win/Loss Difference. Hovering over the bars in the plot shows more details. 
                         The tooltips will display the team name and the value of the selected metric. This visualization allows  
                         easy comparison of the performance of two teams across various metrics, providing a clear and concise 
                         comparison between two teams. Also, the The colour Green shows the better team while the colour red shows the worse one.", style = "text-align: center; font-size: 16px;")
                         
                       )
                )
              ),
              br(),
              h4("Dashboard Description", style = "text-align: center; margin-top: 20px;"),
              p("By examining the combined data across all seasons, we gain insights into the long-term performance stability of teams. 
                The consistency of top teams like Barcelona, Atletico Madrid and Real Madrid is evident, as their high wins tally greatly 
                contrasts with their comparatively low losses. We can further see from the Win/Loss Ratio that Teams with more Wins than 
                Losses (That is shown by the Win/Loss Ratio in the positive region) are bound to perform better", style = "text-align: center; font-size: 16px;"),
              p("This provides valuable insights into the performance dynamics of La Liga teams, offering a data-driven 
                approach to understanding their strengths and weaknesses in offensive and defensive aspects of the game. This corresponds
                to the Win - Losses and ultimately, we can come to the conclusion that way the top attacking teams, i.e. , Barcelona and 
                Real Madrid have done well, and the ultra-defensive team Atletico Madrid has done well too with the wins they have amassed 
                in the past few seasons. Teams like Sevilla, Valencia and Villareal have been noticeable as well.", style = "text-align: center; font-size: 16px;")
      ),
      
      # Player Performance Dashboard tab
      tabItem(tabName = "player_performance",
              h1("Player Performance Dashboard", style = "text-align: center; font-size: 50px"),
              p("This section presents various metrics and visualizations related to player performance in La Liga (2013/14-2019/20).", style = "text-align: center; font-size: 18px"),
              p("The Player Performance Dashboard aims to provide a detailed and comprehensive analysis of individual player performance across various metrics such as
                goals, assists, dribbles, and goalkeeping statistics. It allows users to filter data by season and view top players in different categories.", style = "text-align: center; font-size: 16px"),
              fluidPage(
                useShinyjs(),
                
                div(
                  style = "padding: 10px; border: 1px solid #ccc; border-radius: 4px; background-color: #f9f9f9;",
                  fluidRow(
                    column(
                      12,
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("seasonFilter", "Season", choices = unique_seasons, selected = "All"),
                          sliderInput("goalFilter", "Goals", min = 0, max = 1000, value = c(0, 1000)),
                          sliderInput("assistFilter", "Assists", min = 0, max = 1000, value = c(0, 1000)),
                          sliderInput("dribbleFilter", "Dribbles", min = 0, max = 1000, value = c(0, 1000)),
                          br(),
                          actionButton("reset", "Reset To All Seasons"),
                          conditionalPanel(
                            br(),
                            actionLink("show_player_instructions", "Click here for the Instructions for the Players Performance Plot"),
                            conditionalPanel(
                              condition = "input.show_player_instructions % 2 == 1",
                              p("Use the slider to select the range of goals, assists and dribbles to display the Players in and see the Top Players in that range of values. 
                                The dropdown to filter data by season and the Best players in that season could be found. Further, when viewing the Best Players 
                                across All Seasons, A bubble chart of Top 10 players in terms of Goals and Assists, Goals and Dribbles, Assists and Dribbles can be viewed. Also, Top 20
                                players in terms of Passes can be viewed. The reset button resets the graphs to All Seasons. Also, any category under the Player Category can be 
                                double clicked to see that particular category of Players. 
                                Hover over the dots to see more details.", style = "text-align: center; font-size: 16px;")
                            ),
                            br(),
                            br(),
                            br(),
                            condition = "input.seasonFilter === 'All'",
                            actionLink("show_goals_assists", "Click here to Show Top 10 Players in terms of Goals & Assists"),
                            br(),
                            br(),
                            actionLink("show_goals_dribbles", "Click here to Show Top 10 Players in terms of Goals & Dribbles"),
                            br(),
                            br(),
                            actionLink("show_assists_dribbles", "Click here to Show Top 10 Players in terms of Assists & Dribbles"),
                            br(),
                            br(),
                            actionLink("show_passes", "Click here to Show Top 20 Players in terms of Passes")
                          )
                        ),
                        mainPanel(
                          plotlyOutput("performancePlot", height = "767.5px")
                        )
                      )
                    ),
                    h4("Graph Description", style = "text-align: center; margin-top: 20px;"),
                    p("This plot provides an overview of individual player performance based on the range of goals, assists and dribbles.
                      The interactive plot allows users to explore player statistics in detail, making it easy to identify standout performers in various 
                      categories. The selection of season dives deep into the detail. Further when All Seasons are selected, Top 10 Players in terms of 
                      Goals and Assists, Goals and Dribbles, Assists and Dribbles can be viewed as well as the top 20 players in terms of passes. This
                      increases the level of detail being presented. The Dribbles Data was not available for some earlier seasons, so a 2D plot is 
                      presented for those seasons.", style = "text-align: center; font-size: 16px;")
                  )
                ),
                br(),
                br(),
                
                div(
                  style = "padding: 10px; border: 1px solid #ccc; border-radius: 4px; background-color: #f9f9f9;",
                  br(),
                  br(),
                  fluidRow(
                    column(
                      12,
  
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("numGoalkeepers", "Top Goalkeepers:", min = 1, max = 30, value = 30),
                          selectInput("season", "Season:", choices = c("All Seasons", sort(unique(goalkeeper_stats$season_name)))),
                          actionButton("resetGoalkeepers", "Reset to All Seasons"),
                          br(),
                          br(),
                          actionLink("show_goalkeeper_instructions", "Click here for the Instructions for the Goalkeepers Plot"),
                          conditionalPanel(
                            condition = "input.show_goalkeeper_instructions % 2 == 1",
                            p("The slider can be used to select the number of top goalkeepers in terms of total shots to display. The dropdown 
                            can be used to filter data by season. Hover over the bars to see more details. Use the reset button to
                              go back to the default view of All Seasons.", style = "text-align: center; font-size: 16px;")
                          ),
                          h4("Graph Description", style = "text-align: center; margin-top: 20px;"),
                          p("This plot shows the top goalkeepers based on the number of saved shots for the selected seasons. Users 
                            can filter data by season and adjust the number of top goalkeepers displayed in terms of Total Shots Saved. 
                            The colour is co-ordinated according to Proportion of Shots Saved inside the box. Bluish Tint means less proportion of
                            shots saved inside the box in comparison to the Reddish Tint. This shows which Goalkeeper has been more decisive in saving more Lethal Shots.
                            The interactive plot allows users to explore goalkeeper performance in detail, highlighting the most effective shot-stoppers 
                            in La Liga.", style = "text-align: center; font-size: 16px;"),
                        ),
                        mainPanel(
                          plotlyOutput("savedShotsPlot", height = "670px")
                        )
                      )
                    )
                  )
                )
              ),
              br(),
              h4("Dashboard Description", style = "text-align: center; margin-top: 20px;"),
              p("1. Player Performance Overview: This section provides an overview of individual player performance across various seasons. 
                The Data can be filtered by season and the top players can be viewed in different categories such as goals, assists, and dribbles. 
                The interactive plots allow users to explore player statistics in detail. We can see Lionel Messi by far is the best player
                considering all the metrics Goals, Assists and Dribbles. We can see that he has performed well consistently in all the seasons.", style = "text-align: center; font-size: 16px"),
              p("2. Goalkeeper Performance: This section focuses on the performance of goalkeepers, showcasing statistics such as the 
                number of saved shots as well as proportion of Shots Saved inside the Box. Users can filter data by season and view top 
                goalkeepers based on their performance. This shows Cuellar, Oblak, ter Stegen and Asenjo have been Great GoalKeepers Over the Years.", style = "text-align: center; font-size: 16px")
      ),
      # Transfers Dashboard tab
      tabItem(tabName = "transfers",
              h1("Transfers Dashboard", style = "text-align: center; font-size: 50px"),
              p("This section covers transfer strategies and analysis of player transfers within La Liga (2010-2020).", style = "text-align: center; font-size: 18px"),
              p("Overall, The Transfers Dashboard provides a detailed analysis of the financial transactions involved in player 
                transfers within La Liga. The dashboard includes two main sections: an overview of the financial performance of 
                clubs in the transfer market and a detailed look at the top 10 transfers for the top 3 spending 
                teams.", style = "text-align: center; font-size: 16px"),
              div(
                style = "padding: 10px; border: 1px solid #ccc; border-radius: 4px; background-color: #f9f9f9;",
                fluidRow(
                  column(4,
                         div(
                           style = "padding: 10px; border: 1px solid #ccc; border-radius: 4px; background-color: #f9f9f9;",
                           selectInput("club_filter", "Select Club:", choices = c("All", unique(summary_data$club_name))),
                           selectInput("year_filter", "Select Year:", choices = c("All", unique(summary_data$year))),
                           actionButton("reset", "Reset"),
                           br(),
                           br(),
                           actionLink("show_transfer_instructions", "Click here for the Instructions for the Transfer Plot"),
                           conditionalPanel(
                             condition = "input.show_transfer_instructions % 2 == 1",
                             p("Use the dropdowns above to filter the data by club and year. Hover over the bars to see more details about each club's 
                               financial transactions. Use the reset button to go back to default and see data for All the years and All the Clubs.", style = "text-align: center; font-size: 16px;")
                           ),
                           h4("Graph Description", style = "text-align: center; margin-top: 20px;"),
                           p("This plot provides an overview of the financial transactions of La Liga clubs in the transfer market. The green bars 
                             represent the revenue from outgoing players, while the red bars represent the money spent on incoming players. The net 
                             revenue, shown in dark green, indicates the balance between spending and revenue. The dropdowns are used to filter the data by 
                             club and year, and hover over the bars to see more details.", style = "text-align: center; font-size: 16px;")
                         )
                  ),
                  column(8,
                         plotlyOutput("filtered_plot", height = "510px")
                  )
                )
              ),
              br(),
              br(),
              div(
                style = "padding: 10px; border: 1px solid #ccc; border-radius: 4px; background-color: #f9f9f9;",
                fluidRow(
                  column(4,
                         div(
                           style = "padding: 10px; border: 1px solid #ccc; border-radius: 4px; background-color: #f9f9f9;",
                           selectInput("top_club_filter", "Select Top Club:", choices = c("All", top_clubs)),
                           selectInput("top_movement_filter", "Select Transfer Movement:", choices = c("All", "in", "out")),
                           actionButton("top_reset", "Reset"),
                           br(),
                           br(),
                           actionLink("show_top_transfer_instructions", "Click here for the Instructions for the Top Transfers Plot"),
                           conditionalPanel(
                             condition = "input.show_top_transfer_instructions % 2 == 1",
                             p("Use the dropdowns above to filter the data by club and transfer movement (incoming or outgoing). 
                               Hover over the points to see more details about each transfer, including the player name, transfer fee, and 
                               the year of transfer. Use the reset button to go back to default and see data for All the years and All the 
                               Clubs.", style = "text-align: center; font-size: 16px;")
                           ),
                           h4("Graph Description", style = "text-align: center; margin-top: 20px;"),
                           p("This scatter plot highlights the top 10 transfers for each of the top 3 spending teams in La Liga. The points 
                             are color-coded based on the transfer movement (incoming or outgoing), and their size represents the transfer fee. 
                             Hover over the points to see detailed information about each transfer, including the player name, transfer fee, 
                             age, and year of transfer. The dropdowns are used to filter the data by club and transfer 
                             movement. The size further shows the transfer value.", style = "text-align: center; font-size: 16px;")
                         )
                  ),
                  column(8,
                         plotlyOutput("top_transfers_plot", height = "530px")
                  )
                )
              ),
              br(),
              h4("Dashboard Description", style = "text-align: center; margin-top: 20px;"),
              p("1. Financial Overview of Transfers: This section visualizes the amount of money spent by clubs on incoming players, 
                the revenue generated from outgoing players, and the net revenue for each club. Users can filter the data by club 
                and year to explore how different teams manage their transfer budgets. This shows that teams like Barcelona and Real Madrid
                have spent a lot and have an aggresive spending style. Atletico Madrid too have spent a lot but gained too from the market 
                showing a more subtle approach.", style = "text-align: center; font-size: 16px"),
              p("2. Top 10 Transfers: This section highlights the top 10 transfers for each of the top 3 spending teams as seen from the Overview. The scatter 
                plot provides insights into the transfer fees, player ages, and the year of transfer for these significant 
                transactions. Users can filter the data by club and transfer movement (incoming or outgoing) to focus on specific 
                aspects of the transfer market. This shows that Neymar has been the most expensive Transfer out of Barcelona. He was 
                bought in for a hefty sum too. Griezmann's movement from Atletico to Barcelona is a higlight as well.", style = "text-align: center; font-size: 16px")
      ),
      # Conclusion tab content
      tabItem(
        tabName = "conclusion",
        h1("Conclusion", style = "text-align: center; font-size: 50px"),
        p("This section summarizes the findings and insights from the analysis, providing conclusions and recommendations.", style = "text-align: center; font-size: 18px"),
        h3("Comprehensive Analysis of La Liga:", style = "margin-top: 20px; text-align: center"),
        p("From a comprehensive analysis of La Liga over recent seasons, several insights have been garnered about team performances, 
          player contributions, and financial strategies."),
        p("1. Team Performances: The top teams in La Liga, notably Barcelona, Real Madrid, and Atlético Madrid, have demonstrated
          remarkable consistency, securing the most points across seasons. The fluctuation in points indicates the competitive 
          nature of the league and the small margins that often decide the title."),
        p("2. Player Contributions: Lionel Messi has emerged as a standout performer, not just in scoring, but also in creating 
          opportunities through successful dribbles and key passes. Other players like Cristiano Ronaldo, Luis Suárez, Griezmann and 
          Neymar have also made significant contributions to their teams, with their goal-scoring abilities changing the outcomes of 
          seasons. Oblak has made significant good inside the box saves as well helping to his teams success."),
        p("3. Financial Strategies in Transfers: The transfer market analysis revealed a spectrum of financial approaches, from 
          Barcelona and Real Madrid's aggressive spending to Atlético de Madrid's more balanced financial management. The expenditure 
          on player transfers is a high-stakes gamble that sometimes pays off in terms of on-field success."),
        h4("After the Analysis, The Top Teams and The Best Player in La Liga (A Broad Conclusion)", style = "text-align: center"),
        p("Hover Over The Images for more information about the Top Teams and the Best Player. The Metrics for all the Top Teams 
          are shown which includes Goals and Wins. Also, a Brief description about the Top Teams and the Best Player is also 
          presented on Hover.", style = "text-align: center; font-size: 16px"),
        div(
          style = "display: flex; justify-content: space-between;",
          div(
            style = "flex: 1; text-align: center; margin-top: 75px;",
            h4("Top 3 Teams in La Liga"),
            div(
              style = "display: flex; justify-content: space-around; align-items: center;",
              div(
                style = "text-align: center;",
                img(src = "https://upload.wikimedia.org/wikipedia/en/thumb/4/47/FC_Barcelona_%28crest%29.svg/1200px-FC_Barcelona_%28crest%29.svg.png", height = "200px", id = "barcelona-logo", title = paste0(total_stats$Description[total_stats$Team == "BARCELONA"], "<br>Total Wins: ", total_stats$Total_Wins[total_stats$Team == "BARCELONA"], "<br>Total Losses: ", total_stats$Total_Losses[total_stats$Team == "BARCELONA"], "<br>Total Draws: ", total_stats$Total_Draws[total_stats$Team == "BARCELONA"], "<br>Total Goals Scored: ", total_stats$Total_Goals_Scored[total_stats$Team == "BARCELONA"], "<br>Total Goals Conceded: ", total_stats$Total_Goals_Conceded[total_stats$Team == "BARCELONA"], "<br>Goal Difference: ", total_stats$Goal_Difference[total_stats$Team == "BARCELONA"], "<br>Win/Loss Difference: ", total_stats$Win_Loss_Difference[total_stats$Team == "BARCELONA"])),
                h5("Barcelona")
              ),
              div(
                style = "text-align: center;",
                img(src = "https://upload.wikimedia.org/wikipedia/en/thumb/5/56/Real_Madrid_CF.svg/1200px-Real_Madrid_CF.svg.png", height = "200px", id = "realmadrid-logo", title = paste0(total_stats$Description[total_stats$Team == "REAL MADRID"], "<br>Total Wins: ", total_stats$Total_Wins[total_stats$Team == "REAL MADRID"], "<br>Total Losses: ", total_stats$Total_Losses[total_stats$Team == "REAL MADRID"], "<br>Total Draws: ", total_stats$Total_Draws[total_stats$Team == "REAL MADRID"], "<br>Total Goals Scored: ", total_stats$Total_Goals_Scored[total_stats$Team == "REAL MADRID"], "<br>Total Goals Conceded: ", total_stats$Total_Goals_Conceded[total_stats$Team == "REAL MADRID"], "<br>Goal Difference: ", total_stats$Goal_Difference[total_stats$Team == "REAL MADRID"], "<br>Win/Loss Difference: ", total_stats$Win_Loss_Difference[total_stats$Team == "REAL MADRID"])),
                h5("Real Madrid")
              ),
              div(
                style = "text-align: center;",
                img(src = "https://upload.wikimedia.org/wikipedia/en/thumb/f/f4/Atletico_Madrid_2017_logo.svg/1200px-Atletico_Madrid_2017_logo.svg.png", height = "200px", id = "atletico-logo", title = paste0(total_stats$Description[total_stats$Team == "ATLETICO MADRID"], "<br>Total Wins: ", total_stats$Total_Wins[total_stats$Team == "ATLETICO MADRID"], "<br>Total Losses: ", total_stats$Total_Losses[total_stats$Team == "ATLETICO MADRID"], "<br>Total Draws: ", total_stats$Total_Draws[total_stats$Team == "ATLETICO MADRID"], "<br>Total Goals Scored: ", total_stats$Total_Goals_Scored[total_stats$Team == "ATLETICO MADRID"], "<br>Total Goals Conceded: ", total_stats$Total_Goals_Conceded[total_stats$Team == "ATLETICO MADRID"], "<br>Goal Difference: ", total_stats$Goal_Difference[total_stats$Team == "ATLETICO MADRID"], "<br>Win/Loss Difference: ", total_stats$Win_Loss_Difference[total_stats$Team == "ATLETICO MADRID"])),
                h5("Atlético Madrid")
              )
            )
          ),
          div(
            style = "flex: 1; text-align: center;",
            h4("Best Player of La Liga"),
            img(src = "https://scontent.fmel14-1.fna.fbcdn.net/v/t39.30808-6/395632835_890928682397909_5456183549728424875_n.jpg?_nc_cat=109&ccb=1-7&_nc_sid=5f2048&_nc_ohc=JnLYHWjgElIQ7kNvgGBIz9x&_nc_ht=scontent.fmel14-1.fna&oh=00_AYArJ4dVxGF4NXhIGWNlMhDjHXKsOAGBdomc3gSeSGtPFw&oe=665AC4CA", height = "400px", id = "messi-logo", title = "Lionel Messi: Recognized as the best player for his exceptional performance, not only in scoring but also in creating opportunities."),
            h5("Lionel Messi")
          )
        ),
        tags$script(HTML("
        $(document).on('mouseover', '#barcelona-logo', function() {
          $(this).tooltip({html: true}).tooltip('show');
        });
        $(document).on('mouseover', '#realmadrid-logo', function() {
          $(this).tooltip({html: true}).tooltip('show');
        });
        $(document).on('mouseover', '#atletico-logo', function() {
          $(this).tooltip({html: true}).tooltip('show');
        });
        $(document).on('mouseover', '#messi-logo', function() {
          $(this).tooltip({html: true}).tooltip('show');
        });
      "))
      )
      
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Introductury Team Plot
  output$interactive_plot <- renderPlotly({
    metric <- ifelse(input$metric == "Win/Loss Difference", "Win_Loss_Difference", "Goal_Difference")
    top_3_teams <- get_top_3_teams(metric)
    
    if (metric == "Win_Loss_Difference") {
      plot <- ggplot(total_stats, aes(x = Team)) +
        geom_bar(aes(y = Total_Wins, fill = "Wins"), stat = "identity", position = "dodge") +
        geom_bar(aes(y = -Total_Losses, fill = "Losses"), stat = "identity", position = "dodge") +
        geom_point(aes(y = Total_Draws, color = "Draws"), size = 4) +
        geom_line(aes(y = Win_Loss_Difference, group = 1, color = "Win/Loss Difference"), size = 1) +
        scale_fill_manual(values = c("Wins" = "lightgreen", "Losses" = "tomato")) +
        scale_color_manual(values = c("Draws" = "orange", "Win/Loss Difference" = "blue")) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
        geom_point(data = top_3_teams, aes(y = Win_Loss_Difference, key = Team), shape = 21, size = 8, color = "black", fill = top_3_teams$Medal_Color, stroke = 1) +
        labs(title = "Team Performance Metrics (Win/Loss Difference)",
             x = "Team Names", y = "Metrics (Win/Loss Difference)") +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 90, hjust = 1),
          legend.position = "none"
        )
    } else {
      plot <- ggplot(total_stats, aes(x = Team)) +
        geom_bar(aes(y = Total_Goals_Scored, fill = "Goals Scored"), stat = "identity", position = "dodge") +
        geom_bar(aes(y = -Total_Goals_Conceded, fill = "Goals Conceded"), stat = "identity", position = "dodge") +
        geom_line(aes(y = Goal_Difference, group = 1, color = "Goal Difference"), size = 1) +
        scale_fill_manual(values = c("Goals Scored" = "lightgreen", "Goals Conceded" = "tomato")) +
        scale_color_manual(values = c("Goal Difference" = "blue")) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
        geom_point(data = top_3_teams, aes(y = Goal_Difference, key = Team), shape = 21, size = 8, color = "black", fill = top_3_teams$Medal_Color, stroke = 1) +
        labs(title = "Team Performance Metrics (Goal Difference)",
             x = "Team Names", y = "Metrics (Goal Difference)") +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 90, hjust = 1),
          legend.position = "none"
        )
    }
    
    ggplotly(plot, source = "A", tooltip = c("x", "y")) %>% config(displayModeBar = FALSE)%>% 
      layout(hovermode = "closest")
  })
  
  output$custom_legend <- renderUI({
    if (input$metric == "Win/Loss Difference") {
      div(
        class = "custom-legend",
        div(
          span(class = "legend-wins"),
          "Wins"
        ),
        div(
          span(class = "legend-losses"),
          "Losses"
        ),
        div(
          span(class = "legend-draws"),
          "Draws"
        ),
        div(
          span(class = "legend-winlossdiff"),
          "Win/Loss Difference"
        )
      )
    } else {
      div(
        class = "custom-legend",
        div(
          span(class = "legend-goalscored"),
          "Goals Scored"
        ),
        div(
          span(class = "legend-goalsconceded"),
          "Goals Conceded"
        ),
        div(
          span(class = "legend-goaldiff"),
          "Goal Difference"
        )
      )
    }
  })
  
  output$graph_description <- renderUI({
    if (input$metric == "Win/Loss Difference") {
      p("This graph presents an overview of team performance metrics in La Liga, focusing on wins, losses, draws, and the 
        win/loss difference for each team. The green bars represent the number of wins, while the red bars represent the 
        number of losses for each team. The orange dots indicate the number of draws, and the blue line represents 
        the win/loss difference, which is calculated as the difference between wins and losses. The black outlined 
        points highlight the top 3 teams with the highest win/loss difference, awarded with gold, silver, and bronze medals 
        respectively.", style = "text-align: center; font-size: 16px;")
    } else {
      p("This graph presents an overview of team performance metrics in La Liga, focusing on goals scored, goals conceded, and the 
        goal difference for each team. The green bars represent the number of goals scored, while the red bars represent the 
        number of goals conceded for each team. The blue line represents the goal difference, which is calculated as the difference 
        between goals scored and goals conceded. The black outlined points highlight the top 3 teams with the highest goal difference, 
        awarded with gold, silver, and bronze medals respectively.", style = "text-align: center; font-size: 16px;")
    }
  })
  
  # Leaflet map
  output$teams_map <- renderLeaflet({
    metric <- input$map_metric
    if (metric == "Win/Loss Difference") {
      color_by <- "Win_Loss_Cluster"
      popup_info <- ~paste("<strong>", Team, "</strong><br>",
                           "Stadium: ", Stadium, "<br>",
                           "Wins: ", Total_Wins, "<br>",
                           "Losses: ", Total_Losses, "<br>",
                           "Win/Loss Difference: ", Win_Loss_Difference)
    } else {
      color_by <- "Goal_Difference_Cluster"
      popup_info <- ~paste("<strong>", Team, "</strong><br>",
                           "Stadium: ", Stadium, "<br>",
                           "Goals Scored: ", Total_Goals_Scored, "<br>",
                           "Goals Conceded: ", Total_Goals_Conceded, "<br>",
                           "Goal Difference: ", Goal_Difference)
    }
    
    color_palette <- colorFactor(
      palette = c("Below Average" = "red", "Average" = "yellow", "Above Average" = "green"),
      levels = c("Below Average", "Average", "Above Average")
    )
    
    
    leaflet(stadiums_sf) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        color = ~color_palette(get(color_by)),
        radius = ~ifelse(get(color_by) == "Below Average", 8, ifelse(get(color_by) == "Average", 12, 16)),
        popup = popup_info
      ) %>%
      setView(lng = -3.7038, lat = 40.4168, zoom = 6)
  })
  
  # Showing scatter plot modal on action link click
  observeEvent(input$show_scatterplot, {
    showModal(modalDialog(
      title = "Scatter Plot of Clustering",
      plotlyOutput("scatterplot"),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  # Scatter plot
  output$scatterplot <- renderPlotly({
    metric <- input$map_metric
    
    if (metric == "Win/Loss Difference") {
      plot <- ggplot(stadiums_sf, aes(x = Total_Wins, y = Total_Losses, color = Win_Loss_Cluster, text = Team)) +
        geom_point(size = 3) +
        scale_color_manual(values = c("Below Average" = "red", "Average" = "yellow", "Above Average" = "green")) +
        labs(title = "Scatter Plot of Wins vs Losses",
             x = "Total Wins",
             y = "Total Losses") +
        theme_minimal()
    } else {
      plot <- ggplot(stadiums_sf, aes(x = Total_Goals_Scored, y = Total_Goals_Conceded, color = Goal_Difference_Cluster, text = Team)) +
        geom_point(size = 3) +
        scale_color_manual(values = c("Below Average" = "red", "Average" = "yellow", "Above Average" = "green")) +
        labs(title = "Scatter Plot of Goals Scored vs Goals Conceded",
             x = "Total Goals Scored",
             y = "Total Goals Conceded") +
        theme_minimal()
    }
    
    ggplotly(plot, tooltip = "text") %>% config(displayModeBar = FALSE)
  })
  
  output$map_graph_description <- renderUI({
    metric <- input$map_metric
    if (metric == "Win/Loss Difference") {
      p("This map visualizes the performance of the teams based on focusing on wins, losses, and the win/loss difference for each team. 
         The size and color of the markers represent the performance of the teams. Teams are clustered based on their performance into
         three categories: Below Average (red), Average (yellow), and Above Average (green). These clusters are determined using the 50th and 80th percentiles of the win/loss difference.
         The scatter plot on clicking on the 'Show Scatter Plot of Clustering' link provides a visual representation of 
         how teams are grouped based on their win/loss metrics. Zooming in on the map to see the exact location of 
         each stadium and on clicking on a marker will display detailed information about the team, including the stadium name, wins, losses, and the win/loss difference.", style = "text-align: center; font-size: 16px;")
    } else {
      p("This map visualizes the performance of the teams based on focusing on goals scored, goals conceded, and the goal difference for each team.
         The size and color of the markers represent the performance of the teams. Teams are clustered based on their performance into 
         three categories: Below Average (red), Average (yellow), and Above Average (green). These clusters are determined using the 50th and 80th percentiles of the goal difference.
         The scatter plot on clicking on the 'Show Scatter Plot of Clustering' link provides a visual representation of 
         how teams are grouped based on their goal metrics. Zooming in on the map to see the exact location of 
         each stadium and on licking on a marker will display detailed information about the team, including the stadium name, goals scored, goals conceded, and the 
         goal difference.", style = "text-align: center; font-size: 16px;")
    }
  })
  
  # Dual Bar Plot
  observe({
    available_teams <- if (input$year == "All Years") {
      unique(total_stats_by_year$Team)
    } else {
      unique(total_stats_by_year %>% filter(year == input$year) %>% pull(Team))
    }
    
    updateSelectInput(session, "team1", choices = c("", available_teams))
    updateSelectInput(session, "team2", choices = c("", available_teams))
  })
  
  
  output$dual_bar_graph <- renderPlotly({
    req(input$team1, input$team2, input$metric_team_performance, input$year)
    
    # Filter data based on the selected year
    if (input$year == "All Years") {
      team1_data <- total_stats_by_year %>% filter(Team == input$team1, year == "All Years")
      team2_data <- total_stats_by_year %>% filter(Team == input$team2, year == "All Years")
    } else {
      team1_data <- total_stats_by_year %>% filter(Team == input$team1, year == input$year)
      team2_data <- total_stats_by_year %>% filter(Team == input$team2, year == input$year)
    }
    
    team1_metric <- team1_data[[input$metric_team_performance]]
    team2_metric <- team2_data[[input$metric_team_performance]]
    
    colors <- c()
    if (input$metric_team_performance %in% c("Total_Losses", "Total_Goals_Conceded")) {
      if (team1_metric > team2_metric) {
        colors <- c("tomato", "lightgreen")
      } else {
        colors <- c("lightgreen", "tomato")
      }
    } else {
      if (team1_metric > team2_metric) {
        colors <- c("lightgreen", "tomato")
      } else {
        colors <- c("tomato", "lightgreen")
      }
    }
    names(colors) <- c(input$team1, input$team2)
    
    # Creating a data frame for plotting
    plot_data <- data.frame(
      Metric = c(team1_metric, team2_metric),
      Team = factor(rep(c(input$team1, input$team2), each = 1), levels = c(input$team1, input$team2)),
      Metric_Name = rep(gsub("_", " ", input$metric_team_performance), 2),
      Year = input$year
    )
    
    # Creating the bar plot
    plot <- ggplot(plot_data, aes(x = Team, y = Metric, fill = Team, text = paste("Team:", Team, "<br>", "Year:", Year, "<br>", Metric_Name, ":", Metric))) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
      labs(title = paste("Comparison of", gsub("_", " ", input$metric_team_performance), "\nBetween", input$team1, "and", input$team2, "in", input$year),
           x = "Team",
           y = gsub("_", " ", input$metric_team_performance)) +
      scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
      scale_fill_manual(values = colors) +
      theme_minimal() +
      theme(legend.position = "none")
    
    # Convert ggplot to plotly with correct tooltip assignment
    plotly_plot <- ggplotly(plot, tooltip = "text")
    
    plotly_plot %>% config(displayModeBar = FALSE)
  })
  
  # Function to update filtered data
  updateFilteredData <- function() {
    data <- summary_data
    if (input$club_filter != "All") {
      data <- data %>% filter(club_name == input$club_filter)
    }
    if (input$year_filter != "All") {
      data <- data %>% filter(year == input$year_filter)
    } else {
      data <- data %>%
        group_by(club_name) %>%
        summarise(
          `in` = sum(`in`, na.rm = TRUE),
          `out` = sum(`out`, na.rm = TRUE),
          Net_Revenue = sum(Net_Revenue, na.rm = TRUE)
        )
    }
    filtered_data(data)
  }
  
  # Dynamically updating year dropdown based on selected club
  observe({
    club <- input$club_filter
    available_years <- if (club == "All") {
      unique(summary_data$year)
    } else {
      unique(summary_data %>% filter(club_name == club) %>% pull(year))
    }
    updateSelectInput(session, "year_filter", choices = c("All", available_years), selected = "All")
  })
  
  # Reactive values to store filtered data
  filtered_data <- reactiveVal(summary_data)
  
  # Reset filters to "All" when "Reset" button is clicked
  observeEvent(input$reset, {
    updateSelectInput(session, "club_filter", selected = "All")
    updateSelectInput(session, "year_filter", selected = "All")
    updateFilteredData()
  })
  
  output$year_filter_ui <- renderUI({
    selectInput("year_filter", "Select Year:", choices = c("All", unique(summary_data$year)), selected = "All")
  })
  
  output$filtered_plot <- renderPlotly({
    plot_data <- filtered_data()
    
    p <- ggplot(plot_data, aes(x = club_name)) +
      geom_bar(aes(y = `in`, fill = "Money Spent on Incoming Players", text = paste("Team:", club_name, "<br>Money Spent on Incoming Players: €", `in`)), stat = "identity", position = "dodge") +
      geom_bar(aes(y = -`out`, fill = "Revenue from Outgoing Players", text = paste("Team:", club_name, "<br>Revenue from Outgoing Players: €", `out`)), stat = "identity", position = "dodge") +
      geom_bar(aes(y = Net_Revenue, fill = "Net Revenue", text = paste("Team:", club_name, "<br>Net Revenue: €", Net_Revenue)), stat = "identity", position = "dodge", width = 0.4) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
      theme_minimal() +
      labs(title = "Financial Overview of Transfers", x = "Club Name", y = "Amount (€)", fill = "Transaction Type") +
      scale_fill_manual(values = c("Money Spent on Incoming Players" = "tomato", "Revenue from Outgoing Players" = "lightgreen", "Net Revenue" = "darkgreen")) +
      coord_flip()
    
    ggplotly(p, tooltip = "text") %>% 
      layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2)) %>%
      config(displayModeBar = FALSE)
  })
  
  # Initializing filtered data to default "All" and "All"
  observe({
    updateFilteredData()
  })
  
  # Reactive values to store filtered data
  filtered_data <- reactiveVal(summary_data)
  
  # Reset filters to "All" when "Reset" button is clicked
  observeEvent(input$reset, {
    updateSelectInput(session, "club_filter", selected = "All")
    updateSelectInput(session, "year_filter", selected = "All")
    updateFilteredData()
  })
  
  # Function to update filtered data
  updateFilteredData <- function() {
    data <- summary_data
    if (input$club_filter != "All") {
      data <- data %>% filter(club_name == input$club_filter)
    }
    if (input$year_filter != "All") {
      data <- data %>% filter(year == input$year_filter)
    } else {
      data <- data %>%
        group_by(club_name) %>%
        summarise(
          `in` = sum(`in`, na.rm = TRUE),
          `out` = sum(`out`, na.rm = TRUE),
          Net_Revenue = sum(Net_Revenue, na.rm = TRUE)
        )
    }
    filtered_data(data)
  }
  
  # Dynamically update year dropdown based on selected club
  observe({
    club <- input$club_filter
    available_years <- if (club == "All") {
      unique(summary_data$year)
    } else {
      unique(summary_data %>% filter(club_name == club) %>% pull(year))
    }
    updateSelectInput(session, "year_filter", choices = c("All", available_years), selected = "All")
  })
  
  # Initialize filtered data to default "All" and "All"
  observe({
    updateFilteredData()
  })
  
  # Main filtered plot
  output$filtered_plot <- renderPlotly({
    plot_data <- filtered_data()
    
    p <- ggplot(plot_data, aes(x = club_name)) +
      geom_bar(aes(y = `in`, fill = "Money Spent on Incoming Players", text = paste("Team:", club_name, "<br>Money Spent on Incoming Players: €", `in`)), stat = "identity", position = "dodge") +
      geom_bar(aes(y = -`out`, fill = "Revenue from Outgoing Players", text = paste("Team:", club_name, "<br>Revenue from Outgoing Players: €", `out`)), stat = "identity", position = "dodge") +
      geom_bar(aes(y = Net_Revenue, fill = "Net Revenue", text = paste("Team:", club_name, "<br>Net Revenue: €", Net_Revenue)), stat = "identity", position = "dodge", width = 0.4) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
      theme_minimal() +
      labs(title = "Financial Overview of Transfers", x = "Club Name", y = "Amount (€)", fill = "Transaction Type") +
      scale_fill_manual(values = c("Money Spent on Incoming Players" = "tomato", "Revenue from Outgoing Players" = "lightgreen", "Net Revenue" = "darkgreen")) +
      coord_flip()
    
    ggplotly(p, tooltip = "text") %>% 
      layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2)) %>%
      config(displayModeBar = FALSE)
  })
  
  # Reactive values to store filtered data for top transfers
  filtered_top_transfers <- reactive({
    filtered_data <- top_transfers
    if (input$top_club_filter != "All") {
      filtered_data <- filtered_data %>% filter(club_name == input$top_club_filter)
    }
    if (input$top_movement_filter != "All") {
      filtered_data <- filtered_data %>% filter(transfer_movement == input$top_movement_filter)
    }
    filtered_data
  })
  
  # Reset top transfers filters
  observeEvent(input$top_reset, {
    updateSelectInput(session, "top_club_filter", selected = "All")
    updateSelectInput(session, "top_movement_filter", selected = "All")
  })
  
  # Top transfers plot
  output$top_transfers_plot <- renderPlotly({
    plot_data <- filtered_top_transfers()
    
    scatter_plot <- ggplot(plot_data, aes(x = fee_cleaned, y = year, size = fee_cleaned, shape = club_name, color = transfer_movement, text = paste("Team:", club_name, "<br>Transfer Fee: €", fee_cleaned, "<br>Player Name:", player_name, "<br>Age:", age))) +
      geom_point(alpha = 0.6) +  # Add points with slight transparency
      geom_text_repel(aes(label = paste(player_name, "\nAge-", age)), size = 2, direction = "both", max.overlaps = Inf, colour = "black") +  
      labs(title = "Top 10 Transfers for Each of the Top 3 Spending Teams (Incoming and Outgoing)", 
           x = "Transfer Fee (€ million)", 
           y = "Year of Transfer", 
           size = "Transfer Fee", 
           color = "Transfer Type",
           shape = "Club Name") +  # Change the legend label here
      scale_shape_manual(values = c(16, 17, 18)) +  # Different shapes for each team
      scale_color_manual(values = c("in" = "lightgreen", "out" = "tomato")) +
      scale_y_continuous(breaks = seq(2010, 2020, 1)) +  # Setting breaks for the y-axis
      theme_minimal() +
      theme(legend.position = "bottom") +
      guides(size = FALSE)
    
    ggplotly(scatter_plot, tooltip = c("text")) %>% 
      layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2)) %>%
      config(displayModeBar = FALSE)
  })
  
  # Function to assign colors based on performance combinations
  assign_colors <- function(data, has_dribbles = TRUE) {
    data <- data %>%
      mutate(
        Category = case_when(
          !has_dribbles ~ case_when(
            TotalGoals >= quantile(TotalGoals, 0.98) & 
              TotalAssists >= quantile(TotalAssists, 0.98) ~ "Top Player in Goals and Assists",
            TotalGoals >= quantile(TotalGoals, 0.98) ~ "Top Player in Goals",
            TotalAssists >= quantile(TotalAssists, 0.98) ~ "Top Player in Assists",
            TRUE ~ "Average Player"
          ),
          TRUE ~ case_when(
            TotalGoals >= quantile(TotalGoals, 0.98) & 
              TotalAssists >= quantile(TotalAssists, 0.98) & 
              TotalSuccessfulDribbles >= quantile(TotalSuccessfulDribbles, 0.98) ~ "Top Player in Goals, Assists and Dribbles",
            TotalGoals >= quantile(TotalGoals, 0.98) & 
              TotalAssists >= quantile(TotalAssists, 0.98) ~ "Top Player in Goals and Assists",
            TotalGoals >= quantile(TotalGoals, 0.98) & 
              TotalSuccessfulDribbles >= quantile(TotalSuccessfulDribbles, 0.98) ~ "Top Player in Goals and Dribbles",
            TotalAssists >= quantile(TotalAssists, 0.98) & 
              TotalSuccessfulDribbles >= quantile(TotalSuccessfulDribbles, 0.98) ~ "Top Player in Assists and Dribbles",
            TotalGoals >= quantile(TotalGoals, 0.98) ~ "Top Player in Goals",
            TotalAssists >= quantile(TotalAssists, 0.98) ~ "Top Player in Assists",
            TotalSuccessfulDribbles >= quantile(TotalSuccessfulDribbles, 0.98) ~ "Top Player in Dribbles",
            TRUE ~ "Average Player"
          )
        ),
        Color = case_when(
          Category == "Top Player in Goals, Assists and Dribbles" ~ "black",  # Mixture of red, blue, and green
          Category == "Top Player in Goals and Assists" ~ "purple",  # Mixture of red and blue
          Category == "Top Player in Goals and Dribbles" ~ "yellow",  # Mixture of red and green
          Category == "Top Player in Assists and Dribbles" ~ "cyan",  # Mixture of blue and green
          Category == "Top Player in Goals" ~ "red",
          Category == "Top Player in Assists" ~ "blue",
          Category == "Top Player in Dribbles" ~ "green",
          Category == "Average Player" ~ "grey"
        )
      )
    return(data)
  }
  
  # Observe changes in season filter to update slider inputs
  observe({
    filtered_data <- if (input$seasonFilter == "All") {
      player_total_stats
    } else {
      player_season_stats %>% filter(season_name == input$seasonFilter)
    }
    
    updateSliderInput(session, "goalFilter", min = 0, max = max(filtered_data$TotalGoals, na.rm = TRUE), value = c(0, max(filtered_data$TotalGoals, na.rm = TRUE)))
    updateSliderInput(session, "assistFilter", min = 0, max = max(filtered_data$TotalAssists, na.rm = TRUE), value = c(0, max(filtered_data$TotalAssists, na.rm = TRUE)))
    
    # Determine if the current season should include the dribbles filter
    has_dribbles <- !(input$seasonFilter %in% unique(full_data$season_name[full_data$season_id %in% c(8578, 6559)]))
    
    # Conditionally update or hide the dribble filter slider based on has_dribbles
    if (has_dribbles) {
      updateSliderInput(session, "dribbleFilter", min = 0, max = max(filtered_data$TotalSuccessfulDribbles, na.rm = TRUE), value = c(0, max(filtered_data$TotalSuccessfulDribbles, na.rm = TRUE)))
      shinyjs::show("dribbleFilter")
    } else {
      shinyjs::hide("dribbleFilter")
    }
  })
  
  # Observe reset button to reset all filters to default values
  observeEvent(input$reset, {
    updateSelectInput(session, "seasonFilter", selected = "All")
    filtered_data <- player_total_stats
    updateSliderInput(session, "goalFilter", min = 0, max = max(filtered_data$TotalGoals, na.rm = TRUE), value = c(0, max(filtered_data$TotalGoals, na.rm = TRUE)))
    updateSliderInput(session, "assistFilter", min = 0, max = max(filtered_data$TotalAssists, na.rm = TRUE), value = c(0, max(filtered_data$TotalAssists, na.rm = TRUE)))
    updateSliderInput(session, "dribbleFilter", min = 0, max = max(filtered_data$TotalSuccessfulDribbles, na.rm = TRUE), value = c(0, max(filtered_data$TotalSuccessfulDribbles, na.rm = TRUE)))
    shinyjs::show("dribbleFilter")
  })
  
  # Interactive plot
  output$performancePlot <- renderPlotly({
    filtered_data <- if (input$seasonFilter == "All") {
      player_total_stats
    } else {
      player_season_stats %>% filter(season_name == input$seasonFilter)
    }
    
    filtered_data <- filtered_data %>%
      filter(TotalGoals >= input$goalFilter[1], TotalGoals <= input$goalFilter[2],
             TotalAssists >= input$assistFilter[1], TotalAssists <= input$assistFilter[2])
    
    # Determine if the current season should include the dribbles filter
    has_dribbles <- !(input$seasonFilter %in% unique(full_data$season_name[full_data$season_id %in% c(8578, 6559)]))
    
    if (has_dribbles) {
      filtered_data <- filtered_data %>%
        filter(TotalSuccessfulDribbles >= input$dribbleFilter[1], TotalSuccessfulDribbles <= input$dribbleFilter[2])
    }
    
    # Assign colors and categories to data
    filtered_data <- assign_colors(filtered_data, has_dribbles)
    
    if (has_dribbles) {
      plot_ly(filtered_data, 
              x = ~TotalGoals, 
              y = ~TotalAssists, 
              z = ~TotalSuccessfulDribbles, 
              color = ~Category,
              colors = c("Top Player in Goals, Assists and Dribbles" = "black",
                         "Top Player in Goals and Assists" = "purple",
                         "Top Player in Goals and Dribbles" = "yellow",
                         "Top Player in Assists and Dribbles" = "cyan",
                         "Top Player in Goals" = "red",
                         "Top Player in Assists" = "blue",
                         "Top Player in Dribbles" = "green",
                         "Average Player" = "grey"),
              text = ~paste("Player: ", player_name, 
                            "<br>Goals: ", TotalGoals, 
                            "<br>Assists: ", TotalAssists, 
                            "<br>Dribbles: ", TotalSuccessfulDribbles), 
              hoverinfo = "text") %>%
        add_markers() %>%
        layout(
          title = "Player Performance Based on Goals, Assists, and Dribbles",
          scene = list(
            xaxis = list(title = 'Total Goals'),
            yaxis = list(title = 'Total Assists'),
            zaxis = list(title = 'Total Dribbles'),
            camera = list(
              eye = list(x = 1.5, y = 1.5, z = 1.5)  # Adjust the camera position for zoom
            )
          ),
          legend = list(
            title = list(text = 'Player Performance'),
            orientation = 'h',
            x = 0.5,
            y = -0.2,
            xanchor = 'center'
          ),
          showlegend = TRUE
        ) %>%
        config(displayModeBar = FALSE)
    } else {
      plot_ly(filtered_data, 
              x = ~TotalGoals, 
              y = ~TotalAssists, 
              color = ~Category,
              colors = c("Top Player in Goals and Assists" = "purple",
                         "Top Player in Goals" = "red",
                         "Top Player in Assists" = "blue",
                         "Average Player" = "grey"),
              text = ~paste("Player: ", player_name, 
                            "<br>Goals: ", TotalGoals, 
                            "<br>Assists: ", TotalAssists), 
              hoverinfo = "text") %>%
        add_markers() %>%
        layout(
          title = "Player Performance Based on Goals and Assists",
          xaxis = list(title = 'Total Goals'),
          yaxis = list(title = 'Total Assists'),
          legend = list(
            title = list(text = 'Player Performace'),
            orientation = 'h',
            x = 0.5,
            y = -0.2,
            xanchor = 'center'
          ),
          showlegend = TRUE
        ) %>%
        config(displayModeBar = FALSE)
    }
  })
  
  # Passes plot
  output$passesPlot <- renderPlotly({
    # Create ggplot2 scatter plot with specific aesthetics
    scatter_plot <- ggplot() +
      geom_point(data = top_20_players, aes(x = TotalPasses, y = KeyPasses, size = KeyPasses, color = AccuratePasses), alpha = 0.6) +
      geom_text(data = top_20_players, aes(x = TotalPasses, y = KeyPasses, label = player_name), vjust = 3, size = 3) +
      geom_smooth(data = top_20_players, aes(x = TotalPasses, y = KeyPasses, group = 1), method = "lm", se = FALSE, color = "black", linetype = "dashed") +
      scale_color_gradient(low = "blue", high = "red") +
      scale_size(name = "Key Passes") +
      labs(title = "Scatter Plot: Relationship Between Total Passes and Key Passes",
           x = "Total Passes",
           y = "Key Passes",
           color = "Accurate Passes") +
      theme_minimal() +
      theme(legend.position = "right")
    
    ggplotly(scatter_plot)
  })
  
  # Functions to create bubble charts
  create_bubble_chart <- function(data, x, y, size, title, tooltip_text) {
    plot_ly(data, 
            x = ~get(x), 
            y = ~get(y), 
            size = ~size, 
            text = ~tooltip_text, 
            hoverinfo = "text",
            marker = list(opacity = 0.6)) %>%
      add_markers() %>%
      layout(title = title,
             xaxis = list(title = x),
             yaxis = list(title = y),
             showlegend = FALSE) %>%
      config(displayModeBar = FALSE)
  }
  
  observeEvent(input$show_goals_assists, {
    showModal(modalDialog(
      title = "Top 10 Players by Goals & Assists",
      plotlyOutput("goalsAssistsPlot"),
      size = "l",
      easyClose = TRUE
    ))
    output$goalsAssistsPlot <- renderPlotly({
      top_players <- player_total_stats %>%
        arrange(desc(GoalsAssistsSum)) %>%
        head(10)
      top_players <- top_players %>%
        mutate(tooltip_text = paste("Player: ", player_name, "<br>Goals: ", TotalGoals, "<br>Assists: ", TotalAssists))
      create_bubble_chart(top_players, "TotalGoals", "TotalAssists", top_players$GoalsAssistsSum, "Top 10 Players by Goals & Assists", top_players$tooltip_text)
    })
  })
  
  observeEvent(input$show_goals_dribbles, {
    showModal(modalDialog(
      title = "Top 10 Players by Goals & Dribbles",
      plotlyOutput("goalsDribblesPlot"),
      size = "l",
      easyClose = TRUE
    ))
    output$goalsDribblesPlot <- renderPlotly({
      top_players <- player_total_stats %>%
        arrange(desc(GoalsDribblesSum)) %>%
        head(10)
      top_players <- top_players %>%
        mutate(tooltip_text = paste("Player: ", player_name, "<br>Goals: ", TotalGoals, "<br>Dribbles: ", TotalSuccessfulDribbles))
      create_bubble_chart(top_players, "TotalGoals", "TotalSuccessfulDribbles", top_players$GoalsDribblesSum, "Top 10 Players by Goals & Dribbles", top_players$tooltip_text)
    })
  })
  
  observeEvent(input$show_assists_dribbles, {
    showModal(modalDialog(
      title = "Top 10 Players by Assists & Dribbles",
      plotlyOutput("assistsDribblesPlot"),
      size = "l",
      easyClose = TRUE
    ))
    output$assistsDribblesPlot <- renderPlotly({
      top_players <- player_total_stats %>%
        arrange(desc(AssistsDribblesSum)) %>%
        head(10)
      top_players <- top_players %>%
        mutate(tooltip_text = paste("Player: ", player_name, "<br>Assists: ", TotalAssists, "<br>Dribbles: ", TotalSuccessfulDribbles))
      create_bubble_chart(top_players, "TotalAssists", "TotalSuccessfulDribbles", top_players$AssistsDribblesSum, "Top 10 Players by Assists & Dribbles", top_players$tooltip_text)
    })
  })
  
  observeEvent(input$show_passes, {
    showModal(modalDialog(
      title = "Passes Plot",
      plotlyOutput("modalPassesPlot"),
      size = "l",
      easyClose = TRUE
    ))
    output$modalPassesPlot <- renderPlotly({
      top_20_players <- top_20_players %>%
        mutate(tooltip_text = paste(
          "Player Name: ", player_name, 
          "<br>Total Passes: ", TotalPasses, 
          "<br>Key Passes: ", KeyPasses, 
          "<br>Accurate Passes: ", AccuratePasses
        ))
      
      # Create ggplot2 scatter plot with specific aesthetics
      scatter_plot <- ggplot() +
        geom_point(data = top_20_players, aes(
          x = TotalPasses, 
          y = KeyPasses, 
          size = KeyPasses, 
          color = AccuratePasses, 
          text = tooltip_text
        ), alpha = 0.6) +
        scale_color_gradient(low = "blue", high = "red") +
        scale_size(name = "Key Passes") +
        labs(title = "Top 20 Passers: Relationship Between Total Passes and Key Passes and Accurate Passes",
             x = "Total Passes",
             y = "Key Passes",
             color = "Accurate Passes") +
        theme_minimal() +
        theme(legend.position = "right")
      
      ggplotly(scatter_plot, tooltip = "text") %>%
        config(displayModeBar = FALSE)
    })
    
  })
  
  # Observe reset button to reset goalkeeper filters
  observeEvent(input$resetGoalkeepers, {
    updateSelectInput(session, "season", selected = "All Seasons")
  })
  
  # Saved shots plot for goalkeepers
  output$savedShotsPlot <- renderPlotly({
    filtered_goalkeeper_data <- if (input$season == "All Seasons") {
      goalkeeper_stats_all_seasons
    } else {
      goalkeeper_stats_by_season %>% filter(season_name == input$season)
    }
    
    top_goalkeepers <- filtered_goalkeeper_data %>%
      arrange(desc(TotalSavedShots)) %>%
      head(input$numGoalkeepers)
    
    plot <- ggplot(top_goalkeepers, aes(x = TotalSavedShots, y = player_name, size = TotalSavedShots, color = SaveRatio)) +
      geom_point(alpha = 0.8, aes(text = paste("Goalkeeper Name:", player_name,
                                               "<br>Total Shots Saved:", TotalSavedShots,
                                               "<br>Percentage of Inside the Box Saves:", scales::percent(SaveRatio)))) +
      scale_size(range = c(3, 10)) +
      scale_color_gradient(low = "blue", high = "red", name = "Proportion\n(Inside the\nBox Saves)") +
      labs(x = "Total Saved Shots", y = "Goalkeepers", title = "Top La Liga Goalkeepers by Total Saved Shots") +
      theme_minimal() 
    
    ggplotly(plot, tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
  
  # Shiny observeEvent for plotly click
  observeEvent(event_data("plotly_click", source = "A"), {
    click_data <- event_data("plotly_click", source = "A")
    if (!is.null(click_data)) {
      clicked_team <- click_data$key
      if (!is.null(clicked_team) && clicked_team %in% unlist(total_stats$Team)) {
        team_info <- total_stats %>% filter(Team == clicked_team)
        team_logo <- team_logos[[clicked_team]]
        
        showModal(modalDialog(
          title = team_info$Team,
          div(
            style = "text-align: center;",
            img(src = team_logo, height = "300px"),
            h4(paste0("Total Wins: ", team_info$Total_Wins)),
            h4(paste0("Total Losses: ", team_info$Total_Losses)),
            h4(paste0("Total Draws: ", team_info$Total_Draws)),
            h4(paste0("Total Goals Scored: ", team_info$Total_Goals_Scored)),
            h4(paste0("Total Goals Conceded: ", team_info$Total_Goals_Conceded)),
            h4(paste0("Win/Loss Difference: ", team_info$Win_Loss_Difference)),
            h4(paste0("Goal Difference: ", team_info$Goal_Difference))
          ),
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
      }
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
