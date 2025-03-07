# La Liga Analytics Dashboard

This repository contains an interactive dashboard designed to provide an in-depth analysis of team performance, player contributions, and transfer strategies within the La Liga football league. The project uses R and Shiny to visualize various metrics and trends, offering insights to analysts, coaches, and fans alike.

---

## Contents

### Main Files

1. **`DOCUMENTATION.docx`**
   - Detailed project documentation, including the problem description, design process, and implementation details.

2. **`LALIGA_RSHINY.R`**
   - Main R script for the Shiny dashboard implementation.
   - Contains data preprocessing, interactive visualization components, and dashboard structure.

3. **`LaLiga_Stadiums.csv`**
   - Dataset containing stadium locations for all La Liga teams.

4. **`combined_data_laliga.csv`**
   - Core dataset with match results and performance statistics from 2014 to 2020.

5. **`primera-division.csv_2010_2020.csv`**
   - Dataset with player transfer details from 2010 to 2020.

6. **`filtered_players_stats.csv`**
   - Preprocessed dataset with player performance metrics.

7. **`player_id_mapping.csv`**
   - Maps player IDs to their respective names.

8. **`season_mapping.csv`**
   - Maps season IDs to season names, renaming "Primera Division" to "La Liga."

9. **`es_10km.shp`**
   - Shapefile for visualizing geographical data on La Liga stadium locations.

---

## Dashboard Structure

The interactive dashboard is divided into five main tabs:

1. **Introduction**
   - Overview of the project objectives and insights.
   - Interactive introductory plot showcasing team performance metrics.

2. **Teams Performance Dashboard**
   - Geographic and temporal analysis of team performance metrics (e.g., goals, wins, losses).
   - Interactive map and bar chart for comparing teams across metrics and seasons.

3. **Player Performance Dashboard**
   - Analysis of individual player statistics (e.g., goals, assists, dribbles).
   - Focus on top-performing players and goalkeepers across seasons.

4. **Transfers Dashboard**
   - Analysis of club spending and revenue in the transfer market.
   - Highlights the top 10 transfers for the top 3 spending clubs.

5. **Conclusion**
   - Summarizes key findings and provides recommendations.

---

## Setup and Usage

### Prerequisites
- Install R and RStudio.
- Install required R packages using the following script (included in `LALIGA_RSHINY.R`):

```r
necessary_packages <- c("shiny", "shinydashboard", "dplyr", "ggplot2", "lubridate", "readr", "tidyr", "stringr", "plotly", "sf", "leaflet", "ggrepel", "shinyjs")

for (package in necessary_packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package)
  }
}
```

### Steps to Run
1. Clone the repository:
   ```bash
   git clone https://github.com/your-username/LaLiga-Dashboard.git
   ```
2. Open `LALIGA_RSHINY.R` in RStudio.
3. Run the script to start the Shiny dashboard.
4. Open the provided localhost URL in your browser to view the dashboard.

---

## Key Features

- **Interactive Visualizations**: Dropdowns, sliders, and tooltips for an engaging user experience.
- **Geospatial Analysis**: Choropleth maps showing team performance by region.
- **Player Insights**: Top performers in goals, assists, and goalkeeping.
- **Financial Analysis**: Detailed view of club spending and revenue in the transfer market.

---

## Data Sources

- [La Liga Stadium Data](https://example.com)
- [Primera Division Match Data (2010-2020)](https://example.com)
- [Player Performance Data](https://example.com)

---

## Contributions

Contributions are welcome! Feel free to submit pull requests for enhancements or bug fixes. Contact me at aadhikary252000@gmail.com

---

## License

This project is licensed under GNU (General Public License). See the LICENSE file for details.
