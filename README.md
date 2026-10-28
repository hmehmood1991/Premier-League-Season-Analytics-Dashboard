# Premier League Season Analytics Dashboard

An interactive R Shiny dashboard that explores one full season of English Premier
League data (2024/25). The data is stored in an SQLite database, and every
visualization pulls its data through SQL queries, including window functions for
the cumulative-points and rolling-form calculations.

## Overview

Raw football statistics usually sit in dense tables that are hard to read at a
glance. This project turns a season of match and shot data into a set of linked,
interactive visualizations so a user can explore the season visually instead of
reading through spreadsheets. Rather than just filtering raw numbers, the app
computes derived performance measures (cumulative points, expected-goals
differentials, rolling form, home/away splits, spatial shot aggregation) and
presents them graphically.

## Features

The dashboard has seven tabbed views, driven by a team selector, a matchweek
range slider, and a label toggle:

- **League race** - cumulative points across the season, with the selected team highlighted
- **Title race** - an animated bump chart of every team's league position by matchweek; press Play to watch the season run once from week 1 to 38, with the final champion, the top four (Champions League places) and the relegation zone highlighted and everyone else in grey
- **xG analysis** - goals scored vs expected goals, showing over- and under-performance
- **Team form** - rolling 5-match average points per game
- **Home vs away** - points-per-game split, shown as a dumbbell chart
- **Shot map** - every shot on a pitch, sized by xG and coloured by outcome
- **Table** - the full league table

A row of value boxes shows live headline stats (position, points, goals,
goals - xG) for the selected team.

### How the controls affect each view

- The **team selector** drives the three single-team views (League race highlight, Team form, Shot map) and the value boxes.
- The **matchweek slider** zooms the League race and Team form charts and filters the aggregate views (Table, xG analysis, Home vs away) and the value boxes to only the matches played inside the selected window, so you can inspect any stretch of the season as its own mini-table.
- The **Title race** tab has its own compact Play slider (with the play button to the left) that runs the season once from week 1 to 38 and then stops, independent of the sidebar.

## SQL-driven analysis

Every figure is produced by a SQL query against the database rather than
computed in memory, including three window functions:

- **Cumulative points** use `SUM(points) OVER (PARTITION BY team ORDER BY matchweek)`.
- **Rolling form** uses `AVG(points) OVER (ORDER BY matchweek ROWS BETWEEN 4 PRECEDING AND CURRENT ROW)`.
- **League position by matchweek** (the Title race bump chart) uses a `ROW_NUMBER()` ranking window function over a CTE of cumulative points, with goal difference and goals for as tie-breaks.

The league table and home/away splits use `GROUP BY` aggregates, the aggregate
views filter to the selected window with `WHERE matchweek BETWEEN ? AND ?`, and
all per-team figures use parameterized queries (`WHERE team = ?`).

## Tech stack

- **R** with **Shiny** (web interface) and **bslib** (Bootstrap 5 theming)
- **ggplot2** for the visualizations
- **SQLite** via **DBI** + **RSQLite** for storage and querying
- **dplyr** for the data-preparation step
- **worldfootballR** (optional) for real data from FBref and Understat

## Project structure

```
.
├── app.R                 # the Shiny application
├── build_db.R            # builds epl.sqlite from the dataset
├── epl.sqlite            # SQLite database (generated; see Data below)
├── README.md
└── .gitignore
```

## Getting started

### Prerequisites

- R (version 4.1 or newer recommended)

Install the required packages:

```r
install.packages(c("shiny", "bslib", "ggplot2", "DBI", "RSQLite", "dplyr"))
# Optional, only needed for real (downloaded) data:
# install.packages("worldfootballR")
```

### Run the app

```r
shiny::runApp("app.R")
```

On first launch, if `epl.sqlite` is missing the app runs `build_db.R`
automatically to create it. You can also build it manually:

```r
Rscript build_db.R
```

## Data

The app uses two datasets: match results and shot-level data.

- **Sample data (default).** `build_db.R` generates a statistically realistic
  season so the app runs with no network connection. Note that these numbers are
  simulated for demonstration, not real results.
- **Real data (optional).** Set `USE_REAL_DATA <- TRUE` in `build_db.R`, install
  `worldfootballR`, and delete `epl.sqlite` so it rebuilds from FBref and
  Understat.

The database contains two tables (`matches`, `shots`) and one view
(`team_match`). See `docs/04_dataset_and_database.docx` for a full data dictionary.

## License

Released under the MIT License. See `LICENSE` for details.

## Author

Hamid Mehmood - (https://github.com/hmehmood1991)
