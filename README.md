# Premier League Season Analytics Dashboard

An interactive R Shiny dashboard that explores one full season of English Premier
League data (2024/25). The data is stored in an SQLite database, and every
visualization pulls its data through SQL queries, including window functions for
the cumulative-points and rolling-form calculations.

> Status: in development. See the [Roadmap](#roadmap) below.

## Overview

Raw football statistics usually sit in dense tables that are hard to read at a
glance. This project turns a season of match and shot data into a set of linked,
interactive visualizations so a user can explore the season visually instead of
reading through spreadsheets. Rather than just filtering raw numbers, the app
computes derived performance measures (cumulative points, expected-goals
differentials, rolling form, home/away splits, spatial shot aggregation) and
presents them graphically.

## Features

The dashboard has six tabbed views, driven by a team selector, a matchweek
range slider, and a label toggle:

- **League race** - cumulative points across the season, with the selected team highlighted
- **xG analysis** - goals scored vs expected goals, showing over- and under-performance
- **Team form** - rolling 5-match average points per game
- **Home vs away** - points-per-game split, shown as a dumbbell chart
- **Shot map** - every shot on a pitch, sized by xG and coloured by outcome
- **Table** - the full league table

A row of value boxes shows live headline stats (position, points, goals,
goals - xG) for the selected team.

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
├── .gitignore
└── docs/                 # written deliverables (proposal, code docs, data docs)
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
(`team_match`). See `docs/` for a full data dictionary.

## Documentation

The `docs/` folder contains the project write-ups: the proposal, code
explanations for each R file, and a dataset/database design document.

## Roadmap

- [x] Stage 1 - Project setup and scaffolding
- [ ] Stage 2 - Data layer (worldfootballR pipeline and sample-data generator)
- [ ] Stage 3 - SQLite database creation (tables, view, index)
- [ ] Stage 4 - Shiny app skeleton (UI layout, DB connection, tabs)
- [ ] Stage 5 - SQL queries and all six visualizations
- [ ] Stage 6 - Styling, polish, and documentation

## License

Released under the MIT License. See `LICENSE` for details.

## Author

Your Name - [your GitHub profile](https://github.com/your-username)
