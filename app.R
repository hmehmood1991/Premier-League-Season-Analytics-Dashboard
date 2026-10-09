# =============================================================================
# app.R  --  Shiny app skeleton  (Stage 4)
# -----------------------------------------------------------------------------
# Sets up the interface and the database connection, with the full layout in
# place: sidebar inputs, a row of value boxes, and the six tabs. The charts
# themselves are placeholders at this stage; the SQL queries and visualizations
# are added in Stage 5.
#
# Run:  shiny::runApp("app.R")
# =============================================================================

library(shiny)
library(bslib)
library(DBI)
library(RSQLite)

DB_PATH <- "epl.sqlite"
if (!file.exists(DB_PATH)) source("build_db.R")   # build the database on first run

# One shared read-only connection for the app session.
con <- dbConnect(SQLite(), DB_PATH, flags = SQLITE_RO)
onStop(function() dbDisconnect(con))

# Populate the input choices from the database.
teams  <- sort(dbGetQuery(con, "SELECT DISTINCT team FROM team_match ORDER BY team")$team)
max_mw <- dbGetQuery(con, "SELECT MAX(matchweek) m FROM team_match")$m

pl_purple <- "#37003c"; pl_ink <- "#1d1d1f"

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------
theme <- bs_theme(version = 5, primary = pl_purple, bg = "#ffffff", fg = pl_ink)

# placeholder shown in each tab until the charts are built in Stage 5
placeholder <- function(label) {
  div(style = "height:440px;display:flex;align-items:center;justify-content:center;
               border:2px dashed #d8cede;border-radius:8px;color:#9a8fa3;
               font-size:18px;background:#faf7fb;",
      paste0(label, " — coming in Stage 5"))
}

ui <- page_sidebar(
  title = "Premier League 2024/25 — Season Analytics",
  theme = theme,
  sidebar = sidebar(
    width = 260,
    selectInput("team", "Focus team", choices = teams, selected = "Liverpool"),
    sliderInput("mw", "Matchweek range", min = 1, max = max_mw,
                value = c(1, max_mw), step = 1, sep = ""),
    checkboxInput("labels", "Show team labels", TRUE),
    hr(),
    helpText("Data is queried live from an SQLite database.")
  ),
  layout_columns(
    fill = FALSE, col_widths = c(3, 3, 3, 3),
    value_box("League position", textOutput("vb_pos"),  theme = "primary"),
    value_box("Points",          textOutput("vb_pts"),  theme = "text-primary"),
    value_box("Goals",           textOutput("vb_gf"),   theme = "text-primary"),
    value_box("Goals − xG", textOutput("vb_diff"), theme = "text-primary")
  ),
  navset_card_tab(
    nav_panel("League race",  placeholder("League race")),
    nav_panel("xG analysis",  placeholder("xG analysis")),
    nav_panel("Team form",    placeholder("Team form")),
    nav_panel("Home vs away", placeholder("Home vs away")),
    nav_panel("Shot map",     placeholder("Shot map")),
    nav_panel("Table",        placeholder("Table"))
  )
)

# -----------------------------------------------------------------------------
# SERVER
# -----------------------------------------------------------------------------
# At this stage the value boxes show placeholder dashes. They are wired to real
# per-team SQL queries in Stage 5.
server <- function(input, output, session) {
  output$vb_pos  <- renderText("–")
  output$vb_pts  <- renderText("–")
  output$vb_gf   <- renderText("–")
  output$vb_diff <- renderText("–")
}

shinyApp(ui, server)
