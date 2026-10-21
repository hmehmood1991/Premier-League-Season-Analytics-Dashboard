# =============================================================================
# app.R  --  SQL queries + all six visualizations  (Stage 5)
# -----------------------------------------------------------------------------
# Builds on the Stage 4 skeleton. Adds the SQL query helpers (including window
# functions for cumulative points and rolling form) and wires every tab and
# value box to a real chart or figure. Styling polish comes in Stage 6.
#
# Run:  shiny::runApp("app.R")
# =============================================================================

library(shiny)
library(bslib)
library(ggplot2)
library(DBI)
library(RSQLite)

DB_PATH <- "epl.sqlite"
if (!file.exists(DB_PATH)) source("build_db.R")   # build the database on first run

# One shared read-only connection for the app session.
con <- dbConnect(SQLite(), DB_PATH, flags = SQLITE_RO)
onStop(function() dbDisconnect(con))

teams  <- sort(dbGetQuery(con, "SELECT DISTINCT team FROM team_match ORDER BY team")$team)
max_mw <- dbGetQuery(con, "SELECT MAX(matchweek) m FROM team_match")$m

pl_purple <- "#37003c"; pl_pink <- "#e90052"
pl_blue   <- "#2b7bba"; pl_ink  <- "#1d1d1f"

# -----------------------------------------------------------------------------
# SQL query helpers (the data layer)
# -----------------------------------------------------------------------------
# Cumulative points per team per matchweek -- SQL window function.
q_trajectory <- function() dbGetQuery(con, "
  SELECT team, matchweek,
         SUM(points) OVER (PARTITION BY team ORDER BY matchweek) AS cum_points
  FROM team_match")

# Full league table -- grouped aggregates.
q_totals <- function() dbGetQuery(con, "
  SELECT team,
         COUNT(*) P,
         SUM(points = 3) W, SUM(points = 1) D, SUM(points = 0) L,
         SUM(gf) GF, SUM(ga) GA, SUM(gf) - SUM(ga) GD,
         ROUND(SUM(xg), 1) xGF, SUM(points) Pts
  FROM team_match GROUP BY team
  ORDER BY Pts DESC, GD DESC, GF DESC")

# Rolling 5-match average points -- windowed average with a frame.
q_form <- function(team) dbGetQuery(con, "
  SELECT matchweek,
         AVG(points) OVER (ORDER BY matchweek
                           ROWS BETWEEN 4 PRECEDING AND CURRENT ROW) AS form
  FROM team_match WHERE team = ? ORDER BY matchweek", params = list(team))

# Home/away points per game -- grouped aggregate.
q_homeaway <- function() dbGetQuery(con, "
  SELECT team, venue, AVG(points) AS ppg
  FROM team_match GROUP BY team, venue")

# One team's shots -- parameterized filter.
q_shots <- function(team) dbGetQuery(con, "
  SELECT x, y, xg, goal FROM shots WHERE team = ?", params = list(team))

# Headline numbers for the value boxes.
q_team_line <- function(team) dbGetQuery(con, "
  SELECT SUM(points) Pts, SUM(gf) GF, ROUND(SUM(xg),1) xGF, SUM(gf) - SUM(ga) GD
  FROM team_match WHERE team = ?", params = list(team))

q_position <- function(team) { tot <- q_totals(); which(tot$team == team) }

# ordinal suffix for league position (1 -> 1st, 2 -> 2nd, ...)
sfx <- function(n) {
  if (n %% 100 %in% 11:13) "th" else
    switch(as.character(n %% 10), "1" = "st", "2" = "nd", "3" = "rd", "th")
}

# vertical half-pitch for the shot map
pitch_layers <- function() {
  ln <- "grey75"; turf <- "#eaf6ea"
  list(
    annotate("rect", xmin = 0,  xmax = 80, ymin = 60,  ymax = 120, fill = turf, colour = ln),
    annotate("rect", xmin = 18, xmax = 62, ymin = 102, ymax = 120, fill = NA,   colour = ln),
    annotate("rect", xmin = 30, xmax = 50, ymin = 114, ymax = 120, fill = NA,   colour = ln),
    annotate("point", x = 40, y = 108, colour = ln, size = 1)
  )
}

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------
theme <- bs_theme(version = 5, primary = pl_purple, bg = "#ffffff", fg = pl_ink)

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
    helpText("Data is queried live from an SQLite database. The focus team is",
             "highlighted where relevant.")
  ),
  layout_columns(
    fill = FALSE, col_widths = c(3, 3, 3, 3),
    value_box("League position", textOutput("vb_pos"),  theme = "primary"),
    value_box("Points",          textOutput("vb_pts"),  theme = "text-primary"),
    value_box("Goals",           textOutput("vb_gf"),   theme = "text-primary"),
    value_box("Goals − xG", textOutput("vb_diff"), theme = "text-primary")
  ),
  navset_card_tab(
    nav_panel("League race",  plotOutput("p_traj",  height = 440)),
    nav_panel("xG analysis",  plotOutput("p_xg",    height = 440)),
    nav_panel("Team form",    plotOutput("p_form",  height = 440)),
    nav_panel("Home vs away", plotOutput("p_ha",    height = 520)),
    nav_panel("Shot map",     plotOutput("p_shots", height = 520)),
    nav_panel("Table",        tableOutput("tbl"))
  )
)

# -----------------------------------------------------------------------------
# SERVER
# -----------------------------------------------------------------------------
server <- function(input, output, session) {

  totals <- reactive(q_totals())

  # ---- value boxes ----
  output$vb_pos  <- renderText({ p <- q_position(input$team); paste0(p, sfx(p)) })
  output$vb_pts  <- renderText({ q_team_line(input$team)$Pts })
  output$vb_gf   <- renderText({ q_team_line(input$team)$GF })
  output$vb_diff <- renderText({
    d <- q_team_line(input$team); sprintf("%+.1f", d$GF - d$xGF)
  })

  # ---- League race: cumulative points ----
  output$p_traj <- renderPlot({
    d <- q_trajectory()
    d <- d[d$matchweek >= input$mw[1] & d$matchweek <= input$mw[2], ]
    hi   <- d[d$team == input$team, ]
    ends <- d[d$matchweek == max(d$matchweek), ]
    top4 <- ends[order(-ends$cum_points), ][1:4, ]
    ggplot(d, aes(matchweek, cum_points, group = team)) +
      geom_line(colour = "grey82", linewidth = 0.5) +
      geom_line(data = hi, colour = pl_pink, linewidth = 1.5) +
      geom_point(data = hi[nrow(hi), ], colour = pl_pink, size = 2.6) +
      { if (input$labels) geom_text(data = top4, aes(label = team),
          hjust = 0, nudge_x = 0.4, size = 3.4, colour = "grey30", check_overlap = TRUE) } +
      scale_x_continuous(expand = expansion(mult = c(0.01, 0.14))) +
      labs(title = "The title & relegation race",
           subtitle = paste0("Cumulative points · highlighted: ", input$team),
           x = "Matchweek", y = "Points") +
      theme_minimal(base_size = 13)
  })

  # ---- xG analysis: goals vs expected goals ----
  output$p_xg <- renderPlot({
    d <- totals(); d$diff <- d$GF - d$xGF
    lim <- range(c(d$GF, d$xGF)) + c(-4, 6)
    p <- ggplot(d, aes(xGF, GF)) +
      geom_abline(slope = 1, intercept = 0, linetype = 2, colour = "grey70") +
      geom_point(aes(fill = diff), shape = 21, colour = "white", size = 4.4, stroke = 0.6) +
      scale_fill_gradient2(low = pl_blue, mid = "grey80", high = pl_pink,
                           midpoint = 0, name = "Goals − xG") +
      coord_equal(xlim = lim, ylim = lim) +
      labs(title = "Finishing: goals vs expected goals",
           subtitle = "Above the dashed line = scored more than expected",
           x = "Expected goals (xG)", y = "Goals scored") +
      theme_minimal(base_size = 13)
    if (input$labels)
      p <- p + geom_text(aes(label = team), vjust = -1.1, size = 3.1,
                         colour = "grey25", check_overlap = TRUE)
    p
  })

  # ---- Team form: rolling 5-match points ----
  output$p_form <- renderPlot({
    d <- q_form(input$team)
    d <- d[d$matchweek >= input$mw[1] & d$matchweek <= input$mw[2], ]
    ggplot(d, aes(matchweek, form)) +
      geom_hline(yintercept = c(1, 2), linetype = 3, colour = "grey80") +
      geom_area(fill = pl_purple, alpha = 0.12, na.rm = TRUE) +
      geom_line(colour = pl_purple, linewidth = 1.3, na.rm = TRUE) +
      geom_point(colour = pl_pink, size = 2.6, na.rm = TRUE) +
      scale_y_continuous(limits = c(0, 3), breaks = 0:3) +
      labs(title = paste("Form & momentum —", input$team),
           subtitle = "Rolling 5-match average points per game (SQL window function)",
           x = "Matchweek", y = "Points per game") +
      theme_minimal(base_size = 13)
  })

  # ---- Home vs away: dumbbell ----
  output$p_ha <- renderPlot({
    ha <- q_homeaway()
    home <- ha[ha$venue == "Home", c("team", "ppg")]; names(home)[2] <- "Home"
    away <- ha[ha$venue == "Away", c("team", "ppg")]; names(away)[2] <- "Away"
    wide <- merge(home, away, by = "team")
    wide <- wide[order(wide$Home + wide$Away), ]
    wide$team <- factor(wide$team, levels = wide$team)
    ggplot(wide) +
      geom_segment(aes(x = Away, xend = Home, y = team, yend = team),
                   colour = "grey80", linewidth = 1.1) +
      geom_point(aes(Away, team, colour = "Away"), size = 3.2) +
      geom_point(aes(Home, team, colour = "Home"), size = 3.2) +
      scale_colour_manual(values = c(Home = pl_purple, Away = pl_pink), name = NULL) +
      labs(title = "Home comfort vs away resilience",
           subtitle = "Points per game at home and away · each line is one team",
           x = "Points per game", y = NULL) +
      theme_minimal(base_size = 13) + theme(legend.position = "bottom")
  })

  # ---- Shot map ----
  output$p_shots <- renderPlot({
    d <- q_shots(input$team)
    d$px <- d$y; d$py <- d$x                    # width -> x, length -> y (attack up)
    d$Outcome <- ifelse(d$goal == 1, "Goal", "No goal")
    ggplot(d, aes(px, py)) +
      pitch_layers() +
      geom_point(aes(size = xg, fill = Outcome), shape = 21,
                 colour = "white", alpha = 0.85, stroke = 0.4) +
      scale_size_continuous(range = c(1.4, 7), name = "xG") +
      scale_fill_manual(values = c(Goal = pl_pink, `No goal` = "grey55"), name = NULL) +
      coord_fixed(xlim = c(0, 80), ylim = c(60, 121)) +
      labs(title = paste("Where the chances came from —", input$team),
           subtitle = "Every shot, sized by xG · attacking towards the top") +
      theme_minimal(base_size = 13) +
      theme(axis.text = element_blank(), axis.title = element_blank(),
            panel.grid = element_blank(), legend.position = "bottom")
  })

  # ---- League table ----
  output$tbl <- renderTable({
    t <- totals(); t$Pos <- seq_len(nrow(t))
    t[, c("Pos", "team", "P", "W", "D", "L", "GF", "GA", "GD", "xGF", "Pts")] |>
      setNames(c("Pos", "Team", "P", "W", "D", "L", "GF", "GA", "GD", "xG", "Pts"))
  }, striped = TRUE, hover = TRUE, digits = 1, width = "100%")
}

shinyApp(ui, server)
