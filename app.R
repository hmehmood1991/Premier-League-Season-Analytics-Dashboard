# =============================================================================
# Premier League Season Analytics Dashboard  (SQL-backed)
# -----------------------------------------------------------------------------
# Data is stored in an SQLite database (epl.sqlite) and every visualization
# pulls its data through SQL queries, including window functions for the
# cumulative-points and rolling-form calculations. If the database is missing,
# it is built automatically from build_db.R on first launch.
#
# Run with:  shiny::runApp("app.R")
# Files:     build_db.R (creates epl.sqlite) and this app.R
# =============================================================================

library(shiny)
library(bslib)
library(ggplot2)
library(DBI)
library(RSQLite)

DB_PATH <- "epl.sqlite"
if (!file.exists(DB_PATH)) source("build_db.R")   # build DB on first run

# One shared read-only connection for the app session.
con <- dbConnect(SQLite(), DB_PATH, flags = SQLITE_RO)
onStop(function() dbDisconnect(con))

teams  <- sort(dbGetQuery(con, "SELECT DISTINCT team FROM team_match ORDER BY team")$team)
max_mw <- dbGetQuery(con, "SELECT MAX(matchweek) m FROM team_match")$m

# ---- palette + ggplot theme -------------------------------------------------
pl_purple <- "#37003c"; pl_pink <- "#e90052"
pl_blue   <- "#2b7bba"; pl_ink  <- "#1d1d1f"
# Premier League finishing zones (used to colour the Title race bump chart)
zone_cols <- c("Champion"    = "#37003c",   # 1st
               "Top 4 (UCL)" = "#2b7bba",   # 2nd-4th, Champions League
               "Relegation"  = "#e63946",   # bottom three
               "Mid-table"   = "grey82")

theme_pl <- function(base = 13) {
  theme_minimal(base_size = base) +
    theme(
      plot.title      = element_text(face = "bold", colour = pl_purple, size = rel(1.25)),
      plot.subtitle   = element_text(colour = "grey35", size = rel(0.95),
                                     margin = margin(b = 10)),
      plot.caption    = element_text(colour = "grey60", size = rel(0.72)),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "grey92"),
      axis.title      = element_text(colour = "grey30"),
      legend.position = "bottom",
      plot.margin     = margin(14, 18, 10, 12)
    )
}
CAP <- "Sample data for demonstration - pipeline: FBref / Understat via worldfootballR"

# ---- SQL query helpers ------------------------------------------------------
# The aggregate helpers take a matchweek window (w1, w2) and count only the
# matches played inside it, so the table, xG chart, home/away chart and the
# value boxes all respond to the sidebar slider.
q_trajectory <- function() dbGetQuery(con, "
  SELECT team, matchweek,
         SUM(points) OVER (PARTITION BY team ORDER BY matchweek) AS cum_points
  FROM team_match")

q_totals <- function(w1 = 1, w2 = max_mw) dbGetQuery(con, "
  SELECT team,
         COUNT(*) P,
         SUM(points = 3) W, SUM(points = 1) D, SUM(points = 0) L,
         SUM(gf) GF, SUM(ga) GA, SUM(gf) - SUM(ga) GD,
         ROUND(SUM(xg), 1) xGF, SUM(points) Pts
  FROM team_match WHERE matchweek BETWEEN ? AND ?
  GROUP BY team
  ORDER BY Pts DESC, GD DESC, GF DESC", params = list(w1, w2))

q_form <- function(team) dbGetQuery(con, "
  SELECT matchweek,
         AVG(points) OVER (ORDER BY matchweek
                           ROWS BETWEEN 4 PRECEDING AND CURRENT ROW) AS form
  FROM team_match WHERE team = ? ORDER BY matchweek", params = list(team))

q_homeaway <- function(w1 = 1, w2 = max_mw) dbGetQuery(con, "
  SELECT team, venue, AVG(points) AS ppg
  FROM team_match WHERE matchweek BETWEEN ? AND ?
  GROUP BY team, venue", params = list(w1, w2))

q_shots <- function(team) dbGetQuery(con, "
  SELECT x, y, xg, goal FROM shots WHERE team = ?", params = list(team))

q_team_line <- function(team, w1 = 1, w2 = max_mw) dbGetQuery(con, "
  SELECT SUM(points) Pts, SUM(gf) GF, ROUND(SUM(xg),1) xGF,
         SUM(gf) - SUM(ga) GD
  FROM team_match WHERE team = ? AND matchweek BETWEEN ? AND ?",
  params = list(team, w1, w2))

q_position <- function(team, w1 = 1, w2 = max_mw) {
  tot <- q_totals(w1, w2); which(tot$team == team)
}

# League position after every matchweek, for the bump chart. A ROW_NUMBER()
# ranking window function sits on top of cumulative points (goal difference and
# goals for break ties), giving each team a unique 1-20 position every week.
q_positions <- function() dbGetQuery(con, "
  WITH cum AS (
    SELECT team, matchweek,
           SUM(points)  OVER (PARTITION BY team ORDER BY matchweek) AS cp,
           SUM(gf - ga) OVER (PARTITION BY team ORDER BY matchweek) AS cgd,
           SUM(gf)      OVER (PARTITION BY team ORDER BY matchweek) AS cgf
    FROM team_match
  )
  SELECT matchweek, team,
         ROW_NUMBER() OVER (PARTITION BY matchweek
                            ORDER BY cp DESC, cgd DESC, cgf DESC) AS position
  FROM cum")

# ordinal suffix for league position
sfx <- function(n) {
  if (n %% 100 %in% 11:13) "th" else
    switch(as.character(n %% 10), "1" = "st", "2" = "nd", "3" = "rd", "th")
}

# label for the selected matchweek window (used in subtitles)
wk_lbl <- function(mw) {
  if (mw[1] == 1 && mw[2] == max_mw) "full season"
  else paste0("weeks ", mw[1], "-", mw[2])
}

# ---- vertical half-pitch for the shot map -----------------------------------
pitch_layers <- function() {
  ln <- "grey75"; turf <- "#eaf6ea"
  list(
    annotate("rect", xmin = 0,  xmax = 80, ymin = 60,  ymax = 120, fill = turf, colour = ln),
    annotate("rect", xmin = 18, xmax = 62, ymin = 102, ymax = 120, fill = NA,   colour = ln),
    annotate("rect", xmin = 30, xmax = 50, ymin = 114, ymax = 120, fill = NA,   colour = ln),
    annotate("rect", xmin = 36, xmax = 44, ymin = 120, ymax = 120.6, fill = NA, colour = ln, linewidth = 1),
    annotate("point", x = 40, y = 108, colour = ln, size = 1),
    annotate("path",
             x = 40 + 10 * cos(seq(pi, 2*pi, length.out = 60)),
             y = 102 + 6  * sin(seq(pi, 2*pi, length.out = 60)) * -1,
             colour = ln)
  )
}

# =============================================================================
# UI
# =============================================================================
theme <- bs_theme(
  version = 5, primary = pl_purple,
  bg = "#ffffff", fg = pl_ink,
  "border-radius" = "0.6rem", "card-border-color" = "#ece3ef"
)

ui <- page_sidebar(
  title = "Premier League 2024/25 - Season Analytics",
  theme = theme,
  sidebar = sidebar(
    width = 260,
    selectInput("team", "Focus team", choices = teams, selected = "Liverpool"),
    sliderInput("mw", "Matchweek range", min = 1, max = max_mw,
                value = c(1, max_mw), step = 1, sep = ""),
    checkboxInput("labels", "Show team labels", TRUE),
    hr(),
    helpText("Data is queried live from an SQLite database. The focus team is",
             "highlighted where relevant, and the matchweek slider filters the",
             "table, xG and home/away charts to the selected window.")
  ),
  layout_columns(
    fill = FALSE, col_widths = c(3, 3, 3, 3),
    value_box("League position", textOutput("vb_pos"),  theme = "purple"),
    value_box("Points",          textOutput("vb_pts"),  theme = "text-purple"),
    value_box("Goals",           textOutput("vb_gf"),   theme = "text-purple"),
    value_box("Goals - xG", textOutput("vb_diff"), theme = "text-purple")
  ),
  navset_card_tab(
    nav_panel("League race",  plotOutput("p_traj",  height = 440)),
    nav_panel("Title race",
              tags$div(
                class = "race-ctrl",
                tags$style(HTML(
                  ".race-ctrl{max-width:470px;position:relative;padding-left:42px;}
                   .race-ctrl .form-group{margin-bottom:4px;}
                   .race-ctrl .control-label{font-size:12px;margin-bottom:2px;}
                   .race-ctrl .slider-animate-container{position:absolute;left:0;top:26px;margin:0;}
                   .race-ctrl .slider-animate-button{font-size:20px;}")),
                sliderInput("race_wk", "Play the season (week)",
                            min = 1, max = max_mw, value = max_mw, step = 1, sep = "",
                            width = "100%",
                            animate = animationOptions(interval = 450, loop = FALSE))),
              plotOutput("p_bump", height = 640)),
    nav_panel("xG analysis",  plotOutput("p_xg",    height = 440)),
    nav_panel("Team form",    plotOutput("p_form",  height = 440)),
    nav_panel("Home vs away", plotOutput("p_ha",    height = 520)),
    nav_panel("Shot map",     plotOutput("p_shots", height = 520)),
    nav_panel("Table",        tableOutput("tbl"))
  )
)

# =============================================================================
# SERVER
# =============================================================================
server <- function(input, output, session) {

  # League table for the selected matchweek window, shared by the boxes,
  # the xG chart and the table.
  totals <- reactive(q_totals(input$mw[1], input$mw[2]))

  # ---- value boxes (also follow the matchweek window) -----------------------
  output$vb_pos  <- renderText({
    p <- q_position(input$team, input$mw[1], input$mw[2]); paste0(p, sfx(p))
  })
  output$vb_pts  <- renderText({ q_team_line(input$team, input$mw[1], input$mw[2])$Pts })
  output$vb_gf   <- renderText({ q_team_line(input$team, input$mw[1], input$mw[2])$GF })
  output$vb_diff <- renderText({
    d <- q_team_line(input$team, input$mw[1], input$mw[2]); sprintf("%+.1f", d$GF - d$xGF)
  })

  # ---- League race ----------------------------------------------------------
  output$p_traj <- renderPlot({
    d <- q_trajectory()
    d <- d[d$matchweek >= input$mw[1] & d$matchweek <= input$mw[2], ]
    hi <- d[d$team == input$team, ]
    ggplot(d, aes(matchweek, cum_points, group = team)) +
      geom_line(colour = "grey82", linewidth = 0.5) +
      geom_line(data = hi, colour = pl_pink, linewidth = 1.7) +
      geom_point(data = hi[nrow(hi), ], colour = pl_pink, size = 2.8) +
      scale_x_continuous(expand = expansion(mult = c(0.01, 0.04))) +
      labs(title = "The title & relegation race",
           subtitle = paste0("Cumulative points - the selected team (", input$team,
                             ") is shown in colour"),
           x = "Matchweek", y = "Points", caption = CAP) +
      theme_pl()
  })

  # ---- Title race: league position by matchweek (bump chart) ----------------
  output$p_bump <- renderPlot({
    d <- q_positions()
    # Zones are fixed to the FINAL standings and reflect real Premier League
    # outcomes. The play slider reveals the season up to the chosen week.
    fin <- d[d$matchweek == max_mw, c("team", "position")]
    names(fin)[2] <- "final_pos"
    d <- merge(d, fin, by = "team")
    releg <- max(d$final_pos) - 2              # bottom three positions
    d$zone <- ifelse(d$final_pos == 1, "Champion",
              ifelse(d$final_pos <= 4, "Top 4 (UCL)",
              ifelse(d$final_pos >= releg, "Relegation", "Mid-table")))
    vis  <- d[d$matchweek <= input$race_wk, ]
    mid  <- vis[vis$zone == "Mid-table", ]
    hl   <- vis[vis$zone != "Mid-table", ]
    hl$zone <- factor(hl$zone, levels = c("Champion", "Top 4 (UCL)", "Relegation"))
    ends <- hl[hl$matchweek == max(vis$matchweek), ]
    ggplot(mapping = aes(matchweek, position, group = team)) +
      geom_line(data = mid, colour = "grey82", linewidth = 0.6) +
      geom_line(data = hl, aes(colour = zone), linewidth = 1.5) +
      geom_point(data = ends, aes(colour = zone), size = 3) +
      { if (input$labels) geom_text(data = ends, aes(label = team, colour = zone),
          hjust = 0, nudge_x = 0.4, size = 3.3, fontface = "bold",
          show.legend = FALSE, check_overlap = TRUE) } +
      scale_colour_manual(values = zone_cols, name = NULL) +
      scale_y_reverse(breaks = c(1, 4, 8, 12, 16, 20), expand = expansion(mult = 0.03)) +
      scale_x_continuous(limits = c(1, max_mw + 5), breaks = c(1, 10, 20, 30, max_mw),
                         expand = expansion(mult = c(0.01, 0.02))) +
      labs(title = "Race for the title - league position by matchweek",
           subtitle = paste0("Season up to week ", input$race_wk,
                             " - champion, top 4 (UCL) and relegation zone highlighted"),
           x = "Matchweek", y = "League position", caption = CAP) +
      theme_pl()
  })

  # ---- xG analysis ----------------------------------------------------------
  output$p_xg <- renderPlot({
    d <- totals(); d$diff <- d$GF - d$xGF
    lim <- range(c(d$GF, d$xGF)) + c(-4, 6)
    p <- ggplot(d, aes(xGF, GF)) +
      geom_abline(slope = 1, intercept = 0, linetype = 2, colour = "grey70") +
      annotate("text", x = lim[1] + 3, y = lim[2] - 2, label = "Overperforming",
               hjust = 0, colour = pl_pink, fontface = "italic", size = 3.6) +
      annotate("text", x = lim[2] - 2, y = lim[1] + 3, label = "Underperforming",
               hjust = 1, colour = pl_blue, fontface = "italic", size = 3.6) +
      geom_point(aes(fill = diff), shape = 21, colour = "white",
                 size = 4.4, stroke = 0.6) +
      scale_fill_gradient2(low = pl_blue, mid = "grey80", high = pl_pink,
                           midpoint = 0, name = "Goals - xG") +
      coord_equal(xlim = lim, ylim = lim) +
      labs(title = "Finishing: goals vs expected goals",
           subtitle = paste0("Above the dashed line = scored more than expected - ",
                             wk_lbl(input$mw)),
           x = "Expected goals (xG)", y = "Goals scored", caption = CAP) +
      theme_pl()
    if (input$labels)
      p <- p + geom_text(aes(label = team), vjust = -1.1, size = 3.1,
                         colour = "grey25", check_overlap = TRUE)
    p
  })

  # ---- Team form ------------------------------------------------------------
  output$p_form <- renderPlot({
    d <- q_form(input$team)
    d <- d[d$matchweek >= input$mw[1] & d$matchweek <= input$mw[2], ]
    ggplot(d, aes(matchweek, form)) +
      geom_hline(yintercept = c(1, 2), linetype = 3, colour = "grey80") +
      geom_area(fill = pl_purple, alpha = 0.12, na.rm = TRUE) +
      geom_line(colour = pl_purple, linewidth = 1.3, na.rm = TRUE) +
      geom_point(aes(colour = form), size = 2.6, na.rm = TRUE) +
      scale_colour_gradient(low = "grey70", high = pl_pink, guide = "none") +
      scale_y_continuous(limits = c(0, 3), breaks = 0:3) +
      labs(title = paste("Form & momentum -", input$team),
           subtitle = "Rolling 5-match average points per game (computed with a SQL window function)",
           x = "Matchweek", y = "Points per game", caption = CAP) +
      theme_pl()
  })

  # ---- Home vs away (dumbbell) ---------------------------------------------
  output$p_ha <- renderPlot({
    ha <- q_homeaway(input$mw[1], input$mw[2])
    home <- ha[ha$venue == "Home", c("team", "ppg")]; names(home)[2] <- "Home"
    away <- ha[ha$venue == "Away", c("team", "ppg")]; names(away)[2] <- "Away"
    wide <- merge(home, away, by = "team")
    if (nrow(wide) == 0)
      return(ggplot() +
        annotate("text", x = 0, y = 0, size = 5, colour = "grey45",
                 label = "Widen the matchweek range to compare home and away.") +
        theme_void())
    wide <- wide[order(wide$Home + wide$Away), ]
    wide$team <- factor(wide$team, levels = wide$team)
    ggplot(wide) +
      geom_segment(aes(x = Away, xend = Home, y = team, yend = team),
                   colour = "grey80", linewidth = 1.1) +
      geom_point(aes(Away, team, colour = "Away"), size = 3.2) +
      geom_point(aes(Home, team, colour = "Home"), size = 3.2) +
      scale_colour_manual(values = c(Home = pl_purple, Away = pl_pink), name = NULL) +
      labs(title = "Home comfort vs away resilience",
           subtitle = paste0("Points per game at home and away - each line is one team - ",
                             wk_lbl(input$mw)),
           x = "Points per game", y = NULL, caption = CAP) +
      theme_pl()
  })

  # ---- Shot map -------------------------------------------------------------
  output$p_shots <- renderPlot({
    d <- q_shots(input$team)
    d$px <- d$y                     # width  -> plot x
    d$py <- d$x                     # length -> plot y (attacking upward)
    d$Outcome <- ifelse(d$goal == 1, "Goal", "No goal")
    ggplot(d, aes(px, py)) +
      pitch_layers() +
      geom_point(aes(size = xg, fill = Outcome), shape = 21,
                 colour = "white", alpha = 0.85, stroke = 0.4) +
      scale_size_continuous(range = c(1.4, 7), name = "xG") +
      scale_fill_manual(values = c(Goal = pl_pink, `No goal` = "grey55"), name = NULL) +
      coord_fixed(xlim = c(0, 80), ylim = c(60, 121)) +
      labs(title = paste("Where the chances came from -", input$team),
           subtitle = "Every shot, sized by xG - attacking towards the top",
           caption = CAP) +
      theme_pl() +
      theme(axis.text = element_blank(), axis.title = element_blank(),
            panel.grid = element_blank())
  })

  # ---- Table ----------------------------------------------------------------
  output$tbl <- renderTable({
    t <- totals()
    t$Pos <- seq_len(nrow(t))
    t[, c("Pos", "team", "P", "W", "D", "L", "GF", "GA", "GD", "xGF", "Pts")] |>
      setNames(c("Pos", "Team", "P", "W", "D", "L", "GF", "GA", "GD", "xG", "Pts"))
  }, striped = TRUE, hover = TRUE, digits = 1, width = "100%")
}

shinyApp(ui, server)
