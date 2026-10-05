# =============================================================================
# build_db.R  --  create the SQLite database  (Stage 3)
# -----------------------------------------------------------------------------
# Builds on the Stage 2 data layer. It takes the two data frames (matches, shots)
# and writes them into an SQLite file (epl.sqlite) as two tables plus a
# team_match view, which is what the Shiny app reads from.
#
#   matches     -- one row per match
#   shots       -- one row per shot
#   team_match  -- VIEW: one row per team per match, with points computed in SQL
#
# Run once:  Rscript build_db.R
# To use REAL data, set USE_REAL_DATA <- TRUE (needs the worldfootballR package).
# =============================================================================

suppressMessages({
  library(dplyr)
  library(DBI)
  library(RSQLite)
})

USE_REAL_DATA <- FALSE
SEASON_END_YR <- 2025
DB_PATH       <- "epl.sqlite"
set.seed(2025)

# -----------------------------------------------------------------------------
# 1. Optional real-data path (worldfootballR)
# -----------------------------------------------------------------------------
load_real_data <- function(season_end_year = SEASON_END_YR) {
  if (!requireNamespace("worldfootballR", quietly = TRUE)) return(NULL)

  res <- worldfootballR::fb_match_results(
    country = "ENG", gender = "M",
    season_end_year = season_end_year, tier = "1st")

  matches <- res |>
    transmute(matchweek = as.integer(Wk), date = as.Date(Date),
              home = Home, away = Away,
              home_goals = HomeGoals, away_goals = AwayGoals,
              home_xg = Home_xG, away_xg = Away_xG) |>
    filter(!is.na(home_goals))

  raw <- worldfootballR::understat_league_season_shots(
    league = "EPL", season_start_year = season_end_year - 1)
  shots <- raw |>
    transmute(team = team, x = X * 120, y = Y * 80,
              xg = xG, goal = as.integer(result == "Goal"))

  list(matches = matches, shots = shots)
}

# -----------------------------------------------------------------------------
# 2. Sample-data generator
# -----------------------------------------------------------------------------
epl_teams <- c(
  "Liverpool","Arsenal","Man City","Chelsea","Newcastle","Aston Villa",
  "Tottenham","Man Utd","Brighton","Bournemouth","Fulham","Crystal Palace",
  "Brentford","Everton","West Ham","Wolves","Nott'm Forest","Leicester",
  "Ipswich","Southampton")

strength <- setNames(seq(0.50, -0.55, length.out = length(epl_teams)),
                     epl_teams)[sample(epl_teams)]

round_robin <- function(team_ids) {
  n <- length(team_ids); v <- team_ids; out <- list()
  for (r in seq_len(n - 1)) {
    out[[r]] <- data.frame(round = r,
                           home = v[seq_len(n / 2)],
                           away = rev(v)[seq_len(n / 2)])
    v <- c(v[1], v[n], v[2:(n - 1)])
  }
  do.call(rbind, out)
}

make_sample_data <- function() {
  n  <- length(epl_teams)
  rr <- round_robin(epl_teams)
  rr2 <- transform(rr, home = away, away = home, round = round + (n - 1))
  sched <- rbind(rr, rr2)

  home_adv <- 0.25
  matches <- sched |>
    mutate(
      lam_h = 1.35 * exp(strength[home] - strength[away] + home_adv),
      lam_a = 1.35 * exp(strength[away] - strength[home] - home_adv),
      home_xg = round(lam_h * runif(n(), 0.75, 1.25), 2),
      away_xg = round(lam_a * runif(n(), 0.75, 1.25), 2),
      home_goals = rpois(n(), lam_h),
      away_goals = rpois(n(), lam_a),
      matchweek  = round,
      date = as.character(as.Date("2024-08-16") + (round - 1) * 7)) |>
    select(matchweek, date, home, away,
           home_goals, away_goals, home_xg, away_xg)

  shots <- lapply(epl_teams, function(t) {
    k  <- rpois(1, 300 + 120 * (strength[t] + 0.8))
    x  <- pmin(119, 120 - abs(rnorm(k, 18, 10)))
    y  <- pmin(79, pmax(1, rnorm(k, 40, 14)))
    d  <- sqrt((120 - x)^2 + (40 - y)^2)
    xg <- pmax(0.02, pmin(0.9, exp(-d / 12) + rnorm(k, 0, 0.03)))
    data.frame(team = t, x = x, y = y, xg = round(xg, 3),
               goal = as.integer(runif(k) < xg))
  }) |> bind_rows()

  list(matches = matches, shots = shots)
}

# -----------------------------------------------------------------------------
# 3. Choose the data source
# -----------------------------------------------------------------------------
dat <- if (USE_REAL_DATA) {
  d <- tryCatch(load_real_data(), error = function(e) NULL)
  if (is.null(d)) { message("Real data unavailable; using sample data."); make_sample_data() }
  else d
} else make_sample_data()

# -----------------------------------------------------------------------------
# 4. Write the SQLite database
# -----------------------------------------------------------------------------
if (file.exists(DB_PATH)) file.remove(DB_PATH)      # clean rebuild
con <- dbConnect(SQLite(), DB_PATH)

dbWriteTable(con, "matches", dat$matches, overwrite = TRUE)
dbWriteTable(con, "shots",   dat$shots,   overwrite = TRUE)

# A view giving one row per team per match, with league points computed in SQL.
dbExecute(con, "DROP VIEW IF EXISTS team_match;")
dbExecute(con, "
  CREATE VIEW team_match AS
  SELECT matchweek, home AS team, away AS opponent,
         home_goals AS gf, away_goals AS ga, home_xg AS xg, away_xg AS xga,
         'Home' AS venue,
         CASE WHEN home_goals > away_goals THEN 3
              WHEN home_goals = away_goals THEN 1 ELSE 0 END AS points
  FROM matches
  UNION ALL
  SELECT matchweek, away AS team, home AS opponent,
         away_goals AS gf, home_goals AS ga, away_xg AS xg, home_xg AS xga,
         'Away' AS venue,
         CASE WHEN away_goals > home_goals THEN 3
              WHEN away_goals = home_goals THEN 1 ELSE 0 END AS points
  FROM matches;")

# Index to speed up the app's per-team shot look-ups.
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_shots_team ON shots(team);")

# -----------------------------------------------------------------------------
# 5. Verify and close
# -----------------------------------------------------------------------------
cat("Tables/views:", paste(dbListTables(con), collapse = ", "), "\n")
cat("matches rows:   ", dbGetQuery(con, "SELECT COUNT(*) n FROM matches")$n, "\n")
cat("shots rows:     ", dbGetQuery(con, "SELECT COUNT(*) n FROM shots")$n, "\n")
cat("team_match rows:", dbGetQuery(con, "SELECT COUNT(*) n FROM team_match")$n, "\n")

dbDisconnect(con)
cat("Database written to", DB_PATH, "\n")
