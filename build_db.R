# =============================================================================
# build_db.R  --  create the SQLite database used by the Shiny app
# -----------------------------------------------------------------------------
# Builds epl.sqlite with two tables (matches, shots) and a team_match view.
#
#   USE_REAL_DATA = TRUE   -> download the REAL season from FBref / Understat
#                             via the worldfootballR package.
#   USE_REAL_DATA = FALSE  -> generate realistic SAMPLE data (runs offline).
#
# Run once:  Rscript build_db.R
# =============================================================================

suppressMessages({
  library(dplyr)
  library(DBI)
  library(RSQLite)
})

USE_REAL_DATA <- TRUE          # TRUE = real data (needs worldfootballR + internet)
SEASON_END_YR <- 2025          # 2024/25 season
DB_PATH       <- "epl.sqlite"
set.seed(2025)                 # only affects the sample data

# -----------------------------------------------------------------------------
# 1. Real-data path (worldfootballR -> FBref + Understat)
# -----------------------------------------------------------------------------
load_real_data <- function(season_end_year = SEASON_END_YR) {
  if (!requireNamespace("worldfootballR", quietly = TRUE))
    stop("the 'worldfootballR' package is not installed. ",
         "Run install.packages(\"worldfootballR\") and try again.")

  # FBref actively blocks live scraping (HTTP 403), so use the CACHED loader,
  # which reads pre-collected data from worldfootballR's GitHub data repo.
  # Fall back to a live scrape only if the cached loader is unavailable.
  message("Loading match results (cached FBref data from GitHub)...")
  res <- tryCatch(
    worldfootballR::load_match_results(country = "ENG", gender = "M",
                                       season_end_year = season_end_year, tier = "1st"),
    error = function(e) {
      message("  cached loader failed (", conditionMessage(e),
              "); trying a live FBref scrape...")
      worldfootballR::fb_match_results(country = "ENG", gender = "M",
                                       season_end_year = season_end_year, tier = "1st")
    })

  # xG columns are required by the xG chart; keep them if present, else NA.
  need_m <- c("Wk", "Date", "Home", "Away", "HomeGoals", "AwayGoals")
  miss_m <- setdiff(need_m, names(res))
  if (length(miss_m))
    stop("match results are missing expected columns: ",
         paste(miss_m, collapse = ", "),
         ". Columns returned: ", paste(names(res), collapse = ", "))
  has_xg <- all(c("Home_xG", "Away_xG") %in% names(res))
  if (!has_xg) message("  note: no xG columns in the match data; xG set to NA.")

  matches <- res |>
    transmute(matchweek  = as.integer(Wk),
              date       = as.character(as.Date(Date)),
              home       = Home,
              away       = Away,
              home_goals = as.integer(HomeGoals),
              away_goals = as.integer(AwayGoals),
              home_xg    = if (has_xg) as.numeric(Home_xG) else NA_real_,
              away_xg    = if (has_xg) as.numeric(Away_xG) else NA_real_) |>
    filter(!is.na(home_goals), !is.na(matchweek))

  # Understat (shots) is not the source that blocks you; try it live first,
  # then fall back to the cached Understat loader.
  message("Loading shot data from Understat...")
  raw <- tryCatch(
    worldfootballR::understat_league_season_shots(
      league = "EPL", season_start_year = season_end_year - 1),
    error = function(e) {
      message("  live Understat failed (", conditionMessage(e),
              "); trying the cached Understat loader...")
      s <- worldfootballR::load_understat_league_shots(league = "EPL")
      if ("season" %in% names(s)) s <- s[s$season == season_end_year - 1, ]
      s
    })

  # The shooting team is not a single column: it is the home team when the
  # shot is a home shot and the away team otherwise. Column names differ
  # between worldfootballR versions (home_team/away_team vs h_team/a_team,
  # and h_a vs home_away), so detect whichever is present.
  pick <- function(cands) { hit <- cands[cands %in% names(raw)]; if (length(hit)) hit[1] else NA }
  home_col <- pick(c("home_team", "h_team"))
  away_col <- pick(c("away_team", "a_team"))
  ha_col   <- pick(c("h_a", "home_away"))
  need_s   <- c("X", "Y", "xG", "result")
  miss_s   <- setdiff(need_s, names(raw))
  if (is.na(home_col) || is.na(away_col) || is.na(ha_col) || length(miss_s))
    stop("Understat shots are missing expected columns",
         if (length(miss_s)) paste0(" (", paste(miss_s, collapse = ", "), ")") else "",
         if (is.na(home_col) || is.na(away_col) || is.na(ha_col)) " (team/home-away columns)" else "",
         ". Columns returned: ", paste(names(raw), collapse = ", "))

  is_home <- raw[[ha_col]] %in% c("h", "home", "Home", "H", "HOME")
  shots <- data.frame(
    team = ifelse(is_home, raw[[home_col]], raw[[away_col]]),
    x    = as.numeric(raw$X) * 120,
    y    = as.numeric(raw$Y) * 80,
    xg   = as.numeric(raw$xG),
    goal = as.integer(raw$result == "Goal"),
    stringsAsFactors = FALSE)
  shots <- shots[!is.na(shots$team) & !is.na(shots$x) & !is.na(shots$y), ]

  if (nrow(matches) < 300)
    stop("only ", nrow(matches), " matches returned - the season may be ",
         "incomplete or the download was partial.")

  message("Real data OK: ", nrow(matches), " matches, ", nrow(shots), " shots.")
  list(matches = matches, shots = shots)
}

# -----------------------------------------------------------------------------
# 2. Sample-data generator (offline fallback / demonstration)
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
    select(matchweek, date, home, away, home_goals, away_goals, home_xg, away_xg)

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
# 3. Choose the data source (loud about which one is used)
# -----------------------------------------------------------------------------
if (USE_REAL_DATA) {
  dat <- tryCatch(load_real_data(),
                  error = function(e) {
                    message("\n*** REAL DATA FETCH FAILED ***")
                    message("Reason: ", conditionMessage(e))
                    message("Falling back to SAMPLE data - the numbers will NOT ",
                            "match the real season.\n")
                    NULL
                  })
  if (is.null(dat)) dat <- make_sample_data()
  else message("Using REAL data from FBref / Understat.\n")
} else {
  message("USE_REAL_DATA is FALSE - using SAMPLE data.\n")
  dat <- make_sample_data()
}

# -----------------------------------------------------------------------------
# 4. Write the SQLite database
# -----------------------------------------------------------------------------
if (file.exists(DB_PATH)) file.remove(DB_PATH)
con <- dbConnect(SQLite(), DB_PATH)

dbWriteTable(con, "matches", dat$matches, overwrite = TRUE)
dbWriteTable(con, "shots",   dat$shots,   overwrite = TRUE)

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

dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_shots_team ON shots(team);")

# -----------------------------------------------------------------------------
# 5. Verify + sanity check (so you can see at a glance which data you got)
# -----------------------------------------------------------------------------
cat("Tables/views:  ", paste(dbListTables(con), collapse = ", "), "\n")
cat("matches rows:  ", dbGetQuery(con, "SELECT COUNT(*) n FROM matches")$n, "\n")
cat("shots rows:    ", dbGetQuery(con, "SELECT COUNT(*) n FROM shots")$n, "\n")

top <- dbGetQuery(con, "
  SELECT team, SUM(points) pts FROM team_match
  GROUP BY team ORDER BY pts DESC, SUM(gf - ga) DESC LIMIT 2")
cat(sprintf("Top of table:   1. %s (%d pts)   2. %s (%d pts)\n",
            top$team[1], top$pts[1], top$team[2], top$pts[2]))
cat("  (Real 2024/25 should read: 1. Liverpool (84)  2. Arsenal (74).)\n")

dbDisconnect(con)
cat("Database written to", DB_PATH, "\n")
