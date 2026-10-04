# =============================================================================
# build_db.R  --  data layer  (Stage 2)
# -----------------------------------------------------------------------------
# Produces the two data frames the whole project is built on:
#   * matches  -- one row per match (results + expected goals)
#   * shots    -- one row per shot  (location, xG, goal flag)
#
# The data is either generated as realistic SAMPLE data (default) so the project
# runs with no network, or downloaded from FBref / Understat via worldfootballR.
#
# NOTE: At this stage the script only CREATES the data and prints a summary.
# Writing it into the SQLite database (tables, team_match view, index) is added
# in Stage 3.
#
# Run:  Rscript build_db.R
# =============================================================================

suppressMessages({
  library(dplyr)
})

USE_REAL_DATA <- FALSE     # flip to TRUE to download real data (needs worldfootballR)
SEASON_END_YR <- 2025      # 2024/25 season
set.seed(2025)             # reproducible sample data

# -----------------------------------------------------------------------------
# 1. Optional real-data path (worldfootballR)
# -----------------------------------------------------------------------------
# Returns list(matches, shots) from FBref + Understat, or NULL if the package
# is not installed so the caller can fall back to sample data.
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
  # Understat column names can vary slightly by version; adjust if needed.
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

# relative attacking strength; kept in a modest range so season goal totals are
# realistic. sample() shuffles which club gets which value.
strength <- setNames(seq(0.50, -0.55, length.out = length(epl_teams)),
                     epl_teams)[sample(epl_teams)]

# Build a single round-robin schedule with the classic "circle method":
# one team is held fixed while the others rotate, so every team plays exactly
# once per round. 20 teams -> 19 rounds.
round_robin <- function(team_ids) {
  n <- length(team_ids); v <- team_ids; out <- list()
  for (r in seq_len(n - 1)) {
    out[[r]] <- data.frame(round = r,
                           home = v[seq_len(n / 2)],
                           away = rev(v)[seq_len(n / 2)])
    v <- c(v[1], v[n], v[2:(n - 1)])          # rotate all but the first
  }
  do.call(rbind, out)
}

make_sample_data <- function() {
  n  <- length(epl_teams)
  rr <- round_robin(epl_teams)                           # first half (19 rounds)
  rr2 <- transform(rr, home = away, away = home,         # second half: swap venues
                   round = round + (n - 1))
  sched <- rbind(rr, rr2)                                # 38 matchweeks, 380 matches

  home_adv <- 0.25
  matches <- sched |>
    mutate(
      lam_h = 1.35 * exp(strength[home] - strength[away] + home_adv),
      lam_a = 1.35 * exp(strength[away] - strength[home] - home_adv),
      home_xg = round(lam_h * runif(n(), 0.75, 1.25), 2),
      away_xg = round(lam_a * runif(n(), 0.75, 1.25), 2),
      home_goals = rpois(n(), lam_h),                    # goals ~ Poisson(rate)
      away_goals = rpois(n(), lam_a),
      matchweek  = round,
      date = as.character(as.Date("2024-08-16") + (round - 1) * 7)) |>
    select(matchweek, date, home, away,
           home_goals, away_goals, home_xg, away_xg)

  # synthetic shots: more for stronger teams, clustered near goal, xG falls off
  # with distance, and a goal is drawn at random with probability = xG.
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
# 3. Produce the data frames
# -----------------------------------------------------------------------------
dat <- if (USE_REAL_DATA) {
  d <- tryCatch(load_real_data(), error = function(e) NULL)
  if (is.null(d)) { message("Real data unavailable; using sample data."); make_sample_data() }
  else d
} else make_sample_data()

matches <- dat$matches
shots   <- dat$shots

# -----------------------------------------------------------------------------
# 4. Quick summary (sanity check for this stage)
# -----------------------------------------------------------------------------
cat(sprintf("matches: %d rows x %d cols\n", nrow(matches), ncol(matches)))
cat(sprintf("shots:   %d rows x %d cols\n", nrow(shots), ncol(shots)))
cat("\nFirst matches:\n");  print(head(matches, 3))
cat("\nFirst shots:\n");    print(head(shots, 3))
