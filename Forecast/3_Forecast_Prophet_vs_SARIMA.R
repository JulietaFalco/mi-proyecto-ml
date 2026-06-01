# ================================================================
#  SARIMA vs Prophet — Model Comparison ONLY (no file output)
#  Train : all years except 2025
#  Test  : 2025 (12 months holdout)
#  Metrics: MAE, RMSE, R², AIC, BIC
# ================================================================

# ── PACKAGES ────────────────────────────────────────────────────
required <- c("readxl", "forecast", "prophet", "dplyr", "tidyr", "lubridate")
for (pkg in required) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, repos = "https://cran.r-project.org")
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}
cat("✅ Packages ready\n\n")

# ── DATA ────────────────────────────────────────────────────────
path_input <- "C:/Users/Usuario/Documents/Capstone_Project/Dataset_weather_Prophet_vs_SARIMA.xlsx"

raw <- read_excel(path_input) %>% filter(!is.na(station_name))
cat("📂", nrow(raw), "rows |", n_distinct(raw$station_name),
    "stations | Years:", min(raw$Year, na.rm=TRUE), "–", max(raw$Year, na.rm=TRUE), "\n\n")

# ── VARIABLE DEFINITIONS ─────────────────────────────────────────
var_groups <- list(
  AirTempAvg  = list(pattern = "AirTempAvg_month_%d",  additive = TRUE),
  Rainfall    = list(pattern = "Rainfall_month_%d",    additive = FALSE),
  Radiation   = list(pattern = "Radiation_month_%d",   additive = FALSE),
  WindAvg     = list(pattern = "WindAvg_month_%d",     additive = FALSE),
  Evaporation = list(pattern = "Evaporation_month_%d", additive = FALSE),
  RelHumAvg   = list(pattern = "RelHumAvg_month_%d",   additive = FALSE),
  Tmax        = list(pattern = "Tmax_%d",              additive = TRUE),
  Tmin        = list(pattern = "Tmin_%d",              additive = TRUE)
)

non_negative <- c("Rainfall", "Radiation", "WindAvg", "Evaporation", "RelHumAvg")

# ── HELPER: build monthly series from wide format ────────────────
build_series <- function(station_df, pattern) {
  rows <- list()
  for (i in seq_len(nrow(station_df))) {
    yr <- as.integer(station_df$Year[i])
    for (m in 1:12) {
      col <- sprintf(pattern, m)
      val <- if (col %in% names(station_df)) as.numeric(station_df[[col]][i]) else NA
      rows[[length(rows)+1]] <- data.frame(
        date  = as.Date(sprintf("%d-%02d-01", yr, m)),
        value = val)
    }
  }
  bind_rows(rows) %>% arrange(date) %>% filter(!is.na(value))
}

# ── METRICS ──────────────────────────────────────────────────────
mae  <- function(a, p) mean(abs(a - p), na.rm = TRUE)
rmse <- function(a, p) sqrt(mean((a - p)^2, na.rm = TRUE))
r2   <- function(a, p) {
  ss_res <- sum((a - p)^2, na.rm = TRUE)
  ss_tot <- sum((a - mean(a, na.rm = TRUE))^2, na.rm = TRUE)
  if (ss_tot == 0) return(NA)
  1 - ss_res / ss_tot
}

# ── SARIMA FIT ───────────────────────────────────────────────────
fit_sarima <- function(train_ts, h, additive, var_name) {

  # Safe lambda: avoid Guerrero failure on series with zeros
  lambda <- if (additive) {
    NULL
  } else if (min(train_ts, na.rm = TRUE) <= 0) {
    0       # log transform — safe when zeros present (e.g. Rainfall)
  } else {
    "auto"
  }

  model <- tryCatch(
    auto.arima(train_ts,
               seasonal = TRUE, stepwise = FALSE, approximation = FALSE,
               lambda = lambda,
               max.p = 3, max.q = 3, max.P = 2, max.Q = 2,
               max.d = 2,  max.D = 1),
    error = function(e) {
      # fallback: stepwise=TRUE, no lambda
      auto.arima(train_ts, seasonal = TRUE)
    }
  )

  fc     <- forecast(model, h = h, level = 95)
  values <- as.numeric(fc$mean)
  if (var_name %in% non_negative) values <- pmax(values, 0)

  list(forecast  = values,
       aic       = round(AIC(model), 2),
       bic       = round(BIC(model), 2),
       order_str = paste0("ARIMA(",
                          paste(arimaorder(model)[1:3], collapse=","), ")(",
                          paste(arimaorder(model)[4:6], collapse=","), ")[12]"))
}

# ── PROPHET FIT ──────────────────────────────────────────────────
fit_prophet <- function(train_df, h, additive, var_name) {

  seas_mode <- if (additive) "additive" else "multiplicative"

  m <- suppressMessages(
    prophet(train_df,
            seasonality.mode   = seas_mode,
            yearly.seasonality = TRUE,
            weekly.seasonality = FALSE,
            daily.seasonality  = FALSE,
            interval.width     = 0.95)
  )

  future <- make_future_dataframe(m, periods = h, freq = "month")
  fc     <- predict(m, future)
  values <- tail(fc$yhat, h)

  if (var_name %in% non_negative) values <- pmax(values, 0)
  list(forecast = values)
}

# ── MAIN COMPARISON LOOP ─────────────────────────────────────────
stations   <- sort(unique(raw$station_name))
val_year   <- 2025
results    <- list()

for (station in stations) {

  cat("📍", station, "\n")

  df_st <- raw %>% filter(station_name == station) %>% arrange(Year)

  for (var_name in names(var_groups)) {

    cfg    <- var_groups[[var_name]]
    full   <- build_series(df_st, cfg$pattern)
    train  <- full %>% filter(year(date) < val_year)
    test   <- full %>% filter(year(date) == val_year)

    # Need at least 3 years train + 6 months test
    if (nrow(train) < 36 || nrow(test) < 6) next

    actual    <- test$value
    start_yr  <- year(min(train$date))
    start_mo  <- month(min(train$date))
    train_ts  <- ts(train$value, start = c(start_yr, start_mo), frequency = 12)

    # ── SARIMA ───────────────────────────────────────────────────
    sar <- tryCatch(
      fit_sarima(train_ts, nrow(test), cfg$additive, var_name),
      error = function(e) { cat("  ⚠️ SARIMA failed:", var_name, "\n"); NULL }
    )

    # ── PROPHET ──────────────────────────────────────────────────
    pro_train <- data.frame(ds = train$date, y = train$value)
    pro <- tryCatch(
      fit_prophet(pro_train, nrow(test), cfg$additive, var_name),
      error = function(e) { cat("  ⚠️ Prophet failed:", var_name, "\n"); NULL }
    )

    # ── COLLECT METRICS ──────────────────────────────────────────
    row <- data.frame(Station = station, Variable = var_name,
                      stringsAsFactors = FALSE)

    if (!is.null(sar)) {
      row$SARIMA_MAE   <- round(mae(actual,  sar$forecast), 3)
      row$SARIMA_RMSE  <- round(rmse(actual, sar$forecast), 3)
      row$SARIMA_R2    <- round(r2(actual,   sar$forecast), 3)
      row$SARIMA_AIC   <- sar$aic
      row$SARIMA_BIC   <- sar$bic
      row$SARIMA_Order <- sar$order_str
    } else {
      row[, c("SARIMA_MAE","SARIMA_RMSE","SARIMA_R2",
              "SARIMA_AIC","SARIMA_BIC","SARIMA_Order")] <- NA
    }

    if (!is.null(pro)) {
      row$Prophet_MAE  <- round(mae(actual,  pro$forecast), 3)
      row$Prophet_RMSE <- round(rmse(actual, pro$forecast), 3)
      row$Prophet_R2   <- round(r2(actual,   pro$forecast), 3)
    } else {
      row[, c("Prophet_MAE","Prophet_RMSE","Prophet_R2")] <- NA
    }

    row$Best <- if (!is.na(row$SARIMA_RMSE) & !is.na(row$Prophet_RMSE)) {
      if (row$SARIMA_RMSE <= row$Prophet_RMSE) "SARIMA" else "Prophet"
    } else if (!is.na(row$SARIMA_RMSE)) "SARIMA" else "Prophet"

    cat(sprintf("  %-12s SARIMA: %6.3f | Prophet: %6.3f → %s\n",
                var_name,
                ifelse(is.na(row$SARIMA_RMSE),  999, row$SARIMA_RMSE),
                ifelse(is.na(row$Prophet_RMSE), 999, row$Prophet_RMSE),
                row$Best))

    results[[length(results)+1]] <- row
  }
}

# ── SUMMARY ──────────────────────────────────────────────────────
comparison_df <- bind_rows(results)

summary_df <- comparison_df %>%
  group_by(Variable) %>%
  summarise(
    N                = n(),
    SARIMA_Wins      = sum(Best == "SARIMA",  na.rm = TRUE),
    Prophet_Wins     = sum(Best == "Prophet", na.rm = TRUE),
    Avg_SARIMA_RMSE  = round(mean(SARIMA_RMSE,  na.rm = TRUE), 3),
    Avg_Prophet_RMSE = round(mean(Prophet_RMSE, na.rm = TRUE), 3),
    Avg_SARIMA_R2    = round(mean(SARIMA_R2,    na.rm = TRUE), 3),
    Avg_Prophet_R2   = round(mean(Prophet_R2,   na.rm = TRUE), 3),
    Winner           = ifelse(mean(SARIMA_RMSE,  na.rm = TRUE) <=
                              mean(Prophet_RMSE, na.rm = TRUE),
                              "SARIMA", "Prophet"),
    .groups = "drop"
  )

cat("\n══════════════════════════════════════════════════════════\n")
cat("  FINAL SUMMARY — Average RMSE across all stations\n")
cat("══════════════════════════════════════════════════════════\n")
print(summary_df %>%
        select(Variable, SARIMA_Wins, Prophet_Wins,
               Avg_SARIMA_RMSE, Avg_Prophet_RMSE,
               Avg_SARIMA_R2, Avg_Prophet_R2, Winner),
      row.names = FALSE)

cat("\n📊 Overall wins:\n")
cat("  SARIMA  wins:", sum(comparison_df$Best == "SARIMA",  na.rm=TRUE),
    "out of", nrow(comparison_df), "\n")
cat("  Prophet wins:", sum(comparison_df$Best == "Prophet", na.rm=TRUE),
    "out of", nrow(comparison_df), "\n")
cat("\n✅ Done. 'comparison_df' and 'summary_df' are in memory.\n")
cat("   Run View(summary_df) or View(comparison_df) to inspect.\n")
