# ================================================================
#  PROPHET FORECAST — Final Script (No Validation Block)
#  Model    : Prophet (selected over SARIMA — lower RMSE overall)
#  Train    : all years up to 2025
#  Forecast : Jan 2026 – Dec 2026 (12 months)
#  Outputs  : Excel files + 3 plot types (~20 min runtime)
# ================================================================

# ── 0. PACKAGES ─────────────────────────────────────────────────
required <- c("readxl", "writexl", "prophet", "dplyr", "tidyr",
              "lubridate", "ggplot2", "scales", "ggrepel")

for (pkg in required) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, repos = "https://cran.r-project.org")
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}
cat("✅ Packages loaded\n\n")

# ── 1. PATHS ────────────────────────────────────────────────────
base_path   <- "C:/Users/Usuario/Documents/Capstone_Project/Forecast/"
path_data   <- paste0(base_path, "Dataset_weather_Prophet_vs_SARIMA.xlsx")
path_coords <- paste0(base_path, "stations latitude longitude and cluster.xlsx")
path_out_fc <- paste0(base_path, "Prophet_Forecast_2026_v1.xlsx")
path_plots  <- paste0(base_path, "Plots/")

dir.create(path_plots, showWarnings = FALSE)

# ── 2. LOAD DATA ─────────────────────────────────────────────────
raw    <- read_excel(path_data) %>% filter(!is.na(station_name))
coords <- read_excel(path_coords) %>%
  distinct(station_name, .keep_all = TRUE) %>%
  select(station_name, LONGITUDE, LATITUDE, ABARES_region, Cluster)

cat("📂 Weather data:", nrow(raw), "rows |",
    n_distinct(raw$station_name), "stations | Years:",
    min(raw$Year, na.rm = TRUE), "–", max(raw$Year, na.rm = TRUE), "\n\n")

# ── 3. VARIABLE DEFINITIONS ──────────────────────────────────────
var_groups <- list(
  AirTempAvg  = list(pattern = "AirTempAvg_month_%d",  additive = TRUE,  unit = "°C"),
  Rainfall    = list(pattern = "Rainfall_month_%d",    additive = FALSE, unit = "mm"),
  Radiation   = list(pattern = "Radiation_month_%d",   additive = FALSE, unit = "kJ/m²"),
  WindAvg     = list(pattern = "WindAvg_month_%d",     additive = FALSE, unit = "km/h"),
  Evaporation = list(pattern = "Evaporation_month_%d", additive = FALSE, unit = "mm"),
  RelHumAvg   = list(pattern = "RelHumAvg_month_%d",   additive = FALSE, unit = "%"),
  Tmax        = list(pattern = "Tmax_%d",              additive = TRUE,  unit = "°C"),
  Tmin        = list(pattern = "Tmin_%d",              additive = TRUE,  unit = "°C")
)

non_negative <- c("Rainfall", "Radiation", "WindAvg", "Evaporation", "RelHumAvg")
month_order  <- c("Jan","Feb","Mar","Apr","May","Jun",
                  "Jul","Aug","Sep","Oct","Nov","Dec")

# ── 4. HELPERS ───────────────────────────────────────────────────
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
  list(model = m, forecast = fc)
}

# ── 5. MAIN FORECAST LOOP ────────────────────────────────────────
stations   <- sort(unique(raw$station_name))
forecast_h <- 12
all_fc     <- list()
total      <- length(stations) * length(var_groups)
counter    <- 0

cat("🚀 Starting forecast — 144 Prophet models\n")
cat("   Estimated time: ~20 minutes\n\n")
start_time <- Sys.time()

for (station in stations) {

  cat("📍", station, "\n")
  df_st   <- raw %>% filter(station_name == station) %>% arrange(Year)
  st_info <- coords %>% filter(station_name == station)
  cluster <- if (nrow(st_info) > 0) st_info$Cluster[1] else "C1"

  for (var_name in names(var_groups)) {

    counter <- counter + 1
    cfg     <- var_groups[[var_name]]
    full    <- build_series(df_st, cfg$pattern)

    if (nrow(full) < 36) {
      cat(sprintf("  ⚠️  %-12s — insufficient data\n", var_name))
      next
    }

    full_train <- data.frame(ds = full$date, y = full$value)

    fc_res <- tryCatch(
      fit_prophet(full_train, forecast_h, cfg$additive, var_name),
      error = function(e) {
        cat(sprintf("  ❌ %-12s — %s\n", var_name, e$message))
        NULL
      }
    )

    if (!is.null(fc_res)) {
      fc_2026 <- tail(fc_res$forecast, forecast_h)
      fc_vals <- pmax(fc_2026$yhat,
                      if (var_name %in% non_negative) 0 else -Inf)
      fc_lo   <- pmax(fc_2026$yhat_lower,
                      if (var_name %in% non_negative) 0 else -Inf)

      for (k in 1:forecast_h) {
        all_fc[[length(all_fc)+1]] <- data.frame(
          Station    = station,
          Variable   = var_name,
          Unit       = cfg$unit,
          Cluster    = cluster,
          Date       = as.Date(fc_2026$ds[k]),
          Month      = month(fc_2026$ds[k]),
          Month_Name = format(as.Date(fc_2026$ds[k]), "%b"),
          Forecast   = round(fc_vals[k], 3),
          Lower_95   = round(fc_lo[k], 3),
          Upper_95   = round(fc_2026$yhat_upper[k], 3)
        )
      }

      elapsed <- round(as.numeric(difftime(Sys.time(), start_time, units="mins")), 1)
      cat(sprintf("  ✅ %-12s  [%d/%d | %.1f min elapsed]\n",
                  var_name, counter, total, elapsed))
    }
  }
}

# ── 6. COMPILE & EXPORT EXCEL ────────────────────────────────────
fc_df <- bind_rows(all_fc)

fc_wide <- fc_df %>%
  select(Station, Variable, Unit, Cluster, Month_Name, Forecast) %>%
  pivot_wider(names_from = Month_Name, values_from = Forecast) %>%
  select(Station, Variable, Unit, Cluster, any_of(month_order))

write_xlsx(
  list("Forecast_2026_Wide" = fc_wide,
       "Forecast_2026_Long" = fc_df),
  path_out_fc
)
cat("\n✅ Excel saved:", path_out_fc, "\n\n")

# ════════════════════════════════════════════════════════════════
#  PLOT 1 — RMSE Comparison (SARIMA vs Prophet)
#  Requires comparison_df from Model_Comparison_Only.R session
# ════════════════════════════════════════════════════════════════
if (exists("comparison_df")) {

  summary_plot <- comparison_df %>%
    group_by(Variable) %>%
    summarise(
      SARIMA  = mean(SARIMA_RMSE,  na.rm = TRUE),
      Prophet = mean(Prophet_RMSE, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_longer(c(SARIMA, Prophet), names_to = "Model", values_to = "Avg_RMSE")

  p_cmp <- ggplot(summary_plot,
                  aes(x = reorder(Variable, Avg_RMSE),
                      y = Avg_RMSE, fill = Model)) +
    geom_col(position = "dodge", width = 0.65, alpha = 0.9) +
    scale_fill_manual(values = c(SARIMA = "#C00000", Prophet = "#1F4E79")) +
    coord_flip() +
    labs(title    = "Model Comparison — Average RMSE by Variable (Validation 2025)",
         subtitle = "Lower is better  |  Prophet selected as forecasting model",
         x = NULL, y = "Average RMSE across 18 stations",
         fill = "Model") +
    theme_minimal(base_size = 12) +
    theme(plot.title         = element_text(face = "bold", size = 13),
          plot.subtitle      = element_text(color = "grey40"),
          legend.position    = "bottom",
          panel.grid.major.y = element_blank())

  ggsave(paste0(path_plots, "Plot1_RMSE_Comparison.png"),
         p_cmp, width = 10, height = 6, dpi = 150)
  cat("📊 Plot 1 saved: RMSE Comparison\n")

} else {
  cat("⚠️  Plot 1 skipped — run Model_Comparison_Only.R first\n")
}

# ════════════════════════════════════════════════════════════════
#  PLOT 2 — Forecast 2026 with 95% Uncertainty Intervals
#  One PNG per variable, grid of all 18 stations
# ════════════════════════════════════════════════════════════════
cat("\n📊 Generating forecast plots...\n")

for (var_name in names(var_groups)) {

  df_fc <- fc_df %>%
    filter(Variable == var_name) %>%
    mutate(Month_Name = factor(Month_Name, levels = month_order))

  if (nrow(df_fc) == 0) next

  unit <- var_groups[[var_name]]$unit

  p_fc <- ggplot(df_fc, aes(x = Month_Name, group = 1)) +
    geom_ribbon(aes(ymin = Lower_95, ymax = Upper_95),
                fill = "#1F4E79", alpha = 0.2) +
    geom_line(aes(y = Forecast), color = "#1F4E79", linewidth = 1) +
    geom_point(aes(y = Forecast, color = Cluster), size = 2.5) +
    scale_color_manual(values = c(C1 = "#1F4E79", C2 = "#C00000"),
                       labels = c(C1 = "Cluster 1", C2 = "Cluster 2")) +
    facet_wrap(~Station, ncol = 3, scales = "free_y") +
    labs(title    = paste0("Prophet Forecast 2026 — ", var_name, " (", unit, ")"),
         subtitle = "Line = forecast  |  Band = 95% prediction interval  |  Dot color = cluster",
         x = NULL, y = paste0(var_name, " (", unit, ")"),
         color = "Cluster") +
    theme_minimal(base_size = 10) +
    theme(plot.title       = element_text(face = "bold", size = 12),
          plot.subtitle    = element_text(color = "grey40", size = 9),
          strip.text       = element_text(face = "bold", size = 8),
          axis.text.x      = element_text(angle = 45, hjust = 1),
          legend.position  = "bottom",
          panel.grid.minor = element_blank())

  fname <- paste0(path_plots, "Plot2_Forecast2026_", var_name, ".png")
  ggsave(fname, p_fc, width = 14, height = 10, dpi = 150)
  cat("   ✅", var_name, "\n")
}

# ════════════════════════════════════════════════════════════════
#  PLOT 3 — Station Map colored by Cluster
# ════════════════════════════════════════════════════════════════
station_coords <- coords %>%
  filter(station_name %in% stations) %>%
  distinct(station_name, .keep_all = TRUE) %>%
  mutate(Cluster = factor(Cluster, levels = c("C1", "C2")))

p_map <- ggplot(station_coords,
                aes(x = LONGITUDE, y = LATITUDE,
                    color = Cluster, shape = Cluster)) +
  geom_point(size = 4.5, alpha = 0.9) +
  geom_text(aes(label = station_name),
            size = 2.8, vjust = -0.9, hjust = 0.5,
            show.legend = FALSE) +
  scale_color_manual(values = c(C1 = "#1F4E79", C2 = "#C00000"),
                     labels = c(C1 = "Cluster 1", C2 = "Cluster 2")) +
  scale_shape_manual(values = c(C1 = 16, C2 = 17),
                     labels = c(C1 = "Cluster 1", C2 = "Cluster 2")) +
  coord_quickmap() +
  labs(title    = "Weather Station Network — Western Australia Wheatbelt",
       subtitle = paste0(nrow(station_coords),
                         " stations  |  Colored and shaped by cluster"),
       x = "Longitude", y = "Latitude",
       color = "Cluster", shape = "Cluster") +
  theme_minimal(base_size = 11) +
  theme(plot.title       = element_text(face = "bold", size = 13),
        plot.subtitle    = element_text(color = "grey40"),
        legend.position  = "bottom",
        panel.grid.major = element_line(color = "grey90"))

ggsave(paste0(path_plots, "Plot3_Station_Map.png"),
       p_map, width = 10, height = 10, dpi = 150)
cat("📊 Plot 3 saved: Station map\n")

# ── 7. FINAL SUMMARY ─────────────────────────────────────────────
total_time <- round(as.numeric(difftime(Sys.time(), start_time, units = "mins")), 1)

cat("\n══════════════════════════════════════════════════════════\n")
cat("  FORECAST SUMMARY — Prophet 2026\n")
cat("══════════════════════════════════════════════════════════\n")

fc_summary <- fc_df %>%
  group_by(Variable, Unit) %>%
  summarise(Min  = round(min(Forecast,  na.rm = TRUE), 2),
            Mean = round(mean(Forecast, na.rm = TRUE), 2),
            Max  = round(max(Forecast,  na.rm = TRUE), 2),
            .groups = "drop")
print(fc_summary, n = Inf)

cat("\n📁 Files saved:\n")
cat("  →", path_out_fc, "\n")
cat("  →", paste0(path_plots, "Plot1_RMSE_Comparison.png"), "\n")
cat("  →", paste0(path_plots, "Plot2_Forecast2026_[variable].png  (8 files)"), "\n")
cat("  →", paste0(path_plots, "Plot3_Station_Map.png"), "\n")
cat(sprintf("\n⏱️  Total runtime: %.1f minutes\n", total_time))
cat("✅ ALL DONE\n")
