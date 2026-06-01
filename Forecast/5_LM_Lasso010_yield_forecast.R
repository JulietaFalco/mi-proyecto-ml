# =============================================================================
#  Linear Regression Yield Forecast — FY2025/2026 & FY2026/2027
#  Model: LM on Lasso λ=0.10 (10 features, Z-score normalisation)
#
#  Workflow:
#    1. Load full historical training data
#    2. Retrain LM (OLS) on ALL data using the 10 Lasso-selected features
#       Z-score fitted on full training data
#    3. Load forecast data (climate 2025 → Yield 2025/2026,
#                           climate 2026 → Yield 2026/2027)
#    4. Apply same Z-score scaling
#    5. Predict yield per station per forecast year
#    6. Produce plots separated by Cluster
#    7. Export results to Excel
#
#  10 features selected by Lasso λ=0.10:
#    Wheat_Price, Evaporation_month_10, Tmin_8, Rainfall_month_8,
#    Tmax_9, Rainfall_month_7, Rainfall_month_5, Tmax_10,
#    Nitrogen_Price, RelHumAvg_month_10
# =============================================================================


# ── 0. PACKAGES ───────────────────────────────────────────────────────────────
packages <- c("readxl", "writexl", "dplyr", "ggplot2",
              "tidyr", "scales", "caret", "gt")
for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
library(readxl); library(writexl); library(dplyr)
library(ggplot2); library(tidyr); library(scales)
library(caret);  library(gt)


# ── 1. PATHS ──────────────────────────────────────────────────────────────────
PATH_TRAIN    <- "C:/Users/Usuario/Documents/Capstone_Project/Dataset_wheat_yield.xlsx"
PATH_FORECAST <- "C:/Users/Usuario/Documents/Capstone_Project/Forecast/Forecast_2025-2026_NewData.xlsx"
PATH_OUTPUT   <- "C:/Users/Usuario/Documents/Capstone_Project/Forecast/LM_Yield_Predictions_FY2526_FY2627.xlsx"
PATH_PLOTS    <- "C:/Users/Usuario/Documents/Capstone_Project/Forecast/"


# ── 2. THE 10 SELECTED FEATURES (Lasso λ=0.10) ────────────────────────────────
# Lasso selected these features; LM (OLS) was then re-fitted on them.
# Cluster is NOT a predictor — used only for plot separation.

SELECTED_FEATURES <- c(
  "Wheat_Price",
  "Evaporation_month_10",
  "Tmin_8",
  "Rainfall_month_8",
  "Tmax_9",
  "Rainfall_month_7",
  "Rainfall_month_5",
  "Tmax_10",
  "Nitrogen_Price",
  "RelHumAvg_month_10"
)

TARGET <- "Yield"


# ── 3. LOAD TRAINING DATA ─────────────────────────────────────────────────────
cat("── Loading training data ────────────────────────────────────\n")

df_train <- read_excel(PATH_TRAIN) %>%
  select(all_of(c(SELECTED_FEATURES, TARGET, "SA2", "Cluster"))) %>%
  drop_na()

cat(sprintf("  Training rows     : %d\n",   nrow(df_train)))
cat(sprintf("  Features used     : %d\n",   length(SELECTED_FEATURES)))
cat(sprintf("  Clusters present  : %s\n\n",
            paste(sort(unique(df_train$Cluster)), collapse = ", ")))


# ── 4. Z-SCORE NORMALISATION — fit on full training data ──────────────────────
# Means and SDs from training will be reused on forecast data
cat("── Z-score normalisation (fitted on full training data) ─────\n")

preProc     <- preProcess(df_train[, SELECTED_FEATURES],
                           method = c("center", "scale"))
train_sc    <- predict(preProc, df_train[, SELECTED_FEATURES])
train_sc[[TARGET]]   <- df_train[[TARGET]]

cat("  Scaling parameters saved for forecast data application\n\n")


# ── 5. RETRAIN LINEAR REGRESSION ON FULL DATASET ─────────────────────────────
# No train/test split here — we use ALL available data to maximise
# the information given to the model before predicting unseen future years.
cat("── Retraining Linear Regression on full dataset ─────────────\n")

lm_formula <- as.formula(
  paste(TARGET, "~", paste(SELECTED_FEATURES, collapse = " + "))
)
lm_final <- lm(lm_formula, data = train_sc)

cat("  Model summary:\n")
sm <- summary(lm_final)
cat(sprintf("  R²          : %.4f\n",   sm$r.squared))
cat(sprintf("  Adj. R²     : %.4f\n",   sm$adj.r.squared))
cat(sprintf("  Residual SE : %.4f\n\n", sm$sigma))

# Print coefficients sorted by absolute value
coef_df <- data.frame(
  Feature     = names(coef(lm_final))[-1],
  Coefficient = as.numeric(coef(lm_final))[-1]
) %>% arrange(desc(abs(Coefficient)))

cat("── Final model coefficients (sorted by importance) ──────────\n")
print(coef_df, row.names = FALSE)
cat("\n")


# ── 6. LOAD FORECAST DATA ─────────────────────────────────────────────────────
cat("── Loading forecast data ────────────────────────────────────\n")

df_forecast <- read_excel(PATH_FORECAST)

# Verify all required features exist
missing_cols <- setdiff(c(SELECTED_FEATURES, "YEAR", "SA2", "Cluster"),
                        names(df_forecast))
if (length(missing_cols) > 0) {
  cat("\n*** MISSING COLUMNS IN FORECAST FILE ***\n")
  print(missing_cols)
  stop("Please check the forecast file — required columns are missing.")
}

cat(sprintf("  Forecast rows     : %d\n", nrow(df_forecast)))
cat(sprintf("  Years present     : %s\n",
            paste(sort(unique(df_forecast$YEAR)), collapse = ", ")))
cat(sprintf("  Stations          : %d\n",
            n_distinct(df_forecast$SA2)))
cat(sprintf("  Clusters present  : %s\n\n",
            paste(sort(unique(df_forecast$Cluster)), collapse = ", ")))


# ── 7. APPLY Z-SCORE USING TRAINING PARAMETERS ────────────────────────────────
cat("── Applying Z-score to forecast data ────────────────────────\n")

forecast_sc <- predict(preProc, df_forecast[, SELECTED_FEATURES])
cat("  Normalisation applied using training means & SDs\n\n")


# ── 8. PREDICT YIELD ──────────────────────────────────────────────────────────
cat("── Predicting yield ─────────────────────────────────────────\n")

predicted_yield <- predict(lm_final, newdata = forecast_sc)

# Map YEAR to fiscal year label
# Climate 2025 → Yield FY2025/2026 | Climate 2026 → Yield FY2026/2027
results_df <- df_forecast %>%
  select(SA2, Cluster, YEAR) %>%
  mutate(
    Predicted_Yield = round(predicted_yield, 4),
    FY_Label        = case_match(
      as.character(YEAR),
      "2025" ~ "FY 2025/2026",
      "2026" ~ "FY 2026/2027",
      .default = paste0("FY ", YEAR)
    )
  ) %>%
  arrange(Cluster, SA2, YEAR)

cat(sprintf("  Predictions generated : %d\n\n", nrow(results_df)))


# ── 9. PRINT RESULTS TABLE ────────────────────────────────────────────────────
cat("── Predicted Yield per Station per Fiscal Year ──────────────\n")

results_wide <- results_df %>%
  select(SA2, Cluster, FY_Label, Predicted_Yield) %>%
  pivot_wider(names_from = FY_Label, values_from = Predicted_Yield) %>%
  arrange(Cluster, SA2)

print(results_wide, n = Inf)
cat("\n")


# ── 10. SUMMARY BY CLUSTER AND FISCAL YEAR ────────────────────────────────────
cat("── Summary by Cluster and Fiscal Year ──────────────────────\n")

summary_cluster <- results_df %>%
  group_by(Cluster, FY_Label) %>%
  summarise(
    N_stations = n(),
    Min_Yield  = round(min(Predicted_Yield),  3),
    Mean_Yield = round(mean(Predicted_Yield), 3),
    Max_Yield  = round(max(Predicted_Yield),  3),
    SD_Yield   = round(sd(Predicted_Yield),   3),
    .groups    = "drop"
  )

print(summary_cluster, n = Inf)
cat("\n")


# ── 11. GT TABLE — formatted results ─────────────────────────────────────────
gt_results <- results_wide %>%
  gt(groupname_col = "Cluster") %>%
  tab_header(
    title    = md("**LM on Lasso λ=0.10 — Wheat Yield Forecast**"),
    subtitle = md("*10 features | Z-score normalisation | Grouped by Cluster*")
  ) %>%
  cols_label(SA2 = "SA2 Station") %>%
  fmt_number(columns = starts_with("FY"), decimals = 3) %>%
  data_color(
    columns = starts_with("FY"),
    palette = c("#B85042", "#FFFFFF", "#2C5F2D")
  ) %>%
  tab_footnote(
    footnote  = "Green = higher yield | Red = lower yield",
    locations = cells_title(groups = "subtitle")
  ) %>%
  tab_options(
    table.font.size           = 12,
    heading.align             = "left",
    column_labels.font.weight = "bold",
    row_group.font.weight     = "bold"
  )

print(gt_results)


# ── 12. CLUSTER COLOUR PALETTE ────────────────────────────────────────────────
cluster_ids  <- sort(unique(results_df$Cluster))
cluster_pal  <- setNames(
  c("#2C5F2D", "#C8A84B", "#B85042", "#378ADD",
    "#9B59B6", "#E67E22", "#1ABC9C")[seq_along(cluster_ids)],
  cluster_ids
)

fy_colours <- c("FY 2025/2026" = "#2C5F2D", "FY 2026/2027" = "#C8A84B")


# ── 13. PLOT A — Predicted yield by station, FACETED BY CLUSTER ──────────────
cat("── Generating Plot A: yield by station per cluster ──────────\n")

p_cluster <- ggplot(
  results_df,
  aes(x    = reorder(SA2, Predicted_Yield),
      y    = Predicted_Yield,
      fill = FY_Label)
) +
  geom_col(position = "dodge", width = 0.7) +
  facet_wrap(~ paste0("Cluster ", Cluster),
             scales = "free_y", ncol = 2) +
  scale_fill_manual(values = fy_colours, name = "Fiscal Year") +
  coord_flip() +
  labs(
    title    = "LM on Lasso λ=0.10 — Predicted Wheat Yield by Cluster",
    subtitle = "10 features | Z-score normalisation | Each panel = one cluster",
    x        = NULL,
    y        = "Predicted Yield (t/ha)",
    caption  = "Climate 2025 → FY2025/2026 | Climate 2026 → FY2026/2027"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", size = 13),
    plot.subtitle      = element_text(colour = "grey40", size = 10),
    strip.text         = element_text(face = "bold", size = 11),
    strip.background   = element_rect(fill = "grey92", colour = NA),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position    = "top"
  )

print(p_cluster)
ggsave(paste0(PATH_PLOTS, "A_yield_by_station_per_cluster.png"),
       p_cluster, width = 14, height = 10, dpi = 150)
cat("  Saved: A_yield_by_station_per_cluster.png\n\n")


# ── 14. PLOT B — Year-over-year change per station, FACETED BY CLUSTER ────────
cat("── Generating Plot B: yield change per cluster ───────────────\n")

results_change <- results_wide %>%
  mutate(
    Change    = `FY 2026/2027` - `FY 2025/2026`,
    Direction = ifelse(Change >= 0, "Increase", "Decrease")
  )

p_change_cluster <- ggplot(
  results_change,
  aes(x    = reorder(SA2, Change),
      y    = Change,
      fill = Direction)
) +
  geom_col(width = 0.7) +
  geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.5) +
  facet_wrap(~ paste0("Cluster ", Cluster),
             scales = "free_y", ncol = 2) +
  scale_fill_manual(
    values = c("Increase" = "#2C5F2D", "Decrease" = "#B85042"),
    name   = NULL
  ) +
  coord_flip() +
  labs(
    title    = "LM on Lasso λ=0.10 — Yield Change FY2025/2026 → FY2026/2027",
    subtitle = "Positive = yield expected to improve | Grouped by Cluster",
    x        = NULL,
    y        = "Change in Predicted Yield (t/ha)",
    caption  = "FY2026/2027 − FY2025/2026"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", size = 13),
    plot.subtitle      = element_text(colour = "grey40", size = 10),
    strip.text         = element_text(face = "bold", size = 11),
    strip.background   = element_rect(fill = "grey92", colour = NA),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position    = "top"
  )

print(p_change_cluster)
ggsave(paste0(PATH_PLOTS, "B_yield_change_per_cluster.png"),
       p_change_cluster, width = 14, height = 10, dpi = 150)
cat("  Saved: B_yield_change_per_cluster.png\n\n")


# ── 15. PLOT C — Mean yield per cluster per fiscal year ───────────────────────
cat("── Generating Plot C: mean yield per cluster ─────────────────\n")

p_mean_cluster <- ggplot(
  summary_cluster,
  aes(x    = paste0("Cluster ", Cluster),
      y    = Mean_Yield,
      fill = FY_Label)
) +
  geom_col(position = "dodge", width = 0.6) +
  geom_errorbar(
    aes(ymin = Mean_Yield - SD_Yield,
        ymax = Mean_Yield + SD_Yield),
    position = position_dodge(0.6),
    width    = 0.25, colour = "grey40", linewidth = 0.6
  ) +
  geom_text(
    aes(label = sprintf("%.3f", Mean_Yield)),
    position = position_dodge(0.6),
    vjust    = -0.5, size = 3.5
  ) +
  scale_fill_manual(values = fy_colours, name = "Fiscal Year") +
  labs(
    title    = "LM on Lasso λ=0.10 — Mean Predicted Yield by Cluster",
    subtitle = "Error bars = ±1 SD across stations within cluster",
    x        = NULL,
    y        = "Mean Predicted Yield (t/ha)",
    caption  = "Climate 2025 → FY2025/2026 | Climate 2026 → FY2026/2027"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(colour = "grey40", size = 10),
    panel.grid.minor = element_blank(),
    legend.position  = "top"
  )

print(p_mean_cluster)
ggsave(paste0(PATH_PLOTS, "C_mean_yield_per_cluster.png"),
       p_mean_cluster, width = 10, height = 6, dpi = 150)
cat("  Saved: C_mean_yield_per_cluster.png\n\n")


# ── 16. EXPORT TO EXCEL ───────────────────────────────────────────────────────
write_xlsx(
  list(
    "Predictions_Wide"    = results_wide,
    "Predictions_Long"    = results_df,
    "Summary_by_Cluster"  = summary_cluster,
    "Model_Coefficients"  = coef_df
  ),
  path = PATH_OUTPUT
)
cat(sprintf("── Results exported to:\n  %s\n\n", PATH_OUTPUT))


# ── 17. FINAL SUMMARY ─────────────────────────────────────────────────────────
cat("════════════════════════════════════════════════════════════\n")
cat("  YIELD FORECAST SUMMARY\n")
cat("════════════════════════════════════════════════════════════\n")
cat("  Model         : Linear Regression (OLS)\n")
cat("  Feature sel.  : Lasso λ=0.10\n")
cat("  Features used : 10\n")
cat("  Normalisation : Z-score (training means & SDs)\n")
cat(sprintf("  Stations      : %d\n", n_distinct(results_df$SA2)))
cat(sprintf("  Clusters      : %d\n", n_distinct(results_df$Cluster)))
cat("────────────────────────────────────────────────────────────\n")
for (fy in sort(unique(results_df$FY_Label))) {
  d <- results_df %>% filter(FY_Label == fy)
  cat(sprintf("  %-14s  Mean = %.3f  |  Range: %.3f – %.3f\n",
              fy,
              mean(d$Predicted_Yield),
              min(d$Predicted_Yield),
              max(d$Predicted_Yield)))
}
cat("────────────────────────────────────────────────────────────\n")
cat("  Files saved:\n")
cat(sprintf("    %s\n", PATH_OUTPUT))
cat("    A_yield_by_station_per_cluster.png\n")
cat("    B_yield_change_per_cluster.png\n")
cat("    C_mean_yield_per_cluster.png\n")
cat("════════════════════════════════════════════════════════════\n")
