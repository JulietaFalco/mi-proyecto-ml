# =============================================================================
# K-MEANS CLUSTERING — SA2 Climate Regions
# Dataset: Dataset_wheat_yield_kmeans.xlsx | Variables: 96 weather vars + lat/lon
# Years: 2005–2025
# Final k is determined automatically by the best Silhouette score
# =============================================================================

# ── CONFIGURABLE PARAMETERS ───────────────────────────────────────────────────

YEAR_START <- 2005                                                    # First year to include
YEAR_END   <- 2025                                                    # Last year to include
K_MIN      <- 2                                                       # Minimum clusters to evaluate
K_MAX      <- 7                                                       # Maximum clusters to evaluate
N_ITER     <- 50                                                      # Maximum iterations per run
N_START    <- 50                                                      # Number of random initializations
SEED       <- 42                                                      # Fixed seed for reproducibility
FILE_PATH  <- "C:/Users/Usuario/Documents/Capstone_Project/k_means/Dataset_wheat_yield_kmeans.xlsx"
SHEET_NAME <- "AllYears_2005-2025"                                    # ← updated sheet name

# =============================================================================

# ── EXPERIMENT LOG ────────────────────────────────────────────────────────────
if (!exists("experiments")) {
  experiments <- data.frame(
    run        = integer(),
    N_ITER     = integer(),
    N_START    = integer(),
    iter_used  = integer(),
    best_k     = integer(),
    silhouette = numeric()
  )
}

# ── 1. PACKAGES ───────────────────────────────────────────────────────────────
required_packages <- c("readxl", "dplyr", "tidyr", "cluster",
                       "ggplot2", "factoextra", "gridExtra")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
  library(pkg, character.only = TRUE)
}

# ── 2. LOAD AND FILTER DATA ───────────────────────────────────────────────────
cat("Loading file:", FILE_PATH, "\n")
cat("Sheet:", SHEET_NAME, "\n")
df_raw <- read_excel(FILE_PATH, sheet = SHEET_NAME)

cat("   Total rows:", nrow(df_raw), "| Years available:",
    paste(sort(unique(df_raw$YEAR)), collapse = ", "), "\n")

df <- df_raw %>% filter(YEAR >= YEAR_START & YEAR <= YEAR_END)
cat("   Rows after filtering (", YEAR_START, "-", YEAR_END, "):", nrow(df), "\n\n")

# ── 3. SELECT ALL WEATHER + GEOGRAPHIC VARIABLES ──────────────────────────────
# All 96 monthly climate variables + latitude + longitude
climate_vars <- names(df)[grepl("AirTempAvg|Rainfall|Radiation|WindAvg|Evaporation|RelHumAvg|Tmax|Tmin",
                                names(df))]
geo_vars     <- c("LONGITUDE", "LATITUDE")
feature_vars <- c(climate_vars, geo_vars)

cat("Variables selected:", length(feature_vars),
    "(", length(climate_vars), "climate +", length(geo_vars), "geographic )\n\n")

# ── 4. AVERAGE BY SA2 — one row per region ────────────────────────────────────
# Averaging across years gives the typical climate profile of each SA2
sa2_avg <- df %>%
  group_by(SA2) %>%
  summarise(across(all_of(feature_vars), ~ mean(.x, na.rm = TRUE)),
            .groups = "drop")

cat("SA2 regions found:", nrow(sa2_avg), "\n")
cat("   ", paste(sa2_avg$SA2, collapse = " | "), "\n\n")

# ── 5. PREPROCESSING: IMPUTE MISSING VALUES + Z-SCORE SCALING ────────────────
# Z-score is mandatory: radiation is in MJ/m2 while rainfall is in mm —
# without scaling, radiation would dominate all distance calculations
X <- sa2_avg %>% select(all_of(feature_vars))
X <- X %>% mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
X_scaled <- scale(X)

cat("Data scaled (Z-score). Matrix dimensions:", nrow(X_scaled), "x", ncol(X_scaled), "\n\n")

# ── 6. FIXED SEED ─────────────────────────────────────────────────────────────
if (!is.null(SEED)) {
  set.seed(SEED)
  cat("Fixed seed:", SEED, "\n\n")
}

# ── 7. EVALUATION: INERTIA + SILHOUETTE FOR k = K_MIN to K_MAX ───────────────
cat("Computing metrics for k =", K_MIN, "to", K_MAX, "...\n\n")

results   <- data.frame(k = integer(), inertia = numeric(), silhouette = numeric())
km_models <- list()

for (k in K_MIN:K_MAX) {
  km      <- kmeans(X_scaled, centers = k, iter.max = N_ITER, nstart = N_START)
  sil     <- silhouette(km$cluster, dist(X_scaled))
  sil_avg <- mean(sil[, 3])

  results   <- rbind(results,
                     data.frame(k = k, inertia = km$tot.withinss, silhouette = sil_avg))
  km_models[[as.character(k)]] <- km

  cat(sprintf("   k = %d | Inertia: %8.1f | Silhouette: %.4f\n",
              k, km$tot.withinss, sil_avg))
}

# Automatically select best k by Silhouette
best_k_sil <- results$k[which.max(results$silhouette)]
cat("\nBest k by Silhouette:", best_k_sil,
    "(score =", round(max(results$silhouette), 4), ")\n\n")

# ── 8. EVALUATION PLOTS ───────────────────────────────────────────────────────
p1 <- ggplot(results, aes(x = k, y = inertia)) +
  geom_line(color = "#378ADD", linewidth = 1) +
  geom_point(color = "#378ADD", size = 3, fill = "white", shape = 21, stroke = 2) +
  scale_x_continuous(breaks = K_MIN:K_MAX) +
  labs(title    = "Elbow Method",
       subtitle = paste("iter.max =", N_ITER, "| nstart =", N_START,
                        "| seed =", SEED),
       x = "Number of clusters (k)", y = "Inertia (WCSS)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())

p2 <- ggplot(results, aes(x = k, y = silhouette)) +
  geom_line(color = "#1D9E75", linewidth = 1) +
  geom_point(size = 4, shape = 21, stroke = 2,
             fill  = ifelse(results$k == best_k_sil, "#1D9E75", "white"),
             color = "#1D9E75") +
  geom_vline(xintercept = best_k_sil, linetype = "dashed",
             color = "#1D9E75", alpha = 0.6) +
  annotate("text", x = best_k_sil + 0.15, y = min(results$silhouette),
           label = paste("Best k =", best_k_sil),
           color = "#1D9E75", hjust = 0, size = 3.5) +
  scale_x_continuous(breaks = K_MIN:K_MAX) +
  labs(title    = "Silhouette Score",
       subtitle = "Higher = more compact and well-separated clusters",
       x = "Number of clusters (k)", y = "Average silhouette") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())

print(p1)
print(p2)

png("C:/Users/Usuario/Documents/Capstone_Project/k_means/kmeans_evaluation.png",
    width = 1400, height = 550, res = 130)
gridExtra::grid.arrange(p1, p2, ncol = 2)
dev.off()
cat("Saved: kmeans_evaluation.png\n")

# ── 9. FINAL MODEL ────────────────────────────────────────────────────────────
km_final        <- km_models[[as.character(best_k_sil)]]
sa2_avg$cluster <- km_final$cluster

sil_final <- silhouette(km_final$cluster, dist(X_scaled))
sil_score <- mean(sil_final[, 3])

cat("\nFinal model: k =", best_k_sil, "(selected by Silhouette)\n")
cat("Final silhouette score:", round(sil_score, 4), "\n")
cat("Iterations used:", km_final$iter, "/", N_ITER, "\n\n")

experiments <- rbind(experiments, data.frame(
  run        = nrow(experiments) + 1,
  N_ITER     = N_ITER,
  N_START    = N_START,
  iter_used  = km_final$iter,
  best_k     = best_k_sil,
  silhouette = round(sil_score, 4)
))

cat("=== EXPERIMENT LOG (all runs this session) ===\n")
print(experiments)

# ── 10. RESULTS: SA2 BY CLUSTER ───────────────────────────────────────────────
cat("\n=======================================================\n")
cat("  RESULTS: SA2 BY CLUSTER  (k =", best_k_sil, ")\n")
cat("=======================================================\n")

for (c in sort(unique(sa2_avg$cluster))) {
  members <- sa2_avg$SA2[sa2_avg$cluster == c]
  cat(sprintf("\nCluster %d  (%d SA2):\n", c, length(members)))
  cat(paste(" •", members, collapse = "\n"), "\n")
}

# ── 11. CLUSTER CHARACTERIZATION ─────────────────────────────────────────────
# Use a representative subset of variables for the summary table
vars_summary <- c("AirTempAvg_month_1", "AirTempAvg_month_7",
                  "Rainfall_month_1",   "Rainfall_month_7",
                  "Radiation_month_1",  "Radiation_month_7",
                  "Evaporation_month_1","Evaporation_month_7",
                  "Tmax_1",            "Tmax_7",
                  "Tmin_1",            "Tmin_7")

vars_summary <- vars_summary[vars_summary %in% names(sa2_avg)]

cat("\n=======================================================\n")
cat("  CLUSTER CHARACTERIZATION (averages per cluster)\n")
cat("=======================================================\n")

characterization <- sa2_avg %>%
  group_by(cluster) %>%
  summarise(across(all_of(vars_summary), ~ round(mean(.x, na.rm = TRUE), 1)),
            n_SA2 = n(),
            .groups = "drop")

print(as.data.frame(characterization))

# ── 12. DETAILED SILHOUETTE PLOT ─────────────────────────────────────────────
cluster_colors <- c("#D85A30", "#1D9E75", "#378ADD", "#F0A500",
                    "#9B59B6", "#E67E22", "#1ABC9C")

p_sil <- fviz_silhouette(sil_final,
                          palette = cluster_colors[1:best_k_sil],
                          ggtheme = theme_minimal()) +
  labs(title    = paste("Silhouette detail — k =", best_k_sil),
       subtitle = paste("Average score:", round(sil_score, 4),
                        "| 96 weather variables + lat/lon")) +
  theme(plot.title = element_text(face = "bold"))

print(p_sil)

png("C:/Users/Usuario/Documents/Capstone_Project/k_means/kmeans_silhouette_detail.png",
    width = 900, height = 600, res = 130)
plot(p_sil)
dev.off()
cat("Saved: kmeans_silhouette_detail.png\n")

# ── 13. GEOGRAPHIC MAP ────────────────────────────────────────────────────────
p_map <- ggplot(sa2_avg, aes(x = LONGITUDE, y = LATITUDE,
                              color = factor(cluster),
                              label = SA2)) +
  geom_point(size = 5, alpha = 0.85) +
  geom_text(nudge_y = 0.3, size = 2.8, show.legend = FALSE) +
  scale_color_manual(values = cluster_colors[1:best_k_sil],
                     name   = "Cluster",
                     labels = paste("Cluster", 1:best_k_sil)) +
  labs(title    = paste("Geographic distribution — k =", best_k_sil,
                        "clusters (Silhouette)"),
       subtitle = paste("SA2 regions |", YEAR_START, "–", YEAR_END,
                        "| 96 weather variables + lat/lon",
                        "| Silhouette:", round(sil_score, 3)),
       x = "Longitude", y = "Latitude") +
  theme_minimal(base_size = 12) +
  theme(plot.title       = element_text(face = "bold"),
        legend.position  = "right",
        panel.grid.minor = element_blank())

print(p_map)
ggsave("C:/Users/Usuario/Documents/Capstone_Project/k_means/kmeans_map.png",
       p_map, width = 9, height = 7, dpi = 130)
cat("Saved: kmeans_map.png\n")

# ── 14. EXPORT CSV ────────────────────────────────────────────────────────────
result_export <- sa2_avg %>%
  select(SA2, LONGITUDE, LATITUDE, cluster) %>%
  arrange(cluster, SA2)

write.csv(result_export,
          "C:/Users/Usuario/Documents/Capstone_Project/k_means/SA2_clusters_R.csv",
          row.names = FALSE)
write.csv(experiments,
          "C:/Users/Usuario/Documents/Capstone_Project/k_means/experiment_log.csv",
          row.names = FALSE)

cat("\n=======================================================\n")
cat("FILES SAVED TO k_means folder:\n")
cat("   kmeans_evaluation.png        — Elbow + Silhouette curves\n")
cat("   kmeans_silhouette_detail.png — Silhouette width per SA2\n")
cat("   kmeans_map.png               — Geographic cluster map\n")
cat("   SA2_clusters_R.csv           — Cluster assignment per SA2\n")
cat("   experiment_log.csv           — Parameter combinations tested\n")
cat("=======================================================\n")
