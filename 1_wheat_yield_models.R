# =============================================================================
# Wheat Yield Prediction - Linear Regression & Lasso Regression
# Dataset: Dataset_wheat_yield
# Target: Yield | Preprocessing: Z-score | Split: 70/10/20
# Metrics: RMSE, R2, AIC
# =============================================================================

# --- 1. LOAD LIBRARIES -------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,   # Data manipulation & ggplot2
  readxl,      # Read .xlsx files
  glmnet,      # Lasso regression
  caret,       # Preprocessing (Z-score)
  Metrics,     # RMSE
  knitr,       # Table formatting
  kableExtra   # Enhanced tables
)

# --- 2. LOAD DATA ------------------------------------------------------------
data_path <- "C:/Users/Usuario/Documents/Capstone_Project/Dataset_wheat_yield.xlsx"

df <- read_xlsx(data_path)

cat("Dataset dimensions:", dim(df), "\n")

# -- Column name diagnostics (helps detect prefix mismatches) --
grep_cols <- function(pat) grep(pat, names(df), value = TRUE, ignore.case = TRUE)
cat("\n--- Column name check per group ---\n")
cat("Tmax-like   :", paste(grep_cols("tmax"),       collapse = ", "), "\n")
cat("Tmin-like   :", paste(grep_cols("tmin"),       collapse = ", "), "\n")
cat("AirTemp     :", paste(grep_cols("airtemp"),    collapse = ", "), "\n")
cat("Rainfall    :", paste(grep_cols("rainfall"),   collapse = ", "), "\n")
cat("Radiation   :", paste(grep_cols("radiation"),  collapse = ", "), "\n")
cat("Wind        :", paste(grep_cols("wind"),       collapse = ", "), "\n")
cat("Evaporation :", paste(grep_cols("evap"),       collapse = ", "), "\n")
cat("RelHum      :", paste(grep_cols("relh"),       collapse = ", "), "\n")
cat("\nAll column names:\n")
print(names(df))
cat("\nMissing values per column:\n")
print(colSums(is.na(df)))


# --- 3. DEFINE FEATURES & TARGET ---------------------------------------------
# NOTE: If the diagnostic above shows different prefixes (e.g. "Tmax_1" instead
#       of "Tmax_month_1"), update the prefix strings below accordingly.
month_vars <- function(prefix) paste0(prefix, 1:12)

features <- c(
  "LONGITUDE",
  "LATITUDE",
  "Wheat_Price",
  "Nitrogen_Price",
  month_vars("AirTempAvg_month_"),
  month_vars("Rainfall_month_"),
  month_vars("Radiation_month_"),
  month_vars("WindAvg_month_"),
  month_vars("Evaporation_month_"),
  month_vars("RelHumAvg_month_"),
  month_vars("Tmax_"),   # <-- update prefix if needed
  month_vars("Tmin_")    # <-- update prefix if needed
)

target <- "Yield"

# Safety check: warn about any features not found in the dataset
missing_cols <- setdiff(c(features, target), names(df))
if (length(missing_cols) > 0) {
  cat("\n*** WARNING: These columns were NOT found in the dataset ***\n")
  print(missing_cols)
  cat("Check the diagnostic output above and update the prefix strings.\n")
  stop("Stopping execution — fix column names before proceeding.")
}

# Keep only relevant columns and drop rows with NA
model_df <- df[, c(features, target)] %>% drop_na()

cat("\nFinal dataset size after NA removal:", nrow(model_df), "rows\n")
cat("Number of features:", length(features), "\n")


# --- 4. TRAIN / VALIDATION / TEST SPLIT (70 / 10 / 20) ----------------------
set.seed(42)

n         <- nrow(model_df)
idx_all   <- sample(seq_len(n))

n_train   <- floor(0.70 * n)
n_val     <- floor(0.10 * n)
# test gets the remainder to guarantee no overlap

idx_train <- idx_all[1:n_train]
idx_val   <- idx_all[(n_train + 1):(n_train + n_val)]
idx_test  <- idx_all[(n_train + n_val + 1):n]

train_df  <- model_df[idx_train, ]
val_df    <- model_df[idx_val,   ]
test_df   <- model_df[idx_test,  ]

cat("\nSplit sizes  — Train:", nrow(train_df),
    "| Validation:", nrow(val_df),
    "| Test:", nrow(test_df), "\n")


# --- 5. Z-SCORE PREPROCESSING ------------------------------------------------
# Fit scaler on TRAINING set only, then apply to val & test
preProc <- preProcess(train_df[, features], method = c("center", "scale"))

train_scaled <- predict(preProc, train_df[, features])
val_scaled   <- predict(preProc, val_df[,   features])
test_scaled  <- predict(preProc, test_df[,  features])

# Add target back
train_scaled[[target]] <- train_df[[target]]
val_scaled[[target]]   <- val_df[[target]]
test_scaled[[target]]  <- test_df[[target]]


# --- 6. HELPER: COMPUTE METRICS ----------------------------------------------
compute_metrics <- function(actual, predicted, n_params, model_name) {
  
  n   <- length(actual)
  res <- actual - predicted
  
  rmse_val <- sqrt(mean(res^2))
  ss_res   <- sum(res^2)
  ss_tot   <- sum((actual - mean(actual))^2)
  r2_val   <- 1 - ss_res / ss_tot
  
  # AIC = n * log(RSS/n) + 2k   (OLS / Gaussian likelihood approximation)
  rss      <- ss_res
  aic_val  <- n * log(rss / n) + 2 * n_params
  
  data.frame(
    Model   = model_name,
    RMSE    = round(rmse_val, 4),
    R2      = round(r2_val,   4),
    AIC     = round(aic_val,  4)
  )
}


# --- 7. LINEAR REGRESSION ----------------------------------------------------
cat("\n===== Linear Regression =====\n")

# Formula: Yield ~ all features
lm_formula <- as.formula(paste(target, "~", paste(features, collapse = " + ")))

lm_model <- lm(lm_formula, data = train_scaled)
cat("Linear Regression summary:\n")
print(summary(lm_model))

# Predictions
lm_pred_train <- predict(lm_model, newdata = train_scaled)
lm_pred_val   <- predict(lm_model, newdata = val_scaled)
lm_pred_test  <- predict(lm_model, newdata = test_scaled)

# Number of parameters = intercept + coefficients
lm_k <- length(coef(lm_model))   # includes intercept

# Metrics
lm_metrics_train <- compute_metrics(train_scaled[[target]], lm_pred_train, lm_k, "Linear Reg - Train")
lm_metrics_val   <- compute_metrics(val_scaled[[target]],   lm_pred_val,   lm_k, "Linear Reg - Validation")
lm_metrics_test  <- compute_metrics(test_scaled[[target]],  lm_pred_test,  lm_k, "Linear Reg - Test")

cat("\nLinear Regression Metrics:\n")
print(rbind(lm_metrics_train, lm_metrics_val, lm_metrics_test))


# --- 8. LASSO REGRESSION -----------------------------------------------------
cat("\n===== Lasso Regression =====\n")

# Prepare matrix inputs for glmnet
X_train <- as.matrix(train_scaled[, features])
y_train <- train_scaled[[target]]

X_val   <- as.matrix(val_scaled[, features])
y_val   <- val_scaled[[target]]

X_test  <- as.matrix(test_scaled[, features])
y_test  <- test_scaled[[target]]

lambdas <- c(0.01, 0.05, 0.07, 0.1)

lasso_results <- list()

for (lam in lambdas) {
  
  cat("\n--- Lambda:", lam, "---\n")
  
  lasso_model <- glmnet(X_train, y_train, alpha = 1, lambda = lam)
  
  # Predictions
  lasso_pred_train <- as.vector(predict(lasso_model, newx = X_train, s = lam))
  lasso_pred_val   <- as.vector(predict(lasso_model, newx = X_val,   s = lam))
  lasso_pred_test  <- as.vector(predict(lasso_model, newx = X_test,  s = lam))
  
  # Number of non-zero coefficients (effective parameters) + intercept
  coef_lasso <- coef(lasso_model, s = lam)
  lasso_k    <- sum(coef_lasso != 0)   # includes intercept if non-zero
  
  cat("Non-zero coefficients (incl. intercept):", lasso_k, "\n")
  
  label <- paste0("Lasso λ=", lam)
  
  m_train <- compute_metrics(y_train,     lasso_pred_train, lasso_k, paste(label, "- Train"))
  m_val   <- compute_metrics(y_val,       lasso_pred_val,   lasso_k, paste(label, "- Validation"))
  m_test  <- compute_metrics(y_test,      lasso_pred_test,  lasso_k, paste(label, "- Test"))
  
  lasso_results[[as.character(lam)]] <- list(
    model   = lasso_model,
    metrics = rbind(m_train, m_val, m_test)
  )
  
  cat("Metrics:\n")
  print(rbind(m_train, m_val, m_test))
}


# --- 9. COMPARISON TABLE -----------------------------------------------------
cat("\n===== FULL COMPARISON TABLE =====\n")

# Gather all metrics
all_metrics <- rbind(
  lm_metrics_train, lm_metrics_val, lm_metrics_test
)

for (lam in lambdas) {
  all_metrics <- rbind(all_metrics, lasso_results[[as.character(lam)]]$metrics)
}

# Pretty print
print(all_metrics, row.names = FALSE)


# --- 10. SAVE COMPARISON TABLE AS CSV ----------------------------------------
output_csv <- "C:/Users/Usuario/Documents/Capstone_Project/model_comparison_metrics.csv"
write.csv(all_metrics, output_csv, row.names = FALSE)
cat("\nComparison table saved to:", output_csv, "\n")


# --- 11. LASSO COEFFICIENT PATH PLOT -----------------------------------------
cat("\nGenerating Lasso coefficient path plot...\n")

lasso_path_model <- glmnet(X_train, y_train, alpha = 1)

png("C:/Users/Usuario/Documents/Capstone_Project/lasso_coefficient_path.png",
    width = 900, height = 600, res = 120)
plot(lasso_path_model, xvar = "lambda", label = TRUE,
     main = "Lasso Coefficient Path")
abline(v = log(lambdas), col = "red", lty = 2)
legend("topright", legend = paste0("λ=", lambdas),
       col = "red", lty = 2, cex = 0.8)
dev.off()
cat("Lasso path plot saved.\n")


# --- 12. RESIDUAL PLOTS (Linear Regression) ----------------------------------
cat("\nGenerating residual plots for Linear Regression...\n")

png("C:/Users/Usuario/Documents/Capstone_Project/lm_residual_plots.png",
    width = 1000, height = 700, res = 120)
par(mfrow = c(2, 2))
plot(lm_model, main = "Linear Regression Diagnostics")
dev.off()
cat("Residual plots saved.\n")


# --- 13. METRICS COMPARISON BAR CHART ----------------------------------------
cat("\nGenerating metrics comparison chart...\n")

# Filter test-set rows only for a cleaner comparison chart
test_only <- all_metrics %>%
  filter(grepl("Test", Model)) %>%
  mutate(Model = gsub(" - Test", "", Model))

# RMSE bar chart
p_rmse <- ggplot(test_only, aes(x = reorder(Model, RMSE), y = RMSE, fill = Model)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = round(RMSE, 3)), hjust = -0.1, size = 3.5) +
  coord_flip() +
  labs(title = "Test Set RMSE by Model", x = NULL, y = "RMSE") +
  theme_minimal(base_size = 12)

# R2 bar chart
p_r2 <- ggplot(test_only, aes(x = reorder(Model, R2), y = R2, fill = Model)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = round(R2, 3)), hjust = -0.1, size = 3.5) +
  coord_flip() +
  labs(title = "Test Set R² by Model", x = NULL, y = "R²") +
  theme_minimal(base_size = 12)

# AIC bar chart
p_aic <- ggplot(test_only, aes(x = reorder(Model, AIC), y = AIC, fill = Model)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = round(AIC, 1)), hjust = -0.1, size = 3.5) +
  coord_flip() +
  labs(title = "Test Set AIC by Model", x = NULL, y = "AIC") +
  theme_minimal(base_size = 12)

# Save charts
ggsave("C:/Users/Usuario/Documents/Capstone_Project/comparison_RMSE.png", p_rmse,
       width = 9, height = 5, dpi = 150)
ggsave("C:/Users/Usuario/Documents/Capstone_Project/comparison_R2.png",   p_r2,
       width = 9, height = 5, dpi = 150)
ggsave("C:/Users/Usuario/Documents/Capstone_Project/comparison_AIC.png",  p_aic,
       width = 9, height = 5, dpi = 150)

cat("Comparison charts saved.\n")


# --- 14. FINAL DECISION TABLE (console) --------------------------------------
cat("\n")
cat("=============================================================================\n")
cat("                   FINAL MODEL COMPARISON (TEST SET)\n")
cat("=============================================================================\n")
cat(sprintf("%-30s  %10s  %10s  %12s\n", "Model", "RMSE", "R2", "AIC"))
cat(sprintf("%-30s  %10s  %10s  %12s\n",
            "------------------------------", "----------", "----------", "------------"))
for (i in seq_len(nrow(test_only))) {
  cat(sprintf("%-30s  %10.4f  %10.4f  %12.2f\n",
              test_only$Model[i], test_only$RMSE[i], test_only$R2[i], test_only$AIC[i]))
}
cat("=============================================================================\n")
cat("Decision guide:\n")
cat("  • Lower RMSE  → better predictive accuracy\n")
cat("  • Higher R²   → more variance explained (closer to 1 is best)\n")
cat("  • Lower AIC   → better model fit penalized for complexity\n")
cat("=============================================================================\n")
