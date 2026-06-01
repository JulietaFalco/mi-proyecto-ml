# =============================================================================
# Wheat Yield - Linear Regression on Lasso-Selected Features
# Feature sets: Lasso λ=0.05 (18 feat) | λ=0.07 (12 feat) | λ=0.1 (8 feat)
# Preprocessing: Z-score | Split: 70% Train / 10% Validation / 20% Test
# Metrics: RMSE, R2, AIC
# =============================================================================

# --- 1. LOAD LIBRARIES -------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,   # Data manipulation & ggplot2
  readxl,      # Read .xlsx files
  glmnet,      # Lasso (for feature extraction)
  caret        # Preprocessing (Z-score)
)

# --- 2. LOAD DATA ------------------------------------------------------------
data_path <- "C:/Users/Usuario/Documents/Capstone_Project/Dataset_wheat_yield.xlsx"
df        <- read_xlsx(data_path)

# --- 3. DEFINE ALL FEATURES & TARGET -----------------------------------------
month_vars <- function(prefix) paste0(prefix, 1:12)

all_features <- c(
  "LONGITUDE", "LATITUDE", "Wheat_Price", "Nitrogen_Price",
  month_vars("AirTempAvg_month_"),
  month_vars("Rainfall_month_"),
  month_vars("Radiation_month_"),
  month_vars("WindAvg_month_"),
  month_vars("Evaporation_month_"),
  month_vars("RelHumAvg_month_"),
  month_vars("Tmax_"),
  month_vars("Tmin_")
)
target <- "Yield"

model_df <- df[, c(all_features, target)] %>% drop_na()
cat("Dataset rows after NA removal:", nrow(model_df), "\n")
cat("Total features available     :", length(all_features), "\n\n")

# --- 4. TRAIN / VALIDATION / TEST SPLIT (70 / 10 / 20) ----------------------
set.seed(42)
n         <- nrow(model_df)
idx_all   <- sample(seq_len(n))
n_train   <- floor(0.70 * n)
n_val     <- floor(0.10 * n)

idx_train <- idx_all[1:n_train]
idx_val   <- idx_all[(n_train + 1):(n_train + n_val)]
idx_test  <- idx_all[(n_train + n_val + 1):n]

train_df  <- model_df[idx_train, ]
val_df    <- model_df[idx_val,   ]
test_df   <- model_df[idx_test,  ]

cat(sprintf("Split — Train: %d | Validation: %d | Test: %d\n\n",
            nrow(train_df), nrow(val_df), nrow(test_df)))

# --- 5. Z-SCORE PREPROCESSING (fit on train only) ----------------------------
preProc      <- preProcess(train_df[, all_features], method = c("center", "scale"))
train_scaled <- predict(preProc, train_df[, all_features])
val_scaled   <- predict(preProc, val_df[,   all_features])
test_scaled  <- predict(preProc, test_df[,  all_features])

train_scaled[[target]] <- train_df[[target]]
val_scaled[[target]]   <- val_df[[target]]
test_scaled[[target]]  <- test_df[[target]]

# --- 6. EXTRACT LASSO-SELECTED FEATURES FOR EACH LAMBDA ---------------------
lambdas        <- c(0.05, 0.07, 0.1)
selected_feats <- list()

X_train <- as.matrix(train_scaled[, all_features])
y_train <- train_scaled[[target]]

cat("===== Lasso Feature Selection =====\n")
for (lam in lambdas) {
  lasso_fit  <- glmnet(X_train, y_train, alpha = 1, lambda = lam)
  coef_mat   <- coef(lasso_fit, s = lam)
  # Exclude intercept (row 1), keep features with non-zero coefficient
  kept       <- rownames(coef_mat)[which(coef_mat != 0)]
  kept       <- kept[kept != "(Intercept)"]
  selected_feats[[as.character(lam)]] <- kept
  cat(sprintf("λ=%.2f → %d features selected: %s\n",
              lam, length(kept), paste(kept, collapse = ", ")))
}
cat("\n")

# --- 7. HELPER: COMPUTE METRICS ----------------------------------------------
compute_metrics <- function(actual, predicted, n_params, model_name) {
  n       <- length(actual)
  res     <- actual - predicted
  rmse    <- sqrt(mean(res^2))
  r2      <- 1 - sum(res^2) / sum((actual - mean(actual))^2)
  aic     <- n * log(sum(res^2) / n) + 2 * n_params
  data.frame(Model = model_name,
             RMSE  = round(rmse, 4),
             R2    = round(r2,   4),
             AIC   = round(aic,  4))
}

# --- 8. FIT LINEAR REGRESSION ON EACH LASSO FEATURE SET ---------------------
cat("===== Linear Regression on Lasso-Selected Features =====\n")

all_metrics <- data.frame()

for (lam in lambdas) {
  feats   <- selected_feats[[as.character(lam)]]
  n_feats <- length(feats)
  label   <- sprintf("LM on Lasso λ=%.2f (%d feat)", lam, n_feats)
  cat(sprintf("\n--- %s ---\n", label))

  # Subset scaled sets to selected features only
  tr <- train_scaled[, c(feats, target)]
  va <- val_scaled[,   c(feats, target)]
  te <- test_scaled[,  c(feats, target)]

  # Fit linear regression
  lm_formula <- as.formula(paste(target, "~", paste(feats, collapse = " + ")))
  lm_fit     <- lm(lm_formula, data = tr)
  lm_k       <- length(coef(lm_fit))   # intercept + coefficients

  # Predictions
  pred_train <- predict(lm_fit, newdata = tr)
  pred_val   <- predict(lm_fit, newdata = va)
  pred_test  <- predict(lm_fit, newdata = te)

  # Metrics
  m_train <- compute_metrics(tr[[target]], pred_train, lm_k, paste(label, "- Train"))
  m_val   <- compute_metrics(va[[target]], pred_val,   lm_k, paste(label, "- Validation"))
  m_test  <- compute_metrics(te[[target]], pred_test,  lm_k, paste(label, "- Test"))

  cat("Train      :", sprintf("RMSE=%.4f  R2=%.4f  AIC=%.2f\n",
                               m_train$RMSE, m_train$R2, m_train$AIC))
  cat("Validation :", sprintf("RMSE=%.4f  R2=%.4f  AIC=%.2f\n",
                               m_val$RMSE,   m_val$R2,   m_val$AIC))
  cat("Test       :", sprintf("RMSE=%.4f  R2=%.4f  AIC=%.2f\n",
                               m_test$RMSE,  m_test$R2,  m_test$AIC))

  all_metrics <- rbind(all_metrics, m_train, m_val, m_test)
}

# --- 9. FULL COMPARISON TABLE ------------------------------------------------
cat("\n")
cat("=============================================================================\n")
cat("               FULL COMPARISON TABLE (ALL SPLITS)\n")
cat("=============================================================================\n")
print(all_metrics, row.names = FALSE)

# --- 10. TEST-SET ONLY TABLE (CONSOLE) ---------------------------------------
test_only <- all_metrics %>%
  filter(grepl("Test", Model)) %>%
  mutate(Model = gsub(" - Test", "", Model))

cat("\n")
cat("=============================================================================\n")
cat("               FINAL COMPARISON TABLE (TEST SET ONLY)\n")
cat("=============================================================================\n")
cat(sprintf("%-38s  %8s  %8s  %10s\n", "Model", "RMSE", "R2", "AIC"))
cat(sprintf("%-38s  %8s  %8s  %10s\n",
            "--------------------------------------", "--------", "--------", "----------"))
for (i in seq_len(nrow(test_only))) {
  cat(sprintf("%-38s  %8.4f  %8.4f  %10.2f\n",
              test_only$Model[i], test_only$RMSE[i],
              test_only$R2[i],    test_only$AIC[i]))
}
cat("=============================================================================\n")
cat("Decision guide:\n")
cat("  • Lower RMSE  → better predictive accuracy\n")
cat("  • Higher R²   → more variance explained (closer to 1 is best)\n")
cat("  • Lower AIC   → better model fit penalized for complexity\n")
cat("=============================================================================\n")

# --- 11. SAVE FULL TABLE AS CSV ----------------------------------------------
output_csv <- "C:/Users/Usuario/Documents/Capstone_Project/lasso_selected_lm_metrics.csv"
write.csv(all_metrics, output_csv, row.names = FALSE)
cat("\nFull metrics table saved to:", output_csv, "\n")

# --- 12. BAR CHARTS (TEST SET) -----------------------------------------------
cat("\nGenerating comparison charts...\n")

test_plot <- test_only %>%
  mutate(Model = str_wrap(Model, width = 25))

p_rmse <- ggplot(test_plot, aes(x = reorder(Model, RMSE), y = RMSE, fill = Model)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = round(RMSE, 4)), hjust = -0.1, size = 3.5) +
  coord_flip() +
  labs(title = "Test Set RMSE — LM on Lasso-Selected Features",
       x = NULL, y = "RMSE") +
  theme_minimal(base_size = 12)

p_r2 <- ggplot(test_plot, aes(x = reorder(Model, R2), y = R2, fill = Model)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = round(R2, 4)), hjust = -0.1, size = 3.5) +
  coord_flip() +
  labs(title = "Test Set R² — LM on Lasso-Selected Features",
       x = NULL, y = "R²") +
  theme_minimal(base_size = 12)

p_aic <- ggplot(test_plot, aes(x = reorder(Model, AIC), y = AIC, fill = Model)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = round(AIC, 2)), hjust = -0.1, size = 3.5) +
  coord_flip() +
  labs(title = "Test Set AIC — LM on Lasso-Selected Features",
       x = NULL, y = "AIC") +
  theme_minimal(base_size = 12)

ggsave("C:/Users/Usuario/Documents/Capstone_Project/lasso_lm_RMSE.png", p_rmse,
       width = 10, height = 4, dpi = 150)
ggsave("C:/Users/Usuario/Documents/Capstone_Project/lasso_lm_R2.png",   p_r2,
       width = 10, height = 4, dpi = 150)
ggsave("C:/Users/Usuario/Documents/Capstone_Project/lasso_lm_AIC.png",  p_aic,
       width = 10, height = 4, dpi = 150)

cat("Charts saved.\n")

# --- 13. FEATURE REPORT: LASSO λ=0.10 SELECTED FEATURES ---------------------
cat("\n")
cat("=============================================================================\n")
cat("         FEATURES SELECTED BY LASSO λ=0.10 — USED IN LM MODEL\n")
cat("=============================================================================\n")
 
feats_010 <- selected_feats[["0.1"]]
 
# Re-fit the LM for λ=0.10 to extract coefficients
tr_010     <- train_scaled[, c(feats_010, target)]
lm_010     <- lm(as.formula(paste(target, "~", paste(feats_010, collapse = " + "))),
                 data = tr_010)
 
# Extract coefficients (exclude intercept) and build a tidy data frame
coef_010 <- coef(lm_010)
coef_df  <- data.frame(
  Feature     = names(coef_010[names(coef_010) != "(Intercept)"]),
  Coefficient = as.numeric(coef_010[names(coef_010) != "(Intercept)"]),
  stringsAsFactors = FALSE
) %>%
  arrange(desc(abs(Coefficient)))   # sort by absolute magnitude
 
# Print feature list with coefficients
cat(sprintf("Total features selected: %d\n\n", nrow(coef_df)))
cat(sprintf("  %-3s  %-30s  %s\n", "No.", "Feature", "Coefficient"))
cat(sprintf("  %-3s  %-30s  %s\n", "---", "------------------------------", "-----------"))
for (i in seq_len(nrow(coef_df))) {
  cat(sprintf("  %2d.  %-30s  %+.6f\n", i, coef_df$Feature[i], coef_df$Coefficient[i]))
}
cat("\nThese are the only features passed to the Linear Regression model\n")
cat("'LM on Lasso λ=0.10'. All other features were shrunk to zero by Lasso\n")
cat("and excluded from the model entirely.\n")
cat("=============================================================================\n")
 
# --- Coefficient plot (style matching reference image) -----------------------
# Order bars by ABSOLUTE coefficient value (most important at top after coord_flip)
coef_plot_df <- coef_df %>%
  arrange(abs(Coefficient)) %>%                     # ascending abs: least important at bottom
  mutate(
    Direction   = ifelse(Coefficient > 0, "Positive", "Negative"),
    Feature     = factor(Feature, levels = Feature),  # preserve sorted order for coord_flip
    Label       = sprintf("%+.3f", Coefficient),      # e.g. +0.249 or -0.224
    label_hjust = ifelse(Coefficient >= 0, -0.15, 1.15)  # label outside bar end
  )
 
p_coef <- ggplot(coef_plot_df,
                 aes(x = Feature, y = Coefficient, fill = Direction)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = Label, hjust = label_hjust), size = 3.5) +
  geom_vline(xintercept = 0, colour = "black", linewidth = 0.4) +
  coord_flip(clip = "off") +
  scale_fill_manual(values = c("Positive" = "#2d6a2d",   # dark green
                               "Negative" = "#b5432a"),   # dark red/brown
                    guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0.18, 0.18))) +
  labs(
    title    = "LM on Lasso λ=0.10 — Feature Coefficients (Normalised Scale)",
    subtitle = sprintf("%d features selected by Lasso, fitted via Linear Regression",
                       nrow(coef_df)),
    x        = NULL,
    y        = "Coefficient (normalised scale)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10, colour = "grey40"),
    axis.text.y   = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  )
 
ggsave("C:/Users/Usuario/Documents/Capstone_Project/lm_lasso010_coefficients.png",
       p_coef, width = 9, height = 6, dpi = 150)
cat("\nCoefficient plot saved to: lm_lasso010_coefficients.png\n")