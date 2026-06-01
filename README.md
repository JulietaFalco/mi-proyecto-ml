# Wheat Yield Forecasting — Capstone Project

This repository contains the R scripts and datasets used to predict wheat yield across Western Australia SA2 regions, using weather and economic variables.

---

## Project Structure

The analysis follows five sequential steps, organised across two folders:

### 📁 1_k_means
| File | Description |
|------|-------------|
| `kmeans_sa2_clustering.R` | Clusters the 18 SA2 weather stations into Northern and Southern regions using K-means (k=2, selected via Silhouette Score) |
| `Dataset_wheat_yield_kmeans.xlsx` | Input dataset with 96 weather and 2 geographical features per station, standardised via z-scores |

### 📁 2_Model_selection
| File | Description |
|------|-------------|
| `Dataset_wheat_yield.xlsx` | Input dataset (n=265 observations) with 100 features, split 70/10/20 for training, validation, and testing |
| `1_wheat_yield_models.R` | Evaluates Linear Regression and Lasso Regression (λ = 0.01, 0.05, 0.07, 0.10); assessed via RMSE, R², and AIC |
| `2_lasso_selected_lm_models.R` | Re-fits a Linear Regression using only the non-zero predictors identified by Lasso (best model: RMSE=0.334, R²=0.798, AIC=-96.6) |

### 📁 3_Forecast&Prediction
| File | Description |
|------|-------------|
| `Dataset_weather_Prophet_vs_SARIMA.xlsx` | Historical weather data used to train and test Prophet and SARIMA models |
| `3_Forecast_Prophet_vs_SARIMA.R` | Compares Prophet vs SARIMA for forecasting the 8 weather predictors; both trained up to 2024 and tested on 2025 data |
| `4_Prophet_Forecast_Final.R` | Uses Prophet (selected model) to forecast 2026 monthly weather per SA2 |
| `5_LM_Lasso010_yield_forecast.R` | Inputs observed 2025 and forecast 2026 weather into the final LR model to generate FY25/26 and FY26/27 yield predictions per SA2 |

---

## How to Run

Run the scripts in the following order:

```
1_k_means/kmeans_sa2_clustering.R
2_Model_selection/1_wheat_yield_models.R
2_Model_selection/2_lasso_selected_lm_models.R
3_Forecast&Prediction/3_Forecast_Prophet_vs_SARIMA.R
3_Forecast&Prediction/4_Prophet_Forecast_Final.R
3_Forecast&Prediction/5_LM_Lasso010_yield_forecast.R
```

---

## Notes

- All data is standardised using z-scores prior to modelling
- Weather variables forecast: evaporation, rainfall, Tmax, Tmin, humidity
- The model predicts wheat yield (tonnes/ha) but cannot predict total production in tonnes due to data limitations
