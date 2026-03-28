
dat <- combine_manual_data()

data <- get_finalized_data()

# Kør modellen på dit finalized data
sleep_model <- stats::lm(
  Sleep_Score ~ 
    stats::poly(Bedtime_Numeric, 2, raw = TRUE) + 
    Stress + 
    Resting_Heart_Rate + 
    Active_Calories,
  data = data
)

base::summary(sleep_model)



sjPlot::plot_model(sleep_model, type = "pred", terms = "Bedtime_Numeric [all]") +
  ggplot2::theme_minimal() +
  ggplot2::labs(title = "Marginal effekt af sengetid på Sleep Score",
                x = "Timer efter middag",
                y = "Forventet Sleep Score")





# library(forecast)

# xreg_cols <- data |> 
#   dplyr::select(Bedtime_Numeric, Stress, Resting_Heart_Rate, Active_Calories) |> 
#   base::as.matrix()

# clean_idx <- base::which(base::complete.cases(xreg_cols) & !base::is.na(data$Sleep_Score))
# y <- data$Sleep_Score[clean_idx]
# xreg <- xreg_cols[clean_idx, ]

# # ARIMAX(1,0,0) - Auto-regressiv model med eksterne variable
# arimax_model <- forecast::Arima(y, order = c(1, 0, 0), xreg = xreg)

# base::summary(arimax_model)