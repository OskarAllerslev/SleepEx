
data <- get_finalized_data()
# Visualisering af de to primære drivere
ggplot2::ggplot(data, 
                ggplot2::aes(x = Bedtime_Numeric, y = Sleep_Score)) +
  # Visualiser usikkerheden og den ikke-lineære sammenhæng
  ggplot2::geom_smooth(method = "lm", 
                       formula = y ~ stats::poly(x, 2), 
                       color = "#2C3E50", 
                       fill = "#BDC3C7", 
                       alpha = 0.2) +
  ggplot2::geom_point(ggplot2::aes(color = Stress, size = Active_Calories), 
                      alpha = 0.8) +
  # Tving aksen til at være realistisk (Garmin score er 0-100)
  ggplot2::scale_y_continuous(limits = c(60, 100)) +
  ggplot2::scale_color_gradientn(colors = base::rev(grDevices::terrain.colors(10))) +
  ggplot2::labs(
    title = "Ugentlig Søvnoptimering: Find dit 'Sweet Spot'",
    subtitle = "Modellen tager højde for interaktion mellem stress og timing",
    x = "Sengetid (Timer efter middag)",
    y = "Garmin Sleep Score",
    color = "Stress",
    size = "Træning (Cal)"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "right")

