

data <- combine_manual_data()



data_clean <- data |> 
  purrr::map(\(df) {
    df |> dplyr::mutate(Period = standardize_garmin_date(Period))
  })


weekly_metrics <- base::list(
  data_clean$Steps.csv,
  data_clean$Stress.csv,
  data_clean$`Resting Heart Rate (1).csv`,
  data_clean$Calories.csv
) |> 
  purrr::map(\(df) {
    df |> 
      dplyr::mutate(Week = lubridate::floor_date(Period, "week", week_start = 1)) |> 
      dplyr::group_by(Week) |> 
      dplyr::summarise(dplyr::across(where(is.numeric), ~ base::mean(.x, na.rm = TRUE)))
  }) |> 
  purrr::reduce(dplyr::left_join, by = "Week")

weekly_sleep <- data_clean$`Sleep (1).csv` |> 
  dplyr::mutate(Week = lubridate::floor_date(Period, "week", week_start = 1)) |> 
  dplyr::select(-Period)


final_analysis_df <- weekly_sleep |> 
  dplyr::left_join(weekly_metrics, by = "Week")

final_analysis_df <- final_analysis_df |> 
  dplyr::mutate(
    # 1. Robust Bedtime parsing (Henter tal direkte)
    # Vi finder timer og minutter og tjekker om det er PM
    temp_h = base::as.numeric(base::gsub("^([0-9]+):.*", "\\1", Bedtime)),
    temp_m = base::as.numeric(base::gsub("^[0-9]+:([0-9]+).*", "\\1", Bedtime)),
    is_pm = base::grepl("PM", Bedtime),
    
    # Konverter til timer efter middag (22:30 -> 10.5, 01:00 -> 13.0)
    Bedtime_Numeric = dplyr::case_when(
      is_pm & temp_h != 12 ~ temp_h + (temp_m / 60),
      is_pm & temp_h == 12 ~ 12 + (temp_m / 60),
      !is_pm & temp_h == 12 ~ 0 + (temp_m / 60),
      !is_pm ~ temp_h + 12 + (temp_m / 60) # AM bliver til timer efter middag (1 AM = 13 timer efter middag)
    ),
    
    # 2. Håndter NA i kalorier (Sæt til 0 eller brug måneds-gennemsnit)
    # Hvis du har trænet den måned, men ugen er NA, så udfylder vi med månedsdata
    dplyr::across(dplyr::contains("Calories"), ~ dplyr::coalesce(.x, 0))
  ) |> 
  # Vi fjerner kun rækker, hvor vi reelt mangler søvndata
  dplyr::filter(!base::is.na(Sleep_Score))

# Tjek nu antallet af rækker
base::nrow(final_analysis_df)


# Beregn korrelationer for alle numeriske variable mod Sleep_Score
cor_results <- final_analysis_df |> 
  dplyr::select(Sleep_Score, Stress, Steps, `Resting Heart Rate`, Bedtime_Numeric, Active_Calories) |> 
  stats::cor(use = "complete.obs")

# Vis kun hvad der påvirker søvnen
base::sort(cor_results["Sleep_Score", ], decreasing = TRUE)
