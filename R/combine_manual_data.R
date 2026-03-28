#' @export
combine_manual_data <- function(path = "data-raw") {
  csv_filer <- base::list.files(path = path, pattern = "\\.csv$", full.names = TRUE)
  base::names(csv_filer) <- base::basename(csv_filer)

  all_dat <- purrr::map(csv_filer, \(fil) {
    fname <- base::basename(fil)

    if (base::grepl("Sleep", fname)) {
      raw_lines <- readr::read_lines(fil)
      data_lines <- raw_lines[-(1:2)]
      
      clean_df <- purrr::map_dfr(data_lines, \(line) {
        parts <- base::strsplit(line, ",")[[1]]
        n <- base::length(parts)
        period_str <- base::paste(parts[1:(n-6)], collapse = ", ")
        metrics <- parts[(n-5):n]
        
        base::data.frame(
          Period = period_str,
          Sleep_Score = base::as.numeric(metrics[1]),
          Quality = metrics[2],
          Sleep_Duration = metrics[3],
          Avg_Sleep_Duration = metrics[4],
          Bedtime = metrics[5],
          Wake_up = metrics[6],
          stringsAsFactors = FALSE
        )
      })
    } else {
      df <- readr::read_csv(fil, skip = 1, col_names = FALSE, show_col_types = FALSE)
      base::colnames(df)[1] <- "Period"
      
      if (base::grepl("Calories.csv$", fname)) {
        base::colnames(df)[1:4] <- c("Period", "Active_Calories", "Resting_Calories", "Total_Calories")
      } else if (base::ncol(df) == 2) {
        # Stringent navngivning: fjerner (1), fjerner .csv og erstatter mellemrum med _
        val_name <- base::gsub("\\.csv", "", fname) |> 
          base::gsub(" \\(1\\)", "", x = _) |> 
          base::gsub(" ", "_", x = _)
        base::colnames(df)[2] <- val_name
      }
      clean_df <- df
    }

    clean_df <- clean_df |>
      dplyr::mutate(dplyr::across(
        .cols = dplyr::where(~ base::any(base::grepl("h.*min", base::as.character(.x)))),
        .fns = \(x) {
          h <- base::as.numeric(base::gsub(".*\\b([0-9]+)h.*", "\\1", x))
          m <- base::as.numeric(base::gsub(".*\\b([0-9]+)min.*", "\\1", x))
          h[base::is.na(h)] <- 0; m[base::is.na(m)] <- 0
          return(h + (m / 60))
        }
      ))
    return(clean_df)
  })
  return(all_dat)
}

#' Fixer årstal i Garmins ugentlige søvn-strenge
#' @param x Vektor med strenge som "Mar 22-28"
fix_sleep_years <- function(x) {
  current_year <- 2026
  out_dates <- base::as.Date(base::rep(NA, base::length(x)))
  
  # Find månederne som tal
  months_num <- base::match(base::gsub(" .*", "", x), month.abb)
  
  for (i in base::seq_along(x)) {
    # Hvis måneden hopper (fx Jan -> Dec), er vi gået et år tilbage
    if (i > 1 && !base::is.na(months_num[i]) && !base::is.na(months_num[i-1])) {
      if (months_num[i] > months_num[i-1]) {
        current_year <- current_year - 1
      }
    }
    
    clean_day <- base::gsub("-.*", "", x[i])
    out_dates[i] <- lubridate::mdy(base::paste0(clean_day, ", ", current_year))
  }
  return(out_dates)
}

#' Standardiser Garmin datoer
#' @param x En vektor af perioder fra Garmin
standardize_garmin_date <- function(x) {
  if (base::any(base::grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$", x))) {
    return(lubridate::mdy(x))
  }
  if (base::any(base::grepl("^[A-Za-z]{3} [0-9]{4}$", x))) {
    return(lubridate::my(x))
  }
  # Hvis vi ikke kender formatet, returnerer vi NA som Date i stedet for at crashe
  return(base::as.Date(base::rep(NA, base::length(x))))
}

#' @export
get_finalized_data <- function() {
  data <- combine_manual_data()
  
  data_clean <- purrr::imap(data, \(df, name) {
    if (base::grepl("Sleep", name)) return(df)
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
        dplyr::filter(!base::is.na(Period)) |> 
        dplyr::mutate(Week = lubridate::floor_date(Period, "week", week_start = 1)) |> 
        dplyr::group_by(Week) |> 
        dplyr::summarise(dplyr::across(where(is.numeric), ~ base::mean(.x, na.rm = TRUE)), .groups = "drop")
    }) |> 
    purrr::reduce(dplyr::left_join, by = "Week")

  final_df <- data_clean$`Sleep (1).csv` |> 
    dplyr::mutate(
      Period_Date = fix_sleep_years(Period),
      Week = lubridate::floor_date(Period_Date, "week", week_start = 1)
    ) |> 
    dplyr::left_join(weekly_metrics, by = "Week") |> 
    dplyr::mutate(
      bt_time = lubridate::parse_date_time(Bedtime, "%I:%M %p"),
      h = lubridate::hour(bt_time),
      m = lubridate::minute(bt_time),
      Bedtime_Numeric = dplyr::case_when(
        h >= 12 ~ (h - 12) + (m / 60),
        h < 12  ~ (h + 12) + (m / 60),
        base::is.na(h) ~ NA_real_
      )
    ) |> 
    # Her bruger vi de rene navne uden mellemrum
    dplyr::mutate(dplyr::across(dplyr::contains("Calories"), ~ dplyr::coalesce(.x, 0))) |> 
    dplyr::select(-bt_time, -h, -m, -Period_Date) |> 
    dplyr::filter(!base::is.na(Sleep_Score))

  return(final_df)
}



fix_sleep_years <- function(x) {
  current_year <- 2026
  out_dates <- base::as.Date(base::rep(NA, base::length(x)))
  
  months_num <- base::match(base::gsub("^([A-Za-z]{3}).*", "\\1", x), month.abb)
  
  for (i in base::seq_along(x)) {
    if (base::grepl(", [0-9]{4}", x[i])) {
      year_match <- base::regmatches(x[i], base::regexpr("[0-9]{4}", x[i]))
      current_year <- base::as.numeric(year_match)
    } else if (i > 1 && !base::is.na(months_num[i]) && !base::is.na(months_num[i-1])) {
      if (months_num[i] > months_num[i-1]) {
        current_year <- current_year - 1
      }
    }
    
    clean_day <- base::gsub("^([A-Za-z]{3} [0-9]{1,2}).*", "\\1", x[i])
    out_dates[i] <- lubridate::mdy(base::paste0(clean_day, ", ", current_year))
  }
  return(out_dates)
}