frequency_summary <- function(x, type){
  
  id_cols <- c(
    "JoinToAU.AssessmentUnitIdentifier",
    "ATTAINS.ParameterName",
    "TADA.CharacteristicName",
    "TADA.ResultSampleFractionText",
    "TADA.MethodSpeciationName",
    "TADA.ResultMeasure.MeasureUnitCode",
    "ATTAINS.UseName",
    "AcuteChronic",
    "ATTAINS.OrganizationIdentifier",
    "EquationBased",
    "EquationType",
    "DurationUnit",
    "DurationAggregation",
    "DurationValue",
    "FrequencyCriteriaValue",
    "FrequencyCriteriaMethod"
  )
  
  if (type %in% "MLid"){
    id_cols <- c("TADA.MonitoringLocationIdentifier", id_cols)
  } else {
    id_cols <- id_cols
  }
  
  # Remove methods not able to be calculated for now
  x2 <- x |>
    dplyr::filter(FrequencyCriteriaMethod %in% 
                    c("NumberNotMeeting", "n-samples in 3 years",
                      "Percent of samples not meeting", "Percentile"))
  
  # Percentile
  x_P <- x2 |>
    dplyr::filter(FrequencyCriteriaMethod %in% "Percentile")
  
  x_other <- x2 |>
    dplyr::filter(!FrequencyCriteriaMethod %in% "Percentile")
  
  # Copy the Result_Duration value to E_Value if the frequency 
  # is not the percentile method
  if (nrow(x_other) > 0){
    x_other2 <- x_other |> 
      dplyr::mutate(E_Value = Result_Duration) |>
      dplyr::mutate(Percentile = NA_real_)
  } else {
    x_other2 <- x_other |>
      dplyr::mutate(E_Value = NA_real_) |>
      dplyr::mutate(Percentile = NA_real_)
  }
  
  # Apply different methods to each group
  
  # Percentile: Calculate the percentile
  if (nrow(x_P) > 0){
    x_P2 <- x_P |>
      dplyr::group_by(dplyr::across(dplyr::all_of(id_cols))) |>
      dplyr::mutate(FrequencyCriteriaValue = FrequencyCriteriaValue/100) |>
      dplyr::mutate(Percentile = quantile(Result_Duration,
                                          probs = first(FrequencyCriteriaValue))) |>
      dplyr::mutate(E_Value = Percentile) |>
      dplyr::ungroup()
  } else {
    x_P2 <- x_P |> 
      dplyr::mutate(Percentile = NA_real_) |>
      dplyr::mutate(E_Value = NA_real_)
  }
  
  # Evaluate the excursions
  
  x3 <- bind_rows(x_other2, x_P2)
  x4 <- x3 |> duration_excursion_fun()
  
  # Evaluate the exceedance based on id_cols
  # Separate x4 based on FrequencyCriteriaMethod
  x4_number <- x4 |>
    dplyr::filter(FrequencyCriteriaMethod %in% "NumberNotMeeting")
  
  x4_n3years <- x4 |>
    dplyr::filter(FrequencyCriteriaMethod %in% "n-samples in 3 years")
  
  x4_percentage <- x4 |>
    dplyr::filter(FrequencyCriteriaMethod %in% "Percent of samples not meeting")
  
  x4_percentile <- x4 |>
    dplyr::filter(FrequencyCriteriaMethod %in% "Percentile")
  
  # NumberNotMeeting method
  if (nrow(x4_number) > 0){
    x4_number2 <- x4_number |>
      dplyr::group_by(dplyr::across(dplyr::all_of(id_cols))) |>
      dplyr::summarize(Sample_Count = dplyr::n(),
                       Start_Date = min(Window_End_win, na.rm = TRUE),
                       End_Date = max(Window_End_win, na.rm = TRUE),                     
                       Number_of_Excursions = modSum(Duration_Excursion)) |>
      dplyr::mutate(Excursion_Percentage = Number_of_Excursions/Sample_Count * 100) |>
      dplyr::mutate(Exceedance = ifelse(Number_of_Excursions > 0, "Exceed", "Not Exceed")) |>
      dplyr::ungroup() |>
      dplyr::mutate(Percentile = NA_real_) |>
      dplyr::mutate(Sufficient_Data = "Yes")
  } else {
    x4_number2 <- x4_number |>
      dplyr::select(dplyr::all_of(id_cols)) |>
      dplyr::mutate(
        Sample_Count = NA_integer_,
        Start_Date = as.POSIXct(NA),
        End_Date = as.POSIXct(NA),
        Number_of_Excursions = NA_integer_,
        Excursion_Percentage = NA_real_,
        Exceedance = NA_character_,
        Percentile = NA_real_,
        Sufficient_Data = NA_character_
      )
  }
  
  
  # Percent of samples not meeting Method
  if (nrow(x4_percentage) > 0){
    x4_percentage2 <- x4_percentage |>
      dplyr::group_by(dplyr::across(dplyr::all_of(id_cols))) |>
      dplyr::summarize(Sample_Count = dplyr::n(),
                       Start_Date = min(Window_End_win, na.rm = TRUE),
                       End_Date = max(Window_End_win, na.rm = TRUE),                     
                       Number_of_Excursions = modSum(Duration_Excursion)) |>
      dplyr::mutate(Excursion_Percentage = Number_of_Excursions/Sample_Count * 100) |>
      dplyr::mutate(Exceedance = ifelse(Excursion_Percentage > FrequencyCriteriaValue, 
                                        "Exceed", "Not Exceed")) |>
      dplyr::ungroup() |>
      dplyr::mutate(Percentile = NA_real_) |>
      dplyr::mutate(Sufficient_Data = "Yes")
  } else {
    x4_percentage2 <- x4_percentage |>
      dplyr::select(dplyr::all_of(id_cols)) |>
      dplyr::mutate(
        Sample_Count = NA_integer_,
        Start_Date = as.POSIXct(NA),
        End_Date = as.POSIXct(NA),
        Number_of_Excursions = NA_integer_,
        Excursion_Percentage = NA_real_,
        Exceedance = NA_character_,
        Percentile = NA_real_,
        Sufficient_Data = NA_character_
      )
  }
  
  
  # Percentile Method
  if (nrow(x4_percentile) > 0){
    x4_percentile2 <- x4_percentile |>
      dplyr::group_by(dplyr::across(dplyr::all_of(id_cols))) |>
      dplyr::summarize(Sample_Count = dplyr::n(),
                       Start_Date = min(Window_End_win, na.rm = TRUE),
                       End_Date = max(Window_End_win, na.rm = TRUE), 
                       Percentile = first(Percentile),
                       Number_of_Excursions = modSum(Duration_Excursion)) |>
      dplyr::mutate(Exceedance = ifelse(Number_of_Excursions > 0, "Exceed", "Not Exceed")) |>
      dplyr::ungroup() |>
      dplyr::mutate(Number_of_Excursions = NA_real_) |>
      dplyr::mutate(Excursion_Percentage = NA_real_) |>
      dplyr::mutate(Sufficient_Data = "Yes")
  } else {
    x4_percentile2 <- x4_percentile |>
      dplyr::select(dplyr::all_of(id_cols)) |>
      dplyr::mutate(
        Sample_Count = NA_integer_,
        Start_Date = as.POSIXct(NA),
        End_Date = as.POSIXct(NA),
        Number_of_Excursions = NA_integer_,
        Excursion_Percentage = NA_real_,
        Exceedance = NA_character_,
        Percentile = NA_real_,
        Sufficient_Data = NA_character_
      )
  }
  
  
  # "n-samples in 3 years"
  
  # --- n-samples in 3 years ----------------------------------------------------
  # Assumptions:
  # - x4_n3years has one row per window with columns:
  #     Window_End_win (date/time), Duration_Excursion (0/1), FrequencyCriteriaValue
  # - We count windows with Duration_Excursion == 1 within each trailing 3-year span.
  # - We report the "worst" (max excursions) 3-year block per group.
  
  if (nrow(x4_n3years) > 0) {
    # trailing span length: 3 years inclusive
    three_year_span <- lubridate::years(3) - lubridate::days(1)
    
    # helper to coalesce NA excursions to 0
    nz <- function(z) ifelse(is.na(z), 0L, as.integer(z))
    
    x4_n3years2 <- x4_n3years |>
      dplyr::arrange(dplyr::across(dplyr::all_of(id_cols)), Window_End_win) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(id_cols))) |>
      dplyr::group_modify(function(df, keys) {
        idx <- df$Window_End_win
        
        # Check if data exist
        has_span <- {
          earliest <- suppressWarnings(min(idx, na.rm = TRUE))
          latest   <- suppressWarnings(max(idx, na.rm = TRUE))
          if (is.infinite(earliest) || is.infinite(latest)) FALSE
          else (latest - earliest) >= three_year_span
        }
        
        # rolling count of excursions over trailing 3 years
        n_exc_3yr <- slider::slide_index_int(
          .x = nz(df$Duration_Excursion),
          .i = idx,
          .f = sum,
          .before   = three_year_span,
          .complete = FALSE
        )
        
        # number of windows contributing to each 3-year span
        n_win_3yr <- slider::slide_index_int(
          .x = !is.na(df$Duration_Excursion),
          .i = idx,
          .f = sum,
          .before   = three_year_span,
          .complete = FALSE
        )
        
        # corresponding start date of each 3-year span
        start_3yr <- slider::slide_index_vec(
          .x = idx, .i = idx,
          .f = function(v) if (length(v)) min(v) else as.POSIXct(NA),
          .before   = three_year_span,
          .complete = FALSE,
          .ptype = df$Window_End_win
        )
        
        end_3yr <- idx
        
        # pick the "worst" window (max excursions) per group
        worst_i <- which.max(ifelse(is.na(n_exc_3yr), -Inf, n_exc_3yr))
        if (length(worst_i) == 0L || is.infinite(max(n_exc_3yr, na.rm = TRUE))) {
          return(dplyr::tibble())
        }
        
        Number_of_Excursions <- n_exc_3yr[worst_i]
        Sample_Count         <- n_win_3yr[worst_i]
        Start_Date           <- start_3yr[worst_i]
        End_Date             <- end_3yr[worst_i]
        
        # compare to allowable count in 3 years
        allow_n <- suppressWarnings(as.integer(df$FrequencyCriteriaValue[worst_i]))
        # if NA, treat as 0 allowed (or choose your policy)
        if (is.na(allow_n)) allow_n <- 0L
        
        Exceedance <- ifelse(Number_of_Excursions > allow_n, "Exceed", "Not Exceed")
        
        dplyr::tibble(
          Sample_Count         = Sample_Count,
          Start_Date           = Start_Date,
          End_Date             = End_Date,
          Number_of_Excursions = Number_of_Excursions,
          Excursion_Percentage = NA_real_,
          Exceedance           = Exceedance,
          Percentile           = NA_real_,
          Sufficient_Data      = "Yes"
        )
      }, .keep = TRUE) |>
      dplyr::ungroup()
  } else {
    x4_n3years2 <- x4_n3years |>
      dplyr::select(dplyr::all_of(id_cols)) |>
      dplyr::mutate(
        Sample_Count = NA_integer_,
        Start_Date = as.POSIXct(NA),
        End_Date = as.POSIXct(NA),
        Number_of_Excursions = NA_integer_,
        Excursion_Percentage = NA_real_,
        Exceedance = NA_character_,
        Percentile = NA_real_,
        Sufficient_Data = NA_character_ 
      )
  }
  
  # Combine the data
  x5 <- bind_rows(x4_number2, x4_percentage2, x4_percentile2, x4_n3years2) |>
    dplyr::relocate("Exceedance", .after = "Percentile")
  
  return(x5)
}