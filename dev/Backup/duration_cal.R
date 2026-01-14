duration_cal <- function(x, type, complete_windows = TRUE){
  # Create/standardize window columns
  x <- x |>
    dplyr::mutate(Window_Start = DateTime) |>
    dplyr::mutate(Window_End = DateTime) |>
    dplyr::mutate(DurationAggregation_norm = trimws(tolower(DurationAggregation)))
  
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
  
  x_ordered <- x |>
    dplyr::arrange(dplyr::across(dplyr::all_of(id_cols)), Window_Start)
  
  result <- x_ordered |>
    dplyr::group_by(dplyr::across(dplyr::all_of(id_cols))) |>
    dplyr::mutate(G_ID = dplyr::cur_group_id()) |>
    dplyr::group_modify(function(x2, keys){
      df <- x2 |>
        dplyr::arrange(Window_Start)
      
      idx        <- df$Window_Start
      before_per <- window_before_period(df$DurationUnit[1], df$DurationValue[1])
      agg_raw    <- df$DurationAggregation[1]
      agg_norm   <- df$DurationAggregation_norm[1]
      
      # Measurement windows (compute multiple stats so we can map by label)
      win_mean   <- slider::slide_index_dbl(df$Value, idx, na_mean,  .before = before_per, .complete = complete_windows)
      win_min    <- slider::slide_index_dbl(df$Value, idx, na_min,   .before = before_per, .complete = complete_windows)
      win_max    <- slider::slide_index_dbl(df$Value, idx, na_max,   .before = before_per, .complete = complete_windows)
      win_gmean  <- slider::slide_index_dbl(df$Value, idx, na_gmean, .before = before_per, .complete = complete_windows)
      
      # Also compute window min/max for "extremes" evaluation later
      Value_win_min <- win_min
      Value_win_max <- win_max
      
      Result_Duration <- dplyr::case_when(
        agg_norm %in% c("arithmetic mean", "rolling arithmetic mean") ~ win_mean,
        agg_norm %in% "arithmetic max"       ~ win_max,
        agg_norm %in% "arithmetic min"       ~ win_min,
        agg_norm %in% c("geometric mean")    ~ win_gmean,
        agg_norm %in% "arithmetic extremes"  ~ NA_real_,  # use Value_win_min / Value_win_max vs thresholds later
        TRUE                                 ~ win_mean
      )
      
      # Covariates — always means
      pH_win          <- slider::slide_index_dbl(df$pH,          idx, na_mean, .before = before_per, .complete = complete_windows)
      Temperature_win <- slider::slide_index_dbl(df$Temperature, idx, na_mean, .before = before_per, .complete = complete_windows)
      Hardness_win    <- slider::slide_index_dbl(df$Hardness,    idx, na_mean, .before = before_per, .complete = complete_windows)
      
      # Counts & explicit bounds
      N_in_Window      <- slider::slide_index_int(!is.na(df$Value), idx, sum, .before = before_per, .complete = complete_windows)
      Window_Start_win <- slider::slide_index_vec(idx, idx, 
                                                  function(x){
                                                    if (length(x)){
                                                      return(min(x)) 
                                                    } else {
                                                      return(as.POSIXct(NA))
                                                    }},
                                                  .before = before_per, .complete = complete_windows, .ptype = df$Window_Start)
      Window_End_win   <- idx
      
      # Status logic
      time_complete <- !is.na(Window_Start_win)
      Window_Status <- dplyr::case_when(
        !time_complete                                  ~ "incomplete",
        TRUE                                            ~ "complete"
      )
      
      dplyr::tibble(
        G_ID = df$G_ID[1],
        # keep canonical point value for reference
        Value        = df$Value,
        # window metadata
        Window_Start_win   = Window_Start_win,
        Window_End_win     = Window_End_win,
        Window_Step        = step_label(df$DurationUnit[1]),
        N_in_Window        = N_in_Window,
        Stat_Method        = agg_raw,
        # windowed measurement
        Result_Duration    = Result_Duration,
        Value_win_min      = Value_win_min,
        Value_win_max      = Value_win_max,
        # thresholds (non-equation)
        Threshold_Lower_win = df$MagnitudeValueLower[1],
        Threshold_Upper_win = df$MagnitudeValueUpper[1],
        # windowed covariates
        pH_win             = pH_win,
        Temperature_win    = Temperature_win,
        Hardness_win       = Hardness_win,
        # status
        Window_Status      = Window_Status
      )
    }, .keep = TRUE) |>
    dplyr::ungroup() |>
    dplyr::select(-G_ID)
  
  return(result)
} 