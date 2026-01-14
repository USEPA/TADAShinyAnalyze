# Step 2: windowed statistics with covariates as MEANS,
# and Result_Duration chosen by DurationAggregation (mean / max / min / extremes)
duration_cal <- function(dat) {
  
  stopifnot(all(c(
    "Window_Start","Window_End","canonical_step",
    "DurationUnit","DurationValue","DurationAggregation",
    "Value_canon","pH_canon","Temperature_canon","Hardness_canon"
  ) %in% names(dat)))
  
  # Inclusive window span by DurationUnit & DurationValue
  window_before_period <- function(unit, value) {
    if (is.na(value)) value <- 1
    if (unit == "n-hour")   return(lubridate::hours(max(value, 1) - 1))
    if (unit == "n-day")    return(lubridate::days(max(value, 1) - 1))
    if (unit == "n-month")  return(lubridate::months(max(value, 1)) - lubridate::days(1))
    if (unit == "n-season") return(lubridate::months(3L * max(value, 1)) - lubridate::days(1))
    lubridate::days(max(value, 1) - 1)
  }
  
  step_label <- function(step) ifelse(step == "hour", "1 hour", "1 day")
  
  # Grouping keys (no time columns)
  id_cols <- c(
    "TADA.MonitoringLocationIdentifier",
    "ATTAINS.ParameterName",
    "TADA.CharacteristicName",
    "TADA.ResultSampleFractionText",
    "TADA.MethodSpeciationName",
    "TADA.ResultMeasure.MeasureUnitCode",
    "ATTAINS.UseName",
    "AcuteChronic",
    "ATTAINS.OrganizationIdentifier",
    "JoinToAU.AssessmentUnitIdentifier",
    "DurationUnit",
    "DurationAggregation",
    "DurationValue"
  )
  id_cols <- base::intersect(id_cols, names(dat))
  
  dat_ordered <- dat |>
    dplyr::arrange(dplyr::across(dplyr::all_of(id_cols)), Window_Start)
  
  out <- dat_ordered |>
    dplyr::group_by(dplyr::across(dplyr::all_of(id_cols))) |>
    dplyr::group_modify(function(df, keys) {
      
      idx        <- df$Window_Start
      before_per <- window_before_period(df$DurationUnit[1], df$DurationValue[1])
      agg_label  <- df$DurationAggregation[1]
      
      # Measurement windows
      win_mean <- slider::slide_index_dbl(df$Value_canon, idx,
                                          ~ base::mean(.x, na.rm = TRUE),
                                          .before = before_per, .complete = TRUE
      )
      win_min <- slider::slide_index_dbl(df$Value_canon, idx,
                                         ~ suppressWarnings(base::min(.x, na.rm = TRUE)),
                                         .before = before_per, .complete = TRUE
      )
      win_max <- slider::slide_index_dbl(df$Value_canon, idx,
                                         ~ suppressWarnings(base::max(.x, na.rm = TRUE)),
                                         .before = before_per, .complete = TRUE
      )
      
      Result_Duration <- dplyr::case_when(
        agg_label == "arithmetic mean"     ~ win_mean,
        agg_label == "arithmetic max"      ~ win_max,
        agg_label == "arithmetic min"      ~ win_min,
        agg_label == "arithmetic extremes" ~ NA_real_,
        TRUE                               ~ win_mean
      )
      
      # Covariates — always mean
      pH_win <- slider::slide_index_dbl(df$pH_canon, idx,
                                        ~ base::mean(.x, na.rm = TRUE),
                                        .before = before_per, .complete = TRUE
      )
      Temperature_win <- slider::slide_index_dbl(df$Temperature_canon, idx,
                                                 ~ base::mean(.x, na.rm = TRUE),
                                                 .before = before_per, .complete = TRUE
      )
      Hardness_win <- slider::slide_index_dbl(df$Hardness_canon, idx,
                                              ~ base::mean(.x, na.rm = TRUE),
                                              .before = before_per, .complete = TRUE
      )
      
      # Counts & explicit window bounds
      N_in_Window <- slider::slide_index_int(!is.na(df$Value_canon), idx,
                                             sum, .before = before_per, .complete = TRUE
      )
      Window_Start_win <- slider::slide_index_vec(idx, idx,
                                                  ~ if (length(.x) > 0) base::min(.x) else as.POSIXct(NA),
                                                  .before = before_per, .complete = TRUE, .ptype = df$Window_Start
      )
      Window_End_win <- idx
      
      dplyr::tibble(
        Window_Start_win = Window_Start_win,
        Window_End_win   = Window_End_win,
        N_in_Window      = N_in_Window,
        Window_Step      = step_label(df$canonical_step[1]),
        Stat_Method      = agg_label,
        Result_Duration  = Result_Duration,
        Value_win_min    = win_min,
        Value_win_max    = win_max,
        pH_win           = pH_win,
        Temperature_win  = Temperature_win,
        Hardness_win     = Hardness_win
      )
    }, .keep = TRUE) |>
    dplyr::ungroup()
  
  # Ensure canonical_step present
  if (!"canonical_step" %in% names(out) && "canonical_step" %in% names(dat_ordered)) {
    out <- out |>
      dplyr::mutate(canonical_step = dat_ordered$canonical_step)
  }
  
  out
}
