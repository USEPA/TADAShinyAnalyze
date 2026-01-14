time_aggregate <- function(
    dat5,
    id_cols = c(
      "TADA.MonitoringLocationIdentifier",
      "ATTAINS.ParameterName",
      "TADA.CharacteristicName",
      "TADA.ResultSampleFractionText",
      "TADA.MethodSpeciationName",
      "TADA.ResultMeasure.MeasureUnitCode",
      "ATTAINS.UseName",
      "AcuteChronic",
      "ATTAINS.OrganizationIdentifier"
    )
) {

  dat5 <- dat5 |>
    dplyr::mutate(
      DateTime = lubridate::as_datetime(DateTime),
      Date     = as.Date(DateTime)
    )
  
  # Context columns that define a unique rule/application bucket
  context_cols <- c(
    id_cols,
    "JoinToAU.AssessmentUnitIdentifier",
    "DurationUnit",
    "DurationAggregation",
    "DurationValue",
    "TADA.ResultMeasure.MeasureUnitCode"
  )
  context_cols <- base::intersect(context_cols, names(dat5))
  
  # 1) Collapse duplicate samples at the SAME DateTime (per context) via mean
  collapsed <- dat5 |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(context_cols, "DateTime")))) |>
    dplyr::summarise(
      # measurement at timestamp (mean)
      Value_ts        = base::mean(TADA.ResultMeasureValue, na.rm = TRUE),
      # covariates at timestamp (mean)
      pH_ts           = base::mean(pH, na.rm = TRUE),
      Temperature_ts  = base::mean(Temperature, na.rm = TRUE),
      Hardness_ts     = base::mean(Hardness, na.rm = TRUE),
      N_at_time       = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      canonical_step = dplyr::if_else(DurationUnit == "n-hour", "hour", "day")
    )
  
  # Which units should be aggregated to daily
  needs_daily_units <- c("n-day", "n-season", "n-month")
  
  # 2a) Hourly canonical series: keep post-collapse rows as-is
  hourly <- collapsed |>
    dplyr::filter(canonical_step == "hour") |>
    dplyr::transmute(
      dplyr::across(dplyr::all_of(context_cols)),
      canonical_step,
      Window_Start       = DateTime,
      Window_End         = DateTime,
      N_in_Step          = N_at_time,
      Value_canon        = Value_ts,
      pH_canon           = pH_ts,
      Temperature_canon  = Temperature_ts,
      Hardness_canon     = Hardness_ts
    )
  
  # 2b) Daily canonical series: aggregate timestamp-collapsed rows to daily means
  daily <- collapsed |>
    dplyr::filter(DurationUnit %in% needs_daily_units) |>
    dplyr::mutate(Date = as.Date(DateTime)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(context_cols, "Date")))) |>
    dplyr::summarise(
      Value_day_mean    = base::mean(Value_ts, na.rm = TRUE),
      pH_day            = base::mean(pH_ts, na.rm = TRUE),
      Temperature_day   = base::mean(Temperature_ts, na.rm = TRUE),
      Hardness_day      = base::mean(Hardness_ts, na.rm = TRUE),
      N_in_Step         = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      canonical_step     = "day",
      Value_canon        = Value_day_mean,
      pH_canon           = pH_day,
      Temperature_canon  = Temperature_day,
      Hardness_canon     = Hardness_day,
      Window_Start       = as.POSIXct(Date),
      Window_End         = as.POSIXct(Date)
    ) |>
    dplyr::select(
      dplyr::all_of(context_cols),
      canonical_step,
      Window_Start, Window_End, N_in_Step,
      Value_canon, pH_canon, Temperature_canon, Hardness_canon
    )
  
  # 3) Bind
  result <- dplyr::bind_rows(hourly, daily)
  
  return(result)
}
