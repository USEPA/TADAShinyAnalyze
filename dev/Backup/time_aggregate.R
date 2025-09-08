time_aggregate <- function(x, type){
  
  x <- x |>
    dplyr::rename(Date = ActivityStartDate) |>
    dplyr::mutate(DateTime = lubridate::as_datetime(DateTime))
  
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
  
  # Collapse duplicate samples at the SAME DateTime (per id_cols) via mean
  collapsed <- x |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(id_cols, "Date", "DateTime")))) |>
    dplyr::summarise(
      Value                 = if (all(is.na(TADA.ResultMeasureValue))) NA_real_ else mean(TADA.ResultMeasureValue, na.rm = TRUE),
      MagnitudeValueLower   = if (all(is.na(MagnitudeValueLower)))     NA_real_ else mean(MagnitudeValueLower,    na.rm = TRUE),
      MagnitudeValueUpper   = if (all(is.na(MagnitudeValueUpper)))     NA_real_ else mean(MagnitudeValueUpper,    na.rm = TRUE),
      pH                    = if (all(is.na(pH)))                      NA_real_ else mean(pH,                     na.rm = TRUE),
      Temperature           = if (all(is.na(Temperature)))             NA_real_ else mean(Temperature,            na.rm = TRUE),
      Hardness              = if (all(is.na(Hardness)))                NA_real_ else mean(Hardness,               na.rm = TRUE),
      N_in_Step             = dplyr::n(),
      .groups = "drop"
    ) 
  
  # Hourly canonical series (post-collapse)
  hourly <- collapsed |>
    dplyr::filter(DurationUnit %in% "n-hour")
  
  # Daily canonical series (mean of timestamp-collapsed values per day)
  daily <- collapsed |>
    dplyr::filter(DurationUnit %in% c("n-day", "n-season", "n-month")) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(id_cols, "Date")))) |>
    dplyr::summarise(
      Value                = if (all(is.na(Value))) NA_real_ else mean(Value, na.rm = TRUE),
      MagnitudeValueLower  = if (all(is.na(MagnitudeValueLower))) NA_real_ else mean( MagnitudeValueLower, na.rm = TRUE),
      MagnitudeValueUpper  = if (all(is.na(MagnitudeValueUpper))) NA_real_ else mean(MagnitudeValueUpper, na.rm = TRUE),
      pH                   = if (all(is.na(pH)))              NA_real_ else mean(pH, na.rm = TRUE),
      Temperature          = if (all(is.na(Temperature)))  NA_real_ else mean(Temperature, na.rm = TRUE),
      Hardness             = if (all(is.na(Hardness)))     NA_real_ else mean(Hardness, na.rm = TRUE),
      N_in_Step            = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      # Create the DateTime column for consistency
      DateTime           = as.POSIXct(Date)
    ) 
  
  # Bind and sort
  result <- dplyr::bind_rows(hourly, daily)
  
  result <- result |>
    dplyr::arrange(dplyr::across(dplyr::all_of(id_cols)), DateTime)
  
  
  return(result)
}
