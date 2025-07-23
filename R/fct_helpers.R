#' helpers 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

### A function to join the criteria
### TODO check the columns used to join the criteria
criteria_join <- function(x, y){
  
  x2 <- x |>
    dplyr::left_join(y, by = c("TADA.CharacteristicName",
                               "TADA.ResultSampleFractionText" = "Fraction",
                               "TADA.ResultMeasure.MeasureUnitCode" = "MagnitudeUnit",
                               "ATTAINS.UseName"
    ),
    relationship = "many-to-many")
  
  return(x2)
}

### A function to join the pH data
pH_filter <- function(x){
  x2 <- x |>
    dplyr::filter(TADA.CharacteristicName %in% "PH") |>
    dplyr::select(DateTime,
                  TADA.MonitoringLocationIdentifier, TADA.MonitoringLocationTypeName,
                  TADA.LatitudeMeasure, TADA.LongitudeMeasure,
                  pH = TADA.ResultMeasureValue) |>
    # Calculate average if multiple samples exist
    dplyr::group_by(dplyr::across(-pH)) |>
    dplyr::summarize(pH = mean(pH, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    # Create the upper and lower bound
    dplyr::mutate(DateTime_upper = DateTime + lubridate::days(1),
                  DateTime_lower = DateTime - lubridate::days(1))
  
  return(x2)
}

pH_join <- function(x, y){
  
  by <- dplyr::join_by(TADA.MonitoringLocationIdentifier, TADA.MonitoringLocationTypeName,
                       TADA.LatitudeMeasure, TADA.LongitudeMeasure,
                       dplyr::closest(DateTime >= DateTime_lower), 
                       closest(DateTime <= DateTime_upper))
  
  x2 <- x |>
    dplyr::left_join(y, by = by) |>
    dplyr::rename(DateTime = DateTime.x, DateTime_pH =  DateTime.y) |>
    dplyr::select(-DateTime_lower, -DateTime_upper)
  
  return(x2)
}

pH_fun <- function(x){
  pH_dat <- x |> 
    pH_filter()
  x2 <- x |>
    pH_join(pH_dat)
  return(x2)
}

### A function to join the temperature data
temp_filter <- function(x){
  x2 <- x |>
    dplyr::filter(TADA.CharacteristicName %in% "TEMPERATURE, WATER") |>
    dplyr::select(DateTime,
                  TADA.MonitoringLocationIdentifier, TADA.MonitoringLocationTypeName,
                  TADA.LatitudeMeasure, TADA.LongitudeMeasure,
                  Temperature = TADA.ResultMeasureValue) |>
    # Calculate average if multiple samples exist
    dplyr::group_by(dplyr::across(-Temperature)) |>
    dplyr::summarize(Temperature = mean(Temperature, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    # Create the upper and lower bound
    dplyr::mutate(DateTime_upper = DateTime + lubridate::days(1),
                  DateTime_lower = DateTime - lubridate::days(1))
  
  return(x2)
}

temp_join <- function(x, y){
  
  by <- dplyr::join_by(TADA.MonitoringLocationIdentifier, TADA.MonitoringLocationTypeName,
                       TADA.LatitudeMeasure, TADA.LongitudeMeasure,
                       dplyr::closest(DateTime >= DateTime_lower), 
                       closest(DateTime <= DateTime_upper))
  
  x2 <- x |>
    dplyr::left_join(y, by = by) |>
    dplyr::rename(DateTime = DateTime.x, DateTime_Temperature =  DateTime.y) |>
    dplyr::select(-DateTime_lower, -DateTime_upper)
  
  return(x2)
}

Temperature_fun <- function(x){
  temp_dat <- x |> 
    temp_filter()
  x2 <- x |>
    temp_join(temp_dat)
  return(x2)
}

### A function to join the hardness data
hardness_filter <- function(x){
  x2 <- x |>
    dplyr::filter(TADA.CharacteristicName %in% "HARDNESS, CA, MG") |>
    dplyr::select(ActivityStartDate, `ActivityStartTime.Time`,
                  TADA.MonitoringLocationIdentifier, TADA.MonitoringLocationTypeName,
                  TADA.LatitudeMeasure, TADA.LongitudeMeasure,
                  Hardness = TADA.ResultMeasureValue) |>
    # Calculate average if multiple samples exist
    dplyr::group_by(dplyr::across(-Hardness)) |>
    dplyr::summarize(Hardness = mean(Hardness, na.rm = TRUE)) |>
    dplyr::ungroup() 
  return(x2)
}

hardness_join <- function(x, y){
  x2 <- x |>
    dplyr::left_join(y, by = c(
      "ActivityStartDate", "ActivityStartTime.Time",
      "TADA.MonitoringLocationIdentifier", "TADA.MonitoringLocationTypeName",
      "TADA.LatitudeMeasure", "TADA.LongitudeMeasure"
    ))
  return(x2)
}

hardness_fun <- function(x){
  hard_dat <- x |> 
    hardness_filter()
  x2 <- x |>
    hardness_join(hard_dat) |>
    # Limit the hardness
    dplyr::mutate(Hardness = ifelse(Hardness > 400, 400, Hardness))
  return(x2)
}

### A function to compare the exceedances
exceedance_fun <- function(x){
  x2 <- x |>
    dplyr::mutate(Exceedance = dplyr::case_when(
      is.na(MagnitudeValueLower) & !is.na(MagnitudeValueUpper) & 
        TADA.ResultMeasureValue > MagnitudeValueUpper    ~   TRUE,
      !is.na(MagnitudeValueLower) & is.na(MagnitudeValueUpper) & 
        TADA.ResultMeasureValue < MagnitudeValueLower    ~   TRUE,
      !is.na(MagnitudeValueLower) & !is.na(MagnitudeValueUpper) & 
        (TADA.ResultMeasureValue < MagnitudeValueLower | 
           TADA.ResultMeasureValue > MagnitudeValueUpper)   ~   TRUE,
      TRUE                                             ~  FALSE
    ))
  return(x2)
}

# A function to return NA if all values are NA, otherwise
# DO sum(x, na.rm = TRUE)
modSum <- function(x){
  if(all(is.na(x))){
    y <- NA
  } else {
    y <- sum(x, na.rm = TRUE)
  }
  return(y)
}

# A function to calculate the exceedance percentage data with criteria
exceedance_summary <- function(x, type){
  
  # A look up table for "TADA.MonitoringLocationIdentifier", "TADA.MonitoringLocationName",
  # "JoinToAU.AssessmentUnitIdentifier", "TADA.LongitudeMeasure", "TADA.LatitudeMeasure"
  
  coords <- dplyr::distinct(x, 
                            TADA.MonitoringLocationIdentifier,
                            TADA.MonitoringLocationName,
                            JoinToAU.AssessmentUnitIdentifier,
                            TADA.LongitudeMeasure,
                            TADA.LatitudeMeasure)
  
  if(type %in% c("MLid", "AU_ind")){
    x2 <- x |>
      dplyr::group_by(dplyr::across(
        dplyr::all_of(c("TADA.MonitoringLocationIdentifier", "TADA.MonitoringLocationName",
                        "JoinToAU.AssessmentUnitIdentifier", "ATTAINS.UseName",
                        "TADA.LongitudeMeasure", "TADA.LatitudeMeasure",
                        "TADA.CharacteristicName", "TADA.ResultSampleFractionText",
                        "TADA.ResultMeasure.MeasureUnitCode", "AcuteChronic",
                        "DurationValue", "DurationUnit", "DurationAggregation",
                        "FrequencyCriteriaValue", "FrequencyCriteriaMethod"))))
  } else {
    x2 <- x |>
      dplyr::group_by(dplyr::across(
        dplyr::all_of(c("JoinToAU.AssessmentUnitIdentifier", "ATTAINS.UseName",
                        "TADA.CharacteristicName", "TADA.ResultSampleFractionText",
                        "TADA.ResultMeasure.MeasureUnitCode", "AcuteChronic",
                        "DurationValue", "DurationUnit", "DurationAggregation",
                        "FrequencyCriteriaValue", "FrequencyCriteriaMethod"))))
  }
  
  x3 <- x2 |>
    dplyr::summarize(Sample_Size = dplyr::n(),
                     Start_Date = min(ActivityStartDate, na.rm = TRUE),
                     End_Date = max(ActivityStartDate, na.rm = TRUE),
                     Minimum = min(TADA.ResultMeasureValue, na.rm = TRUE),
                     Median = median(TADA.ResultMeasureValue, na.rm = TRUE),
                     Maximum = max(TADA.ResultMeasureValue, na.rm = TRUE),
                     Number_of_Exceedances = modSum(Exceedance),
                     .groups = "drop") |>
    dplyr::mutate(Exceedance_Percentage = Number_of_Exceedances/Sample_Size * 100) |>
    dplyr::mutate(Exceedance_Result = dplyr::case_when(
      is.na(FrequencyCriteriaMethod) & Number_of_Exceedances > 0      ~ "Exceed",
      FrequencyCriteriaMethod %in%
        "NumberNotMeeting" &
        Number_of_Exceedances >= FrequencyCriteriaValue               ~ "Exceed",
      FrequencyCriteriaMethod %in%
        "Percent of samples not meeting" &
        Exceedance_Percentage >= FrequencyCriteriaValue               ~ "Exceed",
      TRUE                                                            ~ "Not Exceed"
    ))
  
  if (type %in% "MLid"){
    x4 <- x3 |> dplyr::select(-JoinToAU.AssessmentUnitIdentifier)
  } else if (type %in% "AU_ind"){
    x4 <- x3
  } else {
    x4 <- x3 %>%
      dplyr::left_join(coords, by = "JoinToAU.AssessmentUnitIdentifier")
  }
  
  return(x4)
}

### A function to summarize the map data
map_summary <- function(x, type){
  
  if (type %in% c("AU_ind", "AU_group")){
    x2 <- x |>
      dplyr::group_by(TADA.MonitoringLocationIdentifier,
                      TADA.MonitoringLocationName,
                      JoinToAU.AssessmentUnitIdentifier,
                      TADA.LongitudeMeasure,
                      TADA.LatitudeMeasure) 
  } else {
    
    x2 <- x |>
      dplyr::group_by(TADA.MonitoringLocationIdentifier,
                      TADA.MonitoringLocationName,
                      TADA.LongitudeMeasure,
                      TADA.LatitudeMeasure)
  }
  
  x3 <- x2 |>
    dplyr::mutate(
      Description = ifelse(is.na(AcuteChronic),
                           paste(ATTAINS.UseName, 
                                 TADA.CharacteristicName,
                                 paste0(Exceedance_Percentage, "%"),
                                 Exceedance_Result, sep = " - "),
                           paste(ATTAINS.UseName, 
                                 TADA.CharacteristicName,
                                 AcuteChronic,
                                 paste0(Exceedance_Percentage, "%"),
                                 Exceedance_Result, sep = " - "))
    ) |>
    dplyr::summarize(Description = paste0(unique(Description),
                                   collapse = "\n"),
              Exceedance_Result = any(Exceedance_Result %in% "Exceed"))
  
  return(x3)
  
}