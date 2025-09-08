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

# A function to calculate the excursion summary data with criteria
excursion_summary <- function(x, type, group = FALSE){
  
  if (!group){
    
    # A look up table for "TADA.MonitoringLocationIdentifier", "TADA.MonitoringLocationName",
    # "JoinToAU.AssessmentUnitIdentifier", "TADA.LongitudeMeasure", "TADA.LatitudeMeasure"
    
    coords <- dplyr::distinct(x, 
                              TADA.MonitoringLocationIdentifier,
                              TADA.MonitoringLocationName,
                              JoinToAU.AssessmentUnitIdentifier,
                              TADA.LongitudeMeasure,
                              TADA.LatitudeMeasure)
    
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
    
    if(type %in% "MLid"){
      x2 <- x |>
        dplyr::group_by(dplyr::across(
          dplyr::all_of(c("TADA.MonitoringLocationIdentifier", "TADA.MonitoringLocationName",
                          "TADA.LongitudeMeasure", "TADA.LatitudeMeasure", id_cols))))
    } else{
      x2 <- x |>
        dplyr::group_by(dplyr::across(
          dplyr::all_of(id_cols)))
    }
    
    x3 <- x2 |>
      dplyr::summarize(Sample_Count = dplyr::n(),
                       Start_Date = min(ActivityStartDate, na.rm = TRUE),
                       End_Date = max(ActivityStartDate, na.rm = TRUE),
                       Minimum = min(TADA.ResultMeasureValue, na.rm = TRUE),
                       Median = median(TADA.ResultMeasureValue, na.rm = TRUE),
                       Maximum = max(TADA.ResultMeasureValue, na.rm = TRUE),
                       Number_of_Excursions = modSum(Excursion),
                       .groups = "drop") |>
      dplyr::mutate(Excursion_Percentage = Number_of_Excursions/Sample_Count * 100)
    
    # if (type %in% "MLid"){
    #   x4 <- x3
    # } else {
    #   x4 <- x3 |>
    #     dplyr::left_join(coords, by = "JoinToAU.AssessmentUnitIdentifier")
    # }
    
    ans <- list(data = x3, coords = coords)
    
    return(ans)
    
  } else {
    
    x2 <- x |>
      dplyr::group_by(dplyr::across(
        dplyr::all_of(c("ATTAINS.UseName",
                        "TADA.CharacteristicName", "TADA.ResultSampleFractionText",
                        "TADA.ResultMeasure.MeasureUnitCode", "ATTAINS.ParameterName",
                        "AcuteChronic",
                        "DurationValue", "DurationUnit", "DurationAggregation",
                        "FrequencyCriteriaValue", "FrequencyCriteriaMethod"))))
    x3 <- x2 |>
      dplyr::summarize(Sample_Count = dplyr::n(),
                       Start_Date = min(ActivityStartDate, na.rm = TRUE),
                       End_Date = max(ActivityStartDate, na.rm = TRUE),
                       Minimum = min(TADA.ResultMeasureValue, na.rm = TRUE),
                       Median = median(TADA.ResultMeasureValue, na.rm = TRUE),
                       Maximum = max(TADA.ResultMeasureValue, na.rm = TRUE),
                       Number_of_Excursions = modSum(Excursion),
                       .groups = "drop") |>
      dplyr::mutate(Excursion_Percentage = Number_of_Excursions/Sample_Count * 100)
    
    ans <- list(data = x3, coords = NULL)
    
    return(ans)
    
  }
  
}