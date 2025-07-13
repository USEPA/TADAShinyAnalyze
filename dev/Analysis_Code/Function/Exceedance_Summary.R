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

  if(type %in% c("MLid", "AUind")){
    x2 <- x |>
      dplyr::group_by(dplyr::across(
        dplyr::all_of(c("MonitoringLocationIdentifier", "MonitoringLocationName",
                      "JoinToAU.AssessmentUnitIdentifier", "ATTAINS.UseName",
                      "TADA.LongitudeMeasure", "TADA.LatitudeMeasure",
                      "TADA.CharacteristicName", "TADA.ResultSampleFractionText",
                      "TADA.ResultMeasure.MeasureUnitCode", "AcuteChronic",
                      "DurationValue", "DurationUnit", "DurationAggregation",
                      "FrequencyCriteriaValue", "FrequencyCriteriaMethod"))))
  } else if (type %in% "AUgroup"){
    x2 <- x |>
      dplyr::group_by(dplyr::across(
        dplyr::all_of(c("JoinToAU.AssessmentUnitIdentifier", "ATTAINS.UseName",
                        "TADA.LongitudeMeasure", "TADA.LatitudeMeasure",
                        "TADA.CharacteristicName", "TADA.ResultSampleFractionText",
                        "TADA.ResultMeasure.MeasureUnitCode", "AcuteChronic",
                        "DurationValue", "DurationUnit", "DurationAggregation",
                        "FrequencyCriteriaValue", "FrequencyCriteriaMethod"))))
  }
  
  else {
    # some sort of error statement?
    # x2 <- ???
    # or you could also remove the else if and put that code here
  }

  x3 <- x2 |>
    dplyr::summarize(Sample_Size = dplyr::n(),
                     Start_Date = min(ActivityStartDate),
                     End_Date = max(ActivityStartDate),
                     Minimum = min(TADA.ResultMeasureValue),
                     Median = median(TADA.ResultMeasureValue),
                     Maximum = max(TADA.ResultMeasureValue),
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
  } else {
    x4 <- x3
  }

  return(x4)
}

