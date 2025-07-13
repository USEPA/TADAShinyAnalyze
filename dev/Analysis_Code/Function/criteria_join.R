# A function to join the criteria
criteria_join <- function(x, y){
  
  x2 <- x |>
    dplyr::left_join(y, by = c("TADA.CharacteristicName",
                               "TADA.ResultSampleFractionText" = "Fraction",
                               "TADA.ResultMeasure.MeasureUnitCode" = "MagnitudeUnit",
                               # TADA.ComparableDataIdentifier
                               # "ATTAINS.waterTypeCode",
                               # "MonitoringLocationTypeName", 
                               "ATTAINS.UseName"
    ),
    relationship = "many-to-many")
  
  return(x2)
}