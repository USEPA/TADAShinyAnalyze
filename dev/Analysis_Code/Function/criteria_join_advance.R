# A function to join the criteria
# The x argument is the TADA data frame
# The y argument is the criteria table
# Type:
# 1) Summarize each available combination separately – flag combinations that have been 
#    specifically defined in the Criteria Methodology spreadsheet vs. combinations that have not.
# 2) Only summarize the one specified in the Criteria Methodology spreadsheet
# 3) Combine all and summarize by ONLY characteristic and ignore fraction and speciation.

criteria_join <- function(x, y, match_type = "Option 1", filter_type = TRUE){
  
  # Add flags to criteria table
  y2 <- y |> dplyr::mutate(Matched = "Yes")
  
  if (match_type %in% "Option 1"){  # Join Option 1: Including Fraction
    by <- join_by("TADA.CharacteristicName",
                  "TADA.ResultSampleFractionText" == "Fraction",
                  "TADA.ResultMeasure.MeasureUnitCode" == "MagnitudeUnit",
                  "ATTAINS.UseName",
                  "ATTAINS.waterTypeCode")
    
    x2 <- x |>
      dplyr::left_join(y2, by = by, relationship = "many-to-many") |>
      dplyr::mutate(Matched = ifelse(is.na(Matched), "No", Matched))
  
      
  } else { # Join Option 2: No Fraction
    by <- join_by("TADA.CharacteristicName",
                  "TADA.ResultMeasure.MeasureUnitCode" == "MagnitudeUnit",
                  "ATTAINS.UseName",
                  "ATTAINS.waterTypeCode")
    
    y_col <- names(y2)
    y_col2 <- y_col[!y_col %in% "Fraction"]
    
    y3 <- y2 |> dplyr::distinct(dplyr::across(dplyr::all_of(y_col2)))
    
    x2 <- x |>
      dplyr::left_join(y3, by = by, relationship = "many-to-many") |>
      dplyr::mutate(Matched = ifelse(is.na(Matched), "No", Matched))
  }
  
  if (filter_type){ # filter_type = TRUE: Only keep records with Matched as "Yes 
    x2 <- x2 |>
      dplyr::filter(Matched %in% "Yes")
  } 
  
  return(x2)
}