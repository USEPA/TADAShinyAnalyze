### A function to take the dataset and calculate the value based on duration

# A function to convert hourly data to daily data
hourly_to_daily <- function(x){
  # Calculate the daily value
  x2 <- x |>
    dplyr::group_by(across(-c(TADA.ResultMeasureValue,
                              pH, Temperature, Hardness,
                              DateTime))) |>
    dplyr::summarize(across(c(TADA.ResultMeasureValue,
                              pH, Temperature, Hardness),
                            .fns = mean)) |>
    dplyr::ungroup()
  return(x2)
}

Duration_fun <- function(x, Value#, 
                         #Unit, 
                         #Aggregation
                         ){
  
  if (!Value %in% "n-hour"){
    
    # Calculate the daily value
    x2 <- x |> hourly_to_daily()
      
  } else {
    x2 <- x
  }
  
  
  if (Value %in% "n-day"){
    
  } else if (Value %in% "n-hour"){
    
  } else if (Value %in% "n-month"){
    return()
  } else if (Value %in% "n-season"){
    return()
  }
  return(x2)
}