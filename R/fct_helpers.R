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
exceedance_summary <- function(x, type, group = FALSE){
  
  if (!group){
    
    # A look up table for "TADA.MonitoringLocationIdentifier", "TADA.MonitoringLocationName",
    # "JoinToAU.AssessmentUnitIdentifier", "TADA.LongitudeMeasure", "TADA.LatitudeMeasure"
    
    coords <- dplyr::distinct(x, 
                              TADA.MonitoringLocationIdentifier,
                              TADA.MonitoringLocationName,
                              JoinToAU.AssessmentUnitIdentifier,
                              TADA.LongitudeMeasure,
                              TADA.LatitudeMeasure)
    
    if(type %in% "MLid"){
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
                        "TADA.ResultMeasure.MeasureUnitCode", "AcuteChronic",
                        "DurationValue", "DurationUnit", "DurationAggregation",
                        "FrequencyCriteriaValue", "FrequencyCriteriaMethod"))))
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
    
    ans <- list(data = x3, coords = NULL)
    
    return(ans)
    
  }
  
}

### A function to summarize the map data
map_summary <- function(x, type){
  
  if (type %in% "AU"){
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
                                 paste0(round(Exceedance_Percentage, 2), "%"),
                                 Exceedance_Result, sep = " - "),
                           paste(ATTAINS.UseName, 
                                 TADA.CharacteristicName,
                                 AcuteChronic,
                                 paste0(round(Exceedance_Percentage, 2), "%"),
                                 Exceedance_Result, sep = " - "))
    ) |>
    dplyr::summarize(Description = paste0(unique(Description),
                                   collapse = "\n"),
              Exceedance_Result = any(Exceedance_Result %in% "Exceed"))
  
  return(x3)
  
}

# Stored USGS map element names
grp <- c("USGS Topo", "USGS Imagery Only", "USGS Imagery Topo", "USGS Shaded Relief", "Hydrography")

att <- paste0("<a href='https://www.usgs.gov/'>",
              "U.S. Geological Survey</a> | ",
              "<a href='https://www.usgs.gov/laws/policies_notices.html'>",
              "Policies</a>")

# Get the base map
GetURL <- function(service, host = "basemap.nationalmap.gov") {
  sprintf("https://%s/arcgis/services/%s/MapServer/WmsServer", host, service)
}

add_USGS_base <- function(x){
  x <- leaflet::addWMSTiles(x, GetURL("USGSTopo"),
                            group = grp[1], attribution = att, layers = "0")
  x <- leaflet::addWMSTiles(x, GetURL("USGSImageryOnly"),
                            group = grp[2], attribution = att, layers = "0")
  x <- leaflet::addWMSTiles(x, GetURL("USGSImageryTopo"),
                            group = grp[3], attribution = att, layers = "0")
  x <- leaflet::addWMSTiles(x, GetURL("USGSShadedReliefOnly"),
                            group = grp[4], attribution = att, layers = "0")
  
  # Add the tiled overlay for the National Hydrography Dataset to the map widget:
  opt <- leaflet::WMSTileOptions(format = "image/png", transparent = TRUE)
  x <- leaflet::addWMSTiles(x, GetURL("USGSHydroCached"),
                            group = grp[5], options = opt, layers = "0")
  x <- leaflet::hideGroup(x, grp[5])
  
  # Add layer control
  # Add layer controls
  opt2 <- leaflet::layersControlOptions(collapsed = FALSE)
  x <- leaflet::addLayersControl(x, baseGroups = grp[1:4],
                                 overlayGroups = grp[5], options = opt2)
  
  return(x)
}

### Overall exceedance map
create_overall_map <- function(data, coords_data = NULL, group_by = "MLid") {
  
  if (group_by == "MLid") {
    # For MLid grouping, coordinates are in the main data
    map_data <- data |>
      dplyr::group_by(TADA.MonitoringLocationIdentifier, 
                      TADA.MonitoringLocationName,
                      TADA.LongitudeMeasure, 
                      TADA.LatitudeMeasure) |>
      dplyr::summarise(
        has_exceedance = any(Exceedance_Result == "Exceed"),
        total_params = dplyr::n_distinct(TADA.CharacteristicName),
        params_exceeding = sum(Exceedance_Result == "Exceed"),
        uses_affected = paste(unique(ATTAINS.UseName[Exceedance_Result == "Exceed"]), collapse = ", "),
        # Add detailed use-parameter combinations that exceed
        use_param_exceeding = paste(unique(paste(ATTAINS.UseName[Exceedance_Result == "Exceed"], 
                                                 "-", 
                                                 TADA.CharacteristicName[Exceedance_Result == "Exceed"])), 
                                    collapse = "<br>"),
        .groups = 'drop'
      )
  } else {
    # For AU grouping, aggregate by AU then join coordinates
    au_summary <- data |>
      dplyr::group_by(JoinToAU.AssessmentUnitIdentifier) |>
      dplyr::summarise(
        has_exceedance = any(Exceedance_Result == "Exceed"),
        total_params = dplyr::n_distinct(TADA.CharacteristicName),
        params_exceeding = sum(Exceedance_Result == "Exceed"),
        uses_affected = paste(unique(ATTAINS.UseName[Exceedance_Result == "Exceed"]), collapse = ", "),
        use_param_exceeding = paste(unique(paste(ATTAINS.UseName[Exceedance_Result == "Exceed"], 
                                                 "-", 
                                                 TADA.CharacteristicName[Exceedance_Result == "Exceed"])), 
                                    collapse = "<br>"),
        sites_in_au = dplyr::n_distinct(TADA.MonitoringLocationIdentifier),
        .groups = 'drop'
      )
    
    # Join with coordinates
    map_data <- coords_data |>
      dplyr::left_join(au_summary, by = "JoinToAU.AssessmentUnitIdentifier") |>
      dplyr::mutate(
        has_exceedance = tidyr::replace_na(has_exceedance, FALSE),
        total_params = tidyr::replace_na(total_params, 0),
        params_exceeding = tidyr::replace_na(params_exceeding, 0),
        use_param_exceeding = tidyr::replace_na(use_param_exceeding, "")
      )
  }
  
  # Create the map
  leaflet::leaflet(map_data) |>
    add_USGS_base() |>
    leaflet::addCircleMarkers(
      lng = ~TADA.LongitudeMeasure,
      lat = ~TADA.LatitudeMeasure,
      color = ~ifelse(has_exceedance, "black", "black"),
      fillColor = ~ifelse(has_exceedance, "red", "green"),
      fillOpacity = 0.7,
      weight = 1,
      radius = 8,
      popup = ~paste0(
        ifelse(group_by == "MLid", 
               paste0("<b>Site ID:</b> ", TADA.MonitoringLocationIdentifier, "<br>",
                      "<b>Site Name:</b> ", TADA.MonitoringLocationName, "<br>"),
               paste0("<b>AU ID:</b> ", JoinToAU.AssessmentUnitIdentifier, "<br>",
                      "<b>Site:</b> ", TADA.MonitoringLocationName, "<br>",
                      "<b>Sites in AU:</b> ", sites_in_au, "<br>")),
        "<b>Status:</b> ", ifelse(has_exceedance, "Exceeding", "Meeting"), "<br>",
        "<b>Parameters Exceeding:</b> ", params_exceeding, "/", total_params, "<br>",
        ifelse(nchar(use_param_exceeding) > 0, 
               paste0("<b>Use-Parameter Exceedances:</b><br>", use_param_exceeding), "")
      )
    ) |>
    leaflet::addLegend(
      position = "bottomright",
      colors = c("green", "red"),
      labels = c("Meeting Criteria", "Exceeding Criteria"),
      title = paste("Status by", ifelse(group_by == "MLid", "Monitoring Location", "Assessment Unit"))
    )
}

### Use-Specific Map
create_use_map <- function(data, coords_data = NULL, selected_use = NULL, group_by = "MLid") {
  
  # Filter for selected use 
  if (!is.null(selected_use)) {
    data <- data |> dplyr::filter(ATTAINS.UseName == selected_use)
  }
  
  if (group_by == "MLid") {
    # MLid grouping
    map_data <- data |>
      dplyr::group_by(TADA.MonitoringLocationIdentifier,
                      TADA.MonitoringLocationName,
                      TADA.LongitudeMeasure, 
                      TADA.LatitudeMeasure) |>
      dplyr::summarise(
        has_exceedance = any(Exceedance_Result == "Exceed"),
        params_exceeding_count = sum(Exceedance_Result == "Exceed"),
        total_params = dplyr::n(),
        params_exceeding_list = paste(unique(TADA.CharacteristicName[Exceedance_Result == "Exceed"]), 
                                      collapse = ", "),
        .groups = 'drop'
      )
  } else {
    # AU grouping
    au_use_summary <- data |>
      dplyr::group_by(JoinToAU.AssessmentUnitIdentifier) |>
      dplyr::summarise(
        has_exceedance = any(Exceedance_Result == "Exceed"),
        params_exceeding_count = sum(Exceedance_Result == "Exceed"),
        total_params = dplyr::n(),
        params_exceeding_list = paste(unique(TADA.CharacteristicName[Exceedance_Result == "Exceed"]), 
                                      collapse = ", "),
        .groups = 'drop'
      )
    
    map_data <- coords_data |>
      dplyr::left_join(au_use_summary, by = "JoinToAU.AssessmentUnitIdentifier") |>
      dplyr::mutate(
        has_exceedance = tidyr::replace_na(has_exceedance, FALSE),
        params_exceeding_count = tidyr::replace_na(params_exceeding_count, 0),
        total_params = tidyr::replace_na(total_params, 0),
        params_exceeding_list = tidyr::replace_na(params_exceeding_list, "None")
      )
  }
  
  leaflet::leaflet(map_data) |>
    add_USGS_base() |>
    leaflet::addCircleMarkers(
      lng = ~TADA.LongitudeMeasure,
      lat = ~TADA.LatitudeMeasure,
      color = ~ifelse(has_exceedance, "black", "black"),
      fillColor = ~ifelse(has_exceedance, "red", "green"),
      fillOpacity = 0.7,
      weight = 1,
      radius = 8,
      popup = ~paste0(
        "<b>Location:</b> ", TADA.MonitoringLocationName, "<br>",
        ifelse(group_by == "AU", paste0("<b>AU:</b> ", JoinToAU.AssessmentUnitIdentifier, "<br>"), ""),
        "<b>Use:</b> ", selected_use, "<br>",
        "<b>Status:</b> ", ifelse(has_exceedance, "Not Meeting", "Meeting"), "<br>",
        "<b>Parameters Exceeding:</b> ", params_exceeding_count, "/", total_params, "<br>",
        "<b>Exceeding Parameters:</b> ", params_exceeding_list
      )
    ) |>
    leaflet::addLegend(
      position = "bottomright",
      colors = c("green", "red"),
      labels = c("Meeting Criteria", "Exceeding Criteria"),
      title = paste("Use:", selected_use)
    )
}

### Parameter-specific map
create_parameter_map <- function(data, coords_data = NULL, selected_param = NULL, selected_use = NULL, group_by = "MLid") {
  
  # Filter for selected parameter
  if (!is.null(selected_param)) {
    data <- data |> dplyr::filter(TADA.CharacteristicName == selected_param)
  }
  
  # Filter for selected use
  if (!is.null(selected_use)) {
    data <- data |> dplyr::filter(ATTAINS.UseName == selected_use)
  }
  
  if (group_by == "MLid") {
    # MLid grouping
    map_data <- data |>
      dplyr::group_by(TADA.MonitoringLocationIdentifier,
                      TADA.MonitoringLocationName,
                      TADA.LongitudeMeasure, 
                      TADA.LatitudeMeasure) |>
      dplyr::summarise(
        has_exceedance = any(Exceedance_Result == "Exceed"),
        num_exceedances = sum(Number_of_Exceedances, na.rm = TRUE),
        median_value = median(Median, na.rm = TRUE),
        max_value = max(Maximum, na.rm = TRUE),
        .groups = 'drop'
      )
  } else {
    # AU grouping
    au_param_summary <- data |>
      dplyr::group_by(JoinToAU.AssessmentUnitIdentifier) |>
      dplyr::summarise(
        has_exceedance = any(Exceedance_Result == "Exceed"),
        num_exceedances = sum(Number_of_Exceedances, na.rm = TRUE),
        median_value = median(Median, na.rm = TRUE),
        max_value = max(Maximum, na.rm = TRUE),
        .groups = 'drop'
      )
    
    map_data <- coords_data |>
      dplyr::left_join(au_param_summary, by = "JoinToAU.AssessmentUnitIdentifier") |>
      dplyr::mutate(
        has_exceedance = tidyr::replace_na(has_exceedance, FALSE),
        num_exceedances = tidyr::replace_na(num_exceedances, 0)
      )
  }
  
  leaflet::leaflet(map_data) |>
    add_USGS_base() |>
    leaflet::addCircleMarkers(
      lng = ~TADA.LongitudeMeasure,
      lat = ~TADA.LatitudeMeasure,
      color = ~ifelse(has_exceedance, "darkred", "darkgreen"),
      fillColor = ~ifelse(has_exceedance, "red", "green"),
      fillOpacity = 0.6,
      weight = 1,
      radius = ~pmin(sqrt(num_exceedances) * 3 + 5, 20),
      popup = ~paste0(
        "<b>Location:</b> ", TADA.MonitoringLocationName, "<br>",
        ifelse(group_by == "AU", paste0("<b>AU:</b> ", JoinToAU.AssessmentUnitIdentifier, "<br>"), ""),
        "<b>Parameter:</b> ", selected_param, "<br>",
        "<b>Use:</b> ", selected_use, "<br>",
        "<b>Total Exceedances:</b> ", num_exceedances, "<br>",
        "<b>Median Value:</b> ", round(median_value, 2), "<br>",
        "<b>Max Value:</b> ", round(max_value, 2)
      )
    ) |>
    leaflet::addLegend(
      position = "bottomright",
      colors = c("green", "red"),
      labels = c("Meeting Standard", "Exceeding Standard"),
      title = paste(selected_param, "<br>", selected_use)
    )
}


