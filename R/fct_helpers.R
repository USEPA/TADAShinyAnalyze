#' helpers 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

### A function to join the criteria
criteria_join <- function(x, y, match_type = "Option 2",
                          use_type = "Option 1",
                          filter_type = TRUE){

  # Add flags to criteria table
  y2 <- y |> dplyr::mutate(Matched = "Yes")

  # Build join expression as a string
  join_cols <- c(
    "TADA.CharacteristicName",
    "TADA.ResultMeasure.MeasureUnitCode == MagnitudeUnit"
  )

  # Conditionally add columns
  if (use_type == "Option 1") {
    join_cols <- c(join_cols, "ATTAINS.UseName")
  }

  if (match_type == "Option 1") {
    join_cols <- c(join_cols, "TADA.ResultSampleFractionText")
  }

  # Build and evaluate the join_by expression
  join_expr <- paste0("dplyr::join_by(", paste(join_cols, collapse = ", "), ")")
  by <- eval(parse(text = join_expr))

  # Handle x table modifications for Option 2 (no use)
  # In this case, the final ATTAINS.UseName is from the criteria table
  if (use_type == "Option 2") {
    x_col <- names(x)
    x_col2 <- x_col[!x_col %in% "ATTAINS.UseName"]
    x2 <- x |> dplyr::select((dplyr::all_of(x_col2)))
  } else {
    x2 <- x
  }
  
  # Handle y table modifications for Option 2 (no fraction)
  if (match_type == "Option 2") {
    y_col <- names(y2)
    y_col2 <- y_col[!y_col %in% "TADA.ResultSampleFractionText"]
    y2 <- y2 |> dplyr::distinct(dplyr::across(dplyr::all_of(y_col2)))
  }

  # Perform the join
  x3 <- x2 |>
    dplyr::left_join(y2, by = by, relationship = "many-to-many") |>
    dplyr::mutate(Matched = ifelse(is.na(Matched), "No", Matched))

  # Apply filter if requested
  if (filter_type) {
    x3 <- x3 |> dplyr::filter(Matched == "Yes")
  }

  return(x3)
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

### A function to compare the Excusions
excursion_fun <- function(x){
  x2 <- x |>
    dplyr::mutate(Excursion = dplyr::case_when(
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
  # Handle empty or NULL inputs
  if(is.null(x) || length(x) == 0){
    y <- NA
  } else if(all(is.na(x))){
    y <- NA
  } else {
    y <- sum(x, na.rm = TRUE)
  }
  return(y)
}

# A function to calculate the excursion summary data with criteria
excursion_summary <- function(x, type){
  
  # A look up table for "TADA.MonitoringLocationIdentifier", "TADA.MonitoringLocationName",
  # "JoinToAU.AssessmentUnitIdentifier", "TADA.LongitudeMeasure", "TADA.LatitudeMeasure"
  
  x_cols <- names(x)
  
  coords_cols <- c("TADA.MonitoringLocationIdentifier",
                  "TADA.MonitoringLocationName",
                  "JoinToAU.AssessmentUnitIdentifier",
                  "TADA.LongitudeMeasure",
                  "TADA.LatitudeMeasure")
  
  dist_cols <- base::intersect(x_cols, coords_cols)
  
  coords <- x |> dplyr::distinct(dplyr::across(dplyr::all_of(dist_cols)))
  
  id_cols <- c(
    "ATTAINS.ParameterName",
    "TADA.CharacteristicName",
    "TADA.ResultSampleFractionText",
    "TADA.MethodSpeciationName",
    "TADA.ResultMeasure.MeasureUnitCode",
    "ATTAINS.UseName",
    "AcuteChronic",
    "UniqueSpatialCriteria",
    "Season",
    "ATTAINS.OrganizationIdentifier",
    "EquationBased",
    "DurationUnit",
    "DurationMethod",
    "DurationValue",
    "FreqValue",
    "FreqMethod",
    "EquationType"
  )
  
  if (type %in% "MLid"){
    id_cols <- c("TADA.MonitoringLocationIdentifier", "TADA.MonitoringLocationName",
                  "TADA.LongitudeMeasure", "TADA.LatitudeMeasure", "JoinToAU.AssessmentUnitIdentifier",
                  id_cols)
  } else if (type %in% "AU") {
    id_cols <- c("JoinToAU.AssessmentUnitIdentifier", id_cols)
  } else {
    id_cols <- id_cols
  }
  
  # Check if JoinToAU.AssessmentUnitIdentifier exists
  id_cols2 <- base::intersect(x_cols, id_cols)

  x2 <- x |>
    dplyr::group_by(dplyr::across(dplyr::all_of(id_cols2))) |>
    dplyr::summarize(Sample_Count = dplyr::n(),
                     Start_Date = min(ActivityStartDate, na.rm = TRUE),
                     End_Date = max(ActivityStartDate, na.rm = TRUE),
                     Minimum = min(TADA.ResultMeasureValue, na.rm = TRUE),
                     Median = median(TADA.ResultMeasureValue, na.rm = TRUE),
                     Maximum = max(TADA.ResultMeasureValue, na.rm = TRUE),
                     Number_of_Excursions = modSum(Excursion),
                     .groups = "drop") |>
    dplyr::mutate(Excursion_Percentage = Number_of_Excursions/Sample_Count * 100)
  
  ans <- list(data = x2, coords = coords)
  
  return(ans)
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
create_overall_map <- function(data, coords_data = NULL, type = "MLid") {
  
  if (type %in% "MLid") {
    # For MLid grouping, coordinates are in the main data
    map_data <- data |>
      dplyr::group_by(TADA.MonitoringLocationIdentifier, 
                      TADA.MonitoringLocationName,
                      TADA.LongitudeMeasure, 
                      TADA.LatitudeMeasure) |>
      dplyr::summarise(
        has_exceedance = any(Exceedance == "Exceed"),
        total_params = dplyr::n_distinct(TADA.CharacteristicName),
        # params_exceeding = sum(Exceedance == "Exceed"),
        uses_affected = paste(unique(ATTAINS.UseName[Exceedance == "Exceed"]), collapse = ", "),
        # Add detailed use-parameter combinations that exceed
        use_param_exceeding = paste(unique(paste(ATTAINS.UseName[Exceedance == "Exceed"], 
                                                 "-", 
                                                 TADA.CharacteristicName[Exceedance == "Exceed"])), 
                                    collapse = "<br>"),
        .groups = 'drop'
      )
  } else if (type %in% "AU"){
    # For AU grouping, aggregate by AU then join coordinates
    au_summary <- data |>
      dplyr::group_by(JoinToAU.AssessmentUnitIdentifier) |>
      dplyr::summarise(
        has_exceedance = any(Exceedance == "Exceed"),
        total_params = dplyr::n_distinct(TADA.CharacteristicName),
        # params_exceeding = sum(Exceedance == "Exceed"),
        uses_affected = paste(unique(ATTAINS.UseName[Exceedance == "Exceed"]), collapse = ", "),
        use_param_exceeding = paste(unique(paste(ATTAINS.UseName[Exceedance == "Exceed"], 
                                                 "-", 
                                                 TADA.CharacteristicName[Exceedance == "Exceed"])), 
                                    collapse = "<br>"),
        .groups = 'drop'
      )
    
    # Join with coordinates
    map_data <- coords_data |>
      dplyr::left_join(au_summary, by = "JoinToAU.AssessmentUnitIdentifier") |>
      dplyr::mutate(
        has_exceedance = tidyr::replace_na(has_exceedance, FALSE),
        total_params = tidyr::replace_na(total_params, 0),
        # params_exceeding = tidyr::replace_na(params_exceeding, 0),
        use_param_exceeding = tidyr::replace_na(use_param_exceeding, "")
      ) |>
      dplyr::group_by(JoinToAU.AssessmentUnitIdentifier) |>
      dplyr::mutate(
        sites_in_au = dplyr::n_distinct(TADA.MonitoringLocationIdentifier)
      )
  } else {
    # For CG grouping, aggregate by all sites then join coordinates
    cg_summary <- data |>
      dplyr::summarise(
        has_exceedance = any(Exceedance == "Exceed"),
        total_params = dplyr::n_distinct(TADA.CharacteristicName),
        # params_exceeding = sum(Exceedance == "Exceed"),
        uses_affected = paste(unique(ATTAINS.UseName[Exceedance == "Exceed"]), collapse = ", "),
        use_param_exceeding = paste(unique(paste(ATTAINS.UseName[Exceedance == "Exceed"], 
                                                 "-", 
                                                 TADA.CharacteristicName[Exceedance == "Exceed"])), 
                                    collapse = "<br>"),
        .groups = 'drop'
      )
    
    # Join with coordinates
    map_data <- coords_data |>
      tidyr::crossing(cg_summary) |>
      dplyr::mutate(
        has_exceedance = tidyr::replace_na(has_exceedance, FALSE),
        total_params = tidyr::replace_na(total_params, 0),
        # params_exceeding = tidyr::replace_na(params_exceeding, 0),
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
      fillColor = ~ifelse(has_exceedance, "#FF6600", "#0066CC"),
      fillOpacity = 0.7,
      weight = 1,
      radius = 8,
      popup = ~paste0(
        switch(type,
               "MLid" = paste0("<b>Site ID:</b> ", TADA.MonitoringLocationIdentifier, "<br>",
                               "<b>Site Name:</b> ", TADA.MonitoringLocationName, "<br>"),
               "AU" = paste0("<b>AU ID:</b> ", JoinToAU.AssessmentUnitIdentifier, "<br>",
                             "<b>Site:</b> ", TADA.MonitoringLocationName, "<br>",
                             "<b>Sites in AU:</b> ", sites_in_au, "<br>"),
               "CG" = ifelse("JoinToAU.AssessmentUnitIdentifier" %in% names(map_data),
                             paste0("<b>AU ID:</b> ", JoinToAU.AssessmentUnitIdentifier, "<br>",
                                    "<b>Site:</b> ", TADA.MonitoringLocationName, "<br>"),
                             paste0("<b>Site:</b> ", TADA.MonitoringLocationName, "<br>")),
               paste0(paste0("<b>Site:</b> ", TADA.MonitoringLocationName, "<br>"))),
        "<b>Status:</b> ", ifelse(has_exceedance, "Exceeding", "Meeting"), "<br>",
        ifelse(nchar(use_param_exceeding) > 0, 
               paste0("<b>Use-Parameter Exceedances:</b><br>", use_param_exceeding), "")
      )
    ) |>
    leaflet::addLegend(
      position = "bottomright",
      colors = c("#FF6600", "#0066CC"),
      labels = c("Exceeding Criteria", "Meeting Criteria"),
      title = paste("Status by", ifelse(type %in% "MLid", "Monitoring Location", "Assessment Unit"))
    )
}

### Use-Specific Map
create_use_map <- function(data, coords_data = NULL, selected_use = NULL, type = "MLid") {
  
  # Filter for selected use 
  if (!is.null(selected_use)) {
    data <- data |> dplyr::filter(ATTAINS.UseName == selected_use)
  }
  
  if (type %in% "MLid") {
    # MLid grouping
    map_data <- data |>
      dplyr::group_by(TADA.MonitoringLocationIdentifier,
                      TADA.MonitoringLocationName,
                      TADA.LongitudeMeasure, 
                      TADA.LatitudeMeasure) |>
      dplyr::summarise(
        has_exceedance = any(Exceedance == "Exceed"),
        params_exceeding_count = sum(Exceedance == "Exceed"),
        total_params = dplyr::n(),
        params_exceeding_list = paste(unique(TADA.CharacteristicName[Exceedance == "Exceed"]), 
                                      collapse = ", "),
        .groups = 'drop'
      )
  } else if (type %in% "AU"){
    # AU grouping
    au_use_summary <- data |>
      dplyr::group_by(JoinToAU.AssessmentUnitIdentifier) |>
      dplyr::summarise(
        has_exceedance = any(Exceedance == "Exceed"),
        params_exceeding_count = sum(Exceedance == "Exceed"),
        total_params = dplyr::n(),
        params_exceeding_list = paste(unique(TADA.CharacteristicName[Exceedance == "Exceed"]), 
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
  } else {
    # CG grouping
    cg_use_summary <- data |>
      dplyr::summarise(
        has_exceedance = any(Exceedance == "Exceed"),
        params_exceeding_count = sum(Exceedance == "Exceed"),
        total_params = dplyr::n(),
        params_exceeding_list = paste(unique(TADA.CharacteristicName[Exceedance == "Exceed"]), 
                                      collapse = ", "),
        .groups = 'drop'
      )
    
    map_data <- coords_data |>
      tidyr::crossing(cg_use_summary) |>
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
      fillColor = ~ifelse(has_exceedance, "#FF6600", "#0066CC"),
      fillOpacity = 0.7,
      weight = 1,
      radius = 8,
      popup = ~paste0(
        ifelse("JoinToAU.AssessmentUnitIdentifier" %in% names(map_data), 
               paste0("<b>AU ID:</b> ", JoinToAU.AssessmentUnitIdentifier, "<br>"), ""),
        "<b>Site:</b> ", TADA.MonitoringLocationName, "<br>",
        "<b>Use:</b> ", selected_use, "<br>",
        "<b>Status:</b> ", ifelse(has_exceedance, "Not Meeting", "Meeting"), "<br>",
        # "<b>Parameters Exceeding:</b> ", params_exceeding_count, "/", total_params, "<br>",
        "<b>Exceeding Parameters:</b> ", params_exceeding_list
      )
    ) |>
    leaflet::addLegend(
      position = "bottomright",
      colors = c("#FF6600", "#0066CC"),
      labels = c("Exceeding Criteria", "Meeting Criteria"),
      title = paste("Use:", selected_use)
    )
}

### Parameter-specific map
create_parameter_map <- function(data, coords_data = NULL, selected_param = NULL, selected_use = NULL, type = "MLid") {
  
  # Filter for selected parameter
  if (!is.null(selected_param)) {
    data <- data |> dplyr::filter(TADA.CharacteristicName == selected_param)
  }
  
  # Filter for selected use
  if (!is.null(selected_use)) {
    data <- data |> dplyr::filter(ATTAINS.UseName == selected_use)
  }
  
  if (type %in% "MLid") {
    # MLid grouping
    map_data <- data |>
      dplyr::group_by(TADA.MonitoringLocationIdentifier,
                      TADA.MonitoringLocationName,
                      TADA.LongitudeMeasure, 
                      TADA.LatitudeMeasure) |>
      dplyr::summarise(
        has_exceedance = any(Exceedance == "Exceed"),
        num_excursions = sum(Duration_Excursions, na.rm = TRUE),
        median_value = median(Median, na.rm = TRUE),
        max_value = max(Maximum, na.rm = TRUE),
        .groups = 'drop'
      )
  } else if (type %in% "AU"){
    # AU grouping
    au_param_summary <- data |>
      dplyr::group_by(JoinToAU.AssessmentUnitIdentifier) |>
      dplyr::summarise(
        has_exceedance = any(Exceedance == "Exceed"),
        num_excursions = sum(Duration_Excursions, na.rm = TRUE),
        median_value = median(Median, na.rm = TRUE),
        max_value = max(Maximum, na.rm = TRUE),
        .groups = 'drop'
      )
    
    map_data <- coords_data |>
      dplyr::left_join(au_param_summary, by = "JoinToAU.AssessmentUnitIdentifier") |>
      dplyr::mutate(
        has_exceedance = tidyr::replace_na(has_exceedance, FALSE),
        num_excursions = tidyr::replace_na(num_excursions, 0)
      )
  } else {
    # CG grouing
    cg_param_summary <- data |>
      dplyr::summarise(
        has_exceedance = any(Exceedance == "Exceed"),
        num_excursions = sum(Duration_Excursions, na.rm = TRUE),
        median_value = median(Median, na.rm = TRUE),
        max_value = max(Maximum, na.rm = TRUE),
        .groups = 'drop'
      )
    
    map_data <- coords_data |>
      tidyr::crossing(cg_param_summary) |>
      dplyr::mutate(
        has_exceedance = tidyr::replace_na(has_exceedance, FALSE),
        num_excursions = tidyr::replace_na(num_excursions, 0)
      )
  }
  
  leaflet::leaflet(map_data) |>
    add_USGS_base() |>
    leaflet::addCircleMarkers(
      lng = ~TADA.LongitudeMeasure,
      lat = ~TADA.LatitudeMeasure,
      color = ~ifelse(has_exceedance, "black", "black"),
      fillColor = ~ifelse(has_exceedance, "#FF6600", "#0066CC"),
      fillOpacity = 0.6,
      weight = 1,
      radius = ~pmin(sqrt(num_excursions) * 3 + 5, 20),
      popup = ~paste0(
        ifelse("JoinToAU.AssessmentUnitIdentifier" %in% names(map_data), 
               paste0("<b>AU ID:</b> ", JoinToAU.AssessmentUnitIdentifier, "<br>"), ""),
        "<b>Site:</b> ", TADA.MonitoringLocationName, "<br>",
        "<b>Parameter:</b> ", selected_param, "<br>",
        "<b>Use:</b> ", selected_use, "<br>",
        # "<b>Total Excursions:</b> ", num_excursions, "<br>",
        "<b>Median Value:</b> ", round(median_value, 2), "<br>",
        "<b>Max Value:</b> ", round(max_value, 2)
      )
    ) |>
    leaflet::addLegend(
      position = "bottomright",
      colors = c("#FF6600", "#0066CC"),
      labels = c("Exceeding Criteria", "Meeting Criteria"),
      title = paste(selected_param, "<br>", selected_use)
    )
}

time_aggregate <- function(x, type){
  
  x <- x |>
    dplyr::rename(Date = ActivityStartDate) |>
    dplyr::mutate(DateTime = lubridate::as_datetime(DateTime))
  
  x_cols <- names(x)
  
  id_cols <- c(
    "ATTAINS.ParameterName",
    "TADA.CharacteristicName",
    "TADA.ResultSampleFractionText",
    "TADA.MethodSpeciationName",
    "TADA.ResultMeasure.MeasureUnitCode",
    "ATTAINS.UseName",
    "AcuteChronic",
    "UniqueSpatialCriteria",
    "Season",
    "ATTAINS.OrganizationIdentifier",
    "EquationBased",
    "DurationUnit",
    "DurationMethod",
    "DurationValue",
    "FreqValue",
    "FreqMethod",
    "EquationType"
  )
  
  if (type %in% "MLid"){
    base_id_cols <- c("TADA.MonitoringLocationIdentifier")
    # Only add JoinToAU if it exists
    if ("JoinToAU.AssessmentUnitIdentifier" %in% x_cols) {
      id_cols <- c(base_id_cols, "JoinToAU.AssessmentUnitIdentifier", id_cols)
    } else {
      id_cols <- c(base_id_cols, id_cols)
    }
  } else if (type %in% "AU"){
    id_cols <- c("JoinToAU.AssessmentUnitIdentifier", id_cols)
  } else {
    id_cols <- id_cols
  }
  
  # Check if JoinToAU.AssessmentUnitIdentifier exists
  id_cols2 <- base::intersect(x_cols, id_cols)
  
  # Collapse duplicate samples at the SAME DateTime (per id_cols) via mean
  collapsed <- x |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(id_cols2, "Date", "DateTime")))) |>
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
    dplyr::group_by(dplyr::across(dplyr::all_of(c(id_cols2, "Date")))) |>
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
    dplyr::arrange(dplyr::across(dplyr::all_of(id_cols2)), DateTime)
  
  
  return(result)
}

# Function to calculate the duration
duration_cal <- function(x, type, complete_windows = TRUE){
  # Create/standardize window columns
  x <- x |>
    dplyr::mutate(Window_Start = DateTime) |>
    dplyr::mutate(Window_End = DateTime) |>
    dplyr::mutate(DurationMethod_norm = trimws(tolower(DurationMethod)))
  
  x_cols <- names(x)
  
  id_cols <- c(
    "ATTAINS.ParameterName",
    "TADA.CharacteristicName",
    "TADA.ResultSampleFractionText",
    "TADA.MethodSpeciationName",
    "TADA.ResultMeasure.MeasureUnitCode",
    "ATTAINS.UseName",
    "AcuteChronic",
    "UniqueSpatialCriteria",
    "Season",
    "ATTAINS.OrganizationIdentifier",
    "EquationBased",
    "DurationUnit",
    "DurationMethod",
    "DurationValue",
    "FreqValue",
    "FreqMethod",
    "EquationType"
  )
  
  if (type %in% "MLid"){
    base_id_cols <- c("TADA.MonitoringLocationIdentifier")
    # Only add JoinToAU if it exists
    if ("JoinToAU.AssessmentUnitIdentifier" %in% x_cols) {
      id_cols <- c(base_id_cols, "JoinToAU.AssessmentUnitIdentifier", id_cols)
    } else {
      id_cols <- c(base_id_cols, id_cols)
    }
  } else if (type %in% "AU"){
    id_cols <- c("JoinToAU.AssessmentUnitIdentifier", id_cols)
  } else {
    id_cols <- id_cols
  }
  
  # Check if JoinToAU.AssessmentUnitIdentifier exists
  id_cols2 <- base::intersect(x_cols, id_cols)
  
  x_ordered <- x |>
    dplyr::arrange(dplyr::across(dplyr::all_of(id_cols2)), Window_Start)
  
  result <- x_ordered |>
    dplyr::group_by(dplyr::across(dplyr::all_of(id_cols2))) |>
    dplyr::mutate(G_ID = dplyr::cur_group_id()) |>
    dplyr::group_modify(function(x2, keys){
      df <- x2 |>
        dplyr::arrange(Window_Start)
      
      idx        <- df$Window_Start
      before_per <- window_before_period(df$DurationUnit[1], df$DurationValue[1])
      agg_raw    <- df$DurationMethod[1]
      agg_norm   <- df$DurationMethod_norm[1]
      
      # Handle NA in agg_norm
      if(is.na(agg_norm)) {
        agg_norm <- "arithmetic mean"  # Default value
      }
      
      # Measurement windows (compute multiple stats so we can map by label)
      win_mean   <- slider::slide_index_dbl(df$Value, idx, na_mean,  .before = before_per, .complete = complete_windows)
      win_min    <- slider::slide_index_dbl(df$Value, idx, na_min,   .before = before_per, .complete = complete_windows)
      win_max    <- slider::slide_index_dbl(df$Value, idx, na_max,   .before = before_per, .complete = complete_windows)
      win_gmean  <- slider::slide_index_dbl(df$Value, idx, na_gmean, .before = before_per, .complete = complete_windows)
      
      # Also compute window min/max for "extremes" evaluation later
      Value_win_min <- win_min
      Value_win_max <- win_max
      
      Result_Duration <- dplyr::case_when(
        agg_norm %in% c("arithmetic mean", "rolling arithmetic mean") ~ win_mean,
        agg_norm %in% "arithmetic max"       ~ win_max,
        agg_norm %in% "arithmetic min"       ~ win_min,
        agg_norm %in% c("geometric mean")    ~ win_gmean,
        agg_norm %in% "arithmetic extremes"  ~ NA_real_,  # use Value_win_min / Value_win_max vs thresholds later
        TRUE                                 ~ win_mean
      )
      
      # Covariates — always means
      pH_win          <- slider::slide_index_dbl(df$pH,          idx, na_mean, .before = before_per, .complete = complete_windows)
      Temperature_win <- slider::slide_index_dbl(df$Temperature, idx, na_mean, .before = before_per, .complete = complete_windows)
      Hardness_win    <- slider::slide_index_dbl(df$Hardness,    idx, na_mean, .before = before_per, .complete = complete_windows)
      
      # Counts & explicit bounds
      N_in_Window      <- slider::slide_index_int(!is.na(df$Value), idx, sum, .before = before_per, .complete = complete_windows)
      
      Window_Start_win <- slider::slide_index_vec(idx, idx, 
                                                  function(x){
                                                    if (length(x) > 0){
                                                      return(min(x, na.rm = TRUE)) 
                                                    } else {
                                                      return(as.POSIXct(NA))
                                                    }},
                                                  .before = before_per, .complete = complete_windows, .ptype = df$Window_Start)
      Window_End_win   <- idx
      
      # Status logic
      time_complete <- !is.na(Window_Start_win)
      Window_Status <- dplyr::case_when(
        !time_complete                                  ~ "incomplete",
        TRUE                                            ~ "complete"
      )
      
      # Handle step_label more safely
      window_step_value <- tryCatch({
        step_label(df$DurationUnit[1])
      }, error = function(e) {
        "1 day"  # Default value if step_label fails
      })
      
      dplyr::tibble(
        G_ID = df$G_ID[1],
        # keep canonical point value for reference
        Value        = df$Value,
        # window metadata
        Window_Start_win   = Window_Start_win,
        Window_End_win     = Window_End_win,
        Window_Step        = window_step_value,
        N_in_Window        = N_in_Window,
        Stat_Method        = agg_raw,
        # windowed measurement
        Result_Duration    = Result_Duration,
        Value_win_min      = Value_win_min,
        Value_win_max      = Value_win_max,
        # thresholds (non-equation)
        Threshold_Lower_win = if(length(df$MagnitudeValueLower) > 0) df$MagnitudeValueLower[1] else NA_real_,
        Threshold_Upper_win = if(length(df$MagnitudeValueUpper) > 0) df$MagnitudeValueUpper[1] else NA_real_,
        # windowed covariates
        pH_win             = pH_win,
        Temperature_win    = Temperature_win,
        Hardness_win       = Hardness_win,
        # status
        Window_Status      = Window_Status
      )
    }, .keep = TRUE) |>
    dplyr::ungroup() |>
    dplyr::select(-G_ID)
  
  return(result)
} 

### A function to compare the excursion
duration_excursion_fun <- function(x){
  x2 <- x |>
    dplyr::mutate(Duration_Excursion = dplyr::case_when(
      is.na(Threshold_Lower_win) & !is.na(Threshold_Upper_win) & 
        E_Value > Threshold_Upper_win                                   ~   TRUE,
      !is.na(Threshold_Lower_win) & is.na(Threshold_Upper_win) & 
        E_Value < Threshold_Lower_win                                   ~   TRUE,
      !is.na(Threshold_Lower_win) & !is.na(Threshold_Upper_win) & 
        (E_Value < Threshold_Lower_win | E_Value > Threshold_Upper_win)   ~   TRUE,
      TRUE                                                            ~  FALSE
    ))
  return(x2)
}

# A function to update the magnitude
magnitude_update <- function(x, match_type){
  
  ## Hardness
  dat_hardness <- x |>
    dplyr::filter(EquationType %in% "Hardness") |>
    # Check the completeness of the input data
    dplyr::filter(dplyr::if_any(c(Hardness_win), ~!is.na(.)))
  
  if (nrow(dat_hardness) > 0){
    if (match_type %in% "Option 1"){
      hardness_equation2 <- hardness_equation
    } else {
      y_col <- names(hardness_equation)
      y_col2 <- y_col[!y_col %in% "TADA.ResultSampleFractionText"]
      
      hardness_equation2 <- hardness_equation |> 
        dplyr::distinct(dplyr::across(dplyr::all_of(y_col2)))
    }
    
    dat_hardness2 <- dat_hardness |>
      dplyr::left_join(hardness_equation2) |>
    dplyr::mutate(MagnitudeValueUpper = purrr::pmap_dbl(
        list("hardness" = Hardness_win,
             "CF_A" = hardness_param_1, 
             "CF_B" = hardness_param_2, 
             "CF_C" = hardness_param_3,
             "E_A" = hardness_param_4, 
             "E_B" = hardness_param_5),
        .f = hardness_eq
    )) 
  } else {
    dat_hardness2 <- dat_hardness
  }
  
  # pH
  dat_pH <- x |>
    dplyr::filter(EquationType %in% "pH") |>
    # Check the completeness of the input data
    dplyr::filter(dplyr::if_any(c(pH_win), ~!is.na(.))) 
  
  if (nrow(dat_pH) > 0){
    
    if (match_type %in% "Option 1"){
      pH_equation2 <- pH_equation
    } else {
      y_col <- names(pH_equation)
      y_col2 <- y_col[!y_col %in% "TADA.ResultSampleFractionText"]
      
      pH_equation2 <- pH_equation |> 
        dplyr::distinct(dplyr::across(dplyr::all_of(y_col2)))  
    }
    
    dat_pH2 <- dat_pH |>
      dplyr::left_join(pH_equation2) |>
      dplyr::mutate(
        MagnitudeValueUpper = purrr::map2_dbl(
          Equation, pH_win,
          ~ eval(parse(text = .x), envir = list(pH = .y))
        )
      ) 
  } else {
    dat_pH2 <- dat_pH 
  }
  
  # pH and Hardness
  dat_pH_hardness <- x |>
    dplyr::filter(EquationType %in% "pH and Hardness") |>
    # Check the completeness of the input data
    dplyr::filter(dplyr::if_any(c(pH_win, Hardness_win), ~!is.na(.)))
  
  # Check if data are available
  if (nrow(dat_pH_hardness) > 0){
    
    if (match_type %in% "Option 1"){
      pH_Hardness_equation2 <- pH_Hardness_equation
    } else {
      y_col <- names(pH_Hardness_equation)
      y_col2 <- y_col[!y_col %in% "TADA.ResultSampleFractionText"]
      
      pH_Hardness_equation2 <- pH_Hardness_equation |> 
        dplyr::distinct(dplyr::across(dplyr::all_of(y_col2))) 
    }
    
    dat_pH_hardness2 <- dat_pH_hardness |>
      dplyr::left_join(pH_Hardness_equation2) |>
      dplyr::mutate(MagnitudeValueUpper = purrr::pmap_dbl(
        list("hardness" = Hardness_win,
             "CF_A" = hardness_param_1, 
             "CF_B" = hardness_param_2, 
             "CF_C" = hardness_param_3,
             "E_A" = hardness_param_4, 
             "E_B" = hardness_param_5),
        .f = hardness_eq
      )) |>
      dplyr::mutate(MagnitudeValueUpper = if_else(
        pH_win < 7,
        pmin(hardness_param_6, MagnitudeValueUpper),
        MagnitudeValueUpper
      )) 
  } else {
    dat_pH_hardness2 <- dat_pH_hardness
  }
  
  # pH and Temperature
  dat_pH_temperature <- x |>
    dplyr::filter(EquationType %in% "pH and Temperature") |>
    # Check the completeness of the input data
    dplyr::filter(dplyr::if_any(c(pH_win, Temperature_win), ~!is.na(.)))
  
  # Check if data are available
  if (nrow(dat_pH_temperature) > 0){
    
    if (match_type %in% "Option 1"){
      pH_Temperature__equation2 <- pH_Temperature__equation
    } else {
      y_col <- names(pH_equation)
      y_col2 <- y_col[!y_col %in% "TADA.ResultSampleFractionText"]
      
      pH_Temperature_equation2 <- pH_Temperature_equation |> 
        dplyr::distinct(dplyr::across(dplyr::all_of(y_col2))) 
    }
    
    dat_pH_temperature2 <- dat_pH_temperature |>
      dplyr::left_join(pH_Temperature_equation2) |>
      dplyr::mutate(
        MagnitudeValueUpper = purrr::pmap_dbl(
          list(Equation = Equation, pH = pH_win, Temperature = Temperature_win),
          ~ eval(parse(text = .x), envir = list(pH = .y, Temperature = .z))
        )
      ) 
  } else {
    dat_pH_temperature2 <- dat_pH_temperature
  }
  
  
  dat_eq <- dplyr::bind_rows(
    dat_hardness2,
    dat_pH2,
    dat_pH_hardness2,
    dat_pH_temperature2
  )
  
  return(dat_eq)
  
}

# Function to calculate the frequency
frequency_summary <- function(x, type){
  
  id_cols <- c(
    "ATTAINS.ParameterName",
    "TADA.CharacteristicName",
    "TADA.ResultSampleFractionText",
    "TADA.MethodSpeciationName",
    "TADA.ResultMeasure.MeasureUnitCode",
    "ATTAINS.UseName",
    "AcuteChronic",
    "UniqueSpatialCriteria",
    "Season",
    "ATTAINS.OrganizationIdentifier",
    "EquationBased",
    "DurationUnit",
    "DurationMethod",
    "DurationValue",
    "FreqValue",
    "FreqMethod",
    "EquationType"
  )
  
  x_cols <- names(x)
  
  if (type %in% "MLid"){
    base_id_cols <- c("TADA.MonitoringLocationIdentifier")
    # Only add JoinToAU if it exists
    if ("JoinToAU.AssessmentUnitIdentifier" %in% x_cols) {
      id_cols <- c(base_id_cols, "JoinToAU.AssessmentUnitIdentifier", id_cols)
    } else {
      id_cols <- c(base_id_cols, id_cols)
    }
  } else if (type %in% "AU"){
    id_cols <- c("JoinToAU.AssessmentUnitIdentifier", id_cols)
  } else {
    id_cols <- id_cols
  }
  
  # Check if JoinToAU.AssessmentUnitIdentifier exists
  id_cols2 <- base::intersect(x_cols, id_cols)
  
  # Remove methods not able to be calculated for now
  x2 <- x |>
    dplyr::filter(FreqMethod %in% 
                    c("NumberNotMeeting", "n-samples in 3 years",
                      "Percent of samples not meeting", "Percentile"))
  
  # Percentile
  x_P <- x2 |>
    dplyr::filter(FreqMethod %in% "Percentile")
  
  x_other <- x2 |>
    dplyr::filter(!FreqMethod %in% "Percentile")
  
  # Copy the Result_Duration value to E_Value if the frequency 
  # is not the percentile method
  if (nrow(x_other) > 0){
    x_other2 <- x_other |> 
      dplyr::mutate(E_Value = Result_Duration) |>
      dplyr::mutate(Percentile = NA_real_)
  } else {
    x_other2 <- x_other |>
      dplyr::mutate(E_Value = NA_real_) |>
      dplyr::mutate(Percentile = NA_real_)
  }
  
  # Apply different methods to each group
  
  # Percentile: Calculate the percentile
  if (nrow(x_P) > 0){
    x_P2 <- x_P |>
      dplyr::group_by(dplyr::across(dplyr::all_of(id_cols2))) |>
      dplyr::mutate(FreqValue = FreqValue/100) |>
      dplyr::mutate(Percentile = quantile(Result_Duration,
                                          probs = first(FreqValue))) |>
      dplyr::mutate(E_Value = Percentile) |>
      dplyr::ungroup()
  } else {
    x_P2 <- x_P |> 
      dplyr::mutate(Percentile = NA_real_) |>
      dplyr::mutate(E_Value = NA_real_)
  }
  
  # Evaluate the excursions
  
  x3 <- dplyr::bind_rows(x_other2, x_P2)
  x4 <- x3 |> duration_excursion_fun()
  
  # Evaluate the exceedance based on id_cols
  # Separate x4 based on FreqMethod
  x4_number <- x4 |>
    dplyr::filter(FreqMethod %in% "NumberNotMeeting")
  
  x4_n3years <- x4 |>
    dplyr::filter(FreqMethod %in% "n-samples in 3 years")
  
  x4_percentage <- x4 |>
    dplyr::filter(FreqMethod %in% "Percent of samples not meeting")
  
  x4_percentile <- x4 |>
    dplyr::filter(FreqMethod %in% "Percentile")
  
  # NumberNotMeeting method
  if (nrow(x4_number) > 0){
    x4_number2 <- x4_number |>
      dplyr::group_by(dplyr::across(dplyr::all_of(id_cols2))) |>
      dplyr::summarize(Sample_Count = dplyr::n(),
                       Start_Date = min(Window_End_win, na.rm = TRUE),
                       End_Date = max(Window_End_win, na.rm = TRUE),                     
                       Number_of_Excursions = modSum(Duration_Excursion)) |>
      dplyr::mutate(Excursion_Percentage = Number_of_Excursions/Sample_Count * 100) |>
      dplyr::mutate(Exceedance = ifelse(Number_of_Excursions > 0, "Exceed", "Not Exceed")) |>
      dplyr::ungroup() |>
      dplyr::mutate(Percentile = NA_real_) |>
      dplyr::mutate(Sufficient_Data = "Yes")
  } else {
    x4_number2 <- x4_number |>
      dplyr::select(dplyr::all_of(id_cols2)) |>
      dplyr::mutate(
        Sample_Count = NA_integer_,
        Start_Date = as.POSIXct(NA),
        End_Date = as.POSIXct(NA),
        Number_of_Excursions = NA_integer_,
        Excursion_Percentage = NA_real_,
        Exceedance = NA_character_,
        Percentile = NA_real_,
        Sufficient_Data = NA_character_
      )
  }
  
  
  # Percent of samples not meeting Method
  if (nrow(x4_percentage) > 0){
    x4_percentage2 <- x4_percentage |>
      dplyr::group_by(dplyr::across(dplyr::all_of(id_cols2))) |>
      dplyr::summarize(Sample_Count = dplyr::n(),
                       Start_Date = min(Window_End_win, na.rm = TRUE),
                       End_Date = max(Window_End_win, na.rm = TRUE),                     
                       Number_of_Excursions = modSum(Duration_Excursion),
                       FreqValue = first(FreqValue)) |>
      dplyr::mutate(Excursion_Percentage = Number_of_Excursions/Sample_Count * 100) |>
      dplyr::mutate(Exceedance = ifelse(Excursion_Percentage > FreqValue, 
                                        "Exceed", "Not Exceed")) |>
      dplyr::ungroup() |>
      dplyr::mutate(Percentile = NA_real_) |>
      dplyr::mutate(Sufficient_Data = "Yes")
  } else {
    x4_percentage2 <- x4_percentage |>
      dplyr::select(dplyr::all_of(id_cols2)) |>
      dplyr::mutate(
        Sample_Count = NA_integer_,
        Start_Date = as.POSIXct(NA),
        End_Date = as.POSIXct(NA),
        Number_of_Excursions = NA_integer_,
        Excursion_Percentage = NA_real_,
        Exceedance = NA_character_,
        Percentile = NA_real_,
        Sufficient_Data = NA_character_
      )
  }
  
  
  # Percentile Method
  if (nrow(x4_percentile) > 0){
    x4_percentile2 <- x4_percentile |>
      dplyr::group_by(dplyr::across(dplyr::all_of(id_cols2))) |>
      dplyr::summarize(Sample_Count = dplyr::n(),
                       Start_Date = min(Window_End_win, na.rm = TRUE),
                       End_Date = max(Window_End_win, na.rm = TRUE), 
                       Percentile = first(Percentile),
                       Number_of_Excursions = modSum(Duration_Excursion)) |>
      dplyr::mutate(Exceedance = ifelse(Number_of_Excursions > 0, "Exceed", "Not Exceed")) |>
      dplyr::ungroup() |>
      dplyr::mutate(Number_of_Excursions = NA_real_) |>
      dplyr::mutate(Excursion_Percentage = NA_real_) |>
      dplyr::mutate(Sufficient_Data = "Yes")
  } else {
    x4_percentile2 <- x4_percentile |>
      dplyr::select(dplyr::all_of(id_cols2)) |>
      dplyr::mutate(
        Sample_Count = NA_integer_,
        Start_Date = as.POSIXct(NA),
        End_Date = as.POSIXct(NA),
        Number_of_Excursions = NA_integer_,
        Excursion_Percentage = NA_real_,
        Exceedance = NA_character_,
        Percentile = NA_real_,
        Sufficient_Data = NA_character_
      )
  }
  
  
  # "n-samples in 3 years"
  
  # --- n-samples in 3 years ----------------------------------------------------
  # Assumptions:
  # - x4_n3years has one row per window with columns:
  #     Window_End_win (date/time), Duration_Excursion (0/1), FreqValue
  # - We count windows with Duration_Excursion == 1 within each trailing 3-year span.
  # - We report the "worst" (max excursions) 3-year block per group.
  
  if (nrow(x4_n3years) > 0) {
    # trailing span length: 3 years inclusive
    three_year_span <- lubridate::years(3) - lubridate::days(1)
    
    # helper to coalesce NA excursions to 0
    nz <- function(z) ifelse(is.na(z), 0L, as.integer(z))
    
    x4_n3years2 <- x4_n3years |>
      dplyr::arrange(dplyr::across(dplyr::all_of(id_cols2)), Window_End_win) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(id_cols2))) |>
      dplyr::group_modify(function(df, keys) {
        idx <- df$Window_End_win
        
        # Check if data exist
        has_span <- {
          earliest <- suppressWarnings(min(idx, na.rm = TRUE))
          latest   <- suppressWarnings(max(idx, na.rm = TRUE))
          if (is.infinite(earliest) || is.infinite(latest)) FALSE
          else (latest - earliest) >= three_year_span
        }
        
        # rolling count of excursions over trailing 3 years
        n_exc_3yr <- slider::slide_index_int(
          .x = nz(df$Duration_Excursion),
          .i = idx,
          .f = sum,
          .before   = three_year_span,
          .complete = FALSE
        )
        
        # number of windows contributing to each 3-year span
        n_win_3yr <- slider::slide_index_int(
          .x = !is.na(df$Duration_Excursion),
          .i = idx,
          .f = sum,
          .before   = three_year_span,
          .complete = FALSE
        )
        
        # corresponding start date of each 3-year span
        start_3yr <- slider::slide_index_vec(
          .x = idx, .i = idx,
          .f = function(v) if (length(v)) min(v) else as.POSIXct(NA),
          .before   = three_year_span,
          .complete = FALSE,
          .ptype = df$Window_End_win
        )
        
        end_3yr <- idx
        
        # pick the "worst" window (max excursions) per group
        worst_i <- which.max(ifelse(is.na(n_exc_3yr), -Inf, n_exc_3yr))
        if (length(worst_i) == 0L || is.infinite(max(n_exc_3yr, na.rm = TRUE))) {
          return(dplyr::tibble())
        }
        
        Number_of_Excursions <- n_exc_3yr[worst_i]
        Sample_Count         <- n_win_3yr[worst_i]
        Start_Date           <- start_3yr[worst_i]
        End_Date             <- end_3yr[worst_i]
        
        # compare to allowable count in 3 years
        allow_n <- suppressWarnings(as.integer(df$FreqValue[worst_i]))
        # if NA, treat as 0 allowed (or choose your policy)
        if (is.na(allow_n)) allow_n <- 0L
        
        Exceedance <- ifelse(Number_of_Excursions > allow_n, "Exceed", "Not Exceed")
        
        dplyr::tibble(
          Sample_Count         = Sample_Count,
          Start_Date           = Start_Date,
          End_Date             = End_Date,
          Number_of_Excursions = Number_of_Excursions,
          Excursion_Percentage = NA_real_,
          Exceedance           = Exceedance,
          Percentile           = NA_real_,
          Sufficient_Data      = "Yes"
        )
      }, .keep = TRUE) |>
      dplyr::ungroup()
  } else {
    x4_n3years2 <- x4_n3years |>
      dplyr::select(dplyr::all_of(id_cols2)) |>
      dplyr::mutate(
        Sample_Count = NA_integer_,
        Start_Date = as.POSIXct(NA),
        End_Date = as.POSIXct(NA),
        Number_of_Excursions = NA_integer_,
        Excursion_Percentage = NA_real_,
        Exceedance = NA_character_,
        Percentile = NA_real_,
        Sufficient_Data = NA_character_ 
      )
  }
  
  # Combine the data
  x5 <- dplyr::bind_rows(x4_number2, x4_percentage2, x4_percentile2, x4_n3years2) |>
    dplyr::relocate("Exceedance", .after = "Percentile")
  
  return(x5)
}

# Helper functions for duration and frequency analysis
window_before_period <- function(unit, value) {
  if (is.na(value)) value <- 1
  if (is.na(unit)) unit <- "n-day"
  if (unit == "n-hour")   return(lubridate::hours(max(value, 1) - 1))
  if (unit == "n-day")    return(lubridate::days(max(value, 1) - 1))
  if (unit == "n-month")  return(lubridate::months(max(value, 1)) - lubridate::days(1))
  if (unit == "n-season") return(lubridate::months(3L * max(value, 1)) - lubridate::days(1))
  lubridate::days(max(value, 1) - 1)
}


step_label <- function(step) {
  if(is.na(step)) return("1 day")
  if(step == "n-hour") return("1 hour")
  return("1 day")
}

na_mean  <- function(x) { x <- x[!is.na(x)]; if (!length(x)) NA_real_ else mean(x) }
na_min   <- function(x) { x <- x[!is.na(x)]; if (!length(x)) NA_real_ else min(x)  }
na_max   <- function(x) { x <- x[!is.na(x)]; if (!length(x)) NA_real_ else max(x)  }
na_gmean <- function(x) { x <- x[is.finite(x) & !is.na(x) & x > 0]; if (!length(x)) NA_real_ else exp(mean(log(x))) }

### This function simplify the duration and frequency summary output
simplify_duration_frequency <- function(x){
  x2 <- x |>
    dplyr::mutate(DurationUnit = stringr::str_remove(DurationUnit, "n")) |>
    dplyr::mutate(
      DurationValueUnit = stringr::str_c(DurationValue, DurationUnit),
      .keep = "unused"
    ) |>
    dplyr::mutate(
      Duration = stringr::str_c(DurationValueUnit, DurationMethod, sep = " "),
      .keep = "unused"
    ) |>
    dplyr::mutate(
      Frequency = stringr::str_c(FreqValue, FreqMethod, 
                                 sep = " "),
      .keep = "unused"
    )
}

# Hardness calculation function
hardness_eq <- function(hardness, E_A, E_B, CF_A, CF_B, CF_C){
  if (is.na(CF_A) & is.na(CF_B)){
    CF2 <- CF_C
  } else if (!is.na(CF_A) & !is.na(CF_B)){
    CF2 <- CF_A - (log(hardness) * CF_B)
  }
  result <- exp(E_A * log(hardness) + E_B) * CF2
  
  return(result)
}

