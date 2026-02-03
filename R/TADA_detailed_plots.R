# filter to Ph or E.col
data.ph <- Data_MT_MissoulaCounty |> dplyr::filter(TADA.CharacteristicName == "PH")
EPATADA::TADA_Scatterplot(data.ph)

# convert data to time series with equal time space range
data.ph$ActivityStartDate <- as.POSIXct(data.ph$ActivityStartDate)
EPATADA::TADA_Scatterplot(data.ph)

# display boxplots overlay (By YYYY, YYYYMM, seasons etc?). Must modify ActivityStartDate as that is what TADA_Scatteplot is using for X
data.ph$YYYYMM <- lubridate::ceiling_date(data.ph$ActivityStartDate, "month") - lubridate::days(1)
data.ph$YYYY <- format(data.ph$ActivityStartDate, "%Y")

temp <- EPATADA::TADA_Scatterplot(data.ph) |>
  add_trace(
    x = data.ph$YYYYMM,
    y = data.ph$ResultMeasureValue,
    xaxis = 'x2',
    inherit = TRUE,
    type = 'box',
    name = 'Monthly Boxplot',
    boxpoints = "all", # Show all points
    #jitter = 0,      # Jitter points along the x-axis
    #pointpos = 0,      # Center points within the box
    marker = list(opacity = 0), # Make points in the boxplot trace transparent
    #fillcolor = 'rgba(0, 0, 0, 0)', # Make the box plot itself transparent or add color
    line = list(color = 'rgba(7, 40, 89, 1)'), # Add box border color
    showlegend = FALSE, 
    hoverinfo = T
  )

temp <- temp |>
  layout(
    #boxmode = "group",
    #boxgap = 0.5,
    #boxgroupgap = 0, # Set gap between groups to 0
    xaxis2 = list(
      matches = 'x', # Ensure the new axis matches the main x-axis
      overlaying = 'x', # Overlay it on top of the main x-axis
      side = 'right',
      #hovermode = "y",
      showgrid = FALSE, # Optional: hide the grid lines for the boxplot axis
      zeroline = FALSE, # Optional: hide the zero line
      showticklabels = FALSE # Optional: hide tick labels for the boxplot axis
    )
  )
temp
