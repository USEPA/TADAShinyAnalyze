# filter to Ph or E.col
data.ph <- Data_MT_MissoulaCounty |> 
  dplyr::filter(TADA.CharacteristicName == "PH")|>
  tidyr::drop_na(ResultMeasureValue)
EPATADA::TADA_Scatterplot(data.ph) 

# convert data to time series with equal time space range
data.ph$ActivityStartDate <- as.POSIXct(data.ph$ActivityStartDate)
EPATADA::TADA_Scatterplot(data.ph)

# display boxplots overlay (By YYYY, YYYYMM, seasons etc?). Must modify ActivityStartDate as that is what TADA_Scatteplot is using for X
data.ph$YYYYMM <- lubridate::ceiling_date(data.ph$ActivityStartDate, "month") - lubridate::days(1)
data.ph$YYYY <- format(data.ph$ActivityStartDate, "%Y")

temp <- EPATADA::TADA_Scatterplot(data.ph)

# define bin size (duration period)
# If duration is blank, use default density
hour1 <- 86400000/24
day1 <- 86400000
week1 <- 86400000*7
month1 <- 86400000*7*4

bin_size = 86400000

p2 <- plot_ly(
  data.ph,
  x = ~ActivityStartDate,
  type = 'histogram',
  name = 'Density',
  showlegend = FALSE,
  xbins = list(
    start = min(data.ph$ActivityStartDate), # Set the start date for the first bin
    size = bin_size  # Set bin size to 1 week (in milliseconds)
  ),
  hovertemplate = paste(
    "Count: %{y}<extra></extra><br>",
    "ActivityStartDate: %{x|%Y-%m-%d}"
  ))

temp <- subplot(temp, p2, nrows = 2, heights = c(0.6, 0.3), shareX = TRUE, titleY = TRUE)
temp




### boxplots for ml (up to 4?)
filtered_data <- data.ph |>
  dplyr::filter(MonitoringLocationIdentifier %in% c("USGS-12334550", "MDEQ_WQ_WQX-C04CKFKR05"))

temp3 <- EPATADA::TADA_Boxplot(filtered_data)

temp2 <- plotly::plot_ly(
    filtered_data,
    x = ~MonitoringLocationIdentifier,
    y = ~ResultMeasureValue,
    #xaxis = 'x2',
    boxpoints = "all",  # Displays all points
    type = 'box',
    name = 'Monitoring Location Box Plots',
    boxpoints = "all", # Show all points,
    boxmean = TRUE,
    jitter = 0,      # Jitter points along the x-axis
    pointpos = 0,      # Center points within the box
    marker = list(opacity = 0), # Make points in the boxplot trace transparent
    #fillcolor = 'rgba(0, 0, 0, 0)', # Make the box plot itself transparent or add color
    line = list(color = 'rgba(7, 40, 89, 1)'), # Add box border color
    showlegend = FALSE,
    hoverinfo = T
  )

fig_combined <- subplot(temp2, temp3, nrows = 1, shareY = TRUE)
temp2

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
# 
