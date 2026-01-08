#' map_viewer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_map_viewer_ui <- function(id) {
  ns <- NS(id)
  tagList(
    leaflet::leafletOutput(outputId = ns("summary_map"),
                           height = "600px")
  )
}
    
#' map_viewer Server Functions
#'
#' @noRd 
mod_map_viewer_server <- function(id, tadat){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    shiny::observeEvent(tadat$exceed_summary_f, {
      
      # Handle NULL or empty data - clear/reset the map
      if (is.null(tadat$exceed_summary_f) || nrow(tadat$exceed_summary_f) == 0) {
        # Create an empty base map
        output$summary_map <- leaflet::renderLeaflet({
          leaflet::leaflet() |>
            leaflet::addTiles() |>
            leaflet::setView(lng = -98.5795, lat = 39.8283, zoom = 4)  # Default view (US center)
        })
        return()
      }
      
      # Create leaflet map
      output$summary_map <- leaflet::renderLeaflet({
        
        # Create the map data
        dat <- tadat$exceed_summary_f
        
        dat2 <- dat |> map_summary(type = tadat$loc_select)
        
        # Check if data is available
        req(dat2)
        req(nrow(dat2) > 0)
        
        # Define colors based on Exceedance_Result
        color_palette <- leaflet::colorFactor(
          palette = c("#28a745", "#dc3545"),  # Green for FALSE, Red for TRUE
          domain = c(FALSE, TRUE)
        )
        
        # Create the leaflet map
        leaflet::leaflet(dat2) |>
          # Add base tiles
          leaflet::addTiles() |>
          # Set initial view (centered on data)
          leaflet::setView(
            lng = mean(dat2$TADA.LongitudeMeasure, na.rm = TRUE),
            lat = mean(dat2$TADA.LatitudeMeasure, na.rm = TRUE),
            zoom = 6
          ) |>
          # Add circle markers
          leaflet::addCircleMarkers(
            lng = ~TADA.LongitudeMeasure,
            lat = ~TADA.LatitudeMeasure,
            radius = 8,
            color = ~color_palette(Exceedance_Result),
            fillColor = ~color_palette(Exceedance_Result),
            fillOpacity = 0.8,
            opacity = 1,
            weight = 2,
            # Popup content (click)
            popup = ~paste0(
              "<strong>Location:</strong> ", TADA.MonitoringLocationName, "<br>",
              "<strong>Location ID:</strong> ", TADA.MonitoringLocationIdentifier, "<br>",
              ifelse("ATTAINS.AssessmentUnitIdentifier" %in% names(dat2),
                     paste0("<strong>Assessment Unit:</strong> ", ATTAINS.AssessmentUnitIdentifier, "<br>"),
                     ""),
              "<strong>Exceedance:</strong> ", ifelse(Exceedance_Result, "Yes", "No"), "<br>",
              "<strong>Coordinates:</strong> ", round(TADA.LatitudeMeasure, 4), ", ", 
              round(TADA.LongitudeMeasure, 4)
            ),
            # Label content (hover)
            label = ~lapply(Description, function(x) {
              htmltools::HTML(gsub("\n", "<br>", x))
            }),
            labelOptions = leaflet::labelOptions(
              style = list(
                "font-weight" = "normal",
                "padding" = "3px 8px",
                "background-color" = "white",
                "border" = "1px solid #ccc",
                "border-radius" = "4px",
                "box-shadow" = "3px 3px 10px rgba(0,0,0,0.2)"
              ),
              textsize = "12px",
              direction = "auto"
            )
          ) |>
          # Add legend
          leaflet::addLegend(
            position = "bottomright",
            colors = c("#28a745", "#dc3545"),
            labels = c("No Exceedance", "Exceedance"),
            title = "Exceedance Status",
            opacity = 0.8
          )
      })
    }, ignoreNULL = FALSE)
    })
}
    
## To be copied in the UI
# mod_map_viewer_ui("map_viewer_1")
    
## To be copied in the server
# mod_map_viewer_server("map_viewer_1")
