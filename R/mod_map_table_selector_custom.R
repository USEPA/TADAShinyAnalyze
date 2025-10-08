#' map_table_selector UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_map_table_selector_custom_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        htmltools::h4("Choose sites for the analysis"),
        htmltools::p("Select the sites on the map or in the table to conduct analysis. Use the checkbox to select all sites"),
        htmltools::p("If users select 'Custom Grouping' in the 'Custom Analyzed by the spatial unit' menu, selected site will be grouped for the analysis."),
        htmltools::div(
          style = "margin-bottom: 30px;",
          shinyjs::disabled(
            shiny::actionButton(inputId = ns("select_all_sites"), label = "Select All Sites", shiny::icon("circle-check"),
                                style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
            shiny::actionButton(inputId = ns("deselect_all_sites"), label = "Deselect All Sites", shiny::icon("circle"),
                                style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        leaflet::leafletOutput(ns("map_selector_custom"))
      ),
      column(
        width = 6,
        DT::DTOutput(ns("table_selector_custom"))
      )
    )
  )
}
    
#' map_table_selector Server Functions
#'
#' @noRd 
mod_map_table_selector_custom_server <- function(id, tadat){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Initialize reactive value for tracking selected indices
    selected_idx <- reactiveVal(integer(0))
  
    shiny::observe({
      req(tadat$state_tribe_custom, tadat$uses_select_re_custom)

      # Map selector
      output$map_selector_custom <- leaflet::renderLeaflet({
        req(is.data.frame(tadat$site_AU_table_custom))
        req(nrow(tadat$site_AU_table_custom) > 0)
        
        dat <- tadat$site_AU_table_custom
        
        if (tadat$use_type_custom %in% "Option 1"){
          temp_dat <- dat |>
            # Create a label column
            dplyr::mutate(label = paste0("Site ID: ", "<strong>", TADA.MonitoringLocationIdentifier, "</strong>", "<br/>",
                                         "Site Name: ", "<strong>", TADA.MonitoringLocationName, "</strong>", "<br/>",
                                         "AU ID: ", "<strong>", JoinToAU.AssessmentUnitIdentifier,  "</strong>", "<br/>")) 
        } else {
          temp_dat <- dat |>
            # Create a label column
            dplyr::mutate(label = paste0("Site ID: ", "<strong>", TADA.MonitoringLocationIdentifier, "</strong>", "<br/>",
                                         "Site Name: ", "<strong>", TADA.MonitoringLocationName, "</strong>", "<br/>")) 
        }
        
        labs <- as.list(temp_dat$label)
        
        leaflet::leaflet(temp_dat) |>
          add_USGS_base() |>
          leaflet::addCircleMarkers(lng = ~TADA.LongitudeMeasure,
                                    lat = ~TADA.LatitudeMeasure,
                                    layerId = ~TADA.MonitoringLocationIdentifier,
                                    radius = 8, stroke = TRUE, weight = 1,
                                    color = "black",
                                    fillColor = "blue",
                                    fillOpacity = 1,
                                    opacity = 0.5,
                                    label = purrr::map(labs, htmltools::HTML),
                                    group = "base_map")
        
      })
      
      # Table selector
      output$table_selector_custom <- DT::renderDT({
        req(is.data.frame(tadat$site_AU_table_custom))
        req(nrow(tadat$site_AU_table_custom) > 0)
        
        dat <- tadat$site_AU_table_custom
        
        DT::datatable(
          dat,
          filter = "top",
          class = "compact",
          selection = list(mode = "multiple"),
          options = list(scrollX = TRUE,
                         scrollY = TRUE,
                         pageLength = 10,
                         lengthMenu = c(10, 25, 50, 100),
                         autoWidth = TRUE))
      })
      
      # Proxy
      map_selector_custom_proxy   <- leaflet::leafletProxy("map_selector_custom", session = session)
      table_selector_custom_proxy <- DT::dataTableProxy("table_selector_custom", session = session)
      
      # Activate the action button
      shiny::observe({
        shinyjs::toggleState(id = "select_all_sites",
                             condition = !is.null(tadat$site_AU_table_custom))
        shinyjs::toggleState(id = "deselect_all_sites",
                             condition = !is.null(tadat$site_AU_table_custom))
      })
      
      # Handle the checkbox
      observeEvent(input$select_all_sites, {
        req(is.data.frame(tadat$site_AU_table_custom))
        req(nrow(tadat$site_AU_table_custom) > 0)
        
        # Select all rows
        all_rows <- seq_len(nrow(tadat$site_AU_table_custom))
        selected_idx(all_rows)
        table_selector_custom_proxy |> DT::selectRows(all_rows)
        
        # Highlight all points on map
        map_selector_custom_proxy |> leaflet::clearGroup("highlighted_point")
        map_selector_custom_proxy |>
          leaflet::addCircleMarkers(
            lng = tadat$site_AU_table_custom$TADA.LongitudeMeasure,
            lat = tadat$site_AU_table_custom$TADA.LatitudeMeasure,
            layerId = paste0(tadat$site_AU_table_custom$TADA.MonitoringLocationIdentifier, "_new"),
            radius = 9, stroke = TRUE, weight = 1,
            color = "black", fillColor = "red",
            fillOpacity = 1, opacity = 0.8,
            group = "highlighted_point"
          )
        
      }, ignoreInit = TRUE)
      
      observeEvent(input$deselect_all_sites, {
        # Clear all selections
        selected_idx(integer(0))
        table_selector_custom_proxy |> DT::selectRows(numeric(0))
        map_selector_custom_proxy |> leaflet::clearGroup("highlighted_point")
      })
      
      # DT -> Map: highlight selected points
      observeEvent(input$table_selector_custom_rows_selected, ignoreInit = TRUE, ignoreNULL = FALSE, {
        req(tadat$site_AU_table_custom)
        
        # Get current selection - explicitly handle NULL/empty case
        cur <- input$table_selector_custom_rows_selected
        if (is.null(cur) || length(cur) == 0) {
          cur <- integer(0)
        }
        
        # Update the reactive value
        selected_idx(cur)
        
        # Always clear existing highlights first
        map_selector_custom_proxy |> leaflet::clearGroup("highlighted_point")
        
        # Only add highlights if there are selections
        if (length(cur) > 0) {
          # Get selected rows data
          sel <- tadat$site_AU_table_custom |> dplyr::slice(cur)
          
          if (nrow(sel) > 0) {
            map_selector_custom_proxy |>
              leaflet::addCircleMarkers(
                lng = sel$TADA.LongitudeMeasure,
                lat = sel$TADA.LatitudeMeasure,
                layerId = paste0(sel$TADA.MonitoringLocationIdentifier, "_new"),
                radius = 9, stroke = TRUE, weight = 1,
                color = "black", fillColor = "red",
                fillOpacity = 1, opacity = 0.8,
                group = "highlighted_point"
              )
          }
        }
      })
      
      # Map -> DT: clicking markers toggles table selection
      observeEvent(input$map_selector_custom_marker_click, {
        req(is.data.frame(tadat$site_AU_table_custom))
        req(nrow(tadat$site_AU_table_custom) > 0)
        
        click_info <- input$map_selector_custom_marker_click
        if (is.null(click_info)) return()
        
        id <- click_info$id
        tbl <- tadat$site_AU_table_custom
        cur <- selected_idx()
        
        if (stringr::str_detect(id, "_new$")) {
          # Clicked a red overlay marker -> remove selection
          base_id <- stringr::str_remove(id, "_new$")
          row <- which(tbl$TADA.MonitoringLocationIdentifier == base_id)
          
          # Remove from selection
          new_selection <- setdiff(cur, row)
          
          # Update the reactive value
          selected_idx(new_selection)
          
          # Update table selection - handle empty case explicitly
          if (length(new_selection) == 0) {
            # Force clear the table selection
            table_selector_custom_proxy |> DT::selectRows(numeric(0))
            # Also manually clear the map highlights since DT might not trigger
            map_selector_custom_proxy |> leaflet::clearGroup("highlighted_point")
          } else {
            table_selector_custom_proxy |> DT::selectRows(new_selection)
          }
          
        } else {
          # Clicked a base marker -> add or toggle selection
          row <- which(tbl$TADA.MonitoringLocationIdentifier == id)
          
          if (length(row) > 0) {
            if (row %in% cur) {
              # Already selected, remove it
              new_selection <- setdiff(cur, row)
            } else {
              # Not selected, add it
              new_selection <- sort(unique(c(cur, row)))
            }
            
            # Update the reactive value
            selected_idx(new_selection)
            
            # Update table selection - handle empty case explicitly
            if (length(new_selection) == 0) {
              # Force clear the table selection
              table_selector_custom_proxy |> DT::selectRows(numeric(0))
              # Also manually clear the map highlights
              map_selector_custom_proxy |> leaflet::clearGroup("highlighted_point")
            } else {
              table_selector_custom_proxy |> DT::selectRows(new_selection)
            }
          }
        }
      }, ignoreInit = TRUE)
      
      # Reset selection when dataset changes
      observeEvent(c(tadat$state_tribe_custom, tadat$uses_select_re_custom), {
        # Clear selections when filters change
        selected_idx(integer(0))
        table_selector_custom_proxy |> DT::selectRows(numeric(0))
        map_selector_custom_proxy |> leaflet::clearGroup("highlighted_point")
      }, ignoreInit = TRUE)
    })
    
    # Create a reactive expression to get selected monitoring location IDs
    selected_monitoring_locations <- reactive({
      req(tadat$site_AU_table_custom)
      
      # Get current selected indices
      idx <- selected_idx()
      
      # If nothing is selected, return empty character vector
      if (length(idx) == 0) {
        return(character(0))
      }
      
      # Get the selected rows from the table
      selected_rows <- tadat$site_AU_table_custom |> 
        dplyr::slice(idx)
      
      # Extract the MonitoringLocationIdentifier values
      selected_ids <- selected_rows$TADA.MonitoringLocationIdentifier
      
      return(selected_ids)
    })
    
    # Update tadat whenever selection changes
    observeEvent(selected_monitoring_locations(), {
      # Store the selected monitoring location IDs in tadat
      tadat$selected_monitoring_locations_custom <- selected_monitoring_locations()
      
      # Optional: Also store the full selected rows if needed
      if (length(selected_idx()) > 0) {
        tadat$selected_sites_data_custom <- tadat$site_AU_table_custom |> 
          dplyr::slice(selected_idx())
      } else {
        tadat$selected_sites_data_custom <- NULL
      }

    }, ignoreInit = TRUE)
    
  })
}
    
## To be copied in the UI
# mod_map_table_selector_ui("map_table_selector_1")
    
## To be copied in the server
# mod_map_table_selector_server("map_table_selector_1")
