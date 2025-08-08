#' analysis_selector UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' 

# Load files
data_path1 <- app_sys("extdata/Criteria_Table_Input.RData")
load(data_path1)

mod_analysis_selector_batch_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 6,
        shiny::radioButtons(inputId = ns("loc_select"),
                            label = "Batch Analyzed by: ",
                            choices = c("Monitoring Location ID" = "MLid",
                                        "Assessment Unit (Individual)" = "AU_ind",
                                        "Assessment Unit (Group)" = "AU_group"))
      ),
      column(
        width = 3,
        shiny::selectizeInput(inputId = ns("state_tribe"),
                              label = "Select state/tribe",
                              choices = NULL)
      ),
      column(
        width = 3,
        shinyWidgets::virtualSelectInput(
          inputId = ns("uses_select"),
          label = "Select the uses:",
          choices = NULL,
          showValueAsTags = TRUE,
          search = TRUE,
          multiple = TRUE
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        leaflet::leafletOutput(ns("map_selector"))
      ),
      column(
        width = 6,
        DT::DTOutput(ns("table_selector"))
      )
    )
  )
}
    
#' analysis_selector Server Functions
#'
#' @noRd 
mod_analysis_selector_batch_server <- function(id, tadat){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Update the Select state/tribe menu
    shiny::observeEvent(tadat$df_mltoau_input, {
      shiny::updateSelectizeInput(
        session = session,
        inputId = "state_tribe",
        options = list(placeholder = "Select the state/tribe", maxItems = 1),
        selected = character(0),
        choices = sort(unique(criteria_table$ATTAINS.OrganizationIdentifier))
      )
    }, ignoreNULL = TRUE)
    
    ### Remove records need to be reviewed in adat$df_mltoau_input
    shiny::observeEvent(tadat$df_mltoau_input, {
      tadat$df_mltoau_input_f <- tadat$df_mltoau_input |>
        dplyr::filter(Needs_Review == "No")
    })
    
    # Update the available uses
    shiny::observeEvent(c(input$state_tribe, tadat$df_autouse_input), {
      req(input$state_tribe)
      req(tadat$df_autouse_input)
      
      criteria_table_f1 <- criteria_table |>
        dplyr::filter(ATTAINS.OrganizationIdentifier %in% input$state_tribe)
      
      # Get the list of available uses from criteria_table_f1
      criteria_uses <- unique(criteria_table_f1$ATTAINS.UseName)
      
      AU_Use_uses <- unique(tadat$df_autouse_input$ATTAINS.UseName)
      
      # Find the intersection
      available_uses <- base::intersect(criteria_uses, AU_Use_uses)
      
      shinyWidgets::updateVirtualSelect(
        session = session,
        inputId = "uses_select",
        choices = sort(available_uses)
      )
    }, ignoreNULL = TRUE)
    
    ### Save the selected loc_select, state_tribe and uses to tadat
    observe({
      tadat$loc_select <- input$loc_select
      tadat$state_tribe <- input$state_tribe
      tadat$uses_select <- input$uses_select
    })
    
    ### Use the saved information to subset the data
    shiny::observe({
      req(tadat$state_tribe, tadat$uses_select)
      
      ### Get the input data and convert ActivityStartDateTime to dateTime
      dat <- tadat$df_mlid_input
      dat <- dat |>
        dplyr::mutate(ActivityStartDateTime = lubridate::ymd_hms(ActivityStartDateTime)) |>
        dplyr::mutate(ActivityStartDate = lubridate::ymd(ActivityStartDate)) |>
        dplyr::mutate(DateTime = ActivityStartDateTime)
      
      ### Step 1: Join pH, Temperature, and Hardness data
      dat2 <- dat |> 
        pH_fun() |>
        Temperature_fun() |>
        hardness_fun()
      
      ### Step 2: Join the criteria table
      criteria_table_f1 <- criteria_table |>
        dplyr::filter(ATTAINS.OrganizationIdentifier %in% tadat$state_tribe) |>
        dplyr::filter(ATTAINS.UseName %in% tadat$uses_select)
      
      # Filter the AU_Use based on available_uses_s
      AU_Use <- tadat$df_autouse_input
      AU_MLID <- tadat$df_mltoau_input_f |>
        dplyr::mutate(TADA.MonitoringLocationIdentifier = 
                        stringr::str_to_upper(MonitoringLocationIdentifier))
      
      AU_Use_f1 <- AU_Use |>
        dplyr::filter(ATTAINS.UseName %in% tadat$uses_select)
      
      # Filter the AU_MLID based on AU_Use_f1
      AU_MLID_f1 <- AU_MLID |>
        dplyr::filter(JoinToAU.AssessmentUnitIdentifier %in% 
                        AU_Use_f1$JoinToAU.AssessmentUnitIdentifier) |>
        dplyr::select(TADA.MonitoringLocationIdentifier, TADA.LongitudeMeasure,
                      TADA.LatitudeMeasure, JoinToAU.AssessmentUnitIdentifier)
      
      # Filter the input data based on AU_MLID_f1
      dat3 <- dat2 |>
        dplyr::filter(TADA.MonitoringLocationIdentifier %in% 
                        AU_MLID_f1$TADA.MonitoringLocationIdentifier)
      
      # Simplify the sites for data
      dat4 <- dat3 |>
        dplyr::distinct(TADA.MonitoringLocationIdentifier,
                        TADA.MonitoringLocationName,
                        TADA.MonitoringLocationTypeName,
                        TADA.LongitudeMeasure,
                        TADA.LatitudeMeasure)
      
      # Join the criteria_table_f1 and AU_MLID_f1 to dat2
      dat5 <- dat4 |>
        dplyr::left_join(AU_MLID_f1) |>
        dplyr::relocate(JoinToAU.AssessmentUnitIdentifier, 
                        .after = "TADA.MonitoringLocationIdentifier")
      
      # dat6 <- dat5 |>
      #   dplyr::left_join(AU_Use_f1, 
      #                    by = "JoinToAU.AssessmentUnitIdentifier",
      #                    relationship = "many-to-many")
      
      # dat5 will be used for a map
      # dat6 will be used for a table
      
      tadat$site_AU_table <- dat5
      # tadat$site_AU_use_table <- dat6
    
      })
    
    shiny::observe({
      req(tadat$state_tribe, tadat$uses_select)
      
      # Map selector
      output$map_selector <- leaflet::renderLeaflet({
        req(tadat$site_AU_table)
        
        dat <- tadat$site_AU_table
        
        temp_dat <- dat |>
          # Create a label column
          dplyr::mutate(label = paste0("Site ID: ", "<strong>", TADA.MonitoringLocationIdentifier, "</strong>", "<br/>",
                                "Site Name: ", "<strong>", TADA.MonitoringLocationName, "</strong>", "<br/>",
                                "AU ID: ", "<strong>", JoinToAU.AssessmentUnitIdentifier,  "</strong>", "<br/>")) 
        
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
      output$table_selector <- DT::renderDT({
        req(tadat$site_AU_table)
        
        dat <- tadat$site_AU_table
        
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
      map_selector_proxy   <- leaflet::leafletProxy("map_selector", session = session)
      table_selector_proxy <- DT::dataTableProxy("table_selector", session = session)
      
      # DT and Leaflet sync
      selected_idx <- reactiveVal(integer(0))
      
      # --- DT -> Map: highlight selected points --------------------------------
      observeEvent(input$table_selector_rows_selected, ignoreInit = TRUE, ignoreNULL = FALSE, {
        req(tadat$site_AU_table)
        
        # Get current selection - explicitly handle NULL/empty case
        cur <- input$table_selector_rows_selected
        if (is.null(cur) || length(cur) == 0) {
          cur <- integer(0)
        }
        
        # Update the reactive value
        selected_idx(cur)
        
        # Always clear existing highlights first
        map_selector_proxy %>% leaflet::clearGroup("highlighted_point")
        
        # Only add highlights if there are selections
        if (length(cur) > 0) {
          # Get selected rows data
          sel <- tadat$site_AU_table |> dplyr::slice(cur)
          
          if (nrow(sel) > 0) {
            map_selector_proxy %>%
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
      
      # --- Map -> DT: clicking markers toggles table selection -----------------
      observeEvent(input$map_selector_marker_click, {
        req(tadat$site_AU_table)
        
        click_info <- input$map_selector_marker_click
        if (is.null(click_info)) return()
        
        id <- click_info$id
        tbl <- tadat$site_AU_table
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
            table_selector_proxy %>% DT::selectRows(numeric(0))
            # Also manually clear the map highlights since DT might not trigger
            map_selector_proxy %>% leaflet::clearGroup("highlighted_point")
          } else {
            table_selector_proxy %>% DT::selectRows(new_selection)
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
              table_selector_proxy %>% DT::selectRows(numeric(0))
              # Also manually clear the map highlights
              map_selector_proxy %>% leaflet::clearGroup("highlighted_point")
            } else {
              table_selector_proxy %>% DT::selectRows(new_selection)
            }
          }
        }
      }, ignoreInit = TRUE)
      
      # --- Reset selection when dataset changes -------------------------------
      observeEvent(c(tadat$state_tribe, tadat$uses_select), {
        # Clear selections when filters change
        selected_idx(integer(0))
        table_selector_proxy %>% DT::selectRows(numeric(0))
        map_selector_proxy %>% leaflet::clearGroup("highlighted_point")
      }, ignoreInit = TRUE)
  })
  })
}
    
## To be copied in the UI
# mod_analysis_selector_ui("analysis_selector_1")
    
## To be copied in the server
# mod_analysis_selector_server("analysis_selector_1")
