#' boxplot UI Function
#'
#' @description A shiny Module for creating boxplots with dynamic filtering.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_boxplot_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Parameter selectize input (single selection)
    shinyWidgets::selectizeInput(
      inputId = ns("parameter_select"),
      label = "Parameter",
      choices = NULL,
      selected = NULL,
      multiple = FALSE,
      options = list(
        placeholder = "Select a parameter",
        create = FALSE
      )
    ),
    
    # Uses selectize input (multiple selection)
    shinyWidgets::selectizeInput(
      inputId = ns("uses_select"),
      label = "Uses",
      choices = NULL,
      selected = NULL,
      multiple = TRUE,
      options = list(
        placeholder = "Select uses",
        create = FALSE
      )
    ),
    
    # Plot output
    plotOutput(ns("boxplot_view"))
  )
}

#' boxplot Server Functions
#'
#' @noRd 
mod_boxplot_server <- function(id, tadat){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Update selectize inputs when data changes
    observe({
      req(tadat$exceed_dat)
      
      # Get unique values for Parameter dropdown
      param_choices <- unique(tadat$exceed_dat$TADA.CharacteristicName)
      param_choices <- param_choices[!is.na(param_choices)]
      
      # Get unique values for Uses dropdown
      uses_choices <- unique(tadat$exceed_dat$Attains.UseName)
      uses_choices <- uses_choices[!is.na(uses_choices)]
      
      # Update Parameter selectize
      updateSelectizeInput(
        session = session,
        inputId = "parameter_select",
        choices = param_choices,
        selected = if(length(param_choices) > 0) param_choices[1] else NULL,
        server = TRUE
      )
      
      # Update Uses selectize
      updateSelectizeInput(
        session = session,
        inputId = "uses_select",
        choices = uses_choices,
        selected = NULL,
        server = TRUE
      )
    })
    
    # Reactive to filter data based on selections
    filtered_data <- reactive({
      req(tadat$exceed_dat)
      req(input$parameter_select)
      req(input$uses_select)
      
      # Filter by selected parameter
      data_filtered <- tadat$exceed_dat %>%
        dplyr::filter(TADA.CharacteristicName == input$parameter_select)
      
      # Filter by selected uses
      data_filtered <- data_filtered %>%
        dplyr::filter(Attains.UseName %in% input$uses_select)
      
      return(data_filtered)
    })
    
    # Create boxplot
    output$boxplot_view <- renderPlot({
      req(filtered_data())
      
      # Check if there's data to plot
      if(nrow(filtered_data()) == 0) {
        plot.new()
        text(0.5, 0.5, "No data available for selected filters", 
             cex = 1.2, col = "gray50")
        return(NULL)
      }
      
      # Create ggplot boxplot
      p <- ggplot2::ggplot(filtered_data(), 
                           ggplot2::aes(x = Attains.UseName, 
                                        y = measurement)) +
        ggplot2::geom_boxplot(fill = "lightblue", 
                              color = "darkblue",
                              alpha = 0.7) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, 
                                              hjust = 1, 
                                              vjust = 1),
          plot.title = ggplot2::element_text(size = 16, 
                                             face = "bold",
                                             hjust = 0.5),
          axis.title = ggplot2::element_text(size = 12)
        ) +
        ggplot2::labs(
          title = paste("Boxplot of", input$parameter_select),
          x = "Uses",
          y = "Measurement"
        )
      
      # Add sample size annotations if desired
      if(nrow(filtered_data()) > 0) {
        sample_sizes <- filtered_data() %>%
          dplyr::group_by(Attains.UseName) %>%
          dplyr::summarise(n = dplyr::n(), .groups = "drop")
        
        p <- p + 
          ggplot2::geom_text(
            data = sample_sizes,
            ggplot2::aes(x = Attains.UseName, 
                         y = Inf, 
                         label = paste("n =", n)),
            vjust = 1.5,
            size = 3,
            color = "gray40"
          )
      }
      
      return(p)
    })
    
  })
}

## To be copied in the UI
# mod_boxplot_ui("boxplot_1")

## To be copied in the server
# mod_boxplot_server("boxplot_1", tadat = tadat)