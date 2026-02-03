library(shiny)
library(plotly)

# --- Server Logic ---
server <- function(input, output, session) {
  
  # 1. Create a sample list of plotly graphs
  plot_list <- reactive({
    p1 <- EPATADA::TADA_Scatterplot(EPATADA::Data_MT_MissoulaCounty)[[1]]
    p2 <- EPATADA::TADA_Scatterplot(EPATADA::Data_MT_MissoulaCounty)[[2]]
    list(p1, p2)
  })
  
  # 2. Loop through the list of plots and create the output render functions
  observe({
    lapply(1:length(plot_list()), function(i) {
      output[[paste0("plot_", i)]] <- renderPlotly({
        plot_list()[[i]]
      })
    })
  })
  
  # 3. Create the UI elements dynamically based on the number of plots
  output$plot_ui <- renderUI({
    plot_output_list <- lapply(1:length(plot_list()), function(i) {
      plotlyOutput(paste0("plot_", i))
    })
    
    # Convert the list of outputs into a tagList for display
    do.call(tagList, plot_output_list)
  })
}

# --- User Interface ---
ui <- fluidPage(
  titlePanel("Dynamic Plotly Plots from a List"),
  
  sidebarLayout(
    sidebarPanel(
      p("This panel shows a dynamic number of Plotly plots.")
    ),
    mainPanel(
      # The placeholder for all dynamically generated plots
      uiOutput("plot_ui")
    )
  )
)

# --- Run the application ---
shinyApp(ui = ui, server = server)

