testServer(
  mod_criteria_table_server,
  # Add here your module params
  args = list()
  , {
    ns <- session$ns
    expect_true(
      inherits(ns, "function")
    )
    expect_true(
      grepl(id, ns(""))
    )
    expect_true(
      grepl("test", ns("test"))
    )
    # Here are some examples of tests you can
    # run on your module
    # - Testing the setting of inputs
    # session$setInputs(x = 1)
    # expect_true(input$x == 1)
    # - If ever your input updates a reactiveValues
    # - Note that this reactiveValues must be passed
    # - to the testServer function via args = list()
    # expect_true(r$x == 1)
    # - Testing output
    # expect_true(inherits(output$tbl$html, "html"))
})
 
test_that("module ui works", {
  ui <- mod_criteria_table_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_criteria_table_ui)
  for (i in c("id")){
    expect_true(i %in% names(fmls))
  }
})
 
# tests/testthat/test_mod_criteria_table.R

testthat::test_that("mod_criteria_table_ui renders key controls", {
  ui <- mod_criteria_table_ui("criteria_table_1")
  html <- paste0(capture.output(htmltools::renderTags(ui)$html), collapse = "\n")
  
  testthat::expect_true(grepl("2\\. Criteria Table", html))
  testthat::expect_true(grepl("Select the method to generate a draft criteria and methods template", html))
  testthat::expect_true(grepl("Generate and Download Template", html))
  testthat::expect_true(grepl("Select the State/Tribe", html))
  testthat::expect_true(grepl("Choose file to load", html))
})

testthat::test_that("mod_criteria_table_server Option D upload and summary works", {
  fake_deps <- EPATADA::TADA_DefineCriteriaMethodology()
  
  # should run without error when using fake deps
  testthat::expect_error(
    mod_criteria_table_server("id", deps = fake_deps),
    NA
  )
})

testthat::test_that("mod_criteria_table_server Option E upload and summary works", {
  fake_deps <- list(
    TADA_GetCriteriaFile = function(display_name) {
      data.frame(ATTAINS.OrganizationIdentifier = "MTDEQ", stringsAsFactors = FALSE)
    },
    TADA_DefineCriteriaMethodology = function(...) dplyr::tibble()
  )
  
  # should run without error when using fake deps
  testthat::expect_error(
    mod_criteria_table_server("id", deps = fake_deps),
    NA
  )
})
