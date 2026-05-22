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

testthat::test_that("mod_criteria_table_server Option D produces non-blank summary", {
  fake_deps <- list(
    TADA_GetCriteriaFile = function(display_name) {
      data.frame(ATTAINS.OrganizationIdentifier = "MTDEQ", stringsAsFactors = FALSE)
    },
    TADA_DefineCriteriaMethodology = function(...) {
      tibble::tibble(
        ATTAINS.OrganizationIdentifier = "MTDEQ",
        CriteriaElementName = "pH",
        MagnitudeUnit = "s.u.",
        EquationBased = "No"
      )
    }
  )
  
  shiny::testServer(mod_criteria_table_server, args = list(id = "id", deps = fake_deps), {
    # Adjust input id to whatever your UI uses to select Option D
    session$setInputs(criteria_option = "D")
    session$flushReact()
    
    # Adjust `summary_tbl` to the name your module returns
    testthat::expect_true(is.function(summary_tbl))
    out <- summary_tbl()
    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_gt(nrow(out), 0)
  })
})

testthat::test_that("mod_criteria_table_server Option E upload and summary works", {
  fake_deps <- list(
    TADA_GetCriteriaFile = function(display_name) {
      data.frame(ATTAINS.OrganizationIdentifier = "MTDEQ", stringsAsFactors = FALSE)
    },
    TADA_DefineCriteriaMethodology = function(...) tibble::tibble()
  )
  
  # should run without error when using fake deps
  testthat::expect_error(
    mod_criteria_table_server("id", deps = fake_deps),
    NA
  )
})

testthat::test_that("mod_criteria_table_server Option D produces non-blank status text", {
  testthat::skip_if_not_installed("EPATADA")
  testthat::skip_if_not_installed("withr")
  testthat::skip_if_not_installed("tibble")
  
  # Minimal tadat for module init
  tadat <- list(
    criteria_file_list = data.frame(display_name = character()),
    ATTAINS_orgs_vec = character()
  )
  
  # Manually patch EPATADA::TADA_DefineCriteriaMethodology
  ns <- asNamespace("EPATADA")
  testthat::expect_true(
    exists("TADA_DefineCriteriaMethodology", envir = ns, inherits = FALSE),
    info = "EPATADA::TADA_DefineCriteriaMethodology not found"
  )
  
  orig_fun <- get("TADA_DefineCriteriaMethodology", envir = ns)
  was_locked <- bindingIsLocked("TADA_DefineCriteriaMethodology", ns)
  if (was_locked) unlockBinding("TADA_DefineCriteriaMethodology", ns)
  
  assign(
    "TADA_DefineCriteriaMethodology",
    function(...) {
      # Emit captured output so template_status is not blank
      cat("Generated Option D template\n")
      tibble::tibble(dummy = 1)
    },
    envir = ns
  )
  
  withr::defer({
    assign("TADA_DefineCriteriaMethodology", orig_fun, envir = ns)
    if (was_locked) lockBinding("TADA_DefineCriteriaMethodology", ns)
  })
  
  # Exercise the module
  shiny::testServer(mod_criteria_table_server, args = list(tadat = tadat), {
    session$setInputs(criteria_method = "D")
    session$setInputs(Generate_Template = 1)  # simulate button click
    session$flushReact()
    
    txt <- output$template_status
    testthat::expect_type(txt, "character")
    testthat::expect_true(nzchar(trimws(txt)))
    testthat::expect_match(txt, "Generated Option D template")
  })
})
