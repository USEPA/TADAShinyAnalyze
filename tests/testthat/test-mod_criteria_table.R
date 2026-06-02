testServer(
  mod_criteria_table_server,
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
 
testthat::test_that("mod_criteria_table_ui renders key controls", {
  ui <- mod_criteria_table_ui("criteria_table_1")
  html <- paste0(capture.output(htmltools::renderTags(ui)$html), collapse = "\n")
  
  testthat::expect_true(grepl("2\\. Criteria Table", html))
  testthat::expect_true(grepl("Select the method to generate a draft criteria and methods template", html))
  testthat::expect_true(grepl("Generate and Download Template", html))
  testthat::expect_true(grepl("Select the State/Tribe", html))
  testthat::expect_true(grepl("Choose file to load", html))
})

test_that("mod_criteria_table_server Option D (blank template) runs without error", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("EPATADA")
  
  # Minimal stub for the module's expected `tadat` structure
  fake_tadat <- list(
    criteria_file_list = data.frame(display_name = character(), stringsAsFactors = FALSE),
    ATTAINS_orgs_vec   = character(),
    df_mlid_input      = tibble::tibble(TADA.ComparableDataIdentifier = character()),
    df_mltoau_input    = tibble::tibble(),
    df_autouse_input   = tibble::tibble()
  )
  
  expect_error(
    shiny::testServer(
      app = mod_criteria_table_server,
      args = list(id = "id", tadat = fake_tadat),
      {
        # Drive the module to Option D and trigger generation
        session$setInputs(criteria_method = "D")
        session$setInputs(Generate_Template = 1)
        session$flushReact()
        
        # Ensure the generation completed without throwing a wrapped try-error
        cap <- criteria_cap()
        expect_true(is.list(cap))
        expect_false(inherits(cap$result, "try-error"))
        
        # Status output should not be an "Error:" message
        expect_type(output$template_status, "character")
        expect_false(grepl("^Error:", output$template_status))
      }
    ),
    NA
  )
})

test_that("mod_criteria_table_server Option E upload path runs without error", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("readxl")
  testthat::skip_if_not_installed("writexl")
  testthat::skip_if_not_installed("EPATADA") # server references it to build col names
  
  # Minimal Excel with required sheet and org id
  tmp_xlsx <- tempfile(fileext = ".xlsx")
  minimal_df <- data.frame(
    ATTAINS.OrganizationIdentifier = "MTDEQ",
    c2 = NA_character_, c3 = NA_character_, c4 = NA_character_,
    c5 = NA_character_, c6 = NA_character_,  # >=6 cols
    stringsAsFactors = FALSE
  )
  writexl::write_xlsx(list(DefineCriteriaMethodology = minimal_df), path = tmp_xlsx)
  
  # Minimal `tadat` stub
  fake_tadat <- list(
    criteria_file_list = data.frame(display_name = character(), stringsAsFactors = FALSE),
    ATTAINS_orgs_vec   = character(),
    df_mlid_input      = tibble::tibble(TADA.ComparableDataIdentifier = character()),
    df_mltoau_input    = tibble::tibble(),
    df_autouse_input   = tibble::tibble()
  )
  
  # Simulate a Shiny fileInput value
  mk_file_input <- function(path) {
    sz <- suppressWarnings(as.numeric(file.info(path)$size)); if (is.na(sz)) sz <- 0
    data.frame(
      name = basename(path), size = sz,
      type = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
      datapath = path, stringsAsFactors = FALSE
    )
  }
  
  # Mock shinyjs server calls to no-ops
  testthat::with_mocked_bindings(
    toggle      = function(...) NULL,
    toggleState = function(...) NULL,
    enable      = function(...) NULL,
    disable     = function(...) NULL,
    .package = "shinyjs",
    {
      expect_error(
        shiny::testServer(
          app = mod_criteria_table_server,
          args = list(id = "id", tadat = fake_tadat),
          {
            session$setInputs(criteria_method = "E")
            session$flushReact()
            
            session$setInputs(upload_template = mk_file_input(tmp_xlsx))
            session$flushReact()
            
            session$setInputs(state_tribe_select_OP_E = "MTDEQ")
            session$flushReact()
            
            # Do not upload review_template to avoid validators
            
            # Just ensure server ran without error; optionally inspect internal reactive
            expect_true(!is.null(uploaded_temp_table()))
            expect_identical(input$state_tribe_select_OP_E, "MTDEQ")
            # Avoid reading outputs that might re-trigger JS-dependent paths
          }
        ),
        NA
      )
    }
  )
})

test_that("mod_criteria_table_server Option E upload and summary works (mocked validators)", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("readxl")
  testthat::skip_if_not_installed("writexl")
  testthat::skip_if_not_installed("EPATADA")
  testthat::skip_if_not_installed("DT")
  testthat::skip_if_not_installed("htmlwidgets")
  
  # Build minimal Excel with required sheet and org id
  tmp_xlsx <- tempfile(fileext = ".xlsx")
  minimal_df <- data.frame(
    ATTAINS.OrganizationIdentifier = "MTDEQ",
    c2 = NA_character_, c3 = NA_character_, c4 = NA_character_,
    c5 = NA_character_, c6 = NA_character_,
    stringsAsFactors = FALSE
  )
  writexl::write_xlsx(list(DefineCriteriaMethodology = minimal_df), path = tmp_xlsx)
  
  # Minimal tadat stub
  fake_tadat <- list(
    criteria_file_list = data.frame(display_name = character(), stringsAsFactors = FALSE),
    ATTAINS_orgs_vec   = character(),
    df_mlid_input      = tibble::tibble(TADA.ComparableDataIdentifier = character()),
    df_mltoau_input    = tibble::tibble(),
    df_autouse_input   = tibble::tibble()
  )
  
  # Simulate Shiny fileInput value
  mk_file_input <- function(path) {
    sz <- suppressWarnings(as.numeric(file.info(path)$size)); if (is.na(sz)) sz <- 0
    data.frame(
      name = basename(path), size = sz,
      type = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
      datapath = path, stringsAsFactors = FALSE
    )
  }
  
  # Mock validators and shinyjs
  testthat::with_mocked_bindings(
    runAllValidations = function(data) list(overall_status = "Validation status: OK"),
    .package = "TADACommunityHub",
    {
      testthat::with_mocked_bindings(
        toggle      = function(...) NULL,
        toggleState = function(...) NULL,
        enable      = function(...) NULL,
        disable     = function(...) NULL,
        .package = "shinyjs",
        {
          expect_error(
            shiny::testServer(
              app = mod_criteria_table_server,
              args = list(id = "id", tadat = fake_tadat),
              {
                session$setInputs(criteria_method = "E")
                session$flushReact()
                
                session$setInputs(upload_template = mk_file_input(tmp_xlsx))
                session$flushReact()
                
                session$setInputs(state_tribe_select_OP_E = "MTDEQ")
                session$flushReact()
                
                session$setInputs(review_template = mk_file_input(tmp_xlsx))
                session$flushReact()
                
                # Assertions
                expect_type(output$template_summary, "character")
                expect_true(grepl("Validation status: OK", output$template_summary, fixed = TRUE))
                
                # DT output: assert it rendered (non-NULL) rather than requiring a base list
                expect_false(is.null(output$final_template))
              }
            ),
            NA
          )
        }
      )
    }
  )
})
