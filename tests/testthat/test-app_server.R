test_that("app_server initializes with fetched data (success path)", {
  srv <- app_server
  
  # Stub all module server functions to no-ops
  fake_module <- function(id, ...) { invisible(NULL) }
  mockery::stub(srv, "mod_load_file_server", fake_module)
  mockery::stub(srv, "mod_criteria_table_server", fake_module)
  mockery::stub(srv, "mod_batch_analysis_server", fake_module)
  mockery::stub(srv, "mod_custom_analysis_server", fake_module)
  mockery::stub(srv, "mod_TADA_summary_server", fake_module)
  
  # Capture shinyjs::disable calls
  disabled_calls <- list()
  fake_disable <- function(...) {
    disabled_calls <<- c(disabled_calls, list(list(...)))
    invisible(NULL)
  }
  mockery::stub(srv, "shinyjs::disable", fake_disable)
  
  # Stub external data fetches (success)
  fake_EQ_DomainValues <- function(domain) {
    expect_equal(domain, "org_id")
    data.frame(
      code = c("B", "A"),
      name = c("OrgB", "OrgA"),
      stringsAsFactors = FALSE
    )
  }
  mockery::stub(srv, "rExpertQuery::EQ_DomainValues", fake_EQ_DomainValues)
  
  fake_TADA_ListCriteriaFiles <- function() {
    data.frame(
      display_name = c("B file", "A file"),
      path = c("b.csv", "a.csv"),
      stringsAsFactors = FALSE
    )
  }
  mockery::stub(srv, "EPATADA::TADA_ListCriteriaFiles", fake_TADA_ListCriteriaFiles)
  
  # Run the server in a test session and assert internal state
  testServer(srv, {
    # Reactive values list constructed
    expect_true(inherits(tadat, "reactivevalues"))
    
    # Criteria list should be arranged by display_name
    expect_true(is.data.frame(tadat$criteria_file_list))
    expect_equal(tadat$criteria_file_list$display_name, c("A file", "B file"))
    expect_equal(tadat$criteria_file_list$path, c("a.csv", "b.csv"))
    
    # ATTAINS orgs vector should have names sorted by 'name'
    expect_true(is.atomic(tadat$ATTAINS_orgs_vec))
    expect_equal(unname(tadat$ATTAINS_orgs_vec), c("A", "B"))
    expect_equal(names(tadat$ATTAINS_orgs_vec), c("OrgA", "OrgB"))
    
    # Default reactive fields initialized to NULL
    expect_null(tadat$df_mltoau_input)
    expect_null(tadat$df_autouse_input)
    expect_null(tadat$df_mlid_input)
    
    # Job id and default outfile initialized
    expect_true(is.character(tadat$job_id))
    expect_match(tadat$job_id, "^ts[0-9]{14}$")
    expect_equal(
      tadat$default_outfile,
      paste0("tada_analyze_output_", tadat$job_id)
    )
  })
  
  # Verify tabs disabled via shinyjs
  expect_equal(length(disabled_calls), 3)
  selectors <- vapply(
    disabled_calls,
    function(call_args) {
      if (!is.null(names(call_args)) && "selector" %in% names(call_args)) {
        call_args[["selector"]]
      } else {
        call_args[[1]]
      }
    },
    character(1)
  )
  expect_setequal(
    selectors,
    c(
      '.nav li a[data-value="Criteria"]',
      '.nav li a[data-value="Batch"]',
      '.nav li a[data-value="Custom"]'
    )
  )
})

test_that("app_server handles ATTAINS fetch error gracefully", {
  srv <- app_server
  
  # Stub modules and shinyjs
  fake_module <- function(id, ...) { invisible(NULL) }
  mockery::stub(srv, "mod_load_file_server", fake_module)
  mockery::stub(srv, "mod_criteria_table_server", fake_module)
  mockery::stub(srv, "mod_batch_analysis_server", fake_module)
  mockery::stub(srv, "mod_custom_analysis_server", fake_module)
  mockery::stub(srv, "mod_TADA_summary_server", fake_module)
  mockery::stub(srv, "shinyjs::disable", function(...) invisible(NULL))
  
  # Stub ATTAINS to error
  mockery::stub(srv, "rExpertQuery::EQ_DomainValues", function(domain) {
    stop("simulated ATTAINS error")
  })
  
  # Criteria fetch succeeds
  mockery::stub(srv, "EPATADA::TADA_ListCriteriaFiles", function() {
    data.frame(display_name = "Only file", path = "file.csv", stringsAsFactors = FALSE)
  })
  
  expect_warning(
    testServer(srv, {
      # Verify ATTAINS org vector is NULL due to error branch
      expect_null(tadat$ATTAINS_orgs_vec)
      # Criteria list still set (and arranged, though single row)
      expect_true(is.data.frame(tadat$criteria_file_list))
      expect_equal(nrow(tadat$criteria_file_list), 1)
      expect_equal(tadat$criteria_file_list$display_name, "Only file")
    }),
    regexp = "Failed to fetch ATTAINS org IDs:"
  )
})

test_that("app_server warns on criteria list error and continues (guarded arrange)", {
  srv <- app_server
  
  # Stub modules and shinyjs to no-ops
  fake_module <- function(id, ...) { invisible(NULL) }
  mockery::stub(srv, "mod_load_file_server", fake_module)
  mockery::stub(srv, "mod_criteria_table_server", fake_module)
  mockery::stub(srv, "mod_batch_analysis_server", fake_module)
  mockery::stub(srv, "mod_custom_analysis_server", fake_module)
  mockery::stub(srv, "mod_TADA_summary_server", fake_module)
  mockery::stub(srv, "shinyjs::disable", function(...) invisible(NULL))
  
  # ATTAINS succeeds
  mockery::stub(srv, "rExpertQuery::EQ_DomainValues", function(domain) {
    data.frame(code = "X", name = "OrgX", stringsAsFactors = FALSE)
  })
  
  # Criteria list errors
  mockery::stub(srv, "EPATADA::TADA_ListCriteriaFiles", function() {
    stop("simulated criteria fetch error")
  })
  
  expect_warning(
    testServer(srv, {
      # Guard should prevent arrange(NULL) error and leave criteria as NULL
      expect_null(tadat$criteria_file_list)
      # ATTAINS vector should be set
      expect_true(is.atomic(tadat$ATTAINS_orgs_vec))
      expect_equal(unname(tadat$ATTAINS_orgs_vec), "X")
      expect_equal(names(tadat$ATTAINS_orgs_vec), "OrgX")
    }),
    regexp = "Failed to fetch criteria file list from GitHub:"
  )
})

test_that("criteria_file_list without display_name is passed through un-arranged", {
  srv <- app_server
  
  # Stub modules and shinyjs to no-ops
  fake_module <- function(id, ...) { invisible(NULL) }
  mockery::stub(srv, "mod_load_file_server", fake_module)
  mockery::stub(srv, "mod_criteria_table_server", fake_module)
  mockery::stub(srv, "mod_batch_analysis_server", fake_module)
  mockery::stub(srv, "mod_custom_analysis_server", fake_module)
  mockery::stub(srv, "mod_TADA_summary_server", fake_module)
  mockery::stub(srv, "shinyjs::disable", function(...) invisible(NULL))
  
  # ATTAINS succeeds
  mockery::stub(srv, "rExpertQuery::EQ_DomainValues", function(domain) {
    data.frame(code = "A", name = "OrgA", stringsAsFactors = FALSE)
  })
  
  # Criteria returns a frame without display_name; order should be preserved
  mockery::stub(srv, "EPATADA::TADA_ListCriteriaFiles", function() {
    data.frame(title = c("B file", "A file"), path = c("b.csv", "a.csv"), stringsAsFactors = FALSE)
  })
  
  testServer(srv, {
    # Should not arrange (guard prevents it), and leave data as returned
    expect_true(is.data.frame(tadat$criteria_file_list))
    expect_equal(names(tadat$criteria_file_list), c("title", "path"))
    expect_equal(tadat$criteria_file_list$title, c("B file", "A file"))
    expect_equal(tadat$criteria_file_list$path, c("b.csv", "a.csv"))
    
    # ATTAINS vector still set
    expect_true(is.atomic(tadat$ATTAINS_orgs_vec))
    expect_equal(unname(tadat$ATTAINS_orgs_vec), "A")
    expect_equal(names(tadat$ATTAINS_orgs_vec), "OrgA")
  })
})
