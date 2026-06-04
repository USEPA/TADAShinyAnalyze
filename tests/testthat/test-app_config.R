test_that("app_sys returns package root and handles missing files", {
  # Root path should be a non-empty string
  root <- app_sys()
  expect_type(root, "character")
  expect_length(root, 1L)
  expect_true(nzchar(root))
  expect_true(dir.exists(root))

  # Known existing file (skip if not present to keep test portable)
  cfg_path <- app_sys("golem-config.yml")
  skip_if_not(
    nzchar(cfg_path) && file.exists(cfg_path),
    "golem-config.yml not found in package; skipping existence check"
  )
  expect_true(file.exists(cfg_path))

  # Non-existent file should return an empty string
  missing_path <- app_sys("this-file-should-not-exist-123456789.txt")
  expect_identical(missing_path, "")
})

test_that("get_golem_config returns entire config when no key is provided", {
  # Create a temporary, valid config file
  tf <- tempfile(fileext = ".yml")
  on.exit(unlink(tf), add = TRUE)

  writeLines(
    c(
      "default:",
      "  MB_LIMIT: 123",
      "  FOO: bar",
      "production:",
      "  MB_LIMIT: 456"
    ),
    tf
  )

  cfg <- get_golem_config(file = tf)
  expect_type(cfg, "list")
  expect_equal(cfg$MB_LIMIT, 123)
  expect_equal(cfg$FOO, "bar")
})

test_that("get_golem_config returns specific keys and falls back to default", {
  tf <- tempfile(fileext = ".yml")
  on.exit(unlink(tf), add = TRUE)

  writeLines(
    c(
      "default:",
      "  MB_LIMIT: 123",
      "  FOO: bar",
      "production:",
      "  MB_LIMIT: 456"
    ),
    tf
  )

  # Existing key in default profile
  expect_equal(get_golem_config("MB_LIMIT", file = tf), 123)
  expect_identical(get_golem_config("FOO", file = tf), "bar")

  # Missing key returns provided default
  expect_identical(
    get_golem_config("DOES_NOT_EXIST", default = "fallback", file = tf),
    "fallback"
  )

  # Different profile selection
  expect_equal(
    get_golem_config("MB_LIMIT", config = "production", file = tf),
    456
  )
})

test_that("get_golem_config handles missing or unreadable config files", {
  # Missing file -> returns default (or NULL if no default provided)
  tf_missing <- tempfile(fileext = ".yml") # not created
  expect_identical(
    get_golem_config("MB_LIMIT", default = 999, file = tf_missing),
    999
  )
  expect_null(get_golem_config(file = tf_missing)) # no key, default = NULL

  # Malformed YAML -> config::get errors internally; function should catch and return default
  tf_bad <- tempfile(fileext = ".yml")
  on.exit(unlink(tf_bad), add = TRUE)
  writeLines(c("default:", "  : bad"), tf_bad) # intentionally invalid
  expect_identical(
    get_golem_config("MB_LIMIT", default = 777, file = tf_bad),
    777
  )
})
