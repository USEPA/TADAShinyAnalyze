# -----------------------------
# Helper data generators
# -----------------------------

make_base_sites <- function() {
  data.frame(
    TADA.MonitoringLocationIdentifier = c("S1", "S2"),
    TADA.MonitoringLocationName = c("Site 1", "Site 2"),
    TADA.MonitoringLocationTypeName = c("River/Stream", "River/Stream"),
    TADA.LatitudeMeasure = c(45, 46),
    TADA.LongitudeMeasure = c(-122, -123),
    ATTAINS.AssessmentUnitIdentifier = c("AU1", "AU2"),
    stringsAsFactors = FALSE
  )
}

make_base_obs <- function() {
  base_tbl <- data.frame(
    TADA.MonitoringLocationIdentifier = rep("S1", 8),
    TADA.MonitoringLocationName = rep("Site 1", 8),
    TADA.MonitoringLocationTypeName = rep("River/Stream", 8),
    TADA.LatitudeMeasure = rep(45, 8),
    TADA.LongitudeMeasure = rep(-122, 8),
    ATTAINS.AssessmentUnitIdentifier = rep("AU1", 8),
    ATTAINS.ParameterName = rep("ParamX", 8),
    ATTAINS.UseName = rep("Aquatic Life", 8),
    DateTime = as.POSIXct(
      c(
        "2020-01-01 08:00:00",
        "2020-01-01 10:00:00",
        "2020-01-02 08:00:00",
        "2020-01-04 08:00:00",
        # temperature timestamps
        "2020-01-01 09:00:00",
        "2020-01-02 07:59:00",
        "2020-01-03 08:00:00",
        "2020-01-04 20:00:00"
      ),
      tz = "UTC"
    ),
    ActivityStartDate = as.Date(c(
      "2020-01-01",
      "2020-01-01",
      "2020-01-02",
      "2020-01-04",
      "2020-01-01",
      "2020-01-02",
      "2020-01-03",
      "2020-01-04"
    )),
    `ActivityStartTime.Time` = rep("08:00:00", 8),
    TADA.CharacteristicName = c(
      "PH",
      "PH",
      "PH",
      "PH",
      "TEMPERATURE, WATER",
      "TEMPERATURE, WATER",
      "TEMPERATURE, WATER",
      "TEMPERATURE, WATER"
    ),
    TADA.ResultMeasureValue = c(7.2, 7.4, NA, 7.1, 12.3, 10.1, 9.5, 8.8),
    TADA.ResultSampleFractionText = rep(NA_character_, 8),
    TADA.MethodSpeciationName = rep(NA_character_, 8),
    TADA.ResultMeasure.MeasureUnitCode = c(rep(NA_character_, 4), rep("C", 4)),
    stringsAsFactors = FALSE
  )

  hard_tbl <- data.frame(
    TADA.MonitoringLocationIdentifier = "S1",
    TADA.MonitoringLocationName = "Site 1",
    TADA.MonitoringLocationTypeName = "River/Stream",
    TADA.LatitudeMeasure = 45,
    TADA.LongitudeMeasure = -122,
    ATTAINS.AssessmentUnitIdentifier = "AU1",
    ATTAINS.ParameterName = "ParamHard",
    ATTAINS.UseName = "Aquatic Life",
    DateTime = as.POSIXct("2020-01-02 08:00:00", tz = "UTC"),
    ActivityStartDate = as.Date("2020-01-02"),
    `ActivityStartTime.Time` = "08:00:00",
    TADA.CharacteristicName = "HARDNESS, CA, MG",
    TADA.ResultMeasureValue = 450,
    TADA.ResultSampleFractionText = NA_character_,
    TADA.MethodSpeciationName = NA_character_,
    TADA.ResultMeasure.MeasureUnitCode = NA_character_,
    stringsAsFactors = FALSE
  )

  out <- rbind(base_tbl, hard_tbl)
  out
}

make_excursion_input <- function() {
  data.frame(
    TADA.ResultMeasureValue = c(5, 15, 5, 15, 10),
    MagnitudeValueLower = c(NA, NA, 10, NA, 5),
    MagnitudeValueUpper = c(10, 10, NA, 20, 10),
    stringsAsFactors = FALSE
  )
}

make_exceedance_data <- function() {
  data.frame(
    TADA.MonitoringLocationIdentifier = c("S1", "S2"),
    TADA.MonitoringLocationName = c("Site 1", "Site 2"),
    TADA.LongitudeMeasure = c(-122, -123),
    TADA.LatitudeMeasure = c(45, 46),
    ATTAINS.AssessmentUnitIdentifier = c("AU1", "AU2"),
    ATTAINS.UseName = c("Aquatic Life", "Aquatic Life"),
    TADA.CharacteristicName = c("ParamA", "ParamB"),
    # New columns to satisfy mapping helpers
    ParameterForFilter = c("ParamA", "ParamB"),
    UseForFilter = c("Aquatic Life", "Aquatic Life"),
    Exceedance = c("Exceed", "Not Exceed"),
    stringsAsFactors = FALSE
  )
}

make_coords_for_map <- function() {
  data.frame(
    ATTAINS.AssessmentUnitIdentifier = c("AU1", "AU2"),
    TADA.MonitoringLocationIdentifier = c("S1", "S2"),
    TADA.MonitoringLocationName = c("Site 1", "Site 2"),
    TADA.LongitudeMeasure = c(-122, -123),
    TADA.LatitudeMeasure = c(45, 46),
    stringsAsFactors = FALSE
  )
}

# -----------------------------
# Tests
# -----------------------------

test_that("na_* helpers and step/window helpers behave as expected", {
  expect_true(is.na(na_mean(numeric(0))))
  expect_true(is.na(na_mean(c(NA, NA))))
  expect_equal(na_mean(c(1, NA, 3)), 2)

  expect_true(is.na(na_min(c(NA, NA))))
  expect_equal(na_min(c(NA, 2, 5)), 2)

  expect_true(is.na(na_max(c(NA, NA))))
  expect_equal(na_max(c(NA, 2, 5)), 5)

  expect_true(is.na(na_gmean(c(NA, -1, 0))))
  expect_equal(
    round(na_gmean(c(1, 4, 16)), 6),
    round(exp(mean(log(c(1, 4, 16)))), 6)
  )

  expect_equal(step_label(NA), "1 day")
  expect_equal(step_label("n-hour"), "1 hour")
  expect_equal(step_label("n-day"), "1 day")

  wb_h1 <- window_before_period("n-hour", 1)
  wb_d3 <- window_before_period("n-day", 3)

  expect_true(lubridate::is.period(wb_h1))
  expect_true(lubridate::is.period(wb_d3))

  expect_equal(
    lubridate::time_length(lubridate::as.duration(wb_h1), "seconds"),
    0
  )
  expect_equal(lubridate::time_length(lubridate::as.duration(wb_d3), "days"), 2)
})

test_that("hardness_eq implements coefficient fallback and formula", {
  val1 <- hardness_eq(
    hardness = 100,
    E_A = 0.1,
    E_B = 1,
    CF_A = 2,
    CF_B = 0.5,
    CF_C = 1.5
  )
  val2 <- hardness_eq(
    hardness = 100,
    E_A = 0.1,
    E_B = 1,
    CF_A = NA,
    CF_B = 0.5,
    CF_C = 1.5
  )
  expect_true(is.finite(val1))
  expect_equal(val2, exp(0.1 * log(100) + 1) * 1.5)
})

test_that("modSum handles NULL, empty, all-NA, and sums", {
  expect_true(is.na(modSum(NULL)))
  expect_true(is.na(modSum(numeric(0))))
  expect_true(is.na(modSum(c(NA, NA))))
  expect_equal(modSum(c(1, 2, NA)), 3)
})

test_that("excursion_fun flags exceedances correctly", {
  df <- make_excursion_input()
  out <- excursion_fun(df)
  expect_true("Excursion" %in% names(out))
  expect_equal(out$Excursion, c(FALSE, TRUE, TRUE, FALSE, FALSE))
})

test_that("excursion_summary aggregates correctly for MLid and AU", {
  x <- data.frame(
    TADA.MonitoringLocationIdentifier = c("S1", "S1", "S2"),
    TADA.MonitoringLocationName = c("Site 1", "Site 1", "Site 2"),
    TADA.LongitudeMeasure = c(-122, -122, -123),
    TADA.LatitudeMeasure = c(45, 45, 46),
    ATTAINS.AssessmentUnitIdentifier = c("AU1", "AU1", "AU2"),
    ATTAINS.ParameterName = c("ParamA", "ParamA", "ParamB"),
    TADA.CharacteristicName = c("ParamA", "ParamA", "ParamB"),
    TADA.ResultSampleFractionText = c(
      NA_character_,
      NA_character_,
      NA_character_
    ),
    TADA.MethodSpeciationName = c(NA_character_, NA_character_, NA_character_),
    TADA.ResultMeasure.MeasureUnitCode = rep("mg/L", 3),
    ATTAINS.UseName = c("Aquatic Life", "Aquatic Life", "Aquatic Life"),
    AcuteChronic = rep(NA_character_, 3),
    UniqueSpatialCriteria = rep(NA_character_, 3),
    Season = rep(NA_character_, 3),
    ATTAINS.OrganizationIdentifier = rep("Org1", 3),
    EquationBased = rep(NA_character_, 3),
    DurationUnit = rep("n-day", 3),
    DurationMethod = rep("Arithmetic Mean", 3),
    DurationValue = rep(1L, 3),
    FreqValue = rep(0, 3),
    FreqMethod = rep("NumberNotMeeting", 3),
    EquationType = rep(NA_character_, 3),
    ActivityStartDate = as.Date(c("2020-01-01", "2020-01-02", "2020-01-01")),
    TADA.ResultMeasureValue = c(5, 15, 7),
    MagnitudeValueLower = c(NA, NA, NA),
    MagnitudeValueUpper = c(10, 10, 10),
    stringsAsFactors = FALSE
  )
  x <- excursion_fun(x)

  ml <- excursion_summary(x, type = "MLid")
  expect_type(ml, "list")
  expect_true(all(c("data", "coords") %in% names(ml)))
  expect_true(all(
    c("Sample_Count", "Number_of_Excursions", "Excursion_Percentage") %in%
      names(ml$data)
  ))

  au <- excursion_summary(x, type = "AU")
  expect_type(au, "list")
  expect_true(all(c("data", "coords") %in% names(au)))
})

test_that("time_aggregate collapses timestamp duplicates and computes daily means", {
  x <- data.frame(
    TADA.MonitoringLocationIdentifier = rep("S1", 5),
    TADA.MonitoringLocationName = rep("Site 1", 5),
    TADA.LatitudeMeasure = rep(45, 5),
    TADA.LongitudeMeasure = rep(-122, 5),
    ATTAINS.AssessmentUnitIdentifier = rep("AU1", 5),
    ATTAINS.ParameterName = rep("ParamX", 5),
    TADA.CharacteristicName = rep("ParamX", 5),
    TADA.ResultSampleFractionText = rep(NA_character_, 5),
    TADA.MethodSpeciationName = rep(NA_character_, 5),
    TADA.ResultMeasure.MeasureUnitCode = rep("mg/L", 5),
    ATTAINS.UseName = rep("Aquatic Life", 5),
    AcuteChronic = rep(NA_character_, 5),
    UniqueSpatialCriteria = rep(NA_character_, 5),
    Season = rep(NA_character_, 5),
    ATTAINS.OrganizationIdentifier = rep("Org", 5),
    EquationBased = rep(NA_character_, 5),
    DurationUnit = c("n-hour", "n-hour", "n-day", "n-day", "n-season"),
    DurationMethod = rep("Arithmetic Mean", 5),
    DurationValue = rep(1, 5),
    FreqValue = rep(0, 5),
    FreqMethod = rep("NumberNotMeeting", 5),
    EquationType = rep(NA_character_, 5),
    ActivityStartDate = as.Date(c(
      "2020-01-01",
      "2020-01-01",
      "2020-01-02",
      "2020-01-02",
      "2020-03-01"
    )),
    DateTime = as.POSIXct(
      c(
        "2020-01-01 08:00:00",
        "2020-01-01 09:00:00",
        "2020-01-02 08:00:00",
        "2020-01-02 16:00:00",
        "2020-03-01 08:00:00"
      ),
      tz = "UTC"
    ),
    TADA.ResultMeasureValue = c(1, 3, 10, 14, 100),
    MagnitudeValueLower = c(NA, NA, 5, 5, 5),
    MagnitudeValueUpper = c(10, 10, 20, 20, 200),
    pH = c(7, 7, 7.5, 7.2, 7.0),
    Temperature = c(10, 12, 11, 10, 9),
    Hardness = c(100, 100, 120, 110, 90),
    stringsAsFactors = FALSE
  )

  agg <- time_aggregate(x, type = "MLid")
  expect_true(all(c("Value", "N_in_Step", "DateTime") %in% names(agg)))

  d2 <- agg[as.Date(agg$DateTime) == as.Date("2020-01-02"), , drop = FALSE]
  expect_equal(unique(d2$Value), 12)
})

test_that("magnitude_update computes updated thresholds for hardness, pH, and combined equations", {
  x <- data.frame(
    EquationType = c("Hardness", "pH", "pH and Hardness", "pH and Temperature"),
    Hardness_win = c(100, NA, 150, NA),
    pH_win = c(NA, 7.5, 6.8, 8.0),
    Temperature_win = c(NA, NA, NA, 20),
    TADA.ResultSampleFractionText = c(
      "Dissolved",
      "Dissolved",
      "Total",
      "Total"
    ),
    MagnitudeValueUpper = c(NA_real_, NA_real_, NA_real_, NA_real_),
    stringsAsFactors = FALSE
  )

  hardness_equation <- data.frame(
    EquationType = "Hardness",
    hardness_param_1 = 2, # CF_A
    hardness_param_2 = 0.5, # CF_B
    hardness_param_3 = 1.5, # CF_C
    hardness_param_4 = 0.1, # E_A
    hardness_param_5 = 1, # E_B
    stringsAsFactors = FALSE
  )
  pH_equation <- data.frame(
    EquationType = "pH",
    Equation = "pH * 2",
    stringsAsFactors = FALSE
  )
  pH_Hardness_equation <- data.frame(
    EquationType = "pH and Hardness",
    hardness_param_1 = 2,
    hardness_param_2 = 0.5,
    hardness_param_3 = 1.5,
    hardness_param_4 = 0.1,
    hardness_param_5 = 1,
    hardness_param_6 = 30,
    stringsAsFactors = FALSE
  )
  pH_Temperature_equation <- data.frame(
    EquationType = "pH and Temperature",
    Equation = "pH + Temperature",
    stringsAsFactors = FALSE
  )

  out1 <- magnitude_update(
    x = x,
    match_type = "Option 1",
    hardness_equation = hardness_equation,
    pH_equation = pH_equation,
    pH_Hardness_equation = pH_Hardness_equation,
    pH_Temperature_equation = pH_Temperature_equation
  )

  expect_true(all(c("MagnitudeValueUpper", "EquationType") %in% names(out1)))

  h_row <- out1[out1$EquationType == "Hardness", , drop = FALSE]
  pH_row <- out1[out1$EquationType == "pH", , drop = FALSE]
  phh_row <- out1[out1$EquationType == "pH and Hardness", , drop = FALSE]
  pht_row <- out1[out1$EquationType == "pH and Temperature", , drop = FALSE]

  expect_true(is.finite(pmin(h_row$MagnitudeValueUpper, Inf)))
  expect_equal(pH_row$MagnitudeValueUpper, 7.5 * 2, tolerance = 1e-8)
  expect_true(phh_row$MagnitudeValueUpper <= 30 + 1e-8)
  expect_equal(pht_row$MagnitudeValueUpper, 8.0 + 20, tolerance = 1e-8)

  out2 <- magnitude_update(
    x = x,
    match_type = "Option 2",
    hardness_equation = hardness_equation,
    pH_equation = pH_equation,
    pH_Hardness_equation = pH_Hardness_equation,
    pH_Temperature_equation = pH_Temperature_equation
  )
  expect_true(nrow(out2) == nrow(out1))
})

test_that("GetURL and add_USGS_base return expected types", {
  skip_on_cran()
  skip_if_not_installed("leaflet")

  url <- GetURL("USGSTopo")
  expect_true(grepl("USGSTopo", url))

  m <- add_USGS_base(leaflet::leaflet())
  expect_s3_class(m, "leaflet")
})

test_that("create_overall_map returns a leaflet map and builds popup", {
  skip_on_cran()
  skip_if_not_installed("leaflet")

  data <- make_exceedance_data()
  coords <- make_coords_for_map()

  map1 <- create_overall_map(data, type = "MLid", use_type = "Option 1")
  expect_s3_class(map1, "leaflet")

  map2 <- create_overall_map(
    data,
    coords_data = coords,
    type = "AU",
    use_type = "Option 1"
  )
  expect_s3_class(map2, "leaflet")

  map3 <- create_overall_map(
    data,
    coords_data = coords,
    type = "CG",
    use_type = "Option 1"
  )
  expect_s3_class(map3, "leaflet")
})

test_that("create_use_map filters by selected_use and returns leaflet", {
  skip_on_cran()
  skip_if_not_installed("leaflet")

  data <- make_exceedance_data()
  coords <- make_coords_for_map()

  map_ml <- create_use_map(
    data,
    selected_use = "Aquatic Life",
    type = "MLid",
    use_type = "Option 1"
  )
  expect_s3_class(map_ml, "leaflet")

  map_au <- create_use_map(
    data,
    coords_data = coords,
    selected_use = "Aquatic Life",
    type = "AU",
    use_type = "Option 1"
  )
  expect_s3_class(map_au, "leaflet")

  map_cg <- create_use_map(
    data,
    coords_data = coords,
    selected_use = "Aquatic Life",
    type = "CG",
    use_type = "Option 1"
  )
  expect_s3_class(map_cg, "leaflet")
})

test_that("create_parameter_map filters by parameter and returns leaflet", {
  skip_on_cran()
  skip_if_not_installed("leaflet")

  data <- make_exceedance_data()
  coords <- make_coords_for_map()

  map_ml <- create_parameter_map(
    data,
    selected_param = "ParamA",
    selected_use = "Aquatic Life",
    type = "MLid",
    use_type = "Option 1"
  )
  expect_s3_class(map_ml, "leaflet")

  map_au <- create_parameter_map(
    data,
    coords_data = coords,
    selected_param = "ParamA",
    selected_use = "Aquatic Life",
    type = "AU",
    use_type = "Option 1"
  )
  expect_s3_class(map_au, "leaflet")

  map_cg <- create_parameter_map(
    data,
    coords_data = coords,
    selected_param = "ParamA",
    selected_use = "Aquatic Life",
    type = "CG",
    use_type = "Option 1"
  )
  expect_s3_class(map_cg, "leaflet")
})

test_that("simplify_duration_frequency collapses labels", {
  x <- data.frame(
    DurationUnit = "n-day",
    DurationMethod = "Arithmetic Mean",
    DurationValue = 3,
    FreqValue = 10,
    FreqMethod = "Percent of samples not meeting",
    stringsAsFactors = FALSE
  )
  y <- simplify_duration_frequency(x)
  expect_true(all(c("Duration", "Frequency") %in% names(y)))
  expect_equal(y$Duration, "3-day Arithmetic Mean")
  expect_equal(y$Frequency, "10 Percent of samples not meeting")
})

test_that("capture_all_output collects messages and warnings and returns result", {
  res <- capture_all_output({
    message("hello")
    warning("warn here")
    42L
  })
  expect_true(is.list(res))
  expect_equal(res$result, 42L)
  expect_true(any(grepl("MESSAGE:", res$lines)))
  expect_true(any(grepl("WARNING:", res$lines)))
})

test_that("window_before_period returns valid periods for n-month and n-season", {
  wb_m2 <- window_before_period("n-month", 2)
  wb_s1 <- window_before_period("n-season", 1)

  expect_true(lubridate::is.period(wb_m2))
  expect_true(lubridate::is.period(wb_s1))

  dm2 <- lubridate::as.duration(wb_m2)
  ds1 <- lubridate::as.duration(wb_s1)

  expect_true(lubridate::time_length(dm2, "days") > 58)
  expect_true(lubridate::time_length(dm2, "days") < 63)

  expect_true(lubridate::time_length(ds1, "days") > 88)
  expect_true(lubridate::time_length(ds1, "days") < 94)
})

test_that("pH_filter, pH_join, and pH_fun compute means and nearest joins", {
  x <- make_base_obs()
  ph <- pH_filter(x)
  expect_true(all(c("DateTime_upper", "DateTime_lower", "pH") %in% names(ph)))
  expect_true(nrow(ph) >= 1)

  out_join <- pH_join(x, ph)
  expect_true(all(c("DateTime", "DateTime_pH") %in% names(out_join)))
  expect_true(!anyDuplicated(out_join$DateTime))

  out_fun <- pH_fun(x)
  expect_true("pH" %in% names(out_fun))
})

test_that("temp_filter, temp_join, and Temperature_fun compute means and nearest joins", {
  x <- make_base_obs()
  tf <- temp_filter(x)
  expect_true(all(
    c("DateTime_upper", "DateTime_lower", "Temperature") %in% names(tf)
  ))
  expect_true(nrow(tf) >= 1)

  out_join <- temp_join(x, tf)
  expect_true(all(c("DateTime", "DateTime_Temperature") %in% names(out_join)))
  expect_true(!anyDuplicated(out_join$DateTime))

  out_fun <- Temperature_fun(x)
  expect_true("Temperature" %in% names(out_fun))
})

test_that("hardness_filter and hardness_fun cap hardness at 400", {
  hdat <- data.frame(
    ActivityStartDate = as.Date(c("2020-01-02", "2020-01-02")),
    `ActivityStartTime.Time` = c("08:00:00", "08:00:00"),
    TADA.MonitoringLocationIdentifier = "S1",
    TADA.MonitoringLocationTypeName = "River/Stream",
    TADA.LatitudeMeasure = 45,
    TADA.LongitudeMeasure = -122,
    TADA.ResultMeasureValue = c(450, 410),
    TADA.CharacteristicName = "HARDNESS, CA, MG",
    stringsAsFactors = FALSE
  )
  hf <- hardness_filter(hdat)
  expect_true("Hardness" %in% names(hf))
  expect_true(all(is.finite(hf$Hardness)))

  capped <- hardness_fun(hdat)
  expect_true("Hardness" %in% names(capped))
  expect_true(all(capped$Hardness <= 400))
})

test_that("simplify_duration_frequency handles n-hour and n-season labels", {
  x <- data.frame(
    DurationUnit = c("n-hour", "n-season"),
    DurationMethod = c("Arithmetic Mean", "Geometric Mean"),
    DurationValue = c(1, 2),
    FreqValue = c(5, 1),
    FreqMethod = c("NumberNotMeeting", "Percentile"),
    stringsAsFactors = FALSE
  )
  y <- simplify_duration_frequency(x)
  expect_true(all(c("Duration", "Frequency") %in% names(y)))
  expect_equal(y$Duration[1], "1-hour Arithmetic Mean")
  expect_equal(y$Duration[2], "2-season Geometric Mean")
})

test_that("hardness_eq returns expected with both CF_A and CF_B present", {
  val <- hardness_eq(
    hardness = 100,
    E_A = 0.2,
    E_B = 0.8,
    CF_A = 2.2,
    CF_B = 0.4,
    CF_C = 1.1
  )
  CF2 <- 2.2 - (log(100) * 0.4)
  expect_equal(val, exp(0.2 * log(100) + 0.8) * CF2)
})

test_that("capture_all_output returns try-error on stop()", {
  res <- capture_all_output({
    stop("boom")
  })
  expect_true(is.list(res))
  expect_true(inherits(res$result, "try-error"))
  expect_true(length(res$lines) >= 0)
})

test_that("criteria_join handles options and filtering robustly", {
  skip_if_not_installed("EPATADA")

  x <- data.frame(
    TADA.CharacteristicName = c("ParamA", "ParamA", "ParamB"),
    TADA.ResultSampleFractionText = c("Dissolved", "Total", "Total"),
    TADA.MethodSpeciationName = c(NA, NA, NA),
    TADA.ResultMeasure.MeasureUnitCode = c("mg/L", "mg/L", "ug/L"),
    ATTAINS.UseName = c("Aquatic Life", "Aquatic Life", "Recreation"),
    ATTAINS.OrganizationIdentifier = c("Org1", "Org1", "Org2"),
    TADA.MonitoringLocationIdentifier = c("S1", "S2", "S3"),
    TADA.MonitoringLocationTypeName = c(
      "River/Stream",
      "River/Stream",
      "River/Stream"
    ),
    TADA.LatitudeMeasure = c(45, 46, 47),
    TADA.LongitudeMeasure = c(-122, -123, -124),
    DateTime = as.POSIXct(
      c("2020-01-01 08:00:00", "2020-01-01 09:00:00", "2020-01-01 10:00:00"),
      tz = "UTC"
    ),
    stringsAsFactors = FALSE
  )

  y <- data.frame(
    TADA.CharacteristicName = c("ParamA", "ParamB"),
    TADA.ResultSampleFractionText = c("Dissolved", "Total"),
    TADA.MethodSpeciationName = c(NA, NA),
    MagnitudeUnit = c("mg/L", "ug/L"),
    ATTAINS.UseName = c("Aquatic Life", "Recreation"),
    ATTAINS.OrganizationIdentifier = c("Org1", "Org2"),
    MagnitudeValueUpper = c(10, 5),
    stringsAsFactors = FALSE
  )

  out1 <- criteria_join(
    x = x,
    y = y,
    match_type = "Option 1",
    use_type = "Option 1",
    filter_type = FALSE
  )
  expect_s3_class(out1, "data.frame")
  expect_true("Matched" %in% names(out1))
  expect_equal(nrow(out1), nrow(x))
  expect_true(all(out1$Matched %in% c("Yes", "No")))
  expect_true("MagnitudeValueUpper" %in% names(out1))

  out2 <- criteria_join(
    x = x,
    y = y,
    match_type = "Option 1",
    use_type = "Option 2",
    filter_type = FALSE
  )
  expect_s3_class(out2, "data.frame")
  expect_true("Matched" %in% names(out2))
  expect_equal(nrow(out2), nrow(x))
  expect_true(all(out2$Matched %in% c("Yes", "No")))
  expect_true("ATTAINS.UseName" %in% names(out2))

  out3 <- criteria_join(
    x = x,
    y = y,
    match_type = "Option 2",
    use_type = "Option 1",
    filter_type = FALSE
  )
  expect_s3_class(out3, "data.frame")
  expect_true("Matched" %in% names(out3))
  expect_equal(nrow(out3), nrow(x))
  expect_true(all(out3$Matched %in% c("Yes", "No")))
})

test_that("criteria_join returns consistent structure for Option 1/Option 1 with filter_type = FALSE", {
  skip_if_not_installed("EPATADA")

  x <- data.frame(
    TADA.CharacteristicName = c("ParamA", "ParamA", "ParamB"),
    TADA.ResultSampleFractionText = c("Dissolved", "Total", "Total"),
    TADA.MethodSpeciationName = c(NA, NA, NA),
    TADA.ResultMeasure.MeasureUnitCode = c("mg/L", "mg/L", "ug/L"),
    ATTAINS.UseName = c("Aquatic Life", "Aquatic Life", "Recreation"),
    ATTAINS.OrganizationIdentifier = c("Org1", "Org1", "Org2"),
    TADA.MonitoringLocationIdentifier = c("S1", "S2", "S3"),
    TADA.MonitoringLocationTypeName = c(
      "River/Stream",
      "River/Stream",
      "River/Stream"
    ),
    TADA.LatitudeMeasure = c(45, 46, 47),
    TADA.LongitudeMeasure = c(-122, -123, -124),
    DateTime = as.POSIXct(
      c("2020-01-01 08:00:00", "2020-01-01 09:00:00", "2020-01-01 10:00:00"),
      tz = "UTC"
    ),
    stringsAsFactors = FALSE
  )

  y <- data.frame(
    TADA.CharacteristicName = c("ParamA", "ParamB"),
    TADA.ResultSampleFractionText = c("Dissolved", "Total"),
    TADA.MethodSpeciationName = c(NA, NA),
    MagnitudeUnit = c("mg/L", "ug/L"),
    ATTAINS.UseName = c("Aquatic Life", "Recreation"),
    ATTAINS.OrganizationIdentifier = c("Org1", "Org2"),
    MagnitudeValueUpper = c(10, 5),
    stringsAsFactors = FALSE
  )

  out <- criteria_join(
    x = x,
    y = y,
    match_type = "Option 1",
    use_type = "Option 1",
    filter_type = FALSE
  )

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), nrow(x))
  expect_true("Matched" %in% names(out))
  expect_true(all(out$Matched %in% c("Yes", "No")))
  expect_true(all(
    c(
      "TADA.CharacteristicName",
      "TADA.ResultMeasure.MeasureUnitCode",
      "ATTAINS.UseName",
      "ATTAINS.OrganizationIdentifier"
    ) %in%
      names(out)
  ))
  if ("MagnitudeValueUpper" %in% names(out)) {
    expect_true(is.numeric(out$MagnitudeValueUpper))
  }
})

test_that("criteria_join Option 1/Option 2 (use dropped from x) with filter_type = FALSE keeps structure", {
  skip_if_not_installed("EPATADA")

  x <- data.frame(
    TADA.CharacteristicName = c("ParamA", "ParamA", "ParamB"),
    TADA.ResultSampleFractionText = c("Dissolved", "Total", "Total"),
    TADA.MethodSpeciationName = c(NA, NA, NA),
    TADA.ResultMeasure.MeasureUnitCode = c("mg/L", "mg/L", "ug/L"),
    ATTAINS.UseName = c("Aquatic Life", "Aquatic Life", "Recreation"),
    ATTAINS.OrganizationIdentifier = c("Org1", "Org1", "Org2"),
    TADA.MonitoringLocationIdentifier = c("S1", "S2", "S3"),
    TADA.MonitoringLocationTypeName = c(
      "River/Stream",
      "River/Stream",
      "River/Stream"
    ),
    TADA.LatitudeMeasure = c(45, 46, 47),
    TADA.LongitudeMeasure = c(-122, -123, -124),
    DateTime = as.POSIXct(
      c("2020-01-01 08:00:00", "2020-01-01 09:00:00", "2020-01-01 10:00:00"),
      tz = "UTC"
    ),
    stringsAsFactors = FALSE
  )

  y <- data.frame(
    TADA.CharacteristicName = c("ParamA", "ParamB"),
    TADA.ResultSampleFractionText = c("Dissolved", "Total"),
    TADA.MethodSpeciationName = c(NA, NA),
    MagnitudeUnit = c("mg/L", "ug/L"),
    ATTAINS.UseName = c("Aquatic Life", "Recreation"),
    ATTAINS.OrganizationIdentifier = c("Org1", "Org2"),
    MagnitudeValueUpper = c(10, 5),
    stringsAsFactors = FALSE
  )

  out <- criteria_join(
    x = x,
    y = y,
    match_type = "Option 1",
    use_type = "Option 2",
    filter_type = FALSE
  )

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), nrow(x))
  expect_true("Matched" %in% names(out))
  expect_true(all(out$Matched %in% c("Yes", "No")))
  expect_true("ATTAINS.UseName" %in% names(out))
})

test_that("criteria_join Option 2 match_type works with both filter settings", {
  skip_if_not_installed("EPATADA")

  x <- data.frame(
    TADA.CharacteristicName = c("ParamA", "ParamA", "ParamB"),
    TADA.ResultSampleFractionText = c("Dissolved", "Total", "Total"),
    TADA.MethodSpeciationName = c(NA, NA, NA),
    TADA.ResultMeasure.MeasureUnitCode = c("mg/L", "mg/L", "ug/L"),
    ATTAINS.UseName = c("Aquatic Life", "Aquatic Life", "Recreation"),
    ATTAINS.OrganizationIdentifier = c("Org1", "Org1", "Org2"),
    TADA.MonitoringLocationIdentifier = c("S1", "S2", "S3"),
    TADA.MonitoringLocationTypeName = c(
      "River/Stream",
      "River/Stream",
      "River/Stream"
    ),
    TADA.LatitudeMeasure = c(45, 46, 47),
    TADA.LongitudeMeasure = c(-122, -123, -124),
    DateTime = as.POSIXct(
      c("2020-01-01 08:00:00", "2020-01-01 09:00:00", "2020-01-01 10:00:00"),
      tz = "UTC"
    ),
    stringsAsFactors = FALSE
  )

  y <- data.frame(
    TADA.CharacteristicName = c("ParamA", "ParamB"),
    TADA.ResultSampleFractionText = c("Dissolved", "Total"),
    TADA.MethodSpeciationName = c(NA, NA),
    MagnitudeUnit = c("mg/L", "ug/L"),
    ATTAINS.UseName = c("Aquatic Life", "Recreation"),
    ATTAINS.OrganizationIdentifier = c("Org1", "Org2"),
    MagnitudeValueUpper = c(10, 5),
    stringsAsFactors = FALSE
  )

  out_all <- criteria_join(
    x = x,
    y = y,
    match_type = "Option 2",
    use_type = "Option 1",
    filter_type = FALSE
  )
  expect_s3_class(out_all, "data.frame")
  expect_equal(nrow(out_all), nrow(x))
  expect_true("Matched" %in% names(out_all))
  expect_true(all(out_all$Matched %in% c("Yes", "No")))

  out_filt <- criteria_join(
    x = x,
    y = y,
    match_type = "Option 2",
    use_type = "Option 1",
    filter_type = TRUE
  )
  expect_s3_class(out_filt, "data.frame")
  expect_true(nrow(out_filt) <= nrow(x))
  if (nrow(out_filt) > 0) {
    expect_true(all(out_filt$Matched == "Yes"))
    # Optional: ensure MagnitudeValueUpper present when matched
    if ("MagnitudeValueUpper" %in% names(out_filt)) {
      expect_true(all(is.finite(out_filt$MagnitudeValueUpper)))
    }
  }
})

test_that("criteria_join tolerates duplicate criteria rows and preserves Matched", {
  skip_if_not_installed("EPATADA")

  x <- data.frame(
    TADA.CharacteristicName = c("ParamA", "ParamB"),
    TADA.ResultSampleFractionText = c("Dissolved", "Total"),
    TADA.MethodSpeciationName = c(NA, NA),
    TADA.ResultMeasure.MeasureUnitCode = c("mg/L", "ug/L"),
    ATTAINS.UseName = c("Aquatic Life", "Recreation"),
    ATTAINS.OrganizationIdentifier = c("Org1", "Org2"),
    TADA.MonitoringLocationIdentifier = c("S1", "S2"),
    TADA.MonitoringLocationTypeName = c("River/Stream", "River/Stream"),
    TADA.LatitudeMeasure = c(45, 46),
    TADA.LongitudeMeasure = c(-122, -123),
    DateTime = as.POSIXct(
      c("2020-01-01 08:00:00", "2020-01-01 09:00:00"),
      tz = "UTC"
    ),
    stringsAsFactors = FALSE
  )

  y <- data.frame(
    TADA.CharacteristicName = c("ParamA", "ParamA", "ParamB"),
    TADA.ResultSampleFractionText = c("Dissolved", "Dissolved", "Total"),
    TADA.MethodSpeciationName = c(NA, NA, NA),
    MagnitudeUnit = c("mg/L", "mg/L", "ug/L"),
    ATTAINS.UseName = c("Aquatic Life", "Aquatic Life", "Recreation"),
    ATTAINS.OrganizationIdentifier = c("Org1", "Org1", "Org2"),
    MagnitudeValueUpper = c(10, 12, 5),
    stringsAsFactors = FALSE
  )

  out <- criteria_join(
    x = x,
    y = y,
    match_type = "Option 1",
    use_type = "Option 1",
    filter_type = FALSE
  )
  expect_s3_class(out, "data.frame")
  expect_true("Matched" %in% names(out))
  expect_true(all(out$Matched %in% c("Yes", "No")))
})

test_that("duration_excursion_fun flags based on extremes and standard methods", {
  # Extremes: min below lower threshold
  x_ext <- data.frame(
    DurationMethod = "Arithmetic Extremes",
    Threshold_Lower_win = 5,
    Threshold_Upper_win = NA_real_,
    Value_win_min = 4,
    Value_win_max = 10,
    E_Value = NA_real_,
    stringsAsFactors = FALSE
  )
  res_ext <- duration_excursion_fun(x_ext)
  expect_true(res_ext$Duration_Excursion)

  # Standard: E_Value above upper threshold
  x_std <- data.frame(
    DurationMethod = "Arithmetic Mean",
    Threshold_Lower_win = NA_real_,
    Threshold_Upper_win = 10,
    Value_win_min = NA_real_,
    Value_win_max = NA_real_,
    E_Value = 11,
    stringsAsFactors = FALSE
  )
  res_std <- duration_excursion_fun(x_std)
  expect_true(res_std$Duration_Excursion)

  # Standard: E_Value within thresholds => FALSE
  x_std2 <- data.frame(
    DurationMethod = "Arithmetic Mean",
    Threshold_Lower_win = 5,
    Threshold_Upper_win = 10,
    Value_win_min = NA_real_,
    Value_win_max = NA_real_,
    E_Value = 7,
    stringsAsFactors = FALSE
  )
  res_std2 <- duration_excursion_fun(x_std2)
  expect_false(res_std2$Duration_Excursion)
})

test_that("criteria_join with filter_type = TRUE returns empty when no matches", {
  skip_if_not_installed("EPATADA")

  x <- data.frame(
    TADA.CharacteristicName = c("ParamX"),
    TADA.ResultSampleFractionText = c("Total"),
    TADA.MethodSpeciationName = c(NA),
    TADA.ResultMeasure.MeasureUnitCode = c("mg/L"),
    ATTAINS.UseName = c("Aquatic Life"),
    ATTAINS.OrganizationIdentifier = c("Org1"),
    TADA.MonitoringLocationIdentifier = c("S1"),
    TADA.MonitoringLocationTypeName = c("River/Stream"),
    TADA.LatitudeMeasure = c(45),
    TADA.LongitudeMeasure = c(-122),
    DateTime = as.POSIXct("2020-01-01 08:00:00", tz = "UTC"),
    stringsAsFactors = FALSE
  )

  y <- data.frame(
    TADA.CharacteristicName = c("ParamY"), # different
    TADA.ResultSampleFractionText = c("Dissolved"),
    TADA.MethodSpeciationName = c(NA),
    MagnitudeUnit = c("ug/L"),
    ATTAINS.UseName = c("Recreation"),
    ATTAINS.OrganizationIdentifier = c("Org2"),
    MagnitudeValueUpper = c(5),
    stringsAsFactors = FALSE
  )

  out <- criteria_join(
    x = x,
    y = y,
    match_type = "Option 1",
    use_type = "Option 1",
    filter_type = TRUE
  )
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
})

test_that("pH_join handles tie on nearest DateTime and still returns single match per x row", {
  x <- data.frame(
    DateTime = as.POSIXct(
      c("2020-01-01 10:00:00", "2020-01-02 10:00:00"),
      tz = "UTC"
    ),
    TADA.MonitoringLocationIdentifier = c("S1", "S1"),
    TADA.MonitoringLocationTypeName = c("River/Stream", "River/Stream"),
    TADA.LatitudeMeasure = c(45, 45),
    TADA.LongitudeMeasure = c(-122, -122),
    TADA.CharacteristicName = c("ParamA", "ParamA"),
    TADA.ResultMeasureValue = c(1, 2),
    stringsAsFactors = FALSE
  )
  # pH records symmetric around first DateTime
  ph <- data.frame(
    DateTime = as.POSIXct(
      c("2020-01-01 09:00:00", "2020-01-01 11:00:00"),
      tz = "UTC"
    ),
    TADA.MonitoringLocationIdentifier = c("S1", "S1"),
    TADA.MonitoringLocationTypeName = c("River/Stream", "River/Stream"),
    TADA.LatitudeMeasure = c(45, 45),
    TADA.LongitudeMeasure = c(-122, -122),
    pH = c(7.0, 7.2),
    DateTime_upper = as.POSIXct(
      c("2020-01-02 10:00:00", "2020-01-02 10:00:00"),
      tz = "UTC"
    ),
    DateTime_lower = as.POSIXct(
      c("2019-12-31 10:00:00", "2019-12-31 10:00:00"),
      tz = "UTC"
    ),
    stringsAsFactors = FALSE
  )
  out <- pH_join(x, ph)
  # ensure one row per input DateTime
  expect_equal(nrow(out), nrow(x))
  expect_true(all(!is.na(out$DateTime)))
})

test_that("temp_join handles tie on nearest DateTime and returns single match per x row", {
  x <- data.frame(
    DateTime = as.POSIXct(
      c("2020-01-01 10:00:00", "2020-01-02 10:00:00"),
      tz = "UTC"
    ),
    TADA.MonitoringLocationIdentifier = c("S1", "S1"),
    TADA.MonitoringLocationTypeName = c("River/Stream", "River/Stream"),
    TADA.LatitudeMeasure = c(45, 45),
    TADA.LongitudeMeasure = c(-122, -122),
    TADA.CharacteristicName = c("ParamA", "ParamA"),
    TADA.ResultMeasureValue = c(1, 2),
    stringsAsFactors = FALSE
  )
  tf <- data.frame(
    DateTime = as.POSIXct(
      c("2020-01-01 09:00:00", "2020-01-01 11:00:00"),
      tz = "UTC"
    ),
    TADA.MonitoringLocationIdentifier = c("S1", "S1"),
    TADA.MonitoringLocationTypeName = c("River/Stream", "River/Stream"),
    TADA.LatitudeMeasure = c(45, 45),
    TADA.LongitudeMeasure = c(-122, -122),
    Temperature = c(9.0, 10.2),
    DateTime_upper = as.POSIXct(
      c("2020-01-02 10:00:00", "2020-01-02 10:00:00"),
      tz = "UTC"
    ),
    DateTime_lower = as.POSIXct(
      c("2019-12-31 10:00:00", "2019-12-31 10:00:00"),
      tz = "UTC"
    ),
    stringsAsFactors = FALSE
  )
  out <- temp_join(x, tf)
  expect_equal(nrow(out), nrow(x))
  expect_true(all(!is.na(out$DateTime)))
})

test_that("GetURL supports custom host", {
  url <- GetURL("USGSTopo", host = "example.com")
  expect_true(grepl("example.com", url))
  expect_true(grepl("USGSTopo", url))
})

test_that("simplify_duration_frequency handles NA fields gracefully", {
  x <- data.frame(
    DurationUnit = NA_character_,
    DurationMethod = NA_character_,
    DurationValue = NA_real_,
    FreqValue = NA_real_,
    FreqMethod = NA_character_,
    stringsAsFactors = FALSE
  )
  y <- simplify_duration_frequency(x)
  expect_true(all(c("Duration", "Frequency") %in% names(y)))
  expect_true(is.na(y$Duration))
  expect_true(is.na(y$Frequency))
})

test_that("time_aggregate works for type = 'AU' and preserves ordering", {
  x <- data.frame(
    TADA.MonitoringLocationIdentifier = rep("S1", 3),
    TADA.MonitoringLocationName = rep("Site 1", 3),
    TADA.LatitudeMeasure = rep(45, 3),
    TADA.LongitudeMeasure = rep(-122, 3),
    ATTAINS.AssessmentUnitIdentifier = rep("AU1", 3),
    ATTAINS.ParameterName = rep("ParamX", 3),
    TADA.CharacteristicName = rep("ParamX", 3),
    TADA.ResultSampleFractionText = rep(NA_character_, 3),
    TADA.MethodSpeciationName = rep(NA_character_, 3),
    TADA.ResultMeasure.MeasureUnitCode = rep("mg/L", 3),
    ATTAINS.UseName = rep("Aquatic Life", 3),
    AcuteChronic = rep(NA_character_, 3),
    UniqueSpatialCriteria = rep(NA_character_, 3),
    Season = rep(NA_character_, 3),
    ATTAINS.OrganizationIdentifier = rep("Org", 3),
    EquationBased = rep(NA_character_, 3),
    DurationUnit = rep("n-day", 3),
    DurationMethod = rep("Arithmetic Mean", 3),
    DurationValue = rep(1, 3),
    FreqValue = rep(0, 3),
    FreqMethod = rep("NumberNotMeeting", 3),
    EquationType = rep(NA_character_, 3),
    ActivityStartDate = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03")),
    DateTime = as.POSIXct(
      c("2020-01-01 08:00:00", "2020-01-02 08:00:00", "2020-01-03 08:00:00"),
      tz = "UTC"
    ),
    TADA.ResultMeasureValue = c(1, 2, 3),
    MagnitudeValueLower = c(NA, NA, NA),
    MagnitudeValueUpper = c(10, 10, 10),
    pH = c(7, 7.1, 7.2),
    Temperature = c(10, 11, 12),
    Hardness = c(100, 110, 120),
    stringsAsFactors = FALSE
  )
  agg <- time_aggregate(x, type = "AU")
  expect_true("DateTime" %in% names(agg))
  expect_true(is.unsorted(agg$DateTime) == FALSE) # sorted by DateTime
})

test_that("magnitude_update returns empty when no applicable EquationType rows present", {
  x <- data.frame(
    EquationType = c("Other"),
    Hardness_win = NA_real_,
    pH_win = NA_real_,
    Temperature_win = NA_real_,
    TADA.ResultSampleFractionText = c("Total"),
    MagnitudeValueUpper = NA_real_,
    stringsAsFactors = FALSE
  )
  out <- magnitude_update(
    x = x,
    match_type = "Option 1",
    hardness_equation = data.frame(),
    pH_equation = data.frame(),
    pH_Hardness_equation = data.frame(),
    pH_Temperature_equation = data.frame()
  )
  expect_true(nrow(out) == 0)
})

test_that("window_before_period handles unknown unit fallback to days", {
  wb <- window_before_period("n-week", 3)
  expect_true(lubridate::is.period(wb))
  expect_equal(
    lubridate::time_length(lubridate::as.duration(wb), "days"),
    2 # v - 1 days
  )
})

test_that("capture_all_output wraps long messages respecting width and returns result", {
  msg <- paste(rep("long message fragment", 20), collapse = " ")
  res <- capture_all_output(
    {
      message(msg)
      99L
    },
    width = 40
  )
  expect_equal(res$result, 99L)
  # Expect wrapped lines (multiple lines)
  expect_true(length(res$lines) >= 2)
  expect_true(any(grepl("^MESSAGE:", res$lines)))
})

test_that("duration_cal computes window statistics and handles extremes and complete_windows flag (via time_aggregate)", {
  skip_if_not_installed("slider")

  # Build raw measurements (must flow through time_aggregate to create Value)
  x <- data.frame(
    TADA.MonitoringLocationIdentifier = rep("S1", 4),
    TADA.MonitoringLocationName = rep("Site 1", 4),
    TADA.LatitudeMeasure = rep(45, 4),
    TADA.LongitudeMeasure = rep(-122, 4),
    ATTAINS.AssessmentUnitIdentifier = rep("AU1", 4),
    ATTAINS.ParameterName = rep("ParamX", 4),
    TADA.CharacteristicName = rep("ParamX", 4),
    TADA.ResultSampleFractionText = rep(NA_character_, 4),
    TADA.MethodSpeciationName = rep(NA_character_, 4),
    TADA.ResultMeasure.MeasureUnitCode = rep("mg/L", 4),
    ATTAINS.UseName = rep("Aquatic Life", 4),
    AcuteChronic = rep(NA_character_, 4),
    UniqueSpatialCriteria = rep(NA_character_, 4),
    Season = rep(NA_character_, 4),
    ATTAINS.OrganizationIdentifier = rep("Org", 4),
    EquationBased = rep(NA_character_, 4),
    DurationUnit = rep("n-day", 4),
    # Mix extremes and mean so we can test both branches
    DurationMethod = c(
      "Arithmetic Extremes",
      "Arithmetic Extremes",
      "Arithmetic Mean",
      "Arithmetic Mean"
    ),
    DurationValue = rep(1L, 4),
    FreqValue = rep(0L, 4),
    FreqMethod = rep("NumberNotMeeting", 4),
    EquationType = rep(NA_character_, 4),
    ActivityStartDate = as.Date(c(
      "2020-01-01",
      "2020-01-02",
      "2020-01-01",
      "2020-01-02"
    )),
    DateTime = as.POSIXct(
      c(
        "2020-01-01 08:00:00",
        "2020-01-02 08:00:00",
        "2020-01-01 08:00:00",
        "2020-01-02 08:00:00"
      ),
      tz = "UTC"
    ),
    TADA.ResultMeasureValue = c(1, 3, 2, 4),
    MagnitudeValueLower = c(NA, NA, NA, NA),
    MagnitudeValueUpper = c(10, 10, 10, 10),
    pH = c(7, 7, 7, 7),
    Temperature = c(10, 10, 10, 10),
    Hardness = c(100, 100, 100, 100),
    stringsAsFactors = FALSE
  )

  # Pipeline: aggregate -> duration windows
  agg <- time_aggregate(x, type = "MLid")
  expect_true("Value" %in% names(agg))
  out_true <- duration_cal(agg, type = "MLid", complete_windows = TRUE)

  expect_true(all(
    c(
      "Result_Duration",
      "Window_Start_win",
      "Window_End_win",
      "Value_win_min",
      "Value_win_max",
      "Window_Status"
    ) %in%
      names(out_true)
  ))

  # Extremes should have NA Result_Duration per implementation
  ext_rows <- out_true[
    grep("Arithmetic Extremes", out_true$DurationMethod, ignore.case = TRUE),
    ,
    drop = FALSE
  ]
  # Ensure we actually have extremes rows
  expect_true(nrow(ext_rows) >= 1)
  expect_true(all(is.na(ext_rows$Result_Duration)))

  # Means should be finite
  mean_rows <- out_true[
    grep("Arithmetic Mean", out_true$DurationMethod, ignore.case = TRUE),
    ,
    drop = FALSE
  ]
  expect_true(nrow(mean_rows) >= 1)
  expect_true(all(is.finite(mean_rows$Result_Duration)))

  # complete_windows = FALSE should still return complete windows
  out_false <- duration_cal(agg, type = "MLid", complete_windows = FALSE)
  expect_true(any(out_false$Window_Status == "complete"))
})

test_that("frequency_summary computes outputs for all frequency methods (via duration_cal)", {
  skip_if_not_installed("slider")

  # Helper to run the pipeline and frequency summary
  run_freq <- function(df, type) {
    agg <- time_aggregate(df, type = type)
    dur <- duration_cal(agg, type = type, complete_windows = TRUE)
    frequency_summary(dur, type = type)
  }

  # Base raw measurements for NumberNotMeeting and Percent of samples not meeting
  x_num <- data.frame(
    TADA.MonitoringLocationIdentifier = rep("S1", 4),
    TADA.MonitoringLocationName = rep("Site 1", 4),
    TADA.LatitudeMeasure = rep(45, 4),
    TADA.LongitudeMeasure = rep(-122, 4),
    ATTAINS.AssessmentUnitIdentifier = rep("AU1", 4),
    ATTAINS.ParameterName = rep("ParamA", 4),
    TADA.CharacteristicName = rep("ParamA", 4),
    TADA.ResultSampleFractionText = rep(NA_character_, 4),
    TADA.MethodSpeciationName = rep(NA_character_, 4),
    TADA.ResultMeasure.MeasureUnitCode = rep("mg/L", 4),
    ATTAINS.UseName = rep("Aquatic Life", 4),
    AcuteChronic = rep(NA_character_, 4),
    UniqueSpatialCriteria = rep(NA_character_, 4),
    Season = rep(NA_character_, 4),
    ATTAINS.OrganizationIdentifier = rep("Org", 4),
    EquationBased = rep(NA_character_, 4),
    DurationUnit = rep("n-day", 4),
    DurationMethod = rep("Arithmetic Mean", 4),
    DurationValue = rep(1L, 4),
    FreqValue = rep(0L, 4),
    FreqMethod = rep("NumberNotMeeting", 4),
    EquationType = rep(NA_character_, 4),
    ActivityStartDate = as.Date(c(
      "2020-01-01",
      "2020-01-02",
      "2020-01-03",
      "2020-01-04"
    )),
    DateTime = as.POSIXct(
      c(
        "2020-01-01 08:00:00",
        "2020-01-02 08:00:00",
        "2020-01-03 08:00:00",
        "2020-01-04 08:00:00"
      ),
      tz = "UTC"
    ),
    TADA.ResultMeasureValue = c(9, 11, 10, 12), # 2 excursions if upper=10
    MagnitudeValueLower = rep(NA_real_, 4),
    MagnitudeValueUpper = rep(10, 4),
    pH = c(7, 7, 7, 7),
    Temperature = c(10, 10, 10, 10),
    Hardness = c(100, 100, 100, 100),
    stringsAsFactors = FALSE
  )

  fs_num <- run_freq(x_num, type = "MLid")
  expect_true(all(
    c("Exceedance", "Sample_Count", "Number_of_Excursions") %in% names(fs_num)
  ))
  expect_true(any(fs_num$Exceedance == "Exceed"))

  # Percent of samples not meeting: 50% excursions vs threshold 20% => Exceed
  x_pct <- x_num
  x_pct$FreqMethod <- "Percent of samples not meeting"
  x_pct$FreqValue <- 20
  fs_pct <- run_freq(x_pct, type = "MLid")
  expect_true(any(fs_pct$Exceedance == "Exceed"))
  expect_true(is.finite(fs_pct$Excursion_Percentage))

  # Percentile: make a dataset with an outlier; 90th percentile should exceed upper 10
  x_perc <- data.frame(
    TADA.MonitoringLocationIdentifier = rep("S1", 5),
    TADA.MonitoringLocationName = rep("Site 1", 5),
    TADA.LatitudeMeasure = rep(45, 5),
    TADA.LongitudeMeasure = rep(-122, 5),
    ATTAINS.AssessmentUnitIdentifier = rep("AU1", 5),
    ATTAINS.ParameterName = rep("ParamB", 5),
    TADA.CharacteristicName = rep("ParamB", 5),
    TADA.ResultSampleFractionText = rep(NA_character_, 5),
    TADA.MethodSpeciationName = rep(NA_character_, 5),
    TADA.ResultMeasure.MeasureUnitCode = rep("mg/L", 5),
    ATTAINS.UseName = rep("Aquatic Life", 5),
    AcuteChronic = rep(NA_character_, 5),
    UniqueSpatialCriteria = rep(NA_character_, 5),
    Season = rep(NA_character_, 5),
    ATTAINS.OrganizationIdentifier = rep("Org", 5),
    EquationBased = rep(NA_character_, 5),
    DurationUnit = rep("n-day", 5),
    DurationMethod = rep("Arithmetic Mean", 5),
    DurationValue = rep(1L, 5),
    FreqValue = rep(90, 5),
    FreqMethod = rep("Percentile", 5),
    EquationType = rep(NA_character_, 5),
    ActivityStartDate = as.Date(c(
      "2020-01-01",
      "2020-01-02",
      "2020-01-03",
      "2020-01-04",
      "2020-01-05"
    )),
    DateTime = as.POSIXct(
      c(
        "2020-01-01 08:00:00",
        "2020-01-02 08:00:00",
        "2020-01-03 08:00:00",
        "2020-01-04 08:00:00",
        "2020-01-05 08:00:00"
      ),
      tz = "UTC"
    ),
    TADA.ResultMeasureValue = c(1, 2, 3, 4, 100),
    MagnitudeValueLower = rep(NA_real_, 5),
    MagnitudeValueUpper = rep(10, 5),
    pH = c(7, 7, 7, 7, 7),
    Temperature = c(10, 10, 10, 10, 10),
    Hardness = c(100, 100, 100, 100, 100),
    stringsAsFactors = FALSE
  )
  fs_perc <- run_freq(x_perc, type = "MLid")
  expect_true("Percentile" %in% names(fs_perc))
  expect_true(any(fs_perc$Exceedance == "Exceed"))

  # n-samples in 3 years: allow 1 excursion in 3 years; build a 3.5-year series with multiple excursions
  x_n3 <- data.frame(
    TADA.MonitoringLocationIdentifier = rep("S1", 8),
    TADA.MonitoringLocationName = rep("Site 1", 8),
    TADA.LatitudeMeasure = rep(45, 8),
    TADA.LongitudeMeasure = rep(-122, 8),
    ATTAINS.AssessmentUnitIdentifier = rep("AU1", 8),
    ATTAINS.ParameterName = rep("ParamC", 8),
    TADA.CharacteristicName = rep("ParamC", 8),
    TADA.ResultSampleFractionText = rep(NA_character_, 8),
    TADA.MethodSpeciationName = rep(NA_character_, 8),
    TADA.ResultMeasure.MeasureUnitCode = rep("mg/L", 8),
    ATTAINS.UseName = rep("Aquatic Life", 8),
    AcuteChronic = rep(NA_character_, 8),
    UniqueSpatialCriteria = rep(NA_character_, 8),
    Season = rep(NA_character_, 8),
    ATTAINS.OrganizationIdentifier = rep("Org", 8),
    EquationBased = rep(NA_character_, 8),
    DurationUnit = rep("n-day", 8),
    DurationMethod = rep("Arithmetic Mean", 8),
    DurationValue = rep(1L, 8),
    FreqValue = rep(1L, 8), # allow 1 excursion in 3 years
    FreqMethod = rep("n-samples in 3 years", 8),
    EquationType = rep(NA_character_, 8),
    ActivityStartDate = as.Date(c(
      "2018-01-01",
      "2018-06-01",
      "2019-01-01",
      "2019-06-01",
      "2020-01-01",
      "2020-06-01",
      "2021-01-01",
      "2021-06-01"
    )),
    DateTime = as.POSIXct(
      c(
        "2018-01-01 08:00:00",
        "2018-06-01 08:00:00",
        "2019-01-01 08:00:00",
        "2019-06-01 08:00:00",
        "2020-01-01 08:00:00",
        "2020-06-01 08:00:00",
        "2021-01-01 08:00:00",
        "2021-06-01 08:00:00"
      ),
      tz = "UTC"
    ),
    TADA.ResultMeasureValue = c(9, 11, 12, 9, 8, 15, 9, 9),
    MagnitudeValueLower = rep(NA_real_, 8),
    MagnitudeValueUpper = rep(10, 8),
    pH = rep(7, 8),
    Temperature = rep(10, 8),
    Hardness = rep(100, 8),
    stringsAsFactors = FALSE
  )
  fs_n3 <- run_freq(x_n3, type = "AU")
  expect_true("Exceedance" %in% names(fs_n3))
  expect_true(any(fs_n3$Exceedance %in% c("Exceed", "Not Exceed")))
})


test_that("setup: EPATADA and criteria are available", {
  skip_if_not_installed("EPATADA")
  criteria <- EPATADA::TADA_GetCriteriaFile(org_id = "MTDEQ")
  expect_true(is.data.frame(criteria))
  expect_gt(nrow(criteria), 0)
})

test_that("Pass 1: matches by TADA.ComparableDataIdentifier (ID join)", {
  skip_if_not_installed("EPATADA")
  utils::data("Data_MT_MissoulaCounty", package = "EPATADA")
  criteria_all <- Data_MT_MissoulaCounty

  skip_if(!("TADA.ComparableDataIdentifier" %in% names(criteria_all)))
  crit1_row <- criteria_all |>
    dplyr::filter(!is.na(.data$`TADA.ComparableDataIdentifier`)) |>
    dplyr::slice(1)
  skip_if(nrow(crit1_row) == 0)

  # Add a marker so we can confirm the join
  criteria_p1 <- crit1_row |> dplyr::mutate(marker = "p1")

  # Build WQP that matches by ID only; case-mix to test uppercasing
  wqp <- dplyr::tibble(
    TADA.ComparableDataIdentifier = tolower(
      crit1_row$TADA.ComparableDataIdentifier
    ),
    # These are not used in pass 1 keys and can be anything
    TADA.CharacteristicName = "dummy",
    TADA.ResultSampleFractionText = "dummy",
    TADA.MethodSpeciationName = "dummy",
    wqp_row_id = "w1"
  )

  out <- join_wqp_criteria(wqp, criteria_p1, byChar = FALSE)

  expect_equal(nrow(out), nrow(wqp))
  expect_true("marker" %in% names(out))
  expect_equal(unique(out$marker), "p1")
  # Ensure there are no .x/.y suffix columns
  expect_false(any(grepl("\\.(x|y)$", names(out))))
})

test_that("Pass 2: matches by Characteristic + Fraction + Speciation", {
  skip_if_not_installed("EPATADA")
  criteria_all <- EPATADA::TADA_GetCriteriaFile(org_id = "MTDEQ")

  crit2_row <- criteria_all |>
    dplyr::filter(
      is.na(.data$`TADA.ComparableDataIdentifier`),
      !is.na(.data$`TADA.ResultSampleFractionText`),
      !is.na(.data$`TADA.MethodSpeciationName`)
    ) |>
    dplyr::slice(1)
  skip_if(nrow(crit2_row) == 0)

  criteria_p2 <- crit2_row |> dplyr::mutate(marker = "p2")

  # Build WQP using mixed case keys to validate uppercasing
  wqp <- dplyr::tibble(
    TADA.CharacteristicName = tolower(crit2_row$TADA.CharacteristicName),
    TADA.ResultSampleFractionText = tolower(
      crit2_row$TADA.ResultSampleFractionText
    ),
    TADA.MethodSpeciationName = tolower(crit2_row$TADA.MethodSpeciationName),
    wqp_row_id = "w2"
  )

  out <- join_wqp_criteria(wqp, criteria_p2, byChar = FALSE)

  expect_equal(nrow(out), nrow(wqp))
  expect_true("marker" %in% names(out))
  expect_equal(unique(out$marker), "p2")
  expect_false(any(grepl("\\.(x|y)$", names(out))))
})

test_that("Pass 3: matches by Characteristic + Fraction (Speciation is NA)", {
  skip_if_not_installed("EPATADA")
  criteria_all <- EPATADA::TADA_GetCriteriaFile(org_id = "MTDEQ")

  crit3_row <- criteria_all |>
    dplyr::filter(
      is.na(.data$`TADA.ComparableDataIdentifier`),
      !is.na(.data$`TADA.ResultSampleFractionText`),
      is.na(.data$`TADA.MethodSpeciationName`)
    ) |>
    dplyr::slice(1)
  skip_if(nrow(crit3_row) == 0)

  criteria_p3 <- crit3_row |> dplyr::mutate(marker = "p3")

  wqp <- dplyr::tibble(
    TADA.CharacteristicName = tolower(crit3_row$TADA.CharacteristicName),
    TADA.ResultSampleFractionText = tolower(
      crit3_row$TADA.ResultSampleFractionText
    ),
    TADA.MethodSpeciationName = NA_character_, # must be NA to match pass 3
    wqp_row_id = "w3"
  )

  out <- join_wqp_criteria(wqp, criteria_p3, byChar = FALSE)

  expect_equal(nrow(out), nrow(wqp))
  expect_true("marker" %in% names(out))
  expect_equal(unique(out$marker), "p3")
  expect_false(any(grepl("\\.(x|y)$", names(out))))
})

test_that("Pass 4: matches by Characteristic + Speciation (Fraction is NA)", {
  skip_if_not_installed("EPATADA")
  criteria_all <- EPATADA::TADA_GetCriteriaFile(org_id = "MTDEQ")

  crit4_row <- criteria_all |>
    dplyr::filter(
      is.na(.data$`TADA.ComparableDataIdentifier`),
      is.na(.data$`TADA.ResultSampleFractionText`),
      !is.na(.data$`TADA.MethodSpeciationName`)
    ) |>
    dplyr::slice(1)
  skip_if(nrow(crit4_row) == 0)

  criteria_p4 <- crit4_row |> dplyr::mutate(marker = "p4")

  wqp <- dplyr::tibble(
    TADA.CharacteristicName = tolower(crit4_row$TADA.CharacteristicName),
    TADA.ResultSampleFractionText = NA_character_, # must be NA to match pass 4
    TADA.MethodSpeciationName = tolower(crit4_row$TADA.MethodSpeciationName),
    wqp_row_id = "w4"
  )

  out <- join_wqp_criteria(wqp, criteria_p4, byChar = FALSE)

  expect_equal(nrow(out), nrow(wqp))
  expect_true("marker" %in% names(out))
  expect_equal(unique(out$marker), "p4")
  expect_false(any(grepl("\\.(x|y)$", names(out))))
})

test_that("Pass 5: matches by Characteristic only (Fraction and Speciation are NA)", {
  skip_if_not_installed("EPATADA")
  criteria_all <- EPATADA::TADA_GetCriteriaFile(org_id = "MTDEQ")

  crit5_row <- criteria_all |>
    dplyr::filter(
      is.na(.data$`TADA.ComparableDataIdentifier`),
      is.na(.data$`TADA.ResultSampleFractionText`),
      is.na(.data$`TADA.MethodSpeciationName`)
    ) |>
    dplyr::slice(1)
  skip_if(nrow(crit5_row) == 0)

  criteria_p5 <- crit5_row |> dplyr::mutate(marker = "p5")

  wqp <- dplyr::tibble(
    TADA.CharacteristicName = tolower(crit5_row$TADA.CharacteristicName),
    TADA.ResultSampleFractionText = NA_character_,
    TADA.MethodSpeciationName = NA_character_,
    wqp_row_id = "w5"
  )

  out <- join_wqp_criteria(wqp, criteria_p5, byChar = FALSE)

  expect_equal(nrow(out), nrow(wqp))
  expect_true("marker" %in% names(out))
  expect_equal(unique(out$marker), "p5")
  expect_false(any(grepl("\\.(x|y)$", names(out))))
})
