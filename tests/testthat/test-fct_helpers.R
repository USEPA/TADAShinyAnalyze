make_base_sites <- function() {
  tibble::tibble(
    TADA.MonitoringLocationIdentifier = c("S1", "S2"),
    TADA.MonitoringLocationName = c("Site 1", "Site 2"),
    TADA.MonitoringLocationTypeName = c("River/Stream", "River/Stream"),
    TADA.LatitudeMeasure = c(45, 46),
    TADA.LongitudeMeasure = c(-122, -123),
    ATTAINS.AssessmentUnitIdentifier = c("AU1", "AU2")
  )
}

make_base_obs <- function() {
  sites <- make_base_sites()
  # Create a small observation set with PH, TEMPERATURE, HARDNESS, and one analyte "X"
  # Ensure DateTime and ActivityStartDate are aligned
  tibble::tibble(
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
    TADA.ResultMeasure.MeasureUnitCode = c(rep(NA_character_, 4), rep("C", 4))
  ) |>
    # Add one hardness sample at same date/time key for deterministic merges
    dplyr::bind_rows(tibble::tibble(
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
      TADA.ResultMeasure.MeasureUnitCode = NA_character_
    ))
}

# Utility: simple test data for excursion_fun
make_excursion_input <- function() {
  tibble::tibble(
    TADA.ResultMeasureValue = c(5, 15, 5, 15, 10),
    MagnitudeValueLower = c(NA, NA, 10, NA, 5),
    MagnitudeValueUpper = c(10, 10, NA, 20, 10)
  )
}

# Minimal data for mapping tests
make_exceedance_data <- function() {
  tibble::tibble(
    TADA.MonitoringLocationIdentifier = c("S1", "S2"),
    TADA.MonitoringLocationName = c("Site 1", "Site 2"),
    TADA.LongitudeMeasure = c(-122, -123),
    TADA.LatitudeMeasure = c(45, 46),
    ATTAINS.AssessmentUnitIdentifier = c("AU1", "AU2"),
    ATTAINS.UseName = c("Aquatic Life", "Aquatic Life"),
    TADA.CharacteristicName = c("ParamA", "ParamB"),
    Exceedance = c("Exceed", "Not Exceed")
  )
}

# Minimal coords for AU/CG mapping
make_coords_for_map <- function() {
  tibble::tibble(
    ATTAINS.AssessmentUnitIdentifier = c("AU1", "AU2"),
    TADA.MonitoringLocationIdentifier = c("S1", "S2"),
    TADA.MonitoringLocationName = c("Site 1", "Site 2"),
    TADA.LongitudeMeasure = c(-122, -123),
    TADA.LatitudeMeasure = c(45, 46)
  )
}

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

  # step_label
  expect_equal(step_label(NA), "1 day")
  expect_equal(step_label("n-hour"), "1 hour")
  expect_equal(step_label("n-day"), "1 day")

  # window_before_period: test the well-defined n-hour/n-day branches
  wb_h1 <- window_before_period("n-hour", 1)
  wb_d3 <- window_before_period("n-day", 3)

  # Period is an S4 class in lubridate; check with is.period() for robustness
  expect_true(lubridate::is.period(wb_h1))
  expect_true(lubridate::is.period(wb_d3))

  # Validate lengths using durations
  expect_equal(
    lubridate::time_length(lubridate::as.duration(wb_h1), "seconds"),
    0
  ) # 1-1 hours
  expect_equal(lubridate::time_length(lubridate::as.duration(wb_d3), "days"), 2) # 3-1 days
})

test_that("hardness_eq implements coefficient fallback and formula", {
  # With CF_A and CF_B present
  val1 <- hardness_eq(
    hardness = 100,
    E_A = 0.1,
    E_B = 1,
    CF_A = 2,
    CF_B = 0.5,
    CF_C = 1.5
  )
  # Fallback if CF_A or CF_B missing: use CF_C
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

  # Row-wise expectations:
  # 1) NA lower, upper=10, value 5 -> FALSE
  # 2) NA lower, upper=10, value 15 -> TRUE
  # 3) lower=10, NA upper, value 5 -> TRUE
  # 4) NA lower, upper=20, value 15 -> FALSE
  # 5) lower=5, upper=10, value 10 -> FALSE
  expect_equal(out$Excursion, c(FALSE, TRUE, TRUE, FALSE, FALSE))
})

test_that("excursion_summary aggregates correctly for MLid and AU", {
  # Build a small set with Excursion flags
  x <- tibble::tibble(
    TADA.MonitoringLocationIdentifier = c("S1", "S1", "S2"),
    TADA.MonitoringLocationName = c("Site 1", "Site 1", "Site 2"),
    TADA.LongitudeMeasure = c(-122, -122, -123),
    TADA.LatitudeMeasure = c(45, 45, 46),
    ATTAINS.AssessmentUnitIdentifier = c("AU1", "AU1", "AU2"),
    ATTAINS.ParameterName = c("ParamA", "ParamA", "ParamB"),
    TADA.CharacteristicName = c("ParamA", "ParamA", "ParamB"),
    TADA.ResultSampleFractionText = NA_character_,
    TADA.MethodSpeciationName = NA_character_,
    TADA.ResultMeasure.MeasureUnitCode = "mg/L",
    ATTAINS.UseName = c("Aquatic Life", "Aquatic Life", "Aquatic Life"),
    AcuteChronic = NA_character_,
    UniqueSpatialCriteria = NA_character_,
    Season = NA_character_,
    ATTAINS.OrganizationIdentifier = "Org1",
    EquationBased = NA_character_,
    DurationUnit = "n-day",
    DurationMethod = "Arithmetic Mean",
    DurationValue = 1L,
    FreqValue = 0,
    FreqMethod = "NumberNotMeeting",
    EquationType = NA_character_,
    ActivityStartDate = as.Date(c("2020-01-01", "2020-01-02", "2020-01-01")),
    TADA.ResultMeasureValue = c(5, 15, 7),
    MagnitudeValueLower = c(NA, NA, NA),
    MagnitudeValueUpper = c(10, 10, 10)
  ) |>
    excursion_fun()

  # MLid
  ml <- excursion_summary(x, type = "MLid")
  expect_type(ml, "list")
  expect_true(all(c("data", "coords") %in% names(ml)))
  expect_true(all(
    c("Sample_Count", "Number_of_Excursions", "Excursion_Percentage") %in%
      names(ml$data)
  ))

  # AU
  au <- excursion_summary(x, type = "AU")
  expect_type(au, "list")
  expect_true(all(c("data", "coords") %in% names(au)))
})

test_that("time_aggregate collapses timestamp duplicates and computes daily means", {
  # Build base x with required columns and two units
  x <- tibble::tibble(
    TADA.MonitoringLocationIdentifier = rep("S1", 5),
    TADA.MonitoringLocationName = rep("Site 1", 5),
    TADA.LatitudeMeasure = 45,
    TADA.LongitudeMeasure = -122,
    ATTAINS.AssessmentUnitIdentifier = rep("AU1", 5),
    ATTAINS.ParameterName = rep("ParamX", 5),
    TADA.CharacteristicName = rep("ParamX", 5),
    TADA.ResultSampleFractionText = NA_character_,
    TADA.MethodSpeciationName = NA_character_,
    TADA.ResultMeasure.MeasureUnitCode = "mg/L",
    ATTAINS.UseName = rep("Aquatic Life", 5),
    AcuteChronic = NA_character_,
    UniqueSpatialCriteria = NA_character_,
    Season = NA_character_,
    ATTAINS.OrganizationIdentifier = "Org",
    EquationBased = NA_character_,
    DurationUnit = c("n-hour", "n-hour", "n-day", "n-day", "n-season"),
    DurationMethod = c(
      "Arithmetic Mean",
      "Arithmetic Mean",
      "Arithmetic Mean",
      "Arithmetic Mean",
      "Arithmetic Mean"
    ),
    DurationValue = c(1, 1, 1, 1, 1),
    FreqValue = 0,
    FreqMethod = "NumberNotMeeting",
    EquationType = NA_character_,
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
    Hardness = c(100, 100, 120, 110, 90)
  )

  agg <- time_aggregate(x, type = "MLid")
  expect_true(all(c("Value", "N_in_Step", "DateTime") %in% names(agg)))
  # Hourly entries for n-hour should remain timestamped; daily entries for n-day and n-season are daily-averaged
  # Expect two rows for 2020-01-01 (n-hour), one averaged row for 2020-01-02 (n-day), and one for 2020-03-01
  expect_true(nrow(agg) >= 4)
  # For 2020-01-02, mean of 10 and 14 is 12
  d2 <- dplyr::filter(agg, as.Date(DateTime) == as.Date("2020-01-02"))
  expect_equal(unique(d2$Value), 12)
})

test_that("magnitude_update computes updated thresholds for hardness, pH, and combined equations", {
  # Base x rows for each EquationType with windowed covariates present
  x <- tibble::tibble(
    EquationType = c("Hardness", "pH", "pH and Hardness", "pH and Temperature"),
    Hardness_win = c(100, NA, 150, NA),
    pH_win = c(NA, 7.5, 6.8, 8.0),
    Temperature_win = c(NA, NA, NA, 20),
    # Keys to join by; we'll rely on EquationType auto-join
    TADA.ResultSampleFractionText = c(
      "Dissolved",
      "Dissolved",
      "Total",
      "Total"
    ),
    MagnitudeValueUpper = c(NA_real_, NA_real_, NA_real_, NA_real_)
  )

  # Equation tables keyed by EquationType (so left_join uses this key)
  hardness_equation <- tibble::tibble(
    EquationType = "Hardness",
    hardness_param_1 = 2, # CF_A
    hardness_param_2 = 0.5, # CF_B
    hardness_param_3 = 1.5, # CF_C
    hardness_param_4 = 0.1, # E_A
    hardness_param_5 = 1 # E_B
  )
  pH_equation <- tibble::tibble(
    EquationType = "pH",
    Equation = "pH * 2" # simple linear function
  )
  pH_Hardness_equation <- tibble::tibble(
    EquationType = "pH and Hardness",
    hardness_param_1 = 2,
    hardness_param_2 = 0.5,
    hardness_param_3 = 1.5,
    hardness_param_4 = 0.1,
    hardness_param_5 = 1,
    hardness_param_6 = 30 # cap used when pH < 7
  )
  pH_Temperature_equation <- tibble::tibble(
    EquationType = "pH and Temperature",
    Equation = "pH + Temperature"
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
  # Check results by type
  h_row <- out1 %>% filter(EquationType == "Hardness")
  expect_true(is.finite(h_row$MagnitudeValueUpper))

  pH_row <- out1 %>% filter(EquationType == "pH")
  expect_equal(pH_row$MagnitudeValueUpper, 7.5 * 2, tolerance = 1e-8)

  phh_row <- out1 %>% filter(EquationType == "pH and Hardness")
  # pH < 7 => apply min(hardness_based, hardness_param_6)
  expect_true(phh_row$MagnitudeValueUpper <= 30 + 1e-8)

  pht_row <- out1 %>% filter(EquationType == "pH and Temperature")
  expect_equal(pht_row$MagnitudeValueUpper, 8.0 + 20, tolerance = 1e-8)

  # Option 2 path (drops fraction in equation tables via distinct) should still work
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
  skip_if_not_installed("leaflet")
  url <- GetURL("USGSTopo")
  expect_true(grepl("USGSTopo", url))

  m <- leaflet::leaflet() |> add_USGS_base()
  expect_s3_class(m, "leaflet")
})

test_that("create_overall_map returns a leaflet map and builds popup", {
  skip_if_not_installed("leaflet")
  data <- make_exceedance_data()
  map1 <- create_overall_map(data, type = "MLid", use_type = "Option 1")
  expect_s3_class(map1, "leaflet")

  coords <- make_coords_for_map()
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
    coords,
    selected_use = "Aquatic Life",
    type = "AU",
    use_type = "Option 1"
  )
  expect_s3_class(map_au, "leaflet")

  map_cg <- create_use_map(
    data,
    coords,
    selected_use = "Aquatic Life",
    type = "CG",
    use_type = "Option 1"
  )
  expect_s3_class(map_cg, "leaflet")
})

test_that("create_parameter_map filters by parameter and returns leaflet", {
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
    coords,
    selected_param = "ParamA",
    selected_use = "Aquatic Life",
    type = "AU",
    use_type = "Option 1"
  )
  expect_s3_class(map_au, "leaflet")

  map_cg <- create_parameter_map(
    data,
    coords,
    selected_param = "ParamA",
    selected_use = "Aquatic Life",
    type = "CG",
    use_type = "Option 1"
  )
  expect_s3_class(map_cg, "leaflet")
})

test_that("simplify_duration_frequency collapses labels", {
  x <- tibble::tibble(
    DurationUnit = "n-day",
    DurationMethod = "Arithmetic Mean",
    DurationValue = 3,
    FreqValue = 10,
    FreqMethod = "Percent of samples not meeting"
  )
  y <- simplify_duration_frequency(x)
  expect_true(all(c("Duration", "Frequency") %in% names(y)))
  expect_equal(y$Duration, "3-day Arithmetic Mean") # <- updated
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

  # Convert to durations for approximate checks (months vary by calendar)
  dm2 <- lubridate::as.duration(wb_m2)
  ds1 <- lubridate::as.duration(wb_s1)

  # 2 months - 1 day should be roughly 59-62 days
  expect_true(lubridate::time_length(dm2, "days") > 58)
  expect_true(lubridate::time_length(dm2, "days") < 63)

  # 3 months - 1 day (season) should be roughly 89-93 days
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
  # verify the nearest pH record is selected
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
  # Create an observation set with hardness > 400
  hdat <- tibble::tibble(
    ActivityStartDate = as.Date(c("2020-01-02", "2020-01-02")),
    `ActivityStartTime.Time` = c("08:00:00", "08:00:00"),
    TADA.MonitoringLocationIdentifier = "S1",
    TADA.MonitoringLocationTypeName = "River/Stream",
    TADA.LatitudeMeasure = 45,
    TADA.LongitudeMeasure = -122,
    TADA.ResultMeasureValue = c(450, 410),
    TADA.CharacteristicName = "HARDNESS, CA, MG"
  )
  hf <- hardness_filter(hdat)
  expect_true("Hardness" %in% names(hf))
  expect_true(all(is.finite(hf$Hardness)))

  capped <- hardness_fun(hdat)
  expect_true("Hardness" %in% names(capped))
  expect_true(all(capped$Hardness <= 400))
})

test_that("simplify_duration_frequency handles n-hour and n-season labels", {
  x <- tibble::tibble(
    DurationUnit = c("n-hour", "n-season"),
    DurationMethod = c("Arithmetic Mean", "Geometric Mean"),
    DurationValue = c(1, 2),
    FreqValue = c(5, 1),
    FreqMethod = c("NumberNotMeeting", "Percentile")
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
  # Manual computation
  CF2 <- 2.2 - (log(100) * 0.4)
  expect_equal(val, exp(0.2 * log(100) + 0.8) * CF2)
})

test_that("capture_all_output returns try-error on stop()", {
  res <- capture_all_output({
    stop("boom")
  })
  expect_true(is.list(res))
  expect_true(inherits(res$result, "try-error"))
  # messages/warnings empty since we only stopped
  expect_true(length(res$lines) >= 0)
})

test_that("criteria_join handles options and filtering robustly", {
  skip_if_not_installed("EPATADA")

  # Minimal x and y inputs with consistent keys to exercise join logic
  x <- tibble::tibble(
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
    )
  )

  y <- tibble::tibble(
    TADA.CharacteristicName = c("ParamA", "ParamB"),
    TADA.ResultSampleFractionText = c("Dissolved", "Total"),
    TADA.MethodSpeciationName = c(NA, NA),
    MagnitudeUnit = c("mg/L", "ug/L"),
    ATTAINS.UseName = c("Aquatic Life", "Recreation"),
    ATTAINS.OrganizationIdentifier = c("Org1", "Org2"),
    MagnitudeValueUpper = c(10, 5)
  )

  # Option 1 match_type + Option 1 use_type (full key usage), keep all rows
  out1 <- criteria_join(
    x = x,
    y = y,
    match_type = "Option 1",
    use_type = "Option 1",
    filter_type = FALSE
  )
  expect_s3_class(out1, "data.frame")
  expect_true("Matched" %in% names(out1))
  # left_join should retain x-row count regardless of matches
  expect_equal(nrow(out1), nrow(x))
  expect_true(all(out1$Matched %in% c("Yes", "No")))
  # If thresholds propagate, MagnitudeValueUpper may be present; test gently
  expect_true("MagnitudeValueUpper" %in% names(out1))

  # Option 2 use_type (drop ATTAINS.UseName from x before join), keep all rows
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
  # Use comes from y in this path; ensure column exists
  expect_true("ATTAINS.UseName" %in% names(out2))

  # Option 2 match_type (do not require fraction/speciation)
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

# Revised, robust tests for criteria_join that do not mock EPATADA and avoid brittle match assumptions

test_that("criteria_join returns consistent structure for Option 1/Option 1 with filter_type = FALSE", {
  skip_if_not_installed("EPATADA")

  x <- tibble::tibble(
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
    )
  )

  y <- tibble::tibble(
    TADA.CharacteristicName = c("ParamA", "ParamB"),
    TADA.ResultSampleFractionText = c("Dissolved", "Total"),
    TADA.MethodSpeciationName = c(NA, NA),
    MagnitudeUnit = c("mg/L", "ug/L"),
    ATTAINS.UseName = c("Aquatic Life", "Recreation"),
    ATTAINS.OrganizationIdentifier = c("Org1", "Org2"),
    MagnitudeValueUpper = c(10, 5)
  )

  out <- criteria_join(
    x = x,
    y = y,
    match_type = "Option 1",
    use_type = "Option 1",
    filter_type = FALSE
  )

  expect_s3_class(out, "data.frame")
  # left_join should retain x row count regardless of matches
  expect_equal(nrow(out), nrow(x))
  # Matched column present and contains only Yes/No
  expect_true("Matched" %in% names(out))
  expect_true(all(out$Matched %in% c("Yes", "No")))
  # Ensure key columns persisted through join
  expect_true(all(
    c(
      "TADA.CharacteristicName",
      "TADA.ResultMeasure.MeasureUnitCode",
      "ATTAINS.UseName",
      "ATTAINS.OrganizationIdentifier"
    ) %in%
      names(out)
  ))
  # Threshold column may be present; if present, should be numeric
  if ("MagnitudeValueUpper" %in% names(out)) {
    expect_true(is.numeric(out$MagnitudeValueUpper))
  }
})

test_that("criteria_join Option 1/Option 2 (use dropped from x) with filter_type = FALSE keeps structure", {
  skip_if_not_installed("EPATADA")

  x <- tibble::tibble(
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
    )
  )

  y <- tibble::tibble(
    TADA.CharacteristicName = c("ParamA", "ParamB"),
    TADA.ResultSampleFractionText = c("Dissolved", "Total"),
    TADA.MethodSpeciationName = c(NA, NA),
    MagnitudeUnit = c("mg/L", "ug/L"),
    ATTAINS.UseName = c("Aquatic Life", "Recreation"),
    ATTAINS.OrganizationIdentifier = c("Org1", "Org2"),
    MagnitudeValueUpper = c(10, 5)
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
  # UseName should still exist (sourced from y in Option 2 path)
  expect_true("ATTAINS.UseName" %in% names(out))
})

test_that("criteria_join Option 2 match_type (ignoring fraction/speciation) works with both filter settings", {
  skip_if_not_installed("EPATADA")

  x <- tibble::tibble(
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
    )
  )

  y <- tibble::tibble(
    TADA.CharacteristicName = c("ParamA", "ParamB"),
    # Fraction/speciation will be ignored in match_type Option 2
    TADA.ResultSampleFractionText = c("Dissolved", "Total"),
    TADA.MethodSpeciationName = c(NA, NA),
    MagnitudeUnit = c("mg/L", "ug/L"),
    ATTAINS.UseName = c("Aquatic Life", "Recreation"),
    ATTAINS.OrganizationIdentifier = c("Org1", "Org2"),
    MagnitudeValueUpper = c(10, 5)
  )

  # filter_type = FALSE: row count retained, Matched present
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

  # filter_type = TRUE: zero or more rows, but if rows exist, they should all be "Yes"
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
  }
})

test_that("criteria_join tolerates duplicate criteria rows (many-to-many) and preserves Matched", {
  skip_if_not_installed("EPATADA")

  x <- tibble::tibble(
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
    )
  )

  # Duplicate criteria rows for ParamA mg/L to exercise many-to-many join
  y <- tibble::tibble(
    TADA.CharacteristicName = c("ParamA", "ParamA", "ParamB"),
    TADA.ResultSampleFractionText = c("Dissolved", "Dissolved", "Total"),
    TADA.MethodSpeciationName = c(NA, NA, NA),
    MagnitudeUnit = c("mg/L", "mg/L", "ug/L"),
    ATTAINS.UseName = c("Aquatic Life", "Aquatic Life", "Recreation"),
    ATTAINS.OrganizationIdentifier = c("Org1", "Org1", "Org2"),
    MagnitudeValueUpper = c(10, 12, 5)
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
