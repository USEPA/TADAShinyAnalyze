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
    bind_rows(tibble::tibble(
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

test_that("duration_cal produces windowed statistics", {
  skip_if_not_installed("slider")
  # Start from a simple daily-aggregated series
  x <- tibble::tibble(
    TADA.MonitoringLocationIdentifier = rep("S1", 5),
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
    DurationUnit = rep("n-day", 5),
    DurationMethod = rep("Arithmetic Mean", 5),
    DurationValue = rep(3, 5),
    FreqValue = rep(0, 5),
    FreqMethod = rep("NumberNotMeeting", 5),
    EquationType = NA_character_,
    ActivityStartDate = as.Date("2020-01-01") + 0:4,
    DateTime = as.POSIXct("2020-01-01 00:00:00", tz = "UTC") + (0:4) * 86400,
    Value = c(1, 2, 3, 4, 5),
    MagnitudeValueLower = rep(NA_real_, 5),
    MagnitudeValueUpper = rep(4, 5), # threshold to trigger exceed when mean > 4
    pH = NA_real_,
    Temperature = NA_real_,
    Hardness = NA_real_
  )

  dur <- duration_cal(x, type = "MLid", complete_windows = FALSE)
  expect_true(all(
    c(
      "Result_Duration",
      "Window_Start_win",
      "Window_End_win",
      "N_in_Window"
    ) %in%
      names(dur)
  ))

  # The 3-day rolling mean at last point should be mean(3,4,5) = 4
  # Because complete_windows = FALSE, edge windows are allowed
  last_row <- tail(dur, 1)
  expect_equal(round(last_row$Result_Duration, 6), 4)
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
