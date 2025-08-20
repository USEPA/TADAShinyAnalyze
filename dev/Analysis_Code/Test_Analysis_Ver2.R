### This script establishes the analysis workflow

# Clear work space
rm(list = ls())

# Load packages
library(tidyverse)
library(collapse)
library(readxl)
library(tigris)

### Load criteria tables related files

## Load the criteria table
criteria_table <- readxl::read_excel("Data/TADA_Format_Criteria_Table_DRAFT_20250813.xlsx", 
                             sheet = "TADA-Format Criteria",
                             guess_max = 1810)

# Convert the fraction to upper case
criteria_table <- criteria_table |>
  dplyr::mutate(Fraction = toupper(Fraction)) |>
  tidyr::drop_na(TADA.CharacteristicName)

# Get a look-up table of TADA.CharacteristicName and ATTAINS.ParameterName
params <- criteria_table |>
  count(TADA.CharacteristicName, ATTAINS.ParameterName)

params2 <- criteria_table |>
  count(TADA.CharacteristicName, ATTAINS.ParameterName, ATTAINS.OrganizationIdentifier)

## Load equation tables
# Equation based on hardness
hardness_equation <- readxl::read_excel("Data/TADA_Format_Criteria_Table_DRAFT_20250813.xlsx",
                               sheet = "Hardness_eq")

# Equation based on pH and hardness
pH_Hardness_equation <- readxl::read_excel("Data/TADA_Format_Criteria_Table_DRAFT_20250813.xlsx",
                                  sheet = "pH_Hardness_eq")

# Equation based on pH
pH_equation <- readxl::read_excel("Data/TADA_Format_Criteria_Table_DRAFT_20250813.xlsx",
                         sheet = "pH_eq")

# Equation based on pH and Temperature
pH_Temperature_equation <- readxl::read_excel("Data/TADA_Format_Criteria_Table_DRAFT_20250813.xlsx",
                                     sheet = "pH_Temperature_eq")

### Load the example dataset

# The example dataset is based on ND_Little_Muddy.csv
# I used the JoinAU module to find the AU on ND_Little_Muddy.csv

# These three files are assumed to be uploaded by users from the Load tab

# The input file
dat <- readr::read_csv("Data/Example_JoinAU/tada_jointoau_output_ts20250709033312_copy_input_file.csv")

# The AU to Use
AU_Use <- readr::read_csv("Data/Example_JoinAU/tada_jointoau_output_ts20250709033312_autouse_for_review.csv")

# The AU to MLID
AU_MLID <- readr::read_csv("Data/Example_JoinAU/tada_jointoau_output_ts20250709033312_mltoaus_for_review_test.csv") |>
  dplyr::filter(Needs_Review == "No")

# Convert ActivityStartDateTime to dateTime
dat <- dat |>
  dplyr::mutate(ActivityStartDateTime = ymd_hms(ActivityStartDateTime)) |>
  dplyr::mutate(ActivityStartDate = ymd(ActivityStartDate)) |>
  dplyr::mutate(DateTime = ActivityStartDateTime) 

# Remove NA in TADA.ResultMeasureValue
dat <- dat |> drop_na(TADA.ResultMeasureValue)

### Load helper functions
source("Function/pH_fun.R")
source("Function/Temperature_fun.r")
source("Function/Hardness_fun.r")
source("Function/criteria_join_advance.r")
source("Function/Exceedance.r")
source("Function/Exceedance_Summary.r")
source("Function/hardness_eq.r")

### Conduct the analysis

### Step 1: Join the pH, Hardness, and Temperature data
dat2 <- dat |> 
  pH_fun() |>
  Temperature_fun() |>
  hardness_fun()

### Step 2: Filter the criteria table

# This step is based on what users select on the state/tribe

# Assuming users select the state/tribe as default
state_tribe <- "21NDHDWQ"

criteria_table_f1 <- criteria_table |>
  dplyr::filter(ATTAINS.OrganizationIdentifier %in% state_tribe)

# Get the list of available uses from criteria_table_f1
criteria_uses <- unique(criteria_table_f1$ATTAINS.UseName)

AU_Use_uses <- unique(AU_Use$ATTAINS.UseName)

# Find the intersection
available_uses <- base::intersect(criteria_uses, AU_Use_uses)

# The available_uses would contain the available uses for the analysis
# They are the choices in the "Uses to include" seleciton menu

# Assuming users selects all available uses
available_uses_s <- available_uses

### Step 2: Join the criteria table and AU information

# Filter the AU_Use based on available_uses_s
AU_Use_f1 <- AU_Use |>
  dplyr::filter(ATTAINS.UseName %in% available_uses_s)

# Filter the AU_MLID based on AU_Use_f1
AU_MLID_f1 <- AU_MLID |>
  dplyr::filter(JoinToAU.AssessmentUnitIdentifier %in% 
                  AU_Use_f1$JoinToAU.AssessmentUnitIdentifier)

# Filter the input data based on AU_MLID_f1
dat3 <- dat2 |>
  dplyr::filter(TADA.MonitoringLocationIdentifier %in% 
                  AU_MLID_f1$TADA.MonitoringLocationIdentifier)

# Join the criteria_table_f1 and AU_MLID_f1 to dat2
dat4 <- dat3 |>
  dplyr::left_join(AU_MLID_f1) |>
  dplyr::left_join(AU_Use_f1, 
                   by = "JoinToAU.AssessmentUnitIdentifier",
                   relationship = "many-to-many") |>
  criteria_join(criteria_table_f1, match_type = "Option 1", filter_type = FALSE) 

# Select columns
dat4_1 <- dat4 |>
  dplyr::select(
    TADA.MonitoringLocationIdentifier,
    TADA.MonitoringLocationName,
    TADA.LongitudeMeasure,
    TADA.LatitudeMeasure,
    JoinToAU.AssessmentUnitIdentifier,
    ATTAINS.OrganizationIdentifier,
    ATTAINS.ParameterName,
    ATTAINS.UseName,
    AcuteChronic,
    EquationBased,
    Notes2,
    TADA.CharacteristicName,
    TADA.ResultSampleFractionText,
    TADA.MethodSpeciationName,
    TADA.ResultMeasure.MeasureUnitCode,
    TADA.ResultMeasureValue,
    ActivityStartDate,
    DateTime,
    pH,
    Temperature,
    Hardness,
    MagnitudeValueLower,
    MagnitudeValueUpper,
    DurationValue,
    DurationUnit,
    DurationAggregation,
    FrequencyCriteriaValue,
    FrequencyCriteriaMethod,
  )

### Step 3: Separate the dataset based on if criteria exist
dat_na <- dat4_1 |> dplyr::filter(is.na(EquationBased))
dat_yes <- dat4_1 |> dplyr::filter(EquationBased %in% "Yes")
dat_no <- dat4_1 |> dplyr::filter(EquationBased %in% "No")

### Step 4: Compare the dataset that the condition is not based on equation

### TODO Create a list of column names for data frame after the evaluation
dat_no2 <- dat_no |> exceedance_fun()

### Step 5: Compare the dataset that the condition is based on equation

## Hardness
dat_hardness <- dat_yes |>
  dplyr::filter(Notes2 %in% "Hardness")

dat_hardness2 <- dat_hardness |>
  dplyr::left_join(hardness_equation)

# Calculate the criteria
dat_hardness3 <- dat_hardness2 |>
  dplyr::mutate(MagnitudeValueUpper = pmap_dbl(
    list("hardness" = Hardness,
         "CF_A" = CF_A, "CF_B" = CF_B, "CF_C" = CF_C,
         "E_A" = E_A, "E_B" = E_B),
    .f = hardness_eq
  )) |>
  exceedance_fun() |>
  dplyr::select(all_of(names(dat_no2)))

# pH
dat_pH <- dat_yes |>
  dplyr::filter(Notes2 %in% "pH")

dat_pH2 <- dat_pH |>
  dplyr::left_join(pH_equation) |>
  dplyr::mutate(
    MagnitudeValueUpper = purrr::map2_dbl(
            Equation, pH,
            ~ eval(parse(text = .x), envir = list(pH = .y))
          )
  ) |>
  exceedance_fun() |>
  dplyr::select(all_of(names(dat_no2)))

# # Examples
# df <- data.frame(
#   Equation = c(
#     "0.275/(1+10^(7.204-pH)) + 39/(1+10^(pH-7.204))",
#     "0.411/(1+10^(7.204-pH)) + 58.4/(1+10^(pH-7.204))"
#   ),
#   pH = c(7.0, 8.0),
#   stringsAsFactors = FALSE
# )
# 
# df |>
#   dplyr::mutate(
#     result = purrr::map2_dbl(
#       Equation, pH,
#       ~ eval(parse(text = .x), envir = list(pH = .y))
#     )
#   )

# pH and Hardness
dat_pH_hardness <- dat_yes |>
  dplyr::filter(Notes2 %in% "pH and Hardness")

dat_pH_hardness2 <- dat_pH_hardness |>
  dplyr::left_join(pH_Hardness_equation) |>
  dplyr::mutate(MagnitudeValueUpper = pmap_dbl(
    list("hardness" = Hardness,
         "CF_A" = CF_A, "CF_B" = CF_B, "CF_C" = CF_C,
         "E_A" = E_A, "E_B" = E_B),
    .f = hardness_eq
  )) |>
  dplyr::mutate(MagnitudeValueUppe = ifelse(pH < 7, 
                                            min(87, MagnitudeValueUppe),
                                            MagnitudeValueUppe)) |>
  exceedance_fun() |>
  dplyr::select(all_of(names(dat_no2)))

# pH and Temperature
dat_pH_temperature <- dat_yes |>
  dplyr::filter(Notes2 %in% "pH and Temperature") |>
  dplyr::left_join(pH_Temperature_equation) |>
  dplyr::mutate(
    MagnitudeValueUpper = purrr::pmap_dbl(
      list(Equation = Equation, pH = pH, Temperature = Temperature),
      ~ eval(parse(text = .x), envir = list(pH = .y, Temperature = .z))
    )
  ) |>
  exceedance_fun() |>
  dplyr::select(all_of(names(dat_no2)))

# Combine the results from each cases
# Need to make sure all the cases have the same column headers
dat5 <- dplyr::bind_rows(dat_no2, dat_hardness3, dat_pH2, dat_pH_temperature)

# Select relevant columns
dat5_1 <- dat5 |>
  dplyr::select(
    TADA.MonitoringLocationIdentifier,
    TADA.MonitoringLocationName,
    TADA.LongitudeMeasure,
    TADA.LatitudeMeasure,
    JoinToAU.AssessmentUnitIdentifier,
    ATTAINS.OrganizationIdentifier,
    ATTAINS.ParameterName,
    ATTAINS.UseName,
    AcuteChronic,
    TADA.CharacteristicName,
    TADA.ResultSampleFractionText,
    TADA.MethodSpeciationName,
    TADA.ResultMeasure.MeasureUnitCode,
    TADA.ResultMeasureValue,
    ActivityStartDate,
    pH,
    Temperature,
    Hardness,
    MagnitudeValueLower,
    MagnitudeValueUpper,
    DurationValue,
    DurationUnit,
    DurationAggregation,
    FrequencyCriteriaValue,
    FrequencyCriteriaMethod,
    Exceedance
  )

# Save example data
write_csv(dat5, "Example_boxplot_Input.csv", na = "")
write_csv(dat5_1, "Example_boxplot_Input_Simple.csv", na = "")

### Step 6: Summarize the data

# The users can select if they want to see the analysis results as
# 1. MLid
# 2. AU_ind: AU (Individual Sites)
# 3. AU_group: AU (Group Sites)

# Select MLId AU_ind, or AU_group
analysis_unit <- "AU"

dat6 <- dat5_1 |> 
  exceedance_summary(type = analysis_unit)

# dat7 <- map_summary(dat6, type = analysis_unit)

# a <- dat5 %>%
#   distinct( TADA.MonitoringLocationIdentifier,
#             TADA.MonitoringLocationName,
#             JoinToAU.AssessmentUnitIdentifier,
#             ATTAINS.UseName,
#             TADA.LongitudeMeasure,
#             TADA.LatitudeMeasure)
# 
# b <- dat6 %>%
#   left_join(a)

write_csv(dat6, "AU_group_output.csv")


### Frequency Table
frequency_table <- criteria_table |> 
  count(FrequencyCriteriaValue, FrequencyCriteriaMethod) |>
  arrange(desc(n))

### Duration Table
duration_table <- criteria_table |> 
  count(DurationValue, DurationUnit, DurationAggregation) |>
  arrange(desc(n))

### Duration and Frequency Table
duration_frequency_table <- criteria_table |> 
  count(FrequencyCriteriaValue, FrequencyCriteriaMethod, 
        DurationValue, DurationUnit, DurationAggregation) |>
  arrange(desc(n)) %>%
  drop_na(DurationValue)

write_csv(frequency_table, "frequency_table.csv")
write_csv(duration_table, "duration_table.csv")
write_csv(duration_frequency_table, "duration_frequency_table.csv")

duration_frequency_table %>%
  count(DurationAggregation)