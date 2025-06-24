### This script establishes the analysis workflow

# Clear work space
rm(list = ls())

# Load packages
library(tidyverse)
library(collapse)
library(readxl)
library(tigris)

### Preprocessing

# Load the criteria table
criteria_table <- read_excel("Data/TADA_Format_Criteria_Table_DRAFT_20250623_ycw.xlsx")

# Convert the fraction to upper case
criteria_table <- criteria_table |>
  dplyr::mutate(Fraction = toupper(Fraction))

# Load the example dataset
dat <- readRDS("Data/ND_Little_Muddy.rds")

# Convert ActivityStartDateTime to dateTime
dat <- dat |>
  dplyr::mutate(ActivityStartDateTime = ymd_hms(ActivityStartDateTime)) |>
  dplyr::mutate(ActivityStartDate = ymd(ActivityStartDate)) |>
  dplyr::mutate(DateTime = ActivityStartDateTime)

# Change the unit of PH to be NONE
dat <- dat |>
  dplyr::mutate(TADA.ResultDepthHeightMeasure.MeasureUnitCode = ifelse(
    TADA.CharacteristicName %in% "PH", 
    "NONE", 
    TADA.ResultDepthHeightMeasure.MeasureUnitCode
  ))

# Load the state code and name table
state_dat <- read_csv("Data/state_code.csv")
state_code <- state_dat |>
  dplyr::select(StateCode = GEOID,
                StateName = STUSPS)

# Join state_code to dat
dat <- dat |> dplyr::left_join(state_code, by = "StateCode")

# Load functions
source("Function/pH_fun.R")
source("Function/Temperature_fun.r")
source("Function/Hardness_fun.r")
source("Function/criteria_join.r")

### Conduct the analysis

### Step 1: Join the pH, Hardness, and Temperature data
dat2 <- dat |> 
  pH_fun() |>
  Temperature_fun() |>
  hardness_fun()
  

### Step 2: Join the criteria table
dat3 <- dat2 |> criteria_join(criteria_table)

### Step 3: Separate the dataset based on if criteria exist
dat_na <- dat3 |> dplyr::filter(is.na(EquationBased))
dat_yes <- dat3 |> dplyr::filter(EquationBased %in% "Yes")
dat_no <- dat3 |> dplyr::filter(EquationBased %in% "No")

# For none equation-based dataset, separate the dataset into magnitue upper, lower,
# and both

dat_no_upper <- dat_no |> 
  dplyr::filter(is.na(MagnitudeValueLower) & !is.na(MagnitudeValueUpper))

dat_no_lower <- dat_no |> 
  dplyr::filter(!is.na(MagnitudeValueLower) & is.na(MagnitudeValueUpper))

dat_no_between <- dat_no |> 
  dplyr::filter(!is.na(MagnitudeValueLower) & !is.na(MagnitudeValueUpper))

# For equation-based dataset, calculate the threshold 




