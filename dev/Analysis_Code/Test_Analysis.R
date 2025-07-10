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
criteria_table <- read_excel("Data/TADA_Format_Criteria_Table_DRAFT_20250710.xlsx", 
                             sheet = "TADA-Format Criteria")

# Convert the fraction to upper case
criteria_table <- criteria_table |>
  dplyr::mutate(Fraction = toupper(Fraction)) %>%
  drop_na(TADA.CharacteristicName)

## Load equation tables
# Equation based on hardness
hardness_equation <- read_excel("Data/TADA_Format_Criteria_Table_DRAFT_20250710.xlsx",
                               sheet = "Hardness_eq")

# Equation based on pH and hardness
pH_Hardness_equation <- read_excel("Data/TADA_Format_Criteria_Table_DRAFT_20250710.xlsx",
                                  sheet = "pH_Hardness_eq")

# Equation based on pH
pH_equation <- read_excel("Data/TADA_Format_Criteria_Table_DRAFT_20250710.xlsx",
                         sheet = "pH_eq")

# Equation based on pH and Temperature
pH_Temperature_equation <- read_excel("Data/TADA_Format_Criteria_Table_DRAFT_20250710.xlsx",
                                     sheet = "pH_Temperature_eq")

### Load the example dataset

# The example dataset is based on ND_Little_Muddy.csv
# I used the JoinAU module to find the AU on ND_Little_Muddy.csv

# These three files are assumed to be uploaded by users from the Load tab

# The input file
dat <- read_csv("Data/Example_JoinAU/tada_jointoau_output_ts20250709033312_copy_input_file.csv")

# The AU to Use
AU_Use <- read_csv("Data/Example_JoinAU/tada_jointoau_output_ts20250709033312_autouse_for_review.csv")

# The AU to MLID
AU_MLID <- read_csv("Data/Example_JoinAU/tada_jointoau_output_ts20250709033312_mltoaus_for_review.csv")

# Convert ActivityStartDateTime to dateTime
dat <- dat |>
  dplyr::mutate(ActivityStartDateTime = ymd_hms(ActivityStartDateTime)) |>
  dplyr::mutate(ActivityStartDate = ymd(ActivityStartDate)) |>
  dplyr::mutate(DateTime = ActivityStartDateTime)

### Load helper functions
source("Function/pH_fun.R")
source("Function/Temperature_fun.r")
source("Function/Hardness_fun.r")
source("Function/criteria_join.r")
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
  dplyr::filter(JoinToAU.AssessmentUnitIdentifier %in% AU_Use_f1$JoinToAU.AssessmentUnitIdentifier)

# Filter the input data based on AU_MLID_f1
dat2 <- dat |>
  dplyr::filter(MonitoringLocationIdentifier %in% AU_MLID_f1$MonitoringLocationIdentifier)

# Join the criteria_table_f1 and AU_MLID_f1 to dat2
dat3 <- dat2 |>
  left_join(AU_MLID_f1) |>
  left_join(AU_Use_f1, 
            by = "JoinToAU.AssessmentUnitIdentifier",
            relationship = "many-to-many") |>
  criteria_join(criteria_table_f1) 

### Step 3: Separate the dataset based on if criteria exist
dat_na <- dat3 |> dplyr::filter(is.na(EquationBased))
dat_yes <- dat3 |> dplyr::filter(EquationBased %in% "Yes")
dat_no <- dat3 |> dplyr::filter(EquationBased %in% "No")

### Step 4: Compare the dataset that the condition is not based on equation
dat_no2 <- dat_no |> exceedance_fun()

### Step 5: Compare the dataset that the condition is based on equation

# # Hardness 
# dat_hardness <- dat_yes |> 
#   dplyr::filter(Notes2 %in% "Hardness")
# 
# dat_hardness2 <- dat_hardness |>
#   dplyr::left_join(hardness_equation)
# 
# # Calculate the criteria
# dat_hardness3 <- dat_hardness2 |>
#   dplyr::mutate(MagnitudeValueUpper = pmap_dbl(
#     list("hardness" = Hardness, 
#          "CF_A" = CF_A, "CF_B" = CF_B, "CF_C" = CF_C,
#          "E_A" = E_A, "E_B" = E_B),
#     .f = hardness_eq
#   )) |> 
#   exceedance_fun()

# # pH
# dat_pH <- dat_yes |> 
#   dplyr::filter(Notes2 %in% "pH")
# 
# # pH and  Hardness
# dat_pH_hardness <- dat_yes |> 
#   dplyr::filter(Notes2 %in% "pH and Hardness")
# 
# # pH and Temperature
# dat_pH_temperature <- dat_yes |> 
#   dplyr::filter(Notes %in% "pH and Temperature")

# Combine the results from each cases
# Need to make sure all the cases have the same column headers
dat4 <- dplyr::bind_rows(dat_no2)

### Step 6: Summarize the data

# The users can select if they want to see the analysis results as
# 1. MLid
# 2. AU_ind: AU (Individual Sites)
# 3. AU_group: AU (Group Sites)

# Select MLId AU_ind, or AU_group
analysis_unit <- "MLId"

dat5 <- dat4 |> 
  exceedance_summary(type = analysis_unit)