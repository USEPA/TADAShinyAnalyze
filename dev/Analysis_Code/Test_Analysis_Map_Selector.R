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
criteria_table <- readxl::read_excel("Data/TADA_Format_Criteria_Table_DRAFT_20250729.xlsx", 
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
hardness_equation <- readxl::read_excel("Data/TADA_Format_Criteria_Table_DRAFT_20250729.xlsx",
                               sheet = "Hardness_eq")

# Equation based on pH and hardness
pH_Hardness_equation <- readxl::read_excel("Data/TADA_Format_Criteria_Table_DRAFT_20250729.xlsx",
                                  sheet = "pH_Hardness_eq")

# Equation based on pH
pH_equation <- readxl::read_excel("Data/TADA_Format_Criteria_Table_DRAFT_20250729.xlsx",
                         sheet = "pH_eq")

# Equation based on pH and Temperature
pH_Temperature_equation <- readxl::read_excel("Data/TADA_Format_Criteria_Table_DRAFT_20250729.xlsx",
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
                  AU_Use_f1$JoinToAU.AssessmentUnitIdentifier) |>
  dplyr::select(TADA.MonitoringLocationIdentifier, TADA.LongitudeMeasure,
                TADA.LatitudeMeasure, JoinToAU.AssessmentUnitIdentifier)

# Filter the input data based on AU_MLID_f1
dat3 <- dat2 |>
  dplyr::filter(TADA.MonitoringLocationIdentifier %in% 
                  AU_MLID_f1$TADA.MonitoringLocationIdentifier)

dat4 <- dat3 |>
  distinct(TADA.MonitoringLocationIdentifier,
           TADA.MonitoringLocationName,
           TADA.MonitoringLocationTypeName,
           TADA.LongitudeMeasure,
           TADA.LatitudeMeasure)

# Join the criteria_table_f1 and AU_MLID_f1 to dat2
dat5 <- dat4 |>
  dplyr::left_join(AU_MLID_f1) 

dat6 <- dat5 |>
  dplyr::left_join(AU_Use_f1, 
                   by = "JoinToAU.AssessmentUnitIdentifier",
                   relationship = "many-to-many") 
