### This script establishes the analysis workflow

# Clear work space
rm(list = ls())

# Load packages
library(tidyverse)
library(readxl)

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

# Save the work space
save.image("Data/Criteria_Table_Input.RData")