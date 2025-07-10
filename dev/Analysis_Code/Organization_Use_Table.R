### This script creates a look-up table for the organization and use

# Clear work space
rm(list = ls())

# Load packages
library(tidyverse)
library(collapse)
library(readxl)
library(tigris)

criteria_table <- read_excel("Data/TADA_Format_Criteria_Table_DRAFT_20250702.xlsx", 
                             sheet = "TADA-Format Criteria")

dat <- criteria_table |>
  count(ATTAINS.OrganizationIdentifier, ATTAINS.UseName)

write_csv(dat, "Data/Organization_Use.csv")

rm(criteria_table)
rm(dat)
org_use <- read_excel("Data/Organization_Use.xlsx")

save.image("Data/Organization_Use.Rdata")


