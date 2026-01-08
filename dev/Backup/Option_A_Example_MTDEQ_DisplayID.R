### This is an example using the TADA_DefineCriteriaMethodology with 
### Option A: Using templates available in the TADA Community Hub

### Clear the work space
rm(list = ls())

### Load packages
library(EPATADA)
library(readr)

### Get the example data

# The water quality data from MTDEQ
MT_dat <- read_csv("dev/Backup/Example/Montana/tada_jointoau_output_ts20260108122413/TADAShinyJoinToAU_copy_input_file.csv")
MLtoAU <- read_csv("dev/Backup/Example/Montana/tada_jointoau_output_ts20260108122413/TADAShinyJoinToAU_MLtoAUs_for_review.csv")
AUtoUse <- read_csv("dev/Backup/Example/Montana/tada_jointoau_output_ts20260108122413/TADAShinyJoinToAU_AUtoUses_for_review.csv")

# Option A with Montana
criteria_file_list <- getCriteriaFiles(branch = "main")

temp_table_A <- loadCriteria(state_tribe = "Montana",
                             ref = criteria_file_list)

### Apply the TADA_DefineCriteriaMethodology with temp_table

# Case 1: With AUMLRef and AU_UsesRef
criteria_A_Excel <- TADA_DefineCriteriaMethodology(
  .data = MT_dat,
  org_id =  "MTDEQ",
  auto_assign = FALSE,
  criteriaMethods = temp_table_A,
  AUMLRef = MLtoAU,
  AU_UsesRef = AUtoUse,
  excel = TRUE,
  overwrite = TRUE
)

criteria_A_Excel_Dis <- TADA_DefineCriteriaMethodology(
  .data = MT_dat,
  org_id =  "MTDEQ",
  auto_assign = FALSE,
  criteriaMethods = temp_table_A,
  AUMLRef = MLtoAU,
  AU_UsesRef = AUtoUse,
  displayUniqueId = TRUE,
  excel = TRUE,
  overwrite = TRUE
)



