### This is an example using the TADA_DefineCriteriaMethodology with 
### Option A: Using templates available in the TADA Community Hub

### Clear the work space
rm(list = ls())

### Load packages
library(EPATADA)

### Get the example data

# The water quality data from MTDEQ
utils::data("Data_MT_MissoulaCounty", package = "EPATADA")
tada.MT.clean <- Data_MT_MissoulaCounty
rm(Data_MT_MissoulaCounty)

# The AUMLRef
utils::data("Data_MT_AUMLRef", package = "EPATADA")
MT.AUMLRef <- Data_MT_AUMLRef
rm(Data_MT_AUMLRef)

# Create the AU_UsesRef
MT.UseAURef <- TADA_AssignUsesToAU(
  AUMLRef = MT.AUMLRef$ATTAINS_crosswalk,
  org_id = "MTDEQ"
)

### Apply the TADA_DefineCriteriaMethodology with temp_table

# Case 1: With AUMLRef and AU_UsesRef
criteria_B_Excel <- TADA_DefineCriteriaMethodology(
  .data = tada.MT.clean,
  org_id =  "MTDEQ",
  auto_assign = TRUE,
  criteriaMethods = NULL,
  AUMLRef = MT.AUMLRef$ATTAINS_crosswalk,
  AU_UsesRef = MT.UseAURef,
  excel = TRUE,
  overwrite = TRUE
)



