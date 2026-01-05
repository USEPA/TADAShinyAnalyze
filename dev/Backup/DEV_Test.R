# review valid ATTAINS org IDs
ATTAINS_orgs <- rExpertQuery::EQ_DomainValues("org_id")

### Test the four cases from the tool
library(tidyverse)

# Load Examples:
MT_dat <- read_csv("C:/Users/User/Work/TT_Backup2/WY_Tool/TADAShinyAnalyze/dev/Backup/tada.MT.clean.csv")
MLtoAU <- read_csv("C:/Users/User/Work/TT_Backup2/WY_Tool/TADAShinyAnalyze/dev/Backup/MT.AUMLRef.csv")
AUtoUse <- read_csv("C:/Users/User/Work/TT_Backup2/WY_Tool/TADAShinyAnalyze/dev/Backup/MT.UseAURef.csv")


# # Option B
# criteria_B1 <- EPATADA::TADA_DefineCriteriaMethodology(
#   .data = MT_dat,
#   org_id = "MTDEQ",
#   auto_assign = TRUE,
#   # AUMLRef = MLtoAU,
#   # AU_UsesRef = AUtoUse,
#   AUMLRef = NULL,
#   AU_UsesRef = NULL,
#   criteriaMethods = NULL,
#   excel = TRUE,
#   overwrite = TRUE
# )
# 
# criteria_B2 <- EPATADA::TADA_DefineCriteriaMethodology(
#   .data = MT_dat,
#   org_id = "MTDEQ",
#   auto_assign = TRUE,
#   AUMLRef = MLtoAU,
#   AU_UsesRef = AUtoUse,
#   criteriaMethods = NULL,
#   excel = TRUE,
#   overwrite = TRUE
# )

# Option A with TADA_DefineCriteriaMethodology_Shiny
criteria_file_list <- getCriteriaFiles(branch = "main")

temp_table_A <- loadCriteria(state_tribe = "Montana",
                             ref = criteria_file_list)

criteria_A_Excel <- TADA_DefineCriteriaMethodology_Shiny(
  .data = MT_dat,
  org_id =  "MTDEQ",
  AUMLRef = MLtoAU,
  AU_UsesRef = AUtoUse,
  auto_assign = FALSE,
  criteriaMethods = temp_table_A,
  return_workbook = TRUE
)

# Option B with TADA_DefineCriteriaMethodology_Shiny
criteria_B_Excel <- TADA_DefineCriteriaMethodology_Shiny(
  .data = MT_dat,
  org_id = "MTDEQ",
  auto_assign = TRUE,
  criteriaMethods = NULL,
  return_workbook = TRUE
)

# Option C with TADA_DefineCriteriaMethodology_Shiny
criteria_C_Excel <- TADA_DefineCriteriaMethodology_Shiny(
  .data = MT_dat,
  org_id = NULL,
  auto_assign = TRUE,
  criteriaMethods = NULL,
  return_workbook = TRUE
)

criteria_D_Excel <- EPATADA::TADA_DefineCriteriaMethodology(
  excel = TRUE, overwrite = TRUE
)

criteria_D_Excel <- TADA_DefineCriteriaMethodology_Shiny(return_workbook = TRUE)