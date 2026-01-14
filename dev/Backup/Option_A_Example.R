### This is an example using the TADA_DefineCriteriaMethodology with 
### Option A: Using templates available in the TADA Community Hub

### Clear the work space
rm(list = ls())

### Load packages
library(EPATADA)

### Create helper functions

# A function to download the criteria file list from TADACommunityHub
getCriteriaFiles <- function(branch = "main") {
  # GitHub API endpoint for repository contents
  api_url <- sprintf(
    "https://api.github.com/repos/USEPA/TADACommunityHub/contents/inst/extdata?ref=%s",
    branch
  )
  
  # Make the API request
  response <- httr::GET(api_url)
  
  
  # Check for errors
  if (httr::status_code(response) != 200) {
    stop("Failed to fetch file list from GitHub. Status code: ",
         httr::status_code(response))
  }
  
  # Parse JSON response
  content <- httr::content(response, as = "parsed")
  
  # Filter for xlsx files with "_criteria_crosswalk" pattern
  criteria_files <- lapply(content, function(file) {
    if (grepl("_criteria_crosswalk\\.xlsx$", file$name)) {
      # Extract display name by removing the suffix
      display_name <- gsub("_criteria_crosswalk\\.xlsx$", "", file$name)
      # Convert underscores to spaces and title case for nicer display
      display_name <- gsub("_", " ", display_name)
      display_name <- tools::toTitleCase(display_name)
      
      output <- data.frame(
        display_name = display_name,
        file_name = file$name,
        download_url = file$download_url,
        stringsAsFactors = FALSE
      )
      
      return(output)
    }
    return(NULL)
  })
  
  # Combine into a data frame
  result <- dplyr::bind_rows(criteria_files)
  
  if (is.null(result) || nrow(result) == 0) {
    warning("No criteria crosswalk files found in the repository.")
    
    result <- data.frame(
      display_name = character(),
      file_name = character(),
      download_url = character(),
      stringsAsFactors = FALSE
    )
    
    return(result)
  }
  
  return(result)
}

# A functin to load the criteria list
loadCriteria <- function(state_tribe, ref) {
  
  # Get the file_url
  file_url <- ref[ref$display_name %in% state_tribe, "download_url"]
  
  # Create a temporary file
  temp_file <- tempfile(fileext = ".xlsx")
  
  # Download the file (use mode = "wb" for binary files like xlsx)
  utils::download.file(file_url, temp_file, mode = "wb")
  
  # Now read it
  df <- readxl::read_excel(temp_file)
  
  # Clean up
  unlink(temp_file)
  
  return(df)
}

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

# Get the list of criteria table
criteria_file_list <- getCriteriaFiles()
# Get the MTDEQ criteria table from the TADACommunityHub
temp_table <- loadCriteria(state_tribe = "Montana", ref = criteria_file_list)

### Apply the TADA_DefineCriteriaMethodology with temp_table

# Case 1: With AUMLRef and AU_UsesRef
criteria_A_Excel <- TADA_DefineCriteriaMethodology(
  .data = tada.MT.clean,
  org_id =  "MTDEQ",
  auto_assign = FALSE,
  criteriaMethods = temp_table,
  AUMLRef = MT.AUMLRef$ATTAINS_crosswalk,
  AU_UsesRef = MT.UseAURef,
  excel = TRUE,
  overwrite = TRUE
) |> suppressWarnings()

# Error in TADA_DefineCriteriaMethodology(.data = tada.MT.clean, org_id = "MTDEQ",  : 
#                                           (converted from warning) Your user supplied criteriaMethods file is missing 1 unique TADA.ComparableDataIdentifier(s) : 
#                                           PH without an ATTAINS.ParameterName crosswalk. Please review these entries in your crosswalk or remove them/leave them unfilled if not applicable to analysis.

# Case 2: With AUMLRef and AU_UsesRef as NULL
criteria_A_Excel <- TADA_DefineCriteriaMethodology(
  .data = tada.MT.clean,
  org_id =  "MTDEQ",
  auto_assign = FALSE,
  criteriaMethods = temp_table,
  AUMLRef = NULL,
  AU_UsesRef = NULL,
  excel = TRUE,
  overwrite = TRUE
)

# Error in TADA_DefineCriteriaMethodology(.data = tada.MT.clean, org_id = "MTDEQ",  : 
#                                           (converted from warning) Your user supplied criteriaMethods file is missing 1 unique TADA.ComparableDataIdentifier(s) : 
#                                           PH without an ATTAINS.ParameterName crosswalk. Please review these entries in your crosswalk or remove them/leave them unfilled if not applicable to analysis.


a <- TADA_DefineCriteriaMethodology(excel = TRUE, overwrite = TRUE)


