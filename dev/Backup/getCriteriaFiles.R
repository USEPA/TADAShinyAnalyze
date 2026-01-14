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
      
      return(data.frame(
        display_name = display_name,
        file_name = file$name,
        download_url = file$download_url,
        stringsAsFactors = FALSE
      ))
    }
    return(NULL)
  })
  
  # Combine into a data frame
  result <- do.call(rbind, Filter(Negate(is.null), criteria_files))
  
  if (is.null(result) || nrow(result) == 0) {
    warning("No criteria crosswalk files found in the repository.")
    return(data.frame(
      display_name = character(),
      file_name = character(),
      download_url = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  return(result)
}