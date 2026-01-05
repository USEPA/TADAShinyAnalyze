#' Load User Data - Validate ATTAINS.ParameterName
#'
#' Loads a data frame provided by the user.
#' @param data a R data frame. Future dev will allow other data file types.
#' @return A list returning if all ATTAINS.ParameterName are current valid
#' domain values or not. If not, identify which are not valid.
#' @export
#'
#' @examples
#' data("UTAHDWQ")
#' validateATTAINSParam(UTAHDWQ)
#'
validateATTAINSParam <- function(data) {
  # Load or read data if a file path is provided
  if (is.character(data)) {
    # Example: Read CSV
    submitted_data <- utils::read.csv(data)
  } else if (is.data.frame(data)) {
    submitted_data <- data
  } else {
    stop("Input 'data' must be a data frame or a file path.")
  }

  rules_values <- validate::validator(
    toupper(ATTAINS.ParameterName) %in% toupper(spsUtil::quiet(rExpertQuery::EQ_DomainValues("param_name")[, "code"]))
  )

  # Confront data with rules
  out <- validate::confront(submitted_data, rules_values)

  # Generate validation report
  report <- validate::summary(out)

  # Determine acceptance/rejection
  if (all(validate::values(out))) { # Example: All rules passed
    result <- list(status = "Accepted", report = report)
  } else {
    result <- list(status = "Rejected", report = report)
  }

  # display message if accepted vs rejected
  if (result$status == "Accepted") {
    result <- list(status = "Accepted", message = "ATTAINS.ParameterName(s) passed all validation checks.")
  } else {
    result <- list(status = "Rejected", message = "ATTAINS.ParameterName(s) failed some validation checks. Please review the issues.")
  }

  result$issues <- unique(
    data[which(
      !toupper(data[,"ATTAINS.ParameterName"]) %in% 
        toupper(
          spsUtil::quiet(
            rExpertQuery::EQ_DomainValues("param_name")[, "code"])
          )
      ), "ATTAINS.ParameterName"]
    )
  result$nrows_fails <- report$fails
  result$nrows_passes <- report$passes

  return(result)
}



#' Load User Data - Validate WQX Characteristic Names
#'
#' Loads a data frame provided by the user.
#' @param data a R data frame. Future dev will allow other data file types.
#' @return A list returning if all WQX Characteristic Names are current valid
#' domain values or not. If not, identify which are not valid.
#' @export
#'
#' @examples
#' data("UTAHDWQ")
#' validateWQXChar(UTAHDWQ)
#'
validateWQXChar <- function(data) {
  # Load or read data if a file path is provided
  if (is.character(data)) {
    # Example: Read CSV
    submitted_data <- utils::read.csv(data)
  } else if (is.data.frame(data)) {
    submitted_data <- data
  } else {
    stop("Input 'data' must be a data frame or a file path.")
  }

  rules_values <- validate::validator(
    toupper(TADA.CharacteristicName) %in% toupper(utils::read.csv(url("https://cdx.epa.gov/wqx/download/DomainValues/Characteristic.CSV"))[, "Name"])
  )

  # Confront data with rules
  out <- validate::confront(submitted_data, rules_values)

  # Generate validation report
  report <- validate::summary(out)

  # Determine acceptance/rejection
  if (all(validate::values(out))) { # Example: All rules passed
    result <- list(status = "Accepted", report = report)
  } else {
    result <- list(status = "Rejected", report = report)
  }

  # display message if accepted vs rejected
  if (result$status == "Accepted") {
    result <- list(status = "Accepted", message = "WQX.CharacteristicName(s) passed all validation checks.")
  } else {
    result <- list(status = "Rejected", message = "WQX.CharacteristicName(s) failed some validation checks. Please review the issues.")
  }

  # add values to list
  result$issues <- unique(validate::violating(submitted_data, out)[, "TADA.CharacteristicName"])
  result$nrows_fails <- report$fails
  result$nrows_passes <- report$passes

  return(result)
}



#' Load User Data - Validate ATTAINS Use Names
#'
#' Loads a data frame provided by the user.
#' @param data a R data frame. Future dev will allow other data file types.
#' @return A list returning if all ATTAINS Use Names are current valid
#' domain values or not. If not, identify which are not valid.
#' @export
#'
#' @examples
#' \dontrun{
#' validateATTAINSUse(UTAHDWQ)
#' }
#'
validateATTAINSUse <- function(data) {
  # Load or read data if a file path is provided
  if (is.character(data)) {
    # Example: Read CSV
    submitted_data <- utils::read.csv(data)
  } else if (is.data.frame(data)) {
    submitted_data <- data
  } else {
    stop("Input 'data' must be a data frame or a file path.")
  }

  domain <- spsUtil::quiet(rExpertQuery::EQ_DomainValues("use_name")[, "code"])
  
  rules_values <- validate::validator(
    toupper(ATTAINS.UseName) %in% toupper(spsUtil::quiet(rExpertQuery::EQ_DomainValues("use_name")[, "code"]))
  )

  # Confront data with rules
  out <- validate::confront(submitted_data, rules_values)

  # Generate validation report
  report <- validate::summary(out)

  # Determine acceptance/rejection
  if (all(validate::values(out))) { # Example: All rules passed
    result <- list(status = "Accepted", report = report)
  } else {
    result <- list(status = "Rejected", report = report)
  }

  # display message if accepted vs rejected
  if (result$status == "Accepted") {
    result <- list(status = "Accepted", message = "ATTAINS.UseName(s) passed all validation checks.")
  } else {
    result <- list(status = "Rejected", message = "ATTAINS.UseName(s) failed some validation checks. Please review the issues.")
  }

  # add values to list
  result$issues <- data |>
    dplyr::filter(!toupper(ATTAINS.UseName) %in% toupper(domain)) |>
    dplyr::select(ATTAINS.UseName) |>
    dplyr::distinct()
  
  result$nrows_fails <- report$fails
  result$nrows_passes <- report$passes

  return(result)
}



#' Load User Data - Validate ATTAINS Org Names
#'
#' Loads a data frame provided by the user.
#' @param data a R data frame. Future dev will allow other data file types.
#' @return A list returning if all ATTAINS organization names are current valid
#' domain values or not. If not, identify which are not valid.
#' @export
#'
#' @examples
#' data("UTAHDWQ")
#' validateATTAINSOrg(UTAHDWQ)
#'
validateATTAINSOrg <- function(data) {
  # Load or read data if a file path is provided
  if (is.character(data)) {
    # Example: Read CSV
    submitted_data <- utils::read.csv(data)
  } else if (is.data.frame(data)) {
    submitted_data <- data
  } else {
    stop("Input 'data' must be a data frame or a file path.")
  }

  rules_values <- validate::validator(
    toupper(ATTAINS.OrganizationIdentifier) %in% toupper(spsUtil::quiet(rExpertQuery::EQ_DomainValues("org_id")[, "code"]))
  )

  # Confront data with rules
  out <- validate::confront(submitted_data, rules_values)

  # Generate validation report
  report <- validate::summary(out)

  # Determine acceptance/rejection
  if (all(validate::values(out))) { # Example: All rules passed
    result <- list(status = "Accepted", report = report)
  } else {
    result <- list(status = "Rejected", report = report)
  }

  # display message if accepted vs rejected
  if (result$status == "Accepted") {
    result <- list(status = "Accepted", message = "ATTAINS.OrganizationIdentifier(s) passed all validation checks.")
  } else {
    result <- list(status = "Rejected", message = "ATTAINS.OrganizationIdentifier(s) failed some validation checks. Please review the issues.")
  }

  # add values to list
  result$issues <- unique(
    data[which(
      !toupper(data[,"ATTAINS.OrganizationIdentifier"]) %in% 
        toupper(
          spsUtil::quiet(
            rExpertQuery::EQ_DomainValues("org_id")[, "code"])
        )
    ), "ATTAINS.OrganizationIdentifier"]
  )
  result$nrows_fails <- report$fails
  result$nrows_passes <- report$passes

  return(result)
}


#' Load User Data - Validate TADA Magnitude Units
#'
#' Loads a data frame provided by the user.
#' @param data a R data frame. Future dev will allow other data file types.
#' @return A list returning if all Magnitude units are current valid
#' domain values or not. If not, identify which are not valid.
#' @export
#'
#' @examples
#' data("UTAHDWQ")
#' validateWQXUnits(UTAHDWQ)
#'
validateWQXUnits <- function(data) {
  # Load or read data if a file path is provided
  if (is.character(data)) {
    # Example: Read CSV
    submitted_data <- utils::read.csv(data)
  } else if (is.data.frame(data)) {
    submitted_data <- data
  } else {
    stop("Input 'data' must be a data frame or a file path.")
  }
  
  domain <- toupper(
    utils::read.csv(url(
      "https://cdx.epa.gov/wqx/download/DomainValues/MeasureUnit.CSV"
    ))[,"Target.Unit"])
  
  rules_values <- validate::validator(
    toupper(MagnitudeUnit) %in% 
      # read raw csv from url
      toupper(
        utils::read.csv(url(
        "https://cdx.epa.gov/wqx/download/DomainValues/MeasureUnit.CSV"
      ))[,"Target.Unit"]
      )
  )
  
  # Confront data with rules
  out <- validate::confront(submitted_data, rules_values)
  
  # Generate validation report
  report <- validate::summary(out)
  
  # Determine acceptance/rejection
  if (all(validate::values(out))) { # Example: All rules passed
    result <- list(status = "Accepted", report = report)
  } else {
    result <- list(status = "Rejected", report = report)
  }
  
  # display message if accepted vs rejected
  if (result$status == "Accepted") {
    result <- list(status = "Accepted", message = "ATTAINS.OrganizationIdentifier(s) passed all validation checks.")
  } else {
    result <- list(status = "Rejected", message = "ATTAINS.OrganizationIdentifier(s) failed some validation checks. Please review the issues.")
  }
  
  # add values to list
  result$issues <- data |>
    dplyr::filter(!toupper(MagnitudeUnit) %in% toupper(domain)) |>
    dplyr::select(MagnitudeUnit) |>
    dplyr::distinct()
    
  #   unique(
  #   data[which(
  #     !toupper(data[,"MagnitudeUnit"]) %in% 
  #       toupper(
  #         utils::read.csv(url(
  #           "https://cdx.epa.gov/wqx/download/DomainValues/MeasureUnit.CSV"
  #         ))[,"Target.Unit"])
  #   ), "MagnitudeUnit"]
  # )
  result$nrows_fails <- report$fails
  result$nrows_passes <- report$passes
  
  return(result)
}



#' Load User Data - Validate Duration Units
#'
#' Loads a data frame provided by the user.
#' @param data a R data frame. Future dev will allow other data file types.
#' @return A list returning if all Duration units are current valid
#' domain values or not. If not, identify which are not valid.
#' @export
#'
#' @examples
#' data("UTAHDWQ")
#' validateDurationUnits(UTAHDWQ)
#'
validateDurationUnits <- function(data) {
  # Load or read data if a file path is provided
  if (is.character(data)) {
    # Example: Read CSV
    submitted_data <- utils::read.csv(data)
  } else if (is.data.frame(data)) {
    submitted_data <- data
  } else {
    stop("Input 'data' must be a data frame or a file path.")
  }
  
  domain <-
    c(
      "n-hour",
      "n-day",
      "n-week",
      "n-month",
      "n-quarter"
    )
  
  rules_values <- validate::validator(
    toupper(DurationUnit) %in% toupper(c(
      "n-hour",
      "n-day",
      "n-week",
      "n-month",
      "n-quarter"
    ))
  )
  
  # Confront data with rules
  out <- validate::confront(submitted_data, rules_values)
  
  # Generate validation report
  report <- validate::summary(out)
  
  # Determine acceptance/rejection
  if (all(validate::values(out))) { # Example: All rules passed
    result <- list(status = "Accepted", report = report)
  } else {
    result <- list(status = "Rejected", report = report)
  }
  
  # display message if accepted vs rejected
  if (result$status == "Accepted") {
    result <- list(status = "Accepted", message = "DurationUnit(s) passed all validation checks.")
  } else {
    result <- list(status = "Rejected", message = "DurationUnit(s) failed some validation checks. Please review the issues.")
  }
  
  # add values to list
  result$issues <- data |>
    dplyr::filter(!toupper(DurationUnit) %in% toupper(domain)) |>
    dplyr::select(DurationUnit) |>
    dplyr::distinct()
    
  result$nrows_fails <- report$fails
  result$nrows_passes <- report$passes
  
  rm(domain, out, report)
  
  return(result)
}



#' Load User Data - Validate Frequency Methods
#'
#' Loads a data frame provided by the user.
#' @param data a R data frame. Future dev will allow other data file types.
#' @return A list returning if all frequency methods are current valid
#' domain values or not. If not, identify which are not valid.
#' @export
#'
#' @examples
#' data("UTAHDWQ")
#' validateFreqMethod(UTAHDWQ)
#'
validateFreqMethod <- function(data) {
  # Load or read data if a file path is provided
  if (is.character(data)) {
    # Example: Read CSV
    submitted_data <- utils::read.csv(data)
  } else if (is.data.frame(data)) {
    submitted_data <- data
  } else {
    stop("Input 'data' must be a data frame or a file path.")
  }
  
  domain <-
    c(
      "Percent of samples not meeting",
      "percentile",
      "n-samples in 3 years",
      "n-samples in 4 years",
      "n-samples in 5 years",
      "binomial test",
      "NumberNotMeeting"
    )
  
  rules_values <- validate::validator(
    toupper(FreqMethod) %in% toupper(c(
      "Percent of samples not meeting",
      "percentile",
      "n-samples in 3 years",
      "n-samples in 4 years",
      "n-samples in 5 years",
      "binomial test",
      "NumberNotMeeting"
    ))
  )
  
  # Confront data with rules
  out <- validate::confront(submitted_data, rules_values)
  
  # Generate validation report
  report <- validate::summary(out)
  
  # Determine acceptance/rejection
  if (all(validate::values(out))) { # Example: All rules passed
    result <- list(status = "Accepted", report = report)
  } else {
    result <- list(status = "Rejected", report = report)
  }
  
  # display message if accepted vs rejected
  if (result$status == "Accepted") {
    result <- list(status = "Accepted", message = "FreqMethod(s) passed all validation checks.")
  } else {
    result <- list(status = "Rejected", message = "FreqMethod(s) failed some validation checks. Please review the issues.")
  }
  
  # add values to list
  result$issues <- data |>
    dplyr::filter(!toupper(FreqMethod) %in% toupper(domain)) |>
    dplyr::select(FreqMethod) |>
    dplyr::distinct()
  
  result$nrows_fails <- report$fails
  result$nrows_passes <- report$passes
  
  rm(domain, out, report)
  
  return(result)
}



#' Load User Data - Validate Duration Methods
#'
#' Loads a data frame provided by the user.
#' @param data a R data frame. Future dev will allow other data file types.
#' @return A list returning if all Duration Methods are current valid
#' domain values or not. If not, identify which are not valid.
#' @export
#'
#' @examples
#' validateDurationMethod(UTAHDWQ)
#'
validateDurationMethod <- function(data) {
  # Load or read data if a file path is provided
  if (is.character(data)) {
    # Example: Read CSV
    submitted_data <- utils::read.csv(data)
  } else if (is.data.frame(data)) {
    submitted_data <- data
  } else {
    stop("Input 'data' must be a data frame or a file path.")
  }
  
  domain <-
    c(
      "arithmetic mean",
      "arithmetic median",
      "arithmetic max",
      "arithmetic min",
      "geometric mean",
      "rolling geometric mean",
      "rolling arithmetric mean"
    )
  
  rules_values <- validate::validator(
    toupper(DurationMethod) %in% toupper(c(
      "arithmetic mean",
      "arithmetic median",
      "arithmetic max",
      "arithmetic min",
      "geometric mean",
      "rolling geometric mean",
      "rolling arithmetric mean"
    )
    )
  )
  
  # Confront data with rules
  out <- validate::confront(submitted_data, rules_values)
  
  # Generate validation report
  report <- validate::summary(out)
  
  # Determine acceptance/rejection
  if (all(validate::values(out))) { # Example: All rules passed
    result <- list(status = "Accepted", report = report)
  } else {
    result <- list(status = "Rejected", report = report)
  }
  
  # display message if accepted vs rejected
  if (result$status == "Accepted") {
    result <- list(status = "Accepted", message = "DurationMethod(s) passed all validation checks.")
  } else {
    result <- list(status = "Rejected", message = "DurationMethod(s) failed some validation checks. Please review the issues.")
  }
  
  # add values to list
  result$issues <- data |>
    dplyr::filter(!DurationMethod %in% domain) |>
    dplyr::select(DurationMethod) |>
    dplyr::distinct()
  
  result$nrows_fails <- report$fails
  result$nrows_passes <- report$passes
  
  rm(domain, out, report)
  
  return(result)
}



#' Load User Data - Validate Season
#'
#' Loads a data frame provided by the user.
#' @param data a R data frame. Future dev will allow other data file types.
#' @return A list returning if all seasons are current valid
#' domain values or not. If not, identify which are not valid.
#' @export
#'
#' @examples
#' validateSeason(UTAHDWQ)
#'
validateSeason <- function(data) {
  # Load or read data if a file path is provided
  if (is.character(data)) {
    # Example: Read CSV
    submitted_data <- utils::read.csv(data)
  } else if (is.data.frame(data)) {
    submitted_data <- data
  } else {
    stop("Input 'data' must be a data frame or a file path.")
  }
  
  domain <-
    c(
      "Summer",
      "Fall",
      "Spring",
      "Winter"
    )
  
  rules_values <- validate::validator(
    toupper(Season) %in% toupper(c(
      "Summer",
      "Fall",
      "Spring",
      "Winter"
    )
    )
  )
  
  # Confront data with rules
  out <- validate::confront(submitted_data, rules_values)
  
  # Generate validation report
  report <- validate::summary(out)
  
  # Determine acceptance/rejection
  if (all(validate::values(out), na.rm = TRUE)) { # Example: All rules passed
    result <- list(status = "Accepted", report = report)
  } else {
    result <- list(status = "Rejected", report = report)
  }
  
  # display message if accepted vs rejected
  if (result$status == "Accepted") {
    result <- list(status = "Accepted", message = "Season(s) passed all validation checks.")
  } else {
    result <- list(status = "Rejected", message = "Season(s) failed some validation checks. Please review the issues.")
  }
  
  # add values to list
  result$issues <- data |>
    dplyr::filter(!Season %in% domain) |>
    dplyr::select(Season) |>
    dplyr::distinct()
  
  result$nrows_fails <- report$fails
  result$nrows_passes <- report$passes
  
  #rm(domain, out, report)
  
  return(result)
}



#' Validate all data .xlsx in a Folder Path
#'
#' For each criteria tables submitted to a folder path (defaults to those submitted
#' to the inst/extdata folder path of this TADACommunityHub repository) this will
#' validate all criteria table for a single column.
#'
#' @param folder_path The default is "inst/extdata/" to review user submitted criteria
#' table to the TADACommunityHub repository for review.
#'
#' @param validateColumn an R TADACommunityHub validate function. See `validateWQXChar()`,
#' `validateATTAINSParam`, `validateATTAINSUse` and `validateATTAINSOrg`.
#'
#' @return A list of list of what column name contains the current valid
#' domain values or not. If not, identify which are not valid.
#' @export
#'
#' @examples
#' review <- validateAll(validateColumn = validateWQXChar)
#'
validateAll <- function(folder_path = NULL, validateColumn) {
  if (is.null(folder_path)) {
    print("No folder path specified, searching through all files currently found in inst/extdata/")
    folder_path <- system.file("extdata", package = "TADACommunityHub")
  }

  if (is.null(validateColumn)) {
    stop("You must select a column in your criteria and methodology table to validate.")
  }

  file_list <- list.files(path = folder_path, pattern = "\\.xlsx$", full.names = TRUE)

  my_function <- function(x) {
    data <- readxl::read_excel(x)
    do.call(validateColumn, list(data))
  }

  safe_my_function <- purrr::possibly(my_function, otherwise = NULL)

  val_checks <- purrr::map(file_list, safe_my_function)

  names(val_checks) <- gsub("inst/extdata/", "", file_list)
  
  # df_counts <- df |>
  #   mutate(
  #     count_accepted = purrr::map_int(status, ~ sum(.x == "Accepted")),
  #     count_rejected = purrr::map_int(status, ~ sum(.x == "Rejected"))
  #   )
  # 
  # print(df_counts)
  return(val_checks)
}



#' Export data with errors from validateAll
#'
#' Loads the list of unique errors in a column and exports it to df
#' @param data a list of list of multiple data frame that is an output from
#' the TADACommunityHub R validateAll function.
#'
#' @param folder_path The default is "inst/extdata/" to review user submitted criteria
#' table to the TADACommunityHub repository for review.
#'
#' @param excel A boolean value. If TRUE, this will generate an excel spreadheet
#' of all criteria tables in your defined folder to indicate what values not
#' a valid entry in TADA format.
#'
#' @return An excel spreadsheet that shows the invalid column values from the
#' user supplied criteria table(s). Users can choose from a drop down list of
#' allowable valid values for that column name.
#'
#' @export
#'
#' @examples
#' review2 <- validateAll(validateColumn = validateATTAINSUse)
#' err <- exportErrors(review2)
#'
exportErrors <- function(data, folder_path = NULL, excel = FALSE) {
  # Create an empty list to store the dataframes
  list_of_dataframes <- list()

  # Consider flexibility in folder path in future.
  if (is.null(folder_path)) {
    print("No folder path specified, searching through all files currently found in inst/extdata/")
    folder_path <- system.file("extdata", package = "TADACommunityHub")
  }
  file_list <- list.files(path = folder_path, pattern = "\\.xlsx$", full.names = TRUE)

  # Loop through each XLSX file and read it into a dataframe, then add to the list
  for (file_path in file_list) {
    # Extract the file name without extension to use as a list element name
    file_name <- tools::file_path_sans_ext(basename(file_path))

    # Read the Excel file into a dataframe
    df <- readxl::read_excel(file_path)

    # Add the dataframe to the list, using the file name as the element name
    list_of_dataframes[[file_name]] <- df
  }

  errors <- purrr::map(data, ~ .x$issues)

  errors_col <- names(errors[[1]])

  # Filter list of df by errors_col
  list_of_dataframes <- purrr::map(list_of_dataframes, ~ {
    if (errors_col %in% colnames(.x)) {
      unique(.x[, errors_col])
    } else {
      .x[, errors_col] <- NA
    }
    # return(.x)
  })

  # Subset each data frame
  result_list <- errors

  if (excel == TRUE) {
    # 1) openxlsx tab max length is 31 char
    n <- nchar(folder_path) - 11
    names(result_list) <- substr(names(err), 35, nchar(names(err)))
    names(result_list) <- substr(names(result_list), 1, 30)

    downloads_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads")

    file_name <- "my_exported_data.xlsx"

    full_path <- file.path(downloads_path, file_name)

    openxlsx::write.xlsx(result_list, file = full_path)

    # 2. Open the target workbook
    wb <- openxlsx::loadWorkbook(full_path)

    # 3. Get the names of all sheets in the workbook
    sheet_names <- names(wb)

    # 4. Get ATTAINS Parameter domain
    if (errors_col == "ATTAINS.ParameterName") {
      list_values <- as.character(rExpertQuery::EQ_DomainValues(domain = "param_name")[, "code"])
      openxlsx::addWorksheet(wb, "Index", visible = TRUE)
      openxlsx::writeData(
        wb,
        "Index",
        startCol = 1,
        x = list_values
      )
    }

    if (errors_col == "ATTAINS.UseName") {
      list_values <- as.character(rExpertQuery::EQ_DomainValues(domain = "use_name")[, "code"])
      openxlsx::addWorksheet(wb, "Index", visible = TRUE)
      openxlsx::writeData(
        wb,
        "Index",
        startCol = 1,
        x = list_values
      )
    }

    n_sheets <- length(wb$worksheets) - 1
    # m <- ifelse(nrow(result_list[[i]]) == 0, 1, nrow(result_list[[i]]) + 1)

    for (i in 1:n_sheets) {
      if (errors_col == "ATTAINS.ParameterName") {
        openxlsx::writeData(
          wb,
          sheet = sheet_names[i],
          startCol = 2,
          x = "Suggested.ATTAINS.ParameterName"
        )
      }
      if (errors_col == "ATTAINS.UseName") {
        openxlsx::writeData(
          wb,
          sheet = sheet_names[i],
          startCol = 2,
          x = "Suggested.ATTAINS.UseName"
        )
      }

      # openxlsx::conditionalFormatting(
      #   wb,
      #   sheet = sheet_names[i],
      #   cols = 2,
      #   rows = 1:50,
      #   type = "blanks",
      #   style = openxlsx::createStyle(bgFill = "red")
      # )

      openxlsx::conditionalFormatting(
        wb,
        sheet = sheet_names[i],
        cols = 2,
        rows = 1:50,
        type = "notBlanks",
        style = openxlsx::createStyle(bgFill = "green")
      )

      # Apply data validation to the second column (col = 2) for a range of rows
      # For example, rows 2 to 100
      openxlsx::dataValidation(
        wb,
        sheet = sheet_names[i],
        cols = 2,
        rows = 2:1000, # Adjust the row range as needed
        type = "list",
        value = sprintf("'Index'!$A$2:$A$20000")
      )
    }

    openxlsx::saveWorkbook(wb, full_path, overwrite = TRUE)
  }
  return(result_list)
}
