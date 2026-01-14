TADA_DefineCriteriaMethodology_Shiny <- function (.data, 
                                                  org_id = NULL, 
                                                  MLSummaryRef = NULL, 
                                                  criteriaMethods = NULL, 
                                                  auto_assign = FALSE, 
                                                  AUMLRef = NULL, 
                                                  AU_UsesRef = NULL, 
                                                  epa304a = FALSE, 
                                                  displayUniqueId = FALSE, 
                                                  return_workbook = FALSE) 
{
  desired_cols <- c("ATTAINS.OrganizationIdentifier", "ATTAINS.ParameterName", 
                    "ATTAINS.UseName", "TADA.ComparableDataIdentifier", "TADA.CharacteristicName", 
                    "TADA.ResultSampleFractionText", "TADA.MethodSpeciationName", 
                    "ATTAINS.WaterType", "SaltFresh", "DepthCategory", "UniqueSpatialCriteria", 
                    "AcuteChronic", "EquationBased", "MagnitudeValueLower", 
                    "MagnitudeValueUpper", "MagnitudeUnit", "DurationValue", 
                    "DurationUnit", "DurationMethod", "FreqValue", "FreqMethod", 
                    "AssessPeriod", "AssessPeriodStartDate", "AssessPeriodEndDate", 
                    "Season", "SeasonStartDate", "SeasonEndDate", "DistrCount", 
                    "DistrPeriod", "DistrMinSample", "Notes")
  if (missing(.data) && missing(org_id) && missing(MLSummaryRef) && 
      missing(criteriaMethods) && missing(AUMLRef) && missing(AU_UsesRef)) {
    message("All arguments are blank, returning an empty dataframe with column names only.")
    DefineCriteriaMethodology <- data.frame(matrix(ncol = length(desired_cols), 
                                                   nrow = 0))
    names(DefineCriteriaMethodology) <- desired_cols
    DefineCriteriaMethodology <- EPATADA:::correctColType(DefineCriteriaMethodology)
    
    # Return based on return_workbook flag
    if (return_workbook) {
      wb <- create_criteria_workbook(DefineCriteriaMethodology, org_id, MLSummaryRef, .data)
      return(list(data = DefineCriteriaMethodology, workbook = wb))
    } else {
      return(DefineCriteriaMethodology)
    }
    
  }
  else {
    if (!is.logical(auto_assign)) {
      stop("TADA_DefineCriteriaMethodology: auto_assign must be a boolean (TRUE/FALSE) value.")
    }
    if (auto_assign == TRUE && !is.null(criteriaMethods)) {
      stop("TADA_DefineCriteriaMethodology: criteriaMethodology is provided and auto_assign = TRUE are not valid function argument input combinations.")
    }
    if (!is.null(MLSummaryRef) && !is.null(criteriaMethods)) {
      stop("TADA_DefineCriteriaMethodology: MLSummaryRef and criteriaMethods are both provided. You can only proceed with one (or none) of these options provided.")
    }
    if (!is.null(MLSummaryRef) && auto_assign == TRUE) {
      stop("TADA_DefineCriteriaMethodology: MLSummaryRef is provided and auto_assign = TRUE are not valid function argument input combinations.")
    }
    if (!is.character(org_id) & is.null(org_id)) {
      org_id <- ""
    }
    if (tolower("all") %in% tolower(org_id)) {
      if (is.null(AUMLRef)) {
        print(paste0("org_id == 'All' was selected, ", 
                     "No AUMLRef was provided. Returning all unique ATTAINS.OrganizationIdentifiers found as an ATTAINS organization identifier domain value."))
        org_id <- rExpertQuery::EQ_DomainValues("org_id")[, 
                                                          "code"]
      }
      if (!is.null(AUMLRef)) {
        print(paste0("org_id == 'All' was selected, ", 
                     "An AUMLRef was provided. Returning all unique ATTAINS.OrganizationIdentifiers found as an ATTAINS organization identifier in your AUMLRef."))
        org_id <- unique(stats::na.omit(AUMLRef$ATTAINS.OrganizationIdentifier))
      }
    }
    if (auto_assign == FALSE && is.null(MLSummaryRef) && 
        is.null(criteriaMethods)) {
      suppressMessages(TADA_ParamRef <- EPATADA::TADA_ParametersForAnalysis(.data = .data, 
                                                                   org_id = org_id))
      suppressWarnings(TADA_usesRef <- EPATADA::TADA_UsesForAnalysis(.data, 
                                                            paramRef = TADA_ParamRef, org_id = org_id))
      suppressMessages(MLSummaryRef <- EPATADA::TADA_MLSummary(.data, 
                                                      usesRef = TADA_usesRef, org_id = org_id))
    }
    if (auto_assign == TRUE) {
      print(paste0("auto_assign = TRUE selected. Running EPATADA::TADA_ParametersForAnalysis with default assignment."))
      suppressMessages(TADA_ParamRef <- EPATADA::TADA_ParametersForAnalysis(.data, 
                                                                   org_id = org_id, auto_assign = "Org"))
      print(paste0("auto_assign = TRUE selected. Running EPATADA::TADA_UsesForAnalysis with default assignment."))
      suppressWarnings(TADA_usesRef <- EPATADA::TADA_UsesForAnalysis(.data, 
                                                            org_id = org_id, paramRef = TADA_ParamRef, auto_assign = TRUE))
      print(paste0("auto_assign = TRUE selected. Running EPATADA::TADA_MLSummary with default assignment."))
      suppressMessages(MLSummaryRef <- EPATADA::TADA_MLSummary(.data, 
                                                      displayNA = TRUE, org_id = org_id, usesRef = TADA_usesRef, 
                                                      AUMLRef = AUMLRef, AU_UsesRef = AU_UsesRef))
      unique_param <- unique(.data$TADA.CharacteristicName)
      TADA_param <- dplyr::filter(tidyr::complete(dplyr::mutate(dplyr::distinct(.data[, 
                                                                                      c("TADA.ComparableDataIdentifier"), drop = FALSE]), 
                                                                ATTAINS.OrganizationIdentifier = NA_character_), 
                                                  TADA.ComparableDataIdentifier, ATTAINS.OrganizationIdentifier = org_id), 
                                  !is.na(ATTAINS.OrganizationIdentifier))
      MLSummaryRef <- dplyr::full_join(TADA_param, MLSummaryRef, 
                                       by = names(TADA_param))
    }
    if (!is.null(MLSummaryRef) & !is.character(MLSummaryRef)) {
      if (!is.data.frame(MLSummaryRef)) {
        stop("TADA_DefineCriteriaMethodology: 'MLSummaryRef' must be a data frame with six columns:\n          ATTAINS.ParameterName, ATTAINS.UseName, ATTAINS.OrganizationIdentifier, UniqueSpatialCriteria,\n          ATTAINS.WaterType, ATTAINS.AssessmentUnitIdentifier")
      }
      if (is.data.frame(MLSummaryRef)) {
        col.names <- c("ATTAINS.ParameterName", "ATTAINS.UseName", 
                       "ATTAINS.OrganizationIdentifier", "UniqueSpatialCriteria", 
                       "ATTAINS.WaterType", "ATTAINS.AssessmentUnitIdentifier")
        ref.names <- names(MLSummaryRef)
        if (length(setdiff(col.names, ref.names)) > 0) {
          stop("TADA_DefineCriteriaMethodology: 'MLSummaryRef' must be a data frame with six columns:\n          ATTAINS.ParameterName, ATTAINS.UseName, ATTAINS.OrganizationIdentifier, UniqueSpatialCriteria,\n          ATTAINS.WaterType, ATTAINS.AssessmentUnitIdentifier")
        }
      }
    }
    if (!is.null(MLSummaryRef)) {
      MLSummaryRef <- EPATADA:::correctColType(MLSummaryRef)
      MLSummaryRef <- dplyr::right_join(MLSummaryRef, dplyr::distinct(.data[, 
                                                                            c("TADA.ComparableDataIdentifier", "TADA.CharacteristicName")]), 
                                        by = "TADA.ComparableDataIdentifier")
      DefineCriteriaMethodology <- dplyr::distinct(dplyr::arrange(dplyr::select(dplyr::bind_cols(dplyr::mutate(dplyr::mutate(dplyr::mutate(dplyr::select(MLSummaryRef, 
                                                                                                                                                         "ATTAINS.OrganizationIdentifier", "ATTAINS.ParameterName", 
                                                                                                                                                         "ATTAINS.UseName", "TADA.ComparableDataIdentifier", 
                                                                                                                                                         "TADA.CharacteristicName", "SaltFresh", "DepthCategory", 
                                                                                                                                                         "UniqueSpatialCriteria", "ATTAINS.WaterType"), 
                                                                                                                                           ATTAINS.WaterType = dplyr::if_else(is.na(UniqueSpatialCriteria), 
                                                                                                                                                                              as.character(NA), as.character(ATTAINS.WaterType))), 
                                                                                                                             SaltFresh = dplyr::if_else(is.na(UniqueSpatialCriteria), 
                                                                                                                                                        as.character(NA), as.character(SaltFresh))), 
                                                                                                               DepthCategory = dplyr::if_else(is.na(UniqueSpatialCriteria), 
                                                                                                                                              as.character(NA), as.character(DepthCategory))), 
                                                                                                 data.frame(TADA.ResultSampleFractionText = as.character(NA), 
                                                                                                            TADA.MethodSpeciationName = as.character(NA), 
                                                                                                            AcuteChronic = as.character(NA), EquationBased = as.character(NA), 
                                                                                                            MagnitudeValueLower = as.numeric(NA), MagnitudeValueUpper = as.numeric(NA), 
                                                                                                            MagnitudeUnit = as.character(NA), DurationValue = as.numeric(NA), 
                                                                                                            DurationUnit = as.character(NA), DurationMethod = as.character(NA), 
                                                                                                            FreqValue = as.numeric(NA), FreqMethod = as.character(NA), 
                                                                                                            AssessPeriod = as.character(NA), AssessPeriodStartDate = as.Date(NA), 
                                                                                                            AssessPeriodEndDate = as.Date(NA), Season = as.character(NA), 
                                                                                                            SeasonStartDate = as.Date(NA), SeasonEndDate = as.Date(NA), 
                                                                                                            DistrCount = as.numeric(NA), DistrPeriod = as.character(NA), 
                                                                                                            DistrMinSample = as.numeric(NA), Notes = as.character(NA))), 
                                                                                desired_cols), ATTAINS.UseName))
      DefineCriteriaMethodology <- EPATADA:::correctColType(DefineCriteriaMethodology)
    }
    if (!is.null(criteriaMethods)) {
      if ("" %in% org_id) {
        criteriaMethods$ATTAINS.OrganizationIdentifier <- ""
      }
      criteriaMethods$ATTAINS.ParameterName <- toupper(criteriaMethods$ATTAINS.ParameterName)
      unique_param <- unique(.data$TADA.CharacteristicName)
      TADA_param <- tidyr::uncount(dplyr::distinct(.data[, 
                                                         c("TADA.CharacteristicName", "TADA.ComparableDataIdentifier")]), 
                                   weights = length(org_id))
      TADA_param <- dplyr::mutate(TADA_param, ATTAINS.OrganizationIdentifier = as.character(rep(org_id, 
                                                                                                nrow(TADA_param)/length(org_id))))
      criteriaMethods <- dplyr::filter(dplyr::full_join(dplyr::select(criteriaMethods, 
                                                                      -TADA.ComparableDataIdentifier), TADA_param, 
                                                        by = c("ATTAINS.OrganizationIdentifier", "TADA.CharacteristicName")), 
                                       ATTAINS.OrganizationIdentifier %in% org_id)
      missing_cols <- setdiff(desired_cols, names(criteriaMethods))
      if (length(missing_cols) > 0) {
        for (col in missing_cols) {
          criteriaMethods <- dplyr::mutate(criteriaMethods, 
                                           `:=`(!!col, NA))
        }
      }
      non_definedCriteria <- as.data.frame(dplyr::select(dplyr::filter(dplyr::filter(criteriaMethods, 
                                                                                     is.na(ATTAINS.ParameterName)), TADA.CharacteristicName %in% 
                                                                         unique_param), dplyr::all_of(desired_cols)))
      if (nrow(non_definedCriteria) > 0 && displayUniqueId == 
          TRUE) {
        warning(paste("Your user supplied criteriaMethods file is missing", 
                      length(unique(non_definedCriteria$TADA.ComparableDataIdentifier)), 
                      "unique TADA.ComparableDataIdentifier(s)", 
                      ": \n", paste0(unique(non_definedCriteria$TADA.ComparableDataIdentifier), 
                                     collapse = ", "), "without an ATTAINS.ParameterName crosswalk.", 
                      "Please review these entries in your crosswalk or remove them/leave them unfilled if not applicable to analysis."))
      }
      if (nrow(non_definedCriteria) > 0 && displayUniqueId == 
          FALSE) {
        warning(paste("Your user supplied criteriaMethods file is missing", 
                      length(unique(non_definedCriteria$TADA.CharacteristicName)), 
                      "unique TADA.ComparableDataIdentifier(s)", 
                      ": \n", paste0(unique(non_definedCriteria$TADA.CharacteristicName), 
                                     collapse = ", "), "without an ATTAINS.ParameterName crosswalk.", 
                      "Please review these entries in your crosswalk or remove them/leave them unfilled if not applicable to analysis."))
      }
      if (auto_assign == TRUE & is.null(AU_UsesRef)) {
        warning(paste0("You selected auto_assign == TRUE. No AU_UsesRef was provided. ", 
                       "Filling in these blanks with ATTAINS.ParameterName and ATTAINS.UseName pulled in from the prior ATTAINS Assessment Cycle. ", 
                       "Please review or edit these entries in your crosswalk or remove them/leave them unfilled if not applicable to analysis."))
      }
      if (auto_assign == TRUE & !is.null(AU_UsesRef)) {
        warning(paste0("You selected auto_assign == TRUE. An AU_UsesRef was provided. ", 
                       "Filling in these blanks with ATTAINS.ParameterName and ATTAINS.UseName pulled in from your AU_UsesRef. ", 
                       "Please review or edit these entries in your crosswalk or remove them/leave them unfilled if not applicable to analysis."))
      }
      definedCriteria <- as.data.frame(dplyr::relocate(dplyr::filter(criteriaMethods, 
                                                                     TADA.CharacteristicName %in% TADA_param$TADA.CharacteristicName), 
                                                       dplyr::all_of(desired_cols)))
      suppressMessages(DefineCriteriaMethodology <- EPATADA::TADA_DefineCriteriaMethodology())
      desired_types <- sapply(DefineCriteriaMethodology, 
                              class)
      suppressWarnings(for (i in 1:ncol(non_definedCriteria)) {
        if (desired_types[[i]] == "numeric") {
          non_definedCriteria[, i] <- as.numeric(non_definedCriteria[, 
                                                                     i])
          definedCriteria[, i] <- as.numeric(definedCriteria[, 
                                                             i])
        }
        else if (desired_types[[i]] == "character") {
          non_definedCriteria[, i] <- as.character(non_definedCriteria[, 
                                                                       i])
          definedCriteria[, i] <- as.character(definedCriteria[, 
                                                               i])
        }
        else if (desired_types[[i]] == "Date") {
          non_definedCriteria[, i] <- as.Date(non_definedCriteria[, 
                                                                  i])
          definedCriteria[, i] <- as.Date(definedCriteria[, 
                                                          i])
        }
      })
      DefineCriteriaMethodology <- dplyr::distinct(dplyr::arrange(dplyr::full_join(dplyr::select(DefineCriteriaMethodology, 
                                                                                                 ATTAINS.OrganizationIdentifier, ATTAINS.ParameterName, 
                                                                                                 ATTAINS.UseName, TADA.ComparableDataIdentifier, 
                                                                                                 TADA.CharacteristicName), definedCriteria, by = dplyr::join_by(ATTAINS.OrganizationIdentifier, 
                                                                                                                                                                ATTAINS.ParameterName, ATTAINS.UseName, TADA.ComparableDataIdentifier, 
                                                                                                                                                                TADA.CharacteristicName)), ATTAINS.UseName))
      DefineCriteriaMethodology <- dplyr::relocate(DefineCriteriaMethodology, 
                                                   desired_cols)
    }
    if (epa304a == TRUE) {
      print(paste0("epa304a == TRUE was selected: Joining EPA304a recommended standards by each unique TADA.CharacteristicName only if found."))
      epa304a <- utils::read.csv(system.file("extdata", 
                                             "EPA304a_criteria_table.csv", package = "EPATADA"))
      coltype.ref <- utils::read.csv(system.file("extdata", 
                                                 "TADAColTypeRef.csv", package = "EPATADA"))
      epa304a <- dplyr::filter(dplyr::select(suppressWarnings(EPATADA:::correctColType(epa304a)), 
                                             names(epa304a)[names(epa304a) %in% coltype.ref$column_name]), 
                               TADA.CharacteristicName %in% DefineCriteriaMethodology$TADA.CharacteristicName)
      DefineCriteriaMethodology <- plyr::rbind.fill(DefineCriteriaMethodology, 
                                                    epa304a)
    }
    if (displayUniqueId == FALSE) {
      print(paste0("displayUniqueId == FALSE was selected, TADA.ComparableDataIdentifier is converted to NA and duplicated rows are removed. ", 
                   "Users are recommended to fill out any applicable combinations of Characteristic, Fraction and Speciation for analysis."))
      DefineCriteriaMethodology <- dplyr::distinct(dplyr::arrange(dplyr::mutate(DefineCriteriaMethodology, 
                                                                                TADA.ComparableDataIdentifier = NA), ATTAINS.OrganizationIdentifier != 
                                                                    "EPA304a", ATTAINS.OrganizationIdentifier, ATTAINS.UseName))
      
      
      
    }
    
    # Return based on return_workbook flag
    if (return_workbook) {
      wb <- create_criteria_workbook(DefineCriteriaMethodology, org_id, MLSummaryRef, .data)
      return(list(data = DefineCriteriaMethodology, workbook = wb))
    } else {
      return(DefineCriteriaMethodology)
    }
    
  }
}

create_criteria_workbook <- function(DefineCriteriaMethodology, 
                                     org_id = NULL, 
                                     MLSummaryRef = NULL, 
                                     .data = NULL) {
  
  # Create new workbook
  wb <- openxlsx::createWorkbook()
  
  # Add worksheets
  openxlsx::addWorksheet(wb, "DefineCriteriaMethodology")
  openxlsx::addWorksheet(wb, "Index-Criteria", visible = FALSE)
  
  # Set active sheet
  openxlsx::activeSheet(wb) <- "DefineCriteriaMethodology"
  
  # Set zoom level for all sheets
  set_zoom <- function(x, sV) gsub("(?<=zoomScale=\")[0-9]+", x, sV, perl = TRUE)
  n_sheets <- length(wb$worksheets)
  for (i in 1:n_sheets) {
    sV <- wb$worksheets[[i]]$sheetViews
    if (!is.null(sV) && length(sV) > 0) {
      wb$worksheets[[i]]$sheetViews <- set_zoom(90, sV)
    }
  }
  
  # Create header style
  header_st <- openxlsx::createStyle(textDecoration = "Bold")
  
  # Set column widths
  if (ncol(DefineCriteriaMethodology) > 0) {
    openxlsx::setColWidths(wb, sheet = "DefineCriteriaMethodology",
                           cols = 1:ncol(DefineCriteriaMethodology), widths = "auto")
    openxlsx::setColWidths(wb, sheet = "DefineCriteriaMethodology",
                           cols = 1:min(5, ncol(DefineCriteriaMethodology)), widths = 20)
  }
  
  # Write main data
  openxlsx::writeData(wb, "DefineCriteriaMethodology",
                      startCol = 1, x = DefineCriteriaMethodology, headerStyle = header_st)
  
  # Prepare .data for dropdowns (handle missing case)
  if (is.null(.data) || !is.data.frame(.data)) {
    .data <- data.frame(
      TADA.ComparableDataIdentifier = NA_character_,
      TADA.CharacteristicName = NA_character_,
      TADA.ResultSampleFractionText = NA_character_,
      TADA.MethodSpeciationName = NA_character_,
      TADA.ResultMeasure.MeasureUnitCode = NA_character_
    )
  }
  
  # Write Index-Criteria sheet data for dropdowns
  index_cols <- c("TADA.ComparableDataIdentifier", "TADA.CharacteristicName",
                  "TADA.ResultSampleFractionText", "TADA.MethodSpeciationName")
  available_cols <- intersect(index_cols, names(.data))
  if (length(available_cols) > 0) {
    openxlsx::writeData(wb, "Index-Criteria", startCol = 6, startRow = 1,
                        x = unique(.data[, available_cols, drop = FALSE]))
  }
  
  openxlsx::writeData(wb, "Index-Criteria", startCol = 14, startRow = 1,
                      x = data.frame(AcuteChronic = c("Acute", "Chronic", "NA")))
  
  # Get water type list
  tryCatch({
    All.WaterTypeList <- utils::read.csv(
      system.file("extdata", "ATTAINSParamUseEntityRef.csv", package = "EPATADA")
    )
    if (!is.null(org_id) && length(org_id) > 0 && !all(org_id == "")) {
      Org.WaterTypeList <- dplyr::filter(All.WaterTypeList,
                                         ATTAINS.OrganizationIdentifier %in% org_id)
    } else {
      Org.WaterTypeList <- All.WaterTypeList
    }
    if (nrow(Org.WaterTypeList) > 0) {
      openxlsx::writeData(wb, "Index-Criteria", startCol = 10, startRow = 1,
                          x = unique(Org.WaterTypeList$ATTAINS.WaterType))
    }
  }, error = function(e) {
    message("Note: ATTAINSParamUseEntityRef.csv not found, skipping water type dropdown")
  })
  
  openxlsx::writeData(wb, "Index-Criteria", startCol = 11, startRow = 1,
                      x = data.frame(SaltFresh = c("Salt", "Fresh", "NA")))
  
  openxlsx::writeData(wb, "Index-Criteria", startCol = 12, startRow = 1,
                      x = data.frame(DepthCategory = c("No depth info", "Epilimnion-surface",
                                                       "Surface", "Bottom", "Middle")))
  
  # Handle MLSummaryRef for UniqueSpatialCriteria dropdown
  if (is.null(MLSummaryRef) || !is.data.frame(MLSummaryRef)) {
    MLSummaryRef <- data.frame(UniqueSpatialCriteria = NA_character_)
  }
  openxlsx::writeData(wb, "Index-Criteria", startCol = 13, startRow = 1,
                      x = data.frame(UniqueSpatialCriteria = c(unique(MLSummaryRef$UniqueSpatialCriteria), "NA")))
  
  openxlsx::writeData(wb, "Index-Criteria", startCol = 15, startRow = 1,
                      x = data.frame(EquationBased = c("Yes", "No", "NA")))
  
  if ("TADA.ResultMeasure.MeasureUnitCode" %in% names(.data)) {
    openxlsx::writeData(wb, "Index-Criteria", startCol = 18, startRow = 1,
                        x = data.frame(MagnitudeUnit = unique(.data$TADA.ResultMeasure.MeasureUnitCode)))
  }
  
  openxlsx::writeData(wb, "Index-Criteria", startCol = 20, startRow = 1,
                      x = data.frame(DurationUnit = c("n-hour", "n-day", "n-week", "n-month", "n-quarter")))
  
  openxlsx::writeData(wb, "Index-Criteria", startCol = 21, startRow = 1,
                      x = data.frame(DurationMethod = c("arithmetic mean", "arithmetic median",
                                                        "arithmetic max", "arithmetic min",
                                                        "geometric mean", "rolling geometric mean",
                                                        "rolling arithmetric mean")))
  
  openxlsx::writeData(wb, "Index-Criteria", startCol = 23, startRow = 1,
                      x = data.frame(FreqMethod = c("Percent of samples not meeting", "percentile",
                                                    "n-samples in 3 years", "n-samples in 4 years",
                                                    "n-samples in 5 years", "binomial test",
                                                    "NumberNotMeeting")))
  
  openxlsx::writeData(wb, "Index-Criteria", startCol = 24, startRow = 1,
                      x = data.frame(AssessPeriod = c("Last 30 years", "Last 10 years",
                                                      "Last 5 years", "Last 3 years",
                                                      "Last year", "NA")))
  
  openxlsx::writeData(wb, "Index-Criteria", startCol = 27, startRow = 1,
                      x = data.frame(Season = c("Summer", "Fall", "Spring", "Winter", "NA")))
  
  openxlsx::writeData(wb, "Index-Criteria", startCol = 31, startRow = 1,
                      x = data.frame(DistrPeriod = c("Seasonal", "Annual", "Semi-Annual",
                                                     "Quarterly", "Monthly", "Bi-weekly",
                                                     "Weekly", "10 days", "NA")))
  
  # Add data validations (dropdowns)
  validation_specs <- list(
    list(col = 4, ref_col = "F"),
    list(col = 5, ref_col = "G"),
    list(col = 6, ref_col = "H"),
    list(col = 7, ref_col = "I"),
    list(col = 8, ref_col = "J"),
    list(col = 9, ref_col = "K"),
    list(col = 10, ref_col = "L"),
    list(col = 11, ref_col = "M"),
    list(col = 12, ref_col = "N"),
    list(col = 13, ref_col = "O"),
    list(col = 16, ref_col = "R"),
    list(col = 18, ref_col = "T"),
    list(col = 19, ref_col = "U"),
    list(col = 21, ref_col = "W"),
    list(col = 22, ref_col = "X"),
    list(col = 25, ref_col = "AA"),
    list(col = 29, ref_col = "AE")
  )
  
  for (spec in validation_specs) {
    suppressWarnings(
      tryCatch({
        openxlsx::dataValidation(
          wb, sheet = "DefineCriteriaMethodology",
          cols = spec$col, rows = 2:1000, type = "list",
          value = sprintf("'Index-Criteria'!$%s$2:$%s$1000", spec$ref_col, spec$ref_col),
          allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE
        )
      }, error = function(e) NULL)
    )
  }
  
  # Freeze panes
  openxlsx::freezePane(wb, "DefineCriteriaMethodology",
                       firstActiveRow = 2, firstActiveCol = 4)
  
  # Add conditional formatting - use EPATADA color palette if available
  fill_color <- tryCatch(
    EPATADA::TADA_ColorPalette()[8], 
    error = function(e) "#E8F4EA"
  )
  blank_color <- tryCatch(
    EPATADA::TADA_ColorPalette()[13], 
    error = function(e) "#FFF3CD"
  )
  
  if (nrow(DefineCriteriaMethodology) > 0) {
    openxlsx::conditionalFormatting(
      wb, "DefineCriteriaMethodology",
      cols = 1:31, rows = 2:(nrow(DefineCriteriaMethodology) + 1),
      type = "notBlanks",
      style = openxlsx::createStyle(bgFill = fill_color)
    )
    
    openxlsx::conditionalFormatting(
      wb, "DefineCriteriaMethodology",
      cols = 1:31, rows = 2:(nrow(DefineCriteriaMethodology) + 1),
      type = "blanks",
      style = openxlsx::createStyle(bgFill = blank_color)
    )
  }
  
  # Group columns
  if (ncol(DefineCriteriaMethodology) >= 22) {
    openxlsx::groupColumns(wb, sheet = "DefineCriteriaMethodology",
                           cols = 22:ncol(DefineCriteriaMethodology),
                           hidden = FALSE, level = -1)
  }
  
  return(wb)
}
