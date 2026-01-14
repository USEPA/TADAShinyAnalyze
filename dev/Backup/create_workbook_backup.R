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
  
  # Handle missing .data
  if (is.null(.data) || !is.data.frame(.data)) {
    .data <- data.frame(
      TADA.ComparableDataIdentifier = NA_character_, 
      TADA.CharacteristicName = NA_character_, 
      TADA.ResultSampleFractionText = NA_character_, 
      TADA.MethodSpeciationName = NA_character_, 
      TADA.ResultMeasure.MeasureUnitCode = NA_character_
    )
  }
  
  # Write Index-Criteria dropdown data
  openxlsx::writeData(wb, "Index-Criteria", startCol = 6, startRow = 1, 
                      x = unique(.data[, c("TADA.ComparableDataIdentifier", 
                                           "TADA.CharacteristicName", 
                                           "TADA.ResultSampleFractionText", 
                                           "TADA.MethodSpeciationName"), drop = FALSE]))
  
  # Water type list
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
  
  # Handle MLSummaryRef
  if (is.null(MLSummaryRef) || !is.data.frame(MLSummaryRef)) {
    MLSummaryRef <- data.frame(UniqueSpatialCriteria = NA_character_)
  }
  openxlsx::writeData(wb, "Index-Criteria", startCol = 13, startRow = 1, 
                      x = data.frame(UniqueSpatialCriteria = c(unique(MLSummaryRef$UniqueSpatialCriteria), "NA")))
  
  openxlsx::writeData(wb, "Index-Criteria", startCol = 14, startRow = 1, 
                      x = data.frame(AcuteChronic = c("Acute", "Chronic", "NA")))
  
  openxlsx::writeData(wb, "Index-Criteria", startCol = 15, startRow = 1, 
                      x = data.frame(EquationBased = c("Yes", "No", "NA")))
  
  # MagnitudeUnit
  if ("TADA.ResultMeasure.MeasureUnitCode" %in% names(.data)) {
    openxlsx::writeData(wb, "Index-Criteria", startCol = 18, startRow = 1, 
                        x = data.frame(MagnitudeUnit = unique(.data$TADA.ResultMeasure.MeasureUnitCode)))
  }
  
  openxlsx::writeData(wb, "Index-Criteria", startCol = 20, startRow = 1, 
                      x = data.frame(DurationUnit = c("n-hour", "n-day", "n-week", 
                                                      "n-month", "n-quarter")))
  
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
  
  # Data validations (unchanged - these are correct)
  suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", 
                                            cols = 4, rows = 2:1000, type = "list", 
                                            value = "'Index-Criteria'!$F$2:$F$1000", 
                                            allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
  suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", 
                                            cols = 5, rows = 2:1000, type = "list", 
                                            value = "'Index-Criteria'!$G$2:$G$1000", 
                                            allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
  suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", 
                                            cols = 6, rows = 2:1000, type = "list", 
                                            value = "'Index-Criteria'!$H$2:$H$1000", 
                                            allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
  suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", 
                                            cols = 7, rows = 2:1000, type = "list", 
                                            value = "'Index-Criteria'!$I$2:$I$1000", 
                                            allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
  suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", 
                                            cols = 8, rows = 2:1000, type = "list", 
                                            value = "'Index-Criteria'!$J$2:$J$1000", 
                                            allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
  suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", 
                                            cols = 9, rows = 2:1000, type = "list", 
                                            value = "'Index-Criteria'!$K$2:$K$1000", 
                                            allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
  suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", 
                                            cols = 10, rows = 2:1000, type = "list", 
                                            value = "'Index-Criteria'!$L$2:$L$1000", 
                                            allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
  suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", 
                                            cols = 11, rows = 2:1000, type = "list", 
                                            value = "'Index-Criteria'!$M$2:$M$1000", 
                                            allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
  suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", 
                                            cols = 12, rows = 2:1000, type = "list", 
                                            value = "'Index-Criteria'!$N$2:$N$1000", 
                                            allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
  suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", 
                                            cols = 13, rows = 2:1000, type = "list", 
                                            value = "'Index-Criteria'!$O$2:$O$1000", 
                                            allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
  suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", 
                                            cols = 16, rows = 2:1000, type = "list", 
                                            value = "'Index-Criteria'!$R$2:$R$1000", 
                                            allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
  suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", 
                                            cols = 18, rows = 2:1000, type = "list", 
                                            value = "'Index-Criteria'!$T$2:$T$1000", 
                                            allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
  suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", 
                                            cols = 19, rows = 2:1000, type = "list", 
                                            value = "'Index-Criteria'!$U$2:$U$1000", 
                                            allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
  suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", 
                                            cols = 21, rows = 2:1000, type = "list", 
                                            value = "'Index-Criteria'!$W$2:$W$1000", 
                                            allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
  suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", 
                                            cols = 22, rows = 2:1000, type = "list", 
                                            value = "'Index-Criteria'!$X$2:$X$1000", 
                                            allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
  suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", 
                                            cols = 25, rows = 2:1000, type = "list", 
                                            value = "'Index-Criteria'!$AA$2:$AA$1000", 
                                            allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
  suppressWarnings(openxlsx::dataValidation(wb, sheet = "DefineCriteriaMethodology", 
                                            cols = 29, rows = 2:1000, type = "list", 
                                            value = "'Index-Criteria'!$AE$2:$AE$1000", 
                                            allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
  
  # Freeze panes
  openxlsx::freezePane(wb, "DefineCriteriaMethodology", 
                       firstActiveRow = 2, firstActiveCol = 4)
  
  # Conditional formatting
  fill_color <- EPATADA::TADA_ColorPalette()[8]
  blank_color <- EPATADA::TADA_ColorPalette()[13]
  
  if (nrow(DefineCriteriaMethodology) > 0) {
    openxlsx::conditionalFormatting(wb, "DefineCriteriaMethodology", 
                                    cols = 1:31, 
                                    rows = 2:(nrow(DefineCriteriaMethodology) + 1), 
                                    type = "notBlanks", 
                                    style = openxlsx::createStyle(bgFill = fill_color))
    openxlsx::conditionalFormatting(wb, "DefineCriteriaMethodology", 
                                    cols = 1:31, 
                                    rows = 2:(nrow(DefineCriteriaMethodology) + 1), 
                                    type = "blanks", 
                                    style = openxlsx::createStyle(bgFill = blank_color))
  }
  
  # Group columns
  openxlsx::groupColumns(wb, sheet = "DefineCriteriaMethodology", 
                         cols = 22:ncol(DefineCriteriaMethodology), 
                         hidden = FALSE, level = -1)
  
  return(wb)
}

