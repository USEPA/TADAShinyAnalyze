library(tidyverse)

dat <- read_csv("dev/Backup/Example/Montana/tada_jointoau_output_ts20260108122413/TADAShinyJoinToAU_copy_input_file.csv")

dat <- dat |>
  dplyr::mutate(ActivityStartDateTime = 
                  suppressWarnings(
                    lubridate::parse_date_time(ActivityStartDateTime, 
                                               orders = c("ymd HMS", "ymd HM", 
                                                          "ymd", "mdy")))
  ) |>
  dplyr::mutate(ActivityStartDate = lubridate::ymd(ActivityStartDate)) |>
  dplyr::mutate(DateTime = ActivityStartDateTime) |>
  # Remove NA in TADA.ResultMeasureValue and DateTime
  tidyr::drop_na(TADA.ResultMeasureValue) |>
  tidyr::drop_na(DateTime)

dat2 <- dat |> 
  pH_fun() |>
  Temperature_fun() |>
  hardness_fun()

criteria_template <- readxl::read_excel("dev/Backup/Example/Montana/Criteria_Template_20260108144100/Criteria_Methods_Template_edit.xlsx")

criteria_table_f1 <- criteria_template |>
  dplyr::filter(ATTAINS.OrganizationIdentifier %in% "MTDEQ") 

AU_Use <- read_csv("dev/Backup/Example/Montana/tada_jointoau_output_ts20260108122413/TADAShinyJoinToAU_AUtoUses_for_review.csv")
AU_MLID <- read_csv("dev/Backup/Example/Montana/tada_jointoau_output_ts20260108122413/TADAShinyJoinToAU_MLtoAUs_for_review.csv")

AU_Use_f1 <- AU_Use 

# Filter the AU_MLID based on AU_Use_f1
AU_MLID_f1 <- AU_MLID |>
  dplyr::filter(ATTAINS.AssessmentUnitIdentifier %in% 
                  AU_Use_f1$ATTAINS.AssessmentUnitIdentifier)

dat3 <- dat2 |>
  dplyr::filter(TADA.MonitoringLocationIdentifier %in% 
                  AU_MLID_f1$TADA.MonitoringLocationIdentifier)

dat4 <- dat3 |>
  dplyr::left_join(AU_MLID_f1) |>
  dplyr::left_join(AU_Use_f1, 
                   by = c("ATTAINS.AssessmentUnitIdentifier", 
                          "ATTAINS.WaterType",
                          "ATTAINS.OrganizationIdentifier"),
                   relationship = "many-to-many")
  
dat4_1 <- dat4 |>
  criteria_join(criteria_table_f1, 
                match_type = "Option 2") |>
  # Remove NA in TADA.ResultMeasureValue and DateTime
  tidyr::drop_na(TADA.ResultMeasureValue) |>
  tidyr::drop_na(DateTime)

# Construct the selected columns
selected_cols <- c(
  "TADA.MonitoringLocationIdentifier",
  "TADA.MonitoringLocationName",
  "TADA.LongitudeMeasure",
  "TADA.LatitudeMeasure",
  "ATTAINS.OrganizationIdentifier",
  "ATTAINS.ParameterName",
  "ATTAINS.UseName",
  "AcuteChronic",
  "UniqueSpatialCriteria",
  "Season",
  "EquationBased",
  "EquationType", 
  "TADA.CharacteristicName",
  "TADA.ResultSampleFractionText",
  "TADA.MethodSpeciationName",
  "TADA.ResultMeasure.MeasureUnitCode",
  "TADA.ResultMeasureValue",
  "ActivityStartDate",
  "DateTime",
  "pH",
  "Temperature",
  "Hardness",
  "MagnitudeValueLower",
  "MagnitudeValueUpper",
  "DurationValue",
  "DurationUnit",
  "DurationMethod",
  "FreqValue",
  "FreqMethod",
  # Equation coefficient columns
  "Equation",
  "hardness_param_1",
  "hardness_param_2",
  "hardness_param_3",
  "hardness_param_4",
  "hardness_param_5",
  "hardness_param_6",
  "pH_param_1",
  "pH_param_2",
  "pH_param_3",
  "pH_param_4"
)

if (tadat$use_type_batch %in% "Option 1"){
  selected_cols <- c(selected_cols[1:4], 
                     "ATTAINS.AssessmentUnitIdentifier",
                     selected_cols[5:40])
} else {
  selected_cols <- selected_cols
}

# Select columns
dat4_1 <- dat4_1 |> dplyr::select(dplyr::all_of(selected_cols))

# x <- dat4
# y <- criteria_table_f1 |>
#   dplyr::select(
#     -TADA.MethodSpeciationName,
#     -TADA.ComparableDataIdentifier
#   )
# 
# # Add flags to criteria table
# y2 <- y |> dplyr::mutate(Matched = "Yes")
# 
# # Build join expression as a string
# join_cols <- c(
#   "ATTAINS.OrganizationIdentifier",
#   "TADA.CharacteristicName",
#   "TADA.ResultMeasure.MeasureUnitCode == MagnitudeUnit"
# )
# 
# # Conditionally add columns
# if (use_type == "Option 1") {
#   join_cols <- c(join_cols, "ATTAINS.UseName")
# }
# 
# if (match_type == "Option 1") {
#   join_cols <- c(join_cols, "TADA.ResultSampleFractionText")
# }
# 
# # Build and evaluate the join_by expression
# join_expr <- paste0("dplyr::join_by(", paste(join_cols, collapse = ", "), ")")
# by <- eval(parse(text = join_expr))
# 
# # # Handle x table modifications for Option 2 (no use)
# # # In this case, the final ATTAINS.UseName is from the criteria table
# # if (use_type == "Option 2") {
# #   x_col <- names(x)
# #   x_col2 <- x_col[!x_col %in% "ATTAINS.UseName"]
# #   x2 <- x |> dplyr::select((dplyr::all_of(x_col2)))
# # } else {
# #   x2 <- x
# # }
# # 
# # # Handle y table modifications for Option 2 (no fraction)
# # if (match_type == "Option 2") {
# #   y_col <- names(y2)
# #   y_col2 <- y_col[!y_col %in% "TADA.ResultSampleFractionText"]
# #   y2 <- y2 |> dplyr::distinct(dplyr::across(dplyr::all_of(y_col2)))
# # }
# 
# x2 <- x
# 
# # Perform the join
# x3 <- x2 |>
#   dplyr::left_join(y2, by = by, relationship = "many-to-many") |>
#   dplyr::mutate(Matched = ifelse(is.na(Matched), "No", Matched))
# 
# # Apply filter if requested
# if (filter_type) {
#   x3 <- x3 |> dplyr::filter(Matched == "Yes")
# }
