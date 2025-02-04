#' Update TADA Reference Files
#' @return Saves updated reference files
#' This is only needed for ref tables in WQXRefTables.R
#' and the tribal feature layers in TADAGeospatialRefLayers.R
#'
TADA_UpdateAllRefs <- function() {
  TADA_UpdateWQXCharValRef()
  TADA_UpdateMeasureUnitRef()
  TADA_UpdateDetCondRef()
  TADA_UpdateDetLimitRef()
  TADA_UpdateActivityTypeRef()
  TADA_UpdateCharacteristicRef()
  TADA_UpdateMeasureQualifierCodeRef()
  TADA_UpdateMonLocTypeRef()
  TADA_UpdateTribalLayers()
}

## FUNCTION TO UPDATE EXAMPLE DATA

TADA_UpdateExampleData <- function() {
  # Generate Data_Nutrients_UT.rda
  Data_Nutrients_UT <- TADA_DataRetrieval(
    statecode = "UT",
    characteristicName = c("Ammonia", "Nitrate", "Nitrogen"),
    startDate = "2020-10-01",
    endDate = "2022-09-30"
  )
  print("Data_Nutrients_UT")
  print(dim(Data_Nutrients_UT))
  # save(Data_Nutrients_UT, file = "data/Data_Nutrients_UT.rda")
  usethis::use_data(Data_Nutrients_UT,
    internal = FALSE, overwrite = TRUE,
    compress = "xz", version = 3, ascii = FALSE
  )
  rm(Data_Nutrients_UT)

  # Generate Data_6Tribes_5y.rda
  Data_6Tribes_5y <- TADA_DataRetrieval(
    organization = c(
      "REDLAKE_WQX",
      "SFNOES_WQX",
      "PUEBLO_POJOAQUE",
      "FONDULAC_WQX",
      "PUEBLOOFTESUQUE",
      "CNENVSER"
    ),
    startDate = "2018-01-01",
    endDate = "2023-01-01"
  )
  print("Data_6Tribes_5y:")
  print(dim(Data_6Tribes_5y))
  # save(Data_6Tribes_5y, file = "data/Data_6Tribes_5y.rda")
  usethis::use_data(Data_6Tribes_5y,
    internal = FALSE, overwrite = TRUE,
    compress = "xz", version = 3, ascii = FALSE
  )

  # Generate Data_6Tribes_5y_Harmonized.rda
  y <- subset(Data_6Tribes_5y, Data_6Tribes_5y$TADA.ActivityMediaName %in% c("WATER"))
  y <- TADA_RunKeyFlagFunctions(Data_6Tribes_5y)
  rm(Data_6Tribes_5y)
  y <- TADA_FlagMethod(y, clean = TRUE)
  y <- TADA_FlagAboveThreshold(y, clean = TRUE)
  y <- TADA_FlagBelowThreshold(y, clean = TRUE)
  y <- TADA_FindPotentialDuplicatesMultipleOrgs(y, dist_buffer = 100)
  y <- TADA_FindPotentialDuplicatesSingleOrg(y)
  y <- dplyr::filter(y, !(MeasureQualifierCode %in% c("D", "H", "ICA", "*")))
  y <- TADA_SimpleCensoredMethods(y,
    nd_method = "multiplier",
    nd_multiplier = 0.5,
    od_method = "as-is",
    od_multiplier = "null"
  )
  y <- dplyr::filter(y, TADA.ResultMeasureValueDataTypes.Flag != "Text" &
    TADA.ResultMeasureValueDataTypes.Flag != "NA - Not Available" &
    !is.na(TADA.ResultMeasureValue))
  # uses HarmonizationTemplate.csv in the extdata folder
  Data_6Tribes_5y_Harmonized <- TADA_HarmonizeSynonyms(y)
  print("Data_6Tribes_5y_Harmonized:")
  print(dim(Data_6Tribes_5y_Harmonized))
  # save(Data_6Tribes_5y_Harmonized, file = "data/Data_6Tribes_5y_Harmonized.rda")
  usethis::use_data(Data_6Tribes_5y_Harmonized,
    internal = FALSE, overwrite = TRUE,
    compress = "xz", version = 3, ascii = FALSE
  )
  rm(Data_6Tribes_5y_Harmonized)

  # Generate Data_NCTCShepherdstown_HUC12
  Data_NCTCShepherdstown_HUC12 <- TADA_DataRetrieval(
    startDate = "2020-03-14",
    endDate = "null",
    countycode = "null",
    huc = "02070004",
    siteid = "null",
    siteType = "null",
    characteristicName = "null",
    characteristicType = "null",
    sampleMedia = "null",
    statecode = "null",
    organization = "null",
    project = "null",
    applyautoclean = TRUE
  )
  print("Data_NCTCShepherdstown_HUC12:")
  print(dim(Data_NCTCShepherdstown_HUC12))
  # save(Data_NCTCShepherdstown_HUC12, file = "data/Data_NCTCShepherdstown_HUC12.rda")
  usethis::use_data(Data_NCTCShepherdstown_HUC12, internal = FALSE, overwrite = TRUE, compress = "xz", version = 3, ascii = FALSE)
  rm(Data_NCTCShepherdstown_HUC12)

  # Generate Data_R5_TADAPackageDemo
  Data_R5_TADAPackageDemo <- TADA_DataRetrieval(
    startDate = "2019-05-01",
    endDate = "2019-05-07",
    countycode = "null",
    huc = "null",
    siteid = "null",
    siteType = "null",
    characteristicName = "null",
    characteristicType = "null",
    sampleMedia = "null",
    statecode = c("IL", "IN", "MI", "MN", "OH", "WI"),
    organization = "null",
    project = "null",
    applyautoclean = FALSE
  )
  print("Data_R5_TADAPackageDemo:")
  print(dim(Data_R5_TADAPackageDemo))
  # save(Data_R5_TADAPackageDemo, file = "data/Data_R5_TADAPackageDemo.rda")
  usethis::use_data(Data_R5_TADAPackageDemo, internal = FALSE, overwrite = TRUE, compress = "xz", version = 3, ascii = FALSE)
  rm(Data_R5_TADAPackageDemo)
}

## Find char-frac-spec-unit combos not present in TADA HarmonizationTemplate.
## Add new combinations when found to the HarmonizationTemplate.csv and
## NPsummation_key.csv (if relevant to TN or TP summation).

FindSynonyms <- function() {
  test <- TADA_RandomTestingData()
  test1 <- TADA_RunKeyFlagFunctions(test)
  ref <- TADA_GetSynonymRef()
  ref_chars <- unique(ref$TADA.CharacteristicName)
  test_chars <- unique(subset(test1, test1$TADA.CharacteristicName %in% ref_chars)[, c("TADA.CharacteristicName", "TADA.ResultSampleFractionText", "TADA.MethodSpeciationName", "TADA.ResultMeasure.MeasureUnitCode")])
  test_chars_ref <- merge(test_chars, ref, all.x = TRUE)
  new_combos <- subset(test_chars_ref, is.na(test_chars_ref$HarmonizationGroup))[, c("TADA.CharacteristicName", "TADA.ResultSampleFractionText", "TADA.MethodSpeciationName", "TADA.ResultMeasure.MeasureUnitCode")]
  if (dim(new_combos)[1] > 0) {
    print("New combinations found in random dataset test.")
  }
  return(new_combos)
}


# TADA_OvernightTesting
#
# @return console inputs and outputs
#

# TADA_OvernightTesting <- function(){
#
#   testing_log <- file("testing_log.txt") # File name of output log
#
#   sink(testing_log, append = TRUE, type = "output") # Writing console output to log file
#   sink(testing_log, append = TRUE, type = "message")
#
#   #cat(readChar(rstudioapi::getSourceEditorContext()$path, # Writing currently opened R script to file
#   #             file.info(rstudioapi::getSourceEditorContext()$path)$size))
#
#   num_iterations=2
#   master_missing_codes_df <- data.frame(MeasureQualifierCode = NA, TADA.MeasureQualifierCode.Flag = NA)
#
#   for (i in 1:num_iterations) {
#
#     testing <- TADA_RandomTestingData()
#
#     testing2 <- TADA_FlagMeasureQualifierCode(testing)
#
#     #expect_true(all(testing2$TADA.MeasureQualifierCode.Flag != "Not Reviewed"))
#
#     #print(unique(testing2$TADA_FlagMeasureQualifierCode))
#     #print(unique(testing2$MeasureQualifierCode))
#
#     # load in ResultMeasureQualifier Flag Table
#     qc.ref <- TADA_GetMeasureQualifierCodeRef() %>%
#       dplyr::rename(MeasureQualifierCode = Code) %>%
#       dplyr::select(MeasureQualifierCode, TADA.MeasureQualifierCode.Flag)
#
#     codes = unique(testing2$MeasureQualifierCode)
#     missing_codes = codes[!codes %in% qc.ref$MeasureQualifierCode]
#
#     missing_codes_df <- data.frame(MeasureQualifierCode = missing_codes, TADA.MeasureQualifierCode.Flag = "Not Reviewed")
#
#     View(missing_codes_df)
#
#     master_missing_codes_df <- dplyr::full_join(missing_codes_df, master_missing_codes_df, by = c("MeasureQualifierCode", "TADA.MeasureQualifierCode.Flag"), copy = TRUE)
#
#     View(master_missing_codes_df)
#
#     }
#
#   master_missing_codes_distinct = master_missing_codes_df %>% dplyr::distinct()
#
#   View(master_missing_codes_distinct)
#
#   master_missing_codes_freq = as.data.frame(table(master_missing_codes_df))
#
#   View(master_missing_codes_freq)
#
#   closeAllConnections() # Close connection to log file
#
#   return(testing_log)
#
#   }


# # Run styler to style code
# # https://style.tidyverse.org/
# # See: https://styler.r-lib.org/reference/style_pkg.html
# # Run the following with defaults
# library(styler)
# style_pkg()
#
# # Run devtools check and test
# devtools::check()
# devtools::check(manual = TRUE, remote = TRUE, incoming = TRUE) # more robust test for releases (includes broken link check)
# devtools::test()
#
# # spell check
# library(spelling)
# spelling::spell_check_package(
#   pkg = ".",
#   vignettes = TRUE
# )
# # run to update spelling word list
# spelling::get_wordlist()
# spelling::update_wordlist()
