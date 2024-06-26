#' CST-ATTAINS Waterbody 'use' and 'use location' Crosswalk Reference Key
#'
#' Function downloads and returns the newest available CST-ATTAINS use crosswalk
#' reference dataframe. 
#'
#' @return Dataframe of CST-ATTAINS 'use' crosswalk
#'
#' @export

TADA_GetUseCrosswalkRef <- function() {

#' Generate CST-ATTAINS Waterbody 'use' and 'use location' Table
#'
#' This dataframe is used in joining a TADA dataframe to its 
#' parameter and use combinations through the TADA_DefineCriteriaMethodology function. 
#' This will be used as the basis for defining the water quality standards (WQS) 
#' criteria methodologies for each state, territory or tribe. 
#' This function will join in the 'use' portion. The ParmeterMappingRefTables.R file
#' will join in the 'parameter' portion to the TADA dataframe.
#' 
#' Criteria Search Tool (CST) with the ATTAINS by use. 
#' Users should review and validate the 'use' crosswalk reference table for their 
#' own dataset and determine the appropriate mapping use for any additional 'use' 
#' field value(s) added to the ATTAINS or CST domain values.
#' 
#'
#' Function pulls in waterbody 'Use' from the Criteria Search Tool (CST) domain values - data table, 
#' and the ATTAINS domain values. A relational table will be formed by joining 'Uses' and 'Entity'.
#' To form this relational table, we must troubleshoot the allowable domain values within each column
#' of these two tables. Exact matches only constitutes a small amount of matches. We proceed with a 
#' '%LIKE%' match of 'Use' names from the ATTAINS table (values tend to be more aggregated, less detailed)
#' to the CST table 'Use' names (which tends to be more detailed and contain site specific location details)
#' 
#' This relational table will be created and exported as a 'use.mapping.csv' crosswalk 
#' to form the 'DefineCriteriaMethodlogy.csv' file.
#'
#' @param .data TADA dataframe
#' @param display A character string denoting what fields to return in the summary table. Defaults to "key". "all" will return all fields in the dataset, "most" will return most field names except those holding numeric values or units, and "key" returns the most important columns to review. Note that if a field is completely NA, it will not be shown on the summary table.
#' @param characteristicName Optional. Defaults to "null". A vector of TADA-converted (all caps) WQP characteristics a user may provide to filter the results to one or more characteristics of interest. "null" will show a summary table for the whole dataset.
#'
#' @return A summary table yielding the number of unique values in each field.
#'
#' @export
#'
#' @examples
#'

library(arsenal)
library(httr)
library(tidyverse)
library(dplyr)
library(jsonlite)
library(rio)
library(fuzzyjoin)

# Create data frame. Pull in 'uses' from ATTAINS Domains field. This defines the activities that took place at a waterbody. 
temp2 <- GET("https://attains.epa.gov/attains-public/api/domains?domainName=UseName") %>%
  content(as = "text", encoding = "UTF-8") %>%
  fromJSON(flatten = TRUE)

# Create data frame. Pull in 'Entity Name' from ATTAINS Domains field. This defines the allowable 'Uses' for each 'Entity'. 
# This will help reduce manual labor of matching/crosswalking 'Uses' from the CST database and the ATTAINS database.
temp3 <- GET("https://attains.epa.gov/attains-public/api/domains?domainName=OrgStateCode") %>%
  content(as = "text", encoding = "UTF-8") %>%
  fromJSON(flatten = TRUE)

# Contains location descriptions of assessment units that can be found within some
# use name labels in the CST
# au_id <- GET("https://attains.epa.gov/attains-public/api/assessmentUnits?stateCode=FL") %>%
#   content(as = "text", encoding = "UTF-8") %>%
#   fromJSON(flatten = TRUE)
#au_id2 <- au_id[["items"]][["assessmentUnits"]][[1]]

# Creates clean data frame of allowable ATTAINS use for each Entity
ATTAINS.use <- temp2 %>%
  dplyr::select(., "name", "context") %>%
  dplyr::left_join(., temp3, by = "context") %>%
  dplyr::mutate(name.x, toupper(name.x)) %>%
  dplyr::select(., "name.y", "toupper(name.x)" ) %>%
  stats::setNames(., c("ENTITY_ABBR", "ATTAINS_USE_NAME")) %>%
  dplyr::distinct() %>%
  cbind(., unlist(lapply(lapply(strsplit(.$ATTAINS_USE_NAME, " "), 
    function(x) paste0( "(?=.*", x, collapse = "", ")" )),
    function(x) paste0( "^", x, ".*$") ) ) ) %>%
  stats::setNames(., c("ENTITY_ABBR", "ATTAINS_USE_NAME", "USE_NAME_UNLISTED"))

# Pulls in the 'Uses' from the CST domain. Will need to determine if
# additional methods can be used for further matching of ATTAINS AU definitions to CST
CST_raw <- import("https://cfpub.epa.gov/wqsits/wqcsearch/criteria-search-tool-data.xlsx")

# CST_new looks for the row that contains all 23 column names. We identify the row this is in
# rather than manually have to find this
CST_col <- na.omit(CST_raw)

# identifies the rows that has all 23 CST column name domains
CST <- CST_raw[-c(1:(as.numeric(row.names(CST_col))[1])), ]

# rename columns names to the first instance with all 23 CST domains
CST_colnames <- list(CST_raw[as.numeric(row.names(CST))[1] - 1, ])
colnames(CST) <- c(CST_colnames[[1]])

CST.use <- CST %>%
  dplyr::select(., 'USE_CLASS_NAME_LOCATION_ETC', 'ENTITY_ABBR') %>%
  dplyr::mutate(USE_CLASS_NAME_LOCATION_ETC, toupper(USE_CLASS_NAME_LOCATION_ETC)) %>%
  dplyr::select(., "ENTITY_ABBR", "toupper(USE_CLASS_NAME_LOCATION_ETC)" ) %>%
  stats::setNames(., c("ENTITY_ABBR", "CST_USE_NAME")) %>%
  dplyr::distinct()

# Finds exact matches for USE type from the CST table and ATTAINS table
use.matches <- intersect(CST.use$CST_USE_NAME, ATTAINS.use$ATTAINS_USE_NAME)

# A many to one mapping of CST use to ATTAINS use.
USES.MAPPING.UPDATED <- regex_left_join(CST.use, ATTAINS.use, by = c('CST_USE_NAME'='USE_NAME_UNLISTED','ENTITY_ABBR'))

# one to one mapping of ATTAINS use to CST.
USES.MAPPING.UPDATED2 <- regex_right_join(CST.use, ATTAINS.use, by = c('CST_USE_NAME'='USE_NAME_UNLISTED','ENTITY_ABBR'))
select(USES.MAPPING.UPDATED2, "ENTITY_ABBR.y", "CST_USE_NAME", "ATTAINS_USE_NAME")

# Save updated table in cache,
UseMappingTableCache <- USES.MAPPING.UPDATED2

return(UseMappingTableCache)

#utils::write.csv(USES.MAPPING.UPDATED2, "inst/extdata/use_mapping_crosswalk.csv", row.names = FALSE)
#ref <- utils::read.csv(system.file("extdata", "use_mapping_crosswalk.csv", package = "EPATADA"))
#return(ref)
}

# Update 'use_mapping_crosswalk.csv' Reference Table internal file (for internal use only)

TADA_UpdateUseMappingRef <- function() {
 utils::write.csv(TADA_GetUseCrosswalkRef(),"inst/extdata/use_mapping_crosswalk.csv", row.names = FALSE)
}
