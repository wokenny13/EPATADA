#' CST-ATTAINS Waterbody 'use' and 'use location' Crosswalk Reference Key
#'
#' Function downloads and returns the newest available CST-ATTAINS use crosswalk
#' reference dataframe. 
#'
#' @return Dataframe of CST-ATTAINS 'use' crosswalk
#'
#' @export

TADA_GetUseCrosswalkRef <- function() {
  ref <- utils::read.csv(system.file("extdata", "use_mapping_crosswalk.csv", package = "EPATADA"))
  return(ref)
}


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

# Create data frame. Pull in 'Uses' from ATTAINS Domains field. This defines the activities that took place at a waterbody. 
temp2 <- GET("https://attains.epa.gov/attains-public/api/domains?domainName=UseName") %>%
  content(as = "text", encoding = "UTF-8") %>%
  fromJSON(flatten = TRUE)

# Create data frame. Pull in 'Entity Name' from ATTAINS Domains field. This defines the allowable 'Uses' for each 'Entity'. 
# This will help reduce manual labor of matching/crosswalking 'Uses' from the CST database and the ATTAINS database.
temp3 <- GET("https://attains.epa.gov/attains-public/api/domains?domainName=OrgStateCode") %>%
  content(as = "text", encoding = "UTF-8") %>%
  fromJSON(flatten = TRUE)

ATTAINS.use <- temp2 %>%
  left_join(., temp3, by = "context") %>%
  select(.,"name.x", "name.y") %>%
  mutate(name.x, toupper(name.x))
  
temp2$name.x <- toupper(temp2$name.x)

au_id <- GET("https://attains.epa.gov/attains-public/api/assessmentUnits?stateCode=FL") %>%
  content(as = "text", encoding = "UTF-8") %>%
  fromJSON(flatten = TRUE)
au_id2 <- au_id[["items"]][["assessmentUnits"]][[1]]

# Removes duplicate rows in the data frame
attains.use.upper2 <- unique(temp2)
colnames(attains.use.upper2) <- c('USE','ENTITY_ABBR')


# Extract name parameter from the Use domains. Can additional context be used for 
# further classification of water use?
attains.entity.use <- temp2$name
attains.use <- temp2$name
# Capitalize characters and converts to a data frame
attains.use.upper <- toupper(attains.use)
attains.use.upper <- as.data.frame(attains.use.upper)
# Removes duplicate rows in the data frame
attains.use.upper2 <- unique(attains.use.upper)
# sort(unique(attains.use.upper))
# Rename the column to 'USE'
colnames(attains.use.upper2) <- c('USE')


library(rio)
# Pulls in the 'Uses' from the CST domain. Will need to determine if
# additional methods can be used for further matching of ATTAINS AU definitions to CST
CST_raw <- import("https://cfpub.epa.gov/wqsits/wqcsearch/criteria-search-tool-data.xlsx")

# CST_new looks for the row that contains all 23 column names. We identify the row this is in
# rather than manually have to find this
CST_col <-na.omit(CST_raw)

# identifies the rows that has all 23 CST column name domains
CST <- CST_raw[-c(1:(as.numeric(row.names(CST_col))[1])), ]

# rename columns names to the first instance with all 23 CST domains
CST_colnames <- list(CST_raw[as.numeric(row.names(CST_new))[1], ])
colnames(CST) <- c(CST_colnames[[1]])
CST_colnames <- list(CST_raw[as.numeric(row.names(CST_new))[1], ])

CST.use <- select(CST, 'USE_CLASS_NAME_LOCATION_ETC', 'ENTITY_ABBR')
CST.use <- as.data.frame(CST.use)
CST.use.upper <- toupper(CST$USE_CLASS_NAME_LOCATION_ETC)
CST.use.upper <- cbind(CST.use, as.data.frame(CST.use.upper))
CST.use.upper2 <- unique(CST.use.upper[,2:3])
colnames(CST.use.upper2) <- c('ENTITY_ABBR','USE')
# head(sort(unique(CST.use.upper)),100)

# CST.use.df <- data.frame(CST.use.upper, CST.use)

# Finds exact matches for USE type from the CST table and ATTAINS table
use.matches <- intersect(CST.use.upper2, attains.use.upper2)

# Filter CST.df with matches by CST.domain.upper
# CST.use.matches <- filter(CST.use.df, CST.use.df$CST.use.upper %in% matches)
attains.use.upper3 <- as.data.frame(sort(attains.use.upper2$USE))
colnames(attains.use.upper3) <- c("USE")
attains.use.upper3 <- left_join(attains.use.upper3, use.matches, keep = TRUE)
colnames(attains.use.upper3) <- c("ATTAINS.USE","CST.NA.USE.INPUTS.NEEDED")

REFRESH.CST.ATTAINS.USE <- full_join(attains.use.upper3, uses.map1, by = c('ATTAINS.USE' = 'DES_USE_ATTAINS'), keep = TRUE)
REFRESH.CST.ATTAINS.USE <- unique(select(REFRESH.CST.ATTAINS.USE, 'ATTAINS.USE', 'DES_USE_ATTAINS'))
colnames(REFRESH.CST.ATTAINS.USE) <- c("NEW.ATTAINS.USE","OLD.ATTAINS.USE")
write.csv(REFRESH.CST.ATTAINS.USE, "CST_ATTAINS_REFRESH.csv")

# Suggest doing a many to one mapping of CST use to ATTAINS use.
USES.MAPPING.UPDATED <- left_join(CST.use.upper2, attains.use.upper2, by = 'USE', keep = TRUE)
# Many to one mapping of CST use to ATTAINS use by 'LIKE' values (joined if contains a substring)
attains.use.upper2$method1 <- #if(length(attains.use.upper2$USE[i]) == 1,
  unlist( lapply( lapply( strsplit( attains.use.upper2$USE, " "), 
                  function(x) paste0( "(?=.*", x, collapse = "", ")" )),
                  function(x) paste0( "^", x, ".*$") ) )
attains.use.upper2$method2 <- #if(length(attains.use.upper2$USE[i]) == 1,
  unlist( lapply( lapply( strsplit( attains.use.upper2$USE, " "), 
                  function(x) paste0( "(?=.*\\\\b", x, "\\\\b", collapse = "", ")" )),
                  function(x) paste0( "^", x, ".*$") ) )

USES.MAPPING.UPDATED2 <- regex_left_join(CST.use.upper2, attains.use.upper2, by = c('USE'='method1','ENTITY_ABBR'))
#write.csv(USES.MAPPING.UPDATED, "USES.MAPPING.UPDATED.csv")
#CST.ATTAINS.use <- left_join(CST.use.upper2, attains.use.upper3, by = c('USE' = 'CST.NA.USE.INPUTS.NEEDED'), keep = TRUE)
ref <- utils::write.csv(USES.MAPPING.UPDATED2, system.file("extdata", "use_mapping_crosswalk.csv"))
return(ref)