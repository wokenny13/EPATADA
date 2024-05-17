---
  title: "R Notebook"
output: html_notebook
---
  
  
  # Access ATTAINS domains ####
temp <- GET("https://attains.epa.gov/attains-public/api/domains?domainName=ParameterName") %>%
  content(as = "text", encoding = "UTF-8") %>%
  fromJSON(flatten = TRUE)

# Reduce name column to a single object

attains.domain <- temp$name

# Access CST domains ####
library(rio)
CST_raw <- import("https://cfpub.epa.gov/wqsits/wqcsearch/criteria-search-tool-data.xlsx")

## BELOW WAS A MANUAL WAY OF REMOVING THE "legend" info in the CST domains ####
# # remove rows with "legend" info (rows 1-189)
# CST <- CST_raw[-c(1:203), ]
# 
# # rename columns names to the column names in row 190
# CST_colnames <- list(CST_raw[203, ])
#########################################################################

# When reviewing the CST file, the rows where the "legend' was changed from
# row 203 to row 206
# suggested method below to automatically find the row that contains the domain values

#CST_new looks for the row that contains all 23 CST domains and we identify the row this is in
#rather than manually have to find this
CST_new<-na.omit(CST_raw)
CST <- CST_raw[-c(1:(as.numeric(row.names(CST_new))[1])), ]

# rename columns names to the column names in row 190
CST_colnames <- list(CST_raw[as.numeric(row.names(CST_new))[1], ])

colnames(CST) <- c(CST_colnames[[1]])

CST.domain.upper <- toupper(CST$POLLUTANT_NAME)
CST.domain <- CST$POLLUTANT_NAME

###########counts and sorts the number of unique field elements in column POLLUTANT_NAME, identifies which pollutant we want to start to work with
CST %>% 
  group_by(CST$POLLUTANT_NAME) %>%
  summarize(Count=n()) %>%
  arrange(desc(Count))

###################################################################

# Find common domains ####

matches <- intersect(CST.domain.upper, attains.domain)


# 'matches' data frame does not contain the true CST pollutant values. 
# Therefore, to append the true CST pollutant values to the matches df,
# we need to add CST.domain and CST.domain.upper as columns in a df. Filter 
# the df with matches by the CST.domain.upper column. 

# To be totally sure the filter is correct, we can extract CST.domain.upper
# column as an object, remove duplicates, and compare to matches.
# Finally, rename columns and export df.





# Create CST df
CST.df <- data.frame(CST.domain.upper, CST.domain)

# Filter CST.df with matches by CST.domain.upper
CST.matches <- filter(CST.df, CST.df$CST.domain.upper %in% matches)

# Remove duplicate rows
CST.matches.unique <- CST.matches[!duplicated(CST.matches),]

# Double check filtering worked properly
CST.domain.upper2 <- CST.matches$CST.domain.upper

# Remove duplicates from CST.domain.upper2
matches2 <- CST.domain.upper2[!duplicated(CST.domain.upper2)]

# check that matches and matches2 are identical
SameElements <- function(a, b) return(identical(sort(a), sort(b)))
SameElements(matches, matches2)

# Rename columns in df of direct matches ####
colnames(CST.matches.unique) <- c("ATTAINS.parameter", "POLLUTANT_NAME") 

# Join std.pollutant.name to crosswalk via pollutant.name
just.pollutants <- select(CST, c("POLLUTANT_NAME", "STD_POLLUTANT_NAME"))

# remove duplicate rows
just.pollutants2 <- just.pollutants[!duplicated(just.pollutants), ]

# Join std.pollutants to crosswalk 
ATTAINS.CST.crosswalk <- inner_join(CST.matches.unique, just.pollutants2,
                                    by = "POLLUTANT_NAME")

# Rename columns
colnames(ATTAINS.CST.crosswalk) <- c("ATTAINS.parameter", "CST.pollutant", "CST.std.pollutant")

# export crosswalk to csv
write.csv(ATTAINS.CST.crosswalk, "ATTAINS_CST_matches.csv", row.names = FALSE)

