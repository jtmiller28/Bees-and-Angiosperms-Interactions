test <- read.delim("/home/jt-miller/Globi-Bees-Plant-Interactions/Plant-Data/GBIF-Pull/Uniform-Methods-Data-Pull/0018087-220831081235567/occurrence-quickview.txt", header = TRUE)
test2 <- read.delim("/home/jt-miller/Globi-Bees-Plant-Interactions/Plant-Data/GBIF-Pull/Uniform-Methods-Data-Pull/0018087-220831081235567/verbatimQuickview.txt", header = TRUE)

grep("Warning", names(test))
names(test)

# What columns of interest do we want to cut from the parent dataset? 
#16,17,19,55, 56, 64, 68, 69, 74, 75, 83, 84, 103, 107, 108, 109, 110, 126, 127, 129, 130, 131, 138, 139, 140, 141, 143, 144, 145, 146, 147, 174, 189, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207, 208, 210, 211, 226, 229, 240, 241, 242

# using grep, I am grabbing certain columns that have to do with the issue at hand. 

# ISSUES AND FLAGS GBIF REF: https://data-blog.gbif.org/post/issues-and-flags/
######### Investigate Issues ########
library(tidyverse)

### Coordinate Percision Issues ###
grep("SRS", names(test)) # 143,144
grep("decimal", names(test)) # 138,139 
grep("references", names(test))
grep("occurrenceID", names(test))

# Issues are in 226

# Bring the data in after processing it with: cat occurrence.txt | cut -f 143,144,226 | grep "GEODETIC_DATUM_INVALID\|PRESUMED_SWAPPED_COORDINATE\|COORDINATE_REPROJECTION_SUSPICIOUS" > CRS-Issues.txt


crs_issues <- read.delim("/home/jt-miller/Globi-Bees-Plant-Interactions/Plant-Data/GBIF-Pull/Uniform-Methods-Data-Pull/0018087-220831081235567/CRS-Issues.txt", header = FALSE)

crs_issues <- crs_issues %>%
  rename(references = V1) %>%
  rename(occurrenceID = V2) %>% 
  rename(decimalLatitude = V3) %>% 
  rename(decimalLongitude = V4) %>% 
  rename(verbatimCoordinateSystem = V5) %>% 
  rename(verbatimSRS = V6) %>% 
  rename(issues = V7)

datum_invalid <- crs_issues %>%
  filter(grepl("GEODETIC_DATUM_INVALID", issues)) # Get rid of these! 

swapped_coords <- crs_issues %>% 
  filter(grepl("PRESUMED_SWAPPED_COORDINATE", issues)) # These could possibly be useful


sus_coords <- crs_issues %>% 
  filter(grepl("COORDINATE_REPROJECTION_SUSPICIOUS", issues)) # Drop these! 

# Remove all of the ones that include inaccurate crs issues 
crs_issues_d <- crs_issues %>% 
  filter(!grepl("GEODETIC_DATUM_INVALID", issues)) %>% 
  filter(!grepl("COORDINATE_REPROJECTION_SUSPICIOUS", issues)) %>% 
  filter(!grepl("PRESUMED_SWAPPED_COORDINATE") & !references =="") # Retain these, GBIF's algorithm uses the inputted country for comparison so this seems rigirous for what collections intended

###############################################################################################################################

### Temporal Issues ###

grep("date", names(test)) # 16 = date, 
grep("day", names(test)) # 107 = year,  108 = month, 109 = day, # 110 verbatimEventDate
# 103 = eventDate
# as usual we want references 43, occurrenceID 68 , and issues 226
# cat occurrence.txt | cut -f 16,43,68,103,107,108,109,110,226 | grep "RECORDED_DATE_INVALID\|RECORDED_DATE_UNLIKELY\|MODIFIED_DATE_UNLIKELY" > Temporal-Issues.txt

temporal_issues <- read.delim("/home/jt-miller/Globi-Bees-Plant-Interactions/Plant-Data/GBIF-Pull/Uniform-Methods-Data-Pull/0018087-220831081235567/Temporal-Issues.txt", header = FALSE)

temporal_issues <- temporal_issues %>% 
  rename(date = V1) %>% 
  rename(references= V2) %>% 
  rename(occurrenceID = V3) %>% 
  rename(eventDate = V4) %>% 
  rename(year = V5) %>% 
  rename(month = V6) %>% 
  rename(day = V7) %>% 
  rename(verbatimEventDate = V8) %>% 
  rename(issues = V9)

invalid_Date <- temporal_issues %>% 
  filter(grepl("RECORDED_DATE_INVALID", issues)) %>% 
 # filter(!verbatimEventDate == "") # Can only fix if provided 
# It seems that most of these are issues to do with formatting of the date + non-specific entries (i.e. month Year)

# To deal with these we are going to need to:
# Specify months as integers 
# Note dates that are only specific to years, and month + year

## First set up patterns ##
jan <- "Jan|January|Late January"
feb <- "Feb|February"

invalid_Date_f <-  temporal_issues %>% 
  filter(grepl("RECORDED_DATE_INVALID", issues)) %>% 
  filter(!verbatimEventDate == "") %>% 
  replace(verbatimEventDate, str_replace(verbatimEventDate, jan, "01-"))# %>% 
  #replace(verbatimEventDate, str_replace(verbatimEventDate, feb, "02-"))


invalid_Date_f <-  temporal_issues %>% 
  filter(grepl("RECORDED_DATE_INVALID", issues)) %>% 
  filter(!verbatimEventDate == "")# %>% 
  #mutate(verbatimEventDate, ifelse(verbatimEventDate == "jan", "01-", verbatimEventDate))



### Matching for four digit years 


# This is the fix we're going with!

invalid_date_fixed <- invalid_Date %>% 
    filter(grepl("[1-2][0-9][0-9][0-9]", verbatimEventDate)) %>% # Filters for anything that has 4 digits with the beginning being 1-2
    mutate(year2 = str_extract(verbatimEventDate, '(\\d)(\\d)(\\d)(\\d)')) %>% # Extracts 4 consecutive digits 
    mutate(fuzzyDate = case_when(
      grepl('to', verbatimEventDate) ~ TRUE, # Assigns a fuzzyDate = TRUE if a range is present OR there was a present year entered prior that doesnt match the gbif inputted year
      year != year2 ~ TRUE, 
  
      
      TRUE ~ FALSE
    ))

# IF NA, REPLACE
invalid_date_fixed$year <- ifelse(is.na(invalid_date_fixed$year), invalid_date_fixed$year2, invalid_date_fixed$year)

# Removal of Unlikely
invalid_date_fixed <- invalid_date_fixed %>% 
  select(!year2)


 

invalid_Date_f <- lapply(invalid_Date["verbatimEventDate"], function(x) as.numeric(sub("(^\\d+).*", "\\1", x)))

unlikelyDates <- temporal_issues %>% 
  filter(grepl("RECORDED_DATE_UNLIKELY", issues)) 
  # Seems that unlikely data is also caught by the 'to' statements, best to lump these two groups together and perform the same fix as above, subsequently removing data that falls out of logical range 

unlikely_date_fixed <- unlikelyDates %>% 
  filter(!is.na(year) | grepl("[1-2][0-9][0-9][0-9]", verbatimEventDate)) %>% # Grabs anything that isnt NA for year OR Filters for anything that has 4 digits with the beginning being 1-2 in verbatimEventDate
  mutate(year2 = str_extract(verbatimEventDate, '(\\d)(\\d)(\\d)(\\d)')) %>% # Extracts 4 consecutive digits 
  mutate(fuzzyDate = case_when(
    grepl('to', verbatimEventDate) ~ TRUE, # Assigns a fuzzyDate = TRUE if a range is present OR there was a present year entered prior that doesnt match the gbif inputted year
    year != year2 ~ TRUE, 
    grepl('&', verbatimEventDate) ~ TRUE,
    grepl('-', verbatimEventDate) ~ TRUE,
    
    
    TRUE ~ FALSE
  ))

unlikely_date_fixed$year <- ifelse(is.na(unlikely_date_fixed$year), unlikely_date_fixed$year2, unlikely_date_fixed$year)





unlikelyDates_f <- unlikelyDates %>% 
  mutate(eventDateFix = gsub("T00:00:00", "", eventDate)) 

unlikelyDates_f <- unlikelyDates_f %>% 
  mutate(year = lubridate::year(eventDateFix), 
         month = lubridate::month(eventDateFix), 
         day = lubridate::day(eventDateFix)) # Resolved Issue, dates were just wonky 


modifed_unlikely <- temporal_issues %>% 
  filter(grepl("MODIFIED_DATE_UNLIKELY", issues)) # It appears once again that the parser for gbif failed probably due to time being included, lets fix that 


modified_unlikely_date_f <- modified_unlikely_date %>%
  mutate(eventDateFix = gsub("T00:00:00", "", eventDate)) %>% 
  mutate(year = lubridate::year(eventDateFix), 
         month = lubridate::month(eventDateFix), 
         day = lubridate::day(eventDateFix)) # Fixes all of the issues 
  

verbatimQuickview <- read.delim("/home/jt-miller/Globi-Bees-Plant-Interactions/Plant-Data/GBIF-Pull/Uniform-Methods-Data-Pull/0018087-220831081235567/verbatimQuickview.txt")





###############################################################################################################################


### Basis of Record issues ###
type_issues <- read.delim("/home/jt-miller/Globi-Bees-Plant-Interactions/Plant-Data/GBIF-Pull/Uniform-Methods-Data-Pull/0018087-220831081235567/type-issues.txt", header = FALSE)

# Filter out type_issues to those that include the statement TYPE 
type_issues_f <- type_issues %>%
  rename(type = V1) %>% 
  rename(typeStatus = V2) %>% 
  rename(issue = V3) %>% 
  filter(grepl("TYPE", issue))

# Summarize 
unique(type_issues_f$type)
unique(type_issues_f$typeStatus) # Identity of the issue, does not seem to pretain to our study since its very specific. 
unique(type_issues_f$issue)

# BasisOfRecord issues 43 = references, 64 = basisOfRecord, 68 = occurrenceID, 226 = issue
basis_issues <- read.delim("/home/jt-miller/Globi-Bees-Plant-Interactions/Plant-Data/GBIF-Pull/Uniform-Methods-Data-Pull/0018087-220831081235567/Basis-Issues.txt", header = FALSE)

basis_issues <- basis_issues %>% 
  rename(references = V1) %>% 
  rename(basisOfRecord = V2) %>% 
  rename(occurrenceID = V3) %>% 
  rename(issue = V4)

# White space issue?
grepl("^\\s*$", basis_issues$basisOfRecord) # Not a white space issue 

# Appears that "OCCURRENCE" is just too ambigious and therefore is flagged for removal. Manually acertain records using references 
basis_issues_r <- basis_issues %>% 
  filter(!references == "") # Then manually acertain NOTE: This should be done as a last step in our cleaning issues process since its manual.
