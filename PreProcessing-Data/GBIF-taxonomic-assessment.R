library(tidyverse)

# Read in completed data 

occurrences <- read.csv("/home/jt-miller/Globi-Bees-Plant-Interactions/Plant-Data/GBIF-Pull/Uniform-Methods-Data-Pull/0018087-220831081235567/Occurrence-Data-Completed.csv")


occurrences_verbSN<- occurrences %>% 
  filter(!verbatimScientificName == "") %>% 
  select(verbatimScientificName) %>% 
  distinct(verbatimScientificName, .keep_all = TRUE)

write.csv(occurrences_verbSN, "/home/jt-miller/Globi-Bees-Plant-Interactions/Plant-Data/GBIF-Pull/Uniform-Methods-Data-Pull/0018087-220831081235567/NomerResolution/Occurrence-VerbatimSN.csv", row.names = FALSE)


parsed_names <- read.csv("/home/jt-miller/Globi-Bees-Plant-Interactions/Plant-Data/GBIF-Pull/Uniform-Methods-Data-Pull/0018087-220831081235567/NomerResolution/Occurrence-VerbatimSN-Parsed.csv")

parsed_only_names <- parsed_names %>% 
  select(Verbatim) %>% 
  distinct(Verbatim)


verbatim_names <- read.delim("/home/jt-miller/Globi-Bees-Plant-Interactions/Plant-Data/GBIF-Pull/Uniform-Methods-Data-Pull/0018087-220831081235567/verbatim-names.txt", header = TRUE)


# How many occurrences have occurrenceID? 
verbatim_names_w_id <- verbatim_names %>% 
  filter(!occurrenceID == "") # 99.5% do 

# N

verbatim_names_full <- verbatim_names_w_id %>% 
  filter(!scientificName == "" & !scientificNameAuthorship == "") # Only approx 37% of the verbatim dataset has NAME and Authorship filled out 

verbatim_names_GSE <- verbatim_names_w_id %>% 
  filter(!scientificName == "") # The vast majority of names have scientificName filled out 99.9% 

verbatim_names_author_included <- verbatim_names_w_id %>% 
  filter(!scientificNameAuthorship == "") #  Approx only 37.6% include authorship. 

# Do any of the names that do not include a scientificName field have a filled out genus + specificEpithet? 
check <- verbatim_names_w_id %>% 
  filter(scientificName =="") %>% 
  filter(!specificEpithet == "" & !genus == "") # Yes 181 records do have this...fix and merge? 

# IS occurrenceID a truly unique field in our dataset?
distinctIDs <- verbatim_names_w_id %>% 
  distinct(occurrenceID, .keep_all = TRUE)# %>% 
  #count() # 99.9% of them do


### Comparing to see if they are the same 
occurrences_w_ID <- occurrences %>% 
  filter(!occurrenceID == "") %>% 
  distinct(occurrenceID, .keep_all = TRUE) %>% 
  filter(!scientificName == "") %>% 
  select(occurrenceID, scientificName, genus, specificEpithet, infraspecificEpithet)

verbatim_data <- verbatim_names %>%
  filter(!occurrenceID == "") %>% 
  distinct(occurrenceID, .keep_all = TRUE) %>% 
  filter(!scientificName == "") %>% 
  select(occurrenceID, scientificName, genus, specificEpithet, infraspecificEpithet)
  


