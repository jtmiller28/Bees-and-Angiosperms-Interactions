### Look at just the names of the data ###
quickview <- read.csv("/home/jt-miller/Globi-Bees-Plant-Interactions/Plant-Data/iDigBio-Pull/Uniform-Methods-Pull/ba15a9dd-d0ba-49ee-8477-7eccbd5a444d/occurrence-quickview.csv")
raw_quickview <- read.csv("/home/jt-miller/Globi-Bees-Plant-Interactions/Plant-Data/iDigBio-Pull/Uniform-Methods-Pull/ba15a9dd-d0ba-49ee-8477-7eccbd5a444d/occurrence-raw-quickview.csv")

# 33 = idigbio.flags, retrieve a full list of this for our occurrence data
flags <- read.csv("/home/jt-miller/Globi-Bees-Plant-Interactions/Plant-Data/iDigBio-Pull/Uniform-Methods-Pull/ba15a9dd-d0ba-49ee-8477-7eccbd5a444d/flags.csv")



#### TEMPORAL ISSUES ####

### For Temporal Issues, we want to access the raw data and look at the following fields 
# 26 = dwc.VerbatimEventDate, 65 = dwc.eventDate, 183 = dwc.verbatimEventDate (?), # 193 = flag_encoding_error, 18 = dcterms.references, 136 = dwc.occurrenceID.
# 18,26,65,136,183,193
# grep for: datecollected_bounds

# Appears that flag_encoding_error is extrememly ambigious, therefore we'll defualt to non-raw data for now 
# 21 = idigbio.eventDate, 64 = occurrenceID, 30 = dwc.eventDate, 33 = idigbio.flags

# cat occurrence.csv | cut -f 21,30,33,64 | grep "datecollected_bounds" > date-bound-flags.csv

date_bounds_flag <- read.csv("/home/jt-miller/Globi-Bees-Plant-Interactions/Plant-Data/iDigBio-Pull/Uniform-Methods-Pull/ba15a9dd-d0ba-49ee-8477-7eccbd5a444d/date-bound-flags.csv", header = FALSE)
date_bounds_flag_less <- date_bounds_flag %>% 
  select(idigbio.eventDate = V21, dwc.eventDate = V30, idigbio.flags = V33, dwc.occurrenceID = V64, dwc.verbatimEventDate = V79)

# IF dwc.verbatimEventDate is filled we could possibly fix some 

date_bounds_flags_d <- date_bounds_flag_less %>% 
  filter(!dwc.verbatimEventDate == "") %>% 
  filter(!grepl("1/1/0001", dwc.verbatimEventDate))

# 4 = dwc.basisOfRecord 
# 79 = dwc.verbatimEventDate
# 21 = idigbio.eventDate
# 30 = dwc.eventDate
# 64 = dwc.occurrenceID
# 37 = idigbio.geoPoint
# 70 = dwc.scientificName 
# 33 = idigbio.flags
# 75 = dwc.taxonomicStatus
# 48 = dwc.kingdom
# 66 = dwc.phylum
# 8 = dwc.class
# 65 = dwc.order
# 31 = dwc.family 
# 35 = dwc.genus
# 71 = dwc.specificEpithet 

# Compiled List: 4,8,21,30,31,33,35,37,48,65,66,70,71,75,79
# Change occurrence.csv > occurrence.txt: cat occurrence.csv | sed -E 's/("([^"]*)")?,/\2\t/g' > occurrence.txt
# Bash query: cat occurrence.txt | cut -f 4,8,21,30,31,33,35,37,48,65,66,70,71,75,79 | grep "datecollected_bounds\|dwc_basisofrecord_invalid\|dwc_basisofrecord_removed\|geopoint_datum_error\|geopoint_similar_coord\|rev_geocode_both_sign\|rev_geocode_corrected\|rev_geocode_failure\|rev_geocode_flip\|rev_geocode_flip_both_sign\|rev_geocode_mismatchz\|" > Occurrences-w-issues-for-tree.csv
# Bash query for non-issued data: cat occurrence.txt | cut -f 4,8,21,30,31,33,35,37,48,65,66,70,71,75,79 | grep -v "datecollected_bounds\|dwc_basisofrecord_invalid\|dwc_basisofrecord_removed\|geopoint_datum_error\|geopoint_similar_coord\|rev_geocode_both_sign\|rev_geocode_corrected\|rev_geocode_failure\|rev_geocode_flip\|rev_geocode_flip_both_sign\|rev_geocode_mismatchz\|" > Occurrences-without-issues-for-tree.csv
issue_occurrences <- read.csv("/home/jt-miller/Globi-Bees-Plant-Interactions/Plant-Data/iDigBio-Pull/Uniform-Methods-Pull/ba15a9dd-d0ba-49ee-8477-7eccbd5a444d/Occurrences-w-issues-for-tree.csv")
# For some reason csv's dont grep correctly?

occurrences <- readr::read_csv("/home/jt-miller/Globi-Bees-Plant-Interactions/Plant-Data/iDigBio-Pull/Uniform-Methods-Pull/ba15a9dd-d0ba-49ee-8477-7eccbd5a444d/occurrence.csv")

occurrences <- occurrences %>% 
  select(`dwc:basisOfRecord`, `dwc:verbatimEventDate`, `idigbio:eventDate`, `dwc:eventDate`, `dwc:occurrenceID`, `idigbio:geoPoint`, `dwc:scientificName`, `idigbio:flags`, `dwc:taxonomicStatus`, `dwc:kingdom`, `dwc:phylum`, `dwc:class`, `dwc:order`, `dwc:family`, `dwc:genus`,`dwc:specificEpithet`)

write.csv(occurrences, "/home/jt-miller/Globi-Bees-Plant-Interactions/Plant-Data/iDigBio-Pull/Uniform-Methods-Pull/ba15a9dd-d0ba-49ee-8477-7eccbd5a444d/occurrence-select-fields.csv")

# Grab the occurrences that contain issues 
issue_occurrences <- occurrences %>% 
  filter(grepl("datecollected_bounds|dwc_basisofrecord_invalid|dwc_basisofrecord_removed|geopoint_datum_error|geopoint_similar_coord|rev_geocode_both_sign|rev_geocode_corrected|rev_geocode_failure|rev_geocode_flip|rev_geocode_flip_both_sign|rev_geocode_mismatchz", `idigbio:flags`))

# Grab the occurrences that are clear of issues 
non_issue_occurrences <- occurrences %>% 
  filter(!grepl("datecollected_bounds|dwc_basisofrecord_invalid|dwc_basisofrecord_removed|geopoint_datum_error|geopoint_similar_coord|rev_geocode_both_sign|rev_geocode_corrected|rev_geocode_failure|rev_geocode_flip|rev_geocode_flip_both_sign|rev_geocode_mismatchz", `idigbio:flags`))


### Spatial Resolution Flags ###
occurrences_spatial_issues_removed <- issue_occurrences %>% 
  filter(grepl("geopoint_datum_error", `idigbio:flags`))# %>%  # Remove anything with invalid Datums 
  #filter(!grepl("rev_geocode_failure", `idigbio:flags`)) # Remove anything that fails to relocate in listed coutnry

datum_info <- read.csv("/home/jt-miller/Globi-Bees-Plant-Interactions/Plant-Data/iDigBio-Pull/Uniform-Methods-Pull/ba15a9dd-d0ba-49ee-8477-7eccbd5a444d/Datum-info.csv", quote = "")
