### A script for building SQL-Remapping ###

# Load in libraries 
library(sqldf)
library(tidyverse)

### Creating a low functional reproducible example ###

# A df of sample data will be built 

scientificName <- c("Cool plant", "Sad plant", "Happy plant", "Angry plant", "Sad plant", "Sad plant", "Sad plant", "Happy plant", "Happy plant", "Happy plant")

sample_df <- data.frame(scientificName)

sample_df <- sample_df %>% 
  mutate(authorship = case_when(
    scientificName == "Cool plant" ~ "JT",
    scientificName == "Sad plant" ~ "TJ",
    scientificName == "Happy plant" ~ "L.", 
    scientificName == "Angry plant" ~ "Not L."
  ))

verbatimScientificNames <- c("Cool plant", "Sad plant", "Happy plant", "Angry plant")
acceptedNames <- c("Cooler plant", "Sad plant", "Happiest plant", "Angry plant")
authorship <- c("JT", "TJ", "L.", "Not L.")

sample_mapping_table <- data.frame(verbatimScientificNames,acceptedNames, authorship)


names_remapped <- sqldf("SELECT dataset.*, real_name_map.resolvedNames 
FROM sample_df dataset 
LEFT JOIN (SELECT DISTINCT sub_dataset.scientificName, MAX(sub_real_names.acceptedNames) resolvedNames
    FROM sample_df sub_dataset
    LEFT JOIN sample_mapping_table sub_real_names ON sub_dataset.scientificName = sub_real_names.verbatimScientificNames
    GROUP BY sub_dataset.scientificName) real_name_map ON dataset.scientificName = real_name_map.scientificName")

# TO PUT IN ENGLISH: Create a new dataset including all fields. First we want to create a real_name_map variable and assign it the field resolvedNames. 
# From the sample_df (occurrences) I want to join the distinct names from the scientificName field in sampledf on the acceptedNames in our mapping table. Create a variable called 
# sub_real_names built from the acceptedNames in the our mapping_table and assign these to the field resolvedNames
# Now join our sub_real_names ON the sub_dataset's scientificName field based upon the sub_real_names's field verbatimScientificNames
# Group these by the sub_data's scientificName, 
# Take the real_name_map field resolved name and match it by the dataset & real_name_maps scientificName, Appending this resolved name field 

### Check functionality if a name isn't present in name list ###
scientificName <- c("Cool plant", "Sad plant", "Happy plant", "Angry plant", "Sad plant", "Sad plant", "Sad plant", "Happy plant", "Happy plant", "Happy plant", "Hidden plant")

sample_df <- data.frame(scientificName)

sample_df <- sample_df %>% 
  mutate(authorship = case_when(
    scientificName == "Cool plant" ~ "JT",
    scientificName == "Sad plant" ~ "TJ",
    scientificName == "Happy plant" ~ "L.", 
    scientificName == "Angry plant" ~ "Not L.", 
    scientificName == "Hidden plant" ~ "Unk."
  ))

verbatimScientificNames <- c("Cool plant", "Sad plant", "Happy plant", "Angry plant")
acceptedNames <- c("Cooler plant", "Sad plant", "Happiest plant", "Angry plant")
authorship <- c("JT", "TJ", "L.", "Not L.")

sample_mapping_table <- data.frame(verbatimScientificNames,acceptedNames, authorship)

names_remapped <- sqldf("SELECT dataset.*, real_name_map.resolvedNames 
FROM sample_df dataset 
LEFT JOIN (SELECT DISTINCT sub_dataset.scientificName, MAX(sub_real_names.acceptedNames) resolvedNames
    FROM sample_df sub_dataset
    LEFT JOIN sample_mapping_table sub_real_names ON sub_dataset.scientificName = sub_real_names.verbatimScientificNames
    GROUP BY sub_dataset.scientificName) real_name_map ON dataset.scientificName = real_name_map.scientificName")

# Notice that NA is generated for names that do not have a valid mapping path according to our mapping table! 

### Now, to improve our query we need to deal with the problems presented by authorship! ###

# Consider that we have the following names once more...
scientificName <- c("Cool plant", "Sad plant", "Happy plant", "Angry plant", "Sad plant", "Sad plant", "Sad plant", "Happy plant", "Happy plant", "Happy plant", "Hidden plant")
authorship <- c("JT", "TJ", "L.", "Not L.", "TJ", "", "TJ", "Lemark", "L.", "Lemark", "Unk.")

sample_df <- data.frame(scientificName, authorship)

# Create a name mapping table that accounts for authorship denoting scientificName mapping 
verbatimScientificNames <- c("Cool plant", "Sad plant", "Happy plant", "Happy plant", "Angry plant")
acceptedNames <- c("Cooler plant", "Sad plant", "Happiest plant", "Happy plant", "Angry plant")
verbatimAuthorship <- c("JT", "TJ", "L.","Lemark", "Not L.")
acceptedAuthorship <- c("JT", "TJ", "Lin", "Lemark", "Not Linneaus")

sample_mapping_table <- data.frame(verbatimScientificNames,acceptedNames, verbatimAuthorship, acceptedAuthorship)


names_remapped <- sqldf("SELECT dataset.*, real_name_map.resolvedNames 
FROM sample_df dataset 
LEFT JOIN (SELECT DISTINCT sub_dataset.scientificName, MAX(sub_real_names.acceptedNames) resolvedNames
    FROM sample_df sub_dataset
    LEFT JOIN sample_mapping_table sub_real_names ON sub_dataset.scientificName = sub_real_names.verbatimScientificNames
    GROUP BY sub_dataset.scientificName) real_name_map ON dataset.scientificName = real_name_map.scientificName
WHERE dataset.authorship != ''")

# Maybe do this in two steps, 1) We assign authorship to ALL names 2) We run the query requiring both the input of the scientificName and Authorship
scientificName <- c("Cool plant", "Sad plant", "Happy plant", "Angry plant", "Sad plant", "Sad plant", "Sad plant", "Happy plant", "Happy plant", "Happy plant", "Hidden plant", "Cool plant", "Happy plant")
authorship <- c("JT", "TJ", "L.", "Not L.", NA, NA, "TJ", NA, "L.", "Lemark", "Unk.", "TJ", "TJ")
# Create a sparse list representative of occurrence data. 
sample_df <- data.frame(scientificName, authorship)

write.csv(sample_df, "/home/jt-miller/Documents/sql_troubleshooting/sample_df.csv", row.names = FALSE)

verbatimScientificNames <- c("Cool plant", "Sad plant", "Happy plant", "Happy plant", "Angry plant")
acceptedNames <- c("Cooler plant", "Sad plant", "Happiest plant", "Happy plant", "Angry plant")
verbatimAuthorship <- c("JT", "TJ", "L.","Lemark", "Not L.")
acceptedAuthorship <- c("JT", "TJ", "Lin", "Lemark", "Not Linneaus")

sample_mapping_table <- data.frame(verbatimScientificNames,acceptedNames, verbatimAuthorship, acceptedAuthorship)

write.csv(sample_mapping_table, "/home/jt-miller/Documents/sql_troubleshooting/sample_mapping_table.csv", row.names = FALSE)

names_remapped <- sqldf("SELECT dataset.*, real_name_map.resolvedNames 
FROM sample_df dataset 
LEFT JOIN (SELECT DISTINCT sub_dataset.scientificName, MAX(sub_real_names.acceptedNames) resolvedNames
    FROM sample_df sub_dataset
    LEFT JOIN sample_mapping_table sub_real_names ON sub_dataset.scientificName = sub_real_names.verbatimScientificNames
    GROUP BY sub_dataset.scientificName) real_name_map ON dataset.scientificName = real_name_map.scientificName
    WHERE dataset.authorship != 'NA'") # This iteration makes it where if authorship is not present in the data.set, then don't map it...

# We need to make this a bit more sofisticated, WHERE there are two name mappings then pull info from authorship instead. 

names_remapped <- sqldf("SELECT 		dataset.*,
			real_names.acceptedNames
			
FROM 		sample_df dataset
LEFT JOIN 	sample_mapping_table real_names ON dataset.scientificName = real_names.verbatimScientificNames

WHERE		(
				real_names.acceptedNames =	(
												SELECT		MAX(sub_real_names.acceptedNames)
												
												FROM		sample_mapping_table sub_real_names
												
												WHERE		dataset.scientificName = sub_real_names.verbatimScientificNames
												AND 		(IFNULL(dataset.authorship, '') = CASE WHEN IFNULL(dataset.authorship, '') = '' THEN '' ELSE sub_real_names.verbatimAuthorship END
												OR  		IFNULL(dataset.authorship, '') = CASE WHEN IFNULL(dataset.authorship, '') = '' THEN '' ELSE sub_real_names.acceptedAuthorship END)
												
												GROUP BY	sub_real_names.verbatimScientificNames
											)

				OR	(
						NOT EXISTS 	(
										SELECT		1
										
										FROM		sample_mapping_table sub_real_names
										
										WHERE		dataset.scientificName = sub_real_names.verbatimScientificNames
										AND 		(IFNULL(dataset.authorship, '') = CASE WHEN IFNULL(dataset.authorship, '') = '' THEN '' ELSE sub_real_names.verbatimAuthorship END
										OR  		IFNULL(dataset.authorship, '') = CASE WHEN IFNULL(dataset.authorship, '') = '' THEN '' ELSE sub_real_names.acceptedAuthorship END)
									)
						
						AND	real_names.acceptedNames =	(
															SELECT		MAX(sub_real_names.acceptedNames)
															
															FROM		sample_mapping_table sub_real_names
															
															WHERE		dataset.scientificName = sub_real_names.verbatimScientificNames
															
															GROUP BY	sub_real_names.verbatimScientificNames
														)
					)
			)")


# Augmented slightly so that we only use resovled authorships for final mapping...

names_remapped <- sqldf("SELECT 		dataset.*,
			real_names.acceptedNames
			
FROM 		sample_df dataset
LEFT JOIN 	sample_mapping_table real_names ON dataset.scientificName = real_names.verbatimScientificNames

WHERE		(
				real_names.acceptedNames =	(
												SELECT		MAX(sub_real_names.acceptedNames)
												
												FROM		sample_mapping_table sub_real_names
												
												WHERE		dataset.scientificName = sub_real_names.verbatimScientificNames
												AND 	
												  		(IFNULL(dataset.authorship, '') = CASE WHEN IFNULL(dataset.authorship, '') = '' THEN '' ELSE sub_real_names.acceptedAuthorship END)
												
												GROUP BY	sub_real_names.verbatimScientificNames
											)

				OR	(
						NOT EXISTS 	(
										SELECT		1
										
										FROM		sample_mapping_table sub_real_names
										
										WHERE		dataset.scientificName = sub_real_names.verbatimScientificNames
										AND 		
										  		(IFNULL(dataset.authorship, '') = CASE WHEN IFNULL(dataset.authorship, '') = '' THEN '' ELSE sub_real_names.acceptedAuthorship END)
									)
						
						AND	real_names.acceptedNames =	(
															SELECT		MAX(sub_real_names.acceptedNames)
															
															FROM		sample_mapping_table sub_real_names
															
															WHERE		dataset.scientificName = sub_real_names.verbatimScientificNames
															
															GROUP BY	sub_real_names.verbatimScientificNames
														)
					)
			)")

### Alternatively, Until I make a sofisticated SQL query that accounts for conditionals we can use a step wise process to map names ###
# Requirements:
# 1) Any names that will map only to one value and one value only can be looked at just by using verbatimScientificName mapping as with our previous query 
# 2) Any names that will map to multiple names just based upon verbatimScientificName will instead require the conditional of verbatimAuthorship in order to successfully map 

# Build our sample Occurrence data: 
scientificName <- c("Cool plant", "Sad plant", "Happy plant", "Angry plant", "Sad plant", "Sad plant", "Sad plant", "Happy plant", "Happy plant", "Happy plant", "Hidden plant", "Cool plant", "Happy plant", "Happy plant")
authorship <- c("JT", "TJ", "L.", "Not L.", NA, NA, "TJ", NA, "L.", "Lemark", "Unk.", "TJ", "TJ", "")
sample_df <- data.frame(scientificName, authorship)
# Build our sample Mapping Table
verbatimScientificNames <- c("Cool plant", "Sad plant", "Happy plant", "Happy plant", "Angry plant")
acceptedNames <- c("Cooler plant", "Sad plant", "Happiest plant", "Happy plant", "Angry plant")
verbatimAuthorship <- c("JT", "TJ", "L.","Lemark", "Not L.")
acceptedAuthorship <- c("JT", "TJ", "Len.", "Lemark", "Not Linneaus")

sample_mapping_table <- data.frame(verbatimScientificNames,acceptedNames, verbatimAuthorship, acceptedAuthorship)

# Another version for my sanity 
# Build our sample Occurrence data: 
scientificName <- c("AAA", "AAA", "AAA", "AAA", "AAA","AAA")
authorship <- c("L.", "Len.", "Lenark", "L.", "", NA)
sample_df <- data.frame(scientificName, authorship)
# Build our sample Mapping Table
verbatimScientificNames <- c("AAA", "AAA", "AAA")
acceptedNames <- c("AAA", "BBB", "BBB")
verbatimAuthorship <- c("L.", "Len.", "Lenark")
acceptedAuthorship <- c("L.", "Lenark", "Lenark")
sample_mapping_table <- data.frame(verbatimScientificNames,acceptedNames, verbatimAuthorship, acceptedAuthorship)

# Step 1: find all names that are unique in mapping, find all names that are non-unique in mapping and store them as a vector
unique_mapping_table <- sample_mapping_table %>% 
  group_by(verbatimScientificNames) %>% 
  filter(n_distinct(acceptedNames) == 1) # As to be expected, this removes happy plant from our designation list 

nonUnique_mapping_table <- sample_mapping_table %>% 
  group_by(verbatimScientificNames) %>% 
  filter(!n_distinct(acceptedNames) == 1) # Happy plants will change in accepted name dependent on the authorship provided 

# Step 2: Run scientificName SQL query on the occurrence table using unique mappings

names_remapped <- sqldf("SELECT dataset.*, real_name_map.resolvedNames 
FROM sample_df dataset 
LEFT JOIN (SELECT DISTINCT sub_dataset.scientificName, MAX(sub_real_names.acceptedNames) resolvedNames
    FROM sample_df sub_dataset
    LEFT JOIN unique_mapping_table sub_real_names ON sub_dataset.scientificName = sub_real_names.verbatimScientificNames
    GROUP BY sub_dataset.scientificName) real_name_map ON dataset.scientificName = real_name_map.scientificName")
# As intended, we can now map anything in our list that doesn't have unique mappings


# Step 3: Run authorship SQL query on the occurrence table using nonUnique mapping table 
names_remapped_auths <- sqldf("SELECT 		dataset.*,
			real_names.acceptedNames
			
FROM 		sample_df dataset
LEFT JOIN 	nonUnique_mapping_table real_names ON dataset.scientificName = real_names.verbatimScientificNames

WHERE		(
				real_names.acceptedNames =	(
												SELECT		MAX(sub_real_names.acceptedNames)
												
												FROM		sample_mapping_table sub_real_names
												
												WHERE		dataset.scientificName = sub_real_names.verbatimScientificNames
												AND 	
												  		dataset.authorship = acceptedAuthorship
                        OR 
                              dataset.authorship = verbatimAuthorship
              
												
												GROUP BY	sub_real_names.verbatimScientificNames
)
)")

# Now for the moment of truth, join those queries together and compare to "occurrence sample" df to see if it operates to expectation.
part_res_df <- names_remapped

names_remapped_auths <- names_remapped_auths %>% 
  rename(resolvedNames = acceptedNames)

full_res_df <- part_res_df %>% 
  filter(!is.na(resolvedNames)) %>% 
  rbind(names_remapped_auths)

