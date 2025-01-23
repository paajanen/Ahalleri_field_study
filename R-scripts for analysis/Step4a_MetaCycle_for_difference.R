#install.packages("MetaCycle")
#install.packages("dplyr")
#install.packages("tidyverse")
#install.packages("readxl")
library(readxl)
library(tidyverse)
library(MetaCycle)
library(dplyr)
library(tidyr)
library(readxl)

#uploading the data from the Step1 cleaned data collection, that is all data, not filtered.
MetaCycleData <- read_tsv("analysis/3-cold-ambient-expression-differences-all-seasons.txt", col_names = TRUE)
#View(MetaCycleData)
dim(MetaCycleData)
#uploading the data from the Step1 cleaned data collection, that is all data, not filtered.
MetaData <- read_tsv("analysis/1-metadata.txt", col_names = TRUE)
#View(MetaData)

#MetaData_header <- MetaData %>% select(-Plant,-Treatment,-Timepoint,-sample_ID) 
  
#names(MetaData_header)
sample_ID <- MetaData %>% filter(Plant %in% c("Ambient_Dec", "Ambient_Jun","Ambient_Sep","Ambient_Mar")) %>% select(sample_ID)
View(sample_ID)
MetaCycleData <- cbind(sample_ID, MetaCycleData)
#names(MetaCycleData) <- names(MetaData_header)
head(MetaCycleData)

Metadata_for_metacycle <- MetaData %>% select(sample_ID,Plant,Treatment,Timepoint) %>% filter(Plant %in% c("Ambient_Dec", "Ambient_Jun","Ambient_Mar","Ambient_Sep"))
#View(Metadata_for_metacycle)
numerics_for_metacycle <- MetaCycleData
#View(numerics_for_metacycle)
names(numerics_for_metacycle)

transpose_dataset <- function(df) {
  df <- df %>%  
    gather(key = var_name, value = value, 2:ncol(df)) %>% 
    spread_(key = names(df)[1],value = 'value')
}

Metacycle_data <- transpose_dataset(numerics_for_metacycle)

# write 'MetaCycle_test' and 'Metadata' into two 'csv' files
write.csv(Metacycle_data, file="MetaCycle_difference_all_seasons.csv",
          row.names=FALSE)
write.csv(Metadata_for_metacycle, file="metadata.csv",
          row.names=FALSE)

# detect circadian transcripts with JTK in studied individuals
meta3d(datafile="MetaCycle_difference_all_seasons.csv", cycMethodOne="JTK",
       designfile="metadata.csv", outdir="difference_all_seasons_JTK", outSymbol="Difference_All_Seasons_JTK",
       filestyle="csv", design_libColm=1, design_subjectColm=2,
       design_hrColm=4, design_groupColm=3)



