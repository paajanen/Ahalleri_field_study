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
MetaCycleData <- read_tsv("analysis/1-metadata.txt", col_names = TRUE)
View(MetaCycleData)
dim(MetaCycleData)




Metadata_for_metacycle <- MetaCycleData %>% select(sample_ID,Plant,Treatment,Timepoint)
View(Metadata_for_metacycle)
numerics_for_metacycle <- MetaCycleData %>% select(-Plant,-Treatment,-Timepoint)
View(numerics_for_metacycle)

September_ambient_2D <- numerics_for_metacycle %>% filter(str_detect(sample_ID, "^S1A"))
December_ambient_2D <- numerics_for_metacycle %>% filter(str_detect(sample_ID, "^D1A"))
March_ambient_2D <- numerics_for_metacycle %>% filter(str_detect(sample_ID, "^M1A"))
June_ambient_2D <- numerics_for_metacycle %>% filter(str_detect(sample_ID, "^J1A"))
September_cold_2D <- numerics_for_metacycle %>% filter(str_detect(sample_ID, "^S1C"))
December_cold_2D <- numerics_for_metacycle %>% filter(str_detect(sample_ID, "^D1C"))
March_cold_2D <- numerics_for_metacycle %>% filter(str_detect(sample_ID, "^M1C"))
June_cold_2D <- numerics_for_metacycle %>% filter(str_detect(sample_ID, "^J1C"))



transpose_dataset <- function(df) {
  df <- df %>%  
    gather(key = var_name, value = value, 2:ncol(df)) %>% 
    spread_(key = names(df)[1],value = 'value')
}


meta2d_Sep_ambient <- transpose_dataset(September_ambient_2D)
meta2d_Dec_ambient <- transpose_dataset(December_ambient_2D)
meta2d_Mar_ambient <- transpose_dataset(March_ambient_2D)
meta2d_Jun_ambient <- transpose_dataset(June_ambient_2D)
meta2d_Sep_cold <- transpose_dataset(September_cold_2D)
meta2d_Dec_cold <- transpose_dataset(December_cold_2D)
meta2d_Mar_cold <- transpose_dataset(March_cold_2D)
meta2d_Jun_cold <- transpose_dataset(June_cold_2D) 

test2d <- head(meta2d_Mar_ambient)

write.csv(test2d, file="test2d.csv", row.names=FALSE)
write.csv(meta2d_Sep_ambient, file="meta2d_Sep_ambient.csv", row.names=FALSE)
write.csv(meta2d_Dec_ambient, file="meta2d_Dec_ambient.csv", row.names=FALSE)
write.csv(meta2d_Mar_ambient, file="meta2d_Mar_ambient.csv", row.names=FALSE)
write.csv(meta2d_Jun_ambient, file="meta2d_Jun_ambient.csv", row.names=FALSE)
write.csv(meta2d_Sep_cold, file="meta2d_Sep_cold.csv", row.names=FALSE)
write.csv(meta2d_Dec_cold, file="meta2d_Dec_cold.csv", row.names=FALSE)
write.csv(meta2d_Mar_cold, file="meta2d_Mar_cold.csv", row.names=FALSE)
write.csv(meta2d_Jun_cold, file="meta2d_Jun_cold.csv", row.names=FALSE)


meta2d(infile="test2d.csv", filestyle="csv",
       outdir="test2d", timepoints=seq(1, 48, by=1))
#Now we can use the meta2d command to get the cyclic behaviours.
#meta2d combines JTK cycle and two other methods for finding rhythmicity.
meta2d(infile="meta2d_Sep_ambient.csv", filestyle="csv",
       outdir="meta2d_Sep_ambient", timepoints=seq(1, 48, by=1))
meta2d(infile="meta2d_Dec_ambient.csv", filestyle="csv",
       outdir="meta2d_Dec_ambient", timepoints=seq(1, 48, by=1))
meta2d(infile="meta2d_Mar_ambient.csv", filestyle="csv",
       outdir="meta2d_Mar_ambient", timepoints=seq(1, 48, by=1))
meta2d(infile="meta2d_Jun_ambient.csv", filestyle="csv",
       outdir="meta2d_Jun_ambient", timepoints=seq(1, 48, by=1))
meta2d(infile="meta2d_Sep_cold.csv", filestyle="csv",
       outdir="meta2d_Sep_cold", timepoints=seq(1, 48, by=1))
meta2d(infile="meta2d_Dec_cold.csv", filestyle="csv",
       outdir="meta2d_Dec_cold", timepoints=seq(1, 48, by=1))
meta2d(infile="meta2d_Mar_cold.csv", filestyle="csv",
       outdir="meta2d_Mar_cold", timepoints=seq(1, 48, by=1))
meta2d(infile="meta2d_Jun_cold.csv", filestyle="csv",
       outdir="meta2d_Jun_cold", timepoints=seq(1, 48, by=1))







###########META3D below, not needed.


Metacycle_data <- transpose_dataset(numerics_for_metacycle)

# write 'MetaCycle_test' and 'Metadata' into two 'csv' files
write.csv(Metacycle_data, file="MetaCycle_AllSeasons.csv",
          row.names=FALSE)
write.csv(Metadata_for_metacycle, file="metadata.csv",
          row.names=FALSE)

# detect circadian transcripts with JTK in studied individuals
meta3d(datafile="MetaCycle_AllSeasons.csv", cycMethodOne="JTK",
       designfile="metadata.csv", outdir="All_Seasons_JTK", outSymbol="All_Seasons_JTK",
       filestyle="csv", design_libColm=1, design_subjectColm=2,
       design_hrColm=4, design_groupColm=3)




### Further possibilities for running MetaCycle, different  methods and phase adjustement or not. 
## Need to explore.
# detect circadian transcripts with JTK in studied individuals
meta3d(datafile="MetaCycle_Dec_Sept_test.csv", cycMethodOne="JTK",
       designfile="metadata.csv", outdir="Sep_Dec", outSymbol="Sept_Dec1",
       filestyle="csv", design_libColm=1, design_subjectColm=2,
       design_hrColm=4, design_groupColm=3)

# detect circadian transcripts with JTK in studied individuals
meta3d(datafile="MetaCycle_test.csv", cycMethodOne="LS",
       designfile="metadata.csv", outdir="exampleARS",
       filestyle="csv", design_libColm=1, design_subjectColm=2,
       design_hrColm=4, design_groupColm=3)

# detect circadian transcripts with JTK in studied individuals
meta3d(datafile="MetaCycle_test.csv", cycMethodOne="JTK",
       designfile="metadata.csv", outdir="exampleJTK2", outSymbol="JTK2",
       adjustPhase="predictedPer",
       filestyle="csv", design_libColm=1, design_subjectColm=2,
       design_hrColm=4, design_groupColm=3)

# detect circadian transcripts with JTK in studied individuals
meta3d(datafile="MetaCycle_test.csv", cycMethodOne="JTK",
       designfile="metadata.csv", outdir="exampleJTK3", outSymbol="JTK3",
       adjustPhase="notAdjusted",
       filestyle="csv", design_libColm=1, design_subjectColm=2,
       design_hrColm=4, design_groupColm=3)






#### Filtering methods for each datatype from here. 
metaCycle_Sept_Cold <-  read_csv("SeptemberData1000_JTK/meta3dGroupID_Cold_metadata.csv", col_names = TRUE) 
metaCycle_Sept_Ambient <- read_csv("SeptemberData1000_JTK/meta3dGroupID_Ambient_metadata.csv", col_names = TRUE) 

MetaCycle_p_value_cold <- metaCycle_Sept_Cold  %>% 
  select(CycID, meta3d_BH.Q) %>% 
  filter(meta3d_BH.Q < 0.0001)
dim(MetaCycle_p_value_cold)
Cold_Genes_September <- MetaCycle_p_value_cold$CycID

MetaCycle_p_value_ambient <- metaCycle_Sept_Ambient  %>% 
  select(CycID, meta3d_BH.Q) %>% 
  filter(meta3d_BH.Q < 0.0001)
dim(MetaCycle_p_value_ambient)
Ambient_Genes_September <-  MetaCycle_p_value_ambient$CycID

length(union(Cold_Genes_September, Ambient_Genes_September))
length(intersect(Cold_Genes_September, Ambient_Genes_September))
length(setdiff(Cold_Genes_September, Ambient_Genes_September))
length(setdiff(Ambient_Genes_September,Cold_Genes_September))

#Check Dim before, this gives us total 32554 genes
September_data_only <- September_data[,-c(1,54)] # Remove the first column and the column NA,54, observation S1C_05
September_data_only_genes <- September_data[,-c(1)] # Remove the first column 
#September_data_only <- remove_missing(September_data)

#Remove rows where expression level of all the samples are less than 2 RPM
Non_zero_expression <- filter_all(September_data_only, all_vars(. > 2))
Non_zero_expression
dim(September_data_non_zero_expression_genes)
#After removing nonexpressed genes, we have 8674 genes left.



#We can continue to fine particular sets of genes, e.g. those that have expression only on cold, or only on ambient temperature.


head(Non_zero_expression[,-1])


