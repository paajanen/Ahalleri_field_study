library(readxl)
library(tidyverse)
#library(MetaCycle)
#library(dplyr)
#library(tidyr)
#library(readxl)



#uploading the data from the full dataset collection. 
# I have added the cell in the top left corner called "gene" in the rpm_data_reformatted
# compared to the rpm_data.txt

transcript_data_raw <- read_tsv("data/rpm_data_reformatted.txt", col_names = TRUE) 
#View(transcript_data_raw)
dim(transcript_data_raw)
head(transcript_data_raw)

### Fix the two samples, remove the point S1C_05, because this was lost,
### and ambient and cold 24 in september data. They were swapped for some reason
transcript_data_raw <- transcript_data_raw %>% mutate(S1C_05 = replace(S1C_05, S1C_05 >= 0 , NA))

#December_data_raw <- December_data_raw %>% mutate(S1C_05 = na_if(S1C_05, S1C_05 >= 0))

transcript_data_raw <- rename(transcript_data_raw, S1A24=S1C_24)
transcript_data_raw <- rename(transcript_data_raw, S1C_24=S1A_24)
transcript_data_raw <- rename(transcript_data_raw, S1A_24=S1A24)
NAvalue <- transcript_data_raw %>% select(starts_with("S1C_05"))
#View(NAvalue)
# Swap samples S1A_24 and S1C_24, arrange it by season in order Sep, Dec, Mar, Jun, Ambient first
# Cold treatment afterwars
transcript_data_raw <- transcript_data_raw %>% select(gene,S1A_01:S1A_04,S1A_05,S1A_06:S1A_23,S1A_24,S1A_25:S1A_48,
                             S1C_01:S1C_04,S1C_05,S1C_06:S1C_23,S1C_24:S1C_48,D1A_01:D1C_48, M1A_01:M1C_48, J1A_01:J1C_48)
  
head(transcript_data_raw)  
dim(transcript_data_raw)
#View(transcript_data_raw)



#Separate the names of collection points to be the sample IDs
gene_names <- transcript_data_raw$gene
gene_names
#Take the gene names out
sample_ID  <- names(transcript_data_raw)[-1]
sample_ID

# Takes out the data without gene names
RPM_only <- transcript_data_raw %>% 
  select(starts_with("S"),starts_with("D"),starts_with("M"), starts_with("J")) 
dim(RPM_only)

#SEPARATE THE SEPTEMBER DATA, TRANSFORM IT LOG2, AND TRANSPOSE
#This function can be used to separate experiments by beginning of data point name. For September use S1A, S1C, for December use
# D1A, D1C, and also one can get the cross samples quickly.
choose_experiments <- function(df,x,y) {
Chosen_experiments <- df %>% 
  select(starts_with(x), starts_with(y)) 
  return(Chosen_experiments)
}

### TRANSFORM DATA LOG2, can be used only with data only, so expects input from the precious section.
log2_plus1 <- function(x) (log2(x+1))
log2_plus1(1)
log2_plus1(0)

log_2_transform <- function(df) {
  transmute_all(df, log2_plus1) #Transforms log2(x+1)
}


#September_log2 <- log_2_transform(choose_experiments(December_data_raw,"S1A","S1C"))
#September_data_subset <- cbind(gene_names, September_log2)

## Transposes data, needs to have gene names
transpose_dataset <- function(df) {
df <- df %>%  
  gather(key = var_name, value = value, 2:ncol(df)) %>% 
  spread_(key = names(df)[1],value = 'value')
}


log_transformed_raw_data <- transpose_dataset(cbind(gene_names,log_2_transform(RPM_only)))
names(log_transformed_raw_data)[names(log_transformed_raw_data)=="var_name"] <- "sample_ID"
dim(log_transformed_raw_data)






#Next create a tibble with the metadata
#The first column will be the sample_IDs that we already have. 

plant1 <- rep("Ambient_Sep",48)
plant2 <- rep("Cold_Sep",48)
plant3 <- rep("Ambient_Dec",48)
plant4 <- rep("Cold_Dec",48)
plant5 <- rep("Ambient_Mar",48)
plant6 <- rep("Cold_Mar",48)
plant7 <- rep("Ambient_Jun",48)
plant8 <- rep("Cold_Jun",48)
Plant <- c(plant1,plant2,plant3,plant4,plant5,plant6,plant7, plant8)

#The next column tells which treatment group each sample belongs.
treatment1 <- rep("Ambient",48)
treatment2 <- rep("Cold",48)
Treatment <- c(treatment1,treatment2,treatment1,treatment2,treatment1,treatment2,treatment1,treatment2)
Treatment

#The next column tells what time point has the plant been collected
time1 <- seq(1, 48, by=1)
time2 <- seq(1, 48, by=1)
Timepoint <- c(time1,time2,time1,time2,time1,time2,time1,time2)
Timepoint









metadata <- tibble(sample_ID, Plant, Treatment, Timepoint)
metadata
#View(metadata)
all_data <- merge(metadata, log_transformed_raw_data, by="sample_ID", all=FALSE)
names(all_data)
head(all_data)
dim(all_data)
#View(all_data)
write_tsv(all_data, "analysis/1-metadata_Mar2024.txt")
