library(tidyverse)
library(grid)
library(readxl)
library(dplyr)
library(ggplot2)
library(ggrepel)

#install.packages("corrr")
library('corrr')
#install.packages("ggcorrplot")
library(ggcorrplot)
#install.packages("FactoMineR")
library("FactoMineR")

rpm <- read_tsv("rpm_data_incl_control.txt", col_names=TRUE)


View(rpm)



gene_names <- rpm %>% select("gene")


control_samples <- rpm %>% select("S1A_09","S1A_21","S1A_08","S1A_20","S1A_10","S1A_22",
                                  "D1A_09","D1A_21","D1A_08","D1A_20","D1A_10","D1A_22",
                                  "M1A_09","M1A_21","M1A_08","M1A_20","M1A_10","M1A_22",
                                  "J1A_09","J1A_21","J1A_08","J1A_20","J1A_10","J1A_22",
               "SC1_01","SC1_02","SC1_03","SC1_04","SC1_05","SC1_06",
               "DC1_01","DC1_02","DC1_03","DC1_04","DC1_05","DC1_06",
               "MC1_01","MC1_02","MC1_03","MC1_04","MC1_05","MC1_06",
               "JC1_01","JC1_02","JC1_03","JC1_04","JC1_05","JC1_06")

sample_ID <- c("SN","SD","SN-1","SD-1","SN+1","SD+1",
               "DN","DD","DN-1","DD-1","DN+1","DD+1",
               "MN","MD","MN-1","MD-1","MN+1","MD+1",
               "JN","JD","JN-1","JD-1","JN+1","JD+1",
               "SNC1","SNC2","SNC3","SDC1","SDC2","SDC3",
               "DNC1","DNC2","DNC3","DDC1","DDC2","DDC3",
               "MNC1","MNC2","MNC3","MDC1","MDC2","MDC3",
               "JNC1","JNC2","JNC3","JDC1","JDC2","JDC3")

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


log_transformed_raw_data <- transpose_dataset(cbind(gene_names,log_2_transform(control_samples)))
names(log_transformed_raw_data)[names(log_transformed_raw_data)=="var_name"] <- "sample_ID"
dim(log_transformed_raw_data)

Time <- c("00:00","12:00","23:00","11:00","01:00","13:00",
          "00:00","12:00","23:00","11:00","01:00","13:00",
          "00:00","12:00","23:00","11:00","01:00","13:00",
          "00:00","12:00","23:00","11:00","01:00","13:00",
          "00:00", "00:00", "00:00", "12:00", "12:00", "12:00",
          "00:00", "00:00", "00:00", "12:00", "12:00", "12:00",
          "00:00", "00:00", "00:00", "12:00", "12:00", "12:00",
          "00:00", "00:00", "00:00", "12:00", "12:00", "12:00")




Season <- c("September","September","September","September","September","September",
            "December","December","December","December","December","December",
            "March","March","March","March","March","March",
            "June","June","June","June","June","June",
            "September","September","September","September","September","September",
            "December","December","December","December","December","December",
            "March","March","March","March","March","March",
            "June","June","June","June","June","June")


Control <- c("Timeseries","Timeseries","Timeseries","Timeseries","Timeseries","Timeseries",
             "Timeseries","Timeseries","Timeseries","Timeseries","Timeseries","Timeseries",
             "Timeseries","Timeseries","Timeseries","Timeseries","Timeseries","Timeseries",
             "Timeseries","Timeseries","Timeseries","Timeseries","Timeseries","Timeseries",
            "Control","Control","Control","Control","Control","Control",
            "Control","Control","Control","Control","Control","Control",
            "Control","Control","Control","Control","Control","Control",
            "Control","Control","Control","Control","Control","Control")


metadata <- tibble(sample_ID, Time, Season,Control)
#View(metadata)

all_data <- merge(metadata, log_transformed_raw_data, by="sample_ID", all=FALSE)

#View(all_data)

#sets.bar.color=c( "#FFD535","#860124", "#FB2372","#76AFB6"))




data.pca <- princomp(control_samples)
summary(data.pca)



comps <- data.pca$loadings[, 1:3]
comps
data_names <- cbind(sample_ID, comps,Time, Season,Control)
data_fr <- as.data.frame(data_names)

data_fr

PCA <- ggplot(data=data_fr, aes(x=Comp.1,y=Comp.2,label=sample_ID, fill=Season, color=Season,shape=Time, size=Control)) + 
  #geom_point(size=2,stroke = 1.75) +
  geom_point()+
  geom_text_repel(size = 1.5,max.overlaps = 100)+
  #scale_fill_manual(values=c( "grey","black"))+
  scale_color_manual(values=c( "#76AFB6","#FFD535","#FB2372","#860124","#FB2372","#860124","#76AFB6","#FFD535","#FB2372","#860124"))+
  scale_fill_manual(values=c( "#76AFB6","#FFD535","#FB2372","#860124","#FB2372","#860124","#76AFB6","#FFD535","#FB2372","#860124"))+
  scale_size_manual(values=c(2,5,6,7,8))+
  scale_shape_manual(values=c(20,21,22,23,24,25)) +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ # THE BACKGROUND OF THE GRAPH, NO GRID
  theme(axis.line = element_line(colour = "black", size = 1)) + # DRAWING THE BLACK LINES AROUND THE PLOT
  theme(axis.text.x=element_blank(),axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),axis.ticks.y=element_blank())+
  scale_x_discrete("PCA1, 69.9%")+
  scale_y_discrete(("PCA2, 7.1%"))+
  coord_fixed(ratio=1)

PCA


ggsave(plot = PCA, "PCA_2025_03_11.pdf", width = 6, height = 4, dpi = "retina")


