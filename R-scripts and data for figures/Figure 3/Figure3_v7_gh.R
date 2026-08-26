library(readxl)
library(tidyverse)
library(dplyr)
library(tidyr)
library(readxl)
#install.packages("gridExtra")
library(gridExtra)
#install.packages("dtw")
library(ggpubr)
library(dtw) 
library(ggplot2)


####### This prints the Figure 1 panels, made on 8.1.2026
####### THIS IS A PRINTING SCRIPT, IT HAS THE SHADES FOR NIGHT, AND AVERAGE
#TEMPERATURES
#OUTPUTS GRAPHS
#NEEDS OTHER ENVIRONMENTAL DATA
# COULD MAKE THE SHADES VISIBLE IF PLOTTING MORE THAN ONE SEASON
### NEEDS IMPROVEMENT, THE SHADING FOR NIGHT SHOULD AUTOMATIC
# NICER COLOURS FOR DIFFERENT SEASONS
# THE LABEL SHOULD HAVE THE COMMON OF NAME OF THE TRANSCRIPT AS WELL, A. THALIANA ORTHOLOGUE


##USING metadata_Mar2024
transcript_data_annotated <- read_tsv("analysis/1-metadata_Mar2024.txt", col_names = TRUE) 

dim(transcript_data_annotated)
#View(transcript_data_annotated)
weather_data <- read_csv("data/Sample_environ_data_long_format.csv", col_names=TRUE)
#View(weather_data)
weather <- weather_data %>% select("sample_ID", "Temperature")

#View(transcript_data_annotated)
dim(transcript_data_annotated)
head(transcript_data_annotated)
all_data <- merge(transcript_data_annotated,weather , by="sample_ID", all=FALSE)

all_data_arranged <- all_data  %>% arrange(factor(Plant, levels = c("Ambient_Sep","Ambient_Dec","Ambient_Mar","Ambient_Jun",
                                                                    "Cold_Sep","Cold_Dec","Cold_Mar","Cold_Jun")))


#View(all_data_arranged)
#pivot_longer(weather_data, !Season,names_to ="Sample", values_to ="Temperature" )
#df.long <- pivot_longer(weather_data, cols=c(S1_Amb,S1_Cold,D1_Amb,D1_Cold), names_to = "Season", values_to = "Temperature")
#df.long


#Sample_ID <- transcript_data_annotated %>% select("sample_ID")
#S1_Amb <- weather_data %>% select("Sample", "S1_Amb")
#S1_Cold <- weather_data %>% select("S1_Cold")
#D1_Amb <- weather_data %>% select("D1_Amb")
#D1_Cold <- weather_data %>% select("D1_Cold")
#M1_Amb <- weather_data %>% select("M1_Amb")
#M1_Cold <- weather_data %>% select("M1_Cold")
#J1_Amb <- weather_data %>% select("J1_Amb")
#J1_Cold <- weather_data %>% select("J1_Cold")


# THESE ARE THE SUNRISE AND SUNSET TIMES FOR EACH SEASON TO GET THE NIGHT-TIME SHADING CORRECTLY IN THE PLOTS

September
December
March
June
Sunrise
05:20
06:40
05:50
04:20
Sunset
18:30
17:00
18:10
19:10
sep_rects <- data.frame(xstart = c(0,3.30,14.20,27.30,38.20), xend = c(3.30,14.20,27.30,38.20,49),
                        col = c("white","gray50","white","gray50","white"))
# Changed every number by 1, to make it better


sep_rects_irradiance <- data.frame(xstart = c(0,4,14,28,38), xend = c(4,14,28,38,48),
                                   col = c("white","gray50","white","gray50","white"))

dec_rects <- data.frame(xstart = c(0,1,14.40,25,38.40), xend = c(1,14.40,25,38.40,48),
                        col = c("white","gray50","white","gray50","white"))

dec_rects_irradiance <- data.frame(xstart = c(0,3,15,27,39), xend = c(3,15,27,39,48),
                                   col = c("white","gray50","white","gray50","white"))



mar_rects <- data.frame(xstart = c(0,2.10,13.50,26.10,37.50), xend = c(2.10,13.50,26.10,37.50,48),
                        col = c("white","gray50","white","gray50","white"))

mar_rects_irradiance <- data.frame(xstart = c(0,4,14,28,38), xend = c(4,14,28,38,48),
                                   col = c("white","gray50","white","gray50","white"))


jun_rects <- data.frame(xstart = c(0,3.10,12.20,27.10,36.20), xend = c(3.10,12.20,27.10,36.20,48),
                        col = c("white","gray50","white","gray50","white"))

jun_rects_irradiance <- data.frame(xstart = c(0,5,13,29,37), xend = c(5,13,29,37,48),
                                   col = c("white","gray50","white","gray50","white"))





September: "#860124"
December: "#76AFB6"
March: "#FB2372"
June: "#FFD535"
plot_jun_irradiance <- function(df,transcript,...) {
  arguments <- list(...)
  filtered <- df %>% filter(Plant %in% arguments)
  p <- c()
  for (i in 1:length(arguments)){  #CHANGE THE COLOURS YOU SEE FIT
    if(arguments[i] == "Ambient_Dec") p[i] = "#860124"
    if(arguments[i] == "Ambient_Sep") p[i] = "#860124"
    if(arguments[i] == "Ambient_Mar") p[i] = "#860124"
    if(arguments[i] == "Ambient_Jun") p[i] = "#FFD535"
    if(arguments[i] == "Cold_Dec") p[i] = "#2f5597"
    if(arguments[i] == "Cold_Sep") p[i] = "#2f5597"
    if(arguments[i] == "Cold_Mar") p[i] = "#2f5597"
    if(arguments[i] == "Cold_Jun") p[i] = "#2f5597"
  }
  chosen_colours <- p
  plot <- ggplot() + # FOR DIFFERENT SEASONS, UNCHECK AND CHECK THESE GEOM_RECTS.
    #geom_rect(data = dec_rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.3)+
    #geom_rect(data = mar_rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.3)+
    #geom_rect(data = sep_rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.3)+
    geom_rect(data = jun_rects_irradiance, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.3)+
    scale_fill_manual(values=c("darkgrey","white","white"), guide=FALSE)+ #THE NIGHT RECTANLGES DONE!
    geom_line(data = filtered , aes(x = Timepoint, y = get(transcript),colour=Plant), size=1.5)+ # THE TRANSCRIPT LEVEL
    geom_line(data = filtered , aes(x = Timepoint, y = Temperature/2,colour=Plant),linetype = "dashed", size=1.5)+ # THE TEMPERATURE
    scale_x_continuous(expand = c(0, 0),breaks=seq(1,49,by=8) ,labels = c("16","00","08","16","00","08","16"))+ # PUTS THE LITTLE TICKS ON THE X-AXIS
    scale_color_manual(values=chosen_colours)+ # THE COLOURS YOU DEFINED AT THE BEGINNING
    geom_point(data = filtered , aes(x = Timepoint, y =get(transcript),colour=Plant), size=1.5, shape=1, alpha= 0.6)+ # PUTTING LITTLE CIRCLES ROUND THE TRANSCRIPT LINE
    coord_fixed(ratio=1.5, xlim=c(0.9,48.1), ylim=c(-0.1,16.5))+ # SCALING THE PLOTS RATIOS
    labs(x=" ")+ # THE X-LABEL
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ # THE BACKGROUND OF THE GRAPH, NO GRID
    theme(axis.line = element_line(colour = "black", size = 1))  + # DRAWING THE BLACK LINES AROUND THE PLOT
    theme(legend.position="none")+ # THE COLOUR CODING BY PLANT POSITION
    scale_y_continuous(expand = c(0, 0),
                       expression("TPM"),
                       sec.axis = sec_axis(~ . * 2.4, name = "Temperature ", breaks = seq(0,36,7)))+ # THE NAMES OF THE Y-AXIS LABELS, BOTH SIDES
    theme(axis.text=element_text(size=20, colour = "black"),
          axis.title=element_text(size=16,face="bold"))
  #ggtitle(transcript)  +  # CHANGE THE TITLE OF THE GRAPH
  return(plot)
}

plot_jun_irradiance(all_data,"g09776.t1", "Ambient_Jun", "Cold_Jun")
plot_jun_irradiance(all_data,"g09776.t1", "Ambient_Sep", "Cold_Sep")
plot_jun_irradiance(all_data,"g09776.t1", "Ambient_Dec", "Cold_Dec")
plot_jun_irradiance(all_data,"g09776.t1", "Ambient_Mar", "Cold_Mar")

g09776.t1
g09640.t1

CCA1 <- plot_jun_irradiance(all_data,"g09640.t1", "Ambient_Jun", "Cold_Jun") #CCA1, class 13
ggsave(plot = CCA1, "CCA1_clock_club.pdf", width = 6, height = 4, dpi = "retina")

PRR9_June_Fig1_23_12_12 <- plot_jun_irradiance(all_data,"g09644.t1", "Ambient_Jun", "Cold_Jun") #PRR9, class 13
LUX_June_Fig1_23_12_12 <-plot_jun_irradiance(all_data,"g12727.t1", "Ambient_Jun", "Cold_Jun") #LUX,

ggsave(plot = PRR9_June_Fig1_23_12_12, "PRR9_June_Fig1_23_12_12.pdf", width = 6, height = 4, dpi = "retina")
ggsave(plot =LUX_June_Fig1_23_12_12, "LUX_June_Fig1_23_12_12.pdf", width = 6, height = 4, dpi = "retina")





#Lower panel 

transcript_data_imputed <- read_tsv("analysis/1-metadata_imputed.txt", col_names = TRUE) 

library(zoo)


smoothed <- transcript_data_imputed  %>%
  group_by(Plant) %>%
  mutate_at(vars(-group_cols(),-Treatment,-Timepoint,-sample_ID), 
            ~ rollapplyr(.x, list(-seq(5)), mean, na.rm = FALSE, partial = TRUE, fill = NA)) %>%
  ungroup

write.table(smoothed, file='smoothed_data5_20241002.tsv', quote=FALSE, sep='\t', row.names = FALSE)

smoothed <- read_tsv("smoothed_data5_20241002.tsv", col_names = TRUE) 



weather_data <- read_csv("data/Sample_environ_data_long_format.csv", col_names=TRUE)
weather <- weather_data %>% dplyr::select("sample_ID", "Temperature")


transcript_data_smoothed <- smoothed %>% filter(Timepoint!=1)
all_data_smoothed <- merge(transcript_data_smoothed,weather , by="sample_ID", all=FALSE)

# THESE ARE THE SUNRISE AND SUNSET TIMES FOR EACH SEASON TO GET THE NIGHT-TIME SHADING CORRECTLY IN THE PLOTS


sep_rects_irradience <- data.frame(xstart = c(0,3.30,14.20,27.30,38.20), xend = c(3.30,14.20,27.30,38.20,49),
                                   col = c("white","gray50","white","gray50","white"))


sep_rects_irradience_irradiance <- data.frame(xstart = c(0,4,14,28,38), xend = c(4,14,28,38,48),
                                              col = c("white","gray50","white","gray50","white"))

dec_rects_irradience <- data.frame(xstart = c(0,1,14.40,25,38.40), xend = c(1,14.40,25,38.40,48),
                                   col = c("white","gray50","white","gray50","white"))

dec_rects_irradience_irradiance <- data.frame(xstart = c(0,3,15,27,39), xend = c(3,15,27,39,48),
                                              col = c("white","gray50","white","gray50","white"))



mar_rects_irradience <- data.frame(xstart = c(0,2.10,13.50,26.10,37.50), xend = c(2.10,13.50,26.10,37.50,48),
                                   col = c("white","gray50","white","gray50","white"))

mar_rects_irradience_irradiance <- data.frame(xstart = c(0,4,14,28,38), xend = c(4,14,28,38,48),
                                              col = c("white","gray50","white","gray50","white"))


jun_rects_irradience <- data.frame(xstart = c(0,3.10,12.20,27.10,36.20), xend = c(3.10,12.20,27.10,36.20,48),
                                   col = c("white","gray50","white","gray50","white"))

jun_rects_irradience_irradiance <- data.frame(xstart = c(0,5,13,29,37), xend = c(5,13,29,37,48),
                                              col = c("white","gray50","white","gray50","white"))



# THESE ARE THE SUNRISE AND SUNSET TIMES FOR EACH SEASON TO GET THE NIGHT-TIME SHADING CORRECTLY IN THE PLOTS



Choose_transcript_and_plot_PRC_Sep <- function(df,title,transcript,season1,season2) {
  template <- all_data %>% dplyr::select(transcript,"Plant") %>% filter(Plant==season1) %>% dplyr::pull(transcript)
  query <- all_data %>% dplyr::select(transcript, "Plant") %>% filter(Plant==season2) %>% dplyr::pull(transcript)
  alignment<-dtw(query,template,keep=TRUE);
  ## Display the warping curve, i.e. the alignment curve
  #plot(alignment,type="threeway")
  ref_frame <- enframe(alignment$index1)
  phase_shift <- enframe(alignment$index1-alignment$index2)
  phase_frame <- merge(ref_frame,phase_shift , by="name", all=FALSE)
  plot <- ggplot() +   geom_line(data = phase_frame , aes(x = value.x, y = -value.y), linewidth=0.5) + # THE TRANSCRIPT LEVEL
    geom_line(data = phase_frame , aes(x = value.x, y =0), linewidth=0.5, linetype = "dashed")+
    geom_rect(data = sep_rects_irradience, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.3)+
    #geom_rect(data = jun_rects_irradience_irradiance, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.3)+
    scale_fill_manual(values=c("darkgrey","white","white"), guide=FALSE)+ #THE NIGHT RECTANLGES DONE!
    scale_x_continuous(expand = c(0, 0),breaks=seq(1,49,by=8) ,labels = c("16","00","08","16","00","08","16"))+ # PUTS THE LITTLE TICKS ON THE X-AXIS
    coord_fixed(ratio=1.5, xlim=c(0.9,48.1), ylim=c(-10,10))+ # SCALING THE PLOTS RATIOS
    labs(x=" ")+ # THE X-LABEL
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ # THE BACKGROUND OF THE GRAPH, NO GRID
    theme(axis.line = element_line(colour = "black", size = 1))  + # DRAWING THE BLACK LINES AROUND THE PLOT
    theme(legend.position="none")+ # THE COLOUR CODING BY PLANT POSITION
    scale_y_continuous(expand = c(0, 0),
                       expression("Phase shift of the cold treatment (h)"))+
    #sec.axis = sec_axis(~ . * 2.4, name = "Temperature ", breaks = seq(0,36,7)))+ # THE NAMES OF THE Y-AXIS LABELS, BOTH SIDES
    theme(axis.text=element_text(size=2, colour = "black"),
          axis.title=element_text(size=1.6,face="bold"))+
    ggtitle(title)  +
    theme(plot.title = element_text(size = 4, face = "bold"))  # CHANGE THE TITLE OF THE GRAPH
  return(plot)
}

Choose_transcript_and_plot_PRC_Dec <- function(df,title,transcript,season1,season2) {
  template <- all_data %>% dplyr::select(transcript,"Plant") %>% filter(Plant==season1) %>% dplyr::pull(transcript)
  query <- all_data %>% dplyr::select(transcript, "Plant") %>% filter(Plant==season2) %>% dplyr::pull(transcript)
  alignment<-dtw(query,template,keep=TRUE);
  ## Display the warping curve, i.e. the alignment curve
  #plot(alignment,type="threeway")
  ref_frame <- enframe(alignment$index1)
  phase_shift <- enframe(alignment$index1-alignment$index2)
  phase_frame <- merge(ref_frame,phase_shift , by="name", all=FALSE)
  plot <- ggplot() +   geom_line(data = phase_frame , aes(x = value.x, y = -value.y), linewidth=0.5) + # THE TRANSCRIPT LEVEL
    geom_line(data = phase_frame , aes(x = value.x, y =0), linewidth=0.5, linetype = "dashed")+
    geom_rect(data = dec_rects_irradience, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.3)+
    scale_fill_manual(values=c("darkgrey","white","white"), guide=FALSE)+ #THE NIGHT RECTANLGES DONE!
    scale_x_continuous(expand = c(0, 0),breaks=seq(1,49,by=8) ,labels = c("16","00","08","16","00","08","16"))+ # PUTS THE LITTLE TICKS ON THE X-AXIS
    coord_fixed(ratio=1.5, xlim=c(0.9,48.1), ylim=c(-10,10))+ # SCALING THE PLOTS RATIOS
    labs(x=" ")+ # THE X-LABEL
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ # THE BACKGROUND OF THE GRAPH, NO GRID
    theme(axis.line = element_line(colour = "black", size = 1))  + # DRAWING THE BLACK LINES AROUND THE PLOT
    theme(legend.position="none")+ # THE COLOUR CODING BY PLANT POSITION
    scale_y_continuous(expand = c(0, 0),
                       expression("Phase shift of the cold treatment (h)"))+
    #sec.axis = sec_axis(~ . * 2.4, name = "Temperature ", breaks = seq(0,36,7)))+ # THE NAMES OF THE Y-AXIS LABELS, BOTH SIDES
    theme(axis.text=element_text(size=2, colour = "black"),
          axis.title=element_text(size=1.6,face="bold"))+
    ggtitle(title)  +
    theme(plot.title = element_text(size = 4, face = "bold"))  # CHANGE THE TITLE OF THE GRAPH
  return(plot)
}

Choose_transcript_and_plot_PRC_Mar <- function(df,title,transcript,season1,season2) {
  template <- all_data %>% dplyr::select(transcript,"Plant") %>% filter(Plant==season1) %>% dplyr::pull(transcript)
  query <- all_data %>% dplyr::select(transcript, "Plant") %>% filter(Plant==season2) %>% dplyr::pull(transcript)
  alignment<-dtw(query,template,keep=TRUE);
  ## Display the warping curve, i.e. the alignment curve
  #plot(alignment,type="threeway")
  ref_frame <- enframe(alignment$index1)
  phase_shift <- enframe(alignment$index1-alignment$index2)
  phase_frame <- merge(ref_frame,phase_shift , by="name", all=FALSE)
  plot <- ggplot() +   geom_line(data = phase_frame , aes(x = value.x, y = -value.y), linewidth=0.5) + # THE TRANSCRIPT LEVEL
    geom_line(data = phase_frame , aes(x = value.x, y =0), linewidth=0.5, linetype = "dashed")+
    geom_rect(data = mar_rects_irradience, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.3)+
    scale_fill_manual(values=c("darkgrey","white","white"), guide=FALSE)+ #THE NIGHT RECTANLGES DONE!
    scale_x_continuous(expand = c(0, 0),breaks=seq(1,49,by=8) ,labels = c("16","00","08","16","00","08","16"))+ # PUTS THE LITTLE TICKS ON THE X-AXIS
    coord_fixed(ratio=1.5, xlim=c(0.9,48.1), ylim=c(-10,10))+ # SCALING THE PLOTS RATIOS
    labs(x=" ")+ # THE X-LABEL
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ # THE BACKGROUND OF THE GRAPH, NO GRID
    theme(axis.line = element_line(colour = "black", size = 1))  + # DRAWING THE BLACK LINES AROUND THE PLOT
    theme(legend.position="none")+ # THE COLOUR CODING BY PLANT POSITION
    scale_y_continuous(expand = c(0, 0),
                       expression("Phase shift of the cold treatment (h)"))+
    #sec.axis = sec_axis(~ . * 2.4, name = "Temperature ", breaks = seq(0,36,7)))+ # THE NAMES OF THE Y-AXIS LABELS, BOTH SIDES
    theme(axis.text=element_text(size=2, colour = "black"),
          axis.title=element_text(size=1.6,face="bold"))+
    ggtitle(title)  +
    theme(plot.title = element_text(size = 4, face = "bold"))  # CHANGE THE TITLE OF THE GRAPH
  return(plot)
}

Choose_transcript_and_plot_PRC_Jun <- function(df,title,transcript,season1,season2) {
  template <- all_data %>% dplyr::select(transcript,"Plant") %>% filter(Plant==season1) %>% dplyr::pull(transcript)
  query <- all_data %>% dplyr::select(transcript, "Plant") %>% filter(Plant==season2) %>% dplyr::pull(transcript)
  alignment<-dtw(query,template,keep=TRUE);
  ## Display the warping curve, i.e. the alignment curve
  #plot(alignment,type="threeway")
  ref_frame <- enframe(alignment$index1)
  phase_shift <- enframe(alignment$index1-alignment$index2)
  phase_frame <- merge(ref_frame,phase_shift , by="name", all=FALSE)
  plot <- ggplot() +   geom_line(data = phase_frame , aes(x = value.x, y = -value.y), linewidth=0.5) + # THE TRANSCRIPT LEVEL
    geom_line(data = phase_frame , aes(x = value.x, y =0), linewidth=0.5, linetype = "dashed")+
    geom_rect(data = jun_rects_irradience, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.3)+
    scale_fill_manual(values=c("darkgrey","white","white"), guide=FALSE)+ #THE NIGHT RECTANLGES DONE!
    scale_x_continuous(expand = c(0, 0),breaks=seq(1,49,by=8) ,labels = c("16","00","08","16","00","08","16"))+ # PUTS THE LITTLE TICKS ON THE X-AXIS
    coord_fixed(ratio=1.5, xlim=c(0.9,48.1), ylim=c(-10,10))+ # SCALING THE PLOTS RATIOS
    labs(x=" ")+ # THE X-LABEL
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ # THE BACKGROUND OF THE GRAPH, NO GRID
    theme(axis.line = element_line(colour = "black", size = 1))  + # DRAWING THE BLACK LINES AROUND THE PLOT
    theme(legend.position="none")+ # THE COLOUR CODING BY PLANT POSITION
    scale_y_continuous(expand = c(0, 0),
                       expression("Phase shift of the cold treatment (h)"))+
    #sec.axis = sec_axis(~ . * 2.4, name = "Temperature ", breaks = seq(0,36,7)))+ # THE NAMES OF THE Y-AXIS LABELS, BOTH SIDES
    theme(axis.text=element_text(size=2, colour = "black"),
          axis.title=element_text(size=1.6,face="bold"))+
    ggtitle(title)  +
    theme(plot.title = element_text(size = 4, face = "bold"))  # CHANGE THE TITLE OF THE GRAPH
  return(plot)
}



c1 <- Choose_transcript_and_plot_PRC_Sep(all_data_smoothed,"LUX September","g12727.t1","Ambient_Sep", "Cold_Sep") # LUX  Massive changes of class
c2 <- Choose_transcript_and_plot_PRC_Dec(all_data_smoothed,"LUX December","g12727.t1","Ambient_Dec", "Cold_Dec")
c3 <- Choose_transcript_and_plot_PRC_Mar(all_data_smoothed,"LUX March","g12727.t1","Ambient_Mar", "Cold_Mar")
c4 <- Choose_transcript_and_plot_PRC_Jun(all_data_smoothed,"LUX June","g12727.t1","Ambient_Jun", "Cold_Jun")



a1 <- Choose_transcript_and_plot_PRC_Sep(all_data_smoothed,"CCA1 September","g09640.t1","Ambient_Sep", "Cold_Sep") # CCA1
a2 <- Choose_transcript_and_plot_PRC_Dec(all_data_smoothed,"CCA1 December","g09640.t1","Ambient_Dec", "Cold_Dec")
a3 <- Choose_transcript_and_plot_PRC_Mar(all_data_smoothed,"CCA1 March","g09640.t1","Ambient_Mar", "Cold_Mar")
a4 <- Choose_transcript_and_plot_PRC_Jun(all_data_smoothed,"CCA1 June","g09640.t1","Ambient_Jun", "Cold_Jun")



b1 <- Choose_transcript_and_plot_PRC_Sep(all_data_smoothed,"PRR9 September","g09644.t1","Ambient_Sep", "Cold_Sep") #PRR9
b2 <- Choose_transcript_and_plot_PRC_Dec(all_data_smoothed,"PRR9 December","g09644.t1","Ambient_Dec", "Cold_Dec")
b3 <- Choose_transcript_and_plot_PRC_Mar(all_data_smoothed,"PRR9 March","g09644.t1","Ambient_Mar", "Cold_Mar")
b4 <- Choose_transcript_and_plot_PRC_Jun(all_data_smoothed,"PRR9 June","g09644.t1","Ambient_Jun", "Cold_Jun")

