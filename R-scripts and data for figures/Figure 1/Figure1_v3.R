library(readxl)
library(tidyverse)


####### This prints the Figure 1 panels, made on 21.8.2026

transcript_data_annotated <- read_tsv("1-metadata_Mar2024.txt", col_names = TRUE) 
weather_data <- read_csv("Sample_environ_data_long_format.csv", col_names=TRUE)
weather <- weather_data %>% select("sample_ID", "Temperature")
all_data <- merge(transcript_data_annotated,weather , by="sample_ID", all=FALSE)


# THESE ARE THE SUNRISE AND SUNSET TIMES FOR EACH SEASON TO GET THE NIGHT-TIME SHADING CORRECTLY IN THE PLOTS
#September
#December
#March
#June
#Sunrise
#05:20
#06:40
#05:50
#04:20
#Sunset
#18:30
#17:00
#18:10
#19:10

sep_rects <- data.frame(xstart = c(0,3.30,14.20,27.30,38.20), xend = c(3.30,14.20,27.30,38.20,49),
                        col = c("white","gray50","white","gray50","white"))

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


plot_all_seasons <- function(df,transcript,...) {
  arguments <- list(...)
  filtered <- df %>% filter(Plant %in% arguments)
  p <- c()
  for (i in 1:length(arguments)){  #CHANGE THE COLOURS YOU SEE FIT
    if(arguments[i] == "Ambient_Sep") p[i] = "#860124"
    if(arguments[i] == "Ambient_Dec") p[i] = "#76AFB6"
    if(arguments[i] == "Ambient_Mar") p[i] = "#FB2372"
    if(arguments[i] == "Ambient_Jun") p[i] = "#FFD535"
    if(arguments[i] == "Cold_Dec") p[i] = "#2f5597"
    if(arguments[i] == "Cold_Sep") p[i] = "#2f5597"
    if(arguments[i] == "Cold_Mar") p[i] = "#2f5597"
    if(arguments[i] == "Cold_Jun") p[i] = "#2f5597"
  }
  chosen_colours <- p
  plot <- ggplot() + # FOR DIFFERENT SEASONS, UNCHECK AND CHECK THESE GEOM_RECTS.
    geom_rect(data = sep_rects_irradiance, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.3)+
    geom_rect(data = dec_rects_irradiance, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.3)+
    geom_rect(data = mar_rects_irradiance, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.3)+
    geom_rect(data = jun_rects_irradiance, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.3)+
    scale_fill_manual(values=c("darkgrey","white","white"), guide=FALSE)+ #THE NIGHT RECTANLGES DONE!
    geom_line(data = filtered , aes(x = Timepoint, y = get(transcript),colour=Plant), size=1.5)+ # THE TRANSCRIPT LEVEL
    #geom_line(data = filtered , aes(x = Timepoint, y = Temperature/2,colour=Plant),linetype = "dashed", size=1.5)+ # THE TEMPERATURE
    scale_x_continuous(expand = c(0, 0),breaks=seq(1,49,by=8) ,labels = c("16","00","08","16","00","08","16"))+ # PUTS THE LITTLE TICKS ON THE X-AXIS
    scale_color_manual(values=chosen_colours)+ # THE COLOURS YOU DEFINED AT THE BEGINNING
    geom_point(data = filtered , aes(x = Timepoint, y =get(transcript),colour=Plant), size=1.5, shape=1, alpha= 0.6)+ # PUTTING LITTLE CIRCLES ROUND THE TRANSCRIPT LINE
    coord_fixed(ratio=1.5, xlim=c(0.9,48.1), ylim=c(-0.1,16.5))+ # SCALING THE PLOTS RATIOS
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ # THE BACKGROUND OF THE GRAPH, NO GRID
    theme(axis.line = element_line(colour = "black", size = 1))  + # DRAWING THE BLACK LINES AROUND THE PLOT
    theme(legend.position="right")+ # THE COLOUR CODING BY PLANT POSITION
    scale_y_continuous(expand = c(0, 0),
                       expression("Log_2(RPM+1)"))+#,
    #sec.axis = sec_axis(~ . * 2, name = "Temperature ", breaks = seq(0,36,7)))+ # THE NAMES OF THE Y-AXIS LABELS, BOTH SIDES
    theme(axis.text=element_text(size=20, colour = "black"),
          axis.title=element_text(size=16,face="bold"))
  #ggtitle(transcript)  +  # CHANGE THE TITLE OF THE GRAPH
  return(plot)
}

a1 <-   plot_all_seasons(all_data, "g09640.t1", "Ambient_Dec", "Ambient_Jun", "Ambient_Mar","Ambient_Sep") #CCA1
a2 <-   plot_all_seasons(all_data, "g01821.t2", "Ambient_Dec", "Ambient_Jun", "Ambient_Mar","Ambient_Sep") #LHY
a3 <-   plot_all_seasons(all_data, "g09644.t1", "Ambient_Dec", "Ambient_Jun", "Ambient_Mar","Ambient_Sep") #PRR9
a4 <-   plot_all_seasons(all_data, "g12727.t1", "Ambient_Dec", "Ambient_Jun", "Ambient_Mar","Ambient_Sep") #LUX

ggsave(plot = a1 , "Figure1_CCA1_080126.pdf", width = 210, height = 297, units = "mm", dpi = "retina")
ggsave(plot = a2 , "Figure1_LHY_080126.pdf", width = 210, height = 297, units = "mm", dpi = "retina")
ggsave(plot = a3 , "Figure1_PRR9_080126.pdf", width = 210, height = 297, units = "mm", dpi = "retina")
ggsave(plot = a4 , "Figure1_LUX_080126.pdf", width = 210, height = 297, units = "mm", dpi = "retina")


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

library("scales")

#The next column tells which day of collection
day1 <- rep("Day1",23)
day2 <- rep("Day2",23)
Day <- c(day1,day2)
Day

#The next column tells what time point has the plant been collected
Timepoint <- seq(1, 46, by=1)

metadata <- tibble(Day, Timepoint)
metadata

all_data_smoothed_lc <-  merge(all_data_smoothed,metadata, by="Timepoint", all=FALSE )

View(all_data_smoothed_lc)

plot_limit_cycle <- function(df,title,transcript1,transcript2,...) {
  arguments <- list(...)
  filtered1 <- df %>% filter(Plant %in% arguments[1])
  filtered2 <- df %>% filter(Plant %in% arguments[2])
  p <- c()
  for (i in 1:length(arguments)){  #CHANGE THE COLOURS YOU SEE FIT
    if(arguments[i] == "Ambient_Dec") chosen_colours =  c("#76AFB6","green")
    if(arguments[i] == "Ambient_Sep") chosen_colours  = c("#860124","red")
    if(arguments[i] == "Ambient_Mar") chosen_colours  = c("#FB2372","pink")
    if(arguments[i] == "Ambient_Jun") chosen_colours  = c("#FFD535","yellow")
    if(arguments[i] == "Cold_Dec") p[i] = "blue"
    if(arguments[i] == "Cold_Sep") p[i] = "blue"
    if(arguments[i] == "Cold_Mar") p[i] = "blue"
    if(arguments[i] == "Cold_Jun") p[i] = "blue"
  }
  #chosen_colours <- p
  plot <- ggplot() + # FOR DIFFERENT SEASONS, UNCHECK AND CHECK THESE GEOM_RECTS.
    geom_point(data = filtered1 , aes(x = rescale(get(transcript1)), y = rescale(get(transcript2)),colour=Day), size=0.8)+ # THE TRANSCRIPT LEVEL
    geom_path(data = filtered1 , aes(x = rescale(get(transcript1)), y = rescale(get(transcript2)),colour=Day), size=0.8, alpha=0.8, arrow=arrow()) +
    geom_point(data = filtered2 , aes(x = rescale(get(transcript1)), y = rescale(get(transcript2)),colour="blue"), size=0.8)+ 
    geom_path(data = filtered2 , aes(x = rescale(get(transcript1)), y = rescale(get(transcript2)),colour="blue"), size=0.8, alpha=0.8, arrow=arrow()) +
    scale_color_manual(values=chosen_colours)+ # THE COLOURS YOU DEFINED AT THE BEGINNING
    labs(x=" ")+ # THE X-LABEL
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ # THE BACKGROUND OF THE GRAPH, NO GRID
    theme(axis.line = element_line(colour = "black", size = 1))  + # DRAWING THE BLACK LINES AROUND THE PLOT
    theme(legend.position="none")+ # THE COLOUR CODING BY PLANT POSITION
    coord_fixed(ratio=0.66, xlim=c(0,1), ylim=c(0,1))+
    scale_y_continuous(expand = c(0, 0),
                       expression("Expression of CCA1"))+
    scale_x_continuous(expand = c(0, 0),
                       expression("Expression of LHY"))+
    #sec.axis = sec_axis(~ . * 2.4, name = "Temperature ", breaks = seq(0,36,7)))+ # THE NAMES OF THE Y-AXIS LABELS, BOTH SIDES
    theme(axis.text=element_text(size=2, colour = "black"),
          axis.title=element_text(size=1.6,face="bold"))+
    ggtitle(title) +
    theme(plot.title = element_text(size = 4, face = "bold"))# CHANGE THE TITLE OF THE GRAPH
  return(plot)
}

plot_limit_cycle(all_data_smoothed_lc,"CCA1/LHY limit cycles September","g09640.t1","g01821.t2", "Ambient_Sep")
plot_limit_cycle(all_data_smoothed_lc,"PRR7/PRR9 limit cycles","g19424.t1","g09644.t1", "Ambient_Sep")

g1 <- plot_limit_cycle(all_data_smoothed_lc,"CCA1/LHY limit cycles September","g09640.t1","g01821.t2", "Ambient_Sep")
g2 <- plot_limit_cycle(all_data_smoothed_lc,"CCA1/LHY limit cycles December","g09640.t1","g01821.t2", "Ambient_Dec")
g3 <- plot_limit_cycle(all_data_smoothed_lc,"CCA1/LHY limit cycles March","g09640.t1","g01821.t2", "Ambient_Mar")
g4 <- plot_limit_cycle(all_data_smoothed_lc,"CCA1/LHY limit cycles June","g09640.t1","g01821.t2", "Ambient_Jun")

h1 <- plot_limit_cycle(all_data_smoothed_lc,"ELF4/LUX limit cycles","g12577.t1","g12727.t1", "Ambient_Sep")
h2 <- plot_limit_cycle(all_data_smoothed_lc,"ELF4/LUX limit cycles","g12577.t1","g12727.t1", "Ambient_Dec")
h3 <- plot_limit_cycle(all_data_smoothed_lc,"ELF4/LUX limit cycles","g12577.t1","g12727.t1", "Ambient_Mar")
h4 <- plot_limit_cycle(all_data_smoothed_lc,"ELF4/LUX limit cycles","g12577.t1","g12727.t1", "Ambient_Jun")


Figure1_pt1 <- ggarrange(g1,g2,g3,g4,h1,h2,h3,h4,
                         ncol = 4, nrow = 2)

ggsave(plot =Figure1_pt1 , "Figure3_pt2_smoothed_20241002.pdf", width = 210, height = 297, units = "mm", dpi = "retina")

Figure1_pt2 <- ggarrange(h1,h2,h3,h4,
                         ncol = 4, nrow = 1)

ggsave(plot =Figure1_pt1 , "Figure1_pt2_smoothed_260709.pdf", width = 210, height = 297, units = "mm", dpi = "retina")



