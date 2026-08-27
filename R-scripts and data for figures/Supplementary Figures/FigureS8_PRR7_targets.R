library(readxl)
library(tidyverse)

transcript_data_annotated <- read_tsv("analysis/1-metadata_Mar2024.txt", col_names = TRUE) 
weather_data <- read_csv("Sample_environ_data_long_format.csv", col_names=TRUE)
weather <- weather_data %>% select("sample_ID", "Temperature")
all_data <- merge(transcript_data_annotated,weather , by="sample_ID", all=FALSE)

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







#September: "#860124"
#December: "#76AFB6"
#March: "#FB2372"
#June: "#FFD535"

plot_jun_irradiance <- function(df,title,transcript,...) {
  arguments <- list(...)
  filtered <- df %>% filter(Plant %in% arguments)
  p <- c()
  for (i in 1:length(arguments)){  #CHANGE THE COLOURS YOU SEE FIT
    if(arguments[i] == "Ambient_Dec") p[i] = "#860124"
    if(arguments[i] == "Ambient_Sep") p[i] = "#860124"
    if(arguments[i] == "Ambient_Mar") p[i] = "#860124"
    if(arguments[i] == "Ambient_Jun") p[i] = "#860124"
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
    geom_line(data = filtered , aes(x = Timepoint, y = get(transcript),colour=Plant), size=0.5)+ # THE TRANSCRIPT LEVEL
    geom_line(data = filtered , aes(x = Timepoint, y = Temperature/2,colour=Plant),linetype = "dashed", size=1.5)+ # THE TEMPERATURE
    scale_x_continuous(expand = c(0, 0),breaks=seq(1,49,by=8) ,labels = c("16","00","08","16","00","08","16"))+ # PUTS THE LITTLE TICKS ON THE X-AXIS
    scale_color_manual(values=chosen_colours)+ # THE COLOURS YOU DEFINED AT THE BEGINNING
    geom_point(data = filtered , aes(x = Timepoint, y =get(transcript),colour=Plant), size=0.5, shape=1, alpha= 0.6)+ # PUTTING LITTLE CIRCLES ROUND THE TRANSCRIPT LINE
    coord_fixed(ratio=1.5, xlim=c(0.9,48.1), ylim=c(-0.1,16.5))+ # SCALING THE PLOTS RATIOS
    labs(x=" ")+ # THE X-LABEL
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ # THE BACKGROUND OF THE GRAPH, NO GRID
    theme(axis.line = element_line(colour = "black", size = 1))  + # DRAWING THE BLACK LINES AROUND THE PLOT
    theme(legend.position="none")+ # THE COLOUR CODING BY PLANT POSITION
    scale_y_continuous(expand = c(0, 0),
                       expression("TPM"))+
    #sec.axis = sec_axis(~ . * 2.4, name = "Temperature ", breaks = seq(0,36,7)))+ # THE NAMES OF THE Y-AXIS LABELS, BOTH SIDES
    theme(axis.text=element_text(size=2, colour = "black"),
          axis.title=element_text(size=1.6,face="bold"))+
    ggtitle(title) +
    theme(plot.title = element_text(size = 4, face = "bold"))# CHANGE THE TITLE OF THE GRAPH
  return(plot)
}




plot_sep_irradiance <- function(df,title,transcript,...) {
  arguments <- list(...)
  filtered <- df %>% filter(Plant %in% arguments)
  p <- c()
  for (i in 1:length(arguments)){  #CHANGE THE COLOURS YOU SEE FIT
    if(arguments[i] == "Ambient_Dec") p[i] = "#860124"
    if(arguments[i] == "Ambient_Sep") p[i] = "#860124"
    if(arguments[i] == "Ambient_Mar") p[i] = "#860124"
    if(arguments[i] == "Ambient_Jun") p[i] = "#860124"
    if(arguments[i] == "Cold_Dec") p[i] = "#2f5597"
    if(arguments[i] == "Cold_Sep") p[i] = "#2f5597"
    if(arguments[i] == "Cold_Mar") p[i] = "#2f5597"
    if(arguments[i] == "Cold_Jun") p[i] = "#2f5597"
  }
  chosen_colours <- p
  plot <- ggplot() + # FOR DIFFERENT SEASONS, UNCHECK AND CHECK THESE GEOM_RECTS.
    #geom_rect(data = dec_rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.3)+
    #geom_rect(data = mar_rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.3)+
    geom_rect(data = sep_rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.3)+
    #geom_rect(data = jun_rects_irradiance, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.3)+
    scale_fill_manual(values=c("darkgrey","white","white"), guide=FALSE)+ #THE NIGHT RECTANLGES DONE!
    geom_line(data = filtered , aes(x = Timepoint, y = get(transcript),colour=Plant), size=0.5)+ # THE TRANSCRIPT LEVEL
    geom_line(data = filtered , aes(x = Timepoint, y = Temperature/2,colour=Plant),linetype = "dashed", size=1.5)+ # THE TEMPERATURE
    scale_x_continuous(expand = c(0, 0),breaks=seq(1,49,by=8) ,labels = c("16","00","08","16","00","08","16"))+ # PUTS THE LITTLE TICKS ON THE X-AXIS
    scale_color_manual(values=chosen_colours)+ # THE COLOURS YOU DEFINED AT THE BEGINNING
    geom_point(data = filtered , aes(x = Timepoint, y =get(transcript),colour=Plant), size=0.5, shape=1, alpha= 0.6)+ # PUTTING LITTLE CIRCLES ROUND THE TRANSCRIPT LINE
    coord_fixed(ratio=1.5, xlim=c(0.9,48.1), ylim=c(-0.1,16.5))+ # SCALING THE PLOTS RATIOS
    labs(x=" ")+ # THE X-LABEL
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ # THE BACKGROUND OF THE GRAPH, NO GRID
    theme(axis.line = element_line(colour = "black", size = 1))  + # DRAWING THE BLACK LINES AROUND THE PLOT
    theme(legend.position="none")+ # THE COLOUR CODING BY PLANT POSITION
    scale_y_continuous(expand = c(0, 0),
                       expression("TPM"))+
    #sec.axis = sec_axis(~ . * 2.4, name = "Temperature ", breaks = seq(0,36,7)))+ # THE NAMES OF THE Y-AXIS LABELS, BOTH SIDES
    theme(axis.text=element_text(size=2, colour = "black"),
          axis.title=element_text(size=1.6,face="bold"))+
    ggtitle(title)  +
    theme(plot.title = element_text(size = 4, face = "bold"))  # CHANGE THE TITLE OF THE GRAPH
  return(plot)
}

plot_dec_irradiance <- function(df,title,transcript,...) {
  arguments <- list(...)
  filtered <- df %>% filter(Plant %in% arguments)
  p <- c()
  for (i in 1:length(arguments)){  #CHANGE THE COLOURS YOU SEE FIT
    if(arguments[i] == "Ambient_Dec") p[i] = "#860124"
    if(arguments[i] == "Ambient_Sep") p[i] = "#860124"
    if(arguments[i] == "Ambient_Mar") p[i] = "#860124"
    if(arguments[i] == "Ambient_Jun") p[i] = "#860124"
    if(arguments[i] == "Cold_Dec") p[i] = "#2f5597"
    if(arguments[i] == "Cold_Sep") p[i] = "#2f5597"
    if(arguments[i] == "Cold_Mar") p[i] = "#2f5597"
    if(arguments[i] == "Cold_Jun") p[i] = "#2f5597"
  }
  chosen_colours <- p
  plot <- ggplot() + # FOR DIFFERENT SEASONS, UNCHECK AND CHECK THESE GEOM_RECTS.
    geom_rect(data = dec_rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.3)+
    #geom_rect(data = mar_rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.3)+
    #geom_rect(data = sep_rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.3)+
    #geom_rect(data = jun_rects_irradiance, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.3)+
    scale_fill_manual(values=c("darkgrey","white","white"), guide=FALSE)+ #THE NIGHT RECTANLGES DONE!
    geom_line(data = filtered , aes(x = Timepoint, y = get(transcript),colour=Plant), size=0.5)+ # THE TRANSCRIPT LEVEL
    geom_line(data = filtered , aes(x = Timepoint, y = Temperature/2,colour=Plant),linetype = "dashed", size=1.5)+ # THE TEMPERATURE
    scale_x_continuous(expand = c(0, 0),breaks=seq(1,49,by=8) ,labels = c("16","00","08","16","00","08","16"))+ # PUTS THE LITTLE TICKS ON THE X-AXIS
    scale_color_manual(values=chosen_colours)+ # THE COLOURS YOU DEFINED AT THE BEGINNING
    geom_point(data = filtered , aes(x = Timepoint, y =get(transcript),colour=Plant), size=0.5, shape=1, alpha= 0.6)+ # PUTTING LITTLE CIRCLES ROUND THE TRANSCRIPT LINE
    coord_fixed(ratio=1.5, xlim=c(0.9,48.1), ylim=c(-0.1,16.5))+ # SCALING THE PLOTS RATIOS
    labs(x=" ")+ # THE X-LABEL
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ # THE BACKGROUND OF THE GRAPH, NO GRID
    theme(axis.line = element_line(colour = "black", size = 1))  + # DRAWING THE BLACK LINES AROUND THE PLOT
    theme(legend.position="none")+ # THE COLOUR CODING BY PLANT POSITION
    scale_y_continuous(expand = c(0, 0),
                       expression("TPM"))+
    #sec.axis = sec_axis(~ . * 2.4, name = "Temperature ", breaks = seq(0,36,7)))+ # THE NAMES OF THE Y-AXIS LABELS, BOTH SIDES
    theme(axis.text=element_text(size=2, colour = "black"),
          axis.title=element_text(size=1.6,face="bold"))+
    ggtitle(title) +
    theme(plot.title = element_text(size = 4, face = "bold"))   # CHANGE THE TITLE OF THE GRAPH
  return(plot)
}


plot_mar_irradiance <- function(df,title,transcript,...) {
  arguments <- list(...)
  filtered <- df %>% filter(Plant %in% arguments)
  p <- c()
  for (i in 1:length(arguments)){  #CHANGE THE COLOURS YOU SEE FIT
    if(arguments[i] == "Ambient_Dec") p[i] = "#860124"
    if(arguments[i] == "Ambient_Sep") p[i] = "#860124"
    if(arguments[i] == "Ambient_Mar") p[i] = "#860124"
    if(arguments[i] == "Ambient_Jun") p[i] = "#860124"
    if(arguments[i] == "Cold_Dec") p[i] = "#2f5597"
    if(arguments[i] == "Cold_Sep") p[i] = "#2f5597"
    if(arguments[i] == "Cold_Mar") p[i] = "#2f5597"
    if(arguments[i] == "Cold_Jun") p[i] = "#2f5597"
  }
  chosen_colours <- p
  plot <- ggplot() + # FOR DIFFERENT SEASONS, UNCHECK AND CHECK THESE GEOM_RECTS.
    #geom_rect(data = dec_rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.3)+
    geom_rect(data = mar_rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.3)+
    #geom_rect(data = sep_rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.3)+
    #geom_rect(data = jun_rects_irradiance, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.3)+
    scale_fill_manual(values=c("darkgrey","white","white"), guide=FALSE)+ #THE NIGHT RECTANLGES DONE!
    geom_line(data = filtered , aes(x = Timepoint, y = get(transcript),colour=Plant), size=0.5)+ # THE TRANSCRIPT LEVEL
    geom_line(data = filtered , aes(x = Timepoint, y = Temperature/2,colour=Plant),linetype = "dashed", size=1.5)+ # THE TEMPERATURE
    scale_x_continuous(expand = c(0, 0),breaks=seq(1,49,by=8) ,labels = c("16","00","08","16","00","08","16"))+ # PUTS THE LITTLE TICKS ON THE X-AXIS
    scale_color_manual(values=chosen_colours)+ # THE COLOURS YOU DEFINED AT THE BEGINNING
    geom_point(data = filtered , aes(x = Timepoint, y =get(transcript),colour=Plant), size=0.5, shape=1, alpha= 0.6)+ # PUTTING LITTLE CIRCLES ROUND THE TRANSCRIPT LINE
    coord_fixed(ratio=1.5, xlim=c(0.9,48.1), ylim=c(-0.1,16.5))+ # SCALING THE PLOTS RATIOS
    labs(x=" ")+ # THE X-LABEL
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ # THE BACKGROUND OF THE GRAPH, NO GRID
    theme(axis.line = element_line(colour = "black", size = 1))  + # DRAWING THE BLACK LINES AROUND THE PLOT
    theme(legend.position="none")+ # THE COLOUR CODING BY PLANT POSITION
    scale_y_continuous(expand = c(0, 0),
                       expression("TPM"))+
    #sec.axis = sec_axis(~ . * 2.4, name = "Temperature ", breaks = seq(0,36,7)))+ # THE NAMES OF THE Y-AXIS LABELS, BOTH SIDES
    theme(axis.text=element_text(size=2, colour = "black"),
          axis.title=element_text(size=1.6,face="bold"))+
    ggtitle(title)+
    theme(plot.title = element_text(size = 4, face = "bold"))# CHANGE THE TITLE OF THE GRAPH
  return(plot)
}




a1 <- plot_sep_irradiance(all_data,"ABA1","g07284.t1","Ambient_Sep", "Cold_Sep") # ABA1
a2 <- plot_dec_irradiance(all_data,"ABA1","g07284.t1","Ambient_Dec", "Cold_Dec")
a3 <- plot_mar_irradiance(all_data,"ABA1","g07284.t1","Ambient_Mar", "Cold_Mar")
a4 <- plot_jun_irradiance(all_data,"ABA1","g07284.t1","Ambient_Jun", "Cold_Jun")


b1 <- plot_sep_irradiance(all_data,"AMY3","g03714.t1","Ambient_Sep", "Cold_Sep") #AMY3
b2 <- plot_dec_irradiance(all_data,"AMY3","g03714.t1","Ambient_Dec", "Cold_Dec")
b3 <- plot_mar_irradiance(all_data,"AMY3","g03714.t1","Ambient_Mar", "Cold_Mar")
b4 <- plot_jun_irradiance(all_data,"AMY3","g03714.t1","Ambient_Jun", "Cold_Jun")

c1 <- plot_sep_irradiance(all_data,"LNK1","g12435.t1","Ambient_Sep", "Cold_Sep") #LNK1
c2 <- plot_dec_irradiance(all_data,"LNK1","g12435.t1","Ambient_Dec", "Cold_Dec")
c3 <- plot_mar_irradiance(all_data,"LNK1","g12435.t1","Ambient_Mar", "Cold_Mar")
c4 <- plot_jun_irradiance(all_data,"LNK1","g12435.t1","Ambient_Jun", "Cold_Jun")

d1 <- plot_sep_irradiance(all_data,"PIF5","g22894.t1","Ambient_Sep", "Cold_Sep") #PIF5
d2 <- plot_dec_irradiance(all_data,"PIF5","g22894.t1","Ambient_Dec", "Cold_Dec")
d3 <- plot_mar_irradiance(all_data,"PIF5","g22894.t1","Ambient_Mar", "Cold_Mar")
d4 <- plot_jun_irradiance(all_data,"PIF5","g22894.t1","Ambient_Jun", "Cold_Jun")



PRR7_targets <-  ggarrange(m1,m2,m3,m4,a1,a2,a3,a4,b1,b2,b3,b4,
                   c1,c2,c3,c4,d1,d2,d3,d4,
                   ncol = 4, nrow = 5)

PRR7_targets

ggsave(plot =PRR7_targets , "Supplementary_Figure_PRR7_targets_260729.pdf", width = 210, height = 297, units = "mm", dpi = "retina")


