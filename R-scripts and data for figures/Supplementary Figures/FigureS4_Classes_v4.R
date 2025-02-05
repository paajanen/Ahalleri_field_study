library(readxl)
library(tidyverse)
library(ggpubr)

####### This prints the Figure 1 panels

transcript_data_annotated <- read_tsv("analysis/1-metadata_Mar2024.txt", col_names = TRUE) 
weather_data <- read_csv("data/Sample_environ_data_long_format.csv", col_names=TRUE)
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


plot_all_seasons <- function(df,transcript,...) {
  arguments <- list(...)
  filtered <- df %>% filter(Plant %in% arguments)
  p <- c()
  for (i in 1:length(arguments)){  #CHANGE THE COLOURS YOU SEE FIT
    if(arguments[i] == "Ambient_Dec") p[i] = "#76AFB6"
    if(arguments[i] == "Ambient_Sep") p[i] = "#860124"
    if(arguments[i] == "Ambient_Mar") p[i] = "#FB2372"
    if(arguments[i] == "Ambient_Jun") p[i] = "#FFD535"
    if(arguments[i] == "Cold_Dec") p[i] = "#2f5597"
    if(arguments[i] == "Cold_Sep") p[i] = "#2f5597"
    if(arguments[i] == "Cold_Mar") p[i] = "#2f5597"
    if(arguments[i] == "Cold_Jun") p[i] = "#2f5597"
  }
  chosen_colours <- p
  plot <- ggplot() + # FOR DIFFERENT SEASONS, UNCHECK AND CHECK THESE GEOM_RECTS.
    geom_rect(data = dec_rects_irradiance, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.3)+
    geom_rect(data = mar_rects_irradiance, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.3)+
    geom_rect(data = sep_rects_irradiance, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.3)+
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
    theme(legend.position="none")+ # THE COLOUR CODING BY PLANT POSITION
    scale_y_continuous(expand = c(0, 0),
                       expression("TPM"))+#,
                       #sec.axis = sec_axis(~ . * 2, name = "Temperature ", breaks = seq(0,36,7)))+ # THE NAMES OF THE Y-AXIS LABELS, BOTH SIDES
    theme(axis.text=element_text(size=20, colour = "black"),
          axis.title=element_text(size=16,face="bold"))
  #ggtitle(transcript)  +  # CHANGE THE TITLE OF THE GRAPH
  return(plot)
}


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
    #geom_line(data = filtered , aes(x = Timepoint, y = Temperature/2,colour=Plant),linetype = "dashed", size=1.5)+ # THE TEMPERATURE
    scale_x_continuous(expand = c(0, 0),breaks=seq(1,49,by=8) ,labels = c("16","00","08","16","00","08","16"))+ # PUTS THE LITTLE TICKS ON THE X-AXIS
    scale_color_manual(values=chosen_colours)+ # THE COLOURS YOU DEFINED AT THE BEGINNING
    geom_point(data = filtered , aes(x = Timepoint, y =get(transcript),colour=Plant), size=0.5, shape=1, alpha= 0.6)+ # PUTTING LITTLE CIRCLES ROUND THE TRANSCRIPT LINE
    coord_fixed(ratio=1.5, xlim=c(0.9,48.1), ylim=c(-0.1,12.5))+ # SCALING THE PLOTS RATIOS
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
    #geom_line(data = filtered , aes(x = Timepoint, y = Temperature/2,colour=Plant),linetype = "dashed", size=1.5)+ # THE TEMPERATURE
    scale_x_continuous(expand = c(0, 0),breaks=seq(1,49,by=8) ,labels = c("16","00","08","16","00","08","16"))+ # PUTS THE LITTLE TICKS ON THE X-AXIS
    scale_color_manual(values=chosen_colours)+ # THE COLOURS YOU DEFINED AT THE BEGINNING
    geom_point(data = filtered , aes(x = Timepoint, y =get(transcript),colour=Plant), size=0.5, shape=1, alpha= 0.6)+ # PUTTING LITTLE CIRCLES ROUND THE TRANSCRIPT LINE
    coord_fixed(ratio=1.5, xlim=c(0.9,48.1), ylim=c(-0.1,12.5))+ # SCALING THE PLOTS RATIOS
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
    #geom_line(data = filtered , aes(x = Timepoint, y = Temperature/2,colour=Plant),linetype = "dashed", size=1.5)+ # THE TEMPERATURE
    scale_x_continuous(expand = c(0, 0),breaks=seq(1,49,by=8) ,labels = c("16","00","08","16","00","08","16"))+ # PUTS THE LITTLE TICKS ON THE X-AXIS
    scale_color_manual(values=chosen_colours)+ # THE COLOURS YOU DEFINED AT THE BEGINNING
    geom_point(data = filtered , aes(x = Timepoint, y =get(transcript),colour=Plant), size=0.5, shape=1, alpha= 0.6)+ # PUTTING LITTLE CIRCLES ROUND THE TRANSCRIPT LINE
    coord_fixed(ratio=1.5, xlim=c(0.9,48.1), ylim=c(-0.1,12.5))+ # SCALING THE PLOTS RATIOS
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
    #geom_line(data = filtered , aes(x = Timepoint, y = Temperature/2,colour=Plant),linetype = "dashed", size=1.5)+ # THE TEMPERATURE
    scale_x_continuous(expand = c(0, 0),breaks=seq(1,49,by=8) ,labels = c("16","00","08","16","00","08","16"))+ # PUTS THE LITTLE TICKS ON THE X-AXIS
    scale_color_manual(values=chosen_colours)+ # THE COLOURS YOU DEFINED AT THE BEGINNING
    geom_point(data = filtered , aes(x = Timepoint, y =get(transcript),colour=Plant), size=0.5, shape=1, alpha= 0.6)+ # PUTTING LITTLE CIRCLES ROUND THE TRANSCRIPT LINE
    coord_fixed(ratio=1.5, xlim=c(0.9,48.1), ylim=c(-0.1,12.5))+ # SCALING THE PLOTS RATIOS
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


#DECEMBER
plot_dec_irradiance(all_data,"Titles are needed", "g00829.t1", "Ambient_Dec", "Cold_Dec") 
No_Change_Dec_g00829.t1_SuppFig1_24_06_06 <-  plot_dec_irradiance(all_data,"No change linear: g00829.t1 December","g00829.t1", "Ambient_Dec", "Cold_Dec") 
#ggsave(plot = No_Change_Dec_g00829.t1_SuppFig1_24_06_06, "Supplementary/No_Change_Dec_g00829.t1_SuppFig1_24_06_06.pdf", width = 6, height = 6, dpi = "retina")


No_change_cyclic_Dec_g29474.t1_SuppFig1_24_06_06 <- plot_dec_irradiance(all_data, "No change cyclic: g29474_t1, December" ,"g29474.t1", "Ambient_Dec", "Cold_Dec") 
#ggsave(plot = No_change_cyclic_Dec_g29474.t1_SuppFig1_24_06_06, "Supplementary/No_change_cyclic_Dec_g29474_SuppFig1_24_06_06.pdf", width = 6, height = 6, dpi = "retina")

RepressionP_Dec_g24907.t1_SuppFig1_24_06_06 <- plot_dec_irradiance(all_data,"Repression at peak: g24907.t1 December","g24907.t1", "Ambient_Dec", "Cold_Dec") 
#ggsave(plot = RepressionP_Dec_g24907.t1_SuppFig1_24_06_06, "Supplementary/RepressionP_Dec_g24907.t1_SuppFig1_24_06_06.pdf", width = 6, height = 6, dpi = "retina")

InductionD_Dec_g03844.t1_SuppFig1_24_06_06 <- plot_dec_irradiance(all_data,"Induction at descending: g03844.t1 December","g03844.t1", "Ambient_Dec", "Cold_Dec") 
#ggsave(plot = InductionD_Dec_g03844.t1_SuppFig1_24_06_06, "Supplementary/InductionD_Dec_g03844.t1_SuppFig1_24_06_06.pdf", width = 6, height = 6, dpi = "retina")

InductionDT_Dec_g00185.t1_SuppFig1_24_06_06 <- plot_dec_irradiance(all_data,"Induction at descending and trough: g00185.t1 December","g00185.t1", "Ambient_Dec", "Cold_Dec") 
#ggsave(plot = InductionDT_Dec_g00185.t1_SuppFig1_24_06_06, "Supplementary/InductionDT_Dec_g00185.t1_SuppFig1_24_06_06.pdf", width = 6, height = 6, dpi = "retina")

Linear_induction_Dec_g10976.t1_SuppFig1_24_06_06 <- plot_dec_irradiance(all_data,"Linear induction: g10976.t1 December","g10976.t1", "Ambient_Dec", "Cold_Dec") 
#ggsave(plot = Linear_induction_Dec_g10976.t1_SuppFig1_24_06_06, "Supplementary/Linear_induction_Dec_g10976.t1_SuppFig1_24_06_06.pdf", width = 6, height = 6, dpi = "retina")



#MARCH
No_expression_Mar_g29210.t1_SuppFig1_24_06_06 <-plot_mar_irradiance(all_data,"No expression: g29210.t1 March","g29210.t1", "Ambient_Mar", "Cold_Mar") 
#ggsave(plot = No_expression_Mar_g29210.t1_SuppFig1_24_06_06, "Supplementary/No_Expression_Mar_g29210.t1_SuppFig1_24_06_06.pdf", width = 6, height = 6, dpi = "retina")

InductionDIP_Mar_g14800.t1_SuppFig1_24_06_06 <-plot_mar_irradiance(all_data,"Induction at increasing, peak and descending: g14800.t1 March","g14800.t1", "Ambient_Mar", "Cold_Mar") 
#ggsave(plot = InductionDIP_Mar_g14800.t1_SuppFig1_24_06_06, "Supplementary/InductionDIP_Mar_g14800.t1_SuppFig1_24_06_06.pdf", width = 6, height = 6, dpi = "retina")

InductionTDI_Mar_g00040.t1_SuppFig1_24_06_06 <-plot_mar_irradiance(all_data,"Induction at descending, through and increasing: g00040.t1 March","g00040.t1", "Ambient_Mar", "Cold_Mar") 
#ggsave(plot = InductionTDI_Mar_g00040.t1_SuppFig1_24_06_06, "Supplementary/InductionTDI_Mar_g00040.t1_SuppFig1_24_06_06.pdf", width = 6, height = 6, dpi = "retina")

Cyclic_Mar_g13177.t1_SuppFig1_24_06_06 <-plot_mar_irradiance(all_data,"g13177.t1","g13177.t1", "Ambient_Mar", "Cold_Mar") 
#ggsave(plot = Cyclic_Mar_g13177.t1_SuppFig1_24_06_06, "Supplementary/Cyclic_Mar_g13177.t1_SuppFig1_24_06_06.pdf", width = 6, height = 6, dpi = "retina")


#JUNE
Phase_later_Jun_g03287.t1_SuppFig1_24_06_06 <-  plot_jun_irradiance(all_data,"Phase later: g03287.t1 June","g03287.t1", "Ambient_Jun", "Cold_Jun") 
#ggsave(plot = Phase_later_Jun_g03287.t1_SuppFig1_24_06_06, "Supplementary/Phase_later_Jun_g03287.t1_SuppFig1_24_06_06.pdf", width = 6, height = 6, dpi = "retina")

InductionP_Jun_g15820.t1_SuppFig1_24_06_06 <-plot_jun_irradiance(all_data,"Induction at peak: g15820.t1 June","g15820.t1", "Ambient_Jun", "Cold_Jun") 
#ggsave(plot = InductionP_Jun_g15820.t1_SuppFig1_24_06_06, "Supplementary/InductionP_Jun_g15820.t1_SuppFig1_24_06_06.pdf", width = 6, height = 6, dpi = "retina")

InductionT_Jun_g10575.t1_SuppFig1_24_06_06 <-plot_jun_irradiance(all_data,"Induction at through: g10575.t1 June","g10575.t1", "Ambient_Jun", "Cold_Jun") 
#ggsave(plot = InductionT_Jun_g10575.t1_SuppFig1_24_06_06, "Supplementary/InductionT_Jun_g10575.t1_SuppFig1_24_06_06.pdf", width = 6, height = 6, dpi = "retina")

InductionI_Jun_g12577.t1_SuppFig1_24_06_06 <-plot_jun_irradiance(all_data,"Induction at increasing: g12577.t1 June","g12577.t1", "Ambient_Jun", "Cold_Jun") 
#ggsave(plot = InductionI_Jun_g12577.t1_SuppFig1_24_06_06, "Supplementary/InductionI_Jun_g12577.t1_SuppFig1_24_06_06.pdf", width = 6, height = 6, dpi = "retina")

InductionPD_Jun_g26923.t1_SuppFig1_24_06_06 <-plot_jun_irradiance(all_data,"Induction at peak and descending: g26923.t1 June","g26923.t1", "Ambient_Jun", "Cold_Jun") 
#ggsave(plot = InductionPD_Jun_g26923.t1_SuppFig1_24_06_06, "Supplementary/InductionPD_Jun_g26923.t1_SuppFig1_24_06_06.pdf", width = 6, height = 6, dpi = "retina")

InductionPI_Jun_g07611.t1_SuppFig1_24_06_06 <-plot_jun_irradiance(all_data,"Induction at increasing and peak: g07611.t1 June","g07611.t1", "Ambient_Jun", "Cold_Jun") 
#ggsave(plot = InductionPI_Jun_g07611.t1_SuppFig1_24_06_06, "Supplementary/InductionPI_Jun_g07611.t1_SuppFig1_24_06_06.pdf", width = 6, height = 6, dpi = "retina")

InductionDI_Jun_g20285.t1_SuppFig1_24_06_06 <-plot_jun_irradiance(all_data,"Induction at decreasing and increasing: g20285.t1 June","g20285.t1", "Ambient_Jun", "Cold_Jun") 
#ggsave(plot = InductionDI_Jun_g20285.t1_SuppFig1_24_06_06, "Supplementary/InductionDI_Jun_g20285.t1_SuppFig1_24_06_06.pdf", width = 6, height = 6, dpi = "retina")

RepressionPI_Jun_g16049.t1_SuppFig1_24_06_06 <-plot_jun_irradiance(all_data,"Repression at peak and increasing: g16049.t1 June","g16049.t1", "Ambient_Jun", "Cold_Jun") 
#ggsave(plot = RepressionPI_Jun_g16049.t1_SuppFig1_24_06_06, "Supplementary/RepressionPI_Jun_g16049.t1_SuppFig1_24_06_06.pdf", width = 6, height = 6, dpi = "retina")

RepressionTI_Jun_g01420.t1_SuppFig1_24_06_06 <-plot_jun_irradiance(all_data,"Repression at trough and increasing: g01420.t1 June","g01420.t1", "Ambient_Jun", "Cold_Jun") 
#ggsave(plot = RepressionTI_Jun_g01420.t1_SuppFig1_24_06_06, "Supplementary/RepressionTI_Jun_g01420.t1_SuppFig1_24_06_06.pdf", width = 6, height = 6, dpi = "retina")

RepressionPID_Jun_g06661.t1_SuppFig1_24_06_06 <-plot_jun_irradiance(all_data,"Repression at trough, increasing and peak: g06661.t1 June","g06661.t1", "Ambient_Jun", "Cold_Jun") 
#ggsave(plot = RepressionPID_Jun_g06661.t1_SuppFig1_24_06_06, "Supplementary/RepressionPID_Jun_g06661.t1_SuppFig1_24_06_06.pdf", width = 6, height = 6, dpi = "retina")

Cyclic_Jun_g13177.t1_SuppFig1_24_06_06 <-plot_jun_irradiance(all_data,"g13177.t1","g13177.t1", "Ambient_Jun", "Cold_Jun") 
#ggsave(plot = Cyclic_Jun_g13177.t1_SuppFig1_24_06_06, "Supplementary/Cyclic_Jun_g13177.t1_SuppFig1_24_06_06.pdf", width = 6, height = 6, dpi = "retina")


#SEPTEMBER

Linear_repression_Sep_g31788.t1_SuppFig1_24_06_06 <- plot_sep_irradiance(all_data,"Linear repression: g31788.t1 September","g31788.t1", "Ambient_Sep", "Cold_Sep") 
#ggsave(plot = Linear_repression_Sep_g31788.t1_SuppFig1_24_06_06, "Supplementary/Linear_repression_Sep_g31788.t1_SuppFig1_24_06_06.pdf", width = 6, height = 6, dpi = "retina")

Noisy_Sep_g17603.t1_SuppFig1_24_06_06 <- plot_sep_irradiance(all_data,"Noisy: g17603.t1 September","g17603.t1", "Ambient_Sep", "Cold_Sep") 
#ggsave(plot = Noisy_Sep_g17603.t1_SuppFig1_24_06_06, "Supplementary/Noisy_Sep_g17603.t1_SuppFig1_24_06_06.pdf", width = 6, height = 6, dpi = "retina")

Cyclic_induction_Sep_g03805.t1_SuppFig1_24_06_06 <- plot_sep_irradiance(all_data,"Cyclic induction: g03805.t1 September","g03805.t1", "Ambient_Sep", "Cold_Sep") 
#ggsave(plot = Cyclic_induction_Sep_g03805.t1_SuppFig1_24_06_06 , "Supplementary/Cyclic_induction_Sep_g03805.t1_SuppFig1_24_06_06.pdf", width = 6, height = 6, dpi = "retina")

Cyclic_repression_Sep_g02757.t1_SuppFig1_24_06_06 <- plot_sep_irradiance(all_data,"Cyclic repression: g02757.t1 September","g02757.t1", "Ambient_Sep", "Cold_Sep") 
#ggsave(plot = Cyclic_repression_Sep_g02757.t1_SuppFig1_24_06_06 , "Supplementary/Cyclic_repression_Sep_g02757.t1_SuppFig1_24_06_06.pdf", width = 6, height = 6, dpi = "retina")

RepressionI_Sep_g29670.t1_SuppFig1_24_06_06 <- plot_sep_irradiance(all_data,"Repression at increasing: g29670.t1 September","g29670.t1", "Ambient_Sep", "Cold_Sep") 
#ggsave(plot = RepressionI_Sep_g29670.t1_SuppFig1_24_06_06 , "Supplementary/RepressionI_Sep_g29670.t1_SuppFig1_24_06_06.pdf", width = 6, height = 6, dpi = "retina")

RepressionTI_Sep_g26487.t1_SuppFig1_24_06_06 <- plot_sep_irradiance(all_data,"Repression at trough and increasing: g26487.t1 September","g26487.t1", "Ambient_Sep", "Cold_Sep") 
#ggsave(plot = RepressionTI_Sep_g26487.t1_SuppFig1_24_06_06 , "Supplementary/RepressionTI_Sep_g26487.t1_SuppFig1_24_06_06.pdf", width = 6, height = 6, dpi = "retina")

Supplementary_Figure1 <-  ggarrange(No_expression_Mar_g29210.t1_SuppFig1_24_06_06,
          No_Change_Dec_g00829.t1_SuppFig1_24_06_06,
          No_change_cyclic_Dec_g29474.t1_SuppFig1_24_06_06, 
          Linear_induction_Dec_g10976.t1_SuppFig1_24_06_06, 
          Linear_repression_Sep_g31788.t1_SuppFig1_24_06_06, 
          Cyclic_induction_Sep_g03805.t1_SuppFig1_24_06_06, 
          Cyclic_repression_Sep_g02757.t1_SuppFig1_24_06_06, 
          Phase_later_Jun_g03287.t1_SuppFig1_24_06_06, 
          InductionD_Dec_g03844.t1_SuppFig1_24_06_06, 
          InductionP_Jun_g15820.t1_SuppFig1_24_06_06, 
          InductionT_Jun_g10575.t1_SuppFig1_24_06_06, 
          InductionI_Jun_g12577.t1_SuppFig1_24_06_06, 
          InductionDT_Dec_g00185.t1_SuppFig1_24_06_06,
          InductionPD_Jun_g26923.t1_SuppFig1_24_06_06, 
          InductionPI_Jun_g07611.t1_SuppFig1_24_06_06, 
          InductionDI_Jun_g20285.t1_SuppFig1_24_06_06, 
          InductionDIP_Mar_g14800.t1_SuppFig1_24_06_06, 
          InductionTDI_Mar_g00040.t1_SuppFig1_24_06_06, 
          RepressionP_Dec_g24907.t1_SuppFig1_24_06_06,
          RepressionI_Sep_g29670.t1_SuppFig1_24_06_06,  
          RepressionPI_Jun_g16049.t1_SuppFig1_24_06_06, 
          RepressionTI_Jun_g01420.t1_SuppFig1_24_06_06, 
          RepressionPID_Jun_g06661.t1_SuppFig1_24_06_06,
          ncol = 4, nrow = 6)
  
ggsave(plot = Supplementary_Figure1 , "Supplementary/Supplementary_Figure1_6Jun2024.pdf", width = 210, height = 297, units = "mm", dpi = "retina")


