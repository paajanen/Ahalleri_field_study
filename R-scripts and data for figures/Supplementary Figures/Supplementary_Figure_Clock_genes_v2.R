library(readxl)
library(tidyverse)
library(ggpubr)

####### This prints the Figure 1 panels, made on 5.11.2024

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
plot_sep_irradiance(all_data,"Titles are needed", "g00829.t1", "Ambient_Sep", "Cold_Sep") 
plot_dec_irradiance(all_data,"Titles are needed", "g00829.t1", "Ambient_Dec", "Cold_Dec") 
plot_mar_irradiance(all_data,"Titles are needed", "g00829.t1", "Ambient_Mar", "Cold_Mar") 
plot_jun_irradiance(all_data,"Titles are needed", "g00829.t1", "Ambient_Jun", "Cold_Jun") 




a1 <- plot_sep_irradiance(all_data,"CCA1","g09640.t1","Ambient_Sep", "Cold_Sep") # CCA1
a2 <- plot_dec_irradiance(all_data,"CCA1","g09640.t1","Ambient_Dec", "Cold_Dec")
a3 <- plot_mar_irradiance(all_data,"CCA1","g09640.t1","Ambient_Mar", "Cold_Mar")
a4 <- plot_jun_irradiance(all_data,"CCA1","g09640.t1","Ambient_Jun", "Cold_Jun")


b1 <- plot_sep_irradiance(all_data,"LHY","g01821.t2","Ambient_Sep", "Cold_Sep") #LHY not stable
b2 <- plot_dec_irradiance(all_data,"LHY","g01821.t2","Ambient_Dec", "Cold_Dec")
b3 <- plot_mar_irradiance(all_data,"LHY","g01821.t2","Ambient_Mar", "Cold_Mar")
b4 <- plot_jun_irradiance(all_data,"LHY","g01821.t2","Ambient_Jun", "Cold_Jun")

c1 <- plot_sep_irradiance(all_data,"PRR7","g19424.t1","Ambient_Sep", "Cold_Sep") #PRR7
c2 <- plot_dec_irradiance(all_data,"PRR7","g19424.t1","Ambient_Dec", "Cold_Dec")
c3 <- plot_mar_irradiance(all_data,"PRR7","g19424.t1","Ambient_Mar", "Cold_Mar")
c4 <- plot_jun_irradiance(all_data,"PRR7","g19424.t1","Ambient_Jun", "Cold_Jun")

d1 <- plot_sep_irradiance(all_data,"PRR9","g09644.t1","Ambient_Sep", "Cold_Sep") #PRR9
d2 <- plot_dec_irradiance(all_data,"PRR9","g09644.t1","Ambient_Dec", "Cold_Dec")
d3 <- plot_mar_irradiance(all_data,"PRR9","g09644.t1","Ambient_Mar", "Cold_Mar")
d4 <- plot_jun_irradiance(all_data,"PRR9","g09644.t1","Ambient_Jun", "Cold_Jun")

e1 <- plot_sep_irradiance(all_data,"PRR5","g27591.t1","Ambient_Sep", "Cold_Sep") # PRR5
e2 <- plot_dec_irradiance(all_data,"PRR5","g27591.t1","Ambient_Dec", "Cold_Dec")
e3 <- plot_mar_irradiance(all_data,"PRR5","g27591.t1","Ambient_Mar", "Cold_Mar")
e4 <- plot_jun_irradiance(all_data,"PRR5","g27591.t1","Ambient_Jun", "Cold_Jun")

f1 <- plot_sep_irradiance(all_data,"PRR3","g10941.t1","Ambient_Sep", "Cold_Sep") 
f2 <- plot_dec_irradiance(all_data,"PRR3","g10941.t1","Ambient_Dec", "Cold_Dec")
f3 <- plot_mar_irradiance(all_data,"PRR3","g10941.t1","Ambient_Mar", "Cold_Mar")
f4 <- plot_jun_irradiance(all_data,"PRR3","g10941.t1","Ambient_Jun", "Cold_Jun")

g1 <- plot_sep_irradiance(all_data,"TOC1","g10811.t1","Ambient_Sep", "Cold_Sep") #TOC1 not stable
g2 <- plot_dec_irradiance(all_data,"TOC1","g10811.t1","Ambient_Dec", "Cold_Dec")
g3 <- plot_mar_irradiance(all_data,"TOC1","g10811.t1","Ambient_Mar", "Cold_Mar")
g4 <- plot_jun_irradiance(all_data,"TOC1","g10811.t1","Ambient_Jun", "Cold_Jun")

h1 <- plot_sep_irradiance(all_data,"LUX","g12727.t1","Ambient_Sep", "Cold_Sep") # LUX  Massive changes of class
h2 <- plot_dec_irradiance(all_data,"LUX","g12727.t1","Ambient_Dec", "Cold_Dec")
h3 <- plot_mar_irradiance(all_data,"LUX","g12727.t1","Ambient_Mar", "Cold_Mar")
h4 <- plot_jun_irradiance(all_data,"LUX","g12727.t1","Ambient_Jun", "Cold_Jun")


i1 <- plot_sep_irradiance(all_data,"ELF3","g29251.t1","Ambient_Sep", "Cold_Sep") #ELF3
i2 <- plot_dec_irradiance(all_data,"ELF3","g29251.t1","Ambient_Dec", "Cold_Dec")
i3 <- plot_mar_irradiance(all_data,"ELF3","g29251.t1","Ambient_Mar", "Cold_Mar")
i4 <- plot_jun_irradiance(all_data,"ELF3","g29251.t1","Ambient_Jun", "Cold_Jun")


j1 <- plot_sep_irradiance(all_data,"ELF4","g12577.t1","Ambient_Sep", "Cold_Sep") #ELF4
j2 <- plot_dec_irradiance(all_data,"ELF4","g12577.t1","Ambient_Dec", "Cold_Dec")
j3 <- plot_mar_irradiance(all_data,"ELF4","g12577.t1","Ambient_Mar", "Cold_Mar")
j4 <- plot_jun_irradiance(all_data,"ELF4","g12577.t1","Ambient_Jun", "Cold_Jun")

k1 <- plot_sep_irradiance(all_data,"GI","g13729.t2","Ambient_Sep", "Cold_Sep") #GI
k2 <- plot_dec_irradiance(all_data,"GI","g13729.t2","Ambient_Dec", "Cold_Dec")
k3 <- plot_mar_irradiance(all_data,"GI","g13729.t2","Ambient_Mar", "Cold_Mar")
k4 <- plot_jun_irradiance(all_data,"GI","g13729.t2","Ambient_Jun", "Cold_Jun")

l1 <- plot_sep_irradiance(all_data,"RVE1","g00040.t1","Ambient_Sep", "Cold_Sep") 
l2 <- plot_dec_irradiance(all_data,"RVE1","g00040.t1","Ambient_Dec", "Cold_Dec")
l3 <- plot_mar_irradiance(all_data,"RVE1","g00040.t1","Ambient_Mar", "Cold_Mar")
l4 <- plot_jun_irradiance(all_data,"RVE1","g00040.t1","Ambient_Jun", "Cold_Jun")

m1 <- plot_sep_irradiance(all_data,"RVE2","g26625.t1","Ambient_Sep", "Cold_Sep") 
m2 <- plot_dec_irradiance(all_data,"RVE2","g26625.t1","Ambient_Dec", "Cold_Dec")
m3 <- plot_mar_irradiance(all_data,"RVE2","g26625.t1","Ambient_Mar", "Cold_Mar")
m4 <- plot_jun_irradiance(all_data,"RVE2","g26625.t1","Ambient_Jun", "Cold_Jun")

n1 <- plot_sep_irradiance(all_data,"RVE7","g14024.t1","Ambient_Sep", "Cold_Sep") 
n2 <- plot_dec_irradiance(all_data,"RVE7","g14024.t1","Ambient_Dec", "Cold_Dec")
n3 <- plot_mar_irradiance(all_data,"RVE7","g14024.t1","Ambient_Mar", "Cold_Mar")
n4 <- plot_jun_irradiance(all_data,"RVE7","g14024.t1","Ambient_Jun", "Cold_Jun")

o1 <- plot_sep_irradiance(all_data,"RVE8","g06095.t1","Ambient_Sep", "Cold_Sep") #RVE8
o2 <- plot_dec_irradiance(all_data,"RVE8","g06095.t1","Ambient_Dec", "Cold_Dec")
o3 <- plot_mar_irradiance(all_data,"RVE8","g06095.t1","Ambient_Mar", "Cold_Mar")
o4 <- plot_jun_irradiance(all_data,"RVE8","g06095.t1","Ambient_Jun", "Cold_Jun")



clockgenes1 <-  ggarrange(a1,a2,a3,a4,b1,b2,b3,b4,
                          c1,c2,c3,c4,d1,d2,d3,d4,e1,e2,e3,e4,
                          ncol = 4, nrow = 5)
ggsave(plot =clockgenes1 , "Supplementary_Figure_clock_genes1.pdf", width = 210, height = 297, units = "mm", dpi = "retina")


clockgenes2 <-  ggarrange(f1,f2,f3,f4,g1,g2,g3,g4,
                          h1,h2,h3,h4,i1,i2,i3,i4,j1,j2,j3,j4,
                          ncol = 4, nrow = 5)
ggsave(plot =clockgenes2 , "Supplementary_Figure_clock_genes2.pdf", width = 210, height = 297, units = "mm", dpi = "retina")


clockgenes3 <-  ggarrange(k1,k2,k3,k4,l1,l2,l3,l4,
                          m1,m2,m3,m4,n1,n2,n3,n4,o1,o2,o3,o4,
                          ncol = 4, nrow = 5)
ggsave(plot =clockgenes3 , "Supplementary_Figure_clock_genes3.pdf", width = 210, height = 297, units = "mm", dpi = "retina")


