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



transcript_data_annotated <- read_tsv("analysis/1-metadata_imputed.txt", col_names = TRUE) 
dim(transcript_data_annotated)
weather_data <- read_csv("data/Sample_environ_data_long_format_irradiance.csv", col_names=TRUE)
#View(weather_data)
weather <- weather_data %>% dplyr::select(all_of("sample_ID"),all_of("Temperature"), all_of("Irradiance"))
head(weather)
sample_weather <- read_csv("data/Sample_environ_data.csv", col_names=TRUE)
irradiance <- read_excel("analysis/irradiance_data_raw.xlsx", col_names=TRUE)
irradiance <- irradiance %>% rename(Timepoint = "Sampling Time")



#View(irradiance)
#View(weather)
#View(transcript_data_annotated)
dim(transcript_data_annotated)
head(transcript_data_annotated)
all_data <- merge(transcript_data_annotated,weather , by="sample_ID", all=FALSE)
head(all_data)
#View(all_data)

library(latticeExtra)

Choose_transcript_and_plot_Sep1 <- function(all_data,name,transcript) {
  Sep_Cold_transcript <- all_data %>% dplyr::select("sample_ID", "Timepoint","Plant", transcript) %>% filter(Plant %in% "Cold_Sep") %>% rename(Sepcold = transcript)
  Sep_Ambient_transcript <- all_data %>% dplyr::select("sample_ID", "Timepoint","Plant", transcript) %>% filter(Plant %in% "Ambient_Sep")  %>% rename(Sepambient = transcript)                                                                                    
  Sep_transcript <- merge(Sep_Cold_transcript,Sep_Ambient_transcript, by="Timepoint", all=FALSE)    
  Sep_transcript <- Sep_transcript %>% mutate(delta = Sepcold - Sepambient)
  Sep_weather <- sample_weather %>% dplyr::select("Sample","S1_Amb", "S1_LUX"  ) %>% rename(Timepoint = Sample)
  head(Sep_weather)
  test <- merge(Sep_transcript , Sep_weather, by="Timepoint", all=FALSE)
  test <- merge(test, irradiance,by="Timepoint", all=FALSE )
  #View(test)
  levelplot(Sepambient ~ S1_Amb * S1_light, data = test,  cex = 0.3, contour=TRUE, panel=panel.levelplot.points,xlab=list(vjust=-2,cex=0.3),ylab=list(vjust=2,cex=0.3),
            margin=F,
            colorkey=list(space = "bottom", labels=list(cex=0.3, font=2, col="black"),
                          height=1, width=1,
                          row=3, column=1, vjust=-2),
            scales=list(tck = c(1,0),y=list(cex=0.3),x=list(cex=0.3)),
            aspect=0.66,
            main=list(name,vjust=4,transcript,side=0.2,line=0.2,size=0.2,cex=0.5))+
    layer_(panel.2dsmoother(..., n = 100))}

Choose_transcript_and_plot_Sep1(all_data,"RVE1 September","g00040.t1")


Choose_transcript_and_plot_Mar1 <- function(all_data,name,transcript) {
  Mar_Cold_transcript <- all_data %>% dplyr::select("sample_ID", "Timepoint","Plant", transcript) %>% filter(Plant %in% "Cold_Mar") %>% rename(Marcold = transcript)
  Mar_Ambient_transcript <- all_data %>% dplyr::select("sample_ID", "Timepoint","Plant", transcript) %>% filter(Plant %in% "Ambient_Mar")  %>% rename(Marambient = transcript)                                                                                    
  Mar_transcript <- merge(Mar_Cold_transcript,Mar_Ambient_transcript, by="Timepoint", all=FALSE)    
  Mar_transcript <- Mar_transcript %>% mutate(delta = Marcold - Marambient)
  Mar_weather <- sample_weather %>% dplyr::select("Sample","M1_Amb", "M1_LUX"  ) %>% rename(Timepoint = Sample)
  head(Mar_weather)
  test <- merge(Mar_transcript , Mar_weather, by="Timepoint", all=FALSE)
  test <- merge(test, irradiance,by="Timepoint", all=FALSE )
  #View(test)
  levelplot(Marambient ~ M1_Amb * M1_light, data = test,  cex = 0.3, contour=TRUE, panel=panel.levelplot.points,xlab=list(vjust=-2,cex=0.3),ylab=list(vjust=2,cex=0.3),
            margin=F,
            colorkey=list(space = "bottom", labels=list(cex=0.3, font=2, col="black"),
                          height=1, width=1,
                          row=3, column=1, vjust=-2),
            scales=list(tck = c(1,0),y=list(cex=0.3),x=list(cex=0.3)),
            aspect=0.66,
            main=list(name,vjust=4,transcript,side=0.2,line=0.2,size=0.2,cex=0.5))+
    layer_(panel.2dsmoother(..., n = 100))}

Choose_transcript_and_plot_Mar1(all_data,"RVE1 September","g00040.t1")

Choose_transcript_and_plot_Jun1 <- function(all_data,name,transcript) {
  Jun_Cold_transcript <- all_data %>% dplyr::select("sample_ID", "Timepoint","Plant", transcript) %>% filter(Plant %in% "Cold_Jun") %>% rename(Juncold = transcript)
  Jun_Ambient_transcript <- all_data %>% dplyr::select("sample_ID", "Timepoint","Plant", transcript) %>% filter(Plant %in% "Ambient_Jun")  %>% rename(Junambient = transcript)                                                                                    
  Jun_transcript <- merge(Jun_Cold_transcript,Jun_Ambient_transcript, by="Timepoint", all=FALSE)    
  Jun_transcript <- Jun_transcript %>% mutate(delta = Juncold - Junambient)
  Jun_weather <- sample_weather %>% dplyr::select("Sample","J1_Amb", "J1_LUX"  ) %>% rename(Timepoint = Sample)
  head(Jun_weather)
  test <- merge(Jun_transcript , Jun_weather, by="Timepoint", all=FALSE)
  test <- merge(test, irradiance,by="Timepoint", all=FALSE )
  #View(test)
  levelplot(Junambient ~ J1_Amb * J1_light, data = test,  cex = 0.3, contour=TRUE, panel=panel.levelplot.points,xlab=list(vjust=-2,cex=0.3),ylab=list(vjust=2,cex=0.3),
            margin=F,
            colorkey=list(space = "bottom", labels=list(cex=0.3, font=2, col="black"),
                          height=1, width=1,
                          row=3, column=1, vjust=-2),
            scales=list(tck = c(1,0),y=list(cex=0.3),x=list(cex=0.3)),
            aspect=0.66,
            main=list(name,vjust=4,transcript,side=0.2,line=0.2,size=0.2,cex=0.5))+
    layer_(panel.2dsmoother(..., n = 100))}

Choose_transcript_and_plot_Jun1(all_data,"RVE1 September","g00040.t1")
Choose_transcript_and_plot_Dec1 <- function(all_data,name,transcript) {
  Dec_Cold_transcript <- all_data %>% dplyr::select("sample_ID", "Timepoint","Plant", transcript) %>% filter(Plant %in% "Cold_Dec") %>% rename(Deccold = transcript)
  Dec_Ambient_transcript <- all_data %>% dplyr::select("sample_ID", "Timepoint","Plant", transcript) %>% filter(Plant %in% "Ambient_Dec")  %>% rename(Decambient = transcript)                                                                                    
  Dec_transcript <- merge(Dec_Cold_transcript,Dec_Ambient_transcript, by="Timepoint", all=FALSE)    
  Dec_transcript <- Dec_transcript %>% mutate(delta = Deccold - Decambient)
  Dec_weather <- sample_weather %>% dplyr::select("Sample","D1_Amb", "D1_LUX"  ) %>% rename(Timepoint = Sample)
  head(Dec_weather)
  test <- merge(Dec_transcript , Dec_weather, by="Timepoint", all=FALSE)
  test <- merge(test, irradiance,by="Timepoint", all=FALSE )
  #View(test)
  levelplot(Decambient ~ D1_Amb * D1_light, data = test,  cex = 0.3, contour=TRUE, panel=panel.levelplot.points,xlab=list(vjust=-2,cex=0.3),ylab=list(vjust=2,cex=0.3),
            margin=F,
            colorkey=list(space = "bottom", labels=list(cex=0.3, font=2, col="black"),
                          height=1, width=1,
                          row=3, column=1, vjust=-2),
            scales=list(tck = c(1,0),y=list(cex=0.3),x=list(cex=0.3)),
            aspect=0.66,
            main=list(name,vjust=4,transcript,side=0.2,line=0.2,size=0.2,cex=0.5))+
    layer_(panel.2dsmoother(..., n = 100))}
Choose_transcript_and_plot_Dec1(all_data,"RVE1 June","g00040.t1")

p2 <- Choose_transcript_and_plot_Dec1(all_data,"CCA1","g09640.t1")
p2
ggsave(plot=p2 , "zeit_testp2_20240925.pdf", width = 210, height = 297, units = "mm", dpi = "retina")

grid.arrange(d1,d2,d3,d4,p2, p2,p2,p2, ncol = 4, nrow=2)

zeit_test <- ggarrange(d1,d2,d3,d4,p2,p2,p2,p2,
                       ncol = 4, nrow = 2)

#zeit_test
ggsave(plot =zeit_test , "zeit_test_20240925.pdf", width = 210, height = 297, units = "mm", dpi = "retina")


#LUX
f1 <- Choose_transcript_and_plot_Sep1(all_data_smoothed,"LUX","g12727.t1")
f2 <- Choose_transcript_and_plot_Dec1(all_data_smoothed,"LUX","g12727.t1")
f3 <- Choose_transcript_and_plot_Mar1(all_data_smoothed,"LUX","g12727.t1")
f4 <- Choose_transcript_and_plot_Jun1(all_data_smoothed,"LUX","g12727.t1")

#PRR9
e1 <- Choose_transcript_and_plot_Sep1(all_data_smoothed,"PRR9","g09644.t1")
e2 <- Choose_transcript_and_plot_Dec1(all_data_smoothed,"PRR9","g09644.t1")
e3 <- Choose_transcript_and_plot_Mar1(all_data_smoothed,"PRR9","g09644.t1")
e4 <- Choose_transcript_and_plot_Jun1(all_data_smoothed,"PRR9","g09644.t1")


#CCA1 g09640.t1 
d1 <- Choose_transcript_and_plot_Sep1(all_data_smoothed,"CCA1","g09640.t1")
d2 <- Choose_transcript_and_plot_Dec1(all_data_smoothed,"CCA1","g09640.t1")
d3 <- Choose_transcript_and_plot_Mar1(all_data_smoothed,"CCA1","g09640.t1")
d4 <- Choose_transcript_and_plot_Jun1(all_data_smoothed,"CCA1","g09640.t1")


zeit_test <- ggarrange(f1,f2,f3,f4,e1,e2,e3,e4,d1,d2,d3,d4,
          ncol = 4, nrow = 3)

zeit_test



ggsave(plot =zeit_test , "Figure3_levelplots_20241002.pdf", width = 210, height = 297, units = "mm", dpi = "retina")


zeit <- grid.arrange(p1,p2,p3,p4,q1,q2,q3,q4,r1,r2,r3,r4,
                     ncol=4,nrow=3)

zeit
#ggsave(plot =zeit , "zeit_20240904.pdf", width = 210, height = 297, units = "mm", dpi = "retina")

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
View(all_data)


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

plot_limit_cycle(all_data,"CCA1/LHY limit cycles September","g09640.t1","g01821.t2", "Ambient_Sep")

g1 <- plot_limit_cycle(all_data_smoothed_lc,"CCA1/LHY limit cycles September","g09640.t1","g01821.t2", "Ambient_Sep")
g2 <- plot_limit_cycle(all_data_smoothed_lc,"CCA1/LHY limit cycles December","g09640.t1","g01821.t2", "Ambient_Dec")
g3 <- plot_limit_cycle(all_data_smoothed_lc,"CCA1/LHY limit cycles March","g09640.t1","g01821.t2", "Ambient_Mar")
g4 <- plot_limit_cycle(all_data_smoothed_lc,"CCA1/LHY limit cycles June","g09640.t1","g01821.t2", "Ambient_Jun")

g4
h1 <- plot_limit_cycle(all_data_smoothed_lc,"PRR7/PRR9 limit cycles","g19424.t1","g09644.t1", "Ambient_Sep")
h2 <- plot_limit_cycle(all_data_smoothed_lc,"PRR7/PRR9 limit cycles","g19424.t1","g09644.t1", "Ambient_Dec")
h3 <- plot_limit_cycle(all_data_smoothed_lc,"PRR7/PRR9 limit cycles","g19424.t1","g09644.t1", "Ambient_Mar")
h4 <- plot_limit_cycle(all_data_smoothed_lc,"PRR7/PRR9 limit cycles","g19424.t1","g09644.t1", "Ambient_Jun")


Figure3_pt1 <- ggarrange(a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4,e1,e2,e3,e4,#f1,f2,f3,f4,g1,g2,g3,g4,h1,h2,h3,h4,
                       ncol = 4, nrow = 5)


ggsave(plot =Figure3_pt1 , "Figure3_pt1_smoothed_20241002.pdf", width = 210, height = 297, units = "mm", dpi = "retina")

Figure3_pt2 <- ggarrange(f1,f2,f3,f4, g1,g2,g3,g4,h1,h2,h3,h4,
                       ncol = 4, nrow = 3)

ggsave(plot =Figure3_pt2 , "Figure3_pt2_smoothed_20241002.pdf", width = 210, height = 297, units = "mm", dpi = "retina")


Figure3_all <- ggarrange(a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4,e1,e2,e3,e4,f1,f2,f3,f4,g1,g2,g3,g4,h1,h2,h3,h4,
                     ncol=4,nrow=8)

ggsave(plot =Figure3_all , "Figure3_all_smoothed_20241002.pdf", width = 210, height = 297, units = "mm", dpi = "retina")


