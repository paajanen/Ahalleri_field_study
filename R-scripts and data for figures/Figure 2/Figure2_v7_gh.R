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

#Figure 2, smooth contour plots January 2026, Pirita Paajanen

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

