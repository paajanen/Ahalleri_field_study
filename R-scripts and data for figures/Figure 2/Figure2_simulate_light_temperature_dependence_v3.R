library(tidyverse)
library(ggpubr)
library(readxl)
library(lattice)
library(gridExtra)
library(ggplot2)
library(latticeExtra)
library(dplyr)

sample_weather <- read_csv("data/Sample_environ_data.csv", col_names=TRUE)
irradiance <- read_excel("analysis/irradiance_data_raw.xlsx", col_names=TRUE)
irradiance <- irradiance %>% rename("Sample" = "Sampling Time")
data_temp_irr <- merge(sample_weather,irradiance, by ="Sample")

ggplot()+geom_path(data = data_temp_irr , aes(x = Sample, y = S1_Amb), size=0.9, alpha=0.8, colour="red")

Sample <- seq(1, 48, by=1)
x <- seq(0,4*pi,length.out=48)
y1_expr <- 3*cos(x)+3
irr <- 500*sin(x)+500 #*1000
temp <- 15*sin(x+(pi+1)/2)+15
y2_expr <- 3*cos(x+7.5)+3
test <- as_tibble(cbind(Sample,y1_expr,y2_expr,irr,temp))
test <- merge(test,data_temp_irr,by="Sample")

View(test)

sep_rects_irradience <- data.frame(xstart = c(0,3.30,14.20,27.30,38.20), xend = c(3.30,14.20,27.30,38.20,49),
                                   col = c("white","gray50","white","gray50","white"))
# Changed every number by 1, to make it better


sep_rects_irradience_irradiance <- data.frame(xstart = c(0,4,14,28,38), xend = c(4,14,28,38,48),
                                              col = c("white","gray50","white","gray50","white"))


# THESE ARE THE SUNRISE AND SUNSET TIMES FOR EACH SEASON TO GET THE NIGHT-TIME SHADING CORRECTLY IN THE PLOTS

ggplot() +   geom_line(data = data_temp_irr , aes(x = Sample, y = S1_Amb), linewidth=0.5) + # THE TRANSCRIPT LEVEL
  geom_line(data = test , aes(x = Sample, y = y1_expr), linewidth=0.5, linetype = "dashed")+
  geom_rect(data = sep_rects_irradience, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.3)
  

ggplot() +   geom_line(data = data_temp_irr , aes(x = Sample, y = S1_light), linewidth=0.5) + # THE TRANSCRIPT LEVEL
  geom_line(data = test , aes(x = Sample, y = y2_expr), linewidth=0.5, linetype = "dashed")+
  geom_rect(data = sep_rects_irradience, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.3)



simulation_temperature <- ggplot()+  geom_path(data = test , aes(x =Sample, y = y1_expr), size=0.9, alpha=0.8, colour="blue")+
  geom_path(data = data_temp_irr , aes(x = Sample, y = S1_Amb/2), size=0.9, alpha=0.8, colour="red")+
  geom_rect(data = sep_rects_irradience, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.3)+
  scale_fill_manual(values=c("darkgrey","white","white"), guide=FALSE)+ #THE NIGHT RECTANLGES DONE!
  scale_x_continuous(expand = c(0, 0),breaks=seq(1,49,by=8) ,labels = c("16","00","08","16","00","08","16"))+ # PUTS THE LITTLE TICKS ON THE X-AXIS
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
  
simulation_irradiance <- 
  ggplot()+  geom_path(data = test , aes(x =Sample, y = y2_expr), size=0.9, alpha=0.8, colour="blue")+
  geom_path(data = data_temp_irr , aes(x = Sample, y = S1_light/150), size=0.9, alpha=0.8, colour="red")+
  geom_rect(data = sep_rects_irradience, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.3)+
  scale_fill_manual(values=c("darkgrey","white","white"), guide=FALSE)+ #THE NIGHT RECTANLGES DONE!
  scale_x_continuous(expand = c(0, 0),breaks=seq(1,49,by=8) ,labels = c("16","00","08","16","00","08","16"))+ # PUTS THE LITTLE TICKS ON THE X-AXIS
  coord_fixed(ratio=1.5, xlim=c(0.9,48.1), ylim=c(-0.1,16.5))+ # SCALING THE PLOTS RATIOS
  labs(x=" ")+ # THE X-LABEL
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ # THE BACKGROUND OF THE GRAPH, NO GRID
  theme(axis.line = element_line(colour = "black", size = 1))  + # DRAWING THE BLACK LINES AROUND THE PLOT
  theme(legend.position="none")+ # THE COLOUR CODING BY PLANT POSITION
  scale_y_continuous(expand = c(0, 0),
                     expression("TPM"),
                     sec.axis = sec_axis(~ . * 150, name = "Irradiance", breaks = seq(0,2500,500)))+ # THE NAMES OF THE Y-AXIS LABELS, BOTH SIDES
  theme(axis.text=element_text(size=20, colour = "black"),
        axis.title=element_text(size=16,face="bold"))

simulation_temperature
simulation_irradiance

temperature_plot <- levelplot(y1_expr ~ S1_Amb * S1_light, data = test,  cex = 0.3, contour=TRUE, panel=panel.levelplot.points,xlab=list(vjust=-2,cex=0.3),ylab=list(vjust=2,cex=0.3),
          margin=F,
          colorkey=list(space = "bottom", labels=list(cex=0.3, font=2, col="black"),
                        height=1, width=1,
                        row=3, column=1, vjust=-2),
          scales=list(tck = c(1,0),y=list(cex=0.3),x=list(cex=0.3)),
          aspect=0.66,
          main=list("Dependent on Temperature",vjust=4,side=0.2,line=0.2,size=0.2,cex=0.5))+
  layer_(panel.2dsmoother(..., n = 100))

irradiance_plot <- levelplot(y2_expr ~ S1_Amb * S1_light, data = test,  cex = 0.3, contour=TRUE, panel=panel.levelplot.points,xlab=list(vjust=-2,cex=0.3),ylab=list(vjust=2,cex=0.3),
          margin=F,
          colorkey=list(space = "bottom", labels=list(cex=0.3, font=2, col="black"),
                        height=1, width=1,
                        row=3, column=1, vjust=-2),
          scales=list(tck = c(1,0),y=list(cex=0.3),x=list(cex=0.3)),
          aspect=0.66,
          main=list("Dependent on Irradiance",vjust=4,side=0.2,line=0.2,size=0.2,cex=0.5))+
          layer_(panel.2dsmoother(..., n = 100))






simulations <- ggarrange(simulation_temperature, simulation_irradiance, temperature_plot,irradiance_plot,
          ncol = 2, nrow = 2)

simulations
ggsave(plot=simulations, "Figure3_irradiance_simulation_plot_240926.pdf", width = 210, height = 297, units = "mm", dpi = "retina")

