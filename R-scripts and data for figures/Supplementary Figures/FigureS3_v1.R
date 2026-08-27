library(readxl)
library(tidyverse)
library(ggplot2)
library(gridExtra)


transcript_data_romanowski <- read_tsv("LUX_ELF4_Romanowski.txt", col_names = TRUE) 
transpose_dataset <- function(df) {
  df <- df %>%  
    gather(key = var_name, value = value, 2:ncol(df)) %>% 
    spread(key = names(df)[1],value = 'value')
}
tr_romanowski <- transpose_dataset(transcript_data_romanowski)

#The next column tells which day of collection
day1 <- rep("Day1",12)
day2 <- rep("Day2",12)
Day <- c(day1,day2)
Day

#The next column tells what time point has the plant been collected
Timepoint <- c(0,0,4,4,8,8,12,12,16,16,20,20,24,24,28,28,32,32,36,36,40,40,44,44)

metadata <- tibble(Day, Timepoint)
metadata

romanowski_full <-  cbind(tr_romanowski,metadata)



View(tr_romanowski)
Romanowski_A<- filter(romanowski_full, grepl('A', var_name))
Romanowski_B<- filter(romanowski_full, grepl('B', var_name))


View(Romanowski_A)

#AT2G46830, CCA1
#AT1G01060, LHY

CCA1A <- ggplot() + # FOR DIFFERENT SEASONS, UNCHECK AND CHECK THESE GEOM_RECTS.
  geom_point(data = Romanowski_A , aes(x = rescale(AT2G46830), y = rescale(AT1G01060),colour=Day), size=0.8)+ # THE TRANSCRIPT LEVEL
  geom_path(data = Romanowski_A , aes(x = rescale(AT2G46830), y = rescale(AT1G01060),colour=Day), size=0.8, alpha=0.8, arrow=arrow()) +
  labs(x=" ")+ # THE X-LABEL
  scale_color_manual(values=c("yellow","pink"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ # THE BACKGROUND OF THE GRAPH, NO GRID
  theme(axis.line = element_line(colour = "black", size = 1)) +  # DRAWING THE BLACK LINES AROUND THE PLOT
  theme(legend.position="none")+ # THE COLOUR CODING BY PLANT POSITION
  coord_fixed(ratio=0.66, xlim=c(0,1), ylim=c(0,1))+
  scale_y_continuous(expand = c(0, 0),
                     expression("Expression of CCA1"))+
  scale_x_continuous(expand = c(0, 0),
                     expression("Expression of LHY"))+
  theme(axis.text=element_text(size=2, colour = "black"),
        axis.title=element_text(size=1.6,face="bold")) +
  ggtitle("CCA1/LHY expressions") +
  theme(plot.title = element_text(size = 4, face = "bold"))
          

CCA1B <- ggplot() + # FOR DIFFERENT SEASONS, UNCHECK AND CHECK THESE GEOM_RECTS.
  geom_point(data = Romanowski_B , aes(x = rescale(AT2G46830), y = rescale(AT1G01060),colour=Day), size=0.8)+ # THE TRANSCRIPT LEVEL
  geom_path(data = Romanowski_B , aes(x = rescale(AT2G46830), y = rescale(AT1G01060),colour=Day), size=0.8, alpha=0.8, arrow=arrow()) +
  labs(x=" ")+ # THE X-LABEL
  scale_color_manual(values=c("yellow","pink"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ # THE BACKGROUND OF THE GRAPH, NO GRID
  theme(axis.line = element_line(colour = "black", size = 1)) +  # DRAWING THE BLACK LINES AROUND THE PLOT
  theme(legend.position="none")+ # THE COLOUR CODING BY PLANT POSITION
  coord_fixed(ratio=0.66, xlim=c(0,1), ylim=c(0,1))+
  scale_y_continuous(expand = c(0, 0),
                     expression("Expression of CCA1"))+
  scale_x_continuous(expand = c(0, 0),
                     expression("Expression of LHY"))+
  theme(axis.text=element_text(size=2, colour = "black"),
        axis.title=element_text(size=1.6,face="bold")) +
  ggtitle("CCA1/LHY expressions") +
  theme(plot.title = element_text(size = 4, face = "bold"))

#AT3G46640 ELF4
#AT2G40080 LUX
  
ELF4A <- ggplot() + # FOR DIFFERENT SEASONS, UNCHECK AND CHECK THESE GEOM_RECTS.
  geom_point(data = Romanowski_A , aes(x = rescale(AT3G46640), y = rescale(AT2G40080),colour=Day), size=0.8)+ # THE TRANSCRIPT LEVEL
  geom_path(data = Romanowski_A , aes(x = rescale(AT3G46640), y = rescale(AT2G40080),colour=Day), size=0.8, alpha=0.8, arrow=arrow()) +
  labs(x=" ")+ # THE X-LABEL
  scale_color_manual(values=c("yellow","pink"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ # THE BACKGROUND OF THE GRAPH, NO GRID
  theme(axis.line = element_line(colour = "black", size = 1)) +  # DRAWING THE BLACK LINES AROUND THE PLOT
  theme(legend.position="none")+ # THE COLOUR CODING BY PLANT POSITION
  coord_fixed(ratio=0.66, xlim=c(0,1), ylim=c(0,1))+
  scale_y_continuous(expand = c(0, 0),
                     expression("Expression of LUX"))+
  scale_x_continuous(expand = c(0, 0),
                     expression("Expression of ELF4"))+
  theme(axis.text=element_text(size=2, colour = "black"),
        axis.title=element_text(size=1.6,face="bold")) +
  ggtitle("LUX/ELF4 expressions") +
  theme(plot.title = element_text(size = 4, face = "bold"))

ELF4B <- ggplot() + # FOR DIFFERENT SEASONS, UNCHECK AND CHECK THESE GEOM_RECTS.
  geom_point(data = Romanowski_B , aes(x = rescale(AT3G46640), y = rescale(AT2G40080),colour=Day), size=0.8)+ # THE TRANSCRIPT LEVEL
  geom_path(data = Romanowski_B , aes(x = rescale(AT3G46640), y = rescale(AT2G40080),colour=Day), size=0.8, alpha=0.8, arrow=arrow()) +
  labs(x=" ")+ # THE X-LABEL
  scale_color_manual(values=c("yellow","pink"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ # THE BACKGROUND OF THE GRAPH, NO GRID
  theme(axis.line = element_line(colour = "black", size = 1)) +  # DRAWING THE BLACK LINES AROUND THE PLOT
  theme(legend.position="none")+ # THE COLOUR CODING BY PLANT POSITION
  coord_fixed(ratio=0.66, xlim=c(0,1), ylim=c(0,1))+
  scale_y_continuous(expand = c(0, 0),
                     expression("Expression of LUX"))+
  scale_x_continuous(expand = c(0, 0),
                     expression("Expression of ELF4"))+
  theme(axis.text=element_text(size=2, colour = "black"),
        axis.title=element_text(size=1.6,face="bold")) +
  ggtitle("LUX/ELF4 expressions") +
  theme(plot.title = element_text(size = 4, face = "bold"))

CCA1_ELF4 <- ggarrange(CCA1A,CCA1B,ELF4A,ELF4B,
                         ncol = 2, nrow = 2)

CCA1_ELF4
ggsave(plot =CCA1_ELF4 , "CCA1_ELF4_limit_cycle_260709.pdf", width = 210, height = 297, units = "mm", dpi = "retina")

  
