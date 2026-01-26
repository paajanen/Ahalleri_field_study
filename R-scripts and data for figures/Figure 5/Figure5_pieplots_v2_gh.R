library(ggplot2)
library(readxl)
library(tidyverse)

classes <- read_tsv("classification_max_with_noise_2024_04_25.txt", col_names = TRUE) 
classes <- as.data.frame(classes)


September_counts <- classes %>%
  count(September) %>%
  group_by(September) 

September_counts

September_counts <- September_counts %>% mutate(September = replace(September, September== "X1", "No expression"))
September_counts <- September_counts %>% mutate(September = replace(September, September== "X2", "No change"))
September_counts <- September_counts %>% mutate(September = replace(September, September== "X3", "Cyclic induction"))
September_counts <- September_counts %>% mutate(September = replace(September, September== "X4", "Cyclic repression"))
September_counts <- September_counts %>% mutate(September = replace(September, September== "X5", "Linear induction"))
September_counts <- September_counts %>% mutate(September = replace(September, September== "X6", "Linear repression"))
September_counts <- September_counts %>% mutate(September = replace(September, September== "X7", "Phase later"))
September_counts <- September_counts %>% mutate(September = replace(September, September== "X8", "Discrete induction"))
September_counts <- September_counts %>% mutate(September = replace(September, September== "X9", "Discrete repression"))
September_counts <- September_counts %>% mutate(September = replace(September, September== "X10","Unclassified"))

September_counts_filtered <- September_counts %>% filter (September %in% c("Linear induction","Linear repression",
                                                                           "Phase later", "Discrete induction",
                                                                           "Discrete repression"))
September_counts_filtered

September_pie <- 
  ggplot(September_counts_filtered, aes(x="", y=n, fill=September)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  scale_fill_brewer(palette="PuOr")+
  theme_void() # remove background, grid, numeric labels


ggsave(plot=September_pie, "FigureSupp_September_pie_080126.pdf", width = 210, height = 297, units = "mm", dpi = "retina")



December_counts <- classes %>%
  count(December) %>%
  group_by(December) 


December_counts <- December_counts %>% mutate(December = replace(December, December== "X1", "No expression"))
December_counts <- December_counts %>% mutate(December = replace(December, December== "X2", "No change"))
December_counts <- December_counts %>% mutate(December = replace(December, December== "X3", "Cyclic induction"))
December_counts <- December_counts %>% mutate(December = replace(December, December== "X4", "Cyclic repression"))
December_counts <- December_counts %>% mutate(December = replace(December, December== "X5", "Linear induction"))
December_counts <- December_counts %>% mutate(December = replace(December, December== "X6", "Linear repression"))
December_counts <- December_counts %>% mutate(December = replace(December, December== "X7", "Phase later"))
December_counts <- December_counts %>% mutate(December = replace(December, December== "X8", "Discrete induction"))
December_counts <- December_counts %>% mutate(December = replace(December, December== "X9", "Discrete repression"))
December_counts <- December_counts %>% mutate(December = replace(December, December== "X10", "Unclassified"))

December_counts

December_counts_filtered <- December_counts %>% filter (December %in% c("Linear induction","Linear repression",
                                                                           "Phase later", "Discrete induction",
                                                                           "Discrete repression"))
December_counts_filtered


December_pie <- ggplot(December_counts_filtered, aes(x="", y=n, fill=December)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  scale_fill_brewer(palette="PuOr")+
  theme_void() # remove background, grid, numeric labels

December_pie
ggsave(plot=December_pie, "FigureSupp_December_pie_080126.pdf", width = 210, height = 297, units = "mm", dpi = "retina")


March_counts <- classes %>%
  count(March) %>%
  group_by(March) 

March_counts <- March_counts %>% mutate(March = replace(March, March== "X1", "No expression"))
March_counts <- March_counts %>% mutate(March = replace(March, March== "X2", "No change"))
March_counts <- March_counts %>% mutate(March = replace(March, March== "X3", "Cyclic induction"))
March_counts <- March_counts %>% mutate(March = replace(March, March== "X4", "Cyclic repression"))
March_counts <- March_counts %>% mutate(March = replace(March, March== "X5", "Linear induction"))
March_counts <- March_counts %>% mutate(March = replace(March, March== "X6", "Linear repression"))
March_counts <- March_counts %>% mutate(March = replace(March, March== "X7", "Phase later"))
March_counts <- March_counts %>% mutate(March = replace(March, March== "X8", "Discrete induction"))
March_counts <- March_counts %>% mutate(March = replace(March, March== "X9", "Discrete repression"))
March_counts <- March_counts %>% mutate(March = replace(March, March== "X10", "Unclassified"))

March_counts

March_counts_filtered <- March_counts %>% filter (March %in% c("Linear induction","Linear repression",
                                                                        "Phase later", "Discrete induction",
                                                                        "Discrete repression"))
March_counts_filtered

March_pie <- ggplot(March_counts_filtered, aes(x="", y=n, fill=March)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  scale_fill_brewer(palette="PuOr")+
  theme_void() # remove background, grid, numeric labels

ggsave(plot=March_pie, "FigureSupp_March_pie_080126.pdf", width = 210, height = 297, units = "mm", dpi = "retina")


Classes_June <- classes


Classes_June <- Classes_June %>% mutate(June = replace(June, June== "X1", "No expression"))
Classes_June <- Classes_June %>% mutate(June = replace(June, June== "X2", "No change"))
Classes_June <- Classes_June %>% mutate(June = replace(June, June== "X3", "Unclassified"))
Classes_June <- Classes_June %>% mutate(June = replace(June, June== "X4", "Unclassified"))
Classes_June <- Classes_June %>% mutate(June = replace(June, June== "X5", "Linear induction"))
Classes_June <- Classes_June %>% mutate(June = replace(June, June== "X6", "Linear repression"))
Classes_June <- Classes_June %>% mutate(June = replace(June, June== "X7", "Phase later"))
Classes_June <- Classes_June %>% mutate(June = replace(June, June== "X8", "Discrete induction"))
Classes_June <- Classes_June %>% mutate(June = replace(June, June== "X9", "Discrete repression"))
Classes_June <- Classes_June %>% mutate(June = replace(June, June== "X10", "Unclassified"))


June_counts <-Classes_June %>%
  count(June) %>%
  group_by(June) 
June_counts

June_counts_filtered <- June_counts %>% filter (June %in% c("Linear induction","Linear repression",
                                                               "Phase later", "Discrete induction",
                                                               "Discrete repression"))

June_counts_filtered

#June_counts <- June_counts %>% mutate(June = replace(June, June== "X1", "No expression"))
#June_counts <- June_counts %>% mutate(June = replace(June, June== "X2", "No change"))
#June_counts <- June_counts %>% mutate(June = replace(June, June== "X3", "Cyclic Induction"))
#June_counts <- June_counts %>% mutate(June = replace(June, June== "X4", "Cyclic repression"))
#June_counts <- June_counts %>% mutate(June = replace(June, June== "X5", "Linear induction"))
#June_counts <- June_counts %>% mutate(June = replace(June, June== "X6", "Linear repression"))
#June_counts <- June_counts %>% mutate(June = replace(June, June== "X7", "Phase later"))
#June_counts <- June_counts %>% mutate(June = replace(June, June== "X8", "Discrete induction"))
#June_counts <- June_counts %>% mutate(June = replace(June, June== "X9", "Discrete repression"))
#June_counts <- June_counts %>% mutate(June = replace(June, June== "X10", "Unclassified"))


June_pie <- ggplot(June_counts_filtered, aes(x="", y=n, fill=June)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  scale_fill_brewer(palette="PuOr")+
  theme_void()

ggsave(plot=June_pie, "FigureSupp_June_pie_080126.pdf", width = 210, height = 297, units = "mm", dpi = "retina")

#ggtitle("June") +
  #theme(plot.title = element_text(hjust = 0.5)) +
  #theme_void() # remove background, grid, numeric labels
dev.off()


