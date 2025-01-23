
library(readxl)
library(tidyverse)
#install.packages("UpSetR")
library(UpSetR)

classes <- read_tsv("classification_max_with_noise_2024_04_25.txt", col_names = TRUE) 

classes <- as.data.frame(classes)

classes_continuous <- classes %>% filter(September=="X7" | December=="X7" | March=="X7" | June=="X7" ) 
count(classes_continuous)


classes_continuous <- classes_continuous %>% mutate(September=case_when(September == "X7" ~ 1,TRUE ~ 0)) %>%
  mutate(December=case_when(December == "X7" ~ 1, TRUE ~ 0)) %>%
  mutate(March=case_when(March == "X7" ~ 1,TRUE ~ 0 )) %>%
  mutate(June=case_when(June == "X7" ~ 1, TRUE ~ 0 ))

classes_continuous <- as.data.frame(classes_continuous)
count(classes_continuous)
#names(classes_continuous) <-NULL
classes_continuous
Classes_continuous <- upset(classes_continuous, nsets = 4, number.angles = 0, point.size = 3.5, line.size = 2, 
      mainbar.y.label = "Seasonal intersections", sets.x.label = "Continously gated genes", 
      text.scale = c(1.3, 1.3, 1, 1, 2, 0.75), order.by = "freq", 
      sets.bar.color=c( "#FFD535","#860124", "#FB2372","#76AFB6"))
Classes_continuous
pdf(file="Fig2_Upset_continuous_gating_2024_04_25.pdf", width = 6, height = 6) # or other device
Classes_continuous
dev.off()



discrete_induction <- classes %>% filter(September=="X8" | December=="X8" | March=="X8" | June=="X8" ) 
count(discrete_induction)


discrete_induction <- discrete_induction %>% mutate(September=case_when(September == "X8" ~ 1,TRUE ~ 0)) %>%
  mutate(December=case_when(December == "X8" ~ 1, TRUE ~ 0)) %>%
  mutate(March=case_when(March == "X8" ~ 1,TRUE ~ 0 )) %>%
  mutate(June=case_when(June == "X8" ~ 1, TRUE ~ 0 ))

discrete_induction <- as.data.frame(discrete_induction)
count(discrete_induction)
Discrete_induction <- upset(discrete_induction, nsets = 4, number.angles = 0, point.size = 3.5, line.size = 2, 
      mainbar.y.label = "Seasonal intersections", sets.x.label = "Discrete induction", 
      text.scale = c(1.3, 1.3, 1, 1, 2, 0.75), order.by = "freq",
      sets.bar.color=c( "#FFD535","#860124", "#FB2372","#76AFB6"))

pdf(file="Fig2_Upset_discrete_induction_2024_04_25.pdf", width = 6, height = 6) # or other device
Discrete_induction
dev.off()

## discrete repression
discrete_repression <- classes %>% filter(September=="X9" | December=="X9" | March=="X9" | June=="X9" ) 
count(discrete_repression)


discrete_repression <- discrete_repression %>% mutate(September=case_when(September == "X9" ~ 1,TRUE ~ 0)) %>%
  mutate(December=case_when(December == "X9" ~ 1, TRUE ~ 0)) %>%
  mutate(March=case_when(March == "X9" ~ 1,TRUE ~ 0 )) %>%
  mutate(June=case_when(June == "X9" ~ 1, TRUE ~ 0 ))

discrete_repression <- as.data.frame(discrete_repression)
discrete_repression
count(discrete_repression)
Discrete_repression <- upset(discrete_repression,nsets=4, number.angles = 0, point.size = 3.5, line.size = 2, 
      mainbar.y.label = "Seasonal intersections", sets.x.label = "Discrete Repression", 
      text.scale = c(1.3, 1.3, 1, 1, 2, 0.75), order.by = "freq",
      sets.bar.color=c( "#FFD535","#860124", "#FB2372","#76AFB6"))

pdf(file="Fig2_Upset_discrete_repression_2024_04_25.pdf", width = 6, height = 6) # or other device
Discrete_repression
dev.off()


## disrete induction

linear_induction <- classes %>% filter(September=="X5" | December=="X5" | March=="X5" | June=="X5" ) 
count(linear_induction)


linear_induction <- linear_induction %>% mutate(September=case_when(September == "X5" ~ 1,TRUE ~ 0)) %>%
  mutate(December=case_when(December == "X5" ~ 1, TRUE ~ 0)) %>%
  mutate(March=case_when(March == "X5" ~ 1,TRUE ~ 0 )) %>%
  mutate(June=case_when(June == "X5" ~ 1, TRUE ~ 0 ))

linear_induction <- as.data.frame(linear_induction)
count(linear_induction)
linear_induction <- upset(linear_induction, nsets = 4, number.angles = 0, point.size = 3.5, line.size = 2, 
                            mainbar.y.label = "Seasonal intersections", sets.x.label = "Linear induction", 
                            text.scale = c(1.3, 1.3, 1, 1, 2, 0.75), order.by = "freq",
                            sets.bar.color=c( "#860124","#FFD535", "#76AFB6","#FB2372"))


pdf(file="Fig2_Upset_linear_induction_2024_06_06.pdf", width = 6, height = 6) # or other device
linear_induction
dev.off()


