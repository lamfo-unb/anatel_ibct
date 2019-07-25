library(tidyverse)
library(readr)
library(lavaan)


df <- read_csv("dados/qualidade_perc.csv")

df %>% 
  mutate(A1_1 = ifelse(is.na(A1_1), 0, 1),
         A1_2 = ifelse(is.na(A1_2), 0, 1),
         A1_3 = ifelse(is.na(A1_3), 0, 1),
         A1_4 = ifelse(is.na(A1_4), 0, 1))

## 

