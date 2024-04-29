install.packages("tidyverse")
install.packages("lubridate")
library(hms)
library(lubridate)
library(tidyverse)
setwd("/home/marcus/Downloads")
df = read.csv("banco_final.csv", header = T, sep = ",", dec =".")

df <- df %>%
  mutate(date_aired = case_when(
    date_aired <= 1970 ~ 1960,
    date_aired >= 1971 & date_aired <= 1980 ~ 1970,
    date_aired >= 1981 & date_aired <= 1990 ~ 1980,
    date_aired >= 1991 & date_aired <= 2000 ~ 1990,
    date_aired >= 2001 & date_aired <= 2010 ~ 2000,
    date_aired >= 2011 ~ 2010
  ))
df <- na.omit(df)

df <- df %>% select(date_aired,format)
df %>% group_by(date_aired,format) %>% 
  summarise(contagem = n())
  