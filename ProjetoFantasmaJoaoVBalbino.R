#1) Número de lançamentos a cada década por formato de lançamento;
#2) Variação da nota IMDB por temporada dos episódios;
#3) Top 3 terrenos mais frequentes pela ativação da armadilha;
#4) Relação entre as notas IMDB e engajamento;
#5) Variação da nota de engajamento pelo personagem que conseguiu capturar o monstro.

library(hms)
library(lubridate)
library(tidyverse)
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
## 1
dflanpordec <- df %>% filter(!is.na(date_aired)) %>% 
  select(date_aired,format) %>% 
  group_by(date_aired,format) %>% 
  summarise(contagem = n()) 

##2
dfimdbptemp <- df %>% select(series_name,title,season,imdb) %>% 
  group_by(series_name,season) %>%
  summarise(media = mean(imdb, na.rm = T),
            variancia = var(imdb, na.rm = T), 
            desvio = sd(imdb, na.rm =T))



##3
dfarm <- df %>% select(setting_terrain) %>% 
  group_by(setting_terrain) %>% 
  summarise(contagem = n()) %>% 
  arrange(desc(contagem)) %>% 
  head(, n = 3L)

##4 Relação entre as notas IMDB e engajamento;
dfimdbeengag <- df %>% select(series_name,title,engagement,imdb) %>% 
  group_by(series_name,season) %>%
  summarise(media = mean(imdb, na.rm = T),
            variancia = var(imdb, na.rm = T), 
            desvio = sd(imdb, na.rm =T))




