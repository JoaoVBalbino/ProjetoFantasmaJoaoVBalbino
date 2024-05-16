#1) Número de lançamentos a cada década por formato de lançamento;
#2) Variação da nota IMDB por temporada dos episódios;
#3) Top 3 terrenos mais frequentes pela ativação da armadilha;
#4) Relação entre as notas IMDB e engajamento;
#5) Variação da nota de engajamento pelo personagem que conseguiu capturar o monstro.

cores_estat <- c("#A11D21", "#003366", "#CC9900", "#663333", "#FF6600", "#CC9966", "#999966", "#006606", "#008091", "#041835", "#666666")
theme_estat <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 12),
      axis.title.x = ggplot2::element_text(colour = "black", size = 12),
      axis.text = ggplot2::element_text(colour = "black", size = 9.5),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "top",
      ...
    )
  return(
    list(
      theme,
      scale_fill_manual(values = cores_estat),
      scale_colour_manual(values = cores_estat)
    )
  )
}
getwd()
setwd("/home/marcus/Downloads")
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

df <- df %>% 
  mutate(trap_work_first = case_when(
    trap_work_first == "True" ~ "True",
    trap_work_first == "False" ~ "False"
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
# Conta as armadilhas por terreno que ativaram de primeira ou não.
df_armardilha <- df %>% select(setting_terrain, trap_work_first) %>%
  na.omit(trap_work_first) %>% 
  group_by(setting_terrain,trap_work_first) %>% 
  summarise(contagem_terr = n(), contagem_capitura = n()) %>% 
  arrange(desc(contagem_terr)) %>% 
  head(, n = 6L)
#Mostra as armadilhas que ativaram de priemira ou não sem realizar a contagem do sucesso ou fracasso. 
df_armardilha2 <- df %>% select(setting_terrain, trap_work_first) %>%
  na.omit(trap_work_first) %>% 
  group_by(setting_terrain,trap_work_first) %>% 
  summarise(contagem_terr = n()) %>% 
  arrange(desc(contagem_terr)) %>% 
  head(, n = 6L)

#Mostra apenas os terrenos mais utilizados sem realizar contagem 
df_armardilha3 <- df %>% select(setting_terrain, trap_work_first) %>%
  na.omit(trap_work_first) %>% 
  group_by(setting_terrain,trap_work_first) %>% 
  filter(setting_terrain %in% c("Urban", "Rural", "Forest"))
#Calculando a carrelação entre duas variaveis qualitativas.
resultado <- chisq.test(table(df_armardilha3$setting_terrain, df_armardilha3$trap_work_first))
print(resultado)


trans_drv <- df_armardilha3 %>%
  group_by(setting_terrain, trap_work_first) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = round(freq / sum(freq) * 100, 1)
  )

porcentagens <- str_c(trans_drv$freq_relativa, "%")
legendas <- str_squish(str_c(trans_drv$freq, " (", porcentagens, ")"))

ggplot(trans_drv) +
  aes(
    x = fct_reorder(setting_terrain, freq, .desc = TRUE), 
    y = freq,
    fill = trap_work_first, 
    label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = 0.9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  labs(x = "Setting Terrain", y = "Frequência") +
  theme_estat()
ggsave("colunas-bi-freq.pdf", width = 158, height = 93, units = "mm")




##4 Relação entre as notas IMDB e engajamento;
#O intuito é entender a relação entre as notas do IMDB e o engajamento. A medida que as notas
#aumentam o engajamento também é alto? Ou o contrário, ou não tem diferença, ou não dá pra
#inferir nada?
#separando as colunas desejadas e ordenando do menor ao meior imdb
dfimdbeengag <- df %>% select(imdb,engagement) %>% 
  arrange((imdb))
#grafico de dispersão para analisar duas variáveis quantitativas 
ggplot(dfimdbeengag) +
  aes(x = engagement, y = imdb) +
  geom_point(colour = "#A11D21", size = 3) +  
  labs(
    x = "Engagement", 
    y = "IMDB",
  ) +
  theme_estat() 
ggsave("graf_disp_pf_4.pdf", width = 158, height = 93, units = "mm")
#calculando o coeficiente de pearson, para duas variaveis quantitativas
correlation <- cor(dfimdbeengag$imdb, dfimdbeengag$engagement)
print(correlation)
  

