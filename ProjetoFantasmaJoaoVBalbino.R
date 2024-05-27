#1) Número de lançamentos a cada década por formato de lançamento;
#2) Variação da nota IMDB por temporada dos episódios;
#3) Top 3 terrenos mais frequentes pela ativação da armadilha;
#4) Relação entre as notas IMDB e engajamento;
#5) Variação da nota de engajamento pelo personagem que conseguiu capturar o monstro.
library(tidyverse)


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


# Supondo que o dataframe `df_lampordec` já foi carregado e processado
trans_drv <- df_lampordec %>%
  group_by(format, date_aired) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = round(freq / sum(freq) * 100, 1)
  )

# Preparação das legendas
porcentagens <- str_c(trans_drv$freq_relativa, "%") %>% str_replace("\\.", ",")
legendas <- str_squish(str_c(trans_drv$freq, " (", porcentagens, ")"))

# Criação do gráfico
ggplot(trans_drv) +
  aes(
    x = fct_reorder(format, freq, .desc = TRUE), y = freq,
    fill = date_aired, label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = 0.9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  labs(x = "Format", y = "Frequency") +
  theme_estat()

# Salvando o gráfico
ggsave("colunas-bi-freq.pdf", width = 158, height = 93, units = "mm")

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
    x = "Engajamento", 
    y = "IMDB",
  ) +
  theme_estat() 
ggsave("graf_disp_pf_4.pdf", width = 158, height = 93, units = "mm")
#calculando o coeficiente de pearson, para duas variaveis quantitativas
correlation <- cor(dfimdbeengag$imdb, dfimdbeengag$engagement)
print(correlation)


## 5
  
df_engag_foi_fred <- df %>% 
  select(caught_fred,engagement) %>% 
  mutate(caught_fred = case_when(
    caught_fred == "True" ~ "Fred",
    caught_fred == "False" ~NA,
    caught_fred == "" ~ NA)) %>% 
  na.omit(caught_fred)
medidas_resumo_foi_fred <- df_engag_foi_fred %>% 
  summarise(mean(engagement),sd(engagement),var(engagement),min(engagement),quantile(engagement, 0.25),median(engagement),quantile(engagement,0.75),
            max(engagement))


df_engag_foi_daphnie <- df %>% 
  select(caught_daphnie,engagement) %>% 
  mutate(caught_daphnie = case_when(
    caught_daphnie == "True" ~ "Daphnie",
    caught_daphnie == "False" ~NA,
    caught_daphnie == "" ~ NA)) %>% 
  na.omit(caught_daphnie)
medidas_resumo_foi_daphnie <- df_engag_foi_daphnie %>% 
  summarise(mean(engagement),sd(engagement),var(engagement),min(engagement),quantile(engagement, 0.25),median(engagement),quantile(engagement,0.75),
            max(engagement))


df_engag_foi_velma <- df %>% 
  select(caught_velma,engagement) %>% 
  mutate(caught_velma = case_when(
    caught_velma == "True" ~ "Velma",
    caught_velma == "False" ~NA,
    caught_velma == "" ~ NA)) %>% 
  na.omit(caught_velma)
medidas_resumo_foi_velma <- df_engag_foi_velma %>% 
  summarise(mean(engagement),sd(engagement),var(engagement),min(engagement),quantile(engagement, 0.25),median(engagement),quantile(engagement,0.75),
            max(engagement))


df_engag_foi_shaggy <- df %>% 
  select(caught_shaggy,engagement) %>% 
  mutate(caught_shaggy = case_when(
    caught_shaggy == "True" ~ "Shaggy",
    caught_shaggy == "False" ~NA,
    caught_shaggy == "" ~ NA)) %>% 
  na.omit(caught_shaggy)
medidas_resumo_foi_shaggy <- df_engag_foi_shaggy %>% 
  summarise(mean(engagement),sd(engagement),var(engagement),min(engagement),quantile(engagement, 0.25),median(engagement),quantile(engagement,0.75),
            max(engagement))


df_engag_foi_scooby <- df %>% 
  select(caught_scooby,engagement) %>% 
  mutate(caught_scooby = case_when(
    caught_scooby == "True" ~ "Scooby",
    caught_scooby == "False" ~NA,
    caught_scooby == "" ~ NA)) %>% 
  na.omit(caught_scooby)
medidas_resumo_foi_scoob <- df_engag_foi_scooby %>% 
  summarise(mean(engagement),sd(engagement),var(engagement),min(engagement),quantile(engagement, 0.25),median(engagement),quantile(engagement,0.75),
            max(engagement))


df_engag_foi_other <- df %>% 
  select(caught_other,engagement) %>% 
  mutate(caught_other = case_when(
    caught_other == "True" ~ "Other",
    caught_other == "False" ~NA,
    caught_other == "" ~ NA)) %>% 
  na.omit(caught_other)
medidas_resumo_foi_other <- df_engag_foi_other %>% 
  summarise(mean(engagement),sd(engagement),var(engagement),min(engagement),quantile(engagement, 0.25),median(engagement),quantile(engagement,0.75),
            max(engagement))



df_engag_foi_fred <- df_engag_foi_fred %>%  rename(caught = caught_fred)
df_engag_foi_daphnie <- df_engag_foi_daphnie %>%  rename(caught = caught_daphnie)
df_engag_foi_velma <- df_engag_foi_velma %>%  rename(caught = caught_velma)
df_engag_foi_shaggy <- df_engag_foi_shaggy %>%  rename(caught = caught_shaggy)
df_engag_foi_scooby <- df_engag_foi_scooby %>%  rename(caught = caught_scooby)
df_engag_foi_other <- df_engag_foi_other %>%  rename(caught = caught_other)

X <- c(df_engag_foi_fred, df_engag_foi_daphnie,df_engag_foi_velma, df_engag_foi_scooby, 
  df_engag_foi_shaggy,df_engag_foi_other)

resultado1 <- merge(df_engag_foi_fred, df_engag_foi_daphnie, by = c("caught","engagement"), all = T)
resultado2 <- merge(df_engag_foi_velma, df_engag_foi_scooby, by = c("caught","engagement"), all = T)
resultado3 <- merge(df_engag_foi_shaggy,df_engag_foi_other, by = c("caught","engagement"), all = T)
resultado4 <- merge(resultado1,resultado2, by = c("caught","engagement"), all = T)
resultado5 <- merge(resultado4,resultado3, by = c("caught","engagement"), all = T)

ggplot(resultado5) +
  aes(
    x = reorder(caught, engagement, FUN = median), 
    y = engagement
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Personagem que Capiturou o Monstro", y = "Engajamento") +
  theme_estat()

ggsave("box_bi_ana5.pdf", width = 158, height = 93, units = "mm")


P_Capiturou = c("Fred" , "Dafine" , "Velma", "Salsicha", "Scooby" , "Outro")
Engajamento = c(177.22,179.39,181.11,177.22,177.71,175.62)
simbora = cbind(P_Capiturou, Engajamento)  
simbora <- as.data.frame(simbora) 


medidas_resumo_foi_fred
medidas_resumo_foi_daphnie
medidas_resumo_foi_velma
medidas_resumo_foi_shaggy
medidas_resumo_foi_scoob
medidas_resumo_foi_other