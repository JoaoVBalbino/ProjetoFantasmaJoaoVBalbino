#1) Número de lançamentos a cada década por formato de lançamento;
#2) Variação da nota IMDB por temporada dos episódios;
#3) Top 3 terrenos mais frequentes pela ativação da armadilha;
#4) Relação entre as notas IMDB e engajamento;
#5) Variação da nota de engajamento pelo personagem que conseguiu capturar o monstro.
print_quadro_resumo <- function(data, var_name, title="Medidas resumo da(o) [nome da variável]", label="quad:quadro_resumo1")
{
  var_name <- substitute(var_name)
  data <- data %>%
    summarize(`Média` = round(mean(!!sym(var_name)),2),
              `Desvio Padrão` = round(sd(!!sym(var_name)),2),
              `Variância` = round(var(!!sym(var_name)),2),
              `Mínimo` = round(min(!!sym(var_name)),2),
              `1º Quartil` = round(quantile(!!sym(var_name), probs = .25),2),
              `Mediana` = round(quantile(!!sym(var_name), probs = .5),2),
              `3º Quartil` = round(quantile(!!sym(var_name), probs = .75),2),
              `Máximo` = round(max(!!sym(var_name)),2)) %>%
    t() %>% 
    as.data.frame() %>%
    rownames_to_column()
  
  latex <- str_c("\\begin{quadro}[H]
\t\\caption{", title, "}
\t\\centering
\t\\begin{adjustbox}{max width=\\textwidth}
\t\\begin{tabular}{", sep="")
  
  col_count <- ncol(data)
  row_count <- nrow(data)
  latex <- str_c(latex, "| l |\n", sep=" ")
  for (i in seq(2, col_count))
  {
    numCount <- data[i, -c(1)] %>%
      as.numeric() %>%
      {floor(log10(.)) + 1} %>%
      max()
    latex <- str_c(latex, "\t\t\tS[table-format = ", numCount ,".2]\n", sep="")
  }
  
  
  latex <- str_c(latex, "\t\t\t|}\n\t\\toprule\n\t\t", sep="")
  if (col_count > 2)
  {
    for (i in seq(1,col_count))
    {
      if (i == 1)
        latex <- str_c(latex, "\\textbf{Estatística}", sep="")
      else
        latex <- str_c(latex, " \\textbf{", data[1, i], "}", sep="")
      
      if (i < col_count)
        latex <- str_c(latex, "&", sep=" ")
      else
        latex <- str_c(latex, "\\\\\n", sep=" ")
    }
  }
  else
  {
    latex <- str_c(latex, "\\textbf{Estatística} & \\textbf{Valor} \\\\\n", sep="")  
  }
  
  latex <- str_c(latex, "\t\t\\midrule\n", sep="")
  
  if (col_count > 2)
    starting_number <- 2
  else
    starting_number <- 1
  
  for (i in seq(starting_number, row_count))
  {
    latex <- str_c(latex, "\t\t", str_flatten(t(data[i,]), collapse = " & "), " \\\\\n")
  }
  latex <- str_c(latex, "\t\\bottomrule
\t\\end{tabular}
\t\\label{", label, "}
\t\\end{adjustbox}
\\end{quadro}", sep="")
  
  writeLines(latex)
}
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
library(caret)

df = read.csv("banco_final.csv", header = T, sep = ",", dec =".")

df <- df %>%  rename(data_lançamento = date_aired)
df <- df %>%  rename(engajamento = engagement)

df <- df %>%  rename(formato = format)
df <- df %>%  rename(armadilha_funcionou_primeiro = trap_work_first)
df <- df %>%  rename(nome_serie = series_name)
df <- df %>%  rename(titulo = title)
df <- df %>%  rename(temporada = season)
df <- df %>%  rename(tipo_terreno = setting_terrain)
df <- df %>%  rename(frede_capiturou = caught_fred)
df <- df %>%  rename(dafine_capiturou = caught_daphnie)
df <- df %>%  rename(velma_capiturou = caught_velma)
df <- df %>%  rename(salsicha_capiturou = caught_shaggy)
df <- df %>%  rename(scooby_capiturou = caught_scooby)
df <- df %>%  rename(outros_capiturou = caught_other)
df <- df %>%  rename(ninguem_capiturou= caught_not)

df$data_lançamento <- year(df$data_lançamento)

df <- df %>%
  mutate(data_lançamento = case_when(
    data_lançamento <= 1969 ~ "1960",
    data_lançamento >= 1970 & data_lançamento <= 1979 ~ "1970",
    data_lançamento >= 1980 & data_lançamento <= 1989 ~ "1980",
    data_lançamento >= 1990 & data_lançamento <= 1999 ~ "1990",
    data_lançamento >= 2000 & data_lançamento <= 2009 ~ "2000",
    data_lançamento >= 2010 & data_lançamento <= 2019 ~ "2010",
    data_lançamento >= 2020 ~ "2020"))



df <- df %>% 
  mutate(armadilha_funcionou_primeiro = case_when(
    armadilha_funcionou_primeiro == "True" ~ "True",
    armadilha_funcionou_primeiro == "False" ~ "False"
    ))
#● Número de lançamentos a cada década por formato de lançamento:
#O cliente gostaria de entender a quantidade dos diferentes tipos de lançamentos 
#(CrossOver, Filmes e Séries) que foram lançados durante as décadas.

## 1




df_lanpordec <- df %>% filter(!is.na(data_lançamento)) %>% 
  select(data_lançamento,formato) %>% 
  group_by(data_lançamento,formato) %>% 
  summarise(contagem = n())

df_lanpordec <- df_lanpordec %>% 
  mutate(formato = case_when(
    formato == "Movie" ~ "Filme",
    formato == "Serie" ~ "Série",
    formato == "CrossOver" ~"CrossOver"
    
  ))


ggplot(df_lanpordec) +
  aes(x = data_lançamento, y = contagem, group = formato, colour = formato) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "Década", y = "Contagem") +
  theme_estat()
ggsave("graf_linha_bi.pdf", width = 158, height = 93, units = "mm")


serie <- df_lanpordec %>% filter(formato == "Série")
mean(serie$contagem)
var(serie$contagem)
sd(serie$contagem)

filme <- df_lanpordec %>% filter(formato == "Filme")
mean(filme$contagem)
var(filme$contagem)
sd(filme$contagem)

crossover <- df_lanpordec %>% filter(formato == "CrossOver")
mean(crossover$contagem)
var(crossover$contagem)
sd(crossover$contagem)

#● Variação da nota IMDB por temporada dos episódios:
#  O cliente quer comparar as notas IMDB por temporada dos episódios, para entender a variação
#das notas das séries entre as temporadas (1,2,3 e 4) . Se em alguma temporada geralmente a
#variação das notas é menor, maior ou se não muda.


##2
dfimdbptemp <- df %>% select(nome_serie,titulo,formato,temporada,imdb) %>% 
  group_by(temporada) %>% 
  filter(formato == "Serie")%>%
  filter(temporada %in% c(1,2,3,4)) #%>% 
  #summarise(media = mean(imdb, na.rm = T),
  #          variancia = var(imdb, na.rm = T), 
  #          desvio = sd(imdb, na.rm =T))


ggplot(dfimdbptemp) +
  aes(
    x = reorder(temporada, imdb, FUN = mean), 
    y = imdb
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Temporada", y = "Imdb") +
  theme_estat()

ggsave("box_bi_temporada.pdf", width = 158, height = 93, units = "mm")


dfimdbptemp %>% 
  group_by(temporada) %>% # caso mais de uma categoria
  print_quadro_resumo(var_name = imdb)

##3
# Conta as armadilhas por terreno que ativaram de primeira ou não.
df_armardilha <- df %>% select(tipo_terreno, armadilha_funcionou_primeiro) %>%
  na.omit(armadilha_funcionou_primeiro) %>% 
  group_by(tipo_terreno, armadilha_funcionou_primeiro) %>% 
  summarise(contagem_terr = n(), contagem_capitura = n()) %>% 
  arrange(desc(contagem_terr)) %>% 
  head(, n = 6L)

#Mostra as armadilhas que ativaram de priemira ou não sem realizar a contagem do sucesso ou fracasso. 
df_armardilha2 <- df %>% select(tipo_terreno, armadilha_funcionou_primeiro) %>%
  na.omit(armadilha_funcionou_primeiro) %>% 
  group_by(tipo_terreno, armadilha_funcionou_primeiro) %>% 
  summarise(contagem_terr = n()) %>% 
  arrange(desc(contagem_terr)) %>% 
  head(, n = 6L)

#Mostra apenas os terrenos mais utilizados sem realizar contagem 
df_armardilha3 <- df %>% select(tipo_terreno, armadilha_funcionou_primeiro) %>%
  na.omit(armadilha_funcionou_primeiro) %>% 
  group_by(tipo_terreno, armadilha_funcionou_primeiro) %>% 
  filter(tipo_terreno %in% c("Urban", "Rural", "Forest"))
#Calculando a carrelação entre duas variaveis qualitativas.
resultado <- chisq.test(table(df_armardilha3$tipo_terreno, df_armardilha3$armadilha_funcionou_primeiro))
print(resultado)

df_armardilha3 <- df_armardilha3 %>% 
  mutate(armadilha_funcionou_primeiro = case_when(
    armadilha_funcionou_primeiro == "True" ~"Verdadeiro",
    armadilha_funcionou_primeiro == "False" ~"Falso"
  ))


df_armardilha3 <- df_armardilha3 %>% 
  mutate(tipo_terreno = case_when(
    tipo_terreno == "Urban" ~"Urbano",
    tipo_terreno == "Rural" ~"Rural",
    tipo_terreno == "Forest" ~"Floresta"
    
  ))



trans_drv <- df_armardilha3 %>%
  group_by(tipo_terreno, armadilha_funcionou_primeiro) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = round(freq / sum(freq) * 100, 1)
  )

porcentagens <- str_c(trans_drv$freq_relativa, "%")
legendas <- str_squish(str_c(trans_drv$freq, " (", porcentagens, ")"))

ggplot(trans_drv) +
  aes(
    x = fct_reorder(tipo_terreno, freq, .desc = TRUE), 
    y = freq,
    fill = armadilha_funcionou_primeiro, 
    label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = 0.9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  labs(x = "Tipo de Terreno", y = "Frequência") +
  theme_estat()
ggsave("colunas-bi-freq1.pdf", width = 158, height = 93, units = "mm")




##4 Relação entre as notas IMDB e engajamento;
#O intuito é entender a relação entre as notas do IMDB e o engajamento. A medida que as notas
#aumentam o engajamento também é alto? Ou o contrário, ou não tem diferença, ou não dá pra
#inferir nada?
#separando as colunas desejadas e ordenando do menor ao meior imdb
dfimdbeengag <- df %>% select(imdb, engajamento) %>% 
  arrange((imdb))
#grafico de dispersão para analisar duas variáveis quantitativas 
ggplot(dfimdbeengag) +
  aes(x = engajamento, y = imdb) +
  geom_point(colour = "#A11D21", size = 3, alpha = 0.4) +  
  labs(
    x = "Engajamento", 
    y = "IMDB",
  ) +
  theme_estat() 
ggsave("graf_disp_pf_4.pdf", width = 158, height = 93, units = "mm")
#calculando o coeficiente de pearson, para duas variaveis quantitativas
correlation <- cor(dfimdbeengag$imdb, dfimdbeengag$engajamento)
print(correlation)

?geom_point()


dfimdbeengag %>% 
  print_quadro_resumo(var_name = "engajamento")

## 5
  
df_engag_foi_fred <- df %>% 
  select(frede_capiturou, engajamento) %>% 
  mutate(frede_capiturou = case_when(
    frede_capiturou == "True" ~ "Frede",
    frede_capiturou == "False" ~NA,
    frede_capiturou == "" ~ NA)) %>% 
  na.omit(frede_capiturou)

df_engag_foi_daphnie <- df %>% 
  select(dafine_capiturou, engajamento) %>% 
  mutate(dafine_capiturou = case_when(
    dafine_capiturou == "True" ~ "Dafine",
    dafine_capiturou == "False" ~NA,
    dafine_capiturou == "" ~ NA)) %>% 
  na.omit(dafine_capiturou)

df_engag_foi_velma <- df %>% 
  select(velma_capiturou, engajamento) %>% 
  mutate(velma_capiturou = case_when(
    velma_capiturou == "True" ~ "Velma",
    velma_capiturou == "False" ~NA,
    velma_capiturou == "" ~ NA)) %>% 
  na.omit(velma_capiturou)

df_engag_foi_shaggy <- df %>% 
  select(salsicha_capiturou, engajamento) %>% 
  mutate(salsicha_capiturou = case_when(
    salsicha_capiturou == "True" ~ "Salsicha",
    salsicha_capiturou == "False" ~NA,
    salsicha_capiturou == "" ~ NA)) %>% 
  na.omit(salsicha_capiturou)

df_engag_foi_scooby <- df %>% 
  select(scooby_capiturou, engajamento) %>% 
  mutate(scooby_capiturou = case_when(
    scooby_capiturou == "True" ~ "Scooby",
    scooby_capiturou == "False" ~NA,
    scooby_capiturou == "" ~ NA)) %>% 
  na.omit(scooby_capiturou)

df_engag_foi_ninguem <- df %>% 
  select(ninguem_capiturou, engajamento) %>% 
  mutate(ninguem_capiturou = case_when(
    ninguem_capiturou == "True" ~ "Ninguém",
    ninguem_capiturou == "False" ~NA,
    ninguem_capiturou == "" ~ NA)) %>% 
  na.omit(ninguem_capiturou)


df_engag_foi_other <- df %>% 
  select(outros_capiturou, engajamento) %>% 
  mutate(outros_capiturou = case_when(
    outros_capiturou == "True" ~ "Outros",
    outros_capiturou == "False" ~NA,
    outros_capiturou == "" ~ NA)) %>% 
  na.omit(outros_capiturou)

df_engag_foi_ninguem <- df_engag_foi_ninguem %>%  rename(capiturou =ninguem_capiturou)
df_engag_foi_fred <- df_engag_foi_fred %>%  rename(capiturou = frede_capiturou)
df_engag_foi_daphnie <- df_engag_foi_daphnie %>%  rename(capiturou = dafine_capiturou)
df_engag_foi_velma <- df_engag_foi_velma %>%  rename(capiturou = velma_capiturou)
df_engag_foi_shaggy <- df_engag_foi_shaggy %>%  rename(capiturou = salsicha_capiturou)
df_engag_foi_scooby <- df_engag_foi_scooby %>%  rename(capiturou = scooby_capiturou)
df_engag_foi_other <- df_engag_foi_other %>%  rename(capiturou = outros_capiturou)

resultado1 <- merge(df_engag_foi_fred, df_engag_foi_daphnie, by = c("capiturou","engajamento"), all = T)
resultado2 <- merge(df_engag_foi_velma, df_engag_foi_scooby, by = c("capiturou","engajamento"), all = T)
resultado3 <- merge(df_engag_foi_shaggy,df_engag_foi_other, by = c("capiturou","engajamento"), all = T)
resultado4 <- merge(resultado1,resultado2, by = c("capiturou","engajamento"), all = T)
resultado5 <- merge(resultado4,resultado3, by = c("capiturou","engajamento"), all = T)
resultado6 <- merge(resultado5,df_engag_foi_ninguem, by = c("capiturou","engajamento"), all = T)

ggplot(resultado6) +
  aes(
    x = reorder(capiturou, engajamento, FUN = mean), 
    y = engajamento
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Personagem que Capiturou o Monstro", y = "Engajamento") +
  theme_estat()

ggsave("box_bi_ana5.pdf", width = 158, height = 93, units = "mm")

resultado6 %>% 
  group_by(capiturou) %>% # caso mais de uma categoria
  print_quadro_resumo(var_name = engajamento)
