#Carregar os pacotes necessários para a realização dos exercícios
install.packages("tidyverse")
install.packages("gt")

library(tidyverse)
library(gt)

#Carregar os arquivos spot1.csv e spot2.csv
setwd("C:/Users/Larissa/OneDrive/Área de Trabalho/trabalho R")

spot1 <- read.csv("spot1.csv")
spot2 <- read.csv("spot2.csv")

#Gerar as estatísticas resumo dos arquivos spot1 e spot2
#estatísticas resumo de spot1
summary(spot1)

#estatísticas resumo de spot2
summary(spot2)

#carregando pacote dplyr
library(dplyr)

#Filtrar apenas as músicas a partir do ano 1950
spot1_filtered <- filter(spot1, year >= 1950)

#Criar uma variável nova classificando a data de lançamento das músicas a cada 20 anos
spot1$year2 <- ifelse(spot1$year >= 1950 & spot1$year <= 1969, "1950-1969",
                      ifelse(spot1$year >= 1970 & spot1$year <= 1989, "1970-1989",
                             ifelse(spot1$year >= 1990 & spot1$year <= 2009, "1990-2009",
                                    ifelse(spot1$year >= 2010 & spot1$year <= 2020, "2010-2020", NA))))


head(spot1)

#Juntar as duas tabelas spot1 e spot2
spotj <- left_join(spot1, spot2, by = "id")

head(spotj)

#Criar uma nova tabela selecionando apenas as músicas explícitas
spotexp <- filter(spotj, explicit == TRUE)

head(spotexp)

#Criar uma nova tabela adicionando apenas os gêneros musicais: Alternative, Country, Dance, Folk, Pop, Rock
spotj <- filter(spotj, genre %in% c("Alternative", "Country", "Dance", "Folk", "Pop", "Rock"))

head(spotj)

#Recodificar a categoria Dance, da variável genre, para Pop
#carregando o pacote forcats
library(forcats)

#recodificando a categoria
spotj$genre <- fct_recode(spotj$genre, "Pop" = "Dance")

head(spotj)

#Criar uma tabela de frequência por gênero musical
freq_by_genre <- spotj %>%
  group_by(genre) %>%
  summarize(count = n())

print(freq_by_genre)

#Adicionar à tabela acima a média de danceability por gênero musical
freq_by_genre <- spotj %>%
  group_by(genre) %>%
  summarize(count = n(), mean_danceability = mean(danceability, na.rm = TRUE))

print(freq_by_genre)

#Adicionar à tabela acima a proporção por gênero musical
total_count <- sum(freq_by_genre$count)
freq_by_genre <- freq_by_genre %>%
  mutate(proportion = count / total_count)

print(freq_by_genre)

#Criar uma tabela cruzada de gênero x década de lançamento
#carregando o pacote janitor
install.packages("janitor")
library(janitor)

#tabela cruzada de gênero x década de lançamento
cross_table <- tabyl(spotj, genre, year2)

print(cross_table)

#Utilizar o pacote gt para apresentar as duas tabelas acima
#tabela de frequência por gênero musical
gt_freq_by_genre <- freq_by_genre %>%
  gt() %>%
  tab_header(
    title = "Frequência e Média de Danceability por Gênero Musical",
    subtitle = "Fonte: Dados do Spotify"
  ) %>%
  cols_label(
    genre = "Gênero",
    count = "Frequência",
    mean_danceability = "Média de Danceability",
    proportion = "Proporção"
  )

print(gt_freq_by_genre)

#tabela cruzada de gênero x década de lançamento
gt_cross_table <- cross_table %>%
  gt() %>%
  tab_header(
    title = "Tabela Cruzada de Gênero x Década de Lançamento",
    subtitle = "Fonte: Dados do Spotify"
  ) %>%
  cols_label(
    genre = "Gênero",
    `1950-1969` = "1950-1969",
    `1970-1989` = "1970-1989",
    `1990-2009` = "1990-2009",
    `2010-2020` = "2010-2020"
  )

print(gt_cross_table)

#Criar um gráfico de pontos da “dançabilidade” x popularidade das músicas do gênero Rock.
#carregando o pacote ggplot2
library(ggplot2)

#filtrando o gênero rock
rock_songs <- filter(spotj, genre == "Rock")

#criando o gráfico de pontos
ggplot(rock_songs, aes(x = danceability, y = popularity)) +
  geom_point() +
  labs(
    title = "Dançabilidade vs. Popularidade das Músicas do Gênero Rock",
    x = "Dançabilidade",
    y = "Popularidade"
  ) +
  theme_light()

#Criar um gráfico de barras simples mostrando a média de popularidade por gênero
ggplot(spotj, aes(x = genre, y = popularity)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(
    title = "Média de Popularidade por Gênero Musical",
    x = "Gênero",
    y = "Média de Popularidade"
  ) +
  theme_light()

#Criar um gráfico de barras empilhado cruzando a proporção de músicas por gênero e o período de lançamento da música
ggplot(spotj, aes(x = year2, fill = genre)) +
  geom_bar(position = "fill") +
  labs(
    title = "Proporção de Músicas por Gênero e Período de Lançamento",
    x = "Década",
    y = "Proporção",
    fill = "Gênero"
  ) +
  theme_classic()
