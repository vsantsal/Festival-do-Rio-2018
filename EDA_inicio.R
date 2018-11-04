## Análise Exploratória do Festival
#load("festival_do_rio.RData")
library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)

#Carregando o dataset
load("festival_tidy.RData")


#### Países ####
paises <- festival_do_rio$paises
paises <- paises[!is.na(paises)]

#Separando os países de cada filme
str_split(paises, "/") %>%
  unlist %>% str_trim  -> paises

#Transformando em um dataset com frequencias
as_tibble(paises) %>%
  rename(pais = value) %>%
  count(pais, sort = T) %>%
  mutate(pais = reorder(pais,n)) -> paises
  

# Fazendo o gráfico
g <- ggplot(data = paises, aes(x = pais, y = n)) +
  geom_col() +
  coord_flip() +
  xlab(NULL) + 
  ylab("Países") +
  ggtitle("Frequências dos Países") 
ggplotly(g)

#### Duração ####
tempos <- festival_do_rio %>% 
  select(duracao, genero) %>% 
  dplyr::filter(!is.na(duracao)) #achei que poderi confundir com o filter do plotly 

#Histograma da duração
ggplot(data = tempos) +
  aes(x = duracao, y=..density..) +
  geom_histogram(bins = 30, fill = "#0c4c8a") +
  labs(title = "Histograma das Durações dos Filmes",
       x = "Duração", y = NULL) +
  theme_minimal() -> g
ggplotly(g)

#### Mostras com mais filmes ####
mostras <- festival_do_rio %>%
  select(mostra,titulo) %>%
  filter(!is.na(mostra)) %>% 
  group_by(mostra) %>%
  summarise(n = length(unique(titulo))) %>%
  mutate(mostra = reorder(mostra, n))

g <- ggplot(data = mostras)+
  aes(x = mostra, y = n) +
  geom_col() +
  ylab("Número de Filmes") +
  coord_flip() +
  ggtitle("Número de filmes por mostra") +
  theme_minimal()
ggplotly(g)
  








