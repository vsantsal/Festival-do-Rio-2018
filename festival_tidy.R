library(dplyr)
library(tidyr)
library(lubridate)
library(chron)
#####ajustes adicionais
###tidyando os dados


load("festival_do_rio.RData")

###retornando cinemas para obter informações de bairro

#obtendo cinemas distintos de nossa base
festival_do_rio %>% 
    select(local) %>% 
    unique -> cinemas

#vetor com os bairros correspondentes
bairros <- c("Centro", "Botafogo", "Ipanema",
             "Niterói", "Gávea", "Botafogo",
             "Gávea", "Botafogo", "Gávea",
             "Botafogo", "Gávea", "Catete",
             "Ipanema", "Catete", "Gávea",
             "Gávea", "Flamengo", "Centro",
             "Guadalupe", "Niterói", "Copacabana")

#armazenando em único data frame as informações acima
cinemas <- data.frame(cinema = cinemas, bairro = bairros)

#salvando o objeto em rdata
save(cinemas, file = "cinemas.RData")


####tidy

#acrescentando coluna de bairros
festival_do_rio %>% 
    inner_join(cinemas, by = c("local")) -> festival_do_rio

#desagregando as informações da coluna dia
festival_do_rio %>% 
    separate(dia, c("dia","data"), sep = ", ") -> festival_do_rio

#desagregando as informações da coluna paises
festival_do_rio %>% 
    separate(paises, c("paises","ano"), sep = " - ") -> festival_do_rio

#transformando os tipos de dados de algumas colunas
festival_do_rio %>% 
    mutate(ano = as.integer(ano),
           dia = as.factor(dia),
           genero = as.factor(genero),
           hora = times(paste0(hora, ":00")),
           duracao = as.integer(duracao),
           data = dmy(paste0(data,"/2018")),
           hora_fim = ifelse(times(hora + duracao/(24*60)) > 1,
                             times(hora + duracao/(24*60)) - 1,
                             times(hora + duracao/(24*60))
                             )
               ) -> festival_do_rio

#formatando como hora
festival_do_rio %>% 
    mutate(hora_fim = times(hora_fim)) -> festival_do_rio

#salvando csv e Rdata
save(festival_do_rio, file = "festival_do_rio_tidy.RData")
write.csv(festival_do_rio, file = "festival_do_rio_tidy.csv")

