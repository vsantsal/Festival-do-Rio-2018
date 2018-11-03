library(rvest)
library(tibble)
library(dplyr)
library(lubridate)
library(tidyr)

converte_na <- function(x){
    if (length(x) == 0) {
        NA
    } else {
        x
    }
}

addTime<-function(timeTxt,mins){ 
    
    #http://r.789695.n4.nabble.com/Adding-minutes-to-24-hour-time-td1596431.html
    start.time<-strsplit(timeTxt,":") 
    start.time<-do.call("rbind",start.time) 
    storage.mode(start.time)<-"numeric" 
    hours<-mins%/%60 
    mins.left<-mins%%60 
    end.mins<-(start.time[,2]+mins.left)%%60 
    end.hours<-(start.time[,1]+hours+(start.time[,2]+mins.left)%/%60)%%24 
    end.time<-paste(end.hours,end.mins,sep=":") 
    end.time
} 


scrap_festival <- function(url){
  #adaptado de função do Luiz utilizada na oficina de web scraping
  require(rvest)
  require(tibble)
  require(readr)
  
  pag <- read_html(url)
  
  titulo <- pag %>%
    html_node(".noticias strong") %>%
    html_text(trim = TRUE) %>% 
    converte_na#pegando título do filme
  
  #print(titulo)
  
  diretor <- pag %>%
    html_nodes(".ficha-tecnica h4") %>%
    html_text(trim = TRUE) %>% 
    converte_na#pegando nome do diretor
  
  #print(diretor)
  
  mostra <- pag %>%
      html_nodes(".ficha-tecnica a") %>%
      html_text(trim = TRUE) %>% 
      converte_na#obtendo a mostra

  #print(mostra)
  
  ficha_tecnica <- pag %>%
      html_nodes("h4+ p") %>%
      html_text() #obtendo ficha técnica, para pegarmos gênero, duração e países
  
    
  genero <- strsplit(ficha_tecnica, "\n")[[1]][1] %>% 
      converte_na
  
  #print(genero)
  
  duracao <- as.character(
      parse_number(
          strsplit(
              ficha_tecnica, 
              "\n")[[1]][2]))%>% 
      converte_na
  
  #print(duracao)
  
  paises <- strsplit(ficha_tecnica, "\n")[[1]][3] %>% 
      converte_na
  
  #print(paises)
  
  sinopse <- pag %>%
      html_nodes("#sinopse p") %>%
      html_text(trim = TRUE) %>% 
      converte_na#pegando sinopse
  
  #print(sinopse)
  
  info_locais <- pag %>%
      html_nodes("td") %>% 
      html_text(trim = TRUE) %>% 
      converte_na#infos de dia, hora e local
  
#  if (sinopse != "Sinopse em breve.") {

      tibble(titulo, 
             diretor, 
             sinopse,
             genero,
             duracao,
             paises,
             mostra = as.factor(mostra),
             dia = info_locais[c(T,F,F)],
             hora = info_locais[c(F,T,F)],
             local = as.factor(info_locais[c(F,F,T)]))
      
            
 # } else {
      
      # tibble(titulo, 
      #        diretor, 
      #        sinopse,
      #        genero = NA,
      #        duracao = NA,
      #        paises = NA,
      #        mostra = NA,
      #        dia = info_locais[c(T,F,F)],
      #        hora = info_locais[c(F,T,F)],
      #        local = as.factor(info_locais[c(F,F,T)]))
      # 
#  }
  
}

url <- "http://www.festivaldorio.com.br/br/programacao?page=" #core do url a ser pesquisado

pag <- 1:65 #número de páginas de filmes

lista <- vector("list", length(pag)) #inicializando vetor em que guardaremos as páginas dos filmes

#fazendo o scraping dos filmes
for (i in seq_along(pag)) {
    
    url_pag <- paste0(url,pag[[i]])
    
    paste0("http://www.festivaldorio.com.br",read_html(url_pag, encoding = "UTF-8") %>% 
        html_nodes(css = "h5 a") %>% 
        html_attr("href"))-> lista[[i]]
    
}

#convertendo em vetor de caracteres e reduzindo a valores únicos
lista <- lista %>% unlist %>% unique

#montando tibble
festival_do_rio <- lapply(lista, scrap_festival) %>%
  do.call(rbind, .)


#salvando rdata
save(festival_do_rio, file = "festival_do_rio.RData")


#####ajustes adicionais

festival_do_rio %>% 
    separate(dia, c("dia","data"), sep = ", ") -> festival_do_rio



#salvando csv
write.csv(festival_do_rio, "festival_do_rio.csv")

teste <- scrap_festival(lista[16])

teste %>% 
    mutate(hora_fim = addTime(hora,duracao)) -> teste

lista[134] %>% 
    scrap_festival()
