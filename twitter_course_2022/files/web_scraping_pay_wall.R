# Exemplo de aula ensinando a extrair sites com paywall

# Data de criação: 2022-11-29

# Autor: Murilo Junqueira (m.junqueira@yahoo.com.br)

# Caso algum dos pacotes abaixo não esteja instalado, usar
# o comando 'install.packages()' para instala-lo.

# Exemplo:
# install.packages("tidyverse")

library(tidyverse) # Reformula o R todo deixando ele maravilhoso!
library(data.table) # ajuda a ler grandes quantidades de dados
library(lubridate) # para lidar com datas
library(stringr) # funções úteis para lidar com texto
library(rvest) # ajuda o webscraping no R
library(xml2) # navegador de documentos XML
library(readxl) # abre arquivos de MS Excel


# Determinar o diretório de trabalho:
# choose.dir() # normalmente só é necessário usar isso aqui uma vez.

# Usar o comando 'setwd()' para determinar o diretório 
# de trabalho (coloquei o diretório do seu computador)
setwd("D:\\Users\\Murilo\\Dropbox\\Trabalho\\UFPA\\Escola de Metodologia\\2022-11 webscraping e Twitter\\Aulas")

# Guardei as senhas em um arquivo separado, para não divulgar
# elas aqui (uma boa prática de segurança)
ArquivoSenhas <- fread("Senhas.csv")

# email_login <-  ArquivoSenhas$Login[1]
# senha <-  ArquivoSenhas$Senha[1]

# Aprendendo a salvar uma página ####

#Address of the login webpage
login <- "https://acesso.estadao.com.br/login/"

#create a web session with the desired login address
pgsession <- session(login)

Formularios <- html_form(pgsession)

pgform <- html_form(pgsession)[[2]]  #in this case the submit is the 2nd form

# names(pgform$fields)

# Preenche o formulário com login e senha
filled_form <- html_form_set(pgform, 
                             email_login = ArquivoSenhas$Login[1], 
                             senha = ArquivoSenhas$Senha[1])

# Envia o formulário preenchido para o site e abre a sessão
session_submit(pgsession, filled_form)

# Retira objetos que não serão mais usados.
rm(pgform, login, filled_form)

# Página que iremos extrair informações
# (não vai funcionar se você não tiver uma senha do Estadão válida)
url <- "https://politica.estadao.com.br/noticias/geral,senador-randolfe-rodrigues-diz-ja-ter-assinaturas-para-criar-cpi-da-covid-19,70003604013"

# Baixa a página em local restrito
page <- session_jump_to(pgsession, url)

# Verifica que a página foi baixada (código 200)
page$response$status_code

# Extrai o Json dos metadados da página
NewsMetadata <- page %>% 
  html_nodes("head") %>%
  html_nodes('[type="application/ld+json"]') %>%
  magrittr::extract(1) %>% 
  html_text() %>% 
  jsonlite::fromJSON()

# Extrai os metadados da notícia através do JSON

# Data da notícia
Jor_Data <- NewsMetadata$datePublished[[1]] %>% 
  ymd_hms(tz = "America/Sao_Paulo", quiet = TRUE) %>% 
  as.character()

# Título da Notícia
Jor_Titulo <- NewsMetadata$headline[[1]]

# Nome do jornal
Jor_Jornal <- NewsMetadata$publisher$name[[1]]

# Descrição da notícia
Jor_Desc <- NewsMetadata$description[[1]]

# Autores da notícia
Jor_Autores <- paste(NewsMetadata$author$name, collapse = "|")

# Data da última atualização da notícia
Jor_DataUp <- NewsMetadata$dateModified[[1]] %>% 
  ymd_hms(tz = "America/Sao_Paulo", quiet = TRUE) %>% 
  as.character()


rm(url, NewsMetadata)

# Salvando as páginas em massa ####

# O processo de logar na página é igual ao início do script
# O que está abaixo não vai funcionar se você não estiver
# logado no site!

# Banco de dados com os links da página
NoticiasEstadao <- read_excel("ExemploEstadao.xlsx", 
                              sheet = "MateriasJornais")

# Olhando os dados
View(NoticiasEstadao)

# Variável só com os links
NewsLinks <- NoticiasEstadao$Jor_Link

NewsLinks[1:5]

# Cria um banco de dados em branco para guardarmos as informações
NewsData_df <- enframe(NewsLinks, name = NULL, value = "Jor_Link") %>% 
  mutate(Jor_Data = NA, # Variável com a data das notícias
         Jor_DataUp = NA, # Data da última atualização
         Jor_Titulo = NA, # Data do Título
         Jor_Jornal = "Estadão", # Nome do Jornal
         Jor_Desc = NA, # Descrição da notícia (lide)
         Jor_Autores = NA # Autores da notícia
         ) %>% 
  # Usamos o select para ordenar as variáveis
  select(Jor_Data, Jor_Titulo, Jor_Jornal, Jor_Link, Jor_Desc, 
         Jor_Autores, Jor_DataUp)

# Olhando o banco de dados em branco:
view(NewsData_df)

# Criamos um loop para baixar todas as notícias da nossa lista
for (i in seq_len(nrow(NewsData_df))[1:5]) {
  # i <- 10
  
  message("Baixando notícia ", i, "/", nrow(NewsData_df))
  
  url <- NewsLinks[i]
  page <- session_jump_to(pgsession, url)
  
  NewsMetadata <- page %>% 
    html_nodes("head") %>%
    html_nodes('[type="application/ld+json"]') %>%
    magrittr::extract(1) %>% 
    html_text() %>% 
    jsonlite::fromJSON()
  
  NewsData_df$Jor_Data[i] <- NewsMetadata$datePublished[[1]] %>% 
    ymd_hms(tz = "America/Sao_Paulo", quiet = TRUE) %>% 
    as.character()
  
  NewsData_df$Jor_Titulo[i] <- NewsMetadata$headline[[1]]
  
  NewsData_df$Jor_Desc[i] <- NewsMetadata$description[[1]]
  
  NewsData_df$Jor_Autores[i] <- paste(NewsMetadata$author$name, collapse = "|")
  
  NewsData_df$Jor_DataUp[i] <- NewsMetadata$dateModified[[1]] %>% 
    ymd_hms(tz = "America/Sao_Paulo", quiet = TRUE) %>% 
    as.character()
  
  # Paraliza a rotina por 2 segundos, 
  # deixando a rotina propositalmente lenta. 
  Sys.sleep(2)
  
}

# Olhando os dados
View(NewsData_df)

fwrite(NewsData_df, "NoticiasEstadaoOut.csv", 
       sep = ";", dec = ",")

# Padrão de enconding usando:
"UTF-8"

# fim
