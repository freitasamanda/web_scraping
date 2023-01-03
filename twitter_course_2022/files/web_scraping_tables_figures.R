# Exemplo de aula ensinando a extrair tabelas e figuras

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


# Determinar o diretório de trabalho:
# choose.dir() # normalmente só é necessário usar isso aqui uma vez.

# Usar o comando 'setwd()' para determinar o diretório 
# de trabalho (coloquei o diretório do seu computador)
setwd("D:\\Users\\Murilo\\Dropbox\\Trabalho\\UFPA\\Escola de Metodologia\\2022-11 webscraping e Twitter\\Aulas")


# página em que vamos extrair as informações:
url <- "https://pt.wikipedia.org/wiki/Lista_de_pa%C3%ADses_e_territ%C3%B3rios_por_%C3%A1rea"


# Comando 'read_html()' lê a página da internet
PaginaBruta <- read_html(url)

# Baixando as tabelas separadamente
Tabelas <- PaginaBruta %>% 
  html_node('body') %>% 
  html_node('div.mw-page-container') %>% 
  html_nodes('table') %>% 
  html_table()

# Unificando as tabelas: ####

# Retirando a última tabela (que não é relevante para nós)
Tabelas <- Tabelas[1:6]

# Banco de dados unificado
TabelasUnificadas <- tibble()

# 'seq_along' gera sequências baseado no número de elementos (length)
# de um objeto.
length(Tabelas) # Tabelas tem 6 elementos

seq_along(Tabelas)

# Vamos usar um laço for para unificar as tabelas
for (i in seq_along(Tabelas)) {
  # i <- 1
  message("Loop ", i)
  
  # Extrai uma tabela do objeto 'Tabelas'
  TabelaNova <- Tabelas[[i]]
  
  # Corrige a variável "Ordem", 
  # deixando ela no tipo 'integer' (números inteiros)
  TabelaNova <- TabelaNova %>% 
    mutate(Ordem = ifelse(Ordem == "-", NA, Ordem)) %>% 
    mutate(Ordem = as.integer(Ordem))
  
  # Usa a função 'bind_rows' para empilhar as tabelas
  TabelasUnificadas <- bind_rows(TabelasUnificadas, TabelaNova)
}

# Vamos dar uma olhada no resultado:
View(TabelasUnificadas)


# Unificado as tabelas (segunda abordagem) ####

# Baixando as tabelas todas juntas
TabelasUnica <- PaginaBruta %>% 
  html_node('body') %>% 
  html_node('div.mw-page-container') %>% 
  html_table()

# Retirando rótulos, que erroneamente foram considerados
# registros:
TabelasUnica <- TabelasUnica %>% 
  dplyr::filter(Ordem != "Ordem")


# Extraindo imagens: ####

# Extraindo o link da imagem
# (basta usar o que aprendemos par achar onde está o link da figura)
URLImagem <- PaginaBruta %>% 
  html_node('body') %>% 
  html_node('[id="content"]') %>%
  html_node('[class="thumbinner"]') %>% 
  html_node('img') %>% 
  # Novinal, o link é um atributo de um dos elementes:
  html_attr('src') %>% 
  # No link está faltando o "https:", vamos inserir esta parte que falta
  paste0("https:", .) # no magrittr, o "." é o resultado das linhas anteriores marcadas com o pipe (%>%)

# Vamos dar uma olhada no link
URLImagem

# Agora podemos usar o link (URL) e a função 'download.file' 
# para baixar o arquivo. 
download.file(URLImagem, 
              'PaisesWikipediaAula.jpg', # Esse aqui é o nome do arquivo
              mode = 'wb')
  
  






