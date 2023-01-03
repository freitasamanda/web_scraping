# Exemplo de aula ensinando a extrair sites 
# que envolvam o uso da barra de busca

# Data de criação: 2022-11-30

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

# Usando a ferramente de busca ####


# Iremos buscar as proposições legislativas sobre a pandemia
# na Assembléia Legislativa de Minas Gerais:

# Ver o site:
"https://www.almg.gov.br/index.html"

# Buscamos as proposições em:
"https://www.almg.gov.br/atividade-parlamentar/projetos-de-lei/"

# Você precisa entender a lógica de busca no site fazendo uma 
# busca manual e comprendendo como os parâmetros se comportam na
# URL

# Vou pesquisar segundo os seguintes parâmetros:

# Assunto: Pandemia
# Tipo: Projeto de Lei
# Ano: 2020

# Deixei como estava todos os outros campos.

# O resultado foi 465 registros, sendo 10 registros por página.

# Na primeira página de busca, a URL da página de busca não muda,
# mas a partir da segunda página, ela passa a ter a 
# seguinte estrutura:

"https://www.almg.gov.br/atividade-parlamentar/projetos-de-lei/?pagina=1&q=pandemia&tipo=5&situacao=2&ano=2020&num=&ordem=0&pesquisou=true&autor="

# Vamos quebrar essa estrutura para entender cada parte:

# A primeira parte da URL é chamada de "HOST". Indica o provedor de dados.
"https://www.almg.gov.br"

# Depois, tudo que vem antes do "?" é a indicação
# de onde está sendo feita a busca. Por padrão, em sites o "?"
# indica que existem parâmetros sendo enviados à página.
"atividade-parlamentar/projetos-de-lei/?"

# Logo em seguida, vem a numeração da página. 
# No caso, tiramos os dados da página 2
"pagina=2&"

# Repare que entre um parâmetro e outro sempre existe um "&"
# É uma forma de contatenar vários parámentros

# Em seguida temos o termos de busca que indicados com o parâmentro "q"

"q=pandemia&"

# Depois tempos a indicação do tipo de proposição. A proposição
# de tipo 5 é projeto de lei. Vocês podem testar na barra de busca
# para ver qual é o número dos outros tipos.

"tipo=5&"

# Agora temos a sitação da proposta. Colocamos na caixa de seleção "todas"
# e a o parâmentro retornou 2, mostrando que 2 == "todas"
"situacao=2&"

# Temos o ano de busca, 2020
"ano=2020&"


# Depois temos o número da proposição. Como deixamos essa caixa
# em branco na busca, ela retornou sem nada. 
"num=&"

# A ordem em quem aparecem a proposição foi em zero (a opção default)
"ordem=0&"

# Existe um parâmetro que se chama "pesquisou", de valor true. 
# Não sei o que é e vou deixar assim
"pesquisou=true&"

# O autor da proposição está vazio porque não selecionamos nada aqui.
# Então vazio == "todos"
"autor="

# Podemos manipular os parâmetros acima para qualquer busca
# que quisermos!

# Por enquanto, vamos apenas baixar as 247 páginas da busca
# manipulando apenas o parâmetro PaginaAtual

"https://www.almg.gov.br/atividade-parlamentar/projetos-de-lei/?pagina=1&q=pandemia&tipo=5&situacao=2&ano=2020&num=&ordem=0&pesquisou=true&autor="

# Vamos usar o início da URL de busca
InicioURLBusca <- "https://www.almg.gov.br/atividade-parlamentar/projetos-de-lei/?pagina="

# E o final da URL
FinalURLBusca <- "&q=pandemia&tipo=5&situacao=2&ano=2020&num=&ordem=0&pesquisou=true&autor="

NumeroPaginas <- ceiling(465/10) # para arredondar para cima

# Vamos gerar as 25 páginas de busca:
PaginasBusca <- paste0(InicioURLBusca, seq_len(NumeroPaginas), FinalURLBusca)

# Vamos verificar:
PaginasBusca[1:3]
length(PaginasBusca)

# agora vamos pegar cada página e extrair os links das proposições:

# Aprendendo a extrair os links de uma página ####

# Vamos baixa uma única página de busca

# Vamos usar como teste a página de busca 2
url <- PaginasBusca[2]

# Baixa o HTML
PaginaBruta <- read_html(url)

# Nome do HOST  do site
HostURL <- "https://www.almg.gov.br"

# Baixa links das propostas
LinksALMG <- PaginaBruta %>% 
  html_nodes('main') %>%
  html_nodes('[class="d-flex align-items-center"]') %>%
  html_nodes('[class="almg-css_tipoCaption"]') %>% 
  html_nodes('h3') %>% 
  html_nodes('a') %>% 
  html_attr("href") %>% 
  # Dentro do site os links estão sem o HOST (pois são referências relativas)
  # Vamos usar o comando paste para adicionar novamente o HOST
  paste0(HostURL, .) 

# Aqui estão os links das proposições da página:
LinksALMG


# Baixando todos os links: #####


# Vamos criar um loop para baixar todos os links:

HostURL <- "https://www.almg.gov.br"

# Banco de dados para juntar todos os dados:
LinksProjetosALMG <- character()

# HOST
HostURL <- "https://www.almg.gov.br"

# Laço for para baixar todas
for (i in seq_len(NumeroPaginas)[1:5]) {
  # i <- 7 # Vamos testar o loop com a página de busca 7
  message("Baixando página ", i, " de ", NumeroPaginas)
  
  url <- PaginasBusca[i]
  
  PaginaBruta <- read_html(url)
  
  # Baixa links das propostas
  LinksALMG <- PaginaBruta %>% 
    html_nodes('main') %>%
    html_nodes('[class="d-flex align-items-center"]') %>%
    html_nodes('[class="almg-css_tipoCaption"]') %>% 
    html_nodes('h3') %>% 
    html_nodes('a') %>% 
    html_attr("href") %>% 
    paste0(HostURL, .) 
  
  LinksProjetosALMG <- c(LinksProjetosALMG, LinksALMG)
  
  Sys.sleep(2)
}

# Lista de todos os links baixados:
LinksProjetosALMG




# fim
