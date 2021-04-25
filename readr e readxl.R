#### ---- HARVARD edX aula readr e readxl ---- ####

library("tidyverse")
library("dslabs")
library("readr")
library("readxl")

# ambas as bibliotecas são feitas para a leitura de diversas planilhas ou bancos de dados dentro do R, porém readxl é focado em ler
# planilhas oriundas do Excel. 

# read_lines() <- mostra as primeiras linhas de um banco de dados

read_lines("data/dados.csv", # caminho relativo até o arquivo selecionado
           n_max = 3) # máximo de linhas a serem lidas

# read_excel() <- lê arquivos do Excel (tanto xls (antigo) quanto xlsx (novo))

read_excel("data/excel.xls")

# read_csv() <- lê arquivos .csv

read_csv("data/dados.csv")

# podemos utilizar a função read_csv diretamente de uma url de um site. Basta acrescentar a url do download dentro da função.  

read_csv("https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv", "murders.csv")

# download.file() <- baixa um banco de dados da url e salva no PC

download.file("https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv", "murders.csv")

# tempfile() <- gera um nome aleatório para um arquivo

tempfile()
