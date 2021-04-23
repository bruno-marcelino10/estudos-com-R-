#### ---- HARVARD edX aula rvest ---- ####

library("tidyverse")
library("dslabs")

# É um pacote feito para realizar web scraping por meio de extração de tabelas de códigos em HTML

url <- "https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state"
h <- read_html(url) # lê o código em HTML da página


df <- h %>% html_nodes("table") # nodes são chunks de código em HTML que dividem cada tipo de objeto criado no site (uma tabela está dentro do node)
tab <- df[[2]] # separa a node desejada

tab <- tab %>% html_table %>% # retira a tabela da node 
    setNames(c("state", "population", "total", "murders", "gun_murders", "gun_ownership", "total_rate", "murder_rate", "gun_murder_rate"))

tab 

# podemos criar funções que fazem a leitura das nodes automaticamente, tendo em vista que as tabelas encontradas em um mesmo site seguem 
# geralmente um mesmo formato 