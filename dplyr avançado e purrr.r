#### ---- HARVARD edX aula dplyr e purrr ---- ####

library("tidyverse")
library("dslabs")

data(murders) 
murders <- tibble(murders)

#### ---- dplyr ---- ####

# summarize() <- é a aplicação de funções nas colunas, feita para retornar somente um valor (podem ser operações aritméticas normais, desde que retornem APENAS um valor).
# Os argumentos são o nome da nova coluna e a função a ser aplicada.
# Obs.: Caso seja aplicada a função group_by() anteriormente (com base em alguma coluna de dados categóricos), será gerado um resultado (coluna) para cada categoria (linhas)

media <- murders %>% 
    group_by(region) %>% 
    summarize("media_pop" = mean(population)) 

# summarize_all() <- é a aplicação de funções nas colunas, feita para retornar somente um valor (podem ser operações aritméticas normais, desde que retornem APENAS um valor).

media <- murders %>% 
    group_by(region) %>% 
    summarize_all(funs(mean)) 

# Caso seja necessário ter como output um vetor numérico com os resultados obtidos (ao invés de um tibble), podemos utilizar a função ".". No caso, a função final retornará um 
# vetor com os resultados obtidos na coluna media_pop.

# Outra maneira de fazer isso é utilizando a função pull(coluna). 

vetor_media <- murders %>% 
    group_by(region) %>% 
    summarize("media_pop" = mean(population)) %>% 
    .$media_pop # acessa valores numéricos dos dados que estão sendo %>%. "." substitui o nome do banco de dados.  

vetor_media2 <- murders %>% 
    group_by(region) %>% 
    summarize("media_pop" = mean(population)) %>% 
    pull(media_pop) # retira uma coluna de dados numéricos.

# arrange() <- realiza a ordenação das observações em ordem crescente baseado em uma coluna em específico. 
murders %>% arrange(total)
    
# Caso queiramos ordenar de forma decrescente, usamos arrange(desc()).
murders %>% arrange(desc(total))

# Podemos ordenar a partir de outra ordenação 
murders %>% arrange(region, total) # ordena pela região, e dentro da categoria ordena pelo total

# top_n() <- mostra as n maiores observações para determinada característica
murders %>% top_n(5, total) # ordena pelo total, e demonstra as 5 maiores observações 

# case_when() <- permite realizar testes lógicos com diversas variáveis condicionais (evita escrever ifelse dentro de ifelse)

murders %>% 
    mutate(group = case_when(abb %in% c("ME", "NH", "VT", "MA", "RI", "CT") ~ "New England", # %in% = "pertence a"
                             abb %in% c("WA", "OR", "CA") ~ "West Coast",
                             region == "South" ~ "South",
                             TRUE ~ "Other")) # O argumento TRUE representa a função "else"

# do() <- aplica uma função a cada grupo de um tibble. 

my_summary <- function(dat){
    x <- quantile(dat$height, c(0, 0.5, 1)) # summary retornaria três respostas para a mesma função 
    tibble(min = x[1], median = x[2], max = x[3])} # criamos nosso próprio sumário

heights %>%
    group_by(sex) %>% # agrupa pelo fator sexo
    do(my_summary(.)) # gera o sumário para cada fator, não só para a coluna toda


#### ---- purrr ---- ####

# map() <- útil quando queremos aplicar uma mesma função a cada elemento de um vetor, e retornar uma lista de mesmo tamanho 

x <- c(0, 5, 8, 3, 2, 1)
f <- function(x) (x + 1)
map(x, f)

# Podemos escolher o output da função map, seguido do seu nome.

map_dbl(x, f) # retorna um vetor numérico
map_chr(x, f) # retorna um vetor de caracteres 

# caso estejamos trabalhando com listas dentro de outras listas (como se fosse uma matriz), podemos usar map(x, índice) para extrair o primeiro elemento de cada lista, 
# assim como se estivéssemos trabalhando com dataframes.

# map_if(vetor, condição, função) <- aplica a função caso algo aconteça

map_if(x, x > 2, f)

# map_at(vetor, posições, função) <- aplica a função em determinadas posições do vetor

map_at(x, c(1:3), f)


