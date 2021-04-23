#### ---- dplyr básico ---- ####

library("tidyverse")
library("dslabs")

data(murders) 
murders <- tibble(murders)

# select() <- seleciona colunas desejadas pelo nome 

# comumente utilizada para retirar somente as colunas úteis de um df, ou reordenar as colunas

murders %>% select(state, abb, total)

# filter() <- filtra determinadas linhas que obedecem a um critério lógico

# Critérios Lógicos: >, <, ==, >=, <=, !=, %in% (pertence a), is.na, &, any(), all()

murders %>% filter(total >= 500)

# mutate() <- altera as colunas de um df

# caso a coluna já exista no df, ela será alterada. Caso ela ainda não exista no df, será adicionada a ele. 

murders %>% mutate("log_total" = log(total))

# mutate_at() <- apluca a mesma alteração em diversas colunas do df

murders %>% mutate_at(4:5, log)

# caso as variáveis estejam agrupadas por group_by(), mutate aplicará as operações em cada grupo separadamente e armazenará tudo no df. 

# Funções comuns para usar mutate: lag, lead, log, min_rank, cum_sum, cumprod, cum_mean, cum_min, case_when ou ifelse 

# transmute() <- cria um novo df com colunas novas. É uma mescla das funções select e mutate.  

murders %>% transmute(state, abb, "total_em_milhares" = total/1000)

# slice() <- seleciona as linhas especificadas

murders %>% slice(5:10) %>% mutate("rank" = cumprod(total))




