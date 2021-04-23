#### ---- HARVARD edX aula dplyr manipulação de dois df ---- ####

library("tidyverse")
library("dslabs")

data(murders) 
data(polls_us_election_2016)

murders <- tibble(murders)
polls_us_election_2016 <- tibble(polls_us_election_2016)
       
tab1 <- slice(murders, 1:6) %>% select(state, population) # retira somente as linhas de 1 a 6
tab2 <- slice(results_us_election_2016, c(1:3, 5, 7:8)) %>% select(state, electoral_votes)

#### ---- UNIR DATAFRAMES COM BASE EM UMA VARIÁVEL EM COMUM ---- ####

# as funções realizam uma junção de dataframes a partir de uma coluna de nome idêntico. 

# left_join() <- utiliza como base nas observações dessa coluna obtidas no df inicial

tab1 %>% left_join(tab2, by = "state")

# right_join() <- utiliza como base nas observações dessa coluna obtidas no novo df acrescentado na função

tab1 %>% right_join(tab2, by = "state")

# inner_join() <- são filtradas somente as observações obtidas para essa coluna que estão presentes em ambos os df

tab1 %>% inner_join(tab2, by = "state")

# full_join() <- cria uma observação para cada linha da coluna de nome idêntico, não importando se há aquela observação está presente nos dois df. Acrescenta NA's caso não haja aquela observação 
# no df de origem de cada variável. 

tab1 %>% full_join(tab2, by = "state")

#### ---- FILTRAR COM BASE EM OUTRO DATAFRAME ---- ####

# semi_join() <- filtra o df antigo com base nas observações da coluna de interesse que estão presentes em ambos os df

tab1 %>% semi_join(tab2, by = "state")

# anti_join() <- filtra o df antigo com base nas observações da coluna de interesse não que estão presentes no novo df acrescentado na função

tab1 %>% anti_join(tab2, by = "state")

#### ---- COMBINAR DATAFRAMES ---- ####

# bind_cols() <- une as colunas de ambos os dataframes 

tab1 %>% bind_cols(tab2)

# bind_rows() <- une as linhas de ambos os dataframes

tab1 %>% bind_rows(tab2)

#### ---- OPERAÇÕES COM CONJUNTOS APLICADA A DATAFRAMES ---- ####

tab1 <- slice(murders, 1:6) %>% select(state, population)
tab3 <- slice(murders, 3:8) %>% select(state, population)

# O único pré-requisito é de que os dataframes comparados nas operações de conjuntos (união, interseção etc) possuam os mesmos nomes para as colunas. 

# intersect() <- retorna as linhas que são idênticas em ambos os df

tab1 %>% intersect(tab3) # interseção de conjuntos

# union() <- acrescenta as linhas que não estão presentes no df original mas que estão no novo, no df original

tab1 %>% union(tab3) # união de conjuntos

# setdiff() <- retira as linhas que também estão no novo df  

tab1 %>% setdiff(tab3) # diferença entre o data frame antigo e o novo (A - B)

# setequal() <- retorna se os dataframes são os mesmos, não importando a ordem

tab1 %>% setequal(tab3)  


