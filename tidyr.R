#### ---- HARVARD edX aula tidyr ---- ####

library("tidyverse")
library("dslabs")

data(murders) 
murders <- tibble(murders)

data(extdata) 
extdata <- tibble(extdata)
path <- system.file("extdata", package = "dslabs")
filename <- file.path(path, "life-expectancy-and-fertility-two-countries-example.csv")
df <- read_csv(filename)


# tidyr é um pacote que permite a manipulação dos dados para o formato tidy, que é preferível em análises de dados.

# Formato TIDY: ocorre quando há sempre uma mesma observação na mesma linha, e colunas para cada variável.

# Formato WIDE: nesse caso, há ao menos uma coluna que contém somente variáveis. Tanto as colunas como essas linhas em específico contém variáveis, e as observações são dados de forma conjunta
# Ex: linha 1 e coluna 3 = uma observação, linha 1 e coluna 2 = OUTRA observação. Não é mais do formato tidy. 


#### ---- FUNÇÕES ---- ####

# spread(key, value) <- significa "separar". Converte dataframe de TIDY -> WIDE

wide <- murders %>% 
    spread(key = region, # seleciona uma coluna e espalha cada categoria dentro dela em uma coluna diferente.
           value = population) # os novos valores para cada coluna nova são os valores obtidos na coluna "value", e o nome dessa coluna some do banco de dados. Ela foi "separada" em várias. 

wide

# gather(key, value, colunas) <- significa "juntar". Converte dataframe de WIDE -> TIDY

# Os nomes de cada coluna selecionada (3º argumento) serão unidos em uma só coluna (1º argumento) e se tornarão uma variável categórica, que se repetirá para cada observação pertencente às
# colunas antigas. Os valores obtidos para as colunas antigas serão movidos para outra coluna (2º argumento)

# key: armazena as categorias. value: armazena as observações. 3º argumento: armazena as colunas cujos nomes serão a key e valores obtidos para cada uma serão o value. 

tidy <- wide %>% 
    gather(key = region, # nome da coluna que receberá variáveis categóricas para cada coluna "wide". Todas essas colunas serão unidas em uma só.
           value = population, # nome da coluna que receberá os valores obtidos em cada coluna antiga a ser unida.  
           -c(state, abb, total)) # colunas "wide" que serão transformadas em uma só 

tidy

# podemos fazer as mesmas operações com as funções novas introduzidas no tidyverse, tendo em vista que spread() e gather() foram descontinuadas. 

# pivot_wider(names_from, values_from) <- substitui spread(key, value)

wider <- murders %>% 
    pivot_wider(names_from = region, 
                values_from = population)
            #    ,values_fill = 0) substitui valores faltantes por esse número

wider

# pivot_longer(cols, names_to, values_to) <- substitui gather(key, value, coluna)

longer <- wider %>% 
    pivot_longer(cols = !c(state, abb, total), 
                 names_to = "region", 
                 values_to = "population",
                 values_drop_na = TRUE) # apaga as observações que eram NA's nas colunas antigas 

longer

# ou

df <- df %>% pivot_longer(cols = -country, names_to = "key", values_to = "value")

# separate() <- separa os valores obtidos na coluna original em várias partes.

df %>% separate(key, #coluna a ser separada
                c("year", "variable_name"), # nome das colunas novas a serem criadas
                sep = "_") # caractere que separa as strings

# quando há colunas que possuem mais de uma string para ser separada e outras não, podemos usar o argumento fill = "right" ou extra = "merge".

df %>% separate(key,
                c("year", "first_variable_name", "second_variable_name"),
                "_",
                fill = "right") # Cria uma coluna que recebe NA quando não tiver essa outra string. 

df %>% separate(key,
                c("year", "variable_name"),
                sep = "_",
                extra = "merge") # Após realizar a separação em 2 colunas, as próximas strings são agrupadas com a última coluna

# unite() <- une os valores obtidos em várias colunas, em uma só

df %>% separate(key, c("year", "first_variable_name", "second_variable_name"), "_", fill = "right") %>% 
    unite(col = "variable_name", # nome da nova coluna criada 
          first_variable_name, # colunas a serem unidas
          second_variable_name, # colunas a serem unidas
          sep = "_") %>% # separador entre as palavras
    mutate(variable_name = ifelse(variable_name == "fertility_NA", "fertility", variable_name))



