#### ---- HARVARD edX aula stringr ---- ####

library("tidyverse")
library("dslabs")

data(murders) 
murders <- tibble(murders)

# Contém funções para manipulação de strings. Todas elas seguem um mesmo formato (str_função)


### STRINGS - DEFINIÇÃO

# podemos criar uma string colocando "" ou '' em volta de um texto. Usamos \ quando queremos usar aspas sem que o R as leia como conversoras de string

# cat() <- demonstra como o R lê a string específica (ao contrário do print, que mostra as strings com as aspas para diferenciá-las)

cat("5'10\"")
print("5'10\"")

# PADRÕES: são substrings (pequeno conjunto de strings que se repete ao longo de toda a string). Todas as funções do pacote stringr visam detectar e manipular padrões (patterns)
# dentro de um vetor de strings, para depois manipulá-los. 

# padrões podem ser dados tanto de forma direta (escrevemos exatamente o que queremos encontrar) quanto por meio de REGEX (expressões regulares)

# REGEX: são uma linguagem que descreve padrões em strings de forma mais abrangente, sem definir exatamente o caractere que desejamos encontrar (mas sim um conjunto de possíveis respostas). 


#### ---- TIPOS DE REGEX (EXPRESSÕES REGULARES) ---- ####

# \\d <- representa todos os números de 0 a 9
# \\s <- espaço em branco

# [:alpha:] <- todas as letras do alfabeto
# [:lower:] <- letras minúsculas
# [:upper:] <- letras maiúsculas 

### ALTERNADOS 
# | <- ou (somente em expressões regulares)
# [] <- pelo menos um
# [^a] <- tudo menos a
# [a-z] <- range (inclui todas as strings dentro desse tipo do caractere, partindo do primeiro até o outro)

### ÂNCORAS (definir a posição dos caracteres em relação ao início e fim da string)
# ^ <- começo da string analisada
# $ <- fim da string analisada

### QUANTIFICADORES (aparecem logo depois a um caractere de interesse)
# ? <- zero ou uma vez
# * <- zero ou mais vezes
# + <- um ou mais vezes
# {1} <- quantas vezes o tipo de caractere aparece
# {1,2} <- define quantas vezes esse tipo de caractere pode aparecer consecutivamente (entre uma ou duas no exemplo)

### GRUPOS (definem ordem de encontro dos caracteres, podemos nos referir a eles ao longo da regex)
# () <- criam grupos
# \\1 <- se refere ao primeiro grupo previamente declarado


#### ---- DETECTAR PADRÕES (SUBSTRINGS) EM STRINGS ---- ####

# str_detect() <- detecta a presença ou não de um padrão (caractere, número etc) em cada elemento de um vetor. 

str_detect(string = murders$population, # vetor que contém os dados 
           pattern = ",") # padrão para analisar se está ou não presente em cada dado. 

# str_which() <- retorna os índices das strings que possuem determinado padrão 
str_which(string = murders$population, pattern = "7") 

# str_count() <- retorna o número de ocorrências de determinado padrão em cada dado 
str_count(string = murders$population, pattern = "7") 

# str_locate() <- retorna a posição ao longo da string onde o padrão ocorre 
str_locate(string = murders$population, pattern = "7") 


#### ---- ALTERAR STRINGS ---- ####

# str_sub() <- altera os caracteres (substrings) das posições desejadas, para cada string

str_sub(string = murders$population, # vetor que contém os dados 
        start = 2, # posição do caractere de início da substring
        end = 5) <- # posição do caractere final da substring
            "xxx" # caracteres novos a serem colocados no lugar

# str_replace_all() <- identifica um padrão em cada string e o substitui por outro

str_replace_all(string = murders$population, # vetor que contém os dados 
                pattern = "7", # padrão a ser localizado
                replacement = "?") # padrão que substituirá o anterior

# str_replace() <- identifica a primeira ocorrência de um padrão em cada string e a substitui por outro

str_replace(string = murders$population, # vetor que contém os dados 
            pattern = "7", # padrão a ser localizado
            replacement = "?") # padrão que substituirá o anterior


# parse_number(PACOTE: READR) <- automaticamente remove vírgulas e converte a coluna para o formato numérico 


#### ---- EXTRAIR STRINGS ---- ####

# str_sub() <- retorna os caracteres (substrings) das posições desejadas, para cada string

str_sub(string = murders$population, # vetor que contém os dados 
        start = 2, # posição do caractere de início da substring
        end = 5) # posição do caractere final da substring

# str_subset() <- retorna somente as strings que contém determinado padrão

str_subset(string = murders$population, # vetor que contém os dados 
           pattern = "7") # padrão para analisar se está ou não presente em cada dado. 

# str_extract() <- retorna um vetor do MESMO tamanho do que contém os dados (o resto vira NA), com a primeira ocorrência do padrão em cada string

pattern_without_groups <- "^[4-7],\\d*$"
pattern_with_groups <-  "^([4-7]),(\\d*)$"

yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
x <- c(yes, no)

str_extract(x, pattern_with_groups)

# str_match() <- retorna vetores do mesmo tamanho do que contém os dados, porém em forma de matriz com uma coluna para cada grupo dentro do padrão

str_match(x, pattern_with_groups)


##### ---- VISUALIZAÇÃO DE STRINGS ---- ####

# str_view() <- permite a visualização em HTML da primeira ocorrência do padrão estabelecido 

str_view(string = murders$population, # vetor que contém os dados 
         pattern = "7") # padrão para analisar se está ou não presente em cada dado. 

# str_view_all() <- permite a visualização em HTML de todas as ocorrências do padrão estabelecido 

str_view_all(string = murders$population, # vetor que contém os dados 
         pattern = "7") # padrão para analisar se está ou não presente em cada dado. 


##### ---- TAMANHO DAS STRINGS ---- ####

# str_length() <- retorna o tamanho de cada string (em número de caracteres)

str_length(string = murders$population) # vetor que contém os dados 

# str_trim() <- retira os espaços em branco da frente e/ou de trás de uma string

str_trim(string = murders$population, # vetor que contém os dados 
         side = c("both", "left", "right")) # local de retirada de espaços em branco


##### ---- JUNTAR OU DIVIDIR STRINGS ---- ####

# str_c() <- junta diversas colunas de string em uma só

str_c(murders$population, murders$abb, # colunas a serem unidas
      sep = "_") # separador entre os elementos das colunas que serão concatenados

# o argumento ' collapse = "" ' permite juntar o vetor inteiro de strings em uma só. 

str_c(murders$population, murders$abb,
      sep = "_", # separa o argumento de cada coluna que será unida
      collapse = "/") # separa cada elemento do vetor que será unido novamente em uma string só

# str_split_fixed() <- permite dividir uma coluna de string em várias, dado um padrão e um número de colunas que se deseja como resultado. 

str_split_fixed(string = murders$population, # coluna a ser dividida
                pattern  = "7", # critério que separa as colunas
                n = 3) # quantas colunas serão criadas a partir dela 
     
