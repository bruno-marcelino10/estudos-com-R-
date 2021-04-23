#### ---- HARVARD edX aula lubridate ---- ####

library("tidyverse")
library("dslabs")
library("lubridate")

data("polls_us_election_2016")
polls_us_election_2016 <- tibble(polls_us_election_2016)


# o pacote "lubridate" tem como objetivo manipular datas (tipo específico de string) a partir de diferentes formatos. 


#### ---- TIPOS DE DATAS ---- ####

# "epoch" é uma data referencial que marca o início da contagem de datas. No Brasil, é o nascimento de Jesus Cristo. Já em diversos programas, é "1970-01-01". Por isso, o formato 
# numérico padrão para datas são os dias que se passaram desde 1970. Ex: 2017-11-02 = 17753.

# Um objeto "hms" (horas, minutos e segundos) armazena o número de segundos que já se passaram em um dia, desde 00:00:00.

# Um objeto "datetime" (data e hora) representa o número de segundos que ocorreram desde "1970-01-01 00:00:00"


#### ---- TIPOS DE FUNÇÕES ---- ####

#### ---- CONVERTEM DATAS PARA O FORMATO PADRÃO DE DATA E HORA  (yyyy-mm-dd hh:mm:ss) ---- ####

# para transformar as datas, basta saber qual o seu formato padrão (ordem de y, m e d). A partir daí, aplicamos a função que corresponde ao formato atual da data, e ela irá alterá-la
# para um formato padrão para todas as datas. Exemplos: 

dmy("04/07/1999") # dia mês e ano
dym("01-2009-04") # dia, ano, mês

mdy("July 4th, 2000") # mês dia e ano
myd("01200931") # mês, ano, dia

ydm("200901 !!! 07") # ano, dia, mês
ymd("Created on 2009 1 6") # ano, mês e dia

mdy_hms("Nov/2/2012 12:34:56")

# stamp() <- faz o caminho contrário, criando uma função que retorna as datas em um formato desejado

modelo <- stamp("Jan 17, 1999 3:34") 
modelo(ymd("2010-04-05"))


#### ---- EXTRAÇÃO DE ELEMENTOS DAS DATAS ---- ####

# as funções de extração de elementos leem o argumento como se estivesse no formato padrão (yyyy-mm-dd)

x <- "09/01/02 11:58:59"

# dia mes e ano
day(x) # dia
month(x) # mês
year(x) # ano

# hora minuto e segundo
second(x) # segundo
minute(x) # minuto
hour(x) # hora

# dia ou noite (vetores lógicos)
am(x)
pm(x)

# estatísticas anuais

wday(x) # dia da semana
week(x) # semana do ano

qday(x) # dia do trimestre 
quarter(x) # trimestre do ano
semester(x) # semestre do ano


#### ---- EXTRAIR HORÁRIO DO SISTEMA ---- ####

now() # data e hora de hoje 
today() # data de hoje


#### ---- MATEMÁTICA COM DATAS ---- ####

### Períodos
# são mudanças na passagem do tempo. Podemos somar ou subtrair períodos para realizar as operações 
# todos os períodos são criados pluralizando o nome da unidade temporal desejada (sufixo "s")

today() + days(1)
today() + months(1)
today() + years(1)
today() + weeks(1)

# note que somando meses podemos obter datas fictícias como 31 de fevereiro, retornando NA. Os operadores aritméticos %m+% (+) e %m-% (-) somam e subtraem
# meses de forma a pular essas quebras no tempo

dmy("31/01/2021") + months(1) # 31/02

dmy("31/01/2021") %m+% months(1) # 28/02

### Intervalos
# são intervalos de tempo entre duas datas, que possuem início e fim

interval(now() - years(1), # início
         now() + years(1)) # fim

# %within% <- apresenta vetor lógico que representa se a data pertence ou não a um intervalo

now() %within% interval(now() - years(1), now() + years(1))


