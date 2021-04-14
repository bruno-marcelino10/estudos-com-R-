##### IMPORTA PACOTES ----------------------------------------------------------------------------------
library(tidyverse)
library(tidyquant)
library(lubridate)

##### IMPORTA DADOS-------------------------------------------------------------------------------------
# Importa os ultimos 5 anos de dados de PETR4
PETR4 <- "PETR4.SA" %>% tq_get(from = Sys.Date()-years(5))

class(PETR4)
##### ANALISE INICIAL DOS DADOS ------------------------------------------------------------------------
#Observa dados importados
PETR4

#Primeiras estatisticas descritivas
summary(PETR4)

#Explorando datas com fechamentos ajustados faltantes
PETR4 %>% filter(is.na(adjusted)) %>% select(date)

##### TRATANDO DADOS -----------------------------------------------------------------------------------
# Seleciona colunas de interesse
PETR4 <- PETR4 %>% select(date, adjusted)

# Trata dados faltantes
PETR4 <- PETR4 %>% mutate(adjusted2 = na.locf(adjusted))

##### CALCULA MEDIAS MOVEIS ----------------------------------------------------------------------------
PETR4 <- PETR4 %>%
  tq_mutate(select = adjusted, #cria media movel de 20 periodos
            mutate_fun = SMA,
            n = 20,
            col_rename = "SMA20") %>% 
  tq_mutate(select = adjusted, #cria media movel de 100 periodos
            mutate_fun = SMA,
            n = 100,
            col_rename = "SMA100")

PETR4 <- PETR4 %>% na.omit() #remove as linhas sem media movel

##### CRIA SINAIS --------------------------------------------------------------------------------------
PETR4 <- PETR4 %>%
  mutate(sinal = ifelse(SMA20 >= SMA100, "1", "-1"))

##### CRIA VETOR DE RETORNOS ---------------------------------------------------------------------------
PETR4 <- PETR4 %>%
  tq_mutate(select = adjusted,
            mutate_fun = dailyReturn,
            col_rename = "return")

##### SIMULA RETORNOS DA ESTRATEGIA --------------------------------------------------------------------
returns <- PETR4 %>%
  mutate(returns = ifelse(lag(sinal) == 1, return, -return)) %>%
  select(date, returns) %>%
  mutate(returns = replace_na(returns, 0))

returns <- returns %>%
  mutate(cum_returns = cumprod(1 + returns))

##### PLOTA O RESULTADO --------------------------------------------------------------------------------
returns %>% ggplot(aes(x = date, y = cum_returns)) +
  geom_line()

########################################################################################################
################################################ FUNÇÕES ###############################################
############################################### UTILIZADAS #############################################
########################################################################################################

# TIDYVERSE --------------------------------------------------------------------------------------------
mutate()
select()
filter()
replace_na()
ggplot()

# TIDYQUANT --------------------------------------------------------------------------------------------
tq_get() # puxa dados de ações
tq_mutate() # nova coluna composta por uma função 
na.locf() # ultima observação preenche o NA
SMA() # médias móveis
dailyReturn() # calcula retornos em uma série de preços

# LUBRIDATE --------------------------------------------------------------------------------------------
years() # operação com anos
year() # retira o ano de uma data 

# R BASICO ---------------------------------------------------------------------------------------------
Sys.Date() # data do sistema
summary() # lista com estatísticas descritivas
is.na() # verifica se tem ou não NA
ifelse() # operador condicional 
cumprod()