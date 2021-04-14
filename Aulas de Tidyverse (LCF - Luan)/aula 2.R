##### IMPORTA PACOTES ----------------------------------------------------------------------------------
library("tidyverse")
library("tidyquant")
library("lubridate")
library("gridExtra")

##### IMPORTA DADOS-------------------------------------------------------------------------------------

tickers <- c("WEGE3.SA", "HGTX3.SA", "CIEL3.SA", "ITUB4.SA")

dados <- tq_get(tickers,
                from = Sys.Date()-years(5))

##### ANALISE INICIAL DOS DADOS ------------------------------------------------------------------------
#Observa dados importados
dados

#Primeiras estatisticas descritivas
summary(dados)

#Observa dados unicos da coluna symbol
dados %>% summarize(unique(symbol)) #summarize retorna dados únicos. Se estão agrupados, retorna uma linha p/ cada grupo

#Observa o numero de fechamentos ajustados faltantes por ativo
dados %>% group_by(symbol) %>% summarise(sum(is.na(adjusted)))

#Observa o numero de NAs por ativo e por coluna
dados %>% group_by(symbol) %>% summarise_all(~sum(is.na(.)))

# Numero de NAs por data
dados %>% filter(is.na(adjusted)) %>% group_by(date) %>% summarize(sum(is.na(adjusted)))

##### TRATANDO DADOS -----------------------------------------------------------------------------------
# Seleciona colunas de interesse
dados <- dados %>% select(symbol, date, adjusted)

# Trata dados faltantes
dados <- dados %>% group_by(symbol) %>% mutate(adjusted = na.locf(adjusted))

##### CALCULA A BANDA DE BOLLINGER ---------------------------------------------------------------------
dados <- dados %>% 
  group_by(symbol) %>% 
  tq_mutate(select = adjusted,
            mutate_fun = BBands)

dados <- dados %>% na.omit() #remove as linhas com NA

##### CRIA SINAIS --------------------------------------------------------------------------------------
dados <- dados %>%
  group_by(symbol) %>%
  mutate(sinal = NA) %>%
  mutate(sinal = ifelse(adjusted >= up, 1, sinal)) %>% #compra
  mutate(sinal = ifelse(adjusted >= mavg & lag(adjusted) < mavg, 0, sinal)) %>% #zera
  mutate(sinal = ifelse(adjusted <= dn, -1, sinal)) %>% #vende
  mutate(sinal = ifelse(adjusted <= mavg & lag(adjusted) > mavg, 0, sinal)) %>% #zera
  mutate(sinal = ifelse(date == min(date) & is.na(sinal), 0, sinal)) %>% # se 1 elemento é NA substitui por 0
  mutate(sinal = na.locf(sinal)) #propaga o sinal

##### CRIA VETOR DE RETORNOS ---------------------------------------------------------------------------
dados <- dados %>%
  group_by(symbol) %>%
  tq_mutate(select = adjusted,
            mutate_fun = dailyReturn,
            col_rename = "return")

##### SIMULA RETORNOS DA ESTRATEGIA --------------------------------------------------------------------
dados <- dados %>%
  group_by(symbol) %>%
  mutate(model_returns = ifelse(lag(sinal) == 1, 
                          return, 
                          ifelse(lag(sinal) == -1, 
                                 -return, 
                                 0))) %>%
  mutate(model_returns = replace_na(model_returns, 0)) %>%
  mutate(cum_model_returns = cumprod(1 + model_returns))

###### PLOTAR RESULTADOS --------------------------------------------------------------------------------
# Retorno acumulado por ativo
dados %>% ggplot(aes(date, cum_model_returns)) + # adiciona eixos x e y
  geom_line() + # adiciona gráfico de linhas
  facet_wrap(~symbol, scales = "free") # separa vários gráficos com escalas diferentes, um para cada ticker

# Retorno acuumulado por ativo - grafico unico
dados %>% ggplot(aes(date, cum_model_returns, col = symbol)) + # separa uma cor para cada ticker
  geom_line() +
  geom_hline(yintercept = 1, 
             linetype = "dashed") # coloca uma linha em destaque

#Posição ao longo do tempo por ativo
dados %>% mutate(sinal = ifelse(sinal == 1, 
                                "Comprado", 
                                ifelse(sinal == -1, 
                                       "Vendido", 
                                       "Não posicionado"))) %>%
  ggplot(aes(date, adjusted)) +
  geom_line() +
  geom_point(aes(col = lag(sinal)), size = 1) +
  facet_wrap(~symbol, scales = "free") +
  theme(legend.position = "bottom")

#Drawdown por ativo
dados %>% 
  group_by(symbol) %>% 
  mutate(DD = DrawdownPeak(model_returns)) %>%
  ggplot(aes(x = date, y = DD, col = DD)) +
  geom_line() +
  facet_wrap(~symbol, scales = "free")

#Todos os tres tipos de graficos de um ativo juntos
dados %>%
  group_by(symbol) %>%
  mutate(DD = DrawdownPeak(model_returns)) %>%
  mutate(sinal = ifelse(sinal == 1, 
                        "Comprado", 
                        ifelse(sinal == -1, 
                               "Vendido", 
                               "Não posicionado"))) %>%
  filter(symbol == "WEGE3.SA") %>%
  select(date, adjusted, sinal, cum_model_returns, DD) %>%
  pivot_longer(cols = c(cum_model_returns, adjusted, DD),
               names_to = "Variavel") %>%
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  geom_point(aes(col = lag(sinal))) +
  facet_wrap(~Variavel, scales = "free", nrow = 3)

#Usando o grid arrange
grafico1 <- dados %>%
  filter(symbol == "WEGE3.SA") %>%
  filter(year(date) == 2020) %>%
  ggplot(aes(date, cum_model_returns)) +
  geom_line() +
  scale_y_continuous(labels = scales::percent) +
  theme_tq() +
  scale_color_tq()

grafico2 <- dados %>%
  filter(symbol == "WEGE3.SA") %>% 
  filter(year(date) == 2020) %>%
  mutate(sinal = ifelse(sinal == 1, 
                        "Comprado", 
                        ifelse(sinal == -1, 
                               "Vendido", 
                               "Não posicionado"))) %>%
  ggplot(aes(date, adjusted)) +
  geom_line() +
  geom_point(aes(col = lag(sinal)), size = 1) +
  theme(legend.position = "bottom") +
  theme_tq() +
  scale_color_tq()


grafico3 <- dados %>%
  filter(symbol == "WEGE3.SA") %>%
  filter(year(date) == 2020) %>%
  mutate(DD = DrawdownPeak(model_returns)) %>%
  ggplot(aes(x = date, y = DD)) +
  geom_line() +
  theme_tq() +
  scale_y_continuous(labels = scales::percent) +
  scale_color_tq()

grid.arrange(grafico1, grafico2, grafico3)
