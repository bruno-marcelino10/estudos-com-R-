### Aluno: Bruno Marcelino ######################### 
### Disciplina: IA para Contabilidade e Finanças ###
### Data: 16/12 ####################################

### --- Enunciado --- ###

# Importe os dados do ativo BBDC4 para o período entre 01/01/2010 e 31/12/2019;
# Calcule o retorno do ativo, considerando o preço de fechamento;
# Selecione e calcule um ou mais indicadores técnicos apresentados no “CIC026 I - Parte III” da disciplina;
# Use o(s) indicador(es) técnico(s) selecionado(s) como dado(s) de entrada para treinamento de um modelo para prever variação do preço do ativo
# (alta ou baixa) do dia seguinte. Este modelo deve ser baseado em um algoritmo de aprendizado de máquina apresentado na disciplina;
# Avalie o desempenho do algoritmo em prever o aumento ou a redução do preço do ativo no dia seguinte com base no modelo.

# OBS:

# Escolha apenas UM algoritmo de aprendizado de máquina estudado na disciplina.
# Avalie o desempenho do algoritmo escolhido com base em uma métrica ADEQUADA conforme o problema de previsão.
# Use, no máximo, 4 (oito) indicadores técnicos.
# Use comentários para explicar todos os procedimentos realizados.

### --- Importação dos Pacotes --- ###
library("tidyverse")
library("tidyquant") # ta_get(), tq_mutate()
library("TTR") # RSI(), BBands(), SAR() 
library("randomForest") # randomForest()
library("Metrics") # accuracy(), auc(), predict()
library("gridExtra")

### --- Importação dos Dados --- ###

# Importa dados do Yahoo Finance
dados <- "BBDC4.SA" %>% 
    tq_get(get = "stock.prices", from = "2010-01-01", to = "2019-12-31") %>% 
    na.omit() %>% 
    select(-c(symbol))

# Análise Gráfica: Preço do Ativo
dados %>% ggplot(aes(x = date, y = adjusted)) +
    geom_line(linewidth = 1) +
    labs(title = "Cotação Diária: BBDC4", x = "", y = "Preço Ajustado") +
    scale_y_continuous(labels = scales::dollar) +
    theme_tq() + 
    scale_color_tq()

# Análise Gráfica: Retornos do Ativo
dados %>% mutate("retorno" = (close/lag(close))-1) %>% # Calculando Retorno do Ativo
    na.omit() %>% 
    ggplot(aes(x = date, y = retorno)) +
    geom_line(linewidth = 1) +
    labs(title = "Retorno Diário: BBDC4", x = "", y = "Retorno") +
    scale_y_continuous(labels = scales::percent) +
    theme_tq() + 
    scale_color_tq()

### --- Criação das Variáveis --- ###

##### Variáveis Criadas: (ta_mutate_fun_options()) 
# Variação da Média-Móvel de 10 Períodos do Preço - Medida da tendência acumulada até hoje
# Volatilidade Média de 10 Períodos do Preço - Medida da volatilidade acumulada até hoje
# Índice de Força Relativa da Ação - Medida de Sobrecompra ou Sobrevenda do Ativo
# Distância entre o SAR Parabólico e o Preço - Medida da força da tendência 

# Obs.: Todos os dados foram transformados para que se tornem osciladores estacionários em teoria. 
# Se seu valor aumenta quando o preço da ação aumenta, não fazem o menor sentido em um modelo real.

##### Alvo Previsto: Direção do Preço do Dia de Amanhã
# 0 - Baixa
# 1 - Alta

variaveis <- dados %>% 
    
    # Variáveis
    tq_mutate(select = "close", mutate_fun = SMA, col_rename = "mm10") %>% # Média-Móvel
    tq_mutate(select = "close", mutate_fun = volatility, col_rename = "vol") %>% # Volatilidade
    tq_mutate(select = "close", mutate_fun = RSI) %>% # Índice de Força Relativa
    tq_mutate(select = c(high, low), mutate_fun = SAR) %>% # SAR Parabólico
    
    # Manipulação das Variáveis
    mutate("sar" = sar - close) %>% # Distância entre o SAR e o Preço
    
    # Manipulação do Alvo
    mutate("retorno" = (close/lag(close))-1) %>% # Retorno do Ativo Hoje
    
    mutate("dir" = ifelse(retorno >= 0, 1, 0)) %>% # Direção do Preço de Hoje
    mutate("dir" = as.factor(dir)) %>% # Transforma o alvo em variável categórica
    
    mutate("dir" = lag(dir)) %>% # Atrasa o Alvo para que seja previsto o retorno de amanhã com dados de hoje
    mutate("retorno" = lag(retorno)) %>% # Atrasa o retorno para que a previsão seja comparada com o retorno de real de amanhã
    
    select(-c(open, high, low, close, adjusted, volume)) %>% # Apagando colunas que não serão mais utilizadas
    na.omit() # Apaga NA's
    
### --- Separação Treinamento e Teste --- ###
split = 0.6 # Série temporal deve ser dividida preservando a ordem dos dados
n_obs = length(variaveis$date) 
n_obs_train = round(split*n_obs, 0)

train = variaveis[1:n_obs_train,]
test = variaveis[(n_obs_train+1):n_obs,]

### --- Modelagem --- ###
set.seed(123) # Garantindo que o mesmo modelo sempre seja criado 
modelo = randomForest(dir ~ mm10 + vol + rsi + sar, data = train, na.action=na.omit) # Cria o modelo
previsoes = predict(modelo, select(test, -c(dir, date, retorno))) # Coluna de Valores Previstos

### --- Avaliação --- ###
acc = accuracy(test$dir, previsoes) # Acurácia do Modelo
auc = auc(test$dir, previsoes) # Área Abaixo da Curva ROC do Modelo
conf_matrix = table("previsto" = previsoes, "observado" = test$dir) # Matriz de Confusão do Modelo
imp = importance(modelo) # Qualidade de Cada Variável no Modelo
imp = imp / sum(imp) # Qualidade Relativa

print(paste("Acurácia =", acc)) 
print(paste("Área Abaixo da Curva ROC =", auc)) 
print("Importância Relativa de Cada Variável:")
print(imp)
print("Matriz de Confusão:")
print(conf_matrix)

### --- Backtest --- ###

# Consiste em avaliar o desempenho do modelo na base de teste (aqui desconsiderados os custos de transação).
# Compramos o ativo caso o modelo indique alta, e vendemos caso indique baixa
backtest <- test %>% 
    select(date, retorno, "observado" = dir) %>% 
    mutate("previsto" = previsoes) %>% 
    mutate("resultado" = ifelse(previsto == 1, retorno, -retorno)) %>% 
    mutate("cum_ret" = cumprod(1 + retorno)-1) %>% 
    mutate("cum_ret_mod" = cumprod(1 + resultado)-1) %>% 
    mutate("drawdown" = DrawdownPeak(resultado)) 
    
# Análise Gráfica: Retorno Acumulado do Modelo
ret_acum = backtest %>% ggplot() +
    geom_line(aes(x = date, y = cum_ret_mod), color = "red", linewidth = 1) +
    geom_line(aes(x = date, y = cum_ret), color = "blue", linewidth = 1) +
    labs(x = "Ano", y = "Retorno Acumulado",
         title = 'Comparação entre o desempenho do Modelo e de uma estratégia "Buy & Hold"',
         caption = "BH: Azul; Modelo: Vermelho") +  
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_y_continuous(labels = scales::percent) +
    theme_tq()

# Análise Gráfica: Drawdown - É a distância entre o valor de hoje e o último pico do modelo (se próximo de zero, 
# o modelo apresenta ganhos constantes sem muita volatilidade na queda)
dd = backtest %>% 
    ggplot(aes(x = date, y = drawdown)) +
    geom_area(linewidth = 1, fill = "darkblue") +
    labs(title = "Drawdown", x = "", y = "") +
    theme_tq() +
    scale_y_continuous(labels = scales::percent)

# Análise Gráfica: Distribuição dos Retornos Auferidos Pelo Modelo
hist_ret = backtest %>% ggplot() + 
    geom_histogram(aes(resultado), binwidth = 0.005, fill = "3", col = "black") +
    labs(title = "Distribuição de Frequência do Retorno Diário do Modelo", x = "", y = "") +
    scale_x_continuous(labels = scales::percent)

grid.arrange(ret_acum, dd, hist_ret)
