### Aluno: Bruno Marcelino ######################### 
### Disciplina: IA para Contabilidade e Finanças ###
### Data: 09/12 ####################################

### --- Enunciado --- ###

# Elaborar um modelo empregando o algoritmo de Redes Neurais para prever os fluxos de caixa livres (FCL) de 
# empresas com base em indicadores econômico-financeiros. Use o arquivo "BD RNA.xlsx".

# 60% dos dados devem ser usados para treinamento e 40% para teste do modelo.
# A métrica empregada para avaliar o desempenho do modelo deve ser a Raiz do Erro Quadrático Médio (RMSE).
# Devem ser usadas de uma a três camadas ocultas de neurônios, contendo de um a três neurônios em cada uma.
# Apresente o gráfico da rede neural que minimiza o valor de RMSE.

### --- Importação dos Pacotes --- ###
library("tidyverse")
library("Metrics") # rmse()
library("readxl") # read_excel()
library("neuralnet") # neuralnet()

### --- Importação dos Dados --- ###
df <- read_excel(file.choose()) # Selecionar "BD RNA.xlsx"

### --- Manipulação dos Dados --- ###
normalizar <- function(x){return ((x - min(x)) / (max(x) - min(x)))}

dados <- df %>% 
    select("Alavancagem" = AlaFin4T19,
           "M_EBITDA" = MrgEbitda4T19,
           "FCL" = FCL4T19,
           "Ativo" = AtivoTot4T19,
           "Ativo_def" = AtivoTot3T19,
           "Risco" = Beta4T19) %>% 
    mutate("Crescimento" = (Ativo - Ativo_def) / Ativo_def) %>% 
    select(-c(Ativo_def)) %>% 
    na.omit() %>% 
    mutate_all(normalizar)

### --- Separação Treinamento e Teste --- ###
split = 0.6 
n_obs = length(dados$Alavancagem) 
n_obs_train = round(split*n_obs, 0)

train = dados[1:n_obs_train, ]
test = dados[(n_obs_train+1):n_obs,]

### --- Definindo Função --- ###
modelo = function(n, m, treino, teste){
    
    # n = numero de camadas
    # m = numero de neurons
    set.seed(123)
    errors = vector() # Vetor que salva o RMSE
    for(i in 1:n){
        
        for (j in 1:m){
            
            camadas = rep(j, i) # Cria o número de camadas do modelo a ser testado
            mod = neuralnet(FCL ~ Alavancagem + M_EBITDA + Ativo + Risco + Crescimento, treino, hidden = camadas) # Cria o modelo
            error = rmse(predict(mod, teste), teste$FCL) # Calcula o RMSE 
            errors = append(errors, error) # Salva o RMSE
            print(paste("RMSE:", error)) # Printa o RMSE do modelo na tela
        }
    }
    
    print(paste("Melhor Modelo:", which(errors %in% min(errors))[1])) # Ao final, retorna o modelo que minimiza o RMSE.
}

### --- Combinações de Camadas Testadas: --- ###

# Modelo 1:    1
# Modelo 2:    2
# Modelo 3:    3   -> MELHOR MODELO
# Modelo 4:   1 1
# Modelo 5:   2 2
# Modelo 6:   3 3
# Modelo 7:  1 1 1
# Modelo 8:  2 2 2
# Modelo 9:  3 3 3

modelo(3,3, train, test) # Testando os Modelos Possíveis
    
melhor_mod = neuralnet(FCL ~ Alavancagem + M_EBITDA + Ativo + Risco + Crescimento, train, hidden = c(3), linear.output=T) # Criando novamente o melhor modelo
plot(melhor_mod, show.weights = T) # Printando a rede
