### Aluno: Bruno Marcelino ######################### 
### Disciplina: IA para Contabilidade e Finanças ###
### Data: 02/12 ####################################

### --- Enunciado --- ###

# Elaborar um modelo empregando o algoritmo Random Forest para prever os setores de 
# empresas com base em indicadores econômico-financeiros. Use o arquivo "Dados Setores 3.xlsx".

# 60% dos dados devem ser usados para treinamento e 40% para teste do modelo.
# A métrica empregada para avaliar o desempenho do modelo deve ser a acurácia.
# Devem ser usadas entre 3 e 50 árvores
# O número de árvores maximiza o valor da acurácia deve ser destacado.

### --- Importação dos Pacotes --- ###
library("tidyverse")
library("Metrics") # accuracy()
library("randomForest") # randomForest()
library("readxl")

### --- Importação dos Dados --- ###
dados <- read_excel(file.choose()) # Selecionar "Dados Setores 3.xlsx"
dados$setor <- as.factor(dados$setor)

### --- Separação Treinamento e Teste --- ###
split = 0.6 
n_obs = length(dados$nome) 
n_obs_train = round(split*n_obs, 0)

train = dados[1:n_obs_train, 2:6]
test = dados[(n_obs_train+1):n_obs, 2:6]

### --- Definindo Função --- ###
modelo = function(n, treino, teste){
    
    # Cria vários modelos em cima dessa base de dados, um para cada número de árvores testado
    
    accuracies = vector() # Vetor que salva a acurácia
    for(i in n){
        mod = randomForest(setor ~ lc + end + roa + mebit, data = treino, n_tree = i) # Cria o modelo
        acc = accuracy(teste$setor, predict(mod, teste)) # Calcula a sua acurácia
        accuracies = append(accuracies, acc) # Salva a acurácia
        print(paste("Número de Árvores =",i , "<-> Acurácia =", acc)) # Printa o modelo na tela
    }
    
    print(paste("Melhor Modelo: n =", which(accuracies %in% max(accuracies))[1])) # Ao final, retorna o menor número de árovres que maximiza a acurácia.
}

modelo(3:50, train, test)
