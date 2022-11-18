### Aluno: Bruno Marcelino ######################### 
### Disciplina: IA para Contabilidade e Finanças ###
### Data: 17/11 ####################################

### --- Enunciado --- ###

# Elaborar um modelo empregando o algoritmo K-Nearest Neighbours para prever os setores de 
# empresas com base em indicadores econômico-financeiros. Use o arquivo "dados_setores.csv".

# 70% dos dados devem ser usados para treinamento e 30% para teste do modelo.
# A métrica empregada para avaliar o desempenho do modelo deve ser a acurácia.
# Devem ser usados valores de "k" que variam entre 1 e 10 (Dica: o loop "for" pode ajudar nisso).
# O valor de k que maximiza o valor da acurácia deve ser destacado.
# Lembre-se de comentar bem o código.

### --- Importação dos Pacotes --- ###
library("Metrics") # accuracy()
library("class") # knn()

### --- Importação dos Dados --- ###
dados = read.csv(file.choose()) # Selecionar "dados_setores.csv"

### --- Separação Treinamento e Teste --- ###
split = 0.7 
n_obs = length(dados$nome) 
n_obs_train = round(split*n_obs, 0)

X_train = dados[1:n_obs_train, 2:6]
y_train = dados[1:n_obs_train, 7]
X_test = dados[(n_obs_train+1):n_obs, 2:6]
y_test = dados[(n_obs_train+1):n_obs, 7]

### --- Definindo Função --- ###
modelo = function(k, X_train, y_train, X_test, y_test){
    
    # -- Argumentos 
    # k: número de modelos criados, em ordem crescente a partir de k = 1
    # X_train: valores observados p/ as variáveis independentes no conjunto de treinamento
    # y_train: valores observados p/ a variável dependente no conjunto de treinamento
    # X_test: valores observados p/ as variáveis independentes no conjunto de teste
    # y_test: valores observados p/ a variável dependente no conjunto de teste
    
    # -- Saída
    # A) Acurácia de cada modelo criado alterando o valor do K
    # B) Valor de K que maximiza a acurácia do modelo KNN
    
    k = seq(k)
    accuracies = vector()
    for(i in k){
        mod = knn(X_train, X_test, y_train, k = i, prob = T)
        acc = accuracy(as.character(mod), y_test)
        accuracies = append(accuracies, acc)
        print(paste("k =", i, "<-> Acurácia =", round(acc*100,2), "%"))
    }
    
    print(paste("Melhor Modelo: k =", which(accuracies %in% max(accuracies))[1]))
}

modelo(10, X_train, y_train, X_test, y_test)

    
    
    
    