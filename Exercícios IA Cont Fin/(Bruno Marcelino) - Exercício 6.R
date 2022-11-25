### Aluno: Bruno Marcelino ######################### 
### Disciplina: IA para Contabilidade e Finanças ###
### Data: 25/11 ####################################

### --- Enunciado --- ###

# Elaborar um modelo empregando o algoritmo Naive Bayes para prever os setores de empresas
# com base em indicadores econômico-financeiros. Use o arquivo "Dados Setores 2.xlsx".

# Mais de 60% dos dados devem ser usados para treinamento e o restante para teste do modelo.
# A métrica empregada para avaliar o desempenho do modelo deve ser a acurácia.

### --- Importação dos Pacotes --- ###
library("tidyverse")
library("readxl") # read_excel()
library("e1071") # naiveBayes()
library("Metrics") # accuracy()

### --- Importação dos Dados --- ###
dados <- read_excel(file.choose()) # Selecionar "Dados Setores 2.xlsx"
dados$setor <- as.factor(dados$setor)

### --- Separação Treinamento e Teste --- ###
split = 0.6 
n_obs = length(dados$nome) 
n_obs_train = round(split*n_obs, 0)

train = dados[1:n_obs_train, 2:6]
test = dados[(n_obs_train+1):n_obs, 2:6]

### --- Modelagem --- ###
modelo <- naiveBayes(setor ~ lc + end + roa + mebit, data = train)
predict(modelo, test)
accuracy(test$setor, predict(modelo, test)) 



