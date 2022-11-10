### Aluno: Bruno Marcelino ######################### 
### Disciplina: IA para Contabilidade e Finanças ###
### Data: 09/11 ####################################

### --- Enunciado --- ###

# Desenvolver funções que calculem as seguintes métricas de regressão para o arquivo "real_previsto.csv":
    
#a) Mean squared error (MSE)
#b) Mean absolute error (MAE)
#c) Root Mean squared error (RMSE)
#d) Mean absolute percentage error (MAPE)
#e) Mean percentage error (MPE)

### --- Importação dos Pacotes --- ###
library("tidyverse")
library("ggplot2")
library("gridExtra")

### --- Importação dos Dados --- ###
dados = read.csv(file.choose()) # Selecionar "real_previsto.csv"

### --- Definindo Funções --- ###
# y = Valores Reais
# x = Valores Previstos

# --- MSE --- #
MSE <- function(df){
    mse = sum((df$Real - df$Predito)^2)/length(dados$Real)
    return(mse)}

# --- MAE --- #
MAE <- function(df){
    mae = sum((abs(df$Real) - abs(df$Predito))^2)/length(dados$Real)
    return(mae)}

# --- RMSE --- #
RMSE <- function(df){
    rmse = sum(sqrt((df$Real - df$Predito)^2))/length(dados$Real)
    return(rmse)}

# --- MAPE --- #
MAPE <- function(df){
    mape = sum((abs(df$Real) - abs(df$Predito))/df$Real)*100/length(dados$Real)
    return(mape)}

# --- MPE --- #
MPE <- function(df){
    mpe = sum((df$Real - df$Predito)/df$Real)*100/length(dados$Real)
    return(mpe)}

### --- Testando as Funções --- ###
MSE(dados)
MAE(dados)
RMSE(dados)
MAPE(dados)
MPE(dados)
