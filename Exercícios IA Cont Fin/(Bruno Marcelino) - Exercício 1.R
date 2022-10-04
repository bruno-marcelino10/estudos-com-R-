### Aluno: Bruno Marcelino ######################### 
### Disciplina: IA para Contabilidade e Finanças ###
### Data: 07/10 ####################################

### --- Enunciado --- ###

# Você deve desenvolver um programa em R que analise os dados do arquivo “precos_2019”.
# Importar o arquivo em formato "csv";
# Calcular as estatísticas descritivas de cada ativo;
# Gerar um painel que contenha os gráficos de todos os ativos ao longo do período;
# Salvar o painel como uma figura em formato “png” .
# Lembre-se de comentar o código para explicar os procedimentos realizados.

### --- Importação dos Pacotes --- ###
library("tidyverse")
library("ggplot2")
library("gridExtra")

### --- Importação dos Dados --- ###
precos = read.csv(file.choose()) # Selecionar "precos_2019"
precos$datas = as.Date(precos$datas)

### --- Estatísticas Descritivas --- ###
summary(precos$CEMIG)
summary(precos$GERDAU)
summary(precos$PETROBRAS)
summary(precos$BB)

### --- Painel com Gráficos de Preços --- ###
graf_CMIG = ggplot(precos, aes(datas, CEMIG)) + geom_line()
graf_GGBR = ggplot(precos, aes(datas, GERDAU)) + geom_line()
graf_PETR = ggplot(precos, aes(datas, PETROBRAS)) + geom_line()
graf_BBSA = ggplot(precos, aes(datas, BB)) + geom_line()
painel = grid.arrange(graf_CMIG, graf_GGBR, graf_PETR, graf_BBSA)
print(painel)

### --- Salvando Painel --- ###
ggsave("painel_1.png", plot = painel)
