### Aluno: Bruno Marcelino ######################### 
### Disciplina: IA para Contabilidade e Finanças ###
### Data: 15/10 ####################################

### --- Enunciado --- ###

# Desenvolva uma função com o nome “grafico_bonito” (sugere-se o uso do arquivo “Retornos.csv” para testar a função)
# Lembre-se de comentar a função e de usar testes lógicos para reportar/evitar erros.

# Argumentos: 
    ### dois vetores com retornos de ativos para um dado período
# Saídas: 
    ### gerar um gráfico de dispersão usando o ggplot2 (incluindo uma linha de tendência); 
    ### salvar o gráfico em "png";
    ### apresentá-lo na área de plotagem do R Studio.

### --- Importação dos Pacotes --- ###
library("tidyverse")
library("ggplot2")
library("gridExtra")

### --- Importação dos Dados --- ###
#retornos = read.csv(file.choose()) # Selecionar "Retornos.csv"
#ativo_1 = retornos$CEMIG
#ativo_2 = retornos$GERDAU

### --- Definindo a Função --- ###
grafico_bonito <- function(v1, v2){
    
    # Conferindo Entradas
    if((class(v1) != "numeric") & (class(v1) != "integer") & (class(v2) != "numeric") & (class(v2) != "integer")){
        stop("ERRO: os valores do vetor devem ser numeric ou integers")}
        
    if(any(is.na(v1)) | any(is.na(v2))){
        stop("ERRO: foram encontrados dados faltantes")}
    
    # A) Geração do Gráfico
    df = data.frame(cbind(v1, v2))
    
    graf <- ggplot(df, aes(x = v1, y = v2)) +
        geom_point() +
        labs(x = "Ativo 1", y = "Ativo 2", title = "Relação entre os Retornos") +
        geom_hline(yintercept = 0, linetype = "dashed") +
        geom_vline(xintercept = 0, linetype = "dashed") +
        scale_y_continuous(labels = scales::percent) +
        scale_x_continuous(labels = scales::percent) +
        geom_smooth(method = "lm", color = "red") + 
        theme_bw()
    
    # B) Salvando o Gráfico
    ggsave("grafico.png", plot = graf)
    
    # C) Mostrando o Gráfico
    print(graf)
}

### --- Testando a Função --- ###
#grafico_bonito(ativo_1, ativo_2)








