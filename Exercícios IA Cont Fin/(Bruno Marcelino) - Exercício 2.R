### Aluno: Bruno Marcelino ######################### 
### Disciplina: IA para Contabilidade e Finanças ###
### Data: 08/10 ####################################

### --- Enunciado --- ###

# Você deve desenvolver um programa em R que analise os dados do arquivo “Retornos.csv”.
# Importar o arquivo;
# Calcular as estatísticas descritivas de cada ativo;
# Gerar um gráfico no GGPlot para a relação entre os retornos de cada ativo e o retorno de mercado (incluindo uma linha de tendência);
# Gerar um painel que contenha os gráficos previamente gerados de todos os ativos ao l;
# Salvar o painel como uma figura “png”;
# Calcular o beta de cada ativo;
# Calcular o custo de capital próprio (mensurado pelo CAPM de cada ativo); (considere o retorno de mercado como “26,64803%” o retorno do ativo livre de risco como “5,875%”)
# Gerar uma tabela que apresente o custo do capital próprio de todos os ativos. 

### --- Importação dos Pacotes --- ###
library("tidyverse")
library("ggplot2")
library("gridExtra")

### --- Importação dos Dados --- ###
retornos = read.csv(file.choose()) # Selecionar "Retornos.csv"

### --- Estatísticas Descritivas --- ###
summary(retornos)

### --- Gráficos do CAPM de cada ativo --- ###

capm_CMIG <- ggplot(retornos, aes(x = retornos$IBOVESPA, y = retornos$CEMIG)) +
    geom_point() +
    labs(x = "Prêmio de Risco do Mercado",
         y = "Prêmio de Risco do Ativo",
         title = "CAPM",
         subtitle = "Empresa: CEMIG") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(labels = scales::percent) +
    geom_smooth(method = "lm", color = "red") + 
    theme_bw()

capm_GGBR <- ggplot(retornos, aes(x = retornos$IBOVESPA, y = retornos$GERDAU)) +
    geom_point() +
    labs(x = "Prêmio de Risco do Mercado",
         y = "Prêmio de Risco do Ativo",
         title = "CAPM",
         subtitle = "Empresa: GERDAU") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(labels = scales::percent) +
    geom_smooth(method = "lm", color = "red") + 
    theme_bw()

capm_PETR <- ggplot(retornos, aes(x = retornos$IBOVESPA, y = retornos$PETROBRAS)) +
    geom_point() +
    labs(x = "Prêmio de Risco do Mercado",
         y = "Prêmio de Risco do Ativo",
         title = "CAPM",
         subtitle = "Empresa: PETROBRAS") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(labels = scales::percent) +
    geom_smooth(method = "lm", color = "red") + 
    theme_bw()

capm_BBSA <- ggplot(retornos, aes(x = retornos$IBOVESPA, y = retornos$BB)) +
    geom_point() +
    labs(x = "Prêmio de Risco do Mercado",
         y = "Prêmio de Risco do Ativo",
         title = "CAPM",
         subtitle = "Empresa: BANCO DO BRASIL") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(labels = scales::percent) +
    geom_smooth(method = "lm", color = "red") + 
    theme_bw()

### --- Painel com Gráficos do CAPM de cada ativo --- ###
painel_capm = grid.arrange(capm_CMIG, capm_GGBR, capm_PETR, capm_BBSA)
print(painel_capm)

### --- Salvando Painel --- ###
ggsave("painel_capm.png", plot = painel_capm)

### --- Cálculo do Beta de Cada Ativo --- ###
beta_CMIG <- cov(retornos$IBOVESPA, retornos$CEMIG) / var(retornos$IBOVESPA)
beta_GGBR <- cov(retornos$IBOVESPA, retornos$GERDAU) / var(retornos$IBOVESPA)
beta_PETR <- cov(retornos$IBOVESPA, retornos$PETROBRAS) / var(retornos$IBOVESPA)
beta_BBAS <- cov(retornos$IBOVESPA, retornos$BB) / var(retornos$IBOVESPA)

### --- Cálculo do Ke (Custo de Capital Próprio) de Cada Ativo --- ###
# Retorno de Mercado = 26,64803%
# Retorno do Ativo Livre de Risco = 5,875%
Ke_CMIG <- 0.05875 + (beta_CMIG * (0.2664803-0.05875))
Ke_GGBR <- 0.05875 + (beta_GGBR * (0.2664803-0.05875))
Ke_PETR <- 0.05875 + (beta_PETR * (0.2664803-0.05875))
Ke_BBAS <- 0.05875 + (beta_BBAS * (0.2664803-0.05875))

### --- Tabela com Custo de Capital Próprio de Cada Ativo --- ###
Ke = c(Ke_CMIG, Ke_GGBR, Ke_PETR, Ke_BBAS)
ativo = c("CEMIG", "GERDAU", "PETROBRAS", "BB")
tabela = cbind(ativo, Ke)
colnames(tabela) = c("Ativo", "Custo de Capital Próprio (Ke)")
print(tabela)