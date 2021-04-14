library("tidyverse")
library("dslabs")

# QQ-Plot 
# (comparação entre os quantis da amostra e os quantis de uma distribuição normal com média e sd iguais aos da amostra).
# Quanto mais próximos da linha x = y, melhor 

data(heights)
tibble(heights)

x <- heights %>% 
    filter(sex == "Male")

# Calcular quantis observados e quantis teóricos (5% até 95%)
p <- seq(0.05, 0.95, 0.05)
observed_quantiles <- quantile(x$height, p)
theoretical_quantiles <- qnorm(p, mean = mean(x$height), sd = sd(x$height))

# Fazendo QQ-plot
plot(theoretical_quantiles, observed_quantiles)
abline(0,1) # 0 = intercepto da linha, 1 = inclinação da linha

# ou
qqplot(observed_quantiles, qnorm(p, mean = mean(x$height), sd = sd(x$height)))
abline(0,1) # 0 = intercepto da linha, 1 = inclinação da linha

# Boxplot
# permite comparar a distribuição de frequência de múltiplas amostras

# Mediana: é a linha central em negrito
# 1 e 3 quartil: são os extremos da vela
# Limites da linha tracejada: são os pontos de máximo e mínimo da amostra, excluindo outliers
# Outliers: são os pontos fora do plot

boxplot(x$height)

# Histograma 
# Mede a distribuição de frequência da amostra
# Barras: representam a quantidade de valores que esteve entre determinados valores de x 

hist(x$height, col = "2", density = TRUE)




