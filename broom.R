#### ---- HARVARD edX aula broom (tidymodels) ---- ####

library("tidymodels")
library("dslabs")
library("broom")

data(murders) 
murders <- tibble(murders)
murders_treino <- slice(murders, round(0.8*nrow(murders)):nrow(murders))
murders_teste <- slice(murders, 1:((nrow(murders)) - nrow(murders_treino)))

mod <- lm(population ~ total, data = murders_treino)
summary(mod)

# o pacote "broom" tem como objetivo extrair as informações contidas no objeto lm de forma agradável ao tidyverse, em formato tibble. 


#### ---- FUNÇÕES UTILIZADAS ---- ####

# tidy() <- retorna os coeficientes contidos dentro de um linear model (lm) em formato tibble 

tidy(mod)

# glance() <- estatísticas gerais do modelo como um todo (individuais)

glance(mod)

# augument() <- adiciona um dataframe com os dados originais, resíduos e valores previstos

augment(mod)

# predict(modelo lm ou glm, df) <- aplica os coeficientes obtidos na regressão, em cima dos objetos presentes no banco de dados de
# teste (cujas variáveis possuem o mesmo nome das utilizadas no modelo). Retorna um vetor com os valores previstos para cada observação. 

predict(mod, murders_teste)
