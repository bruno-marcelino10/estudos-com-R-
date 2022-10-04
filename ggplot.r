#### ---- HARVARD edX aula GGPLOT2 ---- ####

# Para usar o ggplot2, dados SEMPRE devem estar em formato tidy

# Componentes de um Gráfico:

# 1: dados 
# 2: geometria (tipo de gráfico)
# 3: mapeamento estético (cores, dados que estão nos eixos x e y, texto) 
# 4: escala 
# 5: legenda, título, tema etc

library("tidyverse")
library("dslabs")

data(murders) 
tibble(murders)

murders %>% ggplot() # cria o gráfico (em branco), único argumento requerido é o banco de dados

# Cada componente novo a ser adicionado no gráfico é adicionado por camadas, representadas por "+"

# "aes()" é o único argumento requerido para adicionar geometrias. Ela define seu mapeamento (eixos x e y, agrupamento dos dados ou funções específicas).
# Podemos incluir "aes" dentro da função inicial ggplot(), o que irá criar parâmetros globais de mapeamento. Isso significa que caso não
# seja especificado em cada geometria, o mapeamento a ser utilizado será o global. 

# DENTRO do aes: argumentos que afetam os pontos do gráfico separadamente
# FORA do aes: argumentos que afetam TODOS os pontos do gráfico da mesma maneira

murders %>% ggplot(aes(x = population/10^6, y = total)) + # só podemos acessar as colunas pelo nome dentro do aes(), direto da fonte (banco de dados)
    geom_point() # adiciona um gráfico de pontos

# geom_text: adiciona texto no gráfico

murders %>% ggplot(aes(x = population/10^6, y = total)) +
    geom_point() +
    geom_text(aes(x = population/10^6, y = total, label = abb), nudge_x = 1)

# scale_x_continuous: altera escalas do gráfico 

murders %>% ggplot(aes(x = population/10^6, y = total)) +
    geom_point() +
    geom_text(aes(label = abb), nudge_x = 0.05) + 
    scale_x_continuous(trans = "log10") + # põe o eixo em escala logarítmica
    scale_y_continuous(trans = "log10") # põe o eixo em escala logarítmica
    
# col = variável_categórica: adiciona uma cor para cada categoria dessa variável (dentro do aes)

murders %>% ggplot(aes(x = population/10^6, y = total)) +
    geom_point(aes(col = region), size = 3) + # adiciona cores aos pontos conforme a região
    geom_text(aes(label = abb), nudge_x = 0.05) + 
    scale_x_continuous(trans = "log10") +
    scale_y_continuous(trans = "log10") +
    scale_color_discrete(name = "Region") + # Altera o nome da legenda de cores
    xlab("Population in millions (log scale)") + # Adicionam rótulos (labels) aos eixos
    ylab("Total number of murders (log scale)") + # Adicionam rótulos (labels) aos eixos
    ggtitle("US Gun Murders in US 2010") + # Adiciona um título

library("ggthemes") # adiciona temas diversos aos gráficos
library("ggrepel") # adiciona uma geom que permite visualizar o texto distante dos pontos (repelir)

murders %>% ggplot(aes(x = population/10^6, y = total)) +
    geom_point(aes(col = region), size = 3) + # adiciona cores aos pontos conforme a região
    geom_text_repel(aes(label = abb)) + # O texto repele os pontos e fica fácil observar
    scale_x_continuous(trans = "log10") +
    scale_y_continuous(trans = "log10") +
    scale_color_discrete(name = "Region") + # Altera o nome da legenda de cores
    xlab("Population in millions (log scale)") + # Adicionam rótulos (labels) aos eixos
    ylab("Total number of murders (log scale)") + # Adicionam rótulos (labels) aos eixos
    ggtitle("US Gun Murders in US 2010") +
    theme_economist() # adiciona um tema da biblioteca "ggthemes"

# facet_wrap( ~ coluna) <- separa um gráfico em vários, para cada categoria inclusa em "coluna".

# O argumento ncol especifica qual o número de colunas a serem criadas com nossos gráficos. Ex: ncol = 1 coloca todos os gráficos a serem criados em uma mesma coluna. 

# O argumento strip.position especifica onde o rótulo de cada categoria de gráfico está exposto. Ex: strip.position = "left"

#### ---- Gráficos de Distribuição ---- ####

data("heights")
tibble(heights)
heights <- heights %>% filter(sex == "Male")

# HISTOGRAMA
heights %>% ggplot() +
    geom_histogram(aes(height), # vetor com os dados
                   binwidth = 1, # largura de cada barra
                   fill = "3", # Cor do preenchimento
                   col = "black") # Cor da borda das colunas
    
# DENSIDADE
heights %>% ggplot() + 
    geom_density(aes(height), fill = 2) # fill = cor do preenchimento

# QQ-PLOT
heights %>% ggplot() +
    geom_qq(aes(sample = scale(height))) + # scale padroniza a amostra
    geom_abline() # adiciona linha com intercepto 0 e inclinação 1

# Nesse caso podemos utilizar a função dparams (dentro da geometria porém fora do aes) para definir os parâmetros da distribuição da amostra em c(media, sd).

# BARRAS 

heights %>% ggplot() + 
    geom_bar(aes(sex)) 



