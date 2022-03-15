# Coletando os dados

despesas <- read.csv("despesas.csv")

# Visualizando as variáveis
str(despesas)

# Medidas de Tendência Central da variável gastos
summary(despesas$gastos)

# Construindo um histograma
hist(despesas$gastos, main = 'Histograma', xlab = 'Gastos')

# Tabela de contingência das regiões
table(despesas$regiao)

# Explorando relacionamento entre as variáveis: Matriz de Correlação
cor(despesas[c('idade', 'bmi', 'filhos', 'gastos')])

# Nenhuma das correlações da matriz são consideradas fortes, mas existem algumas associações interessantes.
# A idade e o bmi (imc) parecem ter uma correlação positiva fraca, o que significa que com o aumento da idade, a massa corporal tende a aumentar.
# Há também uma correlação positiva entre a idade e os gatos, além do número de filhos e os gastos.

# Visualizando o relacionamento entre as variáveis: Scatterplot
# Perceba que não existe um claro relacionamento entre as variáveis
pairs(despesas[c('idade', 'bmi', 'filhos', 'gastos')])

# Scatterplot Matrix
library(psych)

# Este gráfico fornece mais informações sobre o relacionamento entre as variáveis
pairs.panels(despesas[c('idade', 'bmi', 'filhos', 'gastos')])

## Treinando o modelo
modelo <- lm(gastos ~ ., data = despesas)

# Visualizando os coeficientes
modelo

# Prevendo despesas médicas
previsao <- predict(modelo)
class(previsao)

head(previsao)

# Avaliando a Performance do Modelo
summary(modelo)

## Otimizando o Modelo

# Adicionado uma variável com o dobro do valor das idades
despesas$idade2 <- despesas$idade ^ 2

# Adicionando um indicador para BMI >= 30
despesas$bmi30 <- ifelse(despesas$bmi >= 30, 1, 0)

# Criando o modelo final
modelo_v2 <- lm(gastos ~ idade + idade2 + filhos +bmi +sexo + bmi30 * fumante + regiao, data = despesas)

summary(modelo_v2)
