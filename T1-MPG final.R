# Instalando e carregando os pacotes necessários
if (!require(ggplot2)) {
  install.packages("ggplot2")
}
if (!require(gridExtra)) {
  install.packages("gridExtra")
}
library(ggplot2)
library(gridExtra)

# Limpando o ambiente
rm(list=ls())

# Carregando a base de dados mtcars
data(mtcars)
data(mpg)
# Criando um dataframe com as variáveis de interesse
dados <- mtcars[, c("mpg", "cyl", "wt", "hp")]

# Calcular o consumo médio de combustível na cidade e na estrada por classe de veículo
consumo_medio <- aggregate(cbind(cty, hwy) ~ class, data = mpg, FUN = mean)

# Ordenar as classes de veículo pelo consumo médio na cidade
consumo_medio <- consumo_medio[order(consumo_medio$cty), ]

# Criar o gráfico de barras para consumo médio na cidade
grafico_cty <- ggplot(data = consumo_medio, aes(x = class, y = cty, fill = class)) +
  geom_bar(stat = "identity", width = 0.4) +
  geom_text(aes(label = round(cty, 1)), vjust = -0.5, size = 3) +  # Adiciona rótulos com valores de cty
  labs(title = "Consumo Médio de Combustível na Cidade por Classe de Veículo",
       x = "Classe de Veículo",
       y = "Consumo Médio de Combustível na Cidade (mpg)",
       fill = "Classe de Veículo") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Criar o gráfico de barras para consumo médio na estrada
grafico_hwy <- ggplot(data = consumo_medio, aes(x = class, y = hwy, fill = class)) +
  geom_bar(stat = "identity", width = 0.4) +
  geom_text(aes(label = round(hwy, 1)), vjust = -0.5, size = 3) +  # Adiciona rótulos com valores de hwy
  labs(title = "Consumo Médio de Combustível na Estrada por Classe de Veículo",
       x = "Classe de Veículo",
       y = "Consumo Médio de Combustível na Estrada (mpg)",
       fill = "Classe de Veículo") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Criar o gráfico de barras com cores coloridas e rótulos de valor
grafico_cosumo_medio <- ggplot(data = consumo_medio, aes(x = class)) +
  geom_bar(aes(y = cty, fill = class), stat = "identity", width = 0.4) +
  geom_text(aes(y = cty, label = round(cty, 1)), vjust = -0.5, size = 3, color = "black") +  # Rótulos para cty
  geom_bar(aes(y = hwy, fill = class), stat = "identity", width = 0.4) +
  geom_text(aes(y = hwy, label = round(hwy, 1)), vjust = -0.5, size = 3, color = "black") +  # Rótulos para hwy
  labs(title = "Consumo Médio de Combustível por Classe de Veículo",
       x = "Classe de Veículo",
       y = "Consumo Médio de Combustível (mpg)",
       fill = "Classe de Veículo") +
  scale_fill_manual(values = rainbow(nrow(consumo_medio))) +  # Usar cores do arco-íris
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Criando o gráfico de pizza
grafico_pizza <- ggplot(dados, aes(x = "", fill = factor(cyl))) +
  geom_bar(width = 1) +
  coord_polar("y", start = 0) +
  labs(x = NULL, y = NULL, fill = "Número de Cilindros") +
  ggtitle("Distribuição de Carros por Número de Cilindros") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Exibindo os gráficos lado a lado
grid.arrange(grafico_hwy,grafico_cty, grafico_cosumo_medio,grafico_pizza, ncol = 2, top = "Gráficos de Análise de Dados Automotivos")
