library(dplyr)
library(data.table)
library(ggplot2)
library(ggthemes)
library(caTools)
set.seed(451)
setwd("~/Desktop/DS Brasil/datasciencebrasil/Introdução à Ciência de Dados/Aula 1 - Apresentação")

# [1] Ler arquivo
raw.data = fread('heart.csv')

# [2] Inspecionar dados
glimpse(raw.data)
summary(raw.data)

# [3] Verificar NA's
colSums(is.na(raw.data))

# [4] Verificar proporções da variável target
table(raw.data$target) # 54,45% de DCs

# [5] Criar função para gerar gráficos
plot_density = function(variable, title) {
  ggplot(raw.data, aes(x=variable, fill=as.factor(target)))+
    geom_histogram(color = 'white', alpha = 0.7, position='identity')+
    ggtitle(title)+
    theme_hc(bgcolor='darkunica')
}


plot_dot = function(variable, title) {
  ggplot(raw.data, aes(x=as.factor(target),y=variable, fill=as.factor(target)))+
    geom_boxplot(alpha = 0.7, color='grey80', width=0.25)+
    ggtitle(title)+
    theme_hc(bgcolor='darkunica')
}


plot_dot(raw.data$age, 'Distribuição de Idade vs DC')

plot_density(raw.data$age, 'Distribuição da Idade vs DC')
plot_density(raw.data$trestbps, 'BPM em repouso vs DC')

# [6] Realizar split de treino e teste
raw.data = as.data.frame(raw.data)

splitr = sample.split(raw.data, SplitRatio = 0.7)
train  = subset(raw.data, splitr == TRUE)
test   = subset(raw.data, splitr == FALSE)

# [7] Modelo de regressão linear

linear_model = lm(target ~ ., data = train)
summary(linear_model)

lin.pred = predict(linear_model, newdata = test)
table(test$target, lin.pred >= 0.5)
(33 + 52) / (33 + 52 + 7 + 17)  # 77,98% de acurácia!!!


























