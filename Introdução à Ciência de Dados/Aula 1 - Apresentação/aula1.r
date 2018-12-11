library(dplyr)
library(data.table)
library(ggplot2)
library(ggthemes)
library(caTools)
set.seed(451)
setwd("~/Desktop/DS Brasil")

# [1] Read Data
raw.data = fread('heart.csv')

# [2] Inspect Data
glimpse(raw.data)
summary(raw.data)

# [3] Check for NA's
colSums(is.na(raw.data))

# [4] Check target variable distribution
table(raw.data$target)

# [5] Create function to facilitate data viz
plot_density = function(variable) {
  ggplot(raw.data, aes(x=variable, fill=as.factor(target)))+
    geom_density(alpha=0.7, color='white')+
    theme_hc(bgcolor='darkunica')+
    ggtitle('Variável vs Ataque Cardíaco')
}


# [6] Visualize some variables
plot_density(raw.data$chol)
plot_density(raw.data$age)
plot_density(raw.data$ca)

# [7] Data preparation and split
raw.data = as.data.frame(raw.data)

splitr = sample.split(raw.data, SplitRatio = 0.7)
train  = subset(raw.data, splitr == TRUE)
test   = subset(raw.data, splitr == FALSE)

# [8] Predictive Modeling
linear_model = lm(target ~ ., data=train)
summary(linear_model)

# [9] Model evaluation
lin.pred = predict(linear_model, newdata = test)
table(test$target, lin.pred >= 0.5)
(55 + 32) / (55 + 32 + 5 + 18)





