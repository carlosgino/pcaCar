library(plyr)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(caret)
library(MASS)
library(randomForest)
library(party)


library(readxl)
train_clientes <- read_excel("BBVA Attrition/train_clientes.xlsx")
View(train_clientes)
str(train_clientes)
summary(train_clientes)


train_requerimientos <- read_excel("BBVA Attrition/train_requerimientos.xlsx")
View(train_requerimientos)
str(train_requerimientos)
summary(train_requerimientos)

sapply (train_clientes, function (x) sum (is.na (x)))

train_clientes <- train_clientes [complete.cases (train_clientes),]

