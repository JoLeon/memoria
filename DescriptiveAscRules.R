# Libreria para reglas de asociación
library(arules)
users <- read.csv("Data/users.csv", header = TRUE)
rules <- apriori(users)
