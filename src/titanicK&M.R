##################
####  Titanic  
##################

# ---1. Càrrega de dades---
titanic.original <- read.csv("~/R/TCVD/Titanic/data/titanic_train.csv", header=TRUE)
# Composició
head(titanic.original)
summary(titanic.original)
str(titanic.original)
titanic <- titanic.original
