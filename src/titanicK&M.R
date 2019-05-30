##################
####  Titanic  
##################

# ---Càrrega de dades---
titanic.original <- read.csv("data/titanic_train.csv", header=TRUE)
# Composició
head(titanic.original)
tsummary(titanic.original)
str(titanic.original)
titanic <- titanic.original

# ---Nuls i buits---
colSums(titanic=="")
colSums(is.na(titanic))
titanic["Cabin"] <- NULL
sort(table(titanic$Embarked),decreasing=TRUE)
titanic$Embarked[titanic$Embarked==""]="S"
titanic$Age[is.na(titanic$Age)] <- mean(titanic$Age,na.rm=T)

seeOutlierValues <- function(dataset,arrayToCheck) {
  mean <- mean(arrayToCheck)
  standardDev <- sd(mean(arrayToCheck))
  min_value <- mean(arrayToCheck)-3*sd(arrayToCheck)
  max_value <- mean(arrayToCheck)+3*sd(arrayToCheck)
  newDatasetWOutliers <- dataset[(arrayToCheck<=min_value | arrayToCheck>=max_value),]
  outliers_count <- nrow(dataset)-nrow(newDatasetWOutliers)
  return (newDatasetWOutliers)
}
seeOutlierValues(titanic, titanic$Age)
seeOutlierValues(titanic, titanic$SibSp)
seeOutlierValues(titanic, titanic$Parch)
seeOutlierValues(titanic, titanic$Fare)

removeOutlierValues <- function(dataset,arrayToCheck) {
  mean <- mean(arrayToCheck)
  standardDev <- sd(mean(arrayToCheck))
  min_value <- mean(arrayToCheck)-3*sd(arrayToCheck)
  max_value <- mean(arrayToCheck)+3*sd(arrayToCheck)
  newDatasetWoOutliers <- dataset[(arrayToCheck>=min_value & arrayToCheck<=max_value),]
  outliers_count <- nrow(dataset)-nrow(newDatasetWoOutliers)
  cat("From", deparse(substitute(arrayToCheck)), outliers_count, "skipped tuples", "\n\n") 
  return (newDatasetWoOutliers)
}
titanic <- removeOutlierValues(titanic, titanic$Age)
titanic <- removeOutlierValues(titanic, titanic$SibSp)
titanic <- removeOutlierValues(titanic, titanic$Parch)
titanic <- removeOutlierValues(titanic, titanic$Fare)
write.csv(titanic, "../data/titanic_train_transformed.csv")