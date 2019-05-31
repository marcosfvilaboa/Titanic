##################
####  Titanic  
##################

# ---Data load---
titanic.original <- read.csv("data/titanic_train.csv", header=TRUE)
# Composition of data
head(titanic.original)
summary(titanic.original)
str(titanic.original)

# ---Pre-process---
# subsetting skipping 'passangerId', 'Embarked' and 'Ticket' columns from original

## MANTINC passangerId per poder
titanic <- titanic.original[, c(1:8, 10, 11)]
# extract passenger names saving only the titles
titanic$Name <- as.factor(gsub('(.*, )|(\\..*)', '', titanic$Name))
# change 'Name' column name to 'Title'
names(titanic)[names(titanic) == "Name"] <- "Title"
# combines some passenger titles
# (from tnikaggle user in kaggle --> https://www.kaggle.com/tysonni/extracting-passenger-titiles-in-r)
# and Narcel Reedus September 14, 2017 --> https://rpubs.com/Nreedus/Titanic
library(dplyr)
levels(titanic$Title)
titles_lookup <- data.frame(Title = c("Capt", "Col", "Don", "Dr", "Jonkheer", "Major", "Rev", "Sir",
                                      "Mr", "Master", 
                                      "Lady", "Mlle", "Mme", "Ms", "the Countess",
                                      "Mrs", "Miss"), 
                            New.Title = c(rep("Noble male", 8),
                                          "Mr", "Master",
                                          rep("Noble female", 5),
                                          "Mrs", "Miss"),
                            stringsAsFactors = FALSE)
titanic <- titanic %>%
  left_join(titles_lookup, by = "Title")
titanic <- titanic %>%
  mutate(Title = New.Title) %>%
  select(-New.Title)
# see possibles sex-title missmatch
titanic %>%
  filter((Sex == "female" & (Title == "Noble male" | Title == "Mr" | Title == "Master") |
           (Sex == "male" & (Title == "Noble female" | Title == "Mrs" | Title == "Miss"))))
# change that row
titanic <- titanic %>%
  mutate(Title=replace(Title, (Sex == "female" & (Title == "Noble male")), "Noble female"))

# ---Nulls & empties---
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

# ---Outliers---
#To-Do

write.csv(titanic, "../data/titanic_train_transformed.csv")

