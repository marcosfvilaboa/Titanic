##################
####  Titanic  
##################

# ---Data load---
titanic.original <- read.csv("data/titanic_train.csv", header=TRUE)
##data composition
head(titanic.original)
summary(titanic.original)
str(titanic.original)

# ---Pre-process---
## subsetting skipping 'passangerId', 'Embarked' and 'Ticket' columns from original
titanic <- titanic.original[, c(2:8, 10, 11)]
## extract passenger names saving only the titles
titanic$Name <- as.factor(gsub('(.*, )|(\\..*)', '', titanic$Name))
### change 'Name' column name to 'Title'
names(titanic)[names(titanic) == "Name"] <- "Title"
## combines some passenger titles
## (from tnikaggle user in kaggle --> https://www.kaggle.com/tysonni/extracting-passenger-titiles-in-r)
## and Narcel Reedus September 14, 2017 --> https://rpubs.com/Nreedus/Titanic
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
### see possibles sex-title missmatch
titanic %>%
  filter((Sex == "female" & (Title == "Noble male" | Title == "Mr" | Title == "Master") |
           (Sex == "male" & (Title == "Noble female" | Title == "Mrs" | Title == "Miss"))))
### change that row
titanic <- titanic %>%
  mutate(Title=replace(Title, (Sex == "female" & (Title == "Noble male")), "Noble female"))

# ---Nulls & empties---
colSums(titanic=="")
colSums(is.na(titanic))
titanic["Cabin"] <- NULL
titanic$Age[is.na(titanic$Age)] <- mean(titanic$Age,na.rm=T)

# ---Outliers---
seeOutlierValues <- function(dataset,arrayToCheck) {
  mean <- mean(arrayToCheck)
  standardDev <- sd(mean(arrayToCheck))
  min_value <- mean(arrayToCheck)-3*sd(arrayToCheck)
  max_value <- mean(arrayToCheck)+3*sd(arrayToCheck)
  newDatasetWOutliers <- dataset[(arrayToCheck<=min_value | arrayToCheck>=max_value),]
  outliers_count <- nrow(dataset)-nrow(newDatasetWOutliers)
  return (newDatasetWOutliers)
}
##'Fare' --> OK
seeOutlierValues(titanic, titanic$Fare)
## 'SibSp' & 'Parch' --> Join in 'Family_size'
seeOutlierValues(titanic, titanic$SibSp)
seeOutlierValues(titanic, titanic$Parch)
titanic$Family_size <- titanic$SibSp + titanic$Parch
seeOutlierValues(titanic, titanic$Family_size)
### Discard SibSp & Parch
titanic["SibSp"] <- NULL
titanic["Parch"] <- NULL
## 'Age' --> Remove outliers
seeOutlierValues(titanic, titanic$Age)
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

# ---Export dataset---
str(titanic)
## change types: Pclass, Survived & Title as factor
titanic$Survived <- as.factor(titanic$Survived)
titanic$Pclass <- as.factor(titanic$Pclass)
titanic$Title <- as.factor(titanic$Title)
str(titanic)
write.csv(titanic, "data/titanic_train_transformed.csv")

# ---Data Analysis---
## Groups
### by 'Pclass'
levels(titanic$Pclass)
t_pclass_1 <- titanic %>% filter(Pclass == "1")
t_pclass_2 <- titanic %>% filter(Pclass == "2")
t_pclass_3 <- titanic %>% filter(Pclass == "3")
### by 'Title'
levels(titanic$Title)
t_title_Master <- titanic %>% filter(Title == "Master")
t_title_Miss <- titanic %>% filter(Title == "Miss")
t_title_Mr <- titanic %>% filter(Title == "Mr")
t_title_Mrs <- titanic %>% filter(Title == "Mrs")
t_title_Noble_female <- titanic %>% filter(Title == "Noble female")
t_title_Noble_male <- titanic %>% filter(Title == "Noble male")
### by 'Sex'
levels(titanic$Sex)
t_sex_male <- titanic %>% filter(Sex == "male")
t_sex_female <- titanic %>% filter(Sex == "female")
## Normality - homogenity
shapiro.test(titanic$Age)
### ---> From the output, the p-value > 0.05 implying that the distribution 
### of the data are not significantly different from normal distribution. 
### In other words, we can assume the normality. http://www.sthda.com/english/wiki/normality-test-in-r
### See normality of 'Age' by graphs
shapiro.test(titanic$Fare)
### See normality by graphs
shapiro.test(titanic$Family_size)
### See normality by graphs

