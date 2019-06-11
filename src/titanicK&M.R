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
titanic <- titanic.original[,-which(names(titanic.original) %in% c("Embarked","Ticket","PassengerId"))] #Així queda més clar quines columnes es descarten OK

## extract passenger names saving only the titles
titanic$Title <- as.factor(gsub('(.*, )|(\\..*)', '', titanic$Name))
titanic["Name"] <- NULL 

## combines some passenger titles
## (from tnikaggle user in kaggle --> https://www.kaggle.com/tysonni/extracting-passenger-titiles-in-r)
## and Narcel Reedus September 14, 2017 --> https://rpubs.com/Nreedus/Titanic
#install.packages("dplyr") #<-- If library doesn't be installed previously delete the comment
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


##'Fare'
seeOutlierValues(titanic, titanic$Fare)

## 'SibSp' & 'Parch'
titanic$Family_size <- titanic$SibSp + titanic$Parch
seeOutlierValues(titanic, titanic$Family_size)

### Discard SibSp & Parch
titanic["SibSp"] <- NULL
titanic["Parch"] <- NULL

## 'Age' 
seeOutlierValues(titanic, titanic$Age)
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
seeGroupStatics <- function(resultArray, categoricalArray){
  aggregate(resultArray, list(categoricalArray), FUN = function(x) c(mean = mean(x), count = length(x) ))
}
# Change 'Survived' to integer to compute its mean
# Substract 1 because the values get 1 and 2 in the process
titanic$Survived <- as.integer(titanic$Survived)-1

## Groups
### by 'Pclass'
levels(titanic$Pclass)
seeGroupStatics(titanic$Survived, titanic$Pclass)
t_pclass_1 <- titanic %>% filter(Pclass == "1")
t_pclass_2 <- titanic %>% filter(Pclass == "2")
t_pclass_3 <- titanic %>% filter(Pclass == "3")

### by 'Title'
levels(titanic$Title)
seeGroupStatics(titanic$Survived, titanic$Title)
t_title_Master <- titanic %>% filter(Title == "Master")
t_title_Miss <- titanic %>% filter(Title == "Miss")
t_title_Mr <- titanic %>% filter(Title == "Mr")
t_title_Mrs <- titanic %>% filter(Title == "Mrs")
t_title_Noble_female <- titanic %>% filter(Title == "Noble female")
t_title_Noble_male <- titanic %>% filter(Title == "Noble male")

### by 'Sex'
levels(titanic$Sex)
seeGroupStatics(titanic$Survived, titanic$Sex)
t_sex_male <- titanic %>% filter(Sex == "male")
t_sex_female <- titanic %>% filter(Sex == "female")

##by 'Age'
max(titanic$Age)
titanic$AgeCategorical<-cut(titanic$Age, seq(0,70,5))
seeGroupStatics(titanic$Survived, titanic$AgeCategorical)
titanic$AgeCategorical <- cut(titanic$Age, breaks=c(0, 15, 35, 50, 70), labels=c("Youth","Young Adult","Adult","Senior"))
seeGroupStatics(titanic$Survived, titanic$AgeCategorical)
t_age_youth <- titanic %>% filter(AgeCategorical == "Youth")
t_age_youngAdult <- titanic %>% filter(AgeCategorical == "Young Adult")
t_age_adult <- titanic %>% filter(AgeCategorical == "Adult")
t_age_senior <- titanic %>% filter(AgeCategorical == "Senior")

## Normality
### GGPlot2 library for plots
#install.packages("ggplot2") #<-- If library doesn't be installed previously delete the comment
library(ggplot2)
### See normality of 'Age' by plot
ggplot(titanic, aes(x=Age)) + 
  geom_histogram(aes(y=..density..), binwidth = 6, colour="black", fill="lightblue")
### Shapiro test 'Age'
shapiro.test(titanic$Age)
### Normality of 'Fare' by plot
ggplot(titanic, aes(x=Fare)) + 
  geom_histogram(aes(y=..density..), binwidth = 40, colour="black", fill="lightblue")
### Shapiro test 'Fare'
shapiro.test(titanic$Fare)
### Normality of 'Family_size' by plot
ggplot(titanic, aes(x=Family_size)) + 
  geom_histogram(aes(y=..density..), binwidth = 0.8, colour="black", fill="lightblue")
### Shapiro test 'Family_size'
shapiro.test(titanic$Family_size)

## homocedasticity
# install.packages("car") #<-- If library doesn't be installed previously delete the comment
library(car)
fligner.test(as.integer(Survived) ~ Age, data = titanic)
fligner.test(as.integer(Survived) ~ Fare, data = titanic) # <- Homogeneïtat
fligner.test(as.integer(Survived) ~ Family_size, data = titanic)

## Statistical tests application
###Correlations
####Between Survived and the other attributes
for (i in colnames(titanic)[2:8]){
  pvalue = chisq.test(titanic[i], titanic$Survived)$p.value
  cat("Pvalue for", i, "vs Survived =", pvalue)
  cat("\n")
}
####Correlation between TOP attributes
ps = chisq.test(titanic$Pclass, titanic$Sex)$p.value
pt = chisq.test(titanic$Pclass, titanic$Title)$p.value
st = chisq.test(titanic$Sex, titanic$Title)$p.value
cormatrix = matrix(c(1, ps, pt,
                     ps, 1, st,
                     pt, st, 1),
                   3, 3, byrow = TRUE)

row.names(cormatrix) = colnames(cormatrix) = c("Pclass", "Sex", "Title")
cormatrix
## Non categorical attributes
corr_matrix_non_categorical <- matrix(nc = 2, nr = 0)
colnames(corr_matrix_non_categorical) <- c("estimate", "p-value")
for (i in 2:(ncol(titanic))) {
  if (is.integer(titanic[,i]) | is.numeric(titanic[,i])) {
    pearson_test = cor.test(titanic[,i],
                             titanic$Survived,
                             method = "pearson")
    corr_coef = pearson_test$estimate
    p_val = pearson_test$p.value
    # Add row to matrix
    pair = matrix(ncol = 2, nrow = 1)
    pair[1][1] = corr_coef
    pair[2][1] = p_val
    corr_matrix_non_categorical <- rbind(corr_matrix_non_categorical, pair)
    rownames(corr_matrix_non_categorical)[nrow(corr_matrix_non_categorical)] <- colnames(titanic)[i]
  } 
}

corr_matrix_non_categorical

###Regression
###TOP variables regression
sex_Pclass_lm <- lm(Survived~Sex*Pclass, data=titanic)
cat("r2 regression value:", summary(sex_Pclass_lm)$r.squared)
coef(sex_Pclass_lm)

#### Hypothesis contrast for 'Sex'
titanic_male_survived <-  titanic[titanic$Sex == "male",]$Survived
titanic_female_survived <-  titanic[titanic$Sex == "female",]$Survived
t.test(titanic_male_survived, titanic_female_survived, alternative = "less")


## Representació dels resultats
###Group by 'Sex' and 'Pclass'
titanic_by_sex_pclass <- summarize(
                         group_by(titanic, Sex, Pclass), 
                         Survived = mean(Survived)
                         )
titanic_by_sex_pclass
####plot
titanic_by_sex_pclass.plot <-ggplot(titanic_by_sex_pclass,aes(x =Pclass,y =Survived,color =Sex,group =Sex))
titanic_by_sex_pclass.plot+geom_point()+geom_line()
###Group by 'Age'
titanic_by_age <- summarize(
                  group_by(titanic, AgeCategorical), 
                  Survived = mean(Survived)
                  )
titanic_by_age
####plot
titanic_by_age.plot <-ggplot(titanic_by_age,aes(x =AgeCategorical,y =Survived))
titanic_by_age.plot+geom_bar(stat="identity", position="identity")
###Group by 'Title' and plot
titanic_by_title <- summarize(
                    group_by(titanic, Title), 
                    Survived = mean(Survived)
                    )
titanic_by_title.plot <-ggplot(titanic_by_title,aes(x =Title,y =Survived))
titanic_by_title.plot+geom_bar(stat="identity", position="identity")
#### See noble females
titanic %>%
  filter(Sex == "female" & Title == "Noble female")

# Contributions
library(knitr)
contrib_table <- matrix(c("MFV, JSS", "MFV, JSS", "MFV, JSS"), 3, 2, byrow = TRUE)
row.names(contrib_table) <- c("Investigació prèvia","Redacció de les respostes","Desenvolupament codi")
kable( contrib_table , caption = "Contribucions dels integrants"
       , col.names = c("Contribucions", "Firma")
       , row.names = TRUE
       , digits = 1
       , format.args = list( decimal.mark = ",")
)
