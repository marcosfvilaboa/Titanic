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

#titanic <- titanic.original[, c(2:8, 10, 11)]
titanic <- titanic.original[,-which(names(titanic.original) %in% c("Embarked","Ticket","PassengerId"))] #Així queda més clar quines columnes es descarten OK

##>>>>>> OK -> Proposo crear l'att Title i no sobrescriure el Name. Despr?s borrem el Name comentant que no té sentit guardar-lo. Ho dic perquè queda més clar què estem fent

## extract passenger names saving only the titles
titanic$Title <- as.factor(gsub('(.*, )|(\\..*)', '', titanic$Name))
titanic["Name"] <- NULL #La variable Name ja no t? cap valor

### change 'Name' column name to 'Title'
#names(titanic)[names(titanic) == "Name"] <- "Title"



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
##>>>>>> Em sembla bastant inteligent fer així pero ojo perquè d'alguna manera aquí estem juntant sex + class
##>>>> La idea d'això és per posar el títol corresponent sogons el sexe per a Doctor-Noble (female-male)
##>>>> Més aviat estic associant el títol amb el sexe.

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


##'Fare' --> OK
seeOutlierValues(titanic, titanic$Fare)

## 'SibSp' & 'Parch' --> Join in 'Family_size'
##>>>>>>>> Si els ajuntem no t? molt sentit mirar els outliers per separat
##>>>> vista la justificació que realitzes al Rmd i em sembla perfecte.
#seeOutlierValues(titanic, titanic$SibSp) 
#seeOutlierValues(titanic, titanic$Parch)
titanic$Family_size <- titanic$SibSp + titanic$Parch
seeOutlierValues(titanic, titanic$Family_size)
#No treure els Outliers, el que tenen famílies més petites són els que sobreviuen

### Discard SibSp & Parch
titanic["SibSp"] <- NULL
titanic["Parch"] <- NULL

## 'Age' --> Remove outliers
seeOutlierValues(titanic, titanic$Age)
titanic <- removeOutlierValues(titanic, titanic$Age)

# ---Export dataset---
str(titanic)
## change types: Pclass, Survived & Title as factor
titanic$Survived <- as.factor(titanic$Survived) 
#>>>> Si l'utilitzem com a int tenim la possibilitat d'utilitzar mean
#>>>> Tens raó, pot ser interessant. Però Survived és una variable factor.
#>>>> De fet, es tracta de la variable de classe.
#>>>> Potser es més "correcte" desar-la com a factor i si cal algun càlcul amb
#>>>> 'mean' fer-lo amb as.integer
titanic$Pclass <- as.factor(titanic$Pclass)
titanic$Title <- as.factor(titanic$Title)
str(titanic)
write.csv(titanic, "data/titanic_train_transformed.csv")

# ---Data Analysis---
seeGroupStatics <- function(resultArray, categoricalArray){
  aggregate(resultArray, list(categoricalArray), FUN = function(x) c(mean = mean(x), count = length(x) ))
}
# Pels seguents càlculs la passem a integer tot i ser factor ja que ens interessa la mitja
titanic$Survived <- as.integer(titanic$Survived)
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
### Sembla seguir distribució normal
### Shapiro test 'Age'
shapiro.test(titanic$Age)
### Resulta en ditribució no normal
### ---> If the p-value > 0.05 implying that the distribution 
### of the data are not significantly different from normal distribution. 
### In other words, we can assume the normality. http://www.sthda.com/english/wiki/normality-test-in-r
### Normality of 'Fare' by plot
ggplot(titanic, aes(x=Fare)) + 
  geom_histogram(aes(y=..density..), binwidth = 40, colour="black", fill="lightblue")
### Distribució no normal amb cua a la dreta
### Shapiro test 'Fare'
shapiro.test(titanic$Fare)
### Resulta en distribució no normal
### Normality of 'Family_size' by plot
ggplot(titanic, aes(x=Family_size)) + 
  geom_histogram(aes(y=..density..), binwidth = 0.8, colour="black", fill="lightblue")
### Distribució no normal amb cua a la dreta
### Shapiro test 'Family_size'
shapiro.test(titanic$Family_size)
### Resulta en distribució no normal

## homocedasticity
### --> igualdad de varianzas entre los grupos que se van a comparar
### test de Levene, cuando los datos siguen una distribución normal, 
### test de Fligner-Killeen, alternativa no paramétrica, cuando los datos no cumplen con la condición de normalidad
# install.packages("car") #<-- If library doesn't be installed previously delete the comment
library(car)
fligner.test(as.integer(Survived) ~ Age, data = titanic)
### Puesto que obtenemos un p-valor superior a 0,05, aceptamos la hipótesis de que las varianzas
### de ambas muestras son homogéneas
fligner.test(as.integer(Survived) ~ Fare, data = titanic) # <- Homogeneïtat
fligner.test(as.integer(Survived) ~ Family_size, data = titanic)
### se rechaza la hipótesis nula de homocedasticidad y se concluye que la variable Survived
### presenta varianzas estadísticamente diferentes para Family_size

## Aplicació de proves estadístiques

