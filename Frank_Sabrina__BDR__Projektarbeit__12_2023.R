####################################
# Lung Cancer Risk Prediction in R #
# Author: Sabrina Frank            #
# Date: 15.01.2024                 #
# Project: Big Data Management     #
####################################


##########################################
# Installieren von benötigten Bibliotheken


# install.packages("data.table")
library(data.table)
# install.packages("GGally")
library(GGally) #Visualisierung Pairplots
# install.packages("caret")
library(caret) #Confusionmatrix ToDo
# install.packages("mlbench")
library(mlbench) #Confusionmatrix
# install.packages("utiml")
library(utiml) #paper zitieren
# install.packages("magrittr")
library(magrittr)

# install.packages("tidyverse")
library(tidyverse)
# install.packages("party")
library(party)


# install.packages("vcd")
library(vcd) # für Korrelationsanalyse für nicht numerische Features




##########################################
# Importieren und Sichten des Datensatzes


## Import mit fread
lungcancer_raw <- fread(file = "./data/lung_cancer_patient_datasets.csv")
class(lungcancer_raw)

## Sichten des Datensatzes
lungcancer_raw
dimensions <- dim(lungcancer_raw)
str(lungcancer_raw)
summary(lungcancer_raw)
###Check NAs values
lungcancer_raw %>% sapply(function(x)sum(is.na(x)))

## Reproduzierbarkeit des Codes
set.seed(1)


##########################################
# Säubern des Datensatzes


## Patient ID und Index entfernen
lungcancer_raw[,(names(lungcancer_raw)[0:2]):=NULL]

## Age in Int umwandeln
lungcancer_raw[, Age := as.integer(Age)]

## Gender umcodieren 1=Male, 2=Female
lungcancer_raw[, Gender := as.character(Gender)][Gender == "1", Gender := "M"]
lungcancer_raw[, Gender := as.character(Gender)][Gender == "2", Gender := "F"]

## Faktoren für Level einführen 
lungcancer_raw[,2:24] <- lapply(lungcancer_raw[,2:24],as.factor) #ToDo: schauen, ob mit Gender als Faktor geht
lungcancer_raw[,3:23] <- lapply(lungcancer_raw[,3:23],ordered)

### Faktorordnung der Level-Spalte
lungcancer_raw[, ("Level") := ordered(get("Level"), levels = c("Low", "Medium", "High"))]

### Bereinigung checken
levels(lungcancer_raw$Level)
str(lungcancer_raw)

### in lungcancer_dt abspeichern
lungcancer_dt = copy(lungcancer_raw) 




##########################################
# Visualisierung des Datensatzes


ggpairs(lungcancer_dt) #ToDo zu groß
# https://ggobi.github.io/ggally/articles/ggpairs.html
# https://stackoverflow.com/questions/48123611/using-ggpairs-on-a-large-dataset-with-many-variables


# boxplot()
# hist()

#Check Balance der Target Klassen
ggally_barDiag(lungcancer_dt, 
               mapping = ggplot2::aes(x = Level), 
               rescale = FALSE)
# Frage: unbalanciert?




##########################################
# Reduction (Korrelation suchen um vllt. Features zu entfernen/ Feature Selection)
# https://www.r-bloggers.com/2022/02/beginners-guide-to-machine-learning-in-r-with-step-by-step-tutorial/


corr_level <- lungcancer_dt %>% 
              mutate_if(is.factor, as.numeric) %>%
              cor() %>% 
              as.data.frame() %>% 
              select(Level) %>% 
              arrange(-Level)

corr <- lungcancer_dt %>% 
        mutate_if(is.factor, as.numeric) %>%
        cor() %>% 
        as.data.frame()

corr %>% mutate(var2=rownames(.)) %>%
  pivot_longer(!var2, values_to = "value") %>%
  ggplot(aes(x=name, y=var2, fill = abs(value), label = round(value,2))) +
  geom_tile() + geom_label() + xlab("") + ylab("") +
  ggtitle("Korrelationsmatrix der Prediktoren") +
  labs(fill="Korrelation\n(absolut):")
## Dimensions: Width 2300, Height 1000

# find attributes that are highly corrected (ideally >0.75) -> verwerfen
highly_corr <- findCorrelation(corr, cutoff=0.5)

## ToDo mit Cramer wegen Faktoren Features?
# Für Features
assoc_dt <- table(lungcancer_dt[2:24])
# Calculate Cramer's V
assocstats(lungcancer_dt$cramer)
assocstats(assoc_dt) 


##barplots für categorische variablen

# ToDo: X,y aufspalten??
lungcancer_target <- lungcancer_dt$Level
lungcancer_features <- lungcancer_dt |> .[0:23] |> 


##########################################
# splitten des Datensatzes in Training und Test

split <- sample(nrow(lungcancer_dt),nrow(lungcancer_dt)*0.8) #ToDo schauen, ob 80/20 gut ist und vllt schreibweise aus corrlink
train <- lungcancer_dt[split,-c(1:2)] #todo c vielleicht wegnehmen
test <- lungcancer_dt[-split,-c(1:2)] #todo c vielleicht wegnehmen
dim(train)
dim(test)

# mit utiml
ds <- create_holdout_partition(new.toyml, c(train=0.7, test=0.3), method="stratified")
model <- br(ds$train, "RF")



##########################################
# Encoden und Labeln, preprocessing

### Normalisierung der Spalte Age
train <- normalize_mldata(mdata) #Frage: vor split? oder nacher? 

# k-folf stratified

## Label Encoding und so

# remove_skewness_labels(mdata) #evtl. checken, ob ich das brauch



##################################
# Machine Learning
# https://www.kaggle.com/code/takkimsncn/lung-cancer-prediction/notebook

# Faktorisierung klappt vielleicht nicht mit ML model

# which model to use: https://de.mathworks.com/campaigns/offers/next/choosing-the-best-machine-learning-classification-model-and-avoiding-overfitting.html
# erst mal naive bayes probieren
# bagged decision tree
# svm gut, aber vllt. rechenintensiv, braucht viel tuning

# k-fold cross val?

measures <- c("macro-F1", "micro-F1")
algorithm <- "SVM"


# Rank Features By Importance
# 
# The importance of features can be estimated from data by building a model. Some methods like decision trees have a built in mechanism to report on variable importance. For other algorithms, the importance can be estimated using a ROC curve analysis conducted for each attribute.
# 
# The example below loads the Pima Indians Diabetes dataset and constructs an Learning Vector Quantization (LVQ) model. The varImp is then used to estimate the variable importance, which is printed and plotted. It shows that the glucose, mass and age attributes are the top 3 most important attributes in the dataset and the insulin attribute is the least important.
# Rank features by importance using the caret r package
# R
# # ensure results are repeatable
# set.seed(7)
# # load the library
# library(mlbench)
# library(caret)
# # load the dataset
# data(PimaIndiansDiabetes)
# # prepare training scheme
# control <- trainControl(method="repeatedcv", number=10, repeats=3)
# # train the model
# model <- train(diabetes~., data=PimaIndiansDiabetes, method="lvq", preProcess="scale", trControl=control)
# # estimate variable importance
# importance <- varImp(model, scale=FALSE)
# # summarize importance
# print(importance)
# # plot importance
# plot(importance)

###################################
# Prediction and Confusionmatrix

prediction <- predict(MLmodel,newdata=test[-24])
confusionMatrix(prediction,test$Level)

# mit utiml
predictions <- predict(model, ds$test)
head(predictions)
results <- multilabel_evaluate(ds$test, predictions, c("example-based", "macro-F1"))
round(results, 4)

cm <- multilabel_confusion_matrix(ds$test, predictions)


## ToDO vielleicht machen
### Piping https://www.r-bloggers.com/2021/05/the-new-r-pipe/
### lungcancer <- lungcancer_raw |>

### R Dashboard
### https://www.r-bloggers.com/2021/11/top-r-packages-for-visualizing-table-data-make-stunning-tables-in-minutes/