####################################
# Lung Cancer Risk Prediction in R #
# Author: Sabrina Frank            #
# Date: 15.01.2024                 #
# Project: Big Data Management     #
####################################


##########################################
# Installieren von benötigten Bibliotheken
# ToDo: rausfinden für was Packages sind und eventuell welches rausschmeißen, die nicht gebraucht werden


# install.packages("data.table")
library(data.table)
# install.packages("GGally")
library(GGally) #Visualisierung Pairplots
# install.packages("caret")
library(caret) #Confusionmatrix
# install.packages("mlbench")
library(mlbench) #Confusionmatrix
# install.packages("magrittr")
library(magrittr)
# install.packages("tidyverse")
library(tidyverse)
# install.packages("party")
library(party)
# install.packages("vcd")
library(vcd) # für Korrelationsanalyse für nicht numerische Features
# install.packages("svglite")
library(svglite) # für ggsave Funktion

## Reproduzierbarkeit des Codes
set.seed(1)




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

### Kopie erstellen
lungcancer = copy(lungcancer_raw) 



##########################################
# Säubern des Datensatzes
# ToDo: OccuPational fehler korrigieren Column chronic Lung Disease name

## Patient ID und Index entfernen
lungcancer[,(names(lungcancer)[0:2]):=NULL]

## Age in Integer umwandeln
lungcancer[, Age := as.integer(Age)]

## Gender umcodieren: 1=Male, 2=Female
lungcancer[, Gender := as.character(Gender)][Gender == "1", Gender := "M"]
lungcancer[, Gender := as.character(Gender)][Gender == "2", Gender := "F"]

## Für Visualisierung abspeichern
lungcancer_bar = copy(lungcancer) 

## Faktoren einführen 
lungcancer[,2:24] <- lapply(lungcancer[,2:24],as.factor)
lungcancer[,3:23] <- lapply(lungcancer[,3:23],ordered)
lungcancer[, ("Level") := ordered(get("Level"), levels = c("Low", "Medium", "High"))]

## Bereinigung checken
levels(lungcancer$Level)
str(lungcancer)

## In besäuberte data.table abspeichern
lungcancer_clean = copy(lungcancer) 




##########################################
# Visualisierung des Datensatzes


## Übersichtsgrafik Pairplots aller Features
ggpairs(lungcancer_clean)
### Speichern des Plots
ggsave(filename = "pairplot.svg",
  plot = last_plot(),
  device = "svg",
  path = "./plots",
  scale = 1,
  width = 11000,
  height = 11000,
  units = "px",
  dpi = 300,
  limitsize = FALSE,
)

## Barplots für kategorische Spalten
lungcancer_bar <- lungcancer_bar[, !c("Age", "Gender")]
lungcancer_bar %>% pivot_longer(!Level, values_to = "value") %>%
                  # ggplot(aes(x = value, fill = factor(Level))) +
                  ggplot(aes(x=factor(value), fill=ordered(Level, c("Low", "Medium", "High")))) +
                  scale_fill_manual(values=c("burlywood1", "coral1", "darkred")) +
                  geom_bar(position="fill", alpha=.7)+
                  theme_minimal() +
                  labs(fill="Lungenkrebs:") +
                  facet_wrap(~name, scales="free")
### Speichern des Plots
ggsave(filename = "barplot.svg",
       plot = last_plot(),
       device = "svg",
       path = "./plots",
       scale = 1,
       width = 3000,
       height = 3000,
       units = "px",
       dpi = 300,
       limitsize = FALSE,
)

#Check Balance der Target-Klassen #Frage: unbalanciert?
ggally_barDiag(lungcancer_clean, 
               mapping = ggplot2::aes(x = Level), 
               rescale = FALSE)
### Speichern des Plots
ggsave(filename = "target_balance.svg",
       plot = last_plot(),
       device = "svg",
       path = "./plots",
       scale = 1,
       width = 1000,
       height = 800,
       units = "px",
       dpi = 300,
       limitsize = FALSE,
)




##########################################
# Preprocessing


## Featureselektion und -reduktion
## https://www.r-bloggers.com/2022/02/beginners-guide-to-machine-learning-in-r-with-step-by-step-tutorial/
## Fragen: Reduktion nötig? guter Ansatz?

### Korrelation einzelner Spalten mit dem Level des Lungenkrebs -> je höher desto wichtiger? Fragen
corr_level <- lungcancer_clean %>% 
              mutate_if(is.factor, as.numeric) %>%
              cor() %>% 
              as.data.frame() %>% 
              select(Level) %>% 
              arrange(-Level)

### Korrelation der Features untereinander
corr <- lungcancer_clean %>% 
        mutate_if(is.factor, as.numeric) %>%
        cor() %>% 
        as.data.frame()

### Korrelationsmatrix (Dimensions: 2300x1000)
corr %>% mutate(var2=rownames(.)) %>%
         pivot_longer(!var2, values_to = "value") %>%
         ggplot(aes(x=name, y=var2, fill = abs(value), label = round(value,2))) +
         geom_tile() + geom_label() + xlab("") + ylab("") +
         ggtitle("Korrelationsmatrix der Prediktoren") +
         labs(fill="Korrelation\n(absolut):")
#### Speichern der Korrelationsmatrix
ggsave(filename = "correlation_matrix.svg",
       plot = last_plot(),
       device = "svg",
       path = "./plots",
       scale = 1,
       width = 10000,
       height = 5500,
       units = "px",
       dpi = 300,
       limitsize = FALSE,
)

### Features finden, die hoch korreliert sind (>0.8) und deren Indices verwerfen
highly_corr <- caret::findCorrelation(cor(corr), cutoff=0.8)
highly_corr
### Hochkorrelierte Spalten entfernen
lungcancer_slct <- lungcancer_clean[,(names(lungcancer_raw)[highly_corr]):=NULL]



### ToDo mit Cramer wegen Faktoren Features? # Frage ob das besser ist als Korrelation 
### Für Features
lungcancer_cramer <- lungcancer_clean[, !c("Age","Level")]
assoc <- table(lungcancer_cramer)
### Calculate Cramer's V
### assocstats(lungcancer_cramer$cramer)
assocstats(assoc$cramer) 


## Normalisierung: Min-Max Scalierung der Spalte Age #Frage: vor split okay? 
## ToDo: wie?                 
lungcancer_df["Age"]






##########################################
# Splitten des Datensatzes in Training und Testset

split <- sample(nrow(lungcancer_dt),nrow(lungcancer_dt)*0.8) #ToDo schauen, ob 80/20 gut ist und vllt schreibweise aus corrlink
train <- lungcancer_dt[split,-c(1:2)] #todo c vielleicht wegnehmen
test <- lungcancer_dt[-split,-c(1:2)] #todo c vielleicht wegnehmen
dim(train)
dim(test)




##########################################
# Encoden und Labeln, preprocessing

# k-folf stratified

## Label Encoding und so



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



## ToDO vielleicht machen
### Piping https://www.r-bloggers.com/2021/05/the-new-r-pipe/
### lungcancer <- lungcancer_raw |>

### R Dashboard
### https://www.r-bloggers.com/2021/11/top-r-packages-for-visualizing-table-data-make-stunning-tables-in-minutes/