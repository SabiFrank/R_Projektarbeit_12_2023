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

# Für Machine Learning Algorithmus
# install.packages("randomForest")
library(randomForest)

## Reproduzierbarkeit des Codes
set.seed(1)




##########################################
# Importieren und Sichten des Datensatzes


## Import mit fread
lungcancer_raw <- fread(file = "./data/lung_cancer_patient_datasets.csv")
class(lungcancer_raw)

## Sichten des Datensatzes
lungcancer_raw
dim(lungcancer_raw)
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

## Check Balance der Target-Klassen #Frage: unbalanciert?
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
# Featureselektion und -reduktion
# https://www.r-bloggers.com/2022/02/beginners-guide-to-machine-learning-in-r-with-step-by-step-tutorial/
# Fragen: Reduktion nötig? guter Ansatz?


## Korrelation einzelner Spalten mit dem Level des Lungenkrebs -> je höher desto wichtiger? Fragen
corr_level <- lungcancer_clean %>% 
              mutate_if(is.factor, as.numeric) %>%
              cor() %>% 
              as.data.frame() %>% 
              select(Level) %>% 
              arrange(-Level)

## Korrelation der Features untereinander
corr <- lungcancer_clean %>% 
        mutate_if(is.factor, as.numeric) %>%
        cor() %>% 
        as.data.frame()

## Korrelationsmatrix (Dimensions: 2300x1000)
corr %>% mutate(var2=rownames(.)) %>%
         pivot_longer(!var2, values_to = "value") %>%
         ggplot(aes(x=name, y=var2, fill = abs(value), label = round(value,2))) +
         geom_tile() + geom_label() + xlab("") + ylab("") +
         ggtitle("Korrelationsmatrix der Prediktoren") +
         labs(fill="Korrelation\n(absolut):")
### Speichern der Korrelationsmatrix
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

## Features finden, die hoch korreliert sind (>0.8) und deren Indices verwerfen
highly_corr <- caret::findCorrelation(cor(corr), cutoff=0.8)
highly_corr
## Hochkorrelierte Spalten entfernen
lungcancer_slct <- lungcancer_clean[, (names(lungcancer_raw)[highly_corr]):=NULL]



## ToDo mit Cramer wegen Faktoren Features? # Frage ob das besser ist als Korrelation 
## Für Features
lungcancer_cramer <- lungcancer_clean[, !c("Age","Level")]
assoc <- table(lungcancer_cramer)
## Calculate Cramer's V
## assocstats(lungcancer_cramer$cramer)
assocstats(assoc$cramer) 




##########################################
# Splitten des Datensatzes in Training und Testset


## Splitten zu 80/20 und Conversion zu data frame für preprocessing
split <- sample(1:nrow(lungcancer_slct), as.integer(0.8*nrow(lungcancer_slct)), F)
train <- as.data.frame(lungcancer_slct[split,])
test <- as.data.frame(lungcancer_slct[-split,])

## Checken, ob Dimensionen erhalten sind
dim(train)
dim(test)
class(train)
class(test)




##########################################
# Preprocessing
# ToDo: Muss ich hier noch mehr machen?
# Frage: Muss ich nach skewedness schauen? https://www.r-bloggers.com/2015/07/how-to-make-a-rough-check-to-see-if-your-data-is-normally-distributed/

## Funktion für Preprocessing, das auf die Daten angewandt werden soll
preprocessing <- function(df){
  
  # Normalisierung: Min-Max-Scalierung der Spalte Age
  process <- preProcess(df["Age"], method = c("range"))
  df["Age"] <- predict(process, df["Age"])
  
  return(df[, names(df)!="Level"])
}

## Anwenden der preprocessing Funktion auf Train und Test Datenset
x_train <- preprocessing(train)
x_test <- preprocessing(test)
y_train <- train[, "Level"]
y_test <- test[, "Level"]




##################################
# Machine Learning
# https://www.kaggle.com/code/takkimsncn/lung-cancer-prediction/notebook
# https://www.r-bloggers.com/2022/02/beginners-guide-to-machine-learning-in-r-with-step-by-step-tutorial/


model <- caret::train(x_train,
                      y_train,
                      method = "rf",
                      tuneGrid = expand.grid(mtry = seq(5, ncol(x_train), by = 5)),
                      trControl = trainControl(method = "cv", number = 5, verboseIter = T))
model
# Random Forest 
# 
# 800 samples
# 13 predictor
# 3 classes: 'Low', 'Medium', 'High' 
# 
# No pre-processing
# Resampling: Cross-Validated (5 fold) 
# Summary of sample sizes: 640, 641, 641, 640, 638 
# Resampling results across tuning parameters:
#   
#   mtry  Accuracy  Kappa
# 5    1         1    
# 10    1         1    
# 
# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was mtry = 5.

print(model)

plot(varImp(model), 
     main = "Feature-Wichtigkeit des RF Models",
     xlab = "Wichtigkeit")


train_accuracy <- max(model$results$Accuracy)
train_accuracy



###################################
# Prediction and Confusionmatrix

## Vorhersage auf Grundlage der Testdaten
prediction <- predict(model, newdata = x_test)

## Confusionsmatrix
confusionMatrix(prediction, y_test)


## ToDo: tut nicht
precision(prediction, y_test)
recall(prediction, y_test)
F_meas(prediction, y_test)


propability <- predict(model, type = "prob")
library(ROCR)
perf = prediction(pred1[,2], mydata$Creditability)
# 1. Area under curve
auc = performance(perf, "auc")
auc
# 2. True Positive and Negative Rate
tpn = performance(perf, "tpr","fpr")
# 3. Plot the ROC curve
plot(pred3,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")
