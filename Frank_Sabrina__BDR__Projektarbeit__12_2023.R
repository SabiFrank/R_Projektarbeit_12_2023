##############################################
# Lung Cancer Risk Prediction in R           #
# Author: Sabrina Frank                      #
# Date: 15.01.2024                           #
# Project: Big Data Management mit SQL und R #
##############################################


##########################################
# Installieren von benötigten Bibliotheken


## Pipes
# install.packages("dplyr")
library(dplyr)

## Data frame handling und Datenmanipulierung
# install.packages("data.table")
library(data.table)
# install.packages("tidyr")
library(tidyr)

## Visualisierung
# install.packages("ggplot2")
library(ggplot2)
# install.packages("GGally")
library(GGally)
# install.packages("gridExtra")
library(gridExtra)
# install.packages("lattice")
library(lattice)

## Measure of association (Korrelationsmatrix für kategorische Daten)
# install.packages("ggcorrplot")
library(ggcorrplot) 

## Machine Learning
# install.packages("caret")
library(caret)

## Verwendete Packages
# install.packages("NCmisc")
library(NCmisc)

## Reproduzierbarkeit des Codes
set.seed(1)




##########################################
# Importieren und Sichten des Datensatzes


## Import mit fread
lungcancer_raw <- fread(file = "./data/lung_cancer_patient_datasets.csv")
class(lungcancer_raw)
# [1] "data.table" "data.frame"

## Sichten des Datensatzes
lungcancer_raw
dim(lungcancer_raw)
# [1] 1000   26

str(lungcancer_raw)
# Classes ‘data.table’ and 'data.frame':	1000 obs. of  26 variables:
#   $ index                   : int  0 1 2 3 4 5 6 7 8 9 ...
# $ Patient Id              : chr  "P1" "P10" "P100" "P1000" ...
# $ Age                     : int  33 17 35 37 46 35 52 28 35 46 ...
# $ Gender                  : int  1 1 1 1 1 1 2 2 2 1 ...
# $ Air Pollution           : int  2 3 4 7 6 4 2 3 4 2 ...
# $ Alcohol use             : int  4 1 5 7 8 5 4 1 5 3 ...
# $ Dust Allergy            : int  5 5 6 7 7 6 5 4 6 4 ...
# $ OccuPational Hazards    : int  4 3 5 7 7 5 4 3 5 2 ...
# $ Genetic Risk            : int  3 4 5 6 7 5 3 2 6 4 ...
# $ chronic Lung Disease    : int  2 2 4 7 6 4 2 3 5 3 ...
# $ Balanced Diet           : int  2 2 6 7 7 6 2 4 5 3 ...
# $ Obesity                 : int  4 2 7 7 7 7 4 3 5 3 ...
# $ Smoking                 : int  3 2 2 7 8 2 3 1 6 2 ...
# $ Passive Smoker          : int  2 4 3 7 7 3 2 4 6 3 ...
# $ Chest Pain              : int  2 2 4 7 7 4 2 3 6 4 ...
# $ Coughing of Blood       : int  4 3 8 8 9 8 4 1 5 4 ...
# $ Fatigue                 : int  3 1 8 4 3 8 3 3 1 1 ...
# $ Weight Loss             : int  4 3 7 2 2 7 4 2 4 2 ...
# $ Shortness of Breath     : int  2 7 9 3 4 9 2 2 3 4 ...
# $ Wheezing                : int  2 8 2 1 1 2 2 4 2 6 ...
# $ Swallowing Difficulty   : int  3 6 1 4 4 1 3 2 4 5 ...
# $ Clubbing of Finger Nails: int  1 2 4 5 2 4 1 2 6 4 ...
# $ Frequent Cold           : int  2 1 6 6 4 6 2 3 2 2 ...
# $ Dry Cough               : int  3 7 7 7 2 7 3 4 4 1 ...
# $ Snoring                 : int  4 2 2 5 3 2 4 3 1 5 ...
# $ Level                   : chr  "Low" "Medium" "High" "High" ...
# - attr(*, ".internal.selfref")=<externalptr> 

summary(lungcancer_raw)
# index        Patient Id             Age            Gender      Air Pollution   Alcohol use   
# Min.   :  0.0   Length:1000        Min.   :14.00   Min.   :1.000   Min.   :1.00   Min.   :1.000  
# 1st Qu.:249.8   Class :character   1st Qu.:27.75   1st Qu.:1.000   1st Qu.:2.00   1st Qu.:2.000  
# Median :499.5   Mode  :character   Median :36.00   Median :1.000   Median :3.00   Median :5.000  
# Mean   :499.5                      Mean   :37.17   Mean   :1.402   Mean   :3.84   Mean   :4.563  
# 3rd Qu.:749.2                      3rd Qu.:45.00   3rd Qu.:2.000   3rd Qu.:6.00   3rd Qu.:7.000  
# Max.   :999.0                      Max.   :73.00   Max.   :2.000   Max.   :8.00   Max.   :8.000  
# Dust Allergy   OccuPational Hazards  Genetic Risk  chronic Lung Disease Balanced Diet  
# Min.   :1.000   Min.   :1.00         Min.   :1.00   Min.   :1.00         Min.   :1.000  
# 1st Qu.:4.000   1st Qu.:3.00         1st Qu.:2.00   1st Qu.:3.00         1st Qu.:2.000  
# Median :6.000   Median :5.00         Median :5.00   Median :4.00         Median :4.000  
# Mean   :5.165   Mean   :4.84         Mean   :4.58   Mean   :4.38         Mean   :4.491  
# 3rd Qu.:7.000   3rd Qu.:7.00         3rd Qu.:7.00   3rd Qu.:6.00         3rd Qu.:7.000  
# Max.   :8.000   Max.   :8.00         Max.   :7.00   Max.   :7.00         Max.   :7.000  
# Obesity         Smoking      Passive Smoker    Chest Pain    Coughing of Blood    Fatigue     
# Min.   :1.000   Min.   :1.000   Min.   :1.000   Min.   :1.000   Min.   :1.000     Min.   :1.000  
# 1st Qu.:3.000   1st Qu.:2.000   1st Qu.:2.000   1st Qu.:2.000   1st Qu.:3.000     1st Qu.:2.000  
# Median :4.000   Median :3.000   Median :4.000   Median :4.000   Median :4.000     Median :3.000  
# Mean   :4.465   Mean   :3.948   Mean   :4.195   Mean   :4.438   Mean   :4.859     Mean   :3.856  
# 3rd Qu.:7.000   3rd Qu.:7.000   3rd Qu.:7.000   3rd Qu.:7.000   3rd Qu.:7.000     3rd Qu.:5.000  
# Max.   :7.000   Max.   :8.000   Max.   :8.000   Max.   :9.000   Max.   :9.000     Max.   :9.000  
# Weight Loss    Shortness of Breath    Wheezing     Swallowing Difficulty
# Min.   :1.000   Min.   :1.00        Min.   :1.000   Min.   :1.000        
# 1st Qu.:2.000   1st Qu.:2.00        1st Qu.:2.000   1st Qu.:2.000        
# Median :3.000   Median :4.00        Median :4.000   Median :4.000        
# Mean   :3.855   Mean   :4.24        Mean   :3.777   Mean   :3.746        
# 3rd Qu.:6.000   3rd Qu.:6.00        3rd Qu.:5.000   3rd Qu.:5.000        
# Max.   :8.000   Max.   :9.00        Max.   :8.000   Max.   :8.000        
# Clubbing of Finger Nails Frequent Cold     Dry Cough        Snoring         Level          
# Min.   :1.000            Min.   :1.000   Min.   :1.000   Min.   :1.000   Length:1000       
# 1st Qu.:2.000            1st Qu.:2.000   1st Qu.:2.000   1st Qu.:2.000   Class :character  
# Median :4.000            Median :3.000   Median :4.000   Median :3.000   Mode  :character  
# Mean   :3.923            Mean   :3.536   Mean   :3.853   Mean   :2.926                     
# 3rd Qu.:5.000            3rd Qu.:5.000   3rd Qu.:6.000   3rd Qu.:4.000                     
# Max.   :9.000            Max.   :7.000   Max.   :7.000   Max.   :7.000 

lungcancer_raw %>% sapply(function(x)sum(is.na(x)))
# index               Patient Id                      Age 
# 0                        0                        0 
# Gender            Air Pollution              Alcohol use 
# 0                        0                        0 
# Dust Allergy     OccuPational Hazards             Genetic Risk 
# 0                        0                        0 
# chronic Lung Disease            Balanced Diet                  Obesity 
# 0                        0                        0 
# Smoking           Passive Smoker               Chest Pain 
# 0                        0                        0 
# Coughing of Blood                  Fatigue              Weight Loss 
# 0                        0                        0 
# Shortness of Breath                 Wheezing    Swallowing Difficulty 
# 0                        0                        0 
# Clubbing of Finger Nails            Frequent Cold                Dry Cough 
# 0                        0                        0 
# Snoring                    Level 
# 0                        0 

## Kopie erstellen
lungcancer = copy(lungcancer_raw) 




##########################################
# Säubern des Datensatzes


## Patient ID und Index entfernen
lungcancer[,(names(lungcancer)[0:2]):=NULL]

## Spaltennamen korrigieren
colnames(lungcancer)[colnames(lungcancer) == "Clubbing of Finger Nails"] = "Finger Nails Clubbing"
colnames(lungcancer)[colnames(lungcancer) == "chronic Lung Disease"] = "Chronic Lung Disease"
colnames(lungcancer)[colnames(lungcancer) == "OccuPational Hazards"] = "Occupational Hazards"

## Age in Integer umwandeln
lungcancer[, Age := as.integer(Age)]

## Gender umcodieren: 1=Male, 2=Female
lungcancer[, Gender := as.character(Gender)][Gender == "1", Gender := "M"]
lungcancer[, Gender := as.character(Gender)][Gender == "2", Gender := "F"]

## Für Visualisierung abspeichern
lungcancer_bar = copy(lungcancer[, !c("Age", "Gender")]) 

# Schauen, welche ordinalen Werte es gibt
unique_data <- sort(unique(as.vector(as.matrix(as.data.frame(lungcancer[,2:24])))))
unique_data

## Faktoren einführen 
lungcancer[,2:24] <- lapply(lungcancer[,2:24],as.factor)
lungcancer[,3:23] <- lapply(lungcancer[,3:23],ordered)
lungcancer[, ("Level") := ordered(get("Level"), levels = c("Low", "Medium", "High"))]

## Bereinigung checken
levels(lungcancer$Level)
# [1] "Low"    "Medium" "High" 
str(lungcancer)
# Classes ‘data.table’ and 'data.frame':	1000 obs. of  24 variables:
#   $ Age                  : int  33 17 35 37 46 35 52 28 35 46 ...
# $ Gender               : Factor w/ 2 levels "F","M": 2 2 2 2 2 2 1 1 1 2 ...
# $ Air Pollution        : Ord.factor w/ 8 levels "1"<"2"<"3"<"4"<..: 2 3 4 7 6 4 2 3 4 2 ...
# $ Alcohol use          : Ord.factor w/ 8 levels "1"<"2"<"3"<"4"<..: 4 1 5 7 8 5 4 1 5 3 ...
# $ Dust Allergy         : Ord.factor w/ 8 levels "1"<"2"<"3"<"4"<..: 5 5 6 7 7 6 5 4 6 4 ...
# $ Occupational Hazards : Ord.factor w/ 8 levels "1"<"2"<"3"<"4"<..: 4 3 5 7 7 5 4 3 5 2 ...
# $ Genetic Risk         : Ord.factor w/ 7 levels "1"<"2"<"3"<"4"<..: 3 4 5 6 7 5 3 2 6 4 ...
# $ Chronic Lung Disease : Ord.factor w/ 7 levels "1"<"2"<"3"<"4"<..: 2 2 4 7 6 4 2 3 5 3 ...
# $ Balanced Diet        : Ord.factor w/ 7 levels "1"<"2"<"3"<"4"<..: 2 2 6 7 7 6 2 4 5 3 ...
# $ Obesity              : Ord.factor w/ 7 levels "1"<"2"<"3"<"4"<..: 4 2 7 7 7 7 4 3 5 3 ...
# $ Smoking              : Ord.factor w/ 8 levels "1"<"2"<"3"<"4"<..: 3 2 2 7 8 2 3 1 6 2 ...
# $ Passive Smoker       : Ord.factor w/ 8 levels "1"<"2"<"3"<"4"<..: 2 4 3 7 7 3 2 4 6 3 ...
# $ Chest Pain           : Ord.factor w/ 9 levels "1"<"2"<"3"<"4"<..: 2 2 4 7 7 4 2 3 6 4 ...
# $ Coughing of Blood    : Ord.factor w/ 9 levels "1"<"2"<"3"<"4"<..: 4 3 8 8 9 8 4 1 5 4 ...
# $ Fatigue              : Ord.factor w/ 8 levels "1"<"2"<"3"<"4"<..: 3 1 7 4 3 7 3 3 1 1 ...
# $ Weight Loss          : Ord.factor w/ 8 levels "1"<"2"<"3"<"4"<..: 4 3 7 2 2 7 4 2 4 2 ...
# $ Shortness of Breath  : Ord.factor w/ 8 levels "1"<"2"<"3"<"4"<..: 2 7 8 3 4 8 2 2 3 4 ...
# $ Wheezing             : Ord.factor w/ 8 levels "1"<"2"<"3"<"4"<..: 2 8 2 1 1 2 2 4 2 6 ...
# $ Swallowing Difficulty: Ord.factor w/ 8 levels "1"<"2"<"3"<"4"<..: 3 6 1 4 4 1 3 2 4 5 ...
# $ Finger Nails Clubbing: Ord.factor w/ 9 levels "1"<"2"<"3"<"4"<..: 1 2 4 5 2 4 1 2 6 4 ...
# $ Frequent Cold        : Ord.factor w/ 7 levels "1"<"2"<"3"<"4"<..: 2 1 6 6 4 6 2 3 2 2 ...
# $ Dry Cough            : Ord.factor w/ 7 levels "1"<"2"<"3"<"4"<..: 3 7 7 7 2 7 3 4 4 1 ...
# $ Snoring              : Ord.factor w/ 7 levels "1"<"2"<"3"<"4"<..: 4 2 2 5 3 2 4 3 1 5 ...
# $ Level                : Ord.factor w/ 3 levels "Low"<"Medium"<..: 1 2 3 3 3 3 1 1 2 2 ...
# - attr(*, ".internal.selfref")=<externalptr> 
#   - attr(*, "index")= int(0) 

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
  height = 15000,
  units = "px",
  dpi = 300,
  limitsize = FALSE,
)

## Barplots für kategorische Spalten
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

## Check Balance der Target-Klassen: keine große Imbalance
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

## Korrelation einzelner Spalten mit dem Level des Lungenkrebs
corr_level <- lungcancer_clean %>%
              mutate_if(is.factor, as.numeric) %>%
              cor() %>%
              as.data.frame() %>%
              select(Level) %>%
              arrange(-Level)
### Plotten und speichern der Tabelle
png("./plots/correlation_table.png", height=600, width=300)
grid.table(corr_level)
dev.off()
                  
## Korrelationsplot für kategorische Features
model.matrix(~0+., data = lungcancer_clean) %>%
  cor(use = "pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = FALSE,
             type = "lower",
             lab = TRUE,
             lab_size = 2)
print(model.matrix(~0+., data = lungcancer_clean))
### Speichern der Korrelationsmatrix
ggsave(filename = "correlation_plot.svg",
       plot = last_plot(),
       device = "svg",
       path = "./plots",
       scale = 1,
       width = 15000,
       height = 10000,
       units = "px",
       dpi = 300,
       limitsize = FALSE,
) #ToDo Buchstaben beim Encoding richten




##########################################
# Splitten des Datensatzes in Training und Testset


## Splitten zu 80/20 und Conversion zu data frame für preprocessing
split <- sample(1:nrow(lungcancer_clean), as.integer(0.8*nrow(lungcancer_clean)), F)
train <- as.data.frame(lungcancer_clean[split,])
test <- as.data.frame(lungcancer_clean[-split,])

## Checken, ob Dimensionen erhalten sind
dim(train)
# [1] 800  24
dim(test)
# [1] 200  24
class(train)
# [1] "data.frame"
class(test)
# [1] "data.frame"




##########################################
# Preprocessing


## Funktion für Preprocessing, das auf die Daten angewandt werden soll
preprocessing <- function(df){
  
  # Normalisierung: Min-Max-Scalierung der Spalte Age
  process <- preProcess(df["Age"], method = c("range"))
  df["Age"] <- predict(process, df["Age"])
  
  return(df[, names(df)!="Level"])
}

## Anwenden der preprocessing-Funktion auf Train und Test Datenset
x_train <- preprocessing(train)
x_test <- preprocessing(test)
y_train <- train[, "Level"]
y_test <- test[, "Level"]




##################################
# Machine Learning


## Festlegen der Trainingseinstellungen
ctrl <- trainControl(method = "cv", 
                     number = 5, 
                     verboseIter = TRUE,
                     classProbs = TRUE,
                     savePredictions = TRUE)

grid <- expand.grid(mtry = seq(5, ncol(x_train), 
                    by = 5))

model <- caret::train(x_train,
                      y_train,
                      method = "rf",
                      tuneGrid = grid,
                      trControl = ctrl)
model
# Random Forest 
# 
# 800 samples
# 23 predictor
# 3 classes: 'Low', 'Medium', 'High' 
# 
# No pre-processing
# Resampling: Cross-Validated (5 fold) 
# Summary of sample sizes: 640, 641, 640, 640, 639
# Resampling results across tuning parameters:
#   
# mtry  Accuracy  Kappa
# 05    1         1    
# 10    1         1    
# 15    1         1    
# 20    1         1    
# 
# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was mtry = 5.

## Plotten und speichern der Wichtigkeit der Features im RF Algorithmus
png("./plots/feature_importance.png", height=600, width=600)
plot(varImp(model), 
     main = "Feature-Wichtigkeit des RF Models im Trainig",
     xlab = "Wichtigkeit",
     ylab = "Feature")
dev.off()




###################################
# Prediction and Confusionmatrix

## Vorhersage auf Grundlage der Testdaten
prediction <- predict(model, newdata = x_test)

## Confusionsmatrix
confusionMatrix(prediction, y_test)
# Reference
# Prediction Low Medium High
# Low     63      0    0
# Medium   0     64    0
# High     0      0   73
# 
# Overall Statistics
# 
# Accuracy : 1          
# 95% CI : (0.9817, 1)
# No Information Rate : 0.365      
# P-Value [Acc > NIR] : < 2.2e-16  
# Kappa : 1          
# Mcnemar's Test P-Value : NA         
# 
# Statistics by Class:
#                      Class: Low Class: Medium Class: High
# Class: Low Class: Medium Class: High
# Sensitivity               1.000          1.00       1.000
# Specificity               1.000          1.00       1.000
# Pos Pred Value            1.000          1.00       1.000
# Neg Pred Value            1.000          1.00       1.000
# Prevalence                0.315          0.32       0.365
# Detection Rate            0.315          0.32       0.365
# Detection Prevalence      0.315          0.32       0.365
# Balanced Accuracy         1.000          1.00       1.000




###################################
# Packages checken

pkgs <- NCmisc::list.functions.in.file("Frank_Sabrina__BDR__Projektarbeit__12_2023.R")
summary(pkgs)
#                                   Length      Class      Mode     
# .GlobalEnv                             1     -none- character
# c(".GlobalEnv", "package:caret")       1     -none- character
# c("package:graphics", "package:base")  1     -none- character
# package:base                          30     -none- character
# package:caret                          4     -none- character
# package:data.table                     2     -none- character
# package:dplyr                          3     -none- character
# package:GGally                         2     -none- character
# package:ggcorrplot                     1     -none- character
# package:ggplot2                        9     -none- character
# package:grDevices                      2     -none- character
# package:gridExtra                      1     -none- character
# package:NCmisc                         1     -none- character
# package:stats                          3     -none- character
# package:tidyr                          1     -none- character
# package:utils                          1     -none- character