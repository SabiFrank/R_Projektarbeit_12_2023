#git add
#git commit -m "adding a new file" 
#git status
#git push

####################################
# Lung Cancer Risk Prediction in R #
# Author: Sabrina Frank            #
# Date: 15.01.2024                 #
# Project: Big Data Management     #
####################################


##########################################
# Installieren von benötigten Bibliotheken

# install.packages("data.table")
library("data.table")


#########################################
# Importieren und Sichten des Datensatzes

## Import mit fread
lungcancer_raw <- fread(file = "./data/lung_cancer_patient_datasets.csv")
class(lungcancer_raw)

## Sichten des Datensatzes
lungcancer_raw
dimensions <- dim(lungcancer_raw)
str(lungcancer_raw)
summary <- summary(lungcancer_raw)


#########################
# Säubern des Datensatzes

## Patient ID und Index entfernen
lungcancer_raw [,"Patient Id":=NULL]
# ToDo: subsetting oder slicing schauen wue funktioniert
lungcancer_raw

## Gender umcodieren 1=Male, 2=Female
lungcancer_raw[, Gender := as.character(Gender)][Gender == "1", Gender := "M"]
lungcancer_raw[, Gender := as.character(Gender)][Gender == "2", Gender := "F"]
lungcancer_raw

## Faktoren für Level einführen 
### https://datatables.net/forums/discussion/7001/solved-row-count-start-with-0-or-1
lungcancer_raw[, "Air Pollution":=as.character("Air Pollution")]
lungcancer_raw[, "Air Pollution":=as.character("Air Pollution")]

str(lungcancer_raw)

## Piping
lungcancer <- lungcancer_raw |>
              
