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



##########################################
# Importieren und Sichten des Datensatzes


## Import mit fread
lungcancer_raw <- fread(file = "./data/lung_cancer_patient_datasets.csv")
class(lungcancer_raw)

## Sichten des Datensatzes
lungcancer_raw
dimensions <- dim(lungcancer_raw)
str(lungcancer_raw)
summary <- summary(lungcancer_raw)



##########################################
# Säubern des Datensatzes


## Patient ID und Index entfernen
lungcancer_raw[,(names(lungcancer_raw)[0:2]):=NULL]

## Gender umcodieren 1=Male, 2=Female
lungcancer_raw[, Gender := as.character(Gender)][Gender == "1", Gender := "M"]
lungcancer_raw[, Gender := as.character(Gender)][Gender == "2", Gender := "F"]

## Faktoren für Level einführen 
### https://datatables.net/forums/discussion/7001/solved-row-count-start-with-0-or-1
lungcancer_raw[,3:24] <- lapply(lungcancer_raw[,3:24],as.factor)
lungcancer_raw[,3:23] <- lapply(lungcancer_raw[,3:23],ordered)

### Faktorordnung der Level-Spalte
lungcancer_raw[, ("Level") := ordered(get("Level"), levels = c("Low", "Medium", "High"))]
levels(lungcancer_raw$Level)
str(lungcancer_raw)







## ToDO vielleicht verwerfen
## Piping
# lungcancer <- lungcancer_raw |>
                    
              
