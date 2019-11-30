
library(readxl)
library(dplyr)
table1 <- read.csv("C:\\Users\\eya.b\\Documents\\Eya\\Finance quantitative\\Europe_3_Factors.csv", header = F)
table1 <- table1[1:354,]
FF3 <- read_excel("C:\\Users\\eya.b\\Documents\\Eya\\Finance quantitative\\FF3.xlsx", sheet = "Feuil3",col_names=FALSE)
colnames(FF3) <- c("Date", "Rates")
colnames(table1) <- c("Date", "Mkt-RF", "SMB", "HML", "RF")

####Converting columns to format Year/Month
## We get the characters that correspond to the year from the column Date
year <- substr(table1$Date, 1, 4)
## We get the characters that correspond to the month from the column Date
month <- substr(table1$Date, 5,6)
table1$year <- year
table1$month <- month
table1$date_nouveau<- paste(table1$year,table1$month,sep = "-")
## We remove non useful columns
table1$Date<- NULL
table1$year <-NULL
table1$month <- NULL
FF3$Date <- substr(FF3$Date, 1,7)
## We rename columns into 
colnames(table1) <- c("Mkt-RF", "SMB", "HML", "RF","Date")

### Left merge
Data=left_join(FF3,table1,by=c("Date"="Date"))
## Creation of our target variable target variable target= rates - rf
Data$target= as.double(Data$Rates) - as.double(Data$RF)

### Prinipal component analysis
Data$HML=as.numeric(Data$HML)
Data$SMB=as.numeric(Data$SMB)
Data$`Mkt-RF`=as.numeric(Data$`Mkt-RF`)
pca=prcomp(Data[,c("Mkt-RF", "SMB", "HML", "RF", "Rates")], center=TRUE, scale. = TRUE)
summary(pca)

library("FactoMineR")
res.pca <- PCA(Data[,c("Mkt-RF", "SMB", "HML", "Rates")], graph = TRUE)
print(res.pca)

## Eigen values
library("factoextra")
library(ggplot2)
library("corrplot")
eig.val <- get_eigenvalue(res.pca)
eig.val
## Eigen values plot
eig.viz<-fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))
eig.viz
## Variables extraction from PCA
var <- get_pca_var(res.pca)
var
# Coordonnées
head(var$coord)
# Cos2: qualité de répresentation
head(var$cos2)
# Contributions aux composantes principales
head(var$contrib)

## Cercle de corrélation
# Coordonnées des variables
head(var$coord, 4)
fviz_pca_var(res.pca, col.var = "black")
# Cos2 total des variables sur Dim.1 et Dim.2
fviz_cos2(res.pca, choice = "var", axes = 1:2)
# Contributions des variables à PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions des variables à PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 1:2, top = 10)
fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)
res.desc <- dimdesc(res.pca, axes = c(1,2), proba = 0.05)
# Description de la dimension 1
res.desc$Dim.1
# Description de la dimension 2
res.desc$Dim.2

#### Linear model
linear_model<-lm(target~`Mkt-RF`+SMB+HML,data=Data)
summary(linear_model)

### Interprétation dur l'intercept: l'intercept est singnificatif
