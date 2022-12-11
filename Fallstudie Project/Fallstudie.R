# Packages
install.packages("packcircles")
install.packages("corrgram")
install.packages("corrplot")
install.packages("GPArotation")

# Library laden
library(lattice) 
library(dplyr)
library(packcircles)
library(ggplot2)
library(reshape2)
library(corrgram)
library(corrplot)
library(FactoMineR)
library(factoextra)
library(psych)
library(GPArotation)

# DataFrame laden

load("waste_management.RData")

### Überblick über die Daten

summary(waste_management)
# Mehrere Spalten enthalten NA
View(waste_management)
str(waste_management) # Küstengemeinde Ja/Nein?
sum(waste_management[1,26:29]) # Spalten zusammen ergeben 100%

table(waste_management$Region)
table(waste_management$Provinz)

----------------------
# NA bearbeiten
----------------------
percent_na <- function(df) {
  for (i in 1:ncol(df)) {
    if (is.numeric(df[[i]])) {
      print(paste0(colnames(df[i]),": ", 
                   round(mean(is.na(df[i])),3)))
    }
  }
}

percent_na(waste_management)

exclude_na <- function(df) {
  for (i in 1:ncol(waste_management)) {
    if (anyNA(waste_management[[i]]) & is.numeric(waste_management[[i]])) {
      for (j in 1:length(waste_management[,i])) {
        if (is.na(waste_management[j,i])) {
          waste_management[j,i] <- mean(waste_management[,i], na.rm = T) 
        }
      }
    }
  }
  return(waste_management)
}

waste_management <- exclude_na(waste_management)

anyNA(waste_management)
summary(waste_management)
mean(is.na(waste_management$Gemeinde))

exclude_chr_row <- function(df) {
  for (i in 1:ncol(df)) {
    for (j in 1:length(df[[i]])) {
      if (is.na(df[j,i])) {
        df[j,] <- df[-j,]
      }
    }
  }
  return(df)
}

exclude_chr_row <- function(df) {
  df %>% 
    filter(complete.cases(.))
}


waste_management <- exclude_chr_row(waste_management)


durchschnitt_abfall_provinz <- waste_management %>% 
                               group_by(Provinz) %>% 
                               summarise(
                                 Bevoelkerung = sum(Bevoelkerung),
                                 Gesamt = round(mean(Abfaelle_gesamt),1),
                                 Sortiert = round(mean(Abfaelle_sortiert),1),
                                 Unsortiert = round(mean(Abfaelle_unsortiert),1),
                                 Sortierungsgrad = round(mean(Sortierungsgrad),1)) %>% 
                               arrange(desc(Gesamt))
# Torino mit Abstand höchsten Quoten, obwohl nicht höchste Bevölkerung


count_dist_dimesnion <- function(df, dimension_in_quotation_marks) {
  df %>% 
    summarise(dist_dimension = n_distinct(df[[dimension_in_quotation_marks]]))
}

count_dist_dimesnion(waste_management, "Region")
count_dist_dimesnion(waste_management, "Provinz")
count_dist_dimesnion(waste_management, "Gemeinde")

# Visualisieren

#-- 
# Circle Plot
circlePlot <- function(df, column, label){
  
  pck <- circleProgressiveLayout(df[[column]], sizetype = "area")
  
  mydata <- cbind(df, pck)
  
  myplotcord <- circleLayoutVertices(pck)
  
  p1 <- ggplot() + 
    geom_polygon(data = myplotcord, 
                 aes(x,y, group = id, 
                     fill = as.factor(id))) +
    geom_text(data = mydata, aes(x,y, size = df[[column]], 
                                 label = paste0(df[[label]]))) +
    coord_equal() +
    theme_void() +
    theme(legend.position = "none") +
    labs(title = "Durchschnittliche Abfallmenge pro Provinz")
  p1
}

circlePlot(durchschnitt_abfall_provinz, "Gesamt", "Provinz")
  
#--


##### Dimensionsreduktion

chr_to_factor <- function(df) {
  for (i in 1:ncol(df)) {
    if (is.character(df[[i]])) {
      df[[i]] <- as.factor(df[[i]])
    }
  }
  return(df)
}

waste_management_factor <- chr_to_factor(waste_management)

str(waste_management_factor)

waste_management_numeric <- waste_management[sapply
                                             (waste_management, 
                                               is.numeric)]
str(waste_management_numeric)

# Korrelationsmatrix

cor_matrix <- cor(waste_management_numeric)

# Visualisierung der Korrelationsmatrix

corrgram(cor_matrix)
corrplot(cor_matrix, order = "hclust")

# Hauptkomponentenanalyse
hka_ww_management <- prcomp(waste_management_numeric, 
                            center = TRUE, 
                            scale. = TRUE,
                            rank. = 10)
hka_ww_management

#
# as.data.frage(PC$x[])
# t(as.matrix(PC$rotation[])) + corrplot
#


# Anteil erklärter Varianz
barplot(hka_ww_management$sdev^2, names.arg = paste0("HK",1:31))
abline(h = 0.75, col = "red")
cumsum(hka_ww_management$sdev^2/sum(hka_ww_management$sdev^2))

# Biplot
biplot(hka_ww_management)

# Gesamtvarianz, die von jeder Hauptkomponente erklärt wird (Nele) 
var_explained <- round(hka_ww_management$sdev^2/sum(hka_ww_management$sdev^2), 2) 
var_explained

#### Factoranalyse

fa_waste_management <- factanal(waste_management_numeric, 
                         factors = 10,
                         scores = "Bartlett",
                         rotation = "none")
fa_waste_management

# Bei 10 Factors nur 58% Cululative Var

fa_rotation <- function(df, factor, fa_rotation) {
  fa_df <- factanal(df, 
                    factors = factor,
                    scores = "Bartlett",
                    rotation = fa_rotation)
  return(fa_df)
}
fa_promax <- fa_rotation(waste_management_numeric, 10, "promax")

# Bei 10 Factors und Rotation Promax sind es 62% Cumulative Var

biplot(y = fa_promax$loadings[,1:2], 
       x = fa_promax$scores[,1:2])

# Zu viel Varianz geht verloren bei gleicher Anzahl an Variablen -> weiter mit HK

### Clusteranalyse


