# Packages
install.packages("packcircles")
install.packages("corrgram")
install.packages("corrplot")

# Library laden
library(lattice) 
library(dplyr)
library(packcircles)
library(ggplot2)
library(reshape2)
library(corrgram)
library(corrplot)

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

for (i in 1:ncol(waste_management)) {
  if (is.numeric(waste_management[[i]])) {
    #print(mean(is.na(waste_management[[i]])))
    print(paste0(colnames(waste_management[i]),": ", 
                 round(mean(is.na(waste_management[i])),3)))
  }
}


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

#


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


count_dist_dimesnion <- function(df, dimension) {
  df %>% 
    summarise(dist_dimension = n_distinct(dimension))
}

count_dist_dimesnion(waste_management, waste_management$Region)
count_dist_dimesnion(waste_management, waste_management$Provinz)
count_dist_dimesnion(waste_management, waste_management$Gemeinde)

# Visualisieren

#-- 
# Circle Plot
pck <- circleProgressiveLayout(durchschnitt_abfall_provinz$Gesamt, sizetype = "area")

mydata <- cbind(durchschnitt_abfall_provinz, pck)

myplotcord <- circleLayoutVertices(pck)

p1 <- ggplot()
p1 <- p1 + geom_polygon(data = myplotcord, 
                        aes(x,y, group = id, 
                            fill = as.factor(id)))
p1 <- p1 + geom_text(data = mydata, aes(x,y, size = Gesamt, 
                                        label = paste0(Provinz)))
p1 <- p1 + coord_equal()
p1 <- p1 + theme_void()
p1 <- p1 + theme(legend.position = "none")
p1 <- p1 + labs(title = "Durchschnittliche Abfallmenge pro Provinz")
p1
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


# Anteil erklärter Varianz
barplot(hka_ww_management$sdev^2, names.arg = paste0("HK",1:31))
#abline(h = 0.75, col = "red")
cumsum(hka_ww_management$sdev^2/sum(hka_ww_management$sdev^2))

