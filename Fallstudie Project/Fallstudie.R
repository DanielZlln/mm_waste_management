# Packages
# install.packages("packcircles")
# install.packages("corrgram")
# install.packages("corrplot")
# install.packages("GPArotation")
# install.packages("nFactors")

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
library(nFactors)

# Source laden
source("functions.R")

# DataFrame laden

load("waste_management.RData")

### Überblick über die Daten

summary(waste_management)
# Mehrere Spalten enthalten NA
str(waste_management) # Küstengemeinde Ja/Nein?
sum(waste_management[1,26:29]) # Spalten zusammen ergeben 100%

table(waste_management$Region)
table(waste_management$Provinz)

----------------------
# NA bearbeiten
----------------------


percent_na(waste_management)

waste_management <- exclude_na(waste_management)

anyNA(waste_management)
summary(waste_management)
mean(is.na(waste_management$Gemeinde))


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

count_dist_dimesnion(waste_management, "Region")
count_dist_dimesnion(waste_management, "Provinz")
count_dist_dimesnion(waste_management, "Gemeinde")

# Visualisieren

#-- 
# Circle Plot

circlePlot(durchschnitt_abfall_provinz, "Gesamt", "Provinz")
  
#--


##### Dimensionsreduktion

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



############
#############
# 
#
# as.data.frame(PC$x[])
# t(as.matrix(PC$rotation[])) + corrplot
#


# Anteil erklärter Varianz
barplot(hka_ww_management$sdev^2, names.arg = paste0("HK",1:31))
abline(h = 0.70, col = "red")
cumsum(hka_ww_management$sdev^2/sum(hka_ww_management$sdev^2))

# Biplot
biplot(hka_ww_management)

# Gesamtvarianz, die von jeder Hauptkomponente erklärt wird (Nele) 
var_explained <- round(hka_ww_management$sdev^2/sum(hka_ww_management$sdev^2), 2) 
var_explained

qplot(c(1:31),var_explained, xlab = "Principal Component", 
      ylab = "Variance Explained")

# weitere Biplotvarianten
fviz_pca_biplot(hka_ww_management, axes = c(1,3))
fviz_pca_var(hka_ww_management)

new_waste_management <- as.data.frame(hka_ww_management$x[,1:10])
new_waste_management


#### Factoranalyse

fa_waste_management <- factanal(waste_management_numeric, 
                         factors = 10,
                         scores = "Bartlett",
                         rotation = "none")
fa_waste_management

# Bei 10 Factors nur 58% Cululative Var

fa_promax <- fa_rotation(waste_management_numeric, 10, "promax")

# Bei 10 Factors und Rotation Promax sind es 62% Cumulative Var

biplot(y = fa_promax$loadings[,1:2], 
       x = fa_promax$scores[,1:2])


### Clusteranalyse
distance <- dist(scale(waste_management_numeric, 
                center = TRUE, 
                scale = TRUE),
          method = "manhattan")


# Hierarchische Clusteranalyse
h <- hclust(distance, method = "ward.D2")

# Dendrogramm
plot(h)

# Cluster im Dendogramm makieren
rect.hclust(h, k=8, border = "red")

# Ermitteln der Clusterzugehörigkeit
?cutree
cl <- cutree(h, k = 8)
waste_management$cluster <- cl


waste_management %>% 
  group_by(cluster) %>% 
  summarise(anzahl = n())

waste_management %>% 
  filter(cluster == 8) %>% 
  select(everything())

# Beschreiben
describeBy(waste_management[5:36], group = waste_management$cluster)
