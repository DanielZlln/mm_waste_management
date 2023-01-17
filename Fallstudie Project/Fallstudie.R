# Packages laden
# install.packages("packcircles")
# install.packages("corrgram")
# install.packages("corrplot")
# install.packages("GPArotation")
# install.packages("nFactors")
# install.packages("sass")
# devtools::install_github("agstn/dataxray")
# install.packages("correlationfunnel")
# install.packages("NbClust")
# install.packages("flexclust")

# Library laden
library(flexclust)
library(doMC)
library(NbClust)
library(lattice) 
library(cluster)
library(dplyr)
library(tidyr)
library(packcircles)
library(data.table)
library(ggplot2)
library(reshape2)
library(corrgram)
library(corrplot)
library(FactoMineR)
library(factoextra)
library(psych)
library(GPArotation)
library(dataxray)
library(correlationfunnel)
library(nFactors)
library(tidymodels)
tidymodels_prefer()


# Source laden um Funktionen aufzurufen
source("functions.R")

# Dataframe laden
load("waste_management.RData")

#
#------------------------------------------------------------------------
#

# Überblick über die Daten
summary(waste_management)

# Mehrere Spalten enthalten NA
str(waste_management)

# Datensatz anschauen 
View(waste_management)

# Spalten ueberpruefen 
# Abfaelle_sortiert + Abfaelle_unsortiert = Abfaelle gesamt
sum(waste_management[1,14:15])
waste_management[1,13]

waste_management %>% 
  rowwise() %>% 
  summarise(summe_abfall = sum(Abfaelle_sortiert,
                               Abfaelle_unsortiert, na.rm = T),
            Abfaelle_gesamt)

# Spalten ueberpruefen
# Spalten verschiedener Muellarten = Sortierungsgrad 
sum(waste_management[1,17:25], na.rm = T)
waste_management[1,17:25]
waste_management[1,16]
sum(waste_management[3,17:25], na.rm = T)
waste_management[3,17:25]
waste_management[3,16]

waste_management %>% 
  rowwise() %>% 
  summarise(summe_sort = sum(Sort_Bio,Sort_Papier,Sort_Glas,
                             Sort_Holz,Sort_Metall,
                             Sort_Plastik,Sort_Elektrik,
                             Sort_Textil,Sort_Rest, na.rm = T),
            Sortierungsgrad)

# Sortierungsgrad ergibt sich aus den Werten die da sind
# durch 0 ersetzten oder mean?

# Haben alle nomialen Spalten nur die richtigen ausprägungen?
table(waste_management[,9])
table(waste_management[,10])
table(waste_management[,11])
table(waste_management[,12])
# 1.5 ist falsch

# Spalten ueberpruefen
# Spalten verschiedener Verwendungen ergeben zusammen 100%
sum(waste_management[1,26:29]) 

waste_management %>% 
  rowwise() %>% 
  summarise(summe_verw = sum(Verwendung_Energie, Verwendung_Deponie,
                             Verwendung_Recycling, Verwendung_Unbekannt))

# Haufigkeiten anschauen 
table(waste_management$Region)
table(waste_management$Provinz)

#----------------------
# NA bearbeiten
#----------------------

# Anteile fehlender Werte im Datensatz 
percent_na(waste_management)

# Fuer die Spalten sort_ werden 0 eingesetzt dann passt der Sortierungsgrad
## In einer Funktion 
for (i in 1:nrow(waste_management)) {
  if (i >= 17 & i <= 25) {
    for (j in 1:length(waste_management[[i]])) {
      if (is.na(waste_management[j,i])) {
        waste_management[j,i] <- 0
      }
    }
  }
}


# Fehlende Werte aus dem Datensatz entfernen
waste_management <- exclude_na(waste_management)

# Zeilen mit fehlenden Characterwerten entfernen
waste_management <- exclude_chr_row(waste_management)

# ueberpruefen ob noch fehlende Werte vorliegen
anyNA(waste_management)
summary(waste_management)
mean(is.na(waste_management$Gemeinde))
str(waste_management)
# keine fehlenden Werte mehr im Datensatz! 

#----------------------
# Visualisierungen
#----------------------

# Durchschnittlicher Abfall der Provinzen 
durchschnitt_abfall_provinz <- waste_management %>% 
  group_by(Provinz) %>% 
  summarise(
    Bevoelkerung = sum(Bevoelkerung),
    Gesamt = round(mean(Abfaelle_gesamt),1),
    Sortiert = round(mean(Abfaelle_sortiert),1),
    Unsortiert = round(mean(Abfaelle_unsortiert),1),
    Sortierungsgrad = round(mean(Sortierungsgrad),1),
    Kosten_Basis = round(mean(Kosten_Basis),1)) %>% 
  arrange(desc(Gesamt))

# Circle Plot erstellen
circlePlot(durchschnitt_abfall_provinz, "Kosten_Basis", "Provinz",
           "Kosten pro Provinz")

# Ergebnis: Torino mit Abstand hoechsten Quoten beim Abfall, 
# obwohl nicht hoechste Bevoelkerung (mittelwert auf nur 1 Stadt?)
# Kosten Basis in etwa gleich bei allen Provinzen

# Ausgabe von distinct Auspraegungen 
count_dist_dimesnion(waste_management, "Region")
count_dist_dimesnion(waste_management, "Provinz")
count_dist_dimesnion(waste_management, "Gemeinde")


# welche Region hat die groeßte Bevoelkerung?
region_bev <- waste_management %>% 
  select(Region, Bevoelkerung) %>%
  group_by(Region) %>%
  summarise(summe_bev = sum(Bevoelkerung)) 

region_bev$Region <- as.factor(region_bev$Region)

# plot erstellen 
barplot(region_bev$summe_bev~region_bev$Region, xlab="Region", 
        ylab= "Bevoelkerunganzahl", main= "Region nach Bevoelkerungszahl", 
        col =c(1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1,1,1,1))

# welche Region produziert den meisten Muell?
anteil_muell <- waste_management %>% 
  select(Region, Abfaelle_gesamt) %>%
  group_by(Region) %>%
  summarise(summe_abf = sum(Abfaelle_gesamt)) 

anteil_muell$Region <- as.factor(anteil_muell$Region)

barplot(anteil_muell$summe_abf~anteil_muell$Region, xlab="Region", 
        ylab="Gesamtabfall Abfall", main = "Gesamtabfall in den Regionen",
        col =c(1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1,1,1,1))

# Ergebnis: Die Region mit dem meisten Abfall hat auch die hoeste Bevoelkerung


#----------------------
# Aufbereitung der Daten fuer Dimensionsreduktion (PCA)
# und diskriptive Statistiken 
#----------------------

# Eigenschaften der Stadt als factor
waste_management <- waste_management %>% 
  mutate_at(vars(Inselgemeinde:Geologischer_Indikator), as.factor)

# Neuen data.frame mit numerischen Werte
waste_management_numeric <- waste_management[sapply
                                             (waste_management, 
                                               is.numeric)]

# Korrelationsmatrix
# kontrollieren welche spalten raus koennen wenn zu starke korrelation
cor_matrix <- cor(waste_management_numeric)

# Visualisierung der Korrelationsmatrix
corrgram(cor_matrix)
corrplot(cor_matrix, order = "hclust")

# Loeschen von Spalten die eine zu hohe Korrelation haben
# um die kumulierte var fuer die HKA zu optimieren
waste_management_numeric <- waste_management_numeric %>%  
  select(-ID,-Abfaelle_unsortiert, -Bevoelkerung,
         -Sort_Bio, -Verwendung_Recycling, -Sort_Rest, 
         -Kosten_sonstiges,-Strassen)

# Nochmals die cor_matrix kontrollieren
cor_matrix <- cor(waste_management_numeric)

# Visualisierung der Korrelationsmatrix
# spalten ueber mit hoher korrelation, jedoch verbessert das nicht die kumvar
corrplot(cor_matrix, order = "hclust")


# Ueberpruefen, ob alle Werte nun numerisch sind 
str(waste_management_numeric)

# Indikator muss 1,2,3 sein, ist der nicht mehr wenn durch mean ersetzt wird
# table(waste_management_numeric[,"Geologischer_Indikator"])
# waste_management_numeric$Geologischer_Indikator <- 
#   round(waste_management_numeric$Geologischer_Indikator,0)
# table(waste_management_numeric[,"Geologischer_Indikator"])
# 
# table(waste_management[,"Geologischer_Indikator"])
# waste_management$Geologischer_Indikator <- 
#   round(waste_management$Geologischer_Indikator,0)
# table(waste_management_numeric[,"Geologischer_Indikator"])

table(waste_management[,"Urbanisierungsgrad"])
table(waste_management[,"Kuestengemeinde"])
table(waste_management[,"Inselgemeinde"])
table(waste_management[, "Geologischer_Indikator"]) 
# Spaeter aendern, durch mittelwert jetzt unlogische Werte

boxplot(waste_management_numeric)


#----------------------
# Dimensionsreduktion
#----------------------

# Hauptkomponentenanalyse 
# dort einsetzen, wo Variablen stark korrellieren
hka_ww_management <- prcomp(waste_management_numeric, 
                            center = TRUE, 
                            scale. = TRUE)
hka_ww_management


# Anteil erklaerter Varianz
barplot(hka_ww_management$sdev^2, names.arg = paste0("HK",1:19))

cumsum(hka_ww_management$sdev^2/sum(hka_ww_management$sdev^2))

# Rotationsmatrix
hka_ww_management$rotation

# verschiedene Biplotvarianten
biplot(hka_ww_management)
fviz_pca_biplot(hka_ww_management, axes = c(1,3))
fviz_pca_var(hka_ww_management)

# visusalisation of quality
fviz_eig(hka_ww_management)

# PDF to analyse the principal components
pdf("principal_components.pdf", width = 8.5, height = 6)

number_of_dim <- c(seq(1:10))

for (i in number_of_dim) {
  var <- get_pca_var(hka_ww_management)
  ab <- fviz_contrib(hka_ww_management, 
                     "var", 
                     axes= i, 
                     xtickslab.rt=90)
  plot(ab,main = "Variables percentage contribution of the Principal Components")
}

dev.off()



# Gesamtvarianz, die von jeder Hauptkomponente erklärt wird 
var_explained <- round(hka_ww_management$sdev^2/
                         sum(hka_ww_management$sdev^2), 2) 
var_explained
# bspw. die erste HK erklaert 18% der Gesamtvarianz 

# Visualisierung wie viel Gesamtvarianz die HK erklaeren
qplot(c(1:19),var_explained, xlab = "Principal Component", 
      ylab = "Variance Explained")

# 
plot(cumsum(var_explained), xlab="Principal Component", 
     ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1),type='b')
abline(v = 10, col = "red")
abline(h = 0.812, col = "red")
# mit diesem Dataframe in die Clusteranalyse 
# bei einer Auswahl von 10 Hauptkomponenten 

new_waste_management <- as.data.frame(hka_ww_management$x[,1:10])
new_waste_management


#----------------------
# Faktoranalyse
#----------------------

# Faktoranalyse ohne Rotation
fa_waste_management <- factanal(waste_management_numeric, 
                                factors = 5,
                                scores = "Bartlett",
                                rotation = "none")
fa_waste_management
# bei 10 Faktoren und keiner Rotation sind es 46% Cumulative Var
# aus den 30 Variablen wurden 5 Variablen (Faktoren) gebildet

# Ergebnisse ausgeben lassen mit 2 Nachkommastellen und Faktorladung < 0.5 
# kleinere Faktorladungen brauchen nicht angeschaut werden
print(fa_waste_management, digits = 2, cutoff=0.5)

fa_promax <- fa_rotation(waste_management_numeric, 5, "promax")
fa_promax
# Bei 10 Faktoren und Rotation Promax sind es 47% Cumulative Var

fa_quartimax <- fa_rotation(waste_management_numeric, 5, "quartimax")
fa_quartimax

# Biplot fuer Faktor 1 und 2 
biplot(y = fa_promax$loadings[,1:2], 
       x = fa_promax$scores[,1:2])

# Korrelationsmatrix 
loadings <- t(as.matrix(fa_waste_management$loadings))
corrplot(loadings, method = "circle")

# Ergebnis Faktoranalyse: 
# Zu wenig Var in den neuen Spalten
# somit weiter mit HKA

#----------------------
# Clusteranalyse: hierarchisches Verfahren nach hclust,dist,cutree = agglomerativ
#----------------------

# Distanzberechnungen 
pdf("all_distance_output.pdf", width = 8.5, height = 6)

# verschiedene Methoden der Distanzberechnung moeglich 
methods_dist <- c("euclidian", "maximum", "manhattan", "binary", "minkowski")
methods_fusio <- c("single", "complete", "average", "mcquitty", 
                   "median", "centroid", "ward.D2")

for (i in 1:length(methods_dist)) {
  for (j in 1:length(methods_fusio)) {
    all_distance(new_waste_management, methods_dist[i], methods_fusio[j], 6)
  }
}

dev.off()

all_distance(new_waste_management, "manhattan", "ward.D2", 6)


# Entscheidung feur Distanzberechnung nach "manhatten"
distance <- dist(scale(new_waste_management, 
                       center = TRUE, 
                       scale = TRUE),
                 method = "manhattan",
                 p = 2)

# standardisieren mit scale = TRUE
# wichtig: Variablen mit verschiedenen Skalen, müssen normalisiert werden 
# bevor geclustert wird

# Ermitteln der Clusterzugehoerigkeit
h <- hclust(distance, method = "ward.D2")

# fuer jede Boebachtung Clusterzugehoerigkeit angeben
cl <- cutree(h, k = 6)


# entfernen von den factor variablen in waste_management

waste_management <- waste_management %>% 
  mutate_at(vars(Inselgemeinde:Geologischer_Indikator), as.integer)
str(waste_management)
# Cluster an den Datensatz anfuegen fuer dist
waste_management_dist <- waste_management
waste_management_dist$cluster <- cl


# Profiling mit Excel
# dafuer neue spalten erstellt um die cluster besser zu interpretieren
for (i in 2:8){
  waste_management_dist$cluster <- cutree(h, k = i)
  
  df_profiling <- 
    waste_management_dist %>% 
    group_by(cluster) %>%
    mutate(Inselgemeinde_count = sum(Inselgemeinde==1),
           Kuestengemeinde_count = sum(Kuestengemeinde==1),
           Urban_niedri = sum(Urbanisierungsgrad==1)/sum(Urbanisierungsgrad!=0),
           Urban_mittel = sum(Urbanisierungsgrad==2)/sum(Urbanisierungsgrad!=0),
           Urban_hoch = sum(Urbanisierungsgrad==3)/sum(Urbanisierungsgrad!=0),
           Sueden = sum(Geologischer_Indikator==1)/sum(Geologischer_Indikator!=0),
           Mitte = sum(Geologischer_Indikator==2)/sum(Geologischer_Indikator!=0),
           Norden = sum(Geologischer_Indikator==3)/sum(Geologischer_Indikator!=0)) %>% 
    summarize_all(mean)
  
  df_profiling$n_cluster <- i
  df_profiling$size <- table(waste_management_dist$cluster)
  
  if (i == 2){
    df_profiling_dist <- df_profiling
  } else {
    df_profiling_dist <- 
      df_profiling_dist %>% 
      rows_append(df_profiling)
  }
  
}

write.csv2(df_profiling_dist, file = "profiling.csv")




# Cluster 6 Stadt Rom 

# Anzahl Beobachtungen pro Cluster 
waste_management_dist %>% 
  group_by(cluster) %>% 
  summarise(anzahl = n())

# # hierarchische clusteranalyse diana

# Distanzberechnung diana

diana_dist <- diana(scale(waste_management_numeric), 
                    metric = "euclidean", stand = F)

c_diana <- cutree(diana_dist, k = 7)

waste_management_diana <- waste_management
waste_management_diana$cluster <- c_diana
waste_management_diana


# hierarchische clusteranalyse daisy

# Distanzberechnungen daisy

pdf("all_distance__daisy_output.pdf", width = 8.5, height = 6)

# verschiedene Methoden der Distanzberechnung moeglich 
metric_dist <- c("euclidean","manhattan", "gower")
fusio_method <- c("single", "complete", "average", 
                  "weighted", "ward", "flexible")
metrics <- c("euclidean","manhattan")

for (i in 1:length(metric_dist)) {
  for (j in 1:length(fusio_method)) {
    for (m in 1:length(metrics)) {
      all_distance_daisy(new_waste_management,
                         metric_dist[i],
                         metrics[m],
                         fusio_method[j],
                         6)
    }
  }
}

dev.off()

daisy_dist <- daisy(waste_management_numeric, metric = "euclidean", stand = F)

daisy_agnes <- agnes(daisy_dist, diss = T, 
                   metric = "euclidean", 
                   stand = T, 
                   method = "ward")

c_diasy <- cutree(dia_agnes, k = 7)

waste_management_daisy <- waste_management

waste_management_daisy$cluster <- c_diasy

waste_management_daisy %>% 
  group_by(cluster) %>% 
  summarise(anzahl = n())

for (i in 2:9){
  waste_management_daisy$cluster <- cutree(dia_agnes, k = i)
  
  df_profiling <- 
    waste_management_daisy %>% 
    group_by(cluster) %>%
    mutate(Inselgemeinde_count = sum(Inselgemeinde==1),
           Kuestengemeinde_count = sum(Kuestengemeinde==1),
           Urban_niedri = sum(Urbanisierungsgrad==1)/sum(Urbanisierungsgrad!=0),
           Urban_mittel = sum(Urbanisierungsgrad==2)/sum(Urbanisierungsgrad!=0),
           Urban_hoch = sum(Urbanisierungsgrad==3)/sum(Urbanisierungsgrad!=0),
           Sueden = sum(Geologischer_Indikator==1)/sum(Geologischer_Indikator!=0),
           Mitte = sum(Geologischer_Indikator==2)/sum(Geologischer_Indikator!=0),
           Norden = sum(Geologischer_Indikator==3)/sum(Geologischer_Indikator!=0)) %>% 
    summarize_all(mean)
  
  df_profiling$n_cluster <- i
  df_profiling$size <- table(waste_management_daisy$cluster)
  
  if (i == 2){
    df_profiling_daisy <- df_profiling
  } else {
    df_profiling_daisy <- 
      df_profiling_daisy %>% 
      rows_append(df_profiling)
  }
  
}

write.csv2(df_profiling_daisy, file = "profiling_daisy.csv")

#
   # k-means
# 
fviz_nbclust(waste_management_numeric, kmeans, method = "wss")

# optimale Anzahl an Clustern nach Lueckenstatistik
gap_stat <- clusGap(waste_management_numeric,
                    FUN = kmeans,
                    nstart = 25,
                    K.max = 20,
                    B = 50) # 500 is recommendet, but to long

# perform clustering, B = 500 is recommended
hcluster <- clusGap(waste_management_numeric, FUN = hcut, K.max = 20, B = 50)

# extract results
dat <- data.table(hcluster$Tab)
dat[, k := .I]

# visualize gap statistic
viz_gap <- ggplot(dat, aes(k, gap)) + geom_line() + geom_point(size = 3) +
  geom_errorbar(aes(ymax = gap + SE.sim, ymin = gap - SE.sim), width = 0.25) +
  ggtitle("Clustering Results") +
  labs(x = "Number of Clusters", y = "Gap Statistic") +
  theme(plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"))
viz_gap
# Optimal 5?


k_erg <- data.frame(k = 1:20,
                    totss = 0,
                    withinss = 0,
                    betweenss = 0)

for (i in 1:20) {
  km <- kmeans(scale(waste_management_numeric), centers = i, 
               nstart = 10, iter.max = 20)
  k_erg$totss[i] <- km$totss
  k_erg$withinss[i] <- km$tot.withinss
  k_erg$betweenss[i] <- km$betweenss
}

k_erg

# Plot WithinSS
plot(k_erg$k,
     k_erg$withinss,
     type = "b",
     xlab = "Anzahl Cluster",
     ylab = "WithinSS")

# Plot erklärte Varianz
plot(k_erg$k, 
     k_erg$betweenss/k_erg$totss, 
     type = "b",
     xlab = "Anzahl Cluster",
     ylab = "Erklärte Varianz")

waste_management_kmeans <- waste_management
for (i in 2:9){
  km <- kmeans(scale(waste_management_numeric), 
                                            centers = i, nstart = 10)
  
  cluster_kmeans <- km$cluster
  
  waste_management_kmeans$cluster <- cluster_kmeans
  
  df_profiling_km <- 
    waste_management_kmeans %>% 
    group_by(cluster) %>%
    mutate(Inselgemeinde_count = sum(Inselgemeinde==1),
           Kuestengemeinde_count = sum(Kuestengemeinde==1),
           Urban_niedri = sum(Urbanisierungsgrad==1)/sum(Urbanisierungsgrad!=0),
           Urban_mittel = sum(Urbanisierungsgrad==2)/sum(Urbanisierungsgrad!=0),
           Urban_hoch = sum(Urbanisierungsgrad==3)/sum(Urbanisierungsgrad!=0),
           Sueden = sum(Geologischer_Indikator==1)/sum(Geologischer_Indikator!=0),
           Mitte = sum(Geologischer_Indikator==2)/sum(Geologischer_Indikator!=0),
           Norden = sum(Geologischer_Indikator==3)/sum(Geologischer_Indikator!=0)) %>% 
    summarize_all(mean)
  
  df_profiling_km$n_cluster <- i
  df_profiling_km$size <- table(waste_management_kmeans$cluster)
  
  if (i == 2){
    df_profiling_km_final <- df_profiling_km
  } else {
    df_profiling_km_final <- 
      df_profiling_km_final %>% 
      rows_append(df_profiling_km)
  }
}

write.csv2(df_profiling_km_final, file = "profiling_km.csv")


# zwischenschritt um cluster an normalen datensatz zu hängen
waste_management_kmeans <- waste_management
km <- kmeans(scale(waste_management_numeric), centers = 5, nstart = 10)
cluster_kmeans <- km$cluster
waste_management_kmeans$cluster <- cluster_kmeans

waste_management_kmeans %>% 
  group_by(cluster) %>% 
  summarise_all(mean)
waste_management_kmeans

#
#------------------------------------------------------------------------
#

# Entschieden für hclust nach der analyse in excel

#
#------------------------------------------------------------------------------
# Profiling
head(waste_management)

# Verwendung betrachten
describeBy(waste_management_dist[26:29], 
           group = waste_management_dist$cluster)

waste_verwendung <- waste_management_dist %>% 
  select(26:29, 37)

plot_groups(waste_verwendung)

# Sort betrachten
describeBy(waste_management_dist[17:25], 
           group = waste_management_dist$cluster)

waste_sort <- waste_management_dist %>% 
  select(17:25, 37)

plot_groups(waste_sort)


# Abfaelle
describeBy(waste_management_dist[13:15], 
           group = waste_management_dist$cluster)

waste_abfaelle <- waste_management_dist %>% 
  select(13:15, 37)

plot_groups(waste_abfaelle)

# Geographie
describeBy(waste_management_dist[5:8],
           group = waste_management_dist$cluster)

waste_geo <- waste_management_dist %>% 
  select(5:8, 37)

plot_groups(waste_geo)

# Kosten
describeBy(waste_management_dist[30:34],
           group = waste_management_dist$cluster)

waste_kosten <- waste_management_dist %>% 
  select(32:34,37)

plot_groups(waste_kosten)

# Steuern
describeBy(waste_management_dist[30:31],
           group = waste_management_dist$cluster)

waste_steuern <- waste_management_dist %>% 
  select(30:31,37)

plot_groups(waste_steuern)

# Flaeche und Bevoelkerungsdichte
describeBy(select(waste_management_dist, 5, 7),
           group = waste_management_dist$cluster)

waste_flaeche <- waste_management_dist %>% 
  select(5,7,37)

plot_groups(waste_flaeche)

# Plots in einer PDF abspeichern
pdf("auswertung_dist.pdf", width = 8.5, height = 6)
  plot_groups(waste_kosten)
  plot_groups(waste_abfaelle)
  plot_groups(waste_verwendung)
  plot_groups(waste_sort)
  plot_groups(waste_steuern)
  plot_groups(waste_flaeche)
dev.off()

# Fertig