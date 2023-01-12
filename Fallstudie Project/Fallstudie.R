# Packages
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
library(nFactors)
library(tidymodels)
tidymodels_prefer()

library(dataxray)
library(correlationfunnel)

registerDoMC(cores = 4)

# Source laden um Funktionen aufzurufen
source("functions.R")

# Dataframe laden
load("waste_management.RData")

# Überblick über die Daten
summary(waste_management)

# Mehrere Spalten enthalten NA
str(waste_management) # Küstengemeinde Ja/Nein?

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

# NA 0 oder der Wert fehlt und dann mittelwert bilden und 
# Sortierungsgrad löschen

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
    Sortierungsgrad = round(mean(Sortierungsgrad),1)) %>% 
  arrange(desc(Gesamt))

# Ergebnis: Torino mit Abstand hoechsten Quoten, obwohl nicht hoechste Bevölkerung

count_dist_dimesnion(waste_management, "Region")
count_dist_dimesnion(waste_management, "Provinz")
count_dist_dimesnion(waste_management, "Gemeinde")

# Circle Plot erstellen
circlePlot(durchschnitt_abfall_provinz, "Gesamt", "Provinz")


# welche Region hat die groeßte Bevoelkerung?
region_bev <- waste_management %>% 
  select(Region, Bevoelkerung) %>%
  group_by(Region) %>%
  summarise(summe_bev = sum(Bevoelkerung)) 

region_bev$Region <- as.factor(region_bev$Region)

# plot erstellen 
barplot(region_bev$summe_bev~region_bev$Region, xlab="Region", 
        ylab= "Bevölkerunganzahl", main= "Region nach Bevölkerungszahl", 
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
  mutate_at(vars(Inselgemeinde:Urbanisierungsgrad), as.factor)

# Datensatz nur zu gebrauchen mit numerischen Werten 
waste_management_numeric <- waste_management[sapply
                                             (waste_management, 
                                               is.numeric)]

# Korrelationsmatrix
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
corrplot(cor_matrix, order = "hclust")

# nur Inselgemeinde anzeigen ??? Ueberfluessig ?? 
# waste_management_numeric %>% 
#   filter(Inselgemeinde == 1)

# Ueberpruefen, ob alle Werte nun numerisch sind 
str(waste_management_numeric)

# Indikator muss 1,2,3 sein, ist der nicht mehr wenn durch mean ersetzt wird
table(waste_management_numeric[,"Geologischer_Indikator"])
waste_management_numeric$Geologischer_Indikator <- 
  round(waste_management_numeric$Geologischer_Indikator,0)
table(waste_management_numeric[,"Geologischer_Indikator"])

#----------------------
# Deskriptive Statistik
#----------------------

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
barplot(hka_ww_management$sdev^2, names.arg = paste0("HK",1:20))

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
var_explained <- round(hka_ww_management$sdev^2/sum(hka_ww_management$sdev^2), 2) 
var_explained
# bspw. die erste HK erklaert 19% der Gesamtvarianz 

# Visualisierung wie viel Gesamtvarianz die HK erklaeren
qplot(c(1:20),var_explained, xlab = "Principal Component", 
      ylab = "Variance Explained")

# 
plot(cumsum(var_explained), xlab="Principal Component", 
     ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1),type='b')
abline(v = 10, col = "red")
abline(h = 0.812, col = "red")
# mit diesem Dataframe in die Clusteranalyse 
# bei einer Auswahl von 10 Hauptkomponenten 
# vielleicht nur 9?
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
# bei 10 Faktoren und keiner Rotation sind es 47% Cumulative Var
# aus den 30 Variablen wurden 5 Variablen (Faktoren) gebildet

# Ergebnisse ausgeben lassen mit 2 Nachkommastellen und Faktorladung < 0.5 
# kleinere Faktorladungen brauchen nicht angeschaut werden
print(fa_waste_management, digits = 2, cutoff=0.5)

fa_promax <- fa_rotation(waste_management_numeric, 10, "promax")
fa_promax
# Bei 10 Faktoren und Rotation Promax sind es 47% Cumulative Var

# Biplot fuer Faktor 1 und 2 
biplot(y = fa_promax$loadings[,1:2], 
       x = fa_promax$scores[,1:2])

# Korrelationsmatrix 
loadings <- t(as.matrix(fa_waste_management$loadings))
corrplot(loadings, method = "circle")

# Ergebnis Faktoranalyse: 
# nur schlechte Interpretation der Faktoren moeglich
# mit der Hauptkomponentenanalyse fortfahren 

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

all_distance(new_waste_management, "manhattan", "ward.D2", 8)

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
cl <- cutree(h, k = 8)

# Cluster an den Datensatz anfuegen
waste_management_dist <- waste_management
waste_management_dist$cluster <- cl

# Anzahl Beobachtungen pro Cluster 
waste_management_dist %>% 
  group_by(cluster) %>% 
  summarise(anzahl = n())

# Inselgemeinde hat genau 22 Eintraege wie Cluster 6
# waste_management %>% 
#   filter(cluster == 6) %>% 
#   select(Inselgemeinde)

# Cluster 6 und 7 entfernt 
# waste_management_entfernt <- waste_management %>% 
#   filter(cluster != 6 & cluster != 7) %>% 
#   select(everything())
waste_management


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

dia_dist <- daisy(waste_management_numeric, metric = "euclidean", stand = F)

dia_agnes <- agnes(dia_dist, diss = T, 
                   metric = "euclidean", 
                   stand = T, 
                   method = "ward")

c_diasy <- cutree(dia_agnes, k = 8)

waste_management_daisy <- waste_management

waste_management_daisy$cluster <- c_diasy

waste_management_daisy %>% 
  group_by(cluster) %>% 
  summarise(anzahl = n())

#
   # k-means
# 
fviz_nbclust(new_waste_management, kmeans, method = "wss")

# optimale Anzahl an Clustern nach Lueckenstatistik
gap_stat <- clusGap(new_waste_management,
                    FUN = kmeans,
                    nstart = 25,
                    K.max = 20,
                    B = 50) # 500 is recommendet, but to long
warnings()
# perform clustering, B = 500 is recommended
hcluster = clusGap(new_waste_management, FUN = hcut, K.max = 20, B = 50)

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
# Optimal 4?

# Plot
fviz_gap_stat(gap_stat)

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

km <- kmeans(scale(waste_management_numeric), centers = 4, nstart = 10)

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

# perform clustering, B = 500 is recommended
# hcluster = clusGap(new_waste_management, FUN = hcut, K.max = 7, B = 500)
# 
# # extract results
# dat <- data.table(hcluster$Tab)
# dat[, k := .I]
# 
# # visualize gap statistic
# viz_gap <- ggplot(dat, aes(k, gap)) + geom_line() + geom_point(size = 3) +
#   geom_errorbar(aes(ymax = gap + SE.sim, ymin = gap - SE.sim), width = 0.25) +
#   ggtitle("Clustering Results") +
#   labs(x = "Number of Clusters", y = "Gap Statistic") +
#   theme(plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
#         axis.title = element_text(size = 12, face = "bold"))
# viz_gap
# 
# km <- kmeans(scale(waste_management_numeric),
#              centers = 4,
#              nstart = 10,
#              iter.max = 20)

# zwischenschritt um cluster an normalen datensatz zu hängen
waste_management_kmeans <- waste_management
cluster_kmeans <- km$cluster
waste_management_kmeans$cluster <- cluster_kmeans

waste_management_kmeans %>% 
  group_by(cluster) %>% 
  summarise_all(mean)

# k-medoids
#------------------------------------------------------------------------------
# Profiling
head(waste_management)


# Cluster nach Verwendung vergleichen
describeBy(waste_management_dist[5:7], 
           group = waste_management_dist$cluster)


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

# 

pdf("mean_cluster.pdf", width = 8.5, height = 6)
for (i in 1:ncol(waste_management_dist)) {
  if (is.numeric(waste_management_dist[,i])) {
    plot <- ggplot(waste_management_dist,
                   aes(y = waste_management_dist[,i],
                       x = cluster)) +
      geom_bar(aes(fill = cluster),stat = "summary",position = "dodge",
               fun = "mean") +
      labs(y = names(waste_management_dist)[i])
    
    print(plot)
  }
}
dev.off()

waste_management_prof <- waste_management_numeric
for (i in 2:8) {
  waste_management_prof$cluster <- cutree(h,k = i)
  waste_profiling <- 
    waste_management_prof %>% 
    group_by(cluster) %>% 
    summarise(mean)
  waste_profiling$ncluster <- i
  waste_profiling$size <- table(waste_management_prof$cluster)
  
  if (i == 2) {
    waste_profiling_final <- waste_profiling
  } else {
    waste_profiling_final <- waste_profiling_final %>% 
      rows_append(waste_profiling)
  }
}
rlang::last_error()
