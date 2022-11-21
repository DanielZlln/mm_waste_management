# Library laden
library(lattice) 
library(dplyr)

# DataFrame laden

load("waste_management.RData")

### Überblick über die Daten

summary(waste_management)
View(waste_management)
str(waste_management) # Küstengemeinde Ja/Nein?
sum(waste_management[1,26:29]) # Spalten zusammen ergeben 100%

table(waste_management$Region)
table(waste_management$Provinz)


durchschnitt_abfall_provinz <- waste_management %>% 
                                    group_by(Provinz) %>% 
                                    summarise(Bevoelkerung = sum(Bevoelkerung),
                                              Gesamt = round(mean(Abfaelle_gesamt),1),
                                              Sortiert = round(mean(Abfaelle_sortiert),1),
                                              Unsortiert = round(mean(Abfaelle_unsortiert),1),
                                              Sortierungsgrad = round(mean(Sortierungsgrad),1)) %>% 
                                    arrange(desc(Gesamt))
# Torino mit Abstand höchsten Quoten, obwohl nicht höchste Bevölkerung


count_dist_dimesnion <- function(df, dimension) {
  df %>% 
    summarise(anzahl_dist = n_distinct(dimension))
}

count_dist_dimesnion(waste_management, waste_management$Region)
count_dist_dimesnion(waste_management, waste_management$Provinz)
count_dist_dimesnion(waste_management, waste_management$Gemeinde)

# Visualisieren
