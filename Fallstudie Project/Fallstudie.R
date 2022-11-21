# Packages
install.packages("packcircles")

# Library laden
library(lattice) 
library(dplyr)
library(packcircles)
library(ggplot2)
library(reshape2)

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
    summarise(dist_dimension = n_distinct(dimension))
}

count_dist_dimesnion(waste_management, waste_management$Region)
count_dist_dimesnion(waste_management, waste_management$Provinz)
count_dist_dimesnion(waste_management, waste_management$Gemeinde)

# Visualisieren

barplot(durchschnitt_abfall_provinz$Gesamt~durchschnitt_abfall_provinz$Provinz,
        ylab = "Gesamt Abfälle pro Provinz",
        xlab = "Provinz")

#-- 
# Circle Plot
pck <- circleProgressiveLayout(durchschnitt_abfall_provinz$Gesamt, sizetype = "area")
head(pck)

mydata <- cbind(durchschnitt_abfall_provinz, pck)
head(mydata)

myplotcord <- circleLayoutVertices(pck)

p1 <- ggplot()
p1 <- p1 + geom_polygon(data = myplotcord, aes(x,y, group = id, fill = as.factor(id)), show.legend = F)
p1 <- p1 + geom_text(data = mydata, aes(x,y, size = Gesamt, label = paste0(Provinz)))
p1 <- p1 + coord_equal()
p1 <- p1 + theme_void()
p1 <- p1 + theme(legend.position = "none")
p1 <- p1 + labs(title = "Durchschnittliche Abfallmenge pro Provinz")
p1
#--