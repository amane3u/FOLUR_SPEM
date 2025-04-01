df_beef_chord_filtered<-df_beef_chord_filtered %>%
  filter(Net_Trade_Export!=0)




#MAP FOR BEEF

library(sf)
library(ggplot2)
library(dplyr)

# Charger les données géographiques
geo_data_path <- "C:/Users/Adnane/Desktop/Factsheets/Geodata"
world <- st_read(paste0(geo_data_path, "/ne_110m_admin_0_countries.shp"))
world <- world %>% filter(ADMIN != "Antarctica")

# Filtrer les données pour 2020 et prendre les 10 plus gros flux
df_filtered_2020 <- df_filtered %>% filter(Year == 2020)
top_10_flows <- df_filtered_2020 %>% top_n(10, wt = abs(Net_Trade))
countries_involved <- unique(c(top_10_flows$Exporter_ISO_Code, top_10_flows$Importer_ISO_Code))
world_filtered <- world %>% filter(ISO_A3 %in% countries_involved)

# Centroids pour les pays exportateurs et importateurs
exporter_coords <- world %>% filter(ISO_A3 %in% top_10_flows$Exporter_ISO_Code) %>% st_centroid()
importer_coords <- world %>% filter(ISO_A3 %in% top_10_flows$Importer_ISO_Code) %>% st_centroid()

# Créer un dataframe avec les coordonnées des exportateurs et importateurs
top_10_flows <- top_10_flows %>%
  rowwise() %>%
  mutate(
    exporter_lon = st_coordinates(exporter_coords[exporter_coords$ISO_A3 == Exporter_ISO_Code, ])[, 1],
    exporter_lat = st_coordinates(exporter_coords[exporter_coords$ISO_A3 == Exporter_ISO_Code, ])[, 2],
    importer_lon = st_coordinates(importer_coords[importer_coords$ISO_A3 == Importer_ISO_Code, ])[, 1],
    importer_lat = st_coordinates(importer_coords[importer_coords$ISO_A3 == Importer_ISO_Code, ])[, 2]
  )

# Créer une palette de couleurs pour chaque pays
unique_countries <- unique(countries_involved)
country_colors <- setNames(scales::hue_pal()(length(unique_countries)), unique_countries)

# Créer la carte globale
ggplot(data = world) +
  geom_sf(fill = "white", color = "black") +  # Fond blanc avec contours noirs
  # Colorer chaque pays impliqué avec une couleur différente
  geom_sf(data = world_filtered, 
          aes(fill = ISO_A3),  # Remplissage basé sur le code ISO du pays
          color = "black") +  
  # Ajouter les flèches rouges avec courbure et transparence
  geom_curve(data = top_10_flows, 
             aes(x = exporter_lon, y = exporter_lat, 
                 xend = importer_lon, yend = importer_lat, 
                 linewidth = abs(Net_Trade)/max(abs(Net_Trade))), 
             arrow = arrow(type = "closed", length = unit(0.15, "inches")), 
             color = "red",  # Flèches rouges
             alpha = 0.6,  # Transparence pour éviter l'encombrement
             curvature = 0.3) +  # Courbure légère
  # Titre et thème
  ggtitle("Top Beef Trade Flows by Country in 2020") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "black"),
    # Placer la légende en bas
    legend.position = "bottom",
    legend.box = "horizontal",  # Organiser les éléments de la légende horizontalement
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8),
    # Ajuster l'espacement et l'orientation des étiquettes
    legend.key.size = unit(0.5, "cm"),
    legend.direction = "horizontal",
    legend.box.just = "center"
  ) +
  # Appliquer la palette de couleurs pour les pays et ajouter un titre à la légende
  scale_fill_manual(values = country_colors, name = "Countries") +
  # Supprimer la légende pour l'épaisseur des flèches
  guides(linewidth = "none")



library(sf)
library(dplyr)
library(circlize)

# Charger les données géographiques (pour récupérer les noms des pays si nécessaire)
geo_data_path <- "C:/Users/Adnane/Desktop/Factsheets/Geodata"
world <- st_read(paste0(geo_data_path, "/ne_110m_admin_0_countries.shp"))
world <- world %>% filter(ADMIN != "Antarctica")

# Fonction pour préparer les données et créer un diagramme de cordes esthétique
create_chord_diagram <- function(df, year, title) {
  # Filtrer les données pour l'année spécifiée
  df_year <- df %>% filter(Year == year)
  
  # Prendre tous les flux
  all_flows <- df_year
  
  # Créer une matrice d'adjacence pour les flux (FABLE_Exporter_Mapping → FABLE_Importer_Mapping)
  countries_involved <- unique(c(all_flows$FABLE_Exporter_Mapping, all_flows$FABLE_Importer_Mapping))
  mat <- matrix(0, nrow = length(countries_involved), ncol = length(countries_involved))
  rownames(mat) <- countries_involved
  colnames(mat) <- countries_involved
  
  # Remplir la matrice avec les volumes de Net_Trade
  for (i in 1:nrow(all_flows)) {
    exporter <- all_flows$FABLE_Exporter_Mapping[i]
    importer <- all_flows$FABLE_Importer_Mapping[i]
    volume <- abs(all_flows$Net_Trade[i])
    mat[exporter, importer] <- volume
  }
  
  # Créer une palette de couleurs esthétique (palette pastel pour un rendu élégant)
  country_colors <- setNames(
    scales::hue_pal(h = c(0, 360) + 15, c = 40, l = 75)(length(countries_involved)), 
    countries_involved
  )
  
  # Initialiser le diagramme de cordes
  circos.clear()
  circos.par(gap.after = 3,  # Réduire l'espacement entre les secteurs pour un rendu plus compact
             start.degree = 90,  # Commencer à 90 degrés pour une meilleure orientation
             canvas.xlim = c(-1.2, 1.2), canvas.ylim = c(-1.2, 1.2))  # Ajuster la taille du canevas
  
  # Créer le diagramme de cordes
  chordDiagram(mat,
               grid.col = country_colors,  # Couleurs des secteurs (pays)
               transparency = 0.5,  # Transparence accrue pour éviter l'encombrement
               link.lwd = 1.5,  # Épaisseur des bordures des cordes
               link.border = "black",  # Bordure noire pour les cordes
               link.lty = 1,  # Style de ligne solide
               annotationTrack = "grid",  # Afficher une grille pour les secteurs
               preAllocateTracks = list(track.height = 0.15))  # Hauteur des étiquettes
  
  # Ajouter des étiquettes pour les pays (plus lisibles)
  circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
    sector.name = get.cell.meta.data("sector.index")
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    # Ajuster la position et l'orientation des étiquettes
    circos.text(mean(xlim), ylim[1] + 2, sector.name, 
                facing = "bending.inside",  # Orientation plus lisible
                niceFacing = TRUE, 
                adj = c(0.5, 0),  # Centrer l'étiquette
                cex = 0.9,  # Taille légèrement plus grande
                col = "black",  # Couleur noire pour contraste
                font = 2)  # Police en gras
  }, bg.border = NA)
  
  # Ajouter un fond circulaire pour un rendu esthétique
  circos.trackPlotRegion(track.index = 2, panel.fun = function(x, y) {
    circos.rect(-1, -1, 1, 1, col = "white", border = NA)
  }, bg.border = NA)
  
  # Ajouter un titre
  title(title, cex.main = 1.5, font.main = 2, col.main = "black")
}

# Créer les diagrammes pour 2000 et 2020
# Diagramme pour 2000
create_chord_diagram(df_filtered, 2000, "Beef Trade Flows by Country in 2000")

# Réinitialiser pour le prochain diagramme
circos.clear()

# Diagramme pour 2020
create_chord_diagram(df_filtered, 2020, "Beef Trade Flows by Country in 2020")

#MAP Rice

library(sf)
library(ggplot2)
library(dplyr)

# Charger les données géographiques
geo_data_path <- "C:/Users/Adnane/Desktop/Factsheets/Geodata"
world <- st_read(paste0(geo_data_path, "/ne_110m_admin_0_countries.shp"))
world <- world %>% filter(ADMIN != "Antarctica")

# Filtrer les données pour 2020 et prendre les 10 plus gros flux
df_rice_filtered_2020 <- df_rice_filtered %>% filter(Year == 2020)
top_10_flows_rice <- df_rice_filtered_2020 %>% top_n(10, wt = abs(Net_Trade))
countries_involved <- unique(c(top_10_flows_rice$Exporter_ISO_Code, top_10_flows_rice$Importer_ISO_Code))
world_filtered <- world %>% filter(ISO_A3 %in% countries_involved)

# Centroids pour les pays exportateurs et importateurs
exporter_coords <- world %>% filter(ISO_A3 %in% top_10_flows_rice$Exporter_ISO_Code) %>% st_centroid()
importer_coords <- world %>% filter(ISO_A3 %in% top_10_flows_rice$Importer_ISO_Code) %>% st_centroid()

# Ajouter les coordonnées des exportateurs et importateurs
top_10_flows_rice <- top_10_flows_rice %>%
  rowwise() %>%
  mutate(
    exporter_lon = st_coordinates(exporter_coords[exporter_coords$ISO_A3 == Exporter_ISO_Code, ])[, 1],
    exporter_lat = st_coordinates(exporter_coords[exporter_coords$ISO_A3 == Exporter_ISO_Code, ])[, 2],
    importer_lon = st_coordinates(importer_coords[importer_coords$ISO_A3 == Importer_ISO_Code, ])[, 1],
    importer_lat = st_coordinates(importer_coords[importer_coords$ISO_A3 == Importer_ISO_Code, ])[, 2]
  )

# Créer une palette de couleurs pour chaque pays
unique_countries <- unique(countries_involved)
country_colors <- setNames(scales::hue_pal()(length(unique_countries)), unique_countries)

# Créer la carte globale
ggplot(data = world) +
  geom_sf(fill = "lightgray", color = "black") +  # Fond gris clair pour contraste
  # Colorer chaque pays impliqué avec une couleur différente
  geom_sf(data = world_filtered, 
          aes(fill = ISO_A3),  # Remplissage basé sur le code ISO du pays
          color = "black") +  
  # Ajouter les flèches rouges avec courbure accrue et transparence
  geom_curve(data = top_10_flows_rice, 
             aes(x = exporter_lon, y = exporter_lat, 
                 xend = importer_lon, yend = importer_lat, 
                 linewidth = abs(Net_Trade)/max(abs(Net_Trade)) * 2),  # Ajuster l'échelle de l'épaisseur
             arrow = arrow(type = "closed", length = unit(0.15, "inches")), 
             color = "red",  # Flèches rouges
             alpha = 0.6,  # Transparence pour éviter l'encombrement
             curvature = 0.5) +  # Courbure accrue pour séparer les flèches
  # Titre et thème
  ggtitle("Top Rice Trade Flows by Country in 2020") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "black"),
    # Placer la légende en bas
    legend.position = "bottom",
    legend.box = "horizontal",  # Organiser les éléments de la légende horizontalement
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8),
    # Ajuster l'espacement et l'orientation des étiquettes
    legend.key.size = unit(0.5, "cm"),
    legend.direction = "horizontal",
    legend.box.just = "left"
  ) +
  # Appliquer la palette de couleurs pour les pays et ajouter un titre à la légende
  scale_fill_manual(values = country_colors, name = "Countries") +
  # Supprimer la légende pour l'épaisseur des flèches
  guides(linewidth = "none")



 


