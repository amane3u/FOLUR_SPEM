# Load required libraries for spatial and visualization tasks
library(sf)
library(ggplot2)
library(dplyr)
library(here)

# Define geographic data path
geo_data_path <- here("data", "map", "Trade_map", "Geodata")

# Load world map data and filter out Antarctica
world <- st_read(here("data", "map", "Trade_map", "Geodata", "ne_110m_admin_0_countries.shp")) # Read world map data
world <- world %>% filter(ADMIN != "Antarctica")  # Exclude Antarctica

# =========================================================================
# SECTION: BEEF TRADE MAP
# =========================================================================

# Filter data for 2020 and select top 10 beef trade flows
df_filtered <- read_xlsx(here("data", "map", "Trade_map", "df_filtered.xlsx"))
df_filtered_2020 <- df_filtered %>% filter(Year == 2020)  # Filter for year 2020
top_10_flows_beef <- df_filtered_2020 %>% top_n(10, wt = abs(Net_Trade))  # Get top 10 flows by absolute trade value
countries_involved_beef <- unique(c(top_10_flows_beef$Exporter_ISO_Code, top_10_flows_beef$Importer_ISO_Code))  # Identify involved countries
world_filtered_beef <- world %>% filter(ISO_A3 %in% countries_involved_beef)  # Filter world map to relevant countries

# Calculate centroids for exporters and importers
exporter_coords_beef <- world %>% filter(ISO_A3 %in% top_10_flows_beef$Exporter_ISO_Code) %>% st_centroid()  # Centroids for exporters
importer_coords_beef <- world %>% filter(ISO_A3 %in% top_10_flows_beef$Importer_ISO_Code) %>% st_centroid()  # Centroids for importers

# Add coordinates to trade flows data
top_10_flows_beef <- top_10_flows_beef %>%
  rowwise() %>%
  mutate(
    exporter_lon = st_coordinates(exporter_coords_beef[exporter_coords_beef$ISO_A3 == Exporter_ISO_Code, ])[, 1],  # Exporter longitude
    exporter_lat = st_coordinates(exporter_coords_beef[exporter_coords_beef$ISO_A3 == Exporter_ISO_Code, ])[, 2],  # Exporter latitude
    importer_lon = st_coordinates(importer_coords_beef[importer_coords_beef$ISO_A3 == Importer_ISO_Code, ])[, 1],  # Importer longitude
    importer_lat = st_coordinates(importer_coords_beef[importer_coords_beef$ISO_A3 == Importer_ISO_Code, ])[, 2]   # Importer latitude
  )

# Create color palette for beef countries
unique_countries_beef <- unique(countries_involved_beef)
country_colors_beef <- setNames(scales::hue_pal()(length(unique_countries_beef)), unique_countries_beef)  # Assign unique colors to countries

# Plot beef trade map
ggplot(data = world) +
  geom_sf(fill = "white", color = "black") +  # Base map with white fill and black borders
  geom_sf(data = world_filtered_beef, aes(fill = ISO_A3), color = "black") +  # Highlight involved countries
  geom_curve(data = top_10_flows_beef, aes(x = exporter_lon, y = exporter_lat, xend = importer_lon, yend = importer_lat, 
                                           linewidth = abs(Net_Trade)/max(abs(Net_Trade))), 
             arrow = arrow(type = "closed", length = unit(0.15, "inches")), color = "red", alpha = 0.6, curvature = 0.3) +  # Add curved arrows
  ggtitle("Top Beef Trade Flows by Country in 2020") +  # Set title
  theme_minimal() +  # Minimal theme for clean look
  theme(
    axis.text = element_blank(),  # Remove axis text
    axis.ticks = element_blank(),  # Remove axis ticks
    panel.grid = element_blank(),  # Remove grid lines
    plot.margin = margin(10, 10, 10, 10),  # Add margins
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "black"),  # Center and style title
    legend.position = "bottom",  # Place legend at bottom
    legend.box = "horizontal",  # Horizontal legend arrangement
    legend.title = element_text(size = 10, face = "bold"),  # Legend title style
    legend.text = element_text(size = 8),  # Legend text style
    legend.key.size = unit(0.5, "cm"),  # Legend key size
    legend.direction = "horizontal",  # Horizontal legend direction
    legend.box.just = "center"  # Center legend
  ) +
  scale_fill_manual(values = country_colors_beef, name = "Countries") +  # Apply country colors
  guides(linewidth = "none")  # Remove legend for line width

# =========================================================================
# SECTION: RICE TRADE MAP
# =========================================================================

# Filter data for 2020 and select top 10 rice trade flows
df_rice_filtered <- read_xlsx(here("data", "map", "Trade_map", "df_rice_filtered.xlsx"))
df_rice_filtered_2020 <- df_rice_filtered %>% filter(Year == 2020)  # Filter for year 2020
top_10_flows_rice <- df_rice_filtered_2020 %>% top_n(10, wt = abs(Net_Trade))  # Get top 10 flows by absolute trade value
countries_involved_rice <- unique(c(top_10_flows_rice$Exporter_ISO_Code, top_10_flows_rice$Importer_ISO_Code))  # Identify involved countries
world_filtered_rice <- world %>% filter(ISO_A3 %in% countries_involved_rice)  # Filter world map to relevant countries

# Calculate centroids for exporters and importers
exporter_coords_rice <- world %>% filter(ISO_A3 %in% top_10_flows_rice$Exporter_ISO_Code) %>% st_centroid()  # Centroids for exporters
importer_coords_rice <- world %>% filter(ISO_A3 %in% top_10_flows_rice$Importer_ISO_Code) %>% st_centroid()  # Centroids for importers

# Add coordinates to trade flows data
top_10_flows_rice <- top_10_flows_rice %>%
  rowwise() %>%
  mutate(
    exporter_lon = st_coordinates(exporter_coords_rice[exporter_coords_rice$ISO_A3 == Exporter_ISO_Code, ])[, 1],  # Exporter longitude
    exporter_lat = st_coordinates(exporter_coords_rice[exporter_coords_rice$ISO_A3 == Exporter_ISO_Code, ])[, 2],  # Exporter latitude
    importer_lon = st_coordinates(importer_coords_rice[importer_coords_rice$ISO_A3 == Importer_ISO_Code, ])[, 1],  # Importer longitude
    importer_lat = st_coordinates(importer_coords_rice[importer_coords_rice$ISO_A3 == Importer_ISO_Code, ])[, 2]   # Importer latitude
  )

# Create color palette for rice countries
unique_countries_rice <- unique(countries_involved_rice)
country_colors_rice <- setNames(scales::hue_pal()(length(unique_countries_rice)), unique_countries_rice)  # Assign unique colors to countries

# Plot rice trade map
ggplot(data = world) +
  geom_sf(fill = "lightgray", color = "black") +  # Base map with light gray fill and black borders
  geom_sf(data = world_filtered_rice, aes(fill = ISO_A3), color = "black") +  # Highlight involved countries
  geom_curve(data = top_10_flows_rice, aes(x = exporter_lon, y = exporter_lat, xend = importer_lon, yend = importer_lat, 
                                           linewidth = abs(Net_Trade)/max(abs(Net_Trade)) * 2), 
             arrow = arrow(type = "closed", length = unit(0.15, "inches")), color = "red", alpha = 0.6, curvature = 0.5) +  # Add curved arrows
  ggtitle("Top Rice Trade Flows by Country in 2020") +  # Set title
  theme_minimal() +  # Minimal theme for clean look
  theme(
    axis.text = element_blank(),  # Remove axis text
    axis.ticks = element_blank(),  # Remove axis ticks
    panel.grid = element_blank(),  # Remove grid lines
    plot.margin = margin(10, 10, 10, 10),  # Add margins
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "black"),  # Center and style title
    legend.position = "bottom",  # Place legend at bottom
    legend.box = "horizontal",  # Horizontal legend arrangement
    legend.title = element_text(size = 10, face = "bold"),  # Legend title style
    legend.text = element_text(size = 8),  # Legend text style
    legend.key.size = unit(0.5, "cm"),  # Legend key size
    legend.direction = "horizontal",  # Horizontal legend direction
    legend.box.just = "left"  # Left-align legend
  ) +
  scale_fill_manual(values = country_colors_rice, name = "Countries") +  # Apply country colors
  guides(linewidth = "none")  # Remove legend for line width



 


