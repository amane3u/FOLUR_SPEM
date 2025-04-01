library(dplyr)

Beef_BACI <- Beef_BACI %>%
  group_by(FABLE_Exporter_Mapping, FABLE_Importer_Mapping, FABLE_Item) %>%
  mutate(
    # Calcul des bornes IQR pour les exportations et détection des outliers
    Q1 = quantile(Export, 0.25, na.rm = TRUE),
    Q3 = quantile(Export, 0.75, na.rm = TRUE),
    IQR_Export = Q3 - Q1,
    lower_bound_export = Q1 - 1.5 * IQR_Export,
    upper_bound_export = Q3 + 1.5 * IQR_Export,
    
    # Détection des outliers pour les exportations
    Export_outlier = Export < lower_bound_export | Export > upper_bound_export
  ) %>%
  
  # Tri des données par année pour garantir une séquence temporelle correcte
  arrange(Year) %>%
  
  # Correction des outliers
  mutate(
    Export_corrected = if_else(
      Export_outlier,
      {
        # Trouver les années voisines disponibles pour interpolation
        available_years = sort(unique(Year[!is.na(Export)]))
        
        # Trouver les années avant et après l'année en question
        years_before = available_years[available_years < Year]
        years_after = available_years[available_years > Year]
        
        # Si des années avant et après existent, faire une interpolation entre ces années
        if (length(years_before) > 0 & length(years_after) > 0) {
          # Calculer les valeurs des années avant et après
          prev_export = Export[Year %in% years_before]
          next_export = Export[Year %in% years_after]
          
          # Interpolation simple en utilisant la moyenne des années voisines
          interpolated_value = (mean(prev_export) + mean(next_export)) / 2
        } else if (length(years_before) > 0) {
          # Si seules les années avant existent, utiliser la moyenne des années précédentes
          prev_export = Export[Year %in% years_before]
          interpolated_value = mean(prev_export)
        } else if (length(years_after) > 0) {
          # Si seules les années après existent, utiliser la moyenne des années suivantes
          next_export = Export[Year %in% years_after]
          interpolated_value = mean(next_export)
        } else {
          # Si aucune année voisine, utiliser la moyenne des autres valeurs disponibles dans le groupe
          group_mean = mean(Export, na.rm = TRUE)
          interpolated_value = group_mean
        }
        
        interpolated_value
      },
      Export # Si ce n'est pas un outlier, garder la valeur originale
    )
  ) %>%
  ungroup()





# Fonction de correction des valeurs aberrantes
correct_aberrations <- function(Beef_BACI) {
  Beef_BACI %>%
    group_by(FABLE_Exporter_Mapping, FABLE_Importer_Mapping, FABLE_Item) %>%
    mutate(
      # Détection des bornes IQR pour Export
      Q1_export = quantile(Export, 0.25, na.rm = TRUE),
      Q3_export = quantile(Export, 0.75, na.rm = TRUE),
      IQR_Export = Q3_export - Q1_export,
      lower_bound_export = Q1_export - 1.5 * IQR_Export,
      upper_bound_export = Q3_export + 1.5 * IQR_Export,
      Export_outlier = Export < lower_bound_export | Export > upper_bound_export,
      
      # Détection des bornes IQR pour Import
      Q1_import = quantile(Import, 0.25, na.rm = TRUE),
      Q3_import = quantile(Import, 0.75, na.rm = TRUE),
      IQR_Import = Q3_import - Q1_import,
      lower_bound_import = Q1_import - 1.5 * IQR_Import,
      upper_bound_import = Q3_import + 1.5 * IQR_Import,
      Import_outlier = Import < lower_bound_import | Import > upper_bound_import
    ) %>%
    arrange(Year) %>%
    mutate(
      # Correction des Exportations
      Export_corrected = case_when(
        # Si la valeur n'est pas aberrante, la garder
        !Export_outlier ~ Export,
        
        # Si des années avant et après existent, lisser avec la moyenne
        lag(!Export_outlier) & lead(!Export_outlier) ~ 
          (lag(Export, default = NA) + lead(Export, default = NA)) / 2,
        
        # Si une seule année voisine est disponible, appliquer la médiane du groupe
        lag(!Export_outlier) | lead(!Export_outlier) ~ 
          median(Export[!Export_outlier], na.rm = TRUE),
        
        # Si aucune année voisine n'est disponible, appliquer la médiane du groupe
        TRUE ~ median(Export[!Export_outlier], na.rm = TRUE)
      ),
      
      # Correction des Importations
      Import_corrected = case_when(
        # Si la valeur n'est pas aberrante, la garder
        !Import_outlier ~ Import,
        
        # Si des années avant et après existent, lisser avec la moyenne
        lag(!Import_outlier) & lead(!Import_outlier) ~ 
          (lag(Import, default = NA) + lead(Import, default = NA)) / 2,
        
        # Si une seule année voisine est disponible, appliquer la médiane du groupe
        lag(!Import_outlier) | lead(!Import_outlier) ~ 
          median(Import[!Import_outlier], na.rm = TRUE),
        
        # Si aucune année voisine n'est disponible, appliquer la médiane du groupe
        TRUE ~ median(Import[!Import_outlier], na.rm = TRUE)
      )
    ) %>%
    ungroup()
}
Beef_BACI_corrected <- correct_aberrations(Beef_BACI)


Beef_BACI_cleaned <- Beef_BACI_corrected %>%
  select(FABLE_Exporter_Mapping, FABLE_Importer_Mapping, FABLE_Item, Year, 
         Export, Import, Export_corrected, Import_corrected) %>%
  mutate(Net_Trade_BACI_corrected = Export_corrected - Import_corrected)


colnames(Beef_BACI_cleaned)
colnames(Beef_FAO)


head(Beef_BACI_cleaned)
head(Beef_FAO)

# Agréger les flux bilatéraux pour obtenir une structure unilatérale
aggregate_baci <- Beef_BACI_cleaned %>%
  group_by(FABLE_Exporter_Mapping, FABLE_Item, Year) %>%
  summarise(
    Export_aggregated = sum(Export_corrected, na.rm = TRUE),
    Import_aggregated = sum(Import_corrected, na.rm = TRUE),
    Net_Trade_BACI_corrected_aggregated = sum(Net_Trade_BACI_corrected, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(
    country = FABLE_Exporter_Mapping,
    product = FABLE_Item
  )

# Résultat : structure similaire à FAO
head(aggregate_baci)

# 1. Joindre les données FAO et BACI agrégées
joined_data <- aggregate_baci %>%
  left_join(
    Beef_FAO %>%
      rename(product = product, year = year),
    by = c("country", "product", "Year" = "year")
  )

# 2. Calculer le ratio entre BACI et FAO pour le Net Trade
joined_data <- joined_data %>%
  mutate(
    ratio = Net_Trade_BACI_corrected_aggregated / Net_Trade_FAO,
    adjustment_factor = case_when(
      ratio < 0.89 ~ 0.89 / ratio, # Ratio trop petit
      ratio > 1.13 ~ 1.13 / ratio, # Ratio trop grand
      TRUE ~ 1 # Ratio acceptable
    )
  )

# 3. Ajuster les flux bilatéraux proportionnellement
# Joindre les facteurs d'ajustement aux données bilatérales initiales
adjusted_bilateral <- Beef_BACI_cleaned %>%
  left_join(
    joined_data %>%
      select(country, product, Year, adjustment_factor),
    by = c("FABLE_Exporter_Mapping" = "country", "FABLE_Item" = "product", "Year")
  ) %>%
  mutate(
    Export_corrected_adjusted = Export_corrected * adjustment_factor,
    Import_corrected_adjusted = Import_corrected * adjustment_factor,
    Net_Trade_BACI_corrected_adjusted = Export_corrected_adjusted - Import_corrected_adjusted
  )

# Résultat final : données bilatérales ajustées
adjusted_bilateral_final <- adjusted_bilateral %>%
  select(
    FABLE_Exporter_Mapping, FABLE_Importer_Mapping, FABLE_Item, Year,
    Export_corrected_adjusted, Import_corrected_adjusted, Net_Trade_BACI_corrected_adjusted
  )

# Aperçu des données ajustées
head(adjusted_bilateral_final)

colnames(adjusted_bilateral_final)




aggregate_adjusted_bilateral_final <- adjusted_bilateral_final %>%
  group_by(FABLE_Exporter_Mapping, FABLE_Item, Year) %>%
  summarise(
    Export_aggregated = sum(Export_corrected_adjusted, na.rm = TRUE),
    Import_aggregated = sum(Import_corrected_adjusted, na.rm = TRUE),
    Net_Trade_BACI_corrected_aggregated = sum(Net_Trade_BACI_corrected_adjusted, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(
    country = FABLE_Exporter_Mapping,
    product = FABLE_Item
  )
joined_check_data <- aggregate_adjusted_bilateral_final %>%
  left_join(
    Beef_FAO %>%
      rename(product = product, year = year),
    by = c("country", "product", "Year" = "year")
  )

joined_check_data <- joined_check_data %>%
  mutate(
    ratio = Net_Trade_BACI_corrected_aggregated / Net_Trade_FAO)


# Désactiver la notation scientifique pour tous les affichages numériques
options(scipen = 999)

# Changer le format des colonnes spécifiques
adjusted_bilateral_final$Export_corrected_adjusted <- format(adjusted_bilateral_final$Export_corrected_adjusted, scientific = FALSE)
adjusted_bilateral_final$Import_corrected_adjusted <- format(adjusted_bilateral_final$Import_corrected_adjusted, scientific = FALSE)
adjusted_bilateral_final$Net_Trade_BACI_corrected_adjusted <- format(adjusted_bilateral_final$Net_Trade_BACI_corrected_adjusted, scientific = FALSE)

# Exporter le dataframe modifié en CSV
write.csv(adjusted_bilateral_final, "modified_data.csv", row.names = FALSE)

write.csv(FABLE_Products_WA_VF3, "Bileteral_Trade_Flows_v1_241203.csv", row.names = FALSE)



# Définir le chemin du fichier
file_path <- "C:/Users/Adnane/Downloads/2023-trade-CT-Yes.json.br"

# Lire le fichier .br en tant que données brutes
compressed_data <- readBin(file_path, "raw", file.info(file_path)$size)

# Décompresser les données
decompressed_data <- brotli::brotli_decompress(compressed_data)

# Lire les données JSON
json_data <- fromJSON(rawToChar(decompressed_data))

# Afficher le contenu
print(json_data)




# Définir le chemin du fichier
file_path_2 <- "C:/Users/Adnane/Downloads/2023-trade-CT-No.json.br"

# Lire le fichier .br en tant que données brutes
compressed_data_2 <- readBin(file_path_2, "raw", file.info(file_path_2)$size)

# Décompresser les données
decompressed_data_2 <- brotli::brotli_decompress(compressed_data_2)

# Lire les données JSON
json_data_2 <- fromJSON(rawToChar(decompressed_data_2))

# Afficher le contenu
print(json_data_2)


file_path_3 <- "C:/Users/Adnane/Downloads/2023-trade-NC-Yes.json.br"
file_path_4 <- "C:/Users/Adnane/Downloads/2023-trade-GS-Yes.json.br"

# Lire et décompresser le fichier "2023-trade-NC-Yes.json.br"
compressed_data_3 <- readBin(file_path_3, "raw", file.info(file_path_3)$size)
decompressed_data_3 <- brotli::brotli_decompress(compressed_data_3)
json_data_3 <- fromJSON(rawToChar(decompressed_data_3))

# Lire et décompresser le fichier "2023-trade-GS-Yes.json.br"
compressed_data_4 <- readBin(file_path_4, "raw", file.info(file_path_4)$size)
decompressed_data_4 <- brotli::brotli_decompress(compressed_data_4)
json_data_4 <- fromJSON(rawToChar(decompressed_data_4))

# Afficher les contenus
print(json_data_3)
print(json_data_4)


file_path_5 <- "C:/Users/Adnane/Downloads/2023-ghg-CT-No.json.br"

# Lire et décompresser le fichier "2023-ghg-CT-No.json.br"
compressed_data_5 <- readBin(file_path_5, "raw", file.info(file_path_5)$size)
decompressed_data_5 <- brotli::brotli_decompress(compressed_data_5)
json_data_5 <- fromJSON(rawToChar(decompressed_data_5))

# Afficher le contenu
print(json_data_5)


rm(list = ls())