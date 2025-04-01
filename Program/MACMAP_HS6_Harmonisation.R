# Liste des codes
matching_file <- "C:/Users/Adnane/Desktop/BACI-MacMap/MAC-MAPHS6/Matching_HS6_to_name product.xlsx"
matching_data <- read_excel(matching_file)



codes <- as.character(unique(matching_data$Code))



# Chemin du répertoire
path <- "C:/Users/Adnane/Desktop/BACI-MacMap/MAC-MAPHS6/Raw data/Replic_FGO"

# Obtenir la liste de tous les fichiers CSV dans le répertoire
files <- list.files(path, pattern = "\\.csv$", full.names = TRUE)

# Filtrer les fichiers qui contiennent un des codes dans leur nom
matching_files <- files[sapply(files, function(file) any(grepl(paste(codes, collapse = "|"), file)))]

# Charger chaque fichier dans l'environnement avec un nom basé sur le fichier
for (file in matching_files) {
  # Extraire le nom de base du fichier sans extension
  file_name <- tools::file_path_sans_ext(basename(file))
  
  # Charger le fichier dans l'environnement avec sep=";" pour corriger le format
  assign(file_name, read.csv(file, sep = ";"))
}

# Résumé des objets chargés
cat("Fichiers suivants chargés dans l'environnement :\n")
print(ls())

file_path <- "C:/Users/Adnane/Desktop/BACI-MacMap/MAC-MAPHS6/Raw data/Tariffs_2001_2019/mmhs6_2019.csv"

# Charger les données du fichier CSV
Sigma_2019 <- read.csv(file_path, 
                       header = TRUE,  # Assure que la première ligne est l'en-tête
                       sep = ",",      # Séparateur de colonnes
                       stringsAsFactors = FALSE,  # Évite la conversion automatique en facteurs
                       encoding = "UTF-8")  # Encodage UTF-8 pour les caractères spéciaux




# Liste des noms des dataframes dans l'environnement
dataframes <- c(
  "Sigma_HS6_020110", "Sigma_HS6_020120", "Sigma_HS6_020130",
  "Sigma_HS6_020210", "Sigma_HS6_020220", "Sigma_HS6_020230",
  "Sigma_HS6_021020", "Sigma_HS6_090111", "Sigma_HS6_090112",
  "Sigma_HS6_090121", "Sigma_HS6_090122", "Sigma_HS6_100110",
  "Sigma_HS6_100190", "Sigma_HS6_100510", "Sigma_HS6_100590",
  "Sigma_HS6_100610", "Sigma_HS6_100620", "Sigma_HS6_100630",
  "Sigma_HS6_100640", "Sigma_HS6_110100", "Sigma_HS6_110220",
  "Sigma_HS6_110311", "Sigma_HS6_110313", "Sigma_HS6_110423",
  "Sigma_HS6_110811", "Sigma_HS6_110812", "Sigma_HS6_110900",
  "Sigma_HS6_120100", "Sigma_HS6_120810", "Sigma_HS6_120890",
  "Sigma_HS6_150200", "Sigma_HS6_151110", "Sigma_HS6_151190",
  "Sigma_HS6_160100", "Sigma_HS6_160210", "Sigma_HS6_160250",
  "Sigma_HS6_180100", "Sigma_HS6_180200",
  "Sigma_HS6_180310", "Sigma_HS6_180320", "Sigma_HS6_180500",
  "Sigma_HS6_180610", "Sigma_HS6_180620", "Sigma_HS6_180631",
  "Sigma_HS6_180632", "Sigma_HS6_180690", "Sigma_HS6_190120",
  "Sigma_HS6_190190", "Sigma_HS6_190211", "Sigma_HS6_190219",
  "Sigma_HS6_190410", "Sigma_HS6_190490", "Sigma_HS6_190510",
  "Sigma_HS6_190520", "Sigma_HS6_190531", "Sigma_HS6_190532",
  "Sigma_HS6_210111", "Sigma_HS6_210112", "Sigma_HS6_210130",
  "Sigma_HS6_210310", "Sigma_HS6_230210", "Sigma_HS6_110290"
)

# Boucle pour ajouter les colonnes "Item" et "HS6_Code92" dans chaque dataframe
for (df_name in dataframes) {
  # Vérifier si le dataframe existe dans l'environnement
  if (exists(df_name)) {
    # Récupérer le dataframe
    df <- get(df_name)
    
    # Extraire le code HS6 depuis le nom du dataframe
    code <- gsub(".*_(\\d{6})", "\\1", df_name)
    
    # Trouver le nom du produit et le code HS6_92 correspondant dans le fichier de matching
    product_name <- matching_data$Product[matching_data$Code == code]
    hs6_code_92 <- matching_data$HS6_Code_92[matching_data$Code == code]
    
    # Ajouter les colonnes "Item" et "HS6_Code92"
    df$Item <- ifelse(length(product_name) > 0, product_name, NA)
    df$HS6_Code92 <- ifelse(length(hs6_code_92) > 0, hs6_code_92, NA)
    
    # Réassigner le dataframe modifié dans l'environnement
    assign(df_name, df)
  }
}

# Message de fin
cat("Les colonnes 'Item' et 'HS6_Code92' ont été ajoutées à tous les dataframes.")


# For 2019

Sigma_2019$hs6_2007 <- sprintf("%06d", as.integer(Sigma_2019$hs6_2007))
Sigma_2019_vf1 <- Sigma_2019[Sigma_2019$hs6_2007 %in% codes, ]


Sigma_2019_vf1$hs6_2007 <- as.character(Sigma_2019_vf1$hs6_2007)

# Créer les colonnes HS6_Code92 et Item en utilisant le matching
Sigma_2019_vf1$HS6_Code92 <- sapply(Sigma_2019_vf1$hs6_2007, function(x) {
  # Trouver la ligne dans le fichier de matching pour le code HS6
  match_row <- matching_data[matching_data$Code == x, ]
  
  # Si un match est trouvé, retourner le code HS92 correspondant
  if (nrow(match_row) > 0) {
    return(match_row$HS6_Code_92)
  } else {
    return(NA) # Sinon, retourner NA
  }
})

Sigma_2019_vf1$Item <- sapply(Sigma_2019_vf1$hs6_2007, function(x) {
  # Trouver la ligne dans le fichier de matching pour le code HS6
  match_row <- matching_data[matching_data$Code == x, ]
  
  # Si un match est trouvé, retourner le nom du produit
  if (nrow(match_row) > 0) {
    return(match_row$Product)
  } else {
    return(NA) # Sinon, retourner NA
  }
})


# Message pour confirmer l'ajout des colonnes
cat("Les colonnes 'HS6_Code92' et 'Item' ont été ajoutées au dataframe.")

unique(Sigma_2019_vf1$Item)

Sigma_2019_NA <- Sigma_2019_vf1[is.na(Sigma_2019$Item), ]



# Liste des dataframes à traiter
dataframes_2 <- c(
  "Sigma_HS6_020110", "Sigma_HS6_020120", "Sigma_HS6_020130",
  "Sigma_HS6_020210", "Sigma_HS6_020220", "Sigma_HS6_020230",
  "Sigma_HS6_021020", "Sigma_HS6_090111", "Sigma_HS6_090112",
  "Sigma_HS6_090121", "Sigma_HS6_090122", "Sigma_HS6_100110",
  "Sigma_HS6_100190", "Sigma_HS6_100510", "Sigma_HS6_100590",
  "Sigma_HS6_100610", "Sigma_HS6_100620", "Sigma_HS6_100630",
  "Sigma_HS6_100640", "Sigma_HS6_110100", "Sigma_HS6_110220",
  "Sigma_HS6_110311", "Sigma_HS6_110313", "Sigma_HS6_110423",
  "Sigma_HS6_110811", "Sigma_HS6_110812", "Sigma_HS6_110900",
  "Sigma_HS6_120100", "Sigma_HS6_120810", "Sigma_HS6_120890",
  "Sigma_HS6_150200", "Sigma_HS6_151110", "Sigma_HS6_151190",
  "Sigma_HS6_160100", "Sigma_HS6_160210", "Sigma_HS6_160250",
  "Sigma_HS6_180100", "Sigma_HS6_180200", "Sigma_HS6_110290",
  "Sigma_HS6_180310", "Sigma_HS6_180320", "Sigma_HS6_180500",
  "Sigma_HS6_180610", "Sigma_HS6_180620", "Sigma_HS6_180631",
  "Sigma_HS6_180632", "Sigma_HS6_180690", "Sigma_HS6_190120",
  "Sigma_HS6_190190", "Sigma_HS6_190211", "Sigma_HS6_190219",
  "Sigma_HS6_190410", "Sigma_HS6_190490", "Sigma_HS6_190510",
  "Sigma_HS6_190520", "Sigma_HS6_190531", "Sigma_HS6_190532",
  "Sigma_HS6_210111", "Sigma_HS6_210112", "Sigma_HS6_210130",
  "Sigma_HS6_210310", "Sigma_HS6_230210", "Sigma_2019_vf1" 
  
)

# Boucle pour ajouter la colonne year_baseline
for (df_name in dataframes_2) {
  # Vérifier si le dataframe existe dans l'environnement
  if (exists(df_name)) {
    # Charger le dataframe
    df <- get(df_name)
    
    # Vérifier si la colonne 'year' existe dans le dataframe
    if ("year" %in% colnames(df)) {
      # Ajouter la colonne year_baseline avec la correspondance demandée
      df$year_baseline <- ifelse(df$year == 2001, 2000,
                                 ifelse(df$year == 2004, 2005,
                                        ifelse(df$year == 2010, 2010,
                                               ifelse(df$year == 2016, 2015,
                                                      ifelse(df$year == 2019, 2020, df$year)))))
      
      # Réassigner le dataframe modifié dans l'environnement
      assign(df_name, df)
    } else {
      cat(paste("La colonne 'year' est absente dans", df_name, "\n"))
    }
  } else {
    cat(paste("Le dataframe", df_name, "n'existe pas dans l'environnement\n"))
  }
}

# Message de fin
cat("La colonne 'year_baseline' a été ajoutée à tous les dataframes.\n")



# Filtrer les dataframes de la liste 'dataframes' sur les années de 'year_baseline'
years_to_keep <- c(2000, 2005, 2010, 2015)

for (df_name in dataframes) {
  if (exists(df_name)) {
    df <- get(df_name)
    if ("year_baseline" %in% colnames(df)) {
      assign(df_name, df[df$year_baseline %in% years_to_keep, ])
    }
  }
}

unique(Sigma_HS6_020110$year_baseline)

country_code <- read.csv("C:/Users/Adnane/Desktop/BACI-MacMap/BACI/country_codes_V202401b.csv", 
                         header = TRUE, 
                         stringsAsFactors = FALSE)





# Créer les colonnes 'FABLE_Exporter' et 'FABLE_Importer' dans chaque dataframe de la liste 'dataframes'
for (df_name in dataframes) {
  # Vérifier si le dataframe existe
  if (exists(df_name)) {
    # Récupérer le dataframe
    df <- get(df_name)
    
    # Vérifier que les colonnes 'i' et 'j' existent
    if (all(c("i", "j") %in% colnames(df))) {
      
      # Ajouter FABLE_Exporter pour la colonne 'i' en fonction du match avec 'ISO3_code'
      df$FABLE_Exporter <- Country_mapping_250120_updated_2$FABLE_cc[match(df$i, Country_mapping_250120_updated_2$ISO3_code)]
      
      # Ajouter FABLE_Importer pour la colonne 'j' en fonction du match avec 'ISO3_code'
      df$FABLE_Importer <- Country_mapping_250120_updated_2$FABLE_cc[match(df$j, Country_mapping_250120_updated_2$ISO3_code)]
      
      # Réassigner le dataframe modifié dans l'environnement
      assign(df_name, df)
    } else {
      warning(paste("Les colonnes 'i' et/ou 'j' n'existent pas dans", df_name))
    }
  }
}

# Message de fin
cat("Les colonnes 'FABLE_Exporter' et 'FABLE_Importer' ont été ajoutées à chaque dataframe.")


# Filtrer les dataframes en fonction des valeurs de la colonne 'ISO3_code' dans le fichier 'Country_mapping_250120_updated_2'
for (df_name in dataframes) {
  # Vérifier si le dataframe existe
  if (exists(df_name)) {
    # Récupérer le dataframe
    df <- get(df_name)
    
    # Vérifier que les colonnes 'i' et 'j' existent
    if (all(c("i", "j") %in% colnames(df))) {
      
      # Filtrer les lignes où les valeurs de 'i' et 'j' existent dans 'ISO3_code'
      df <- df[df$i %in% Country_mapping_250120_updated_2$ISO3_code & df$j %in% Country_mapping_250120_updated_2$ISO3_code, ]
      
      # Réassigner le dataframe filtré dans l'environnement
      assign(df_name, df)
    } else {
      warning(paste("Les colonnes 'i' et/ou 'j' n'existent pas dans", df_name))
    }
  }
}

# Message de fin
cat("Les dataframes ont été filtrés en fonction des valeurs de la colonne 'ISO3_code'.")



# Vérifier si le dataframe 'Sigma_2019_vf1' existe
if (exists("Sigma_2019_vf1")) {
  # Vérifier que les colonnes 'i' et 'j' existent
  if (all(c("importer", "exporter") %in% colnames(Sigma_2019))) {
    
    # Ajouter FABLE_Exporter pour la colonne 'i' en fonction du match avec 'ISO3_code'
    Sigma_2019_vf1$FABLE_Exporter <- Country_mapping_250120_updated_2$FABLE_cc[
      match(Sigma_2019_vf1$exporter, Country_mapping_250120_updated_2$ISO3_code)
    ]
    
    # Ajouter FABLE_Importer pour la colonne 'j' en fonction du match avec 'ISO3_code'
    Sigma_2019_vf1$FABLE_Importer <- Country_mapping_250120_updated_2$FABLE_cc[
      match(Sigma_2019_vf1$importer, Country_mapping_250120_updated_2$ISO3_code)
    ]
    
    # Filtrer les lignes où les valeurs de 'i' et 'j' existent dans 'ISO3_code'
    Sigma_2019_vf1 <- Sigma_2019_vf1[
      Sigma_2019_vf1$exporter %in% Country_mapping_250120_updated_2$ISO3_code &
        Sigma_2019_vf1$importer %in% Country_mapping_250120_updated_2$ISO3_code, 
    ]
    
    # Message de succès
    cat("Les colonnes 'FABLE_Exporter' et 'FABLE_Importer' ont été ajoutées et le dataframe 'Sigma_2019_vf1' a été filtré.\n")
  } else {
    warning("Les colonnes 'i' et/ou 'j' n'existent pas dans 'Sigma_2019'.")
  }
} else {
  warning("Le dataframe 'Sigma_2019_vf1' n'existe pas.")
}

# Fusionner tous les dataframes de la liste en un seul
merged_dataframe <- do.call(rbind, lapply(dataframes, get))
colnames(merged_dataframe)
# Afficher un message de succès
cat("Tous les dataframes ont été fusionnés avec succès.\n")


if (exists("merged_dataframe")) {
  na_items_df <- merged_dataframe[is.na(merged_dataframe$Item), ]
  cat("Un nouveau dataframe 'na_items_df' a été créé avec les lignes contenant des NA dans la colonne 'Item'.\n")
} else {
  warning("Le dataframe 'merged_dataframe' n'existe pas.")
}


#TUV (Trade Unites Values FOB (Free On Board) from CEPII)

TUV_country_codes <- read.csv("C:\\Users\\Adnane\\Desktop\\BACI-MacMap\\MAC-MAPHS6\\Raw data\\TUV\\TUV_country_codes_V202104.csv", sep = ",", fileEncoding = "UTF-8", header = TRUE)

TUV_HS07_x_Y2000 <- read.csv("C:\\Users\\Adnane\\Desktop\\BACI-MacMap\\MAC-MAPHS6\\Raw data\\TUV\\TUV_HS07_x_Y2000_V202104.csv", sep = ",", fileEncoding = "UTF-8", header = TRUE)

TUV_HS07_x_Y2005 <- read.csv("C:\\Users\\Adnane\\Desktop\\BACI-MacMap\\MAC-MAPHS6\\Raw data\\TUV\\TUV_HS07_x_Y2005_V202104.csv", sep = ",", fileEncoding = "UTF-8", header = TRUE)

TUV_HS07_x_Y2010 <- read.csv("C:\\Users\\Adnane\\Desktop\\BACI-MacMap\\MAC-MAPHS6\\Raw data\\TUV\\TUV_HS07_x_Y2010_V202104.csv", sep = ",", fileEncoding = "UTF-8", header = TRUE)

TUV_HS07_x_Y2015 <- read.csv("C:\\Users\\Adnane\\Desktop\\BACI-MacMap\\MAC-MAPHS6\\Raw data\\TUV\\TUV_HS07_x_Y2015_V202104.csv", sep = ",", fileEncoding = "UTF-8", header = TRUE)

TUV_HS07_x_Y2019 <- read.csv("C:\\Users\\Adnane\\Desktop\\BACI-MacMap\\MAC-MAPHS6\\Raw data\\TUV\\TUV_HS07_x_Y2019_V202104.csv", sep = ",", fileEncoding = "UTF-8", header = TRUE)
colnames(TUV_country_codes)


# Liste des dataframes à traiter
data.frame_TUV <- c("TUV_HS07_x_Y2000", "TUV_HS07_x_Y2005", "TUV_HS07_x_Y2010", 
                    "TUV_HS07_x_Y2015", "TUV_HS07_x_Y2019")

# Ajouter la colonne ISO3_code_Export dans chaque dataframe
for (df_name in data.frame_TUV) {
  # Obtenir le dataframe à partir de son nom
  df <- get(df_name)
  
  # Ajouter la colonne ISO3_code en effectuant une jointure avec TUV_country_codes
  df$ISO3_code_Exporter <- TUV_country_codes$iso3_digit_alpha[match(df$r, TUV_country_codes$country_code)]
  
  # Réassigner le dataframe modifié à son nom d'origine
  assign(df_name, df)
}

# Ajouter la colonne ISO3_code_importer dans chaque dataframe
for (df_name in data.frame_TUV) {
  # Obtenir le dataframe à partir de son nom
  df <- get(df_name)
  
  # Ajouter la colonne ISO3_code en effectuant une jointure avec TUV_country_codes
  df$ISO3_code_Importer <- TUV_country_codes$iso3_digit_alpha[match(df$p, TUV_country_codes$country_code)]
  
  # Réassigner le dataframe modifié à son nom d'origine
  assign(df_name, df)
}
#Year_baseline selection for 2019

# Vérifier si le dataframe "TUV_HS07_x_Y2019" existe dans l'environnement
if (exists("TUV_HS07_x_Y2019")) {
  # Charger le dataframe
  df <- TUV_HS07_x_Y2019
  
  # Vérifier si la colonne 'year' existe dans le dataframe
  if ("t" %in% colnames(df)) {
    # Ajouter la colonne year_baseline avec la correspondance demandée
    df$year_baseline <- ifelse(df$t == 2019, 2020, df$t)
    
    # Réassigner le dataframe modifié dans l'environnement
    TUV_HS07_x_Y2019 <- df
  } else {
    cat("La colonne 'year' est absente dans le dataframe TUV_HS07_x_Y2019\n")
  }
} else {
  cat("Le dataframe TUV_HS07_x_Y2019 n'existe pas dans l'environnement\n")
}

TUV_HS07_x_Y2019<-TUV_HS07_x_Y2019 %>% select(year_baseline,r,p,k,uv,ISO3_code_Exporter,ISO3_code_Importer)
TUV_HS07_x_Y2019 <- TUV_HS07_x_Y2019 %>% 
  rename(t = year_baseline)



# Product filter HS6_code add product Item" et "HS6_Code92"

for (df_name in data.frame_TUV) {
  if (exists(df_name)) {
    df <- get(df_name)
    if ("k" %in% colnames(df)) {
      # Ajouter un 0 devant les codes de la colonne 'k' si la longueur est inférieure à 6
      df$k <- ifelse(nchar(as.character(df$k)) < 6, paste0("0", as.character(df$k)), as.character(df$k))
      assign(df_name, df)
    }
  }
}


# Step1 product matching
for (df_name in data.frame_TUV) {
  if (exists(df_name)) {
    df <- get(df_name)
    if ("k" %in% colnames(df)) {
      # Filtrer la colonne 'k' en fonction de la colonne 'Code' de 'matching_data'
      df_filtered <- df[df$k %in% matching_data$Code, ]
      
      # Ajouter la colonne 'HS6_Code92' avec la valeur correspondante de 'HS6_Code_92'
      df_filtered$HS6_Code92 <- matching_data$HS6_Code_92[match(df_filtered$k, matching_data$Code)]
      df_filtered$Item<- matching_data$Product[match(df_filtered$k, matching_data$Code)]
      # Réassigner le dataframe filtré et mis à jour
      assign(df_name, df_filtered)
    }
  }
}


#Matching-X FABLE
for (df_name in data.frame_TUV) {
  if (exists(df_name)) {
    df <- get(df_name)
    if ("ISO3_code_Exporter" %in% colnames(df)) {
      # Filtrer la colonne 'ISO3_code' en fonction de la colonne 'ISO3_code' de 'Country_mapping_250120_updated_2'
      df_filtered <- df[df$ISO3_code_Exporter %in% Country_mapping_250120_updated_2$ISO3_code, ]
      
      # Créer la colonne 'FABLE_match_ij' avec la correspondance de 'FABLE_cc' à partir de 'Country_mapping_250120_updated_2'
      df_filtered$FABLE_matching_Exporter <- Country_mapping_250120_updated_2$FABLE_cc[match(df_filtered$ISO3_code_Exporter, Country_mapping_250120_updated_2$ISO3_code)]
      
      # Réassigner le dataframe filtré et mis à jour
      assign(df_name, df_filtered)
    }
  }
}

#Matching-M FABLE
for (df_name in data.frame_TUV) {
  if (exists(df_name)) {
    df <- get(df_name)
    if ("ISO3_code_Importer" %in% colnames(df)) {
      # Filtrer la colonne 'ISO3_code' en fonction de la colonne 'ISO3_code' de 'Country_mapping_250120_updated_2'
      df_filtered <- df[df$ISO3_code_Importer %in% Country_mapping_250120_updated_2$ISO3_code, ]
      
      # Créer la colonne 'FABLE_match_ij' avec la correspondance de 'FABLE_cc' à partir de 'Country_mapping_250120_updated_2'
      df_filtered$FABLE_matching_Importer <- Country_mapping_250120_updated_2$FABLE_cc[match(df_filtered$ISO3_code_Importer, Country_mapping_250120_updated_2$ISO3_code)]
      
      # Réassigner le dataframe filtré et mis à jour
      assign(df_name, df_filtered)
    }
  }
}

merged_TUV<-do.call(rbind, lapply(data.frame_TUV, get))

merged_TUV <- merged_TUV %>%
  rename(
    FABLE_Exporter = FABLE_matching_Exporter,
    FABLE_Importer = FABLE_matching_Importer,
    year_baseline=t,
    i=ISO3_code_Exporter,
    j=ISO3_code_Importer
  )
merged_TUV_no2020<-merged_TUV%>%
  filter(year_baseline!=2020)

merged_dataframe <- merged_dataframe %>%
  left_join(merged_TUV_no2020 %>%
              select(i, j, year_baseline, HS6_Code92, uv), 
            by = c("i", "j", "year_baseline", "HS6_Code92")) %>%
  mutate(spv_ij = ifelse(!is.na(uv), ADV * uv, NA))

# Afficher un message de fin
cat("La colonne spv_ij a été remplie.\n")


# Calcul des pourcentages
total_rows <- nrow(merged_dataframe)

# Nombre de NA
na_count <- sum(is.na(merged_dataframe$spv_ij))

# Nombre de non-NA
non_na_count <- sum(!is.na(merged_dataframe$spv_ij))

# Nombre de zéros
zero_count <- sum(merged_dataframe$spv_ij == 0, na.rm = TRUE)

# Calcul des pourcentages
na_percentage <- (na_count / total_rows) * 100
non_na_percentage <- (non_na_count / total_rows) * 100
zero_percentage <- (zero_count / total_rows) * 100

# Affichage des résultats
cat("Pourcentage de NA dans 'spv_ij':", round(na_percentage, 2), "%\n")
cat("Pourcentage de valeurs non-NA dans 'spv_ij':", round(non_na_percentage, 2), "%\n")
cat("Pourcentage de zéros dans 'spv_ij':", round(zero_percentage, 2), "%\n")


# Charger les bibliothèques nécessaires
library(dplyr)
library(ggplot2)
library(tidyr)

# 1. Statistiques descriptives des valeurs uv
uv_stats <- merged_TUV_no2020 %>%
  group_by(Item, HS6_Code92) %>%
  summarise(
    moyenne_uv = mean(uv, na.rm = TRUE),
    ecart_type_uv = sd(uv, na.rm = TRUE),
    min_uv = min(uv, na.rm = TRUE),
    max_uv = max(uv, na.rm = TRUE),
    nb_observations = n()
  ) %>%
  arrange(desc(moyenne_uv))

# Afficher les statistiques descriptives
cat("Statistiques descriptives des valeurs uv :\n")
print(uv_stats)

# 2. Comparaison des valeurs uv pour tous les produits sous un même Item
uv_comparison_by_item <- merged_TUV_no2020 %>%
  group_by(Item, HS6_Code92) %>%
  summarise(moyenne_uv = mean(uv, na.rm = TRUE)) %>%
  spread(key = HS6_Code92, value = moyenne_uv) %>%
  gather(key = "HS6_Code92", value = "moyenne_uv", -Item)

# Afficher la comparaison des valeurs uv
cat("\nComparaison des valeurs uv par Item :\n")
print(uv_comparison_by_item)

# 3. Visualisation des tendances temporelles des valeurs uv
uv_time_trend <- merged_TUV_no2020 %>%
  group_by(year_baseline, Item, HS6_Code92) %>%
  summarise(moyenne_uv = mean(uv, na.rm = TRUE)) %>%
  ggplot(aes(x = year_baseline, y = moyenne_uv, color = HS6_Code92)) +
  geom_line() +
  facet_wrap(~ Item) +
  labs(
    title = "Tendances temporelles de la valeur UV",
    x = "Année", y = "Moyenne UV"
  )

# Afficher le graphique des tendances temporelles
cat("\nTendances temporelles de la valeur UV (graphique) :\n")
print(uv_time_trend)

# 4. Variation annuelle des valeurs uv
uv_variation <- merged_TUV_no2020 %>%
  group_by(year_baseline, Item, HS6_Code92) %>%
  summarise(moyenne_uv = mean(uv, na.rm = TRUE)) %>%
  arrange(Item, HS6_Code92, year_baseline) %>%
  mutate(variation_uv = moyenne_uv - lag(moyenne_uv)) %>%
  filter(!is.na(variation_uv))

# Afficher les variations annuelles de uv
cat("\nVariation annuelle de la valeur UV :\n")
print(uv_variation)

# 5. Pourcentage de NA, zéro et non-NA dans 'uv'
na_percentage <- sum(is.na(merged_TUV_no2020$uv)) / nrow(merged_TUV_no2020) * 100
zero_percentage <- sum(merged_TUV_no2020$uv == 0, na.rm = TRUE) / nrow(merged_TUV_no2020) * 100
non_na_percentage <- 100 - na_percentage

# Afficher les pourcentages de NA, zéro et non-NA
cat("\nPourcentage de NA dans 'uv':", round(na_percentage, 2), "%\n")
cat("Pourcentage de zéro dans 'uv':", round(zero_percentage, 2), "%\n")
cat("Pourcentage de valeurs non-NA dans 'uv':", round(non_na_percentage, 2), "%\n")

library(sf)
library(ggplot2)



# Lire tout le fichier
raw_data <- readLines("C:/Users/Adnane/Downloads/Beef_data_240212.csv")

# Séparer les lignes par des virgules
data_split <- strsplit(raw_data, split = ",")

# Supprimer les guillemets et les espaces superflus
cleaned_data <- lapply(data_split, function(x) gsub('^\\s+|\\s+$', '', gsub('"', '', x)))

# Convertir en dataframe
Beef_data <- do.call(rbind, cleaned_data)
Beef_data <- as.data.frame(Beef_data, stringsAsFactors = FALSE)

# Vérifier les premières lignes
head(Beef_data)
write.xlsx(Beef_data, "Beef_data_cleaned.xlsx")
colnames(Beef_data)