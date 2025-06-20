Matching_HS6_to_name_product<-select(Matching_HS6_to_name_product,Code,HS6_Code_92,Product)
Matching_HS6_to_name_product<-Matching_HS6_to_name_product %>%
  mutate(Product = ifelse(Product == "Soybean", "Soyabean", Product))



codes <- as.character(unique(Matching_HS6_to_name_product$Code))



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
  "Sigma_HS6_180100", "Sigma_HS6_180200", "Sigma_HS6_110290",
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
    product_name <- Matching_HS6_to_name_product$Product[Matching_HS6_to_name_product$Code == code]
    hs6_code_92 <- Matching_HS6_to_name_product$HS6_Code_92[Matching_HS6_to_name_product$Code == code]
    
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
  match_row <- Matching_HS6_to_name_product[Matching_HS6_to_name_product$Code == x, ]
  
  # Si un match est trouvé, retourner le code HS92 correspondant
  if (nrow(match_row) > 0) {
    return(match_row$HS6_Code_92)
  } else {
    return(NA) # Sinon, retourner NA
  }
})

Sigma_2019_vf1$Item <- sapply(Sigma_2019_vf1$hs6_2007, function(x) {
  # Trouver la ligne dans le fichier de matching pour le code HS6
  match_row <- Matching_HS6_to_name_product[Matching_HS6_to_name_product$Code == x, ]
  
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

unique(Sigma_HS6_020110$year)

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
Tarrifs_dt_1 <- do.call(rbind, lapply(dataframes, get))
colnames(Tarrifs_dt_1)
# Afficher un message de succès
cat("Tous les dataframes ont été fusionnés avec succès.\n")


if (exists("Tarrifs_dt_1")) {
  na_items_df <- Tarrifs_dt_1[is.na(Tarrifs_dt_1$Item), ]
  cat("Un nouveau dataframe 'na_items_df' a été créé avec les lignes contenant des NA dans la colonne 'Item'.\n")
} else {
  warning("Le dataframe 'merged_dataframe' n'existe pas.")
}


Tarrifs_dt_2<-Tarrifs_dt_1%>%
  select(i,FABLE_Exporter,j,FABLE_Importer,HS6_Code92,Item,year_baseline,ADV) %>%
  rename(Exporter_ISO_Code = i,
         Importer_ISO_Code = j,
         HS6 = HS6_Code92,
         FABLE_Item = Item,
         Year = year_baseline,
         adv = ADV)

#Plus tard faudra rajouter les flux qui n'existent pas donc creer une fonction que dit que pour chaque pays i il doit avoir un j et remplace le ADV par une regional_mean


Sigma_2019_vf2<-Sigma_2019_vf1%>%
  select(exporter,FABLE_Exporter,importer,FABLE_Importer,hs6_2007,Item,year_baseline,adv)%>%
  rename(Exporter_ISO_Code = exporter,
         Importer_ISO_Code = importer,
         HS6 = hs6_2007,
         FABLE_Item = Item,
         Year = year_baseline)


#Crop_taff_step1
Tarrifs_dt_3 <- Tarrifs_dt_2 %>%
  full_join(Sigma_2019_vf2, by = c("Exporter_ISO_Code","FABLE_Exporter","Importer_ISO_Code","FABLE_Importer","Year","HS6","FABLE_Item","adv")) %>%
  filter(FABLE_Item != "Beef")%>%
  select(Exporter_ISO_Code,FABLE_Exporter,Importer_ISO_Code,FABLE_Importer,Year,HS6,FABLE_Item,adv)

colnames(Tarrifs_dt_3)



# Step 1: Aggregate adv to adv_agg
Tarrifs_dt_3 <- Tarrifs_dt_3 %>%
  group_by(Exporter_ISO_Code, Importer_ISO_Code, Year, FABLE_Item) %>%
  mutate(adv_agg = mean(adv, na.rm = TRUE)) %>%
  select(- adv, -HS6 )%>%
  ungroup() 



Tarrifs_dt_3_pt1 <- Tarrifs_dt_3 %>%
  distinct(.keep_all = TRUE) %>%  # Keep unique rows based on all columns
  ungroup()  # Ungroup after distinct



# Ensure Tarrifs_dt_3_pt1 is ungrouped and process
Tarrifs_dt_4 <- Tarrifs_dt_3_pt1 %>%
  ungroup() %>%  # Ensure no residual grouping
  full_join(FAO_prices_dt_pt1, by = c("Exporter_ISO_Code", "Year", "FABLE_Item")) %>%
  mutate(FABLE_Exporter = Country_mapping_250120_updated_2$FABLE_cc[match(Exporter_ISO_Code, Country_mapping_250120_updated_2$ISO3_code)]) %>%
  group_by(FABLE_Exporter, Year, FABLE_Item) %>%
  mutate(prodprice_i_agg = median(prodprice_i, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(FABLE_Exporter, Year, FABLE_Item) %>%
  mutate(adv_regional = mean(adv_agg, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(Year, FABLE_Item) %>%
  mutate(worldprice = mean(prodprice_i, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    # Calculate sdt_ij (specific tariff values) with adv replaced by adv_agg
    sdt_ij = case_when(
      !is.na(prodprice_i) & !is.na(adv_agg) ~ adv_agg * prodprice_i,           
      is.na(prodprice_i) & !is.na(adv_agg) & !is.na(prodprice_i_agg) ~ adv_agg * prodprice_i_agg,        
      is.na(adv_agg) & !is.na(prodprice_i) & !is.na(adv_regional) ~ adv_regional * prodprice_i,       
      is.na(adv_agg) & is.na(prodprice_i) & !is.na(prodprice_i_agg) & !is.na(adv_regional) ~ adv_regional * prodprice_i_agg, 
      is.na(prodprice_i) & is.na(prodprice_i_agg) & !is.na(worldprice) & !is.na(adv_agg) ~ adv_agg * worldprice,  # Replaced adv with adv_agg
      is.na(prodprice_i) & is.na(prodprice_i_agg) & !is.na(adv_agg) & !is.na(worldprice) ~ adv_agg * worldprice,  # Replaced adv with adv_agg
      is.na(prodprice_i) & is.na(prodprice_i_agg) & is.na(adv_agg) & !is.na(worldprice) & !is.na(adv_regional) ~ adv_regional * worldprice,  
      TRUE ~ NA_real_                                                           
    ),
    # Indicator for regional price usage
    used_regional = case_when(
      !is.na(prodprice_i) ~ "No",                   
      is.na(prodprice_i) & !is.na(prodprice_i_agg) ~ "Yes",  
      TRUE ~ NA_character_                           
    )
  )

# Check the result


Tarrifs_dt_na_4 <- Tarrifs_dt_4 %>%
  filter(is.na(sdt_ij))

# Extract rows with duplicate sdt_ij values into Tarrifs_dt_4_double
Tarrifs_dt_4_double <- Tarrifs_dt_4 %>%
  group_by(Exporter_ISO_Code,Importer_ISO_Code,FABLE_Item,Year,sdt_ij) %>%  # Group by sdt_ij to find duplicates
  filter(n() > 1) %>%   # Keep only groups with more than one occurrence
  ungroup()




# Extended version of tarrif dataset
write.csv(Tarrifs_dt_4, 
          file = "C:\\Users\\Adnane\\Desktop\\BACI-MacMap\\MAC-MAPHS6\\data_output\\250403_tariff_specific_values_exd.csv", 
          row.names = FALSE)

# REDUCED version of tarrif dataset# Extended version of tarrif dataset

Tarrifs_dt_5<-Tarrifs_dt_4%>%
  select(Exporter_ISO_Code,FABLE_Exporter,Importer_ISO_Code,FABLE_Importer,Year,FABLE_Item,adv_agg,sdt_ij)%>%
  rename(adv_ij = adv_agg)

Tarrifs_dt_4<-Tarrifs_dt_4%>%
  group_by(FABLE_Exporter,FABLE_Importer,Year,FABLE_Item)%>%
  mutate(adv_ij_agg=mean(adv_ij, na.rm = TRUE))

write.csv(Tarrifs_dt_6, 
          file = "C:\\Users\\Adnane\\Desktop\\BACI-MacMap\\MAC-MAPHS6\\data_output\\250403_tariff_specific_values_exd.csv", 
          row.names = FALSE)



Tarrifs_dt_4 <- read.csv("C:\\Users\\Adnane\\Desktop\\BACI-MacMap\\MAC-MAPHS6\\data_output\\250403_tariff_specific_values_exd.csv")


Tarrifs_dt_4<-Tarrifs_dt_4%>%
  group_by(FABLE_Exporter,FABLE_Importer,Year,FABLE_Item)%>%
  mutate(adv_ij_agg=mean(adv_ij, na.rm = TRUE))



Tarrifs_dt_5<-Tarrifs_dt_4%>%
  select(FABLE_Exporter,FABLE_Importer,Year,FABLE_Item,adv_ij_agg,sdt_ij)%>%
  distinct(.keep_all = TRUE) %>% 
  ungroup()

Tarrifs_dt_5<-Tarrifs_dt_5%>%
  filter(FABLE_Exporter != FABLE_Importer)
colnames(Tarrifs_dt_5)






# 1. Extraire les dimensions uniques
exporters <- unique(Tarrifs_dt_5$FABLE_Exporter)
importers <- unique(Tarrifs_dt_5$FABLE_Importer)
years     <- unique(Tarrifs_dt_5$Year)
items     <- unique(Tarrifs_dt_5$FABLE_Item)

# 2. Créer toutes les combinaisons possibles sans les flux intra
all_combinations <- expand.grid(
  FABLE_Exporter = exporters,
  FABLE_Importer = importers,
  Year = years,
  FABLE_Item = items,
  stringsAsFactors = FALSE
) %>%
  filter(FABLE_Exporter != FABLE_Importer)  # Exclusion des flux intra

# 3. Fusionner avec les données existantes
Tarrifs_dt_complete <- all_combinations %>%
  left_join(Tarrifs_dt_5, by = c("FABLE_Exporter", "FABLE_Importer", "Year", "FABLE_Item")) %>%
  mutate(
    adv_ij_agg = ifelse(is.na(adv_ij_agg), 0, adv_ij_agg),
    sdt_ij     = ifelse(is.na(sdt_ij), 0, sdt_ij)
  )


# ---- Étape 1 : Moyennes régionales (par Exportateur, Produit, Année) ----
regional_means <- tarrif_dt %>%
  group_by(FABLE_Exporter, FABLE_Item, Year) %>%
  summarise(
    mean_reg_adv = mean(adv_ij_agg, na.rm = TRUE),
    mean_reg_sdt = mean(sdt_ij, na.rm = TRUE),
    .groups = "drop"
  )

# ---- Étape 2 : Moyennes mondiales (par Produit, Année) ----
global_means <- tarrif_dt %>%
  group_by(FABLE_Item, Year) %>%
  summarise(
    mean_glob_adv = mean(adv_ij_agg, na.rm = TRUE),
    mean_glob_sdt = mean(sdt_ij, na.rm = TRUE),
    .groups = "drop"
  )

# ---- Étape 3 : Jointure des données + Substitution hiérarchique ----
tarrif_dt <- all_combinations %>%
  left_join(Tarrifs_dt_5, by = c("FABLE_Exporter", "FABLE_Importer", "Year", "FABLE_Item")) %>%
  left_join(regional_means, by = c("FABLE_Exporter", "FABLE_Item", "Year")) %>%
  left_join(global_means, by = c("FABLE_Item", "Year")) %>%
  mutate(
    adv_ij_agg = case_when(
      !is.na(adv_ij_agg)            ~ adv_ij_agg,
      is.na(adv_ij_agg) & !is.na(mean_reg_adv) ~ mean_reg_adv,
      TRUE                          ~ mean_glob_adv
    ),
    sdt_ij = case_when(
      !is.na(sdt_ij)                ~ sdt_ij,
      is.na(sdt_ij) & !is.na(mean_reg_sdt) ~ mean_reg_sdt,
      TRUE                          ~ mean_glob_sdt
    )
  ) %>%
  select(-mean_reg_adv, -mean_reg_sdt, -mean_glob_adv, -mean_glob_sdt)

# Afficher un aperçu
print(head(Tarrifs_dt_complete))

write.csv(Tarrifs_dt_complete, 
          file = "C:\\Users\\Adnane\\Desktop\\BACI-MacMap\\MAC-MAPHS6\\data_output\\250409_tariff_specific_values_red.csv", 
          row.names = FALSE)



tarrif_dt <- tarrif_dt %>%
  group_by(FABLE_Exporter, FABLE_Importer, Year, FABLE_Item) %>%
  mutate(sdt_ij_agg = mean(sdt_ij, na.rm = TRUE)) %>%
  select(-sdt_ij) %>%
  distinct()

tarrif_dt$Regional_lv <- Country_mapping_250120_updated_2$regional_lv[match(tarrif_dt$FABLE_Exporter, Country_mapping_250120_updated_2$FABLE_cc)]
colnames(tarrif_dt)

tarrif_dt <- tarrif_dt %>%
  group_by(FABLE_Item, Regional_lv) %>%
  mutate(
    adv_ij_agg = ifelse(adv_ij_agg == 0, mean(adv_ij_agg[adv_ij_agg != 0], na.rm = TRUE), adv_ij_agg),
    sdt_ij_agg = ifelse(sdt_ij_agg == 0, mean(sdt_ij_agg[sdt_ij_agg != 0], na.rm = TRUE), sdt_ij_agg)
  ) %>%
  ungroup() %>%
  # Pour les valeurs restantes qui sont encore égales à 0, on remplace par la moyenne mondiale
  mutate(
    adv_ij_agg = ifelse(adv_ij_agg == 0, mean(adv_ij_agg, na.rm = TRUE), adv_ij_agg),
    sdt_ij_agg = ifelse(sdt_ij_agg == 0, mean(sdt_ij_agg, na.rm = TRUE), sdt_ij_agg)
  )

