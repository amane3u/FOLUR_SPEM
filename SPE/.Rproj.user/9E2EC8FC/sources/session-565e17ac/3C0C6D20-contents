
library(readxl)

path <- "C:/Users/Adnane/Desktop/SPE_inputs/SPE_adaptation_adnane/Output/Calibration/"

# Lister les fichiers
files <- list.files(path = path, pattern = "^trade_step1.*\\.xlsx$", full.names = TRUE)



# Liste des patterns à chercher
patterns <- c("conprice_calibration", 
              "prodprice_calibration",
              "tc_calibration",
              "trade_calibration",
              "calib_calibration")

# Charger chaque fichier et renommer directement les colonnes
for (p in patterns) {
  files <- list.files(path = path, pattern = paste0("^", p, ".*\\.csv$"), full.names = TRUE)
  for (f in files) {
    df_name <- tools::file_path_sans_ext(basename(f))
    df <- read.csv(f, header = FALSE)  # désactive l'entête puisque les colonnes sont absentes
    
    # Renommage selon le type
    if (startsWith(df_name, "conprice_calibration")) {
      colnames(df) <- c("iso3", "conprice")
    } else if (startsWith(df_name, "prodprice_calibration")) {
      colnames(df) <- c("iso3", "prodprice")
    } else if (startsWith(df_name, "tc_calibration")) {
      colnames(df) <- c("from_iso3", "to_iso3", "tc")
    } else if (startsWith(df_name, "trade_calibration")) {
      colnames(df) <- c("from_iso3", "to_iso3", "q_spem")
    } else if (startsWith(df_name, "calib_calibration")) {
      colnames(df) <- c("from_iso3", "to_iso3", "calib")
    }
    
    assign(df_name, df)
  }
}






library(dplyr)
library(writexl)

join_trade_files <- function(crops, path_output) {
  for (crop in crops) {
    calib_file <- paste0("trade_calibration_", crop)
    step1_file <- paste0("trade_step1_", crop)
    
    if (exists(calib_file) && exists(step1_file)) {
      calib_df <- get(calib_file)
      step1_df <- get(step1_file)
      
      # Jointure
      joined_df <- full_join(calib_df, step1_df, by = c("from_iso3", "to_iso3"))
      
      # Colonnes à formater
      cols_to_format <- c("q_initial", "q_step1", "q_spem")
      for (col in cols_to_format) {
        if (col %in% colnames(joined_df)) {
          joined_df[[col]] <- format(round(as.numeric(joined_df[[col]]), 4), nsmall = 4, scientific = FALSE)
        }
      }
      
      # Réordonner les colonnes
      id_cols <- c("from_iso3", "to_iso3")
      existing_cols <- cols_to_format[cols_to_format %in% colnames(joined_df)]
      other_cols <- setdiff(colnames(joined_df), c(id_cols, existing_cols))
      joined_df <- joined_df[, c(id_cols, existing_cols, other_cols)]
      
      # Sauvegarde Excel
      file_name <- paste0("trade_join_", crop, ".xlsx")
      write_xlsx(joined_df, file.path(path_output, file_name))
      
      # Sauvegarder aussi dans l'environnement global si nécessaire
      assign(paste0("trade_join_", crop), joined_df, envir = .GlobalEnv)
      
    } else {
      warning(paste("Données manquantes pour", crop))
    }
  }
}

# Exemple d'appel
crops <- c("WHEA", "COFFEE", "CORN", "SOYB", "PALM", "RICE")
path_output <- "C:/Users/Adnane/Desktop/SPE_inputs/SPE_adaptation_adnane/Output/Calibration_analysis"

join_trade_files(crops, path_output)




# Répertoire des données
data_path <- "C:/Users/Adnane/Desktop/SPE_inputs/SPE_adaptation_adnane/Output/Calibration_analysis"

# Boucle sur chaque culture
for (crop in crops) {
  # Charger les données
  trade_data <- read_excel(file.path(data_path, paste0("trade_join_", crop, ".xlsx")))
  
  # Convertir les colonnes q_initial, q_step1, q_spem en numeric
  trade_data <- trade_data %>%
    mutate(
      q_initial = as.numeric(q_initial),
      q_step1 = as.numeric(q_step1),
      q_spem = as.numeric(q_spem)
    )
  
  # Calcul du net trade par pays (pour chaque iso3)
  net_trade_by_country <- trade_data %>%
    # Pour exportations : regrouper par from_iso3
    group_by(from_iso3) %>%
    summarise(
      export_initial = sum(q_initial[from_iso3 != to_iso3], na.rm = TRUE),
      export_step1 = sum(q_step1[from_iso3 != to_iso3], na.rm = TRUE),
      export_spem = sum(q_spem[from_iso3 != to_iso3], na.rm = TRUE)
    ) %>%
    ungroup() %>%
    # Pour importations : regrouper par to_iso3
    left_join(
      trade_data %>%
        group_by(to_iso3) %>%
        summarise(
          import_initial = sum(q_initial[from_iso3 != to_iso3], na.rm = TRUE),
          import_step1 = sum(q_step1[from_iso3 != to_iso3], na.rm = TRUE),
          import_spem = sum(q_spem[from_iso3 != to_iso3], na.rm = TRUE)
        ) %>%
        ungroup(),
      by = c("from_iso3" = "to_iso3")
    ) %>%
    # Calculer le net trade
    mutate(
      net_trade_initial = export_initial - import_initial,
      net_trade_step1 = export_step1 - import_step1,
      net_trade_spem = export_spem - import_spem
    ) %>%
    # Renommer from_iso3 en iso3 pour clarté
    rename(iso3 = from_iso3) %>%
    # Sélectionner les colonnes pertinentes
    select(iso3, net_trade_initial, net_trade_step1, net_trade_spem)
  
  # Calcul du net trade mondial
  net_trade_world <- data.frame(
    world_net_trade_initial = sum(trade_data$q_initial[trade_data$from_iso3 != trade_data$to_iso3], na.rm = TRUE) -
      sum(trade_data$q_initial[trade_data$from_iso3 != trade_data$to_iso3], na.rm = TRUE),
    world_net_trade_step1 = sum(trade_data$q_step1[trade_data$from_iso3 != trade_data$to_iso3], na.rm = TRUE) -
      sum(trade_data$q_step1[trade_data$from_iso3 != trade_data$to_iso3], na.rm = TRUE),
    world_net_trade_spem = sum(trade_data$q_spem[trade_data$from_iso3 != trade_data$to_iso3], na.rm = TRUE) -
      sum(trade_data$q_spem[trade_data$from_iso3 != trade_data$to_iso3], na.rm = TRUE)
  )
  
  # Sauvegarder les résultats
  write_xlsx(net_trade_by_country, file.path(data_path, paste0("net_trade_by_country_", crop, ".xlsx")))
  write_xlsx(net_trade_world, file.path(data_path, paste0("net_trade_world_", crop, ".xlsx")))
  
  print(paste("Net trade calculé pour", crop))
}





data_path <- "C:/Users/Adnane/Desktop/SPE_inputs/SPE_adaptation_adnane/Output/Calibration_analysis"

for (crop in crops) {
  trade_data <- readxl::read_excel(file.path(data_path, paste0("trade_join_", crop, ".xlsx"))) %>%
    mutate(
      q_initial = as.numeric(q_initial),
      q_step1 = as.numeric(q_step1),
      q_spem = as.numeric(q_spem)
    ) %>%
    mutate(
      change_initial_to_step1 = (q_step1 - q_initial) / q_initial * 100,  # % changement
      change_step1_to_spem = (q_spem - q_step1) / q_step1 * 100,         # % changement
      change_initial_to_spem = (q_spem - q_initial) / q_initial * 100    # % changement
    ) %>%
    arrange(desc(abs(change_initial_to_spem)))  # Trier par changement total
  
  # Sauvegarder
  write_xlsx(trade_data, file.path(data_path, paste0("trade_changes_", crop, ".xlsx")))
  print(paste("Changements calculés pour", crop))
}



for (crop in crops) {
  trade_data <- readxl::read_excel(file.path(data_path, paste0("trade_join_", crop, ".xlsx"))) %>%
    mutate(
      q_initial = as.numeric(q_initial),
      q_step1 = as.numeric(q_step1),
      q_spem = as.numeric(q_spem)
    )
  
  # Top exportateurs
  top_exporters <- trade_data %>%
    filter(from_iso3 != to_iso3) %>%
    group_by(from_iso3) %>%
    summarise(
      total_export_initial = sum(q_initial, na.rm = TRUE),
      total_export_step1 = sum(q_step1, na.rm = TRUE),
      total_export_spem = sum(q_spem, na.rm = TRUE)
    ) %>%
    arrange(desc(total_export_spem))
  
  # Top importateurs
  top_importers <- trade_data %>%
    filter(from_iso3 != to_iso3) %>%
    group_by(to_iso3) %>%
    summarise(
      total_import_initial = sum(q_initial, na.rm = TRUE),
      total_import_step1 = sum(q_step1, na.rm = TRUE),
      total_import_spem = sum(q_spem, na.rm = TRUE)
    ) %>%
    arrange(desc(total_import_spem))
  
  # Sauvegarder
  write_xlsx(top_exporters, file.path(data_path, paste0("top_exporters_", crop, ".xlsx")))
  write_xlsx(top_importers, file.path(data_path, paste0("top_importers_", crop, ".xlsx")))
  print(paste("Top exportateurs/importateurs calculés pour", crop))
}






# Répertoire et cultures
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(writexl)

data_path <- "C:/Users/Adnane/Desktop/SPE_inputs/SPE_adaptation_adnane/Output/Calibration_analysis"

# Liste des cultures

for (crop in crops) {
  # 1. Lire les données Excel
  net_trade <- read_excel(file.path(data_path, paste0("net_trade_by_country_", crop, ".xlsx")))
  
  # 2. Forcer la conversion numérique (éviter les formats texte)
  net_trade <- net_trade %>%
    mutate(
      net_trade_initial = round(as.numeric(net_trade_initial), 4),
      net_trade_step1   = round(as.numeric(net_trade_step1), 4),
      net_trade_spem    = round(as.numeric(net_trade_spem), 4)
    )
  
  # 3. Graphique Initial vs SPEM
  net_trade_long <- net_trade %>%
    select(iso3, net_trade_initial, net_trade_spem) %>%
    pivot_longer(cols = c(net_trade_initial, net_trade_spem), 
                 names_to = "Type", 
                 values_to = "Net_Trade") %>%
    mutate(Type = recode(Type, 
                         "net_trade_initial" = "Initial", 
                         "net_trade_spem" = "SPEM"))
  
  p <- ggplot(net_trade_long, aes(x = reorder(iso3, Net_Trade), y = Net_Trade, fill = Type)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    coord_flip() +
    scale_fill_manual(values = c("Initial" = "#1f77b4", "SPEM" = "#ff7f0e")) +
    labs(title = paste("Net Trade Comparison for", crop),
         subtitle = "Initial vs SPEM Results",
         x = "Country (ISO3)",
         y = "Net Trade (Exports - Imports)") +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(size = 10),
      legend.position = "top",
      legend.title = element_blank(),
      panel.grid.major = element_line(color = "gray85"),
      panel.grid.minor = element_blank()
    )
  
  ggsave(file.path(data_path, paste0("net_trade_comparison_", crop, ".png")), 
         plot = p, width = 10, height = 8, dpi = 300)
  
  # 4. Statistiques descriptives
  stats <- net_trade %>%
    summarise(
      mean_initial  = mean(net_trade_initial, na.rm = TRUE),
      median_initial = median(net_trade_initial, na.rm = TRUE),
      sd_initial     = sd(net_trade_initial, na.rm = TRUE),
      min_initial    = min(net_trade_initial, na.rm = TRUE),
      max_initial    = max(net_trade_initial, na.rm = TRUE),
      
      mean_step1  = mean(net_trade_step1, na.rm = TRUE),
      median_step1 = median(net_trade_step1, na.rm = TRUE),
      sd_step1     = sd(net_trade_step1, na.rm = TRUE),
      min_step1    = min(net_trade_step1, na.rm = TRUE),
      max_step1    = max(net_trade_step1, na.rm = TRUE),
      
      mean_spem  = mean(net_trade_spem, na.rm = TRUE),
      median_spem = median(net_trade_spem, na.rm = TRUE),
      sd_spem     = sd(net_trade_spem, na.rm = TRUE),
      min_spem    = min(net_trade_spem, na.rm = TRUE),
      max_spem    = max(net_trade_spem, na.rm = TRUE)
    )
  
  # 5. Sauvegarde des statistiques en .xlsx (sans row names, sans notation scientifique)
  writexl::write_xlsx(stats, file.path(data_path, paste0("net_trade_stats_", crop, ".xlsx")))
  
  print(paste("✅ Fichiers générés pour", crop))

}






library(dplyr)

# Chemin de base et cultures (ajustées selon les dossiers)
base_path <- "C:/Users/Adnane/Documents/GitHub/FOLUR_SPEM/data/Calibration_output"
crops <- c("Coffee", "Corn", "Palm_oil", "Rice", "Soyabean", "Wheat")

# Types de fichiers et noms des colonnes
file_types <- c("calib_calibration_", "conprice_calibration_", "prodprice_calibration_", 
                "tc_calibration_", "trade_calibration_")
col_names <- list(
  "calib_calibration_" = c("from_iso3", "to_iso3", "calib"),
  "conprice_calibration_" = c("iso3", "to_iso3", "conprice_spem"),
  "prodprice_calibration_" = c("iso3", "to_iso3", "prodprice_spem"),
  "tc_calibration_" = c("from_iso3", "to_iso3", "tc_spem"),
  "trade_calibration_" = c("from_iso3", "to_iso3", "q_spem")
)

# Boucle sur chaque culture
for (crop in crops) {
  crop_path <- file.path(base_path, crop)
  
  # Boucle sur chaque type de fichier
  for (file_type in file_types) {
    file_name <- paste0(file_type, crop, ".csv")
    file_path <- file.path(crop_path, file_name)
    
    # Vérifier si le fichier existe
    if (file.exists(file_path)) {
      # Lire le fichier sans noms de colonnes
      data <- read.csv(file_path, header = FALSE)
      
      # Ajuster selon le type de fichier
      if (file_type %in% c("conprice_calibration_", "prodprice_calibration_")) {
        # Ces fichiers ont 2 colonnes (iso3, valeur), on ajoute to_iso3 comme NA
        colnames(data) <- c(col_names[[file_type]][1], col_names[[file_type]][3])
        data <- data %>%
          mutate(to_iso3 = NA) %>%
          select(iso3, to_iso3, !!sym(col_names[[file_type]][3]))
      } else {
        # Les autres fichiers ont 3 colonnes (from_iso3, to_iso3, valeur)
        colnames(data) <- col_names[[file_type]]
      }
      
      # Sauvegarder
      write.csv(data, file_path, row.names = FALSE)
      print(paste("Colonnes ajoutées pour", file_name))
    } else {
      print(paste("Fichier non trouvé :", file_name))
    }
  }
}



