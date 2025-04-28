# Calculer les exports totaux pour chaque paire (Exporter, Importer, Year, FABLE_Item)
exports <- FABLE_Products_WA_VF3 %>%
  select(Exporter_ISO_Code,FABLE_Exporter_Mapping,Importer_ISO_Code,FABLE_Importer_Mapping, Year, FABLE_Item, Trade_Quantity_Adjusted) %>%
  rename(Trade_Quantity = Trade_Quantity_Adjusted) %>%
  mutate(Trade_Type = "Export")

# Calculer les imports totaux pour chaque paire (Importer, Exporter, Year, FABLE_Item)
imports <- FABLE_Products_WA_VF3 %>%
  select(Importer_ISO_Code,FABLE_Importer_Mapping,Exporter_ISO_Code, FABLE_Exporter_Mapping, Year, FABLE_Item, Trade_Quantity_Adjusted) %>%
  rename(Trade_Quantity = Trade_Quantity_Adjusted) %>%
  mutate(Trade_Type = "Import") %>%
  rename(FABLE_Exporter_Mapping = FABLE_Importer_Mapping,
         FABLE_Importer_Mapping = FABLE_Exporter_Mapping,
         Exporter_ISO_Code=Importer_ISO_Code,
         Importer_ISO_Code=Exporter_ISO_Code)

# Combiner les exports et imports
combined <- bind_rows(exports, imports)

# Calculer le total des exportations et importations pour chaque paire (Exporter, Importer, Year, FABLE_Item)
combined <- combined %>%
  mutate(Trade_Quantity = as.numeric(Trade_Quantity))

# Si certaines valeurs ne peuvent pas être converties, elles deviendront NA.
# Ensuite, on peut effectuer la sommation.

net_trade_WA_VF11 <- combined %>%
  group_by(Exporter_ISO_Code,FABLE_Exporter_Mapping,Importer_ISO_Code,FABLE_Importer_Mapping, Year, FABLE_Item, Trade_Type) %>%
  summarize(Total_Quantity = sum(Trade_Quantity, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = Trade_Type, values_from = Total_Quantity, values_fill = list(Total_Quantity = 0)) %>%
  mutate(Net_Trade = Export - Import)  


# Afficher le résultat
print(net_trade)


# Convertir les quantités de tonnes métriques en milliers de tonnes





# Agréger les données par country, product et year
net_trade_aggregated_WA_VF1 <- net_trade_WA_VF1 %>%
  group_by(FABLE_Exporter_Mapping, FABLE_Item, Year) %>%
  summarise(
    import_quantity = sum(Import, na.rm = TRUE),
    export_quantity = sum(Export, na.rm = TRUE),
    Net_Trade = sum(Net_Trade, na.rm = TRUE),
    .groups = 'drop'
  )
net_trade_aggregated_WA_VF2 <- net_trade_aggregated_WA_VF1 %>%
  mutate(across(c(import_quantity, export_quantity, Net_Trade), ~ format(., scientific = FALSE, digits = 4)))

# Renommer les colonnes pour correspondre au format souhaité
net_trade_aggregated_WA_VF2 <- net_trade_aggregated_WA_VF2 %>%
  rename(
    country = FABLE_Exporter_Mapping,
    product = FABLE_Item,
    year = Year
  )

# Formater les colonnes avec 4 chiffres après la virgule
net_trade_aggregated_WA_VF3 <- net_trade_aggregated_WA_VF2 %>%
  mutate(across(c(import_quantity, export_quantity, Net_Trade), ~ format(., scientific = FALSE, digits = 4)))

net_trade_aggregated_WA_VF3 <- net_trade_aggregated_WA_VF3 %>%
  mutate(
    import_quantity = as.numeric(import_quantity) / 1000,
    export_quantity = as.numeric(export_quantity) / 1000,
    Net_Trade = as.numeric(Net_Trade) / 1000
  )


net_trade_aggregated_WA_VF3 <- net_trade_aggregated_WA_VF3 %>%
  rename_with(~ paste0(., "_BACI"), -c(country,product, year))




df_FB <- full_join(Scenathon_Fao_filtered_VF2, net_trade_aggregated_WA_VF3, 
                       by = c("country", "product", "year"))

df_FB_2 <- df_FB %>%
  select(
    country,
    product,
    year,
    import_quantity_FAO,
    import_quantity_BACI,
    export_quantity_FAO,
    export_quantity_BACI,
    Net_Trade_FAO,
    Net_Trade_BACI
  )

df_FB_3 <- df_FB_2 %>%
  mutate(ratio_BACI_FAO = round(abs(Net_Trade_BACI / Net_Trade_FAO), 4))

df_FB_3$ratio_BACI_FAO <- ifelse(is.na(df_FB_3$ratio_BACI_FAO) | is.infinite(df_FB_3$ratio_BACI_FAO), 
                                 "-", df_FB_3$ratio_BACI_FAO)

df_FB_3$sign_comparison <- ifelse((df_FB_3$Net_Trade_FAO == 0 | df_FB_3$Net_Trade_BACI == 0), 
                                  "Same", 
                                  ifelse(sign(df_FB_3$Net_Trade_FAO) != sign(df_FB_3$Net_Trade_BACI), 
                                         "Flag", "Same"))

df_FB_3 <- df_FB_3[, c(setdiff(names(df_FB_3), "sign_comparison"), "sign_comparison")]
df_FB_3_distinct <- df_FB_3 %>%
  distinct()

Beef_output3<-df_FB_3_distinct %>%
  filter(product =="Beef")

colnames(Beef_output3)



setwd("C:/Users/Adnane/Desktop")

write.xlsx(Beef_output3, file = "Beef_output41.xlsx", sheetName = "Data", row.names = FALSE)
  
df_top_exporters <- df_FB_3_distinct %>%
  filter(!is.na(export_quantity_BACI)) %>%
  group_by(year, product) %>%
  arrange(desc(export_quantity_BACI)) %>%
  mutate(total_export = sum(export_quantity_BACI)) %>%  # Calculer le total des exportations
  ungroup() %>%
  arrange(year)

print(df_top_exporters)


df_top_exporters_2 <- df_top_exporters %>%
  select(country,product,year,export_quantity_FAO,export_quantity_BACI,Net_Trade_FAO,Net_Trade_BACI,ratio_BACI_FAO,total_export)
  

df_top_exporters_2 <- df_top_exporters %>%
  group_by(year, product) %>%
  mutate(total_export = sum(export_quantity_BACI, na.rm = TRUE)) %>%  # Calculer le total des exportations par produit et année
  ungroup() %>%
  group_by(country, product, year) %>%
  mutate(
    Market_Share = export_quantity_BACI / total_export
  ) %>%
  ungroup()


df_top_exporters_3 <- df_top_exporters_2 %>%
  group_by(year, product) %>%
  arrange(desc(export_quantity_BACI)) %>%
  mutate(
    total_export = sum(export_quantity_BACI, na.rm = TRUE),
    Market_Share = export_quantity_BACI / total_export,
    cumulative_market_share = cumsum(Market_Share)
  ) %>%
  filter(Market_Share >= 0.95 | cumulative_market_share <= 0.95) %>%
  ungroup()

  



write.xlsx(df_FB_3_distinct, file = "df_FB_3_distinct.xlsx", sheetName = "Data", row.names = FALSE)
write.xlsx(df_top_exporters_2, file = "Top_exporters_sheet.xlsx", sheetName = "Data", row.names = FALSE)
write.xlsx(df_world_trade, file = "df_world_trade.xlsx", sheetName = "Data", row.names = FALSE)

df_world_trade <- df_FB_3_distinct %>%
  group_by(product, year) %>%
  summarise(
    import_quantity_BACI_world = round(sum(import_quantity_BACI, na.rm = TRUE), 4),
    import_quantity_FAO_world = round(sum(import_quantity_FAO, na.rm = TRUE), 4),
    export_quantity_BACI_world = round(sum(export_quantity_BACI, na.rm = TRUE), 4),
    export_quantity_FAO_world = round(sum(export_quantity_FAO, na.rm = TRUE), 4)
  ) %>%
  mutate(
    country = "WORLD",
    diff_percentage_FAO = round(((export_quantity_FAO_world - import_quantity_FAO_world) / import_quantity_FAO_world) * 100, 1),
    diff_percentage_BACI = round(((export_quantity_BACI_world - import_quantity_BACI_world) / import_quantity_BACI_world) * 100, 1)
  ) %>%
  ungroup() %>%
  select(country, product, year, import_quantity_BACI_world, export_quantity_BACI_world, diff_percentage_BACI, 
         import_quantity_FAO_world, export_quantity_FAO_world, diff_percentage_FAO)



liste_annees <- unique(df_FB_3_distinct$year)

for (annee in liste_annees) {
  
  tableau_pivot <- df_FB_3_distinct %>%
    filter(year == annee) %>%
    select(country, product, ratio_BACI_FAO) %>%
    pivot_wider(names_from = country, values_from = ratio_BACI_FAO)
  
  assign(paste0("pivot_", annee), tableau_pivot)
  
}


write.xlsx(pivot_2000, file = "pivot_2000.xlsx", sheetName = "Data", row.names = FALSE)
write.xlsx(pivot_2005, file = "pivot_2005.xlsx", sheetName = "Data", row.names = FALSE)
write.xlsx(pivot_2010, file = "pivot_2010.xlsx", sheetName = "Data", row.names = FALSE)
write.xlsx(pivot_2015, file = "pivot_2015.xlsx", sheetName = "Data", row.names = FALSE)
write.xlsx(pivot_2020, file = "pivot_2020.xlsx", sheetName = "Data", row.names = FALSE)