library(readr)
library(dplyr)
library(broom)

############################# GDP Percapita########################################
# Charger les données
gdp_percapita <- read_csv("C:/Users/Adnane/Desktop/SPE_inputs/SPE_adaptation_adnane/Inputs_data/Elasticites_crops/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_19346.csv", 
                          skip = 4)
rice_val_calib <- read_csv("C:/Users/Adnane/Desktop/SPE_inputs/SPE_adaptation_adnane/Inputs_data/Elasticites_crops/RICE_calibrated_values.csv")
bilateral_trade_cost_RICE<-read_csv("C:/Users/Adnane/Desktop/SPE_inputs/SPE_adaptation_adnane/Model_input/Trade_cost/bilateral_trade_cost_RICE.csv")

colnames(comp_rice)
library(dplyr)

############# Remplacer par cout inexistant = 0 #################

# Net trade pour q_bal
net_trade_q_bal <- comp_rice %>%
  group_by(from_iso3) %>%
  summarise(exports = sum(q_bal, na.rm = TRUE)) %>%
  full_join(
    comp_rice %>%
      group_by(to_iso3) %>%
      summarise(imports = sum(q_bal, na.rm = TRUE)),
    by = c("from_iso3" = "to_iso3")
  ) %>%
  mutate(net_trade_q_bal = exports - imports) %>%
  select(country = from_iso3, net_trade_q_bal)

# Net trade pour q_calib
net_trade_q_calib <- comp_rice %>%
  group_by(from_iso3) %>%
  summarise(exports = sum(q_calib, na.rm = TRUE)) %>%
  full_join(
    comp_rice %>%
      group_by(to_iso3) %>%
      summarise(imports = sum(q_calib, na.rm = TRUE)),
    by = c("from_iso3" = "to_iso3")
  ) %>%
  mutate(net_trade_q_calib = exports - imports) %>%
  select(country = from_iso3, net_trade_q_calib)

# Fusion des deux net_trade dans un même tableau
net_trade_combined <- full_join(net_trade_q_bal, net_trade_q_calib, by = c("country"))
# Total global des net trades
total_net_trade <- net_trade_combined %>%
  summarise(
    total_net_trade_q_bal = sum(net_trade_q_bal, na.rm = TRUE),
    total_net_trade_q_calib = sum(net_trade_q_calib, na.rm = TRUE)
  )

print(total_net_trade)


# Sélectionner Country Code et années 1999 à 2021
years <- as.character(1999:2021)
gdp_percapita <- gdp_percapita %>%
  select(`Country Code`, all_of(years))

# Mapper FABLE_country
gdp_percapita$FABLE_country <- Country_mapping_250120_updated_2$FABLE_cc[match(gdp_percapita$`Country Code`, Country_mapping_250120_updated_2$ISO3_code)]

# Filtrer les NA
gdp_percapita <- gdp_percapita %>%
  filter(!is.na(FABLE_country))

# Sélectionner les années à lisser
smooth_years <- c("2000", "2005", "2010", "2015", "2020")
gdp_percapita <- gdp_percapita %>%
  select(FABLE_country, all_of(smooth_years))

# Calculer la médiane par FABLE_country pour chaque année
gdp_percapita_fable <- gdp_percapita %>%
  group_by(FABLE_country) %>%
  summarise(across(all_of(smooth_years), ~median(.x, na.rm = TRUE))) %>%
  rename(FABLE_Exporter = FABLE_country)

# Restructurer : une colonne Year et une colonne gdp_percapita
gdp_percapita_fable <- gdp_percapita_fable %>%
  pivot_longer(cols = all_of(smooth_years), 
               names_to = "Year", 
               values_to = "gdp_percapita") %>%
  mutate(Year = as.integer(Year))

################# Producer Price ##########
Prodprice_folur<-FAO_prices_dt_red %>%
  filter(FABLE_Item %in% c("Cocoa","Palm Oil","Coffee"))%>%
  select(FABLE_Exporter,FABLE_Item,Year,prodprice_i_agg_imp_final)%>%
  rename(prodprice_usd_t = prodprice_i_agg_imp_final)%>%
  filter(FABLE_Exporter != "IRL")

###### D,S and Inputs_costs ########
totaleq_folur_ct_adj$product<- str_to_title(totaleq_folur_ct_adj$product)

totaleq_costs<-totaleq_folur_ct_adj %>%
  filter(product %in% c("Cocoa","Palm_oil","Coffee"))%>%
  select(location,year,product,demande_q,supply_q,total_input_cost)%>%
  rename(FABLE_Exporter = location,
         FABLE_Item =  product,
         Year = year)%>%
  mutate(FABLE_Item = case_when(
    FABLE_Item == "Palm_oil" ~ "Palm Oil",
    TRUE ~ FABLE_Item
  ))


  
elas_dt <- totaleq_costs %>%
  full_join(Prodprice_folur, by = c("FABLE_Exporter", "FABLE_Item", "Year"))

elas_dt<-elas_dt%>%
  full_join(gdp_percapita_fable, by=c("FABLE_Exporter","Year"))%>%
  filter(FABLE_Exporter != "IRL")%>%
  filter(demande_q !=0 )
  

colnames(elas_dt)



# Nettoyer les outliers (1er et 99e percentiles)
quantiles_dq <- quantile(elas_dt$demande_q, probs = c(0.01, 0.99), na.rm = TRUE)
quantiles_sq <- quantile(elas_dt$supply_q, probs = c(0.01, 0.99), na.rm = TRUE)
quantiles_price <- quantile(elas_dt$prodprice_usd_t, probs = c(0.01, 0.99), na.rm = TRUE)
quantiles_cost <- quantile(elas_dt$total_input_cost, probs = c(0.01, 0.99), na.rm = TRUE)

elas_dt_filtered <- elas_dt %>%
  filter(demande_q > 0, supply_q > 0, prodprice_usd_t > 0, 
         gdp_percapita > 0, total_input_cost > 0,
         demande_q >= quantiles_dq[1] & demande_q <= quantiles_dq[2],
         supply_q >= quantiles_sq[1] & supply_q <= quantiles_sq[2],
         prodprice_usd_t >= quantiles_price[1] & prodprice_usd_t <= quantiles_price[2],
         total_input_cost >= quantiles_cost[1] & total_input_cost <= quantiles_cost[2]) %>%
  mutate(
    ln_demande_q = log(demande_q),
    ln_supply_q = log(supply_q),
    ln_prodprice = log(prodprice_usd_t),
    ln_gdp = log(gdp_percapita),
    ln_cost = log(total_input_cost)
  )

# Régression Demande (par pays/produit)
demand_models <- elas_dt_filtered %>%
  group_by(FABLE_Exporter, FABLE_Item) %>%
  do(model = lm(ln_demande_q ~ ln_prodprice + ln_gdp, 
                data = ., na.action = na.exclude)) %>%
  summarise(
    FABLE_Exporter = FABLE_Exporter[1],
    FABLE_Item = FABLE_Item[1],
    elast_price_demand = tidy(model)$estimate[2],
    elast_gdp = tidy(model)$estimate[3]
  )

# Régression Offre (par pays/produit)
supply_models <- elas_dt_filtered %>%
  group_by(FABLE_Exporter, FABLE_Item) %>%
  do(model = lm(ln_supply_q ~ ln_prodprice + ln_cost, 
                data = ., na.action = na.exclude)) %>%
  summarise(
    FABLE_Exporter = FABLE_Exporter[1],
    FABLE_Item = FABLE_Item[1],
    elast_price_supply = tidy(model)$estimate[2],
    elast_cost = tidy(model)$estimate[3]
  )

# Filtrer résultats plausibles
demand_models <- demand_models %>%
  mutate(
    elast_price_demand = if_else(elast_price_demand > 0 | elast_price_demand < -3, NA_real_, elast_price_demand),
    elast_gdp = if_else(elast_gdp < -1 | elast_gdp > 3, NA_real_, elast_gdp)
  )

supply_models <- supply_models %>%
  mutate(
    elast_price_supply = if_else(elast_price_supply < 0 | elast_price_supply > 2, NA_real_, elast_price_supply),
    elast_cost = if_else(elast_cost > 0 | elast_cost < -2, NA_real_, elast_cost)
  )

# Résultats
print(demand_models)
print(supply_models)


demand_models$Regional_lv <- Country_mapping_250120_updated_2$regional_lv[
  match(demand_models$FABLE_Exporter, Country_mapping_250120_updated_2$FABLE_cc)
]

supply_models$Regional_lv <- Country_mapping_250120_updated_2$regional_lv[
  match(supply_models$FABLE_Exporter, Country_mapping_250120_updated_2$FABLE_cc)
]


# Calculer le nombre de valeurs valides par pays
valid_count_demand <- demand_models %>%
  filter(!is.na(elast_price_demand)) %>%
  count(FABLE_Exporter, name = "n_valid")

# Calcul de la moyenne par pays uniquement si n_valid >= 2
country_means_demand <- demand_models %>%
  left_join(valid_count_demand, by = "FABLE_Exporter") %>%
  filter(n_valid >= 2) %>%
  group_by(FABLE_Exporter) %>%
  summarise(mean_country = mean(elast_price_demand, na.rm = TRUE), .groups = "drop")

# Imputer dans demand_models
demand_models <- demand_models %>%
  left_join(country_means_demand, by = "FABLE_Exporter") %>%
  mutate(
    elast_price_demand = if_else(is.na(elast_price_demand), mean_country, elast_price_demand)
  ) %>%
  select(-mean_country)

# Imputation régionale (par région ET produit)
regional_means_demand <- demand_models %>%
  group_by(Regional_lv, FABLE_Item) %>%
  summarise(mean_region = mean(elast_price_demand, na.rm = TRUE), .groups = "drop")

demand_models <- demand_models %>%
  left_join(regional_means_demand, by = c("Regional_lv", "FABLE_Item")) %>%
  mutate(
    elast_price_demand = if_else(is.na(elast_price_demand), mean_region, elast_price_demand)
  ) %>%
  select(-mean_region)

# Étape 3 — Imputation par moyenne mondiale du produit (FABLE_Item)
global_means_demand <- demand_models %>%
  group_by(FABLE_Item) %>%
  summarise(global_mean = mean(elast_price_demand, na.rm = TRUE), .groups = "drop")

demand_models <- demand_models %>%
  left_join(global_means_demand, by = "FABLE_Item") %>%
  mutate(
    elast_price_demand = if_else(is.na(elast_price_demand), global_mean, elast_price_demand)
  ) %>%
  select(-global_mean)




valid_count_supply <- supply_models %>%
  filter(!is.na(elast_price_supply)) %>%
  count(FABLE_Exporter, name = "n_valid")

country_means_supply <- supply_models %>%
  left_join(valid_count_supply, by = "FABLE_Exporter") %>%
  filter(n_valid >= 2) %>%
  group_by(FABLE_Exporter) %>%
  summarise(mean_country = mean(elast_price_supply, na.rm = TRUE), .groups = "drop")

supply_models <- supply_models %>%
  left_join(country_means_supply, by = "FABLE_Exporter") %>%
  mutate(
    elast_price_supply = if_else(is.na(elast_price_supply), mean_country, elast_price_supply)
  ) %>%
  select(-mean_country)

regional_means_supply <- supply_models %>%
  group_by(Regional_lv, FABLE_Item) %>%
  summarise(mean_region = mean(elast_price_supply, na.rm = TRUE), .groups = "drop")

supply_models <- supply_models %>%
  left_join(regional_means_supply, by = c("Regional_lv", "FABLE_Item")) %>%
  mutate(
    elast_price_supply = if_else(is.na(elast_price_supply), mean_region, elast_price_supply)
  ) %>%
  select(-mean_region)


# Étape 3 — Imputation par moyenne mondiale du produit (FABLE_Item)
global_means_supply <- supply_models %>%
  group_by(FABLE_Item) %>%
  summarise(global_mean = mean(elast_price_supply, na.rm = TRUE), .groups = "drop")

supply_models <- supply_models %>%
  left_join(global_means_supply, by = "FABLE_Item") %>%
  mutate(
    elast_price_supply = if_else(is.na(elast_price_supply), global_mean, elast_price_supply)
  ) %>%
  select(-global_mean)




demand_models <- demand_models %>%
  select(FABLE_Exporter, FABLE_Item, elas_d = elast_price_demand) 

supply_models <- supply_models %>%
  select(FABLE_Exporter, FABLE_Item, elas_s = elast_price_supply)
elast_estim <- full_join(demand_models, supply_models, by = c("FABLE_Exporter", "FABLE_Item"))
elast_estim$Year <- 2020
elast_estim <- full_join(elast_estim, elas_dt, by = c("FABLE_Exporter", "FABLE_Item", "Year"))

elast_estim_2020 <- elast_estim %>%
  filter(Year == "2020") %>%
  select(-total_input_cost, -gdp_percapita) %>%
  select(everything(), elas_d, elas_s)



elast_estim_2020 <- elast_estim_2020 %>%
  group_by(FABLE_Item) %>%
  mutate(
    elas_d = ifelse(elas_d == 0, min(elas_d[elas_d != 0], na.rm = TRUE), elas_d),
    elas_s = ifelse(elas_s == 0, max(elas_s[elas_s != 0], na.rm = TRUE), elas_s)
  ) %>%
  ungroup()




elast_estim_2020$Regional_lv <- Country_mapping_250120_updated_2$regional_lv[
  match(elast_estim_2020$FABLE_Exporter, Country_mapping_250120_updated_2$FABLE_cc)
]

elast_estim_2020 <- elast_estim_2020 %>%
  group_by(Regional_lv,FABLE_Item) %>%  # Regrouper par FABLE_Item, ou selon d'autres critères pertinents
  mutate(
    # Remplacer les valeurs aberrantes de elas_d avec la moyenne du groupe
    elas_d = ifelse(elas_d < -1.5 | elas_d > -1, 
                    mean(elas_d, na.rm = TRUE), elas_d),
    
    # Remplacer les valeurs aberrantes de elas_s avec la moyenne du groupe
    elas_s = ifelse(elas_s < 0.2 | elas_s > 0.6, 
                    mean(elas_s, na.rm = TRUE), elas_s)
  ) %>%
  ungroup()  # Retirer le groupe après l'ajustement


#Rice_elas
rice_elas<-country_information_RICE %>%
  select(iso3,demand_elas,supply_elas)

rice_elas$FABLE_Exporter <- Country_mapping_250120_updated_2$FABLE_cc[
  match(rice_elas$iso3, Country_mapping_250120_updated_2$ISO3_code)
]

rice_elas <- rice_elas %>%
  filter(!is.na(FABLE_Exporter))


rice_elas_2 <- rice_elas %>%
  group_by(FABLE_Exporter) %>%
  summarise(
    demand_elas = mean(demand_elas, na.rm = TRUE),
    supply_elas = mean(supply_elas, na.rm = TRUE)
  )%>%
  filter(FABLE_Exporter != "IRL")

rice_eq<-totaleq_folur_ct_adj%>%
  filter(product=="Rice"& year=="2020")%>%
  select(location,product,demande_q,supply_q)%>%
  rename(FABLE_Exporter = location,
         FABLE_Item = product)


country_info_step_1_RICE<-rice_elas_2%>%
  full_join(rice_eq,by=c("FABLE_Exporter"))%>%
  select(- FABLE_Item)



# Générer le résumé
summary_stats <- summary(elast_estim_2020)

# Sauvegarder le résumé sous format image JPG
png("C:/Users/Adnane/Desktop/SPE_inputs/SPE_adaptation_adnane/summary_stats.jpg", width = 800, height = 600)

# Afficher le résumé sur l'image
gridExtra::grid.table(summary_stats)

# Fermer le fichier image
dev.off()