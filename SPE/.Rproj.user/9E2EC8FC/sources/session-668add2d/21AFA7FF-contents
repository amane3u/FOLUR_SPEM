# Transport costs:
#Step 1: BACI data extration for products
#Step 2: Geodata 
#Step 3: Hummels specification
rm(list=ls())


#Step 1
J_rice_ <- read.csv("C:/Users/Adnane/Documents/bilateral_trade_cost_RICE.csv")
J_soyb<-read.csv("C:/Users/Adnane/Desktop/BACI-MacMap/SPE_doc/SPE_jasper_input/Trade_cost/bilateral_trade_cost_SOYB.csv")
J_wheat<-read.csv("C:/Users/Adnane/Desktop/BACI-MacMap/SPE_doc/SPE_jasper_input/Trade_cost/bilateral_trade_cost_WHEA.csv")
J_Corn<-read.csv("C:/Users/Adnane/Desktop/BACI-MacMap/SPE_doc/SPE_jasper_input/Trade_cost/bilateral_trade_cost_MAIZ.csv")

"C:\Users\Adnane\Downloads\FAOSTAT_data_en_3-26-2025.csv"
#FOLUR Analysis

FOLOUR_trade_dt <- FABLE_Products_WA_VF3 %>%
  filter(FABLE_Item %in% c("Corn", "Wheat", "Rice", "Cocoa", "Palm_oil", "Coffee", "Soyabean"))

FOLOUR_trade_dt$Trade_Quantity_Adjusted <- as.numeric(FOLOUR_trade_dt$Trade_Quantity_Adjusted) / 1000
FOLOUR_trade_dt$Trade_Value_Adjusted <- as.numeric(FOLOUR_trade_dt$Trade_Value_Adjusted) / 1000
colnames(FOLOUR_trade_dt)


# Fonction pour détecter et traiter les outliers sur Trade_Quantity_Adjusted
# et ajuster Trade_Value_Adjusted proportionnellement
detect_and_treat_outliers <- function(df, k = 1.5) {
  
  # Regrouper par Exporter, Importer, Produit et trier par année
  df_grouped <- df %>%
    group_by(Exporter_ISO_Code, Importer_ISO_Code, FABLE_Item) %>%
    arrange(Year) %>%
    mutate(
      # Ajouter les valeurs des années précédentes (t-1) et suivantes (t+1) pour la quantité
      Prev_Year_Qty = lag(Trade_Quantity_Adjusted),
      Next_Year_Qty = lead(Trade_Quantity_Adjusted),
      
      # Calcul des quartiles et IQR pour Trade_Quantity_Adjusted
      Q1_Qty = quantile(Trade_Quantity_Adjusted, 0.25, na.rm = TRUE),
      Q3_Qty = quantile(Trade_Quantity_Adjusted, 0.75, na.rm = TRUE),
      IQR_Qty = Q3_Qty - Q1_Qty,
      Lower_Bound_Qty = Q1_Qty - k * IQR_Qty,
      Upper_Bound_Qty = Q3_Qty + k * IQR_Qty,
      
      # Identifier les outliers pour Trade_Quantity_Adjusted
      Is_Outlier_Qty = Trade_Quantity_Adjusted < Lower_Bound_Qty | Trade_Quantity_Adjusted > Upper_Bound_Qty,
      
      # Traitement des outliers pour Trade_Quantity_Adjusted
      Trade_Quantity_Corrected = case_when(
        # Cas 1 : Pas d'outlier, garder la valeur originale
        !Is_Outlier_Qty ~ Trade_Quantity_Adjusted,
        
        # Cas 2 : Outlier avec deux voisins valides (t-1 et t+1), moyenne des voisins
        Is_Outlier_Qty & !is.na(Prev_Year_Qty) & !is.na(Next_Year_Qty) ~ 
          (Prev_Year_Qty + Next_Year_Qty) / 2,
        
        # Cas 3 : Outlier avec un seul voisin valide (t-1 ou t+1), médiane des voisins
        Is_Outlier_Qty & (xor(!is.na(Prev_Year_Qty), !is.na(Next_Year_Qty))) ~ 
          median(c(Prev_Year_Qty, Next_Year_Qty), na.rm = TRUE),
        
        # Cas 4 : Outlier sans voisins valides, médiane du groupe
        Is_Outlier_Qty & is.na(Prev_Year_Qty) & is.na(Next_Year_Qty) ~ 
          median(Trade_Quantity_Adjusted[!Is_Outlier_Qty], na.rm = TRUE),
        
        # Par défaut, garder la valeur
        TRUE ~ Trade_Quantity_Adjusted
      ),
      
      # Ajustement proportionnel de Trade_Value_Adjusted
      Trade_Value_Corrected = case_when(
        # Si pas d'outlier sur la quantité, garder la valeur originale
        !Is_Outlier_Qty ~ Trade_Value_Adjusted,
        # Si outlier sur la quantité, ajuster proportionnellement
        Is_Outlier_Qty & Trade_Quantity_Adjusted != 0 ~ 
          Trade_Value_Adjusted * (Trade_Quantity_Corrected / Trade_Quantity_Adjusted),
        # Si quantité initiale est 0 (éviter division par 0), garder la valeur
        TRUE ~ Trade_Value_Adjusted
      )
    ) %>%
    ungroup()
  
  return(df_grouped)
}

# Appliquer la fonction au dataframe FOLOUR_trade_dt
FOLOUR_trade_dt_corrected <- detect_and_treat_outliers(FOLOUR_trade_dt, k = 1.5)

# Vérifier les résultats
head(FOLOUR_trade_dt_corrected)

##################################################################################
FABLE_Products_WA_VF2 <- FABLE_Products_WA_VF1 %>%
  distinct(Trade_Quantity_Adjusted, .keep_all = TRUE)
FABLE_Products_WA_VF3 <- FABLE_Products_WA_VF2 %>%
  filter(FABLE_Exporter != FABLE_Importer)
#################################################################################

FOLUR_trade_dt_vf2<-FOLOUR_trade_dt_corrected %>% 
  select(Exporter_ISO_Code,FABLE_Exporter,Importer_ISO_Code,FABLE_Importer,Year,FABLE_Item,Trade_Quantity_Adjusted,Trade_Quantity_Corrected,Trade_Value_Adjusted,Trade_Value_Corrected)

FOLUR_trade_dt_vf2 <- FOLUR_trade_dt_vf2 %>% mutate(Trade_Quantity_Corrected = format(Trade_Quantity_Corrected, scientific = FALSE), Trade_Value_Corrected = format(Trade_Value_Corrected, scientific = FALSE))

FOLUR_trade_dt_vf2 <- FOLUR_trade_dt_vf2 %>% 
  rename(Q=Trade_Quantity_Adjusted,
         V=Trade_Value_Adjusted)

colnames(FOLUR_trade_dt_vf2)
#Step2:

#Matching and regional dist aggregation

# Création des colonnes en faisant correspondre les codes ISO3 avec FABLE_cc
dist_cepii$FABLE_Exporter <- Country_mapping_250120_updated_2$FABLE_cc[match(dist_cepii$iso_o, Country_mapping_250120_updated_2$ISO3_code)]
dist_cepii$FABLE_Importer <- Country_mapping_250120_updated_2$FABLE_cc[match(dist_cepii$iso_d, Country_mapping_250120_updated_2$ISO3_code)]

dist_cepii_2<-dist_cepii %>%
  select(iso_o,FABLE_Exporter,iso_d,FABLE_Importer,distw)
dist_cepii_2 <- dist_cepii_2 %>% filter(!is.na(FABLE_Exporter) & !is.na(FABLE_Importer))
unique(dist_cepii_2$FABLE_Exporter)

dist_cepii_2 <- dist_cepii_2 %>%
  filter(iso_o != iso_d)

dist_cepii_2$distw <- as.numeric(dist_cepii_2$distw)

dist_cepii_agg<-dist_cepii_2%>%
  group_by(FABLE_Exporter,FABLE_Importer)%>%
  mutate(dist_adj=mean(distw,na.rm=TRUE),.groups="drop")
dist_cepii_agg<-dist_cepii_agg%>%
  select(-.groups)


dist_cepii_agg_2 <- dist_cepii_agg %>%
  filter(FABLE_Exporter != FABLE_Importer)%>%
  select(- distw)%>%
  distinct(dist_adj)
  
transport_costs_dt <- merge(dist_cepii_agg_2, FOLUR_trade_dt_vf2, 
                   by = c("FABLE_Exporter", "FABLE_Importer"), 
                   all.x = TRUE)

transport_costs_dt<-transport_costs_dt%>%
  select(Exporter_ISO_Code,FABLE_Exporter,Importer_ISO_Code,FABLE_Importer,Year,FABLE_Item,Trade_Quantity_Corrected,Trade_Value_Corrected,dist_adj)

Matching_HS6_to_name_product<-select(Matching_HS6_to_name_product,Code,HS6_Code_92,Product)
Matching_HS6_to_name_product<-Matching_HS6_to_name_product %>%
  mutate(Product = ifelse(Product == "Soybean", "Soyabean", Product))

#Freight rate for i,j,y,k

TUV_country_codes <- read.csv("C:/Users/Adnane/Desktop/BACI-MacMap/Transport_costs/CIF/TUV_country_codes_V202104.csv")
TUV_prod_codes <- read.csv("C:/Users/Adnane/Desktop/BACI-MacMap/Transport_costs/CIF/TUV_product_codes_HS96_V202104.csv")
colnames(TUV_country_codes)
#CIF (Imports)

CIF_2000 <- read.csv("C:/Users/Adnane/Desktop/BACI-MacMap/Transport_costs/CIF/TUV_HS96_m_Y2000_V202104.csv")
CIF_2005 <- read.csv("C:/Users/Adnane/Desktop/BACI-MacMap/Transport_costs/CIF/TUV_HS96_m_Y2005_V202104.csv")
CIF_2010 <- read.csv("C:/Users/Adnane/Desktop/BACI-MacMap/Transport_costs/CIF/TUV_HS96_m_Y2010_V202104.csv")
CIF_2015 <- read.csv("C:/Users/Adnane/Desktop/BACI-MacMap/Transport_costs/CIF/TUV_HS96_m_Y2015_V202104.csv")
CIF_2020 <- read.csv("C:/Users/Adnane/Desktop/BACI-MacMap/Transport_costs/CIF/TUV_HS96_m_Y2019_V202104.csv")

#FOB (Exports)

FOB_2000 <- read.csv("C:/Users/Adnane/Desktop/BACI-MacMap/Transport_costs/FOB/TUV_HS96_x_Y2000_V202104.csv")
FOB_2005 <- read.csv("C:/Users/Adnane/Desktop/BACI-MacMap/Transport_costs/FOB/TUV_HS96_x_Y2005_V202104.csv")
FOB_2010 <- read.csv("C:/Users/Adnane/Desktop/BACI-MacMap/Transport_costs/FOB/TUV_HS96_x_Y2010_V202104.csv")
FOB_2015 <- read.csv("C:/Users/Adnane/Desktop/BACI-MacMap/Transport_costs/FOB/TUV_HS96_x_Y2015_V202104.csv")
FOB_2020 <- read.csv("C:/Users/Adnane/Desktop/BACI-MacMap/Transport_costs/FOB/TUV_HS96_x_Y2019_V202104.csv")


colnames(transport_costs_dt)
#For 2020 : Baseline Year
#Country Mapping
CIF_2020$ISO3_exp <- TUV_country_codes$iso3_digit_alpha[match(CIF_2020$r, TUV_country_codes$country_code)]
CIF_2020$ISO3_imp <- TUV_country_codes$iso3_digit_alpha[match(CIF_2020$p, TUV_country_codes$country_code)]
CIF_2020$FABLE_Exporter <- Country_mapping_250120_updated_2$FABLE_cc[match(CIF_2020$ISO3_exp, Country_mapping_250120_updated_2$ISO3_code)]
CIF_2020$FABLE_Importer <- Country_mapping_250120_updated_2$FABLE_cc[match(CIF_2020$ISO3_imp, Country_mapping_250120_updated_2$ISO3_code)]

FOB_2020$ISO3_exp <- TUV_country_codes$iso3_digit_alpha[match(FOB_2020$r, TUV_country_codes$country_code)]
FOB_2020$ISO3_imp <- TUV_country_codes$iso3_digit_alpha[match(FOB_2020$p, TUV_country_codes$country_code)]
FOB_2020$FABLE_Exporter <- Country_mapping_250120_updated_2$FABLE_cc[match(FOB_2020$ISO3_exp, Country_mapping_250120_updated_2$ISO3_code)]
FOB_2020$FABLE_Importer <- Country_mapping_250120_updated_2$FABLE_cc[match(FOB_2020$ISO3_imp, Country_mapping_250120_updated_2$ISO3_code)]

CIF_2020 <- CIF_2020 %>%
  filter(!is.na(FABLE_Exporter), !is.na(FABLE_Importer))%>%
  filter(FABLE_Exporter != FABLE_Importer)

FOB_2020 <- FOB_2020 %>%
  filter(!is.na(FABLE_Exporter), !is.na(FABLE_Importer))%>%
  filter(FABLE_Exporter != FABLE_Importer)

#Product Mapping
CIF_2020 <- CIF_2020 %>%
  mutate(k = if_else(nchar(as.character(k)) == 5, 
                                    str_pad(as.character(k), width = 6, side = "left", pad = "0"),
                                    as.character(k)))

FOB_2020 <- FOB_2020 %>%
  mutate(k = if_else(nchar(as.character(k)) == 5, 
                     str_pad(as.character(k), width = 6, side = "left", pad = "0"),
                     as.character(k)))

CIF_2020$FABLE_Item<- Matching_HS6_to_name_product$Product[match(CIF_2020$k, Matching_HS6_to_name_product$Code)]
FOB_2020$FABLE_Item<- Matching_HS6_to_name_product$Product[match(FOB_2020$k, Matching_HS6_to_name_product$Code)]

CIF_2020 <- CIF_2020 %>%
  filter(!is.na(FABLE_Item))
FOB_2020 <- FOB_2020 %>%
  filter(!is.na(FABLE_Item))
#Primary equivalent conversion
#adjust_uv <- function(df) {
  #df %>%
    #mutate(uv_adj = case_when(
      
      # Beef
#  k %in% c("020130", "020230") ~ uv * 1.40,
#  k == "021020" ~ uv * 1.20,  
#   k == "160250" ~ uv * 1.42,  
#  k == "160100" ~ uv * 1.57,  
      
      # Palm_oil
#  k == "151190" ~ uv * 1.10, 
      
      # Corn
#    k %in% c("110220", "110313", "110812", "110423", "230210") ~ uv * 1.25,
      
      # Produits avec conversions spécifiques
#   k == "110814" ~ uv * (1 / 0.25),
#   k == "190300" ~ uv * (1 / 0.15),
      
      # Cocoa
#  #k %in% c("180310", "180320") ~ uv * (1 / 0.80) * (1 / 0.47),#
      #k %in% c("180500", "180610", "180620", "180631", "180632", "180690") ~ uv * (1 / 0.80) * (1 / 0.53),#
      
      # Coffee
#  k %in% c("210110", "210130") ~ uv * 2.6,
      
      # Eggs
#  k %in% c("040811", "040891") ~ uv * (1 / 0.25),
      
      # Grape
#  k %in% c("220421", "220429") ~ uv * (1 / 0.70),
#  k == "220430" ~ uv * (1 / 0.86),
# k == "200960" ~ uv * (1 / 0.75),
      
      # Grapefruit
#  k == "200920" ~ uv * (1 / 0.51),
      
      # Groundnut
#  k == "200811" ~ uv * (1 / 0.7),
      
      # Milk
# k %in% c("040210", "040221", "040229") ~ uv * (1 / 0.2),
      
      # Potato
#  FABLE_Item == "Potato" & k %in% c("071010", "200410", "200520") ~ uv * (1 / 0.5),
#  FABLE_Item == "Potato" & k %in% c("110510", "110520", "110813") ~ uv * (1 / 0.16),
      
      # Rice
#  FABLE_Item == "Rice" & k %in% c("100640", "100630") ~ uv * (1 / 0.67),
# FABLE_Item == "Rice" & k == "100620" ~ uv * (1 / 0.77),
# FABLE_Item == "Rice" & k == "110230" ~ uv * (1 / 0.70),
      
      #TRUE ~ uv  
    #))
#}

# Appliquer la fonction aux deux dataframes
#CIF_2020 <- adjust_uv(CIF_2020)
#FOB_2020 <- adjust_uv(FOB_2020)

#Aggregation of UV by i,j,t,k(P)
CIF_2020_agg <- CIF_2020 %>%
  group_by(FABLE_Exporter,FABLE_Importer, t, FABLE_Item) %>%
  summarise(
    uv_adj_cif = mean(uv_adj, na.rm = TRUE)
  ) %>%
  ungroup()

FOB_2020_agg <- FOB_2020 %>%
  group_by(FABLE_Exporter,FABLE_Importer, t, FABLE_Item) %>%
  summarise(
    uv_adj_fob = mean(uv_adj, na.rm = TRUE)
  ) %>%
  ungroup()

# Fusionner avec inner_join pour ne garder que les paires communes
Tuv_dt_2020 <- FOB_2020_agg %>%
  inner_join(CIF_2020_agg, 
             by = c("FABLE_Exporter", "FABLE_Importer", "t", "FABLE_Item"))



Tuv_dt_2020<-Tuv_dt_2020%>%
  rename(Year=t)

Tuv_dt_2020 <- Tuv_dt_2020 %>%
  mutate(Year = ifelse(Year == 2019, 2020, Year))

transport_costs_dt2<-transport_costs_dt%>%
  select(- Exporter_ISO_Code, - Importer_ISO_Code)%>%
  filter(Year=="2020")


#transport_costs_dt3 <- transport_costs_dt2 %>%
 # full_join(Tuv_dt_2020, 
            # by = c("FABLE_Exporter", "FABLE_Importer", "Year", "FABLE_Item"))%>%
 # filter(FABLE_Item!="Beef")
transport_costs_dt2 <- transport_costs_dt2 %>%
  mutate(
    Trade_Quantity_Corrected = as.numeric(as.character(Trade_Quantity_Corrected)),
    Trade_Value_Corrected = as.numeric(as.character(Trade_Value_Corrected))
  )








# Regrouper pour avoir un flux total par FABLE_Exporter, FABLE_Importer, FABLE_Item, et Year
transport_costs_dt4 <- transport_costs_dt2 %>%
  filter(!is.na(Trade_Quantity_Corrected) & Trade_Quantity_Corrected > 0 &
           !is.na(Trade_Value_Corrected) & Trade_Value_Corrected > 0) %>%
  group_by(FABLE_Exporter, FABLE_Importer, FABLE_Item, Year,dist_adj) %>%
  summarise(
    Trade_Quantity_Corrected = sum(Trade_Quantity_Corrected, na.rm = TRUE),
    Trade_Value_Corrected = sum(Trade_Value_Corrected, na.rm = TRUE),

  ) %>%
  ungroup()

# Joindre avec Tuv_dt_2020 (pour avoir uv_adj_fob et uv_adj_cif initiaux, mais on va les ajuster)
transport_costs_dt4 <- transport_costs_dt4 %>%
  full_join(Tuv_dt_2020, by = c("FABLE_Exporter", "FABLE_Importer", "Year", "FABLE_Item")) %>%
  filter(FABLE_Item != "Beef")

# Recalculer uv_adj_fob à partir de Trade_Value_Corrected et Trade_Quantity_Corrected
# Trade_Quantity_Corrected est en kt, Trade_Value_Corrected est en kUSD, donc uv_adj_fob sera en USD/tonne
transport_costs_dt4 <- transport_costs_dt4 %>%
  mutate(
    uv_adj_fob = (Trade_Value_Corrected / Trade_Quantity_Corrected) * 1000  # Conversion kt -> tonnes
  )

# Estimer uv_adj_cif en utilisant un freight_adv réaliste basé sur la distance
# Supposons un freight_adv de 10% pour < 2000 km, 20% pour 2000-5000 km, 30% pour > 5000 km
transport_costs_dt4 <- transport_costs_dt4 %>%
  mutate(
    freight_adv_estimated = case_when(
      dist_adj < 2000 ~ 0.10,
      dist_adj >= 2000 & dist_adj < 5000 ~ 0.20,
      TRUE ~ 0.30
    ),
    uv_adj_cif = uv_adj_fob * (1 + freight_adv_estimated)
  )

# Définir des seuils absolus pour chaque produit (basés sur des prix réalistes en 2020)
price_thresholds <- tibble(
  FABLE_Item = c("Rice", "Cocoa", "Coffee", "Corn", "Palm Oil", "Soyabean", "Wheat"),
  fob_min = c(400, 2500, 2000, 150, 600, 300, 200),
  fob_max = c(600, 5000, 4000, 300, 1000, 500, 400),
  cif_min = c(450, 2700, 2200, 170, 650, 330, 220),
  cif_max = c(700, 5500, 4500, 350, 1100, 550, 450)
)

# Joindre les seuils
transport_costs_dt4 <- transport_costs_dt4 %>%
  left_join(price_thresholds, by = "FABLE_Item")

# Calculer les bornes IQR et médianes par FABLE_Exporter et FABLE_Item
bounds <- transport_costs_dt4 %>%
  group_by(FABLE_Exporter, FABLE_Item) %>%
  summarise(
    Q1_fob = quantile(uv_adj_fob, 0.25, na.rm = TRUE),
    Q3_fob = quantile(uv_adj_fob, 0.75, na.rm = TRUE),
    IQR_fob = Q3_fob - Q1_fob,
    lower_fob = Q1_fob - 1.5 * IQR_fob,
    upper_fob = Q3_fob + 1.5 * IQR_fob,
    Q1_cif = quantile(uv_adj_cif, 0.25, na.rm = TRUE),
    Q3_cif = quantile(uv_adj_cif, 0.75, na.rm = TRUE),
    IQR_cif = Q3_cif - Q1_cif,
    lower_cif = Q1_cif - 1.5 * IQR_cif,
    upper_cif = Q3_cif + 1.5 * IQR_cif,
    median_fob = median(uv_adj_fob, na.rm = TRUE),
    median_cif = median(uv_adj_cif, na.rm = TRUE)
  )

# Joindre les bornes et détecter les outliers (IQR + seuils absolus)
transport_costs_dt4 <- transport_costs_dt4 %>%
  left_join(bounds, by = c("FABLE_Exporter", "FABLE_Item")) %>%
  mutate(
    outlier_fob = (uv_adj_fob < lower_fob | uv_adj_fob > upper_fob) | 
      (uv_adj_fob < fob_min | uv_adj_fob > fob_max),
    outlier_cif = (uv_adj_cif < lower_cif | uv_adj_cif > upper_cif) | 
      (uv_adj_cif < cif_min | uv_adj_cif > cif_max),
    uv_adj_fob = case_when(
      outlier_fob == TRUE ~ median_fob,
      TRUE ~ uv_adj_fob
    ),
    uv_adj_cif = case_when(
      outlier_cif == TRUE ~ median_cif,
      TRUE ~ uv_adj_cif
    )
  ) %>%
  select(-Q1_fob, -Q3_fob, -IQR_fob, -lower_fob, -upper_fob,
         -Q1_cif, -Q3_cif, -IQR_cif, -lower_cif, -upper_cif,
         -median_fob, -median_cif, -outlier_fob, -outlier_cif,
         -fob_min, -fob_max, -cif_min, -cif_max)

# Vérifier une dernière fois que uv_adj_cif >= uv_adj_fob
transport_costs_dt4 <- transport_costs_dt4 %>%
  mutate(
    uv_adj_cif_new = ifelse(uv_adj_cif < uv_adj_fob, uv_adj_fob, uv_adj_cif),
    uv_adj_fob_new = ifelse(uv_adj_cif < uv_adj_fob, uv_adj_cif, uv_adj_fob),
    uv_adj_cif = uv_adj_cif_new,
    uv_adj_fob = uv_adj_fob_new
  ) %>%
  select(-uv_adj_cif_new, -uv_adj_fob_new)

# Recalculer freight_adv, ln_freight et transport_cost_usd_t
transport_costs_dt4 <- transport_costs_dt4 %>%
  mutate(
    freight_adv = (uv_adj_cif - uv_adj_fob) / uv_adj_fob,
    ln_freight = ifelse(freight_adv > 0, log(freight_adv), NA),
    transport_cost_usd_t = freight_adv * uv_adj_fob
  )

# Calculer ln_dist, ln_wgt_v, alpha et epsilon (Hummels)
beta <- 0.26
delta <- 0.24

transport_costs_dt4 <- transport_costs_dt4 %>%
  mutate(
    ln_dist = log(dist_adj),
    ln_wgt_v = log(Trade_Quantity_Corrected / Trade_Value_Corrected),
    alpha = ln_freight - beta * ln_dist - delta * ln_wgt_v,
    epsilon = ln_freight - alpha - beta * ln_dist - delta * ln_wgt_v
  ) %>%
  select(-freight_adv_estimated)  # Supprimer la colonne temporaire







# Regrouper pour avoir un flux total par FABLE_Exporter, FABLE_Importer, FABLE_Item, et Year
transport_costs_dt4 <- transport_costs_dt2 %>%
  filter(!is.na(Trade_Quantity_Corrected) & Trade_Quantity_Corrected > 0 &
           !is.na(Trade_Value_Corrected) & Trade_Value_Corrected > 0) %>%
  group_by(FABLE_Exporter, FABLE_Importer, FABLE_Item, Year, dist_adj) %>%
  summarise(
    Trade_Quantity_Corrected = sum(Trade_Quantity_Corrected, na.rm = TRUE),
    Trade_Value_Corrected = sum(Trade_Value_Corrected, na.rm = TRUE)
  ) %>%
  ungroup()

# Calculer ln_dist et ln_wgt_v
transport_costs_dt4 <- transport_costs_dt4 %>%
  mutate(
    ln_dist = log(dist_adj),
    ln_wgt_v = log(Trade_Quantity_Corrected / Trade_Value_Corrected)
  )

# Calculer ln_dist et ln_quantity_kt
transport_costs_dt4 <- transport_costs_dt4 %>%
  mutate(
    ln_dist = log(dist_adj),
    quantity_kt = Trade_Quantity_Corrected *1000,  # Conversion en kilotonnes
    ln_quantity_kt = log(quantity_kt)  # Logarithme de la quantité en kt
  )

# Regrouper pour avoir un flux total par FABLE_Exporter, FABLE_Importer, FABLE_Item, et Year
transport_costs_dt4 <- transport_costs_dt2 %>%
  filter(!is.na(Trade_Quantity_Corrected) & Trade_Quantity_Corrected > 0 &
           !is.na(Trade_Value_Corrected) & Trade_Value_Corrected > 0) %>%
  group_by(FABLE_Exporter, FABLE_Importer, FABLE_Item, Year, dist_adj) %>%
  summarise(
    Trade_Quantity_Corrected = sum(Trade_Quantity_Corrected, na.rm = TRUE),
    Trade_Value_Corrected = sum(Trade_Value_Corrected, na.rm = TRUE)
  ) %>%
  ungroup()

# Calculer ln_dist et ln_quantity_kt
transport_costs_dt4 <- transport_costs_dt4 %>%
  mutate(
    ln_dist = log(dist_adj),
    quantity_kt = Trade_Quantity_Corrected / 1000,  # Conversion en kilotonnes
    ln_quantity_kt = log(quantity_kt)  # Logarithme de la quantité en kt
  )

base_cost <- 0.005  # Ajusté pour des coûts réalistes (7-153 USD/tonne)
beta <- 0.26       # Réduit pour limiter l'amplification exponentielle
gamma <- -0.01     # Conservé pour de légères économies d'échelle

transport_costs_dt4 <- transport_costs_dt4 %>%
  mutate(
    cost_per_tonne_km = base_cost * exp(beta * ln_dist + gamma * ln_quantity_kt),
    transport_cost_usd_t = cost_per_tonne_km * dist_adj
  )



FOLUR_trade_dt <- read.csv("C:\\Users\\Adnane\\Desktop\\BACI-MacMap\\FOLUR_bil_4_final_spe.csv")
