rm(list = ls())
bilateral_trade_cost_RICE <- read.csv("C:/Users/Adnane/Desktop/BACI-MacMap/SPE_doc/SPE_jasper_input/Trade_cost/bilateral_trade_cost_RICE.csv")
country_information_RICE <- read.csv("C:/Users/Adnane/Desktop/BACI-MacMap/SPE_doc/SPE_jasper_input/Country_data/country_information_RICE.csv")
country_information_SOYB<- read.csv("C:/Users/Adnane/Desktop/BACI-MacMap/SPE_doc/SPE_jasper_input/Country_data/country_information_SOYB.csv")




FOLUR_trade_dt_2<- read.csv("C:/Users/Adnane/Documents/GitHub/FOLUR_SPEM/data/250403_FOLUR_trade_dt.csv")
FOLUR_trade_dt_2<- FOLUR_trade_dt_2 %>%
  select(FABLE_Exporter, FABLE_Importer, Year, FABLE_Item, Q_export, net_trade_ij) %>%
  rename(Q_ij = Q_export)
write.csv(FOLUR_trade_dt_2, 
          file = "C:\\Users\\Adnane\\Desktop\\BACI-MacMap\\MAC-MAPHS6\\data_output\\250403_FOLUR_trade_dt.csv", 
          row.names = FALSE)




#Load DATA
FOLUR_trade_dt<- read.csv("C:/Users/Adnane/Documents/GitHub/FOLUR_SPEM/data/250403_FOLUR_trade_dt.csv")
tarrif_dt<- read.csv("C:/Users/Adnane/Documents/GitHub/FOLUR_SPEM/data/250424_tariff_specific_values_red.csv")
trans_cost_dt<- read.csv("C:/Users/Adnane/Documents/GitHub/FOLUR_SPEM/data/250403_transport_costs_dt.csv")
FAO_prices_dt_2<- read.csv("C:/Users/Adnane/Documents/GitHub/FOLUR_SPEM/data/250423_FAO_prices_dt_red.csv")
FAO_prices_dt_2<-FAO_prices_dt_2%>%
  filter(FABLE_Exporter!= "IRL")
#Rice

Rice_2020_bil_trade <- FOLUR_trade_dt %>%
  filter(FABLE_Item == "Rice" & Year == 2020)
Rice_2020_trans_cost <- trans_cost_dt %>%
  filter(FABLE_Item == "Rice" & Year == 2020)%>%
  select(FABLE_Exporter,FABLE_Importer,Year,FABLE_Item,Q_ij,net_trade_ij,transport_cost_usd_t)
Rice_2020_tarrif_cost<-tarrif_dt%>%
  filter(FABLE_Item == "Rice" & Year == 2020)


Rice_2020_prices <- FAO_prices_dt_2 %>%
  filter(FABLE_Item == "Rice", Year == 2020) %>%
  distinct()

list_of_countries<-FAO_prices_dt_2 %>%
  distinct(FABLE_Exporter)
Rice_2020_prices <- Rice_2020_prices %>%
  full_join(list_of_countries, by = c("FABLE_Exporter")) %>%
  mutate(
    prodprice_i_agg = ifelse(is.na(prodprice_i_agg), 0, prodprice_i_agg),
    FABLE_Item = ifelse(is.na(FABLE_Item), "Rice", FABLE_Item),
    Year = ifelse(is.na(Year), 2020, Year)
  )

Rice_2020_prices$Regional_lv <- Country_mapping_250120_updated_2$regional_lv[match(Rice_2020_prices$FABLE_Exporter, Country_mapping_250120_updated_2$FABLE_cc)]

Rice_2020_prices_2 <- Rice_2020_prices %>%
  group_by(Regional_lv, FABLE_Item) %>%
  mutate(prodprice_i_agg = ifelse(prodprice_i_agg == 0, mean(prodprice_i_agg[prodprice_i_agg > 0], na.rm = TRUE), prodprice_i_agg)) %>%
  ungroup()



Rice_2020_prices_2 <- Rice_2020_prices_2 %>%
  select(-Regional_lv)


























bilateral_trade_cost_RICE_<-Rice_2020_trans_cost%>%
  full_join(Rice_2020_tarrif_cost, by = c("FABLE_Exporter", "FABLE_Importer", "FABLE_Item","Year"))



country_information_rice<-Rice_2020_prices_2%>%
  select(FABLE_Exporter,prodprice_i_agg)%>%
  rename(iso3 = FABLE_Exporter,
         Production_USD_t = prodprice_i_agg  )


write.csv(country_information_rice, 
          file = "C:/Users/Adnane/Desktop/SPE_inputs/SPE_adaptation_adnane/Model_input/country_information_RICE.csv", 
          row.names = FALSE)

bilateral_trade_cost_rice<-bilateral_trade_cost_RICE_%>%
  select(FABLE_Exporter,FABLE_Importer,Q_ij,transport_cost_usd_t,adv_ij_agg,sdt_ij_agg)%>%
  rename(from_iso3 = FABLE_Exporter,
         to_iso3 = FABLE_Importer,
         q_calib = Q_ij,
         trade_USD_t = transport_cost_usd_t,
         adv = adv_ij_agg,
         sdt_USD_t = sdt_ij_agg)


bilateral_trade_cost_rice <- bilateral_trade_cost_rice %>%
  filter(from_iso3 != "IRL")%>%
  filter(to_iso3 != "IRL")


