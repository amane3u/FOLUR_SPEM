Scenathon_2023_dt <- read_excel("C:/Users/Adnane/Desktop/Factsheets/Scenathon_df2023.xlsx")

colnames(Scenathon_2023_dt)
unique(Scenathon_2023_dt$pathway_id)
unique(Scenathon_2023_dt$tradeadjustment)


#Total Demande and Total Supply for CT

Scenathon_2023_dt_spem<-Scenathon_2023_dt%>%
  group_by(pathway_id,tradeadjustment,location,product,year)%>%
  mutate(supply_q = prodq_feas +import_quantity + stockvar,
         demande_q =supply_q - export_quantity - prodlossesq,
         total_input_cost = fertilizercost + labourcost + machineryrunningcost + dieselcost + pesticidecost)%>%
  select(pathway_id,tradeadjustment,location,year,product,demande_q,supply_q,total_input_cost)

Folur_sch_dt <- Scenathon_2023_dt_spem %>%
  filter(tradeadjustment=="No")%>%
  filter(product %in% c("beef","corn","wheat","soyabean","cocoa","coffee","rice","palm_oil"))%>%
  filter(location !="WORLD")

write.csv(Scenathon_2023_dt_spem, 
          file = "C:\\Users\\Adnane\\Desktop\\BACI-MacMap\\MAC-MAPHS6\\data_output\\250423_totaleq_spem.csv", 
          row.names = FALSE)


write.csv(Folur_sch_dt, 
          file = "C:\\Users\\Adnane\\Desktop\\BACI-MacMap\\MAC-MAPHS6\\data_output\\250423_Folur_totaleq.csv", 
          row.names = FALSE)


################ For baseline Year ################

# Filtrer pathway, produit et années
totaleq_folur_ct <- Folur_sch_dt %>%
  filter(pathway_id == "CT" & product != "beef" & year %in% c(2000, 2005, 2010, 2015, 2020))

# Ajouter Regional_lv
totaleq_folur_ct$Regional_lv <- Country_mapping_250120_updated_2$regional_lv[
  match(totaleq_folur_ct$location, Country_mapping_250120_updated_2$FABLE_cc)
]

# Imputation des coûts (par année)
totaleq_folur_ct_adj <- totaleq_folur_ct %>%
  group_by(location, year) %>%
  mutate(n_nonzero = sum(!is.na(total_input_cost)),
         nat_mean = mean(total_input_cost, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(Regional_lv, product, year) %>%
  mutate(reg_mean = mean(total_input_cost, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(product, year) %>%
  mutate(global_mean = mean(total_input_cost, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(total_input_cost = if_else(
    total_input_cost == 0,
    coalesce(
      if_else(n_nonzero >= 2, nat_mean, reg_mean),
      global_mean
    ),
    total_input_cost
  )) %>%
  select(-n_nonzero, -nat_mean, -reg_mean, -global_mean)


