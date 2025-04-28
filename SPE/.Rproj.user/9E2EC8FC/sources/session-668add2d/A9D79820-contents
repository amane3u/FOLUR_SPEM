

folder_path <- "C:\\Users\\Adnane\\Desktop\\BACI-MacMap\\BACI"

files_to_import <- c("BACI_HS92_Y2000_V202401b", 
                     "BACI_HS92_Y2005_V202401b", 
                     "BACI_HS92_Y2010_V202401b", 
                     "BACI_HS92_Y2015_V202401b", 
                     "BACI_HS92_Y2020_V202401b", 
                     "country_codes_v202401b", 
                     "product_codes_HS92_v202401b",
                     "BACI_HS12_Y2015_V202401b",
                     "BACI_HS12_Y2020_V202401b")

for (file in files_to_import) {
  assign(tools::file_path_sans_ext(file), read.csv(file.path(folder_path, paste0(file, ".csv"))))
}




BACI_HS12_Y2015_V202401b<-BACI_HS12_Y2015_V202401b%>%
  filter(k %in% c("71430", "80310"))

BACI_HS12_Y2020_V202401b<-BACI_HS12_Y2020_V202401b%>%
  filter(k %in% c("71430", "80310"))

list_dataframes<-list(BACI_HS92_Y2000_V202401b,BACI_HS92_Y2005_V202401b,BACI_HS92_Y2010_V202401b,BACI_HS92_Y2015_V202401b,BACI_HS92_Y2020_V202401b,BACI_HS12_Y2015_V202401b,BACI_HS12_Y2020_V202401b)
list_dataframes <- lapply(list_dataframes, as.data.table)
merged_data_BACI <- rbindlist(list_dataframes, use.names = TRUE, fill = TRUE)

colnames(merged_data_BACI)
table(merged_data_BACI$t)

list_item<-c(170112,170191,170199,170410,170490,080810,080300,100300,071331,071332,071333,020110,020120,020210,020220,020130,020230,071410,
             110814,100810,110290,110319,100890,110430,080590,200930,090700,180100,180310,180320,180500,180610,180620,180631,180632,180690,
             080110,120300,090111,090112,090121,090122,210110,210130,120720,080410,350210,040700,040819,040899,040811,040891,080820,080910,
             081310,080920,080930,080940,081320,081010,081020,081030,081040,080710,080420,080720,080450,081090,080440,081340,081350,190300,
             081330,200980,081110,081120,081210,081400,200791,200799,200830,200840,200850,200860,200870,200880,200891,200899,110630,200600,
             200710,080540,200920,080610,080620,200960,220421,220429,220430,120210,120220,200811,080530,100510,100590,110220,110313,110812,
             040120,040130,040299,040310,040390,040291,040221,040229,040210,040410,040610,040620,040630,040640,040690,040110,040490,100820,
             020410,020421,020422,020423,020430,020441,020442,020443,020450,080120,080130,080240,080211,080231,080250,080290,080221,080212,
             080232,080222,200819,100400,110412,110422,120799,120730,120760,120791,120400,120810,120890,071120,200570,070310,080510,200911,
             200919,151110,151190,120710,071310,090411,090412,020311,020312,020319,020321,020322,020329,021011,021012,021019,160241,160242,
             090420,080430,200820,200940,080310,070110,070190,110510,110520,071010,110813,020710,020750,020741,020742,020739,020731,020723,
             020722,020721,071350,071320,071339,071340,121490,071390,110610,120500,120750,210330,100610,100620,100630,100640,071490,100200,
             120740,100700,120100,210310,090500,090610,090620,090810,090820,090830,090920,090930,090950,091010,091020,091030,091091,091099,
             121291,120600,071420,090210,090220,090230,090240,090300,210120,240110,240120,240130,240220,240290,240210,240310,240391,240399,
             070200,200950,200290,200210,070420,070490,070990,070920,070511,070519,070521,070529,070970,070410,070700,070930,070960,070320,
             070390,070820,070810,070890,070610,071040,200580,070951,070952,071230,200310,200320,121299,121292,070690,070940,071220,071290,
             200110,200520,200540,200551,200559,200560,200590,071021,071022,071029,071030,071080,071090,071140,071190,200410,200490,200510,
             090140,100110,100190,110100,110311,190211,190219,190510,190540,190520,190530,110811,110900,190410,190490,190120,190190,071430,
             151311,151319,151221,151229,230610,230500,150810,150890,151590,151530,151511,151519,150990,151790,151710,151800,151610,151620,
             230690,230620,150910,151000,230660,151321,151329,230640,151410,151490,230400,150710,150790,230630,151211,151219,151550,110620,
             200892,110230,110423,230210,160250,160210,160100,170310,170390,150200,021020)
        

             
FABLE_Products<-merged_data_BACI%>%
  filter(k %in%list_item)

FABLE_Products <- FABLE_Products %>%
  mutate(FABLE_Item = case_when(
    
    k %in% c(170112, 170191, 170199, 170410, 170490) ~ "Sugar Raw",

    k %in% c(080810) ~ "Apple",
    k %in% c(080300) ~ "Banana",
    k %in% c(100300) ~ "Barley",
    k %in% c(071331,071332,071333) ~ "Beans",
    k %in% c(020110,020120,020210,020220,020130,020230,160250,160210,160100,021020) ~ "Beef",
    k %in% c(071410,110814,110620,190300) ~ "Cassava",
    
    k %in% c(100810,110290, 110319,100890,110430) ~ "Cereal_other",
    k %in% c(080590,200930) ~ "Citrus_other",
    k %in% c(090700) ~ "Clove",
    k %in% c(180100,180310,180320,180500,180610, 180620, 180631, 180632, 180690) ~ "Cocoa",
    k %in% c(080110,120300) ~ "Coconut",
    k %in% c(090111,090112,090121,090122,210110,210130) ~ "Coffee",
    k %in% c(120720) ~ "Cotton",
    
    k %in% c(080410) ~ "Date",
    k %in% c(350210,040700,040819,040899,040811,040891) ~ "Eggs",
    
    k %in% c(080820,080910,081310,080920,080930,080940,081320,081010,081020,081030,081040,
             080710,080420,080720,080450,081090,
             080440,081340,081350,081330,200980,081110,081120,081210,
             081400,200791,200799,200830,200840,200850,200860,200870,200880,
             200891,200893,200897,200899,110630,200600,200710,	
             200892) ~ "Fruit_other",
    
    k %in% c(080540,200920) ~ "Grapefruit",
    k %in% c(080610,080620,200960,220421,220429,220430) ~ "Grape",
    k %in% c(120210,120220,200811) ~ "Groundnut",

    k %in% c(080530) ~ "Lemon",
    k %in% c(100510,100590,110220,110313,110812,110423,230210) ~ "Corn",
  
    
    k %in% c(040120,040130,040299,040310,040390,040291,040221,040229,040210,040410,040610,040620,040630,040640,040690,040110,040490) ~ "Milk",
    k %in% c(100820) ~ "Millet",
    k %in% c(020410, 020421, 020422, 020423, 020430, 020441, 020442, 020443,020450) ~ "Mutton_goat",
    k %in% c(080120,080130,080240,080211,080231,080250,080290,080221,080212,080232,080222,200819) ~ "Nuts",
    
    k %in% c(100400,110412,110422) ~ "Oats",
    k %in% c(120799,120730,120760,120791,120400,120810,120890) ~ "Oilseed_other",
    k %in% c(071120,200570) ~ "Olive",
    k %in% c(070310) ~ "Onion",
    
    k %in% c(080510,200911,200919) ~ "Orange",
    k %in% c(151110, 151190) ~ "Palm_oil", 
    k %in% c(120710) ~ "Oil_Palmfruit",
    k %in% c(071310) ~ "Peas",
    k %in% c(090411,090412) ~ "Pepper",
    k %in% c(020311,020312,020319,020321,020322,020329,021011,021012,021019,160241,160242) ~ "Pork",
    
    k %in% c(090420) ~ "Piment",
    k %in% c(080430,200820,200940) ~ "Pinapple",
    k %in% c(080310) ~ "Plantain",
    k %in% c(070110,070190,110510,110520,071010,110813,200520,200410) ~ "Potato",
    k %in% c(020710,020750,020743,020742,020741,020739,020731,020723,020722,020721) ~ "Chicken",
    k %in% c(071350,071320,071339,071340,121490,071390,110610) ~ "Pulses_other",
    
    k %in% c(120500,120750,210330) ~ "Rapeseed",
    k %in% c(100610,100620,100630,100640,110230) ~ "Rice",
    k %in% c(071490) ~ "Tuber_other",
    k %in% c(100200) ~ "Rye",
    k %in% c(120740) ~ "Sesame",

    k %in% c(100700) ~ "Sorghum",
    k %in% c(120100,210310) ~ "Soyabean",
    k %in% c(090500,090610,090620,090810,090820,090830,090920,090930,090950,091010,091020,091030,091091,091099) ~ "Spices_other",
    k %in% c(121291,170390) ~ "Sugarbeet",
    k %in% c(121292,170310) ~ "Sugarcane",  
    k %in% c(120600) ~ "Sunflower",
    k %in% c(071420) ~ "Sweet_potato",
    k %in% c(090210,090220,090230,090240,090300,210120) ~ "Tea",
    k %in% c(240110, 240120,240130,240220,240290,240210,240310, 240391,240399) ~ "Tobacco",  
    k %in% c(070200,200950,200290,200210) ~ "Tomato",
    k %in% c(070420, 070490,070990,070920,070511, 070519, 070521, 070529,070970,070410,070700,070930,070960,
             070320,070390,070820,070810,070890,070610,071040,200580,070951,070952,071230,200310, 
             200320,121299,070690,070940,071220,071290,200110,200540, 200551, 200559, 
             200560,200590,071021, 071022, 071029, 071030, 071080, 071090,071140,071190,200490,200510,090140) ~ "Vegetable_other",
   
    k %in% c(100110, 100190,110100, 110311, 
             190211, 190219,190510, 190540,
             190520, 190530,110811,110900,
             190410, 190490,190120,190190) ~ "Wheat",
    k %in% c(071430) ~ "Yams",  
    k %in% c(151311, 151319) ~ "Coco_Oil",
    k %in% c(151221,151229) ~ "Cottonseed_Oil",  
    k %in% c(230610) ~ "Cottonseed_cake",
    k %in% c(230500) ~ "Groundnut_cake",
    
    k %in% c(150810,150890) ~ "Groundnut_Oil",  
    k %in% c(151590,151530,151511, 151519,150990,151790,151710,151800,151610, 151620) ~ "Other_Oil",
    k %in% c(230690,230620) ~ "Other_olscake",
    k %in% c(150910,151000) ~ "Olive_Oil",  
    k %in% c(230660) ~ "Palmkernel_cake",
    k %in% c(151321,151329) ~ "Palmkernel_Oil",
    
    k %in% c(230640) ~ "Rape_cake",
    k %in% c(151410,151490) ~ "Rape_Oil",  
    k %in% c(230400) ~ "Soy_cake",
    k %in% c(150710,150790) ~ "Soy_oil",
    k %in% c(230630) ~ "Sunfl_cake",
    k %in% c(151211,151219) ~ "Sunfl_Oil",  
    k %in% c(151550) ~ "Sesam_Oil"
   
 ))



FABLE_Products$q <- as.numeric(FABLE_Products$q) 
FABLE_Products$v <- as.numeric(FABLE_Products$V) 

#Association des countries codes and name with BACI country code

get_country_info <- function(code, df_info) {
  info <- df_info[df_info$country_code == code, ]
  if (nrow(info) > 0) {
    return(list(info$country_name, info$country_iso3))
  } else {
    return(list(NA, NA)) 
  }
}


exporter_info <- sapply(FABLE_Products$i, get_country_info, df_info = country_codes_v202401b)
importer_info <- sapply(FABLE_Products$j, get_country_info, df_info = country_codes_v202401b)

FABLE_Products$Country_Exporter <- exporter_info[1, ]
FABLE_Products$Country_Exporter_Code <- exporter_info[2, ]
FABLE_Products$Country_Importer <- importer_info[1, ]
FABLE_Products$Country_Importer_Code <- importer_info[2, ]

FABLE_Products<-select(FABLE_Products,Country_Exporter_Code,i,Country_Importer_Code,j,t,k,FABLE_Item,q,v)





#FABLE Country matching
valid_ISO3 <- Country_mapping_250120_updated_2$ISO3_code

FABLE_Products <- FABLE_Products[FABLE_Products$Country_Exporter_Code %in% valid_ISO3 & 
                                   FABLE_Products$Country_Importer_Code %in% valid_ISO3, ]

 



FABLE_Products<-select(FABLE_Products,Country_Exporter_Code,FABLE_Exporter,Country_Importer_Code,FABLE_Importer,t,k,FABLE_Item,q,v)
FABLE_Products<-FABLE_Products %>%
  rename( Year=t,
          HS6_Product_Code=k,
          Trade_Quantity_Raw=q,
          Trade_Value=v)
  





FABLE_Products$Trade_Quantity_Raw <- as.numeric(FABLE_Products$Trade_Quantity_Raw)
FABLE_Products$Trade_Value <- as.numeric(FABLE_Products$Trade_Value)



# Ajouter un zéro devant les codes HS6 à 5 chiffres
FABLE_Products <- FABLE_Products %>%
  mutate(HS6_Product_Code = if_else(nchar(as.character(HS6_Product_Code)) == 5, 
                                    str_pad(as.character(HS6_Product_Code), width = 6, side = "left", pad = "0"),
                                    as.character(HS6_Product_Code)))


  FABLE_Products_PE_raw <- FABLE_Products %>%
    mutate(Trade_Quantity_Adjusted_FR = case_when(
      
      #Beef
      HS6_Product_Code == "020130" ~ Trade_Quantity_Raw * (1.40),
      HS6_Product_Code == "020230" ~ Trade_Quantity_Raw * (1.40),
      HS6_Product_Code == "021020" ~ Trade_Quantity_Raw * (1.20),  
      HS6_Product_Code == "160250" ~ Trade_Quantity_Raw * (1.42),  
      HS6_Product_Code == "160100" ~ Trade_Quantity_Raw * (1.57),  
      
      #Cheecken
      
      # Corn
      HS6_Product_Code %in% c("110220","110313","110812","110423")~ Trade_Quantity_Raw * (1.25),
      HS6_Product_Code == "230210" ~ Trade_Quantity_Raw * (1.25),
      
      HS6_Product_Code == "110814" ~ Trade_Quantity_Raw * (1 / 0.25),  # Avec conversion
      HS6_Product_Code == "190300" ~ Trade_Quantity_Raw * (1 / 0.15),  # Avec conversion
      
      # Cocoa
      HS6_Product_Code %in% c("180310", "180320") ~ Trade_Quantity_Raw * (1 / 0.80) * (1 / 0.47),  # Avec conversion
      HS6_Product_Code %in% c("180500", "180610", "180620", "180631", "180632", "180690") ~ Trade_Quantity_Raw * (1 / 0.80) * (1 / 0.53),  # Avec conversion
      
      # Coffee
      HS6_Product_Code %in% c("210110", "210130") ~ Trade_Quantity_Raw * 2.6,  # Avec conversion
      
      # Eggs
      HS6_Product_Code %in% c("040811", "040891") ~ Trade_Quantity_Raw * (1 / 0.25),  # Avec conversion
      
      # Grape
      HS6_Product_Code == "220421" ~ Trade_Quantity_Raw * (1 / 0.70),  # Avec conversion
      HS6_Product_Code == "220429" ~ Trade_Quantity_Raw * (1 / 0.70),  # Avec conversion
      HS6_Product_Code == "220430" ~ Trade_Quantity_Raw * (1 / 0.86),  # Avec conversion
      HS6_Product_Code == "200960" ~ Trade_Quantity_Raw * (1 / 0.75),  # Avec conversion
      
      # Grapefruit
      HS6_Product_Code == "200920" ~ Trade_Quantity_Raw * (1 / 0.51),  # Avec conversion
      
      # Groundnut
      HS6_Product_Code == "200811" ~ Trade_Quantity_Raw * (1 / 0.7),  # Avec conversion
      
      # Milk
      HS6_Product_Code %in% c("040210", "040221", "040229") ~ Trade_Quantity_Raw * (1 / 0.2),  # Avec conversion
      
      # Potato
      FABLE_Item == "Potato" & HS6_Product_Code %in% c("071010", "200410", "200520") ~ Trade_Quantity_Raw * (1 / 0.5),  # Avec conversion
      FABLE_Item == "Potato" & HS6_Product_Code %in% c("110510", "110520", "110813") ~ Trade_Quantity_Raw * (1 / 0.16),  # Avec conversion
      
      # Rice
      FABLE_Item == "Rice" & HS6_Product_Code == "100640" ~ Trade_Quantity_Raw * (1 / 0.67),  # Avec conversion
      FABLE_Item == "Rice" & HS6_Product_Code == "100630" ~ Trade_Quantity_Raw * (1 / 0.67),  # Avec conversion
      FABLE_Item == "Rice" & HS6_Product_Code == "100620" ~ Trade_Quantity_Raw * (1 / 0.77),  # Avec conversion
      FABLE_Item == "Rice" & HS6_Product_Code == "110230" ~ Trade_Quantity_Raw * (1 / 0.70),  # Avec conversion
      
      TRUE ~ Trade_Quantity_Raw  
    ))  

  

  
  
  
  
# Convertir les colonnes si elles ne sont pas du bon type
  FABLE_Products$Exporter_ISO_Code <- as.character(FABLE_Products$Exporter_ISO_Code)
  FABLE_Products$Importer_ISO_Code <- as.character(FABLE_Products$Importer_ISO_Code)
  FABLE_Products$Year <- as.integer(FABLE_Products$Year) # ou as.numeric si c'est une valeur numérique
  FABLE_Products$FABLE_Item <- as.character(FABLE_Products$FABLE_Item)
  FABLE_Products$Trade_Quantity_Raw <- as.numeric(FABLE_Products$Trade_Quantity_Raw)
  


  # Étape 1 : Calculer les Parts de Marché
FABLE_Products_WA_1 <- FABLE_Products_PE_raw %>%
  group_by(Exporter_ISO_Code, Importer_ISO_Code, Year, FABLE_Item) %>%
  mutate(
    Total_Quantity = sum(Trade_Quantity_Adjusted_FR, na.rm = TRUE),
    Market_Share = Trade_Quantity_Adjusted_FR / Total_Quantity
  ) %>%
  ungroup()

# Étape 2 : Ajuster les Quantités en utilisant les Parts de Marché
FABLE_Products_WA_2 <- FABLE_Products_WA_1 %>%
  group_by(Exporter_ISO_Code, Importer_ISO_Code, Year, FABLE_Item) %>%
  mutate(
    Trade_Quantity_Adjusted = sum(Market_Share * Trade_Quantity_Adjusted_FR, na.rm = TRUE)
  ) %>%
  ungroup()

# Étape 3 : Répéter la Quantité Ajustée pour chaque produit k dans le groupe P
FABLE_Products_WA_3 <- FABLE_Products_WA_2 %>%
  group_by(Exporter_ISO_Code, Importer_ISO_Code, Year, FABLE_Item) %>%
  mutate() %>%
  mutate(
    Trade_Quantity_Adjusted = first(Trade_Quantity_Adjusted)
  ) %>%
  ungroup()
  
  


###################################################################
# Étape 1 : Calculer les Parts de Marché
FABLE_Products_WA_11 <- FABLE_Products_WA_3 %>%
  group_by(Exporter_ISO_Code, Importer_ISO_Code, Year, FABLE_Item) %>%
  mutate(
    Trade_Value_tot = sum(Trade_Value, na.rm = TRUE),
    Market_Share_x = Trade_Value / Trade_Value_tot
  ) %>%
  ungroup()

# Étape 2 : Ajuster les Quantités en utilisant les Parts de Marché
FABLE_Products_WA_21 <- FABLE_Products_WA_11 %>%
  group_by(Exporter_ISO_Code, Importer_ISO_Code, Year, FABLE_Item) %>%
  mutate(
    Trade_Value_Adjusted = sum(Market_Share_x * Trade_Value, na.rm = TRUE)
  ) %>%
  ungroup()

# Étape 3 : Répéter la Quantité Ajustée pour chaque produit k dans le groupe P
FABLE_Products_WA_31 <- FABLE_Products_WA_21 %>%
  group_by(Exporter_ISO_Code, Importer_ISO_Code, Year, FABLE_Item) %>%
  mutate() %>%
  mutate(
    Trade_Value_Adjusted = first(Trade_Value_Adjusted)
  ) %>%
  ungroup()

  colnames(FABLE_Products_WA_31)





FABLE_Products_WA_VF1 <- FABLE_Products_WA_31 %>%
  select(-Total_Quantity, -Market_Share, -Trade_Quantity_Adjusted_FR, -HS6_Product_Code, -Trade_Quantity_Raw, -Trade_Value, -Trade_Value_tot, -Market_Share_x)

FABLE_Products_WA_VF1$Trade_Quantity_Adjusted <- format(round(as.numeric(FABLE_Products_WA_VF1$Trade_Quantity_Adjusted), 4), scientific = FALSE)
FABLE_Products_WA_VF1$Trade_Value_Adjusted <- format(round(as.numeric(FABLE_Products_WA_VF1$Trade_Value_Adjusted), 4), scientific = FALSE)
FABLE_Products_WA_VF2 <- FABLE_Products_WA_VF1 %>%
  distinct(Trade_Quantity_Adjusted, .keep_all = TRUE)





FABLE_Products_WA_VF3 <- FABLE_Products_WA_VF2 %>%
  filter(FABLE_Exporter != FABLE_Importer)








