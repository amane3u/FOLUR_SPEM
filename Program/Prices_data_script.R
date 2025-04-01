# Load data
FAO_prices <- read.csv("C:\\Users\\Adnane\\Desktop\\BACI-MacMap\\Prices_prod_data\\FAOSTAT_data_en_3-27-2025.csv")
rm(list = setdiff(ls(), "FAO_prices_clean"))
# Select key columns
FAO_prices <- select(FAO_prices, Area.Code..ISO3., Item, Year, Value)

# Map FABLE country codes
FAO_prices$FABLE_Exporter <- Country_mapping_250120_updated_2$FABLE_cc[
  match(FAO_prices$Area.Code..ISO3., Country_mapping_250120_updated_2$ISO3_code)
]

# Map item names using case_when
FAO_prices <- FAO_prices %>%
  mutate(FABLE_Item = case_when(
    Item == "Coffee, green" ~ "Coffee",
    Item == "Cocoa beans" ~ "Cocoa",
    Item == "Maize (corn)" ~ "Corn",
    Item == "Wheat" ~ "Wheat",
    Item == "Palm oil" ~ "Palm_oil",
    Item == "Soya beans" ~ "Soyabean",
    Item == "Rice" ~ "Rice",
    TRUE ~ Item  # Default: keep original Item if no match
  ))

# Rename and reorder columns
FAO_prices <- FAO_prices %>%
  rename(Exporter_ISO_Code = Area.Code..ISO3.) %>%
  select(Exporter_ISO_Code, FABLE_Exporter, FABLE_Item, Year, Value)
                                      
FAO_prices_dt <- FAO_prices %>%
  group_by(FABLE_Exporter, FABLE_Item, Year) %>%
  summarise(prod_price = median(Value, na.rm = TRUE)) %>%
  ungroup()                                  
       
FAO_prices_dt <- FAO_prices_dt %>%
  group_by(FABLE_Exporter, FABLE_Item) %>%
  mutate(
    Median_Price = median(prod_price, na.rm = TRUE),                # Median of prices
    MAD = median(abs(prod_price - Median_Price), na.rm = TRUE) * 1.4826,  # MAD calculation
    Lower_Bound = Median_Price - 3 * MAD,                          # Lower outlier threshold
    Upper_Bound = Median_Price + 3 * MAD,                          # Upper outlier threshold
    Is_Outlier = prod_price < Lower_Bound | prod_price > Upper_Bound  # Flag outliers
  ) %>%
  ungroup()

outliers <- FAO_prices_dt %>%
  filter(Is_Outlier == TRUE) %>%
  select(FABLE_Exporter, FABLE_Item, Year, prod_price, Lower_Bound, Upper_Bound)                                       




FAO_prices_clean <- FAO_prices_dt %>%
  group_by(FABLE_Exporter, FABLE_Item) %>%
  mutate(
    Median_Price = median(prod_price, na.rm = TRUE),                # Median of prices in group
    MAD = median(abs(prod_price - Median_Price), na.rm = TRUE) * 1.4826,  # MAD calculation
    Lower_Bound = Median_Price - 2 * MAD,                          # Lower threshold
    Upper_Bound = Median_Price + 2 * MAD,                          # Upper threshold
    Is_Outlier = prod_price < Lower_Bound | prod_price > Upper_Bound,  # Flag outliers
    prod_price_winsorized = ifelse(Is_Outlier & prod_price < Lower_Bound, Lower_Bound,  # Winsorize below
                                   ifelse(Is_Outlier & prod_price > Upper_Bound, Upper_Bound,  # Winsorize above
                                          prod_price))  # Keep original if not outlier
  ) %>%
  ungroup()

# Optional: Check the winsorized values and outliers
winsorized_outliers <- FAO_prices_clean %>%
  filter(Is_Outlier == TRUE) %>%
  select(FABLE_Exporter, FABLE_Item, Year, prod_price, prod_price_winsorized, Lower_Bound, Upper_Bound)