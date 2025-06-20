# --------------------------------------------
# Load Required Libraries
# --------------------------------------------
library(dplyr)     # For data manipulation
library(tidyr)     # For tidying data
library(reader)    # For reading files
library(zoo)       # For rolling operations

# --------------------------------------------
# Step 1: Load FAO Prices Data
# --------------------------------------------
FAO_prices <- read.csv("C:\\Users\\Adnane\\Desktop\\BACI-MacMap\\Prices_prod_data\\FAOSTAT_data_en_4-18-2025.csv")

# --------------------------------------------
# Step 2: Select Key Columns
# --------------------------------------------
FAO_prices <- select(FAO_prices, Area.Code..ISO3., Item, Year, Value)

# --------------------------------------------
# Step 3: Map Country Codes to FABLE Exporter
# --------------------------------------------
FAO_prices$FABLE_Exporter <- Country_mapping_250120_updated_2$FABLE_cc[
  match(FAO_prices$Area.Code..ISO3., Country_mapping_250120_updated_2$ISO3_code)
]

# --------------------------------------------
# Step 4: Map Item Names to FABLE Items
# --------------------------------------------
FAO_prices <- FAO_prices %>%
  mutate(FABLE_Item = case_when(
    Item == "Coffee, green" ~ "Coffee",
    Item == "Cocoa beans" ~ "Cocoa",
    Item == "Maize (corn)" ~ "Corn",
    Item == "Wheat" ~ "Wheat",
    Item == "Palm oil" ~ "Palm Oil",
    Item == "Soya beans" ~ "Soyabean",
    Item == "Rice" ~ "Rice",
    TRUE ~ Item
  ))

# --------------------------------------------
# Step 5: Rename and Reorder Columns
# --------------------------------------------
FAO_prices <- FAO_prices %>%
  rename(Exporter_ISO_Code = Area.Code..ISO3.) %>%
  select(Exporter_ISO_Code, FABLE_Exporter, FABLE_Item, Year, Value)

# --------------------------------------------
# Step 6: Clean Data and Detect Outliers
# --------------------------------------------
FAO_prices_clean <- FAO_prices %>%
  rename(prod_price_i = Value) %>%
  group_by(Exporter_ISO_Code, FABLE_Item) %>%
  arrange(Year) %>%
  mutate(
    local_median = sapply(seq_along(prod_price_i), function(i) median(prod_price_i[-i], na.rm = TRUE)),
    rel_deviation = abs((prod_price_i - local_median) / local_median),
    Is_Outlier = rel_deviation > 1
  ) %>%
  ungroup() %>%
  select(Exporter_ISO_Code, FABLE_Item, Year, prod_price_i, Is_Outlier, rel_deviation)

# --------------------------------------------
# Step 7: Correct Outliers Based on Deviation
# --------------------------------------------
FAO_prices_clean_aft <- FAO_prices_clean %>%
  mutate(prod_price_i_aft = case_when(
    !Is_Outlier ~ prod_price_i,
    Is_Outlier ~ prod_price_i * (1 / rel_deviation)
  ))

# Map FABLE Exporter again
FAO_prices_clean_aft$FABLE_Exporter <- Country_mapping_250120_updated_2$FABLE_cc[
  match(FAO_prices_clean_aft$Exporter_ISO_Code, Country_mapping_250120_updated_2$ISO3_code)
]

# --------------------------------------------
# Step 8: Aggregate by Median Price
# --------------------------------------------
FAO_prices_dt <- FAO_prices_clean_aft %>%
  group_by(FABLE_Exporter, FABLE_Item, Year) %>%
  mutate(prod_price_i_aft_agg = median(prod_price_i_aft, na.rm = TRUE)) %>%
  ungroup() %>%
  select(Exporter_ISO_Code, FABLE_Exporter, FABLE_Item, Year, prod_price_i_aft, prod_price_i_aft_agg) %>%
  rename(prodprice_i = prod_price_i_aft,
         prodprice_i_agg = prod_price_i_aft_agg) %>%
  filter(!is.na(FABLE_Exporter))

# --------------------------------------------
# Step 9: Add Regional Level Information
# --------------------------------------------
FAO_prices_dt$Regional_lv <- Country_mapping_250120_updated_2$regional_lv[
  match(FAO_prices_dt$FABLE_Exporter, Country_mapping_250120_updated_2$FABLE_cc)
]

# --------------------------------------------
# Step 10: Redefine Data to Focus on Aggregated Price
# --------------------------------------------
FAO_prices_dt_red <- FAO_prices_dt %>%
  select(FABLE_Exporter, FABLE_Item, Year, prodprice_i_agg) %>%
  distinct()

# --------------------------------------------
# Step 11: Smooth Data (for Specific Years)
# --------------------------------------------
years_of_interest <- c(2000, 2005, 2010, 2015, 2020)

# Expand data to all combinations of Exporter, Item, and Year
all_combinations <- expand.grid(
  FABLE_Exporter = unique(FAO_prices_dt_red$FABLE_Exporter),
  FABLE_Item = unique(FAO_prices_dt_red$FABLE_Item),
  Year = years_of_interest
)

# Merge combinations with the data and set NA prices to 0
FAO_prices_dt_red <- FAO_prices_dt_red %>%
  full_join(all_combinations, by = c("FABLE_Exporter", "FABLE_Item", "Year")) %>%
  mutate(
    prodprice_i_agg = ifelse(is.na(prodprice_i_agg), 0, prodprice_i_agg)
  )

# --------------------------------------------
# Step 12: Apply Conditional Smoothing (Only Positive Values)
# --------------------------------------------
FAO_prices_dt_red <- FAO_prices_dt_red %>%
  group_by(FABLE_Item, FABLE_Exporter) %>%
  arrange(Year) %>%
  mutate(
    raw_roll = zoo::rollmean(prodprice_i_agg, k = 3, fill = NA, align = 'center'),
    prodprice_i_agg_lisse = case_when(
      Year %in% years_of_interest & raw_roll > 0 ~ raw_roll,
      TRUE ~ prodprice_i_agg
    )
  ) %>%
  ungroup()

# --------------------------------------------
# Step 13: Re-add Regional Level Information
# --------------------------------------------
FAO_prices_dt_red$Regional_lv <- Country_mapping_250120_updated_2$regional_lv[
  match(FAO_prices_dt_red$FABLE_Exporter, Country_mapping_250120_updated_2$FABLE_cc)
]

# --------------------------------------------
# Step 14: Filter Data for Years of Interest
# --------------------------------------------
FAO_prices_dt_red <- FAO_prices_dt_red %>%
  filter(Year %in% years_of_interest)

# --------------------------------------------
# Step 15: Calculate Regional Means
# --------------------------------------------
regional_means <- FAO_prices_dt_red %>%
  filter(prodprice_i_agg_lisse != 0) %>%
  group_by(FABLE_Item, Year, Regional_lv) %>%
  summarise(regional_mean = mean(prodprice_i_agg_lisse, na.rm = TRUE), .groups = "drop")

# --------------------------------------------
# Step 16: Calculate Global Means
# --------------------------------------------
world_means <- FAO_prices_dt_red %>%
  filter(prodprice_i_agg_lisse != 0) %>%
  group_by(FABLE_Item, Year) %>%
  summarise(world_mean = mean(prodprice_i_agg_lisse, na.rm = TRUE), .groups = "drop")

# --------------------------------------------
# Step 17: Impute Missing Data Based on Regional or Global Means
# --------------------------------------------
FAO_prices_dt_red <- FAO_prices_dt_red %>%
  left_join(regional_means, by = c("FABLE_Item", "Year", "Regional_lv")) %>%
  left_join(world_means, by = c("FABLE_Item", "Year")) %>%
  mutate(
    prodprice_i_agg_imp = case_when(
      prodprice_i_agg_lisse != 0 ~ prodprice_i_agg_lisse,
      !is.na(regional_mean) ~ regional_mean,
      !is.na(world_mean) ~ world_mean,
      TRUE ~ mean(prodprice_i_agg_lisse, na.rm = TRUE)
    )
  )

# --------------------------------------------
# Step 18: Flag Imputed Data with Rolling Average
# --------------------------------------------
FAO_prices_dt_red <- FAO_prices_dt_red %>%
  mutate(is_imputed_roll = prodprice_i_agg_lisse != prodprice_i_agg)

# --------------------------------------------
# Step 19: Perform Second Outlier Detection (Only for Imputed Data)
# --------------------------------------------
FAO_prices_dt_red <- FAO_prices_dt_red %>%
  group_by(FABLE_Exporter, FABLE_Item) %>%
  arrange(Year) %>%
  mutate(
    local_median_roll = sapply(seq_along(prodprice_i_agg_imp), function(i) {
      if (is_imputed_roll[i]) {
        median(prodprice_i_agg_imp[-i], na.rm = TRUE)
      } else {
        NA
      }
    }),
    rel_deviation_roll = abs((prodprice_i_agg_imp - local_median_roll) / local_median_roll),
    Is_Second_Outlier = ifelse(is_imputed_roll & rel_deviation_roll > 1, TRUE, FALSE),
    prodprice_i_agg_imp_final = case_when(
      Is_Second_Outlier ~ prodprice_i_agg_imp * (1 / rel_deviation_roll),
      TRUE ~ prodprice_i_agg_imp
    )
  ) %>%
  ungroup()

# --------------------------------------------
# Step 20: Save Final Processed Data to CSV
# --------------------------------------------
write.csv(FAO_prices_dt_red, 
          file = "C:\\Users\\Adnane\\Desktop\\BACI-MacMap\\MAC-MAPHS6\\data_output\\250423_FAO_prices_dt_red.csv", 
          row.names = FALSE)

 

  
