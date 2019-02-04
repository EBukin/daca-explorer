# This is a package for combining data from various sources and developing a 
# comprehansive visualisation tool.

# 
# install.packages("wbstats")
# install.packages("WDI")
library(wbstats)
library(tidyverse)
library(readxl)

# References --------------------------------------------------------------

# Country reference list 
country_list <- 
  wbstats::wb_cachelist$countries %>% 
  tbl_df() %>% 
  select(iso3c, country, regionID, region, incomeID, income) %>% 
  filter(region != "Aggregates") %>% 
  rename(country_code = iso3c,
         region_code = regionID,
         income_code = incomeID)

# Loading RAW data --------------------------------------------------------

# Data source 1 - GSMA data
# https://www.mobileconnectivityindex.com/#year=2017&dataSet=dimension
# In gsma data there are regions. These regions are conditional on the years for each coutry.
# We remove regional aggregations for the momment

gsma <- 
  excel_sheets("raw-data/GSMA_Mobile_Connectivity_Index_Data_10072018.xlsx") %>% 
  map_dfr(.f = function(x) {
    read_excel(
      "raw-data/GSMA_Mobile_Connectivity_Index_Data_10072018.xlsx", 
      sheet = x
      ) %>% 
      rename(country_code = `ISO Code`,
             country = Country,
             gsma_region = Region,
             gsma_cluster = Cluster,
             year = Year) %>% 
      gather(var, val, 6:length(.)) %>% 
      mutate(`Variable group` = str_c("GSMA ", x))
  }) %>% 
  mutate(var_code = str_replace_all(str_replace_all(str_c(str_to_lower(var), ".", str_to_lower(`Variable group`)), "a|o|i|u|y|e|\\(|\\)|-|\\-", ""), " ", ".")) %>% 
  select(-gsma_region, -gsma_cluster) %>% 
  mutate(Source = "GSMA")

# gsma %>%
#   write_rds("clean-data/gsma.rds", compress = "gz")

# gsma %>% 
#   distinct(iso3, country, gsma_region, gsma_cluster) %>% 
#   write_rds("clean-data/gsma_countries.rds", compress = "gz")

# Data source 2 - Digital adoption index World Bank
# http://www.worldbank.org/en/publication/wdr2016/Digital-Adoption-Index
digital_adoption <-
  read_excel("raw-data/DAIforweb.xlsx") %>% 
  gather(var, val, 3:length(.)) %>% 
  left_join(select(country_list, country, country_code), by = 'country') %>% 
  mutate(var_code = str_replace_all(str_replace_all(str_c(str_to_lower(var)), "a|o|i|u|y|e|\\(|\\)", ""), " ", ".")) %>% 
  rename(year = Year) %>% 
  mutate(Source = "Digital Adoption Index")


# digital_adoption %>% 
#   write_rds("clean-data/digital-adoption.rds", compress = "gz")

# Data source 3 - World Development Indicators
# We will load from a file as it does not work with the API
# excel_sheets("raw-data/WB_INDICATORS/Data_Extract_From_World_Development_Indicators.xlsx")
# wdi <- 
#   read_xlsx("raw-data/Data_Extract_From_World_Development_Indicators.xlsx", sheet = "Data") %>% 
#   rename(country = `Country Name`,
#          country_code = `Country Code`,
#          var = `Series Name`,
#          var_code = `Series Code`,
#          year = Time,
#          val = Value) %>% 
#   select(-`Time Code`) %>% 
#   mutate(val = as.numeric(val)) %>%
#   filter(!is.na(val)) %>% 
#   mutate(Source = "WDI")

interest_indicators <- 
  c("NV.AGR.TOTL.ZS","NV.AGR.TOTL.KD.ZG","NV.AGR.TOTL.KD","SL.AGR.EMPL.ZS",
   "SL.AGR.EMPL.FE.ZS","SL.AGR.EMPL.MA.ZS","SP.RUR.TOTL","SP.RUR.TOTL.ZS",
   "SP.RUR.TOTL.ZG","SI.POV.RUGP","NY.GDP.FCST.KD","SP.POP.GROW",
   "SP.POP.TOTL.FE.IN","SP.POP.TOTL.MA.IN","SP.POP.TOTL.FE.ZS","SP.POP.TOTL.MA.ZS",
   "SP.POP.TOTL","SN.ITK.DEFC.ZS","SI.POV.RUHC","SP.URB.TOTL",
   "SP.URB.TOTL.IN.ZS","SP.URB.GROW","SI.POV.DDAY","SI.POV.LMIC",
   "SI.POV.UMIC", "EN.POP.DNST", "EN.POP.EL5M.RU.ZS", "EN.POP.EL5M.UR.ZS",
   "EN.POP.EL5M.ZS", "EN.POP.SLUM.UR.ZS")

wdi2_indicators <- 
  read_xlsx("raw-data/WDI_excel/WDIEXCEL.xlsx", sheet = excel_sheets("raw-data/WDI_excel/WDIEXCEL.xlsx")[[3]]) 
wdi2_indicators %>% View()
# wdi2_indicators$`Series Code` %>% 
#   .[str_detect(.,"EN.POP")]

wdi2 <- 
  read_xlsx("raw-data/WDI_excel/WDIEXCEL.xlsx", sheet = "Data")


wdi <- 
  wdi2 %>% 
  filter(`Indicator Code` %in%interest_indicators ) %>% 
  gather(year, val, matches("\\d{4}")) %>% 
  rename(country = `Country Name`,
         country_code = `Country Code`,
         var = `Indicator Name`,
         var_code = `Indicator Code`) %>% 
  mutate(val = as.numeric(val),
         year = as.numeric(year)) %>%
  filter(!is.na(val)) %>% 
  mutate(Source = "WDI")



# Universal data format ---------------------------------------------------
# 
# library(Hmisc)
# The universal data format is a normalised data format
country_list

data <-
bind_rows(
  wdi,
  gsma,
  digital_adoption
) %>% 
  select(-country) %>% 
  spread(year, val) %>%
  gather(year, val, matches("\\d{4}"))%>% 
  spread(country_code, val) %>%
  gather(country_code, val, 6:length(.)) %>% 
  left_join(country_list, by = c("country_code")) %>% 
  filter(country_code %in% country_list$country_code) %>% 
  arrange(var, country, year)

data_p1 <- 
  data %>%
  filter(!year %in% c(2005:2017)) %>% 
  mutate(flag = if_else(is.na(val), "Missing", "Original"))%>% 
  mutate(year = as.numeric(year))

data_p2 <- 
  data %>%
  filter(year %in% c(2005:2017)) %>% 
  arrange(country_code, var_code, year) %>% 
  group_by(country_code, var_code) %>%
  do({
    mutate(., 
           val2 = zoo::na.locf(val, na.rm = FALSE),
           val3 = zoo::na.locf(val2, na.rm = FALSE, fromLast = TRUE)
    )
  }) %>% 
  ungroup() %>% 
  mutate(flag = ifelse(is.na(val) & !is.na(val3), "Imputed", NA),
         flag = ifelse(!is.na(val) , "Original", flag),
         flag = ifelse(is.na(flag), "Missing", flag)) %>% 
  # mutate(val = val3) %>% 
  # select(-val3, -val2) %>% 
  mutate(year = as.numeric(year))

pd <-
  bind_rows(
    data_p1,
    data_p2%>% 
      mutate(val = val3) %>%
      select(-val3, -val2)
  ) %>%
  mutate(flag = ifelse(is.na(val) & is.na(flag), "Missing", flag)) %>%
  mutate(var_code = str_replace_all(var_code, "-", "")) %>%
  mutate(var_code = str_replace_all(var_code, "1", "one")) %>%
  mutate(var_code = str_replace_all(var_code, "2", "two")) %>%
  mutate(var_code = str_replace_all(var_code, "3", "thre")) %>%
  mutate(var_code = str_replace_all(var_code, "4", "four")) %>%
  mutate(var_code = str_replace_all(var_code, "5", "five")) %>%
  mutate(var_code = str_replace_all(var_code, "6", "six")) %>%
  mutate(var_code = str_replace_all(var_code, "7", "seven")) %>%
  mutate(var_code = str_replace_all(var_code, "8", "eight")) %>%
  mutate(var_code = str_replace_all(var_code, "9", "nine"))


write_rds(pd, "clean-data/analysis_data.rds", compress = "gz")

