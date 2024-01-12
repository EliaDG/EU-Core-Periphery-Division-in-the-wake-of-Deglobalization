source("../00_code/__library.R")
source("../00_code/__functions.R")

table <- read_excel("C:/Users/eliad/Desktop/Edu/WU Wien/Courses/3 Semester/Int econ/International-econ-paper/01_input_data/tables_wdi.xlsx")
glimpse(table)

table_wider <- table %>%
  slice(1:1518) %>% 
  select(-2, -4) %>% 
  rename_all(~ gsub("\\[.*\\]", "", .)) %>%
  pivot_longer(cols = -c("Country Name", "Time"), names_to = "Indicator", values_to = "Value") %>%
  mutate(Value = as.numeric(Value)) %>%
  pivot_wider(names_from = Time, values_from = Value)

#Table 1 ----
#Growth Rates of GDP in Selected Countries (Annual Percentage Change): 2016–22
table1 <- table_wider %>%
  filter(Indicator %in% "GDP growth (annual %) ") %>% 
  select(c(1, 28:35)) %>%
  rename(Country = "Country Name") %>% 
  filter(Country %in% c("High income", 
                         "European Union",
                         "North America",
                         "OECD members",
                         "United States",
                         "Germany",
                         "France",
                         "Italy",
                         "Japan",
                         "Middle income",
                         "Central Europe and the Baltics",
                         "South America & Caribbean",
                         "Middle East & North Africa",
                         "China",
                         "Low income"
                         ))
#Table 2 ----
#Percentage Distribution of FDI by Country or Group of Countries (%): 1990-2022
table2 <- table_wider %>%
  filter(Indicator %in% "Foreign direct investment, net inflows (% of GDP) ") %>% 
  select(c(1, seq(3, 35, by=8))) %>%
  rename(Country = "Country Name") %>% 
  filter(Country %in% c("High income", 
                        "European Union",
                        "North America",
                        "OECD members",
                        "Middle income",
                        "Central Europe and the Baltics",
                        "South America & Caribbean",
                        "Middle East & North Africa",
                        "Low income",
                        "South Asia",
                        "China",
                        "Russia Federation",
                        "Brazil",
                        "India",
                        "South Africa",
                        "Mexico"))

#Table 3 ----
# Core-Periphery in Europe at a Country Level (GDP Per Capita, EU-28 = 100)
eu_countries <- c( "Portugal", "Spain", "France", "Italy", "Malta", "Cyprus", "Greece",
  "Austria", "Croatia", "Slovenia", "Slovak Republic", "Hungary",
  "Bulgaria", "Romania", "Poland", "Germany", "Netherlands", "Belgium",
  "Luxembourg", "Denmark", "Sweden", "Finland", "Estonia", "Latvia",
  "Lithuania", "Czechia", "Ireland")

table3a <- table_wider %>%
  rename(Country = "Country Name") %>% 
  filter(Country %in% eu_countries) %>%
  filter(Indicator == "GDP per capita (current US$) ") %>% 
  select(1, 29, 35) %>% 
  mutate(`GDP Per Capita, EU-27=100 in 2016` = (`2016` / mean(`2016`, na.rm = TRUE)) * 100) %>% 
  mutate(`GDP Per Capita, EU-27=100 in 2022` = (`2022` / mean(`2022`, na.rm = TRUE)) * 100) %>% 
  select(-2,-3)

table3b <- table_wider %>%
  rename(Country = "Country Name") %>% 
  filter(Country %in% eu_countries) %>%
  filter(Indicator == "GDP growth (annual %) ") %>% 
  select(1, 29:35) %>% 
  mutate(`Average annual GDP growth rate (%) - 2016/19` = (`2016`+`2017`+`2018`+`2019`) / 4) %>% 
  select(-c(2:8))

table3 <- merge(table3a, table3b, by = "Country", all = TRUE)
