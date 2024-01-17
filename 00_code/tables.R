# SOURCES ----
source("../00_code/__library.R")
source("../00_code/__functions.R")

# READING DATA ----
table <- read_excel("C:/Users/eliad/Desktop/Edu/WU Wien/Courses/3 Semester/Int econ/International-econ-paper/01_input_data/tables_wdi.xlsx")
glimpse(table)

# CLEANING ----
table_clean <- table %>%
  slice(1:1518) %>% 
  select(-2, -4) %>% 
  rename_all(~ gsub("\\[.*\\]", "", .)) %>%
  pivot_longer(cols = -c("Country Name", "Time"), names_to = "Indicator", values_to = "Value") %>%
  mutate(Value = as.numeric(Value)) %>%
  pivot_wider(names_from = Time, values_from = Value)

#SUBSETS ----
## Table 4 ----
# Core-Periphery in Europe at a Country Level (GDP Per Capita, EU-28 = 100)
eu_countries <- c( "Portugal", "Spain", "France", "Italy", "Malta", "Cyprus", "Greece",
  "Austria", "Croatia", "Slovenia", "Slovak Republic", "Hungary",
  "Bulgaria", "Romania", "Poland", "Germany", "Netherlands", "Belgium",
  "Luxembourg", "Denmark", "Sweden", "Finland", "Estonia", "Latvia",
  "Lithuania", "Czechia", "Ireland")

table_4a <- table_clean %>%
  rename(Country = "Country Name") %>% 
  filter(Country %in% eu_countries) %>%
  filter(Indicator == "GDP per capita (current US$) ") %>% 
  select(1, 29, 35) %>% 
  mutate(`GDP Per Capita, EU-27=100 in 2016` = (`2016` / mean(`2016`, na.rm = TRUE)) * 100) %>% 
  mutate(`GDP Per Capita, EU-27=100 in 2022` = (`2022` / mean(`2022`, na.rm = TRUE)) * 100) %>% 
  select(-2,-3) %>% 
  mutate_at(vars(2:3), ~ round(., 2))

table_4b <- table_clean %>%
  rename(Country = "Country Name") %>% 
  filter(Country %in% eu_countries) %>%
  filter(Indicator == "GDP growth (annual %) ") %>% 
  select(1, 29:35) %>% 
  mutate(`Average annual GDP growth rate (%) - 2016/19` = (`2016`+`2017`+`2018`+`2019`) / 4) %>% 
  select(-c(2:8)) %>% 
  mutate_at(vars(2), ~ round(., 2))

table_4 <- merge(table_4a, table_4b, by = "Country", all = TRUE)
#richer <- table_4 %>%
#  filter(`GDP Per Capita, EU-27=100 in 2022` - `GDP Per Capita, EU-27=100 in 2016` > 0) %>%
#  select(Country)

## Table klusters ----
k00 <- c(2, 2, 1, 1, 1, 1, 2, 1, 2, 2, 2, 1, 1, 2, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 2)
k11 <- c(1, 1, 3, 3, 3, 3, 1, 3, 1, 1, 1, 2, 3, 2, 2, 3, 3, 1, 3, 1, 3, 2, 3, 3, 3, 2, 1)
k21 <- c(1, 1, 3, 3, 3, 3, 1, 3, 1, 1, 1, 2, 3, 3, 2, 3, 3, 1, 3, 1, 3, 3, 3, 3, 3, 2, 1)

table_2022a <- table_clean %>%
  filter(Indicator %in% c("Foreign direct investment, net inflows (% of GDP) ",
                          "GDP (current US$) ")) %>%
  rename(Country = "Country Name") %>%
  filter(Country %in% eu_countries) %>%
  select(1,2,35) %>%
  pivot_wider(names_from = 2, values_from = 3) %>%
  arrange(Country) %>% 
  mutate(cluster = k21) %>% 
  group_by(cluster) %>%
  summarize(Cluster_FDI_IN = sum(`Foreign direct investment, net inflows (% of GDP) ` * `GDP (current US$) `, na.rm = TRUE)/sum(`GDP (current US$) `)) %>%
  mutate(Group = case_when(
    cluster == 1 ~ "Core",
    cluster == 3  ~ "Periphery",
    cluster == 2 ~ "Southern Europe",
    TRUE ~ as.character(cluster)
  )) %>% 
  arrange(Group) %>% 
  mutate(year = 2022) %>% 
  select(-"cluster")

table_2011a <- table_clean %>%
  filter(Indicator %in% c("Foreign direct investment, net inflows (% of GDP) ",
                          "GDP (current US$) ")) %>%
  rename(Country = "Country Name") %>%
  filter(Country %in% eu_countries) %>%
  select(1,2,24) %>%
  pivot_wider(names_from = 2, values_from = 3) %>%
  arrange(Country) %>% 
  mutate(cluster = k11) %>% 
  group_by(cluster) %>%
  summarize(Cluster_FDI_IN = sum(`Foreign direct investment, net inflows (% of GDP) ` * `GDP (current US$) `, na.rm = TRUE)/sum(`GDP (current US$) `)) %>%
  mutate(Group = case_when(
    cluster == 1 ~ "Core",
    cluster == 3  ~ "Periphery",
    cluster == 2 ~ "Southern Europe",
    TRUE ~ as.character(cluster)
  )) %>% 
  arrange(Group) %>% 
  mutate(year = 2011)%>% 
  select(-"cluster")


table_2000a <- table_clean %>%
  filter(Indicator %in% c("Foreign direct investment, net inflows (% of GDP) ",
                          "GDP (current US$) ")) %>%
  rename(Country = "Country Name") %>%
  filter(Country %in% eu_countries) %>%
  select(1,2,13) %>%
  pivot_wider(names_from = 2, values_from = 3) %>%
  arrange(Country) %>% 
  mutate(cluster = k00) %>% 
  group_by(cluster) %>%
  summarize(Cluster_FDI_IN = sum(`Foreign direct investment, net inflows (% of GDP) ` * `GDP (current US$) `, na.rm = TRUE)/sum(`GDP (current US$) `)) %>%
  mutate(Group = case_when(
    cluster == 1 ~ "Core",
    cluster == 2  ~ "Periphery",
    TRUE ~ as.character(cluster)
  )) %>% 
  arrange(Group) %>% 
  mutate(year = 2000)%>% 
  select(-"cluster")

table_2022b <- table_clean %>%
  filter(Indicator %in% c("Foreign direct investment, net outflows (% of GDP) ", 
                          "GDP (current US$) ")) %>%
  rename(Country = "Country Name") %>%
  filter(Country %in% eu_countries) %>%
  select(1,2,35) %>%
  pivot_wider(names_from = 2, values_from = 3) %>%
  arrange(Country) %>% 
  mutate(cluster = k21) %>% 
  group_by(cluster) %>%
  summarize(Cluster_FDI_OUT = sum(`Foreign direct investment, net outflows (% of GDP) ` * `GDP (current US$) `, na.rm = TRUE)/sum(`GDP (current US$) `)) %>%
  mutate(Group = case_when(
    cluster == 1 ~ "Core",
    cluster == 3  ~ "Periphery",
    cluster == 2 ~ "Southern Europe",
    TRUE ~ as.character(cluster)
  )) %>% 
  arrange(Group) %>% 
  mutate(year = 2022)%>% 
  select(-"cluster")

table_2011b <- table_clean %>%
  filter(Indicator %in% c("Foreign direct investment, net outflows (% of GDP) ", 
                          "GDP (current US$) ")) %>%
  rename(Country = "Country Name") %>%
  filter(Country %in% eu_countries) %>%
  select(1,2,24) %>%
  pivot_wider(names_from = 2, values_from = 3) %>%
  arrange(Country) %>% 
  mutate(cluster = k11) %>% 
  group_by(cluster) %>%
  summarize(Cluster_FDI_OUT = sum(`Foreign direct investment, net outflows (% of GDP) ` * `GDP (current US$) `, na.rm = TRUE)/sum(`GDP (current US$) `)) %>%
  mutate(Group = case_when(
    cluster == 1 ~ "Core",
    cluster == 3  ~ "Periphery",
    cluster == 2 ~ "Southern Europe",
    TRUE ~ as.character(cluster)
  )) %>% 
  arrange(Group) %>% 
  mutate(year = 2011)%>% 
  select(-"cluster")

table_2000b <- table_clean %>%
  filter(Indicator %in% c("Foreign direct investment, net outflows (% of GDP) ", 
                          "GDP (current US$) ")) %>%
  rename(Country = "Country Name") %>%
  filter(Country %in% eu_countries) %>%
  select(1,2,13) %>%
  pivot_wider(names_from = 2, values_from = 3) %>%
  arrange(Country) %>% 
  mutate(cluster = k00) %>% 
  group_by(cluster) %>%
  summarize(Cluster_FDI_OUT = sum(`Foreign direct investment, net outflows (% of GDP) ` * `GDP (current US$) `, na.rm = TRUE)/sum(`GDP (current US$) `)) %>%
  mutate(Group = case_when(
    cluster == 1 ~ "Core",
    cluster == 2  ~ "Periphery",
    TRUE ~ as.character(cluster) 
  )) %>% 
  arrange(Group) %>% 
  mutate(year = 2000)%>% 
  select(-"cluster")

fdi_in <- bind_rows(table_2000a, table_2011a, table_2022a) %>%
  arrange(year)
fdi_out <- bind_rows(table_2000b, table_2011b, table_2022b) %>%
  arrange(year)

table00 <- table_clean %>%
  filter(Indicator %in% c("Trade (% of GDP) ", 
                          "GDP (current US$) ")) %>%
  rename(Country = "Country Name") %>%
  filter(Country %in% eu_countries) %>%
  select(1,2,13) %>%
  pivot_wider(names_from = 2, values_from = 3) %>%
  arrange(Country) %>% 
  mutate(cluster = k00) %>% 
  group_by(cluster) %>%
  summarize(Cluster_Trade = sum(`Trade (% of GDP) ` * `GDP (current US$) `, na.rm = TRUE)/sum(`GDP (current US$) `)) %>%
  mutate(Group = case_when(
    cluster == 1 ~ "Core",
    cluster == 2  ~ "Periphery",
    TRUE ~ as.character(cluster)
  )) %>% 
  arrange(Group) %>% 
  mutate(year = 2000)%>% 
  select(-"cluster")

table11 <- table_clean %>%
  filter(Indicator %in% c("Trade (% of GDP) ", 
                          "GDP (current US$) ")) %>%
  rename(Country = "Country Name") %>%
  filter(Country %in% eu_countries) %>%
  select(1,2,24) %>%
  pivot_wider(names_from = 2, values_from = 3) %>%
  arrange(Country) %>% 
  mutate(cluster = k11) %>% 
  group_by(cluster) %>%
  summarize(Cluster_Trade = sum(`Trade (% of GDP) ` * `GDP (current US$) `, na.rm = TRUE)/sum(`GDP (current US$) `)) %>%
  mutate(Group = case_when(
    cluster == 1 ~ "Core",
    cluster == 3  ~ "Periphery",
    cluster == 2 ~ "Southern Europe",
    TRUE ~ as.character(cluster)
  )) %>% 
  arrange(Group) %>% 
  mutate(year = 2011)%>% 
  select(-"cluster")

table22 <- table_clean %>%
  filter(Indicator %in% c("Trade (% of GDP) ", 
                          "GDP (current US$) ")) %>%
  rename(Country = "Country Name") %>%
  filter(Country %in% eu_countries) %>%
  select(1,2,35) %>%
  pivot_wider(names_from = 2, values_from = 3) %>%
  arrange(Country) %>% 
  mutate(cluster = k21) %>% 
  group_by(cluster) %>%
  summarize(Cluster_Trade = sum(`Trade (% of GDP) ` * `GDP (current US$) `, na.rm = TRUE)/sum(`GDP (current US$) `)) %>%
  mutate(Group = case_when(
    cluster == 1 ~ "Core",
    cluster == 3  ~ "Periphery",
    cluster == 2 ~ "Southern Europe",
    TRUE ~ as.character(cluster)
  )) %>% 
  arrange(Group) %>% 
  mutate(year = 2022)%>% 
  select(-"cluster")

trade <- bind_rows(table00, table11, table22) %>%
  arrange(year)

# VISUALS ----
## Long format ----
table_4_long <- table_4 %>%
  select(1:3) %>% 
  gather(key = "Indicator", value = "Value", -Country)
order <- table_4$Country[order(-table_4$`GDP Per Capita, EU-27=100 in 2016`)]
table_4_long$Country <- factor(table_4_long$Country, levels = order)

## Plots ----
plot_4 <- ggplot(table_4_long, aes(x = Country, y = Value, fill = Indicator)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 100, linetype = "dashed", color = "black") +
  labs(title = "GDP Per Capita (EU-27=100)",
       x = "Country",
       y = "GDP Per Capita") +
  scale_fill_manual(values = c("GDP Per Capita, EU-27=100 in 2016" = "blue", "GDP Per Capita, EU-27=100 in 2022" = "yellow"),
                    labels = c("2016" = "Year 2016", "2022" = "Year 2022")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

plot_5 <- ggplot(fdi_in, aes(x = Group, y = Cluster_FDI_IN, fill = factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "FDI Inflow as % GDP by cluster",
       x = "Cluster",
       y = "% GDP",
       fill = "Year") +
  scale_fill_manual(values = c("2000" = "lightblue", "2011" = "lightgreen", "2022" = "lightcoral")) +
  theme_minimal()

plot_6 <- ggplot(fdi_out, aes(x = Group, y = Cluster_FDI_OUT, fill = factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "FDI Outflow as % GDP by cluster",
       x = "Cluster",
       y = "% GDP",
       fill = "Year") +
  scale_fill_manual(values = c("2000" = "lightblue", "2011" = "lightgreen", "2022" = "lightcoral")) +
  theme_minimal()

plot_7 <- ggplot(trade, aes(x = Group, y = Cluster_Trade, fill = factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Trade as % GDP by cluster",
       x = "Cluster",
       y = "% GDP",
       fill = "Year") +
  scale_fill_manual(values = c("2000" = "lightblue", "2011" = "lightgreen", "2022" = "lightcoral")) +
  theme_minimal()
