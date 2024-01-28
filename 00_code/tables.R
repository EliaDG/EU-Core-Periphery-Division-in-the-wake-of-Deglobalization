# SOURCES ----
source("../00_code/__library.R")
source("../00_code/__functions.R")

# READING DATA ----
table <- read_excel("../01_input_data/tables_wdi.xlsx")
eu_table <- read_csv("../02_intermediary_data/macro_data.csv")

# CLEANING ----
table_clean <- table %>%
  slice(1:1518) %>% 
  select(-2, -4) %>% 
  rename_all(~ gsub("\\[.*\\]", "", .)) %>%
  pivot_longer(cols = -c("Country Name", "Time"), names_to = "Indicator", values_to = "Value") %>%
  mutate(Value = as.numeric(Value)) %>%
  pivot_wider(names_from = Time, values_from = Value)

## Preparation Tables x Visualization ----
# EU debt in 2011
eu_table_debt <- eu_table %>%
  arrange(year) %>%
  filter(year %in% c(2011)) %>%
  select("country","year",
         "c_gross_debt") %>% 
  rename("share_debt" = 3) %>%
  mutate(cluster = k11) %>%
  mutate(Group = case_when(
    cluster == 1 ~ "Core",
    cluster == 3  ~ "Periphery",
    cluster == 2 ~ "PIIGS",
    TRUE ~ as.character(cluster)
  )) %>% 
  arrange(desc(share_debt))

order <- eu_table_debt$country[order(-eu_table_debt$share_debt)]
eu_table_debt$country <- factor(eu_table_debt$country, levels = order)


# Difference of GDP per capita by cluster
gdp_capita_k <- eu_table %>% 
  select(1:2, 9, 27) %>% 
  filter(year == 2022 & country %in% eu_countries) %>% 
  mutate(cluster = k21) %>%
  group_by(cluster) %>%
  summarize(year = first(year), k_gdp_capita = sum(gdp)*1000000 / sum(pop)) %>% 
  mutate(cluster_label = case_when(
    cluster == 1 ~ "Core",
    cluster == 3  ~ "Periphery",
    cluster == 2 ~ "Southern Europe",
    TRUE ~ as.character(cluster)
  ))

# Core-Periphery in Europe at a Country Level (GDP Per Capita, EU-28 = 100)
table_gdp_average <- table_clean %>%
  rename(Country = "Country Name") %>% 
  filter(Country %in% eu_countries) %>%
  filter(Indicator == "GDP per capita (current US$) ") %>% 
  select(1, 24, 35) %>% 
  mutate(`GDP Per Capita, EU-27=100 in 2011` = (`2011` / mean(`2011`, na.rm = TRUE)) * 100) %>% 
  mutate(`GDP Per Capita, EU-27=100 in 2022` = (`2022` / mean(`2022`, na.rm = TRUE)) * 100) %>% 
  select(-2,-3) %>% 
  mutate_at(vars(2:3), ~ round(., 2))

table_gdp_average_long <- table_gdp_average %>%
  gather(key = "Indicator", value = "Value", -Country)
order <- table_gdp_average$Country[order(-table_gdp_average$`GDP Per Capita, EU-27=100 in 2011`)]
table_gdp_average_long$Country <- factor(table_gdp_average_long$Country, levels = order)

# FDI In and Out
table_FDI2022in <- table_clean %>%
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

table_FDI2011in <- table_clean %>%
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

table_FDI2000in <- table_clean %>%
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

table_FDI2022out <- table_clean %>%
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

table_FDI2011out <- table_clean %>%
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

table_FDI2000out <- table_clean %>%
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

fdi_in <- bind_rows(table_FDI2000in, table_FDI2011in, table_FDI2022in) %>%
  arrange(year)
fdi_out <- bind_rows(table_FDI2000out, table_FDI2011out, table_FDI2022out) %>%
  arrange(year)

#Table on Trade
table_trade00 <- table_clean %>%
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

table_trade11 <- table_clean %>%
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

table_trade22 <- table_clean %>%
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

trade <- bind_rows(table_trade00, table_trade11, table_trade22) %>%
  arrange(year)

# VISUALS ----
plot_debt <- ggplot(eu_table_debt, aes(x = country, y = share_debt, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = mean(eu_table_clean$share_debt), linetype = "dashed", color = "black") +
  labs(title = "Share of Gross Debt",
       x = "Country",
       y = "Gross Debt") +
  scale_fill_manual(values = c("PIIGS" = "lightblue", "Core" = "lightgreen", "Periphery" = "lightcoral")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

plot_gdp_kluster <- ggplot(gdp_capita_k, aes(x = reorder(cluster_label, k_gdp_capita), y = k_gdp_capita)) +
  geom_bar(stat = "identity", position = "dodge", fill = c("lightgreen", "lightblue", "coral")) +
  labs(title = "GDP per Capita by Cluster in 2022",
       x = "Cluster",
       y = "GDP per Capita") +
  theme_minimal()

plot_eu_gdp <- ggplot(table_gdp_average_long, aes(x = Country, y = Value, fill = Indicator)) +
  geom_bar(stat = "identity", position = "dodge", color = "black",) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "black") +
  labs(title = "GDP Per Capita (EU-27=100)",
       x = "Country",
       y = "GDP Per Capita") +
  scale_fill_manual(values = c("GDP Per Capita, EU-27=100 in 2011" = "blue", "GDP Per Capita, EU-27=100 in 2022" = "yellow"),
                    labels = c("2011" = "Year 2011", "2022" = "Year 2022")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

plot_fdi_in <- ggplot(fdi_in, aes(x = Group, y = Cluster_FDI_IN, fill = factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "FDI Inflow as % GDP by cluster",
       x = "Cluster",
       y = "% GDP",
       fill = "Year") +
  scale_fill_manual(values = c("2000" = "lightblue", "2011" = "lightgreen", "2022" = "lightcoral")) +
  theme_minimal()

plot_fdi_out <- ggplot(fdi_out, aes(x = Group, y = Cluster_FDI_OUT, fill = factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "FDI Outflow as % GDP by cluster",
       x = "Cluster",
       y = "% GDP",
       fill = "Year") +
  scale_fill_manual(values = c("2000" = "lightblue", "2011" = "lightgreen", "2022" = "lightcoral")) +
  theme_minimal()

plot_trade <- ggplot(trade, aes(x = Group, y = Cluster_Trade, fill = factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Trade as % GDP by cluster",
       x = "Cluster",
       y = "% GDP",
       fill = "Year") +
  scale_fill_manual(values = c("2000" = "lightblue", "2011" = "lightgreen", "2022" = "lightcoral")) +
  theme_minimal()
