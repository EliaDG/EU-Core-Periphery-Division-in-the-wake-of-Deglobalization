
# HEADER ----

# CONTIGENCIES ----
source("../00_code/__library.R")
source("../00_code/__functions.R")

# READING DATA ----
eu_data <- read_csv("../02_intermediary_data/macro_data.csv")

# CLUSTER ANALYSIS ----
## Cleaning: ----
  # Consider just most important features
eu_clean <- eu_data %>%
  arrange(year) %>%
  filter(year %in% c(2000, 2011, 2021)) %>%
  select("country","year",
         "gdp_gg",
         "nip",
         "fdi_share",
         "share_trade",
         "qual",
         "g_effect",
         "voic_acc",
         "pol_stab",
         "rul_law",
         "corpt",
         "c_gross_debt",
         "share_gdp_gerd",
         "share_gdp_pens",
         "img",
         "wage_pps",
         "unemp",
         "rur_pop",
         "frty") %>% 
  rename("gdp_growth" = 3,
         "fdi" = 5,
         "trade" = 6,
         "regulatory_quality" = 7,
         "gov_effectiveness" = 8,
         "accountability" = 9,
         "political_stability" = 10,
         "rule_law" = 11,
         "control_corruption" = 12,
         "gross_debt" = 13,
         "share_gerd" = 14,
         "share_pension" = 15,
         "fertility"= 20)

### Correlation ----
cor_matrix <- cor(eu_clean[,-c(1,2)], use = "complete.obs")
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.7, tl.col = "black", diag = FALSE)

high_corr_columns <- findCorrelation(cor_matrix, cutoff = 0.62) #max threshold of correlation accepted is 0.6
eu_filtered <- eu_clean[, -c(1,2, high_corr_columns)]

### Missing values ----
round((colSums(is.na(eu_filtered)) / nrow(eu_filtered)) * 100,2)

eu_imp <- mice(eu_filtered, method = "pmm", m = 5, maxit = 50)
eu_imputed <- complete(eu_imp)

### Scaling ----
eu_scaled <- scale(eu_imputed)
eu_final <- cbind(eu_clean[,c(1,2)], eu_scaled)
glimpse(eu_final)

### Subsets ----
country_names <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus",
                   "Czechia", "Denmark", "Estonia", "Finland", "France",
                   "Germany", "Greece", "Hungary", "Ireland", "Italy",
                   "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands",
                   "Poland", "Portugal", "Romania", "Slovakia", "Slovenia",
                   "Spain", "Sweden")
eu_2000 <- eu_final %>% 
  filter(year %in% 2000) %>% 
  select(-1, -2) %>%
  `rownames<-`(., country_names)
eu_2011 <- eu_final %>% 
  filter(year %in% 2011) %>% 
  select(-1, -2) %>%
  `rownames<-`(., country_names)
eu_2021 <- eu_final %>% 
  filter(year %in% 2021) %>% 
  select(-1, -2) %>%
  `rownames<-`(., country_names)

## KCluster 2000 ----
DM00 <- get_dist(eu_2000); fviz_dist(DM00, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) + ggtitle("Distance Matrix Year 2000")
opt_K <- NbClust(eu_2000, method = "kmeans", max.nc = 10)

df1a = eu_2000
df2a = eu_2000

kmean00 <- kmeans(df1a, centers = 2, nstart = 50); kclusters00 <- kmean00$cluster
df2a$kcluster <- kclusters00
fviz_cluster(kmean00, data = df2a, labelsize = 18,palette = c("red", "green")) + ggtitle("K Mean clustering 2000")
#kruskal.test(unemp ~ kcluster, data = df2a)

df2a$Region <- ifelse(df2a$kcluster == 2, "Core", "Periphery")
ggRadar(df2a[,-12], aes(color = Region), rescale = FALSE) + 
  ggtitle("K Means Centers 2000") +
  theme_bw()+
  theme(legend.position = "right",
        axis.text = element_text(size = 14)) +
  scale_color_manual(values = c("Core" = "green", "Periphery" = "red")) +
  scale_fill_manual(values = c("Core" = "green", "Periphery" = "red")) +
  scale_y_continuous(breaks = seq(-1,2,by=0.5))

## KCluster 2011 ----
DM11 <- distance <- get_dist(eu_2011); fviz_dist(DM16, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) + ggtitle("Distance Matrix Year 2016")
optK <- NbClust(eu_2011, method = "kmeans", max.nc = 10)

df1b = eu_2011
df2b = eu_2011

kmean11 <- kmeans(df1b, centers = 3, nstart = 50); kclusters11 <- kmean11$cluster
df2b$kcluster <- kclusters11
fviz_cluster(kmean11, data = df2b, labelsize = 18, palette = c("green", "blue", "red")) + ggtitle("K Mean clustering 2011")

df2b$Region <- ifelse(df2b$kcluster == 2, "PIIGS", 
                     ifelse(df2b$kcluster == 1, "Core", 
                            ifelse(df2b$kcluster == 3, "Periphery", NA)))
ggRadar(df2b[,-12], aes(color = Region), rescale = FALSE) + 
  ggtitle("K Means Centers 2011") +
  theme_bw() +
  theme(legend.position = "right",
        axis.text = element_text(size = 14)) +
  scale_color_manual(values = c("Core" = "green", "PIIGS" = "blue", "Periphery" = "red")) +
  scale_fill_manual(values = c("Core" = "green", "PIIGS" = "blue", "Periphery" = "red")) +
  scale_y_continuous(breaks = seq(-1,2,by=0.5))

## KCluster 2021 ----
DM21 <- distance <- get_dist(eu_2021); fviz_dist(DM21, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) + ggtitle("Distance Matrix Year 2021")
opt_K <- NbClust(eu_2021, method = "kmeans", max.nc = 10)

df1c = eu_2021
df2c = eu_2021

kmean21 <- kmeans(df1c, centers = 3, nstart = 50); kclusters21 <- kmean21$cluster
df2c$kcluster <- kclusters21
fviz_cluster(kmean21, data = df2c, labelsize = 18, palette = c("green","blue", "red")) + ggtitle("K Mean clustering 2021")

df2c$Region <- ifelse(df2c$kcluster == 2, "Southern Europe", 
                     ifelse(df2c$kcluster == 3, "Periphery", 
                            ifelse(df2c$kcluster == 1, "Core", NA)))
ggRadar(df2c[,-12], aes(color = Region), rescale = FALSE) + 
  ggtitle("K Means Centers 2021") +
  theme_bw() +
  theme(legend.position = "right",
        axis.text = element_text(size = 14)) +
  scale_color_manual(values = c("Core" = "green", "Southern Europe" = "blue", "Periphery" = "red")) +
  scale_fill_manual(values = c("Core" = "green", "Southern Europe" = "blue", "Periphery" = "red")) +
  scale_y_continuous(breaks = seq(-1,2,by=0.5))
colnames(eu_clean)

## Descriptive stats----
KK_stats00 <- summaryBy(. ~ kcluster, data = df2a, FUN = "mean")
KK_stats11 <- summaryBy(. ~ kcluster, data = df2b, FUN = "mean")
KK_stats21 <- summaryBy(. ~ kcluster, data = df2c, FUN = "mean")

# GDP per Capita by Cluster ----
#as the cluster numbering assignment is always different I saved it for reproducibility
#k00 <- c(2, 2, 1, 1, 1, 1, 2, 1, 2, 2, 2, 1, 1, 2, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 2)
#k11 <- c(1, 1, 3, 3, 3, 3, 1, 3, 1, 1, 1, 2, 3, 2, 2, 3, 3, 1, 3, 1, 3, 2, 3, 3, 3, 2, 1)
k21 <- c(1, 1, 3, 3, 3, 3, 1, 3, 1, 1, 1, 2, 3, 3, 2, 3, 3, 1, 3, 1, 3, 3, 3, 3, 3, 2, 1)

'ext_00 <- eu_data %>% 
  select(1:2, 9, 27) %>% 
  filter(year == 2000 & country %in% country_names) %>% 
  mutate(cluster = k00) %>% 
  group_by(cluster) %>%
  summarize(year = first(year), k_gdp_capita = sum(gdp)*1000000 / sum(pop)) %>% 
  mutate(cluster_label = case_when(
    cluster == 1 ~ "Core",
    cluster == 2 ~ "Periphery",
    TRUE ~ as.character(cluster)
  ))

ext_11 <- eu_data %>% 
  select(1:2, 9, 27) %>% 
  filter(year == 2011 & country %in% country_names) %>% 
  mutate(cluster = k11) %>% 
  group_by(cluster) %>%
  summarize(year = first(year), k_gdp_capita = sum(gdp)*1000000 / sum(pop)) %>% 
  mutate(cluster_label = case_when(
    cluster == 1 ~ "Core",
    cluster == 3 ~ "Periphery",
    cluster == 2 ~ "Southern Europe",
    TRUE ~ as.character(cluster)
  ))'

ext_21 <- eu_data %>% 
  select(1:2, 9, 27) %>% 
  filter(year == 2021 & country %in% country_names) %>% 
  mutate(cluster = k21) %>%
  group_by(cluster) %>%
  summarize(year = first(year), k_gdp_capita = sum(gdp)*1000000 / sum(pop)) %>% 
  mutate(cluster_label = case_when(
    cluster == 1 ~ "Core",
    cluster == 3  ~ "Periphery",
    cluster == 2 ~ "Southern Europe",
    TRUE ~ as.character(cluster)
  ))

'k_cluster_gdpc <- bind_rows(ext_00, ext_11, ext_21) %>% 
  select(-cluster) %>%
  arrange(year)'

ggplot(ext_21, aes(x = reorder(cluster_label, k_gdp_capita), y = k_gdp_capita)) +
  geom_bar(stat = "identity", position = "dodge", fill = "blue") +
  labs(title = "GDP per Capita by Cluster in 2022",
       x = "Cluster",
       y = "GDP per Capita") +
  theme_minimal()
