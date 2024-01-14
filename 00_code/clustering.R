
# HEADER ----

# CONTIGENCIES ----
source("../00_code/__library.R")
source("../00_code/__functions.R")
set.seed(123)

# READING DATA ----
#load(url("https://github.com/EliaDG/International-econ-paper/tree/main/02_intermediary_data/macro_data.cvs"))
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
#DM00 <- get_dist(eu_2000); fviz_dist(DM00, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) + ggtitle("Distance Matrix Year 2000")
fviz_nbclust(eu_2000, kmeans, method = "wss")
fviz_nbclust(eu_2000, kmeans, method = "silhouette")
gap_stat <- clusGap(eu_2000, FUN = kmeans, nstart = 25, K.max = 10, B = 50) ;fviz_gap_stat(gap_stat)
opt_K <- NbClust(eu_2000, method = "kmeans", max.nc = 10)

df1 = eu_2000
df2 = eu_2000

kmean <- kmeans(df1, centers = 2, nstart = 50); kclusters <- kmean$cluster
df2$kcluster <- kclusters
fviz_cluster(kmean, data = df2) + ggtitle("K Mean clustering 2000")

df2$Region <- ifelse(df2$kcluster == 1, "Core", "Periphery")
ggRadar(df2[,-12], aes(color = Region), rescale = FALSE) + 
  ggtitle("K Means Centers 2000") +
  theme_bw()+
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("Core" = "green", "Periphery" = "red")) +
  scale_fill_manual(values = c("Core" = "green", "Periphery" = "red")) +
  scale_y_continuous(breaks = seq(-1,2,by=0.5))

## KCluster 2011 ----
#DM16 <- distance <- get_dist(eu_2011); fviz_dist(DM16, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) + ggtitle("Distance Matrix Year 2016")
fviz_nbclust(eu_2011, kmeans, method = "wss")
fviz_nbclust(eu_2011, kmeans, method = "silhouette")
gap_stat <- clusGap(eu_2011, FUN = kmeans, nstart = 25, K.max = 10, B = 50) ;fviz_gap_stat(gap_stat) #Everything else either 3/4
optK <- NbClust(eu_2011, method = "kmeans", max.nc = 10)

df1 = eu_2011
df2 = eu_2011

kmean <- kmeans(df1, centers = 3, nstart = 50); kclusters <- kmean$cluster
df2$kcluster <- kclusters
fviz_cluster(kmean, data = df2) + ggtitle("K Mean clustering 2011")

df2$Region <- ifelse(df2$kcluster == 1, "PIIGS", 
                     ifelse(df2$kcluster == 2, "Core", 
                            ifelse(df2$kcluster == 3, "Periphery", NA)))
ggRadar(df2[,-12], aes(color = Region), rescale = FALSE) + 
  ggtitle("K Means Centers 2011") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("Core" = "green", "PIIGS" = "blue", "Periphery" = "red")) +
  scale_fill_manual(values = c("Core" = "green", "PIIGS" = "blue", "Periphery" = "red")) +
  scale_y_continuous(breaks = seq(-1,2,by=0.5))

## KCluster 2021 ----
#DM21 <- distance <- get_dist(eu_2021); fviz_dist(DM21, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) + ggtitle("Distance Matrix Year 2021")
fviz_nbclust(eu_2021, kmeans, method = "wss")
fviz_nbclust(eu_2021, kmeans, method = "silhouette")
gap_stat <- clusGap(eu_2021, FUN = kmeans, nstart = 25, K.max = 10, B = 50) ;fviz_gap_stat(gap_stat) # Everything else says 3
opt_K <- NbClust(eu_2021, method = "kmeans", max.nc = 10)

df1 = eu_2021
df2 = eu_2021

kmean <- kmeans(df1, centers = 3, nstart = 50); kclusters <- kmean$cluster
df2$kcluster <- kclusters
fviz_cluster(kmean, data = df2) + ggtitle("K Mean clustering 2021")

df2$Region <- ifelse(df2$kcluster == 1, "Southern Europe", 
                     ifelse(df2$kcluster == 2, "Periphery", 
                            ifelse(df2$kcluster == 3, "Core", NA)))
ggRadar(df2[,-12], aes(color = Region), rescale = FALSE) + 
  ggtitle("K Means Centers 2021") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("Core" = "green", "Southern Europe" = "blue", "Periphery" = "red")) +
  scale_fill_manual(values = c("Core" = "green", "Southern Europe" = "blue", "Periphery" = "red")) +
  scale_y_continuous(breaks = seq(-1,2,by=0.5))

# APPENDIX ----
## Descriptive stats----
KK_stats <- summaryBy(. ~ kcluster, data = df2, FUN = "mean")
