
# HEADER ----

# CONTIGENCIES ----
source("../00_code/__library.R")
source("../00_code/__functions.R")

# READING DATA ----
#load(url("https://github.com/EliaDG/International-econ-paper/tree/main/02_intermediary_data/macro_data.cvs"))
eu_data <- read_csv("../02_intermediary_data/macro_data.csv")

# CLUSTER ANALYSIS ----
## Cleaning: ----
  # Consider just most important features
eu_clean <- eu_data %>%
  arrange(year) %>%
  filter(year %in% c(2000, 2008, 2016, 2021)) %>%
  select("country","year",
         "gdp_gg",
         "fdi_share",
         "nip",
         "exms_gs",
         "c_gross_debt",
         "share_gdp_gerd",
         "share_gdp_pens",
         "army_exp",
         "avg_hicp",
         "tax",
         "corpt",
         "h_exp",
         "img",
         "wage_pps",
         "unemp",
         "rur_pop",
         "frty")

### Correlation ----
cor_matrix <- cor(eu_clean[,-c(1,2)], use = "complete.obs")
corrplot(cor_matrix, method = "color", tl.cex = 0.7, tl.col = "black", diag = FALSE)

high_corr_columns <- findCorrelation(cor_matrix, cutoff = 0.60) #max threshold of correlation accepted is 0.6
eu_filtered <- eu_clean[, -c(1,2, high_corr_columns)]

### Missing values ----
eu_imp <- mice(eu_filtered, method = "pmm", m = 5, maxit = 50)
eu_imputed <- complete(eu_imp)

  ### Alternative to imputation: KKN
  #eu_matrix <- as.matrix(eu_filtered)
  #eu_imputed_matrix <- kNN(eu_matrix, k = 5)
  #eu_imp <- as.data.frame(eu_imputed_matrix)
  #imp_cols <- grep("_imp$", names(eu_imp), value = TRUE)
  #eu_imputed <- eu_imp[, !names(eu_imp) %in% imp_cols]

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
eu_2008 <- eu_final %>% 
  filter(year %in% 2008) %>% 
  select(-1, -2) %>%
  `rownames<-`(., country_names)
eu_2016 <- eu_final %>% 
  filter(year %in% 2016) %>% 
  select(-1, -2) %>%
  `rownames<-`(., country_names)
eu_2021 <- eu_final %>% 
  filter(year %in% 2021) %>% 
  select(-1, -2) %>%
  `rownames<-`(., country_names)

## KCluster 2000 ----
#DM00 <- get_dist(eu_2000); fviz_dist(DM00, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) + ggtitle("Distance Matrix Year 2000")
fviz_nbclust(eu_2000, kmeans, method = "wss") #Not clear, everything else suggests 2 clusters is enough
fviz_nbclust(eu_2000, kmeans, method = "silhouette")
gap_stat <- clusGap(eu_2000, FUN = kmeans, nstart = 25, K.max = 10, B = 50) ;fviz_gap_stat(gap_stat)
opt_K <- NbClust(eu_2000, method = "kmeans", max.nc = 10)

df1 = eu_2000
df2 = eu_2000

kmean <- kmeans(df1, centers = 2, nstart = 50); kclusters <- kmean$cluster
df2$kcluster <- kclusters
fviz_cluster(kmean, data = df2) + ggtitle("K Mean clustering 2000")

ggRadar(df2, aes(color = kcluster), rescale = FALSE) + 
  ggtitle("K Means Centers 2000") +
  theme_bw()

## KCluster 2008 ----
#DM08 <- distance <- get_dist(eu_2008); fviz_dist(DM08, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) + ggtitle("Distance Matrix Year 2008")
fviz_nbclust(eu_2008, kmeans, method = "wss")
fviz_nbclust(eu_2008, kmeans, method = "silhouette")
gap_stat <- clusGap(eu_2008, FUN = kmeans, nstart = 25, K.max = 10, B = 50) ;fviz_gap_stat(gap_stat)
opt_K <- NbClust(eu_2008, method = "kmeans", max.nc = 10)

df1 = eu_2008
df2 = eu_2008

kmean <- kmeans(df1, centers = 2, nstart = 50); kclusters <- kmean$cluster
df2$kcluster <- kclusters
fviz_cluster(kmean, data = df2) + ggtitle("K Mean clustering 2008")

ggRadar(df2, aes(color = kcluster), rescale = FALSE) + 
  ggtitle("K Means Centers 2008") +
  theme_bw()

## KCluster 2016 ----
#DM16 <- distance <- get_dist(eu_2016); fviz_dist(DM16, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) + ggtitle("Distance Matrix Year 2016")
fviz_nbclust(eu_2016, kmeans, method = "wss")
fviz_nbclust(eu_2016, kmeans, method = "silhouette")
gap_stat <- clusGap(eu_2016, FUN = kmeans, nstart = 25, K.max = 10, B = 50) ;fviz_gap_stat(gap_stat) #Everything else either 3/4
opt_K <- NbClust(eu_2016, method = "kmeans", max.nc = 10)

df1 = eu_2016
df2 = eu_2016

kmean <- kmeans(df1, centers = 3, nstart = 50); kclusters <- kmean$cluster
df2$kcluster <- kclusters
fviz_cluster(kmean, data = df2) + ggtitle("K Mean clustering 2016")

ggRadar(df2, aes(color = kcluster), rescale = FALSE) + 
  ggtitle("K Means Centers 2016") +
  theme_bw()

## KCluster 2021 ----
#DM21 <- distance <- get_dist(eu_2021); fviz_dist(DM21, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) + ggtitle("Distance Matrix Year 2021")
fviz_nbclust(eu_2021, kmeans, method = "wss")
fviz_nbclust(eu_2021, kmeans, method = "silhouette")
gap_stat <- clusGap(eu_2016, FUN = kmeans, nstart = 25, K.max = 10, B = 50) ;fviz_gap_stat(gap_stat) # Everything else says 3
opt_K <- NbClust(eu_2021, method = "kmeans", max.nc = 10)

df1 = eu_2021
df2 = eu_2021

kmean <- kmeans(df1, centers = 3, nstart = 50); kclusters <- kmean$cluster
df2$kcluster <- kclusters
fviz_cluster(kmean, data = df2) + ggtitle("K Mean clustering 2021")

ggRadar(df2, aes(color = kcluster), rescale = FALSE) + 
  ggtitle("K Means Centers 2021") +
  theme_bw()

# APPENDIX ----
## Hierarchical clustering ----
hc <- hclust(dist(df1), method = "complete")
clusters <- cutree(hc, k = 2)
par(mar = c(6, 6, 3, 3))
plot(hc, main = "Hierarchical Clustering Dendrogram 20__", xlab = "Observations", sub = NULL)

## Descriptive stats----
df3 <- data.frame(df1)
df3$cluster <- as.factor(clusters)
HR_stats <- aggregate(. ~ cluster, df3, mean)

means_by_hc <- summaryBy(. ~ clusters, data = df2, FUN = "mean")
KK_stats <- summaryBy(. ~ kcluster, data = df2, FUN = "mean")
