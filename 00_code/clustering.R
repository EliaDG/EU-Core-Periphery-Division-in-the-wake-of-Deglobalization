
# HEADER ----

#CONTIGENCIES ----
source("../00_code/__library.R")
source("../00_code/__functions.R")

# CLUSTER ANALYSIS ----
## Method: ----
  # Consider just most important features
#load(url("https://github.com/EliaDG/International-econ-paper/tree/main/02_intermediary_data/macro_data.cvs"))
eu_data <- read_csv("../02_intermediary_data/macro_data.csv")

eu_clean <- eu_data %>%
  arrange(year) %>%
  filter(year %in% c(2000, 2008, 2016, 2021)) %>%
  select("country","year",
         "gdp_gg",
         "fdi_share",
         "nip",
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
         "urb_pop",
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


### Individual datasets ----
glimpse(eu_final)

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

distance <- get_dist(eu_2000); fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) + ggtitle("Distance Matrix Year 2000")
fviz_nbclust(eu_2000, kmeans, method = "wss")
fviz_nbclust(eu_2000, kmeans, method = "silhouette")
gap_stat <- clusGap(eu_2000, FUN = kmeans, nstart = 25, K.max = 10, B = 50) ;fviz_gap_stat(gap_stat)
opt_K <- NbClust(eu_2000, method = "kmeans", max.nc = 10)

distance <- get_dist(eu_2008); fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) + ggtitle("Distance Matrix Year 2008")
fviz_nbclust(eu_2008, kmeans, method = "wss")
fviz_nbclust(eu_2008, kmeans, method = "silhouette")
gap_stat <- clusGap(eu_2008, FUN = kmeans, nstart = 25, K.max = 10, B = 50) ;fviz_gap_stat(gap_stat)
opt_K <- NbClust(eu_2008, method = "kmeans", max.nc = 10)

distance <- get_dist(eu_2016); fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) + ggtitle("Distance Matrix Year 2016")
fviz_nbclust(eu_2016, kmeans, method = "wss")
fviz_nbclust(eu_2016, kmeans, method = "silhouette")
gap_stat <- clusGap(eu_2016, FUN = kmeans, nstart = 25, K.max = 10, B = 50) ;fviz_gap_stat(gap_stat)
opt_K <- NbClust(eu_2016, method = "kmeans", max.nc = 10)

distance <- get_dist(eu_2021); fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) + ggtitle("Distance Matrix Year 2021")
fviz_nbclust(eu_2021, kmeans, method = "wss")
fviz_nbclust(eu_2021, kmeans, method = "silhouette")
gap_stat <- clusGap(eu_2016, FUN = kmeans, nstart = 25, K.max = 10, B = 50) ;fviz_gap_stat(gap_stat)
opt_K <- NbClust(eu_2021, method = "kmeans", max.nc = 10)

### Cluster visualization ----
df1 = eu_2016
df2 = eu_2016

hc <- hclust(dist(df1), method = "complete")
clusters <- cutree(hc, k = 3)
par(mar = c(6, 6, 3, 3))
plot(hc, main = "Hierarchical Clustering Dendrogram 20__", xlab = "Observations", sub = NULL)

kmeans <- kmeans(df1, centers = 3, nstart = 50); kclusters <- kmeans$cluster
df2$kcluster <- kclusters
fviz_cluster(kmeans, data = df2) + ggtitle("K Mean clustering 20__")

centers <- data.frame(df2)
ggRadar(centers, aes(color = kcluster), rescale = FALSE) + 
  ggtitle("K Means Centers 20__") +
  theme_bw()

### Descriptive Stats ----
df3 <- data.frame(df1)
df3$cluster <- as.factor(clusters)
HR_stats <- aggregate(. ~ cluster, df3, mean)

means_by_hc <- summaryBy(. ~ clusters, data = df2, FUN = "mean")
KK_stats <- summaryBy(. ~ kcluster, data = df2, FUN = "mean")
