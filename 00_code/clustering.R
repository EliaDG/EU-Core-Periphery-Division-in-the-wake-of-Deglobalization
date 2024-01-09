
# HEADER ----

#CONTIGENCIES ----
source("../00_code/__library.R")
source("../00_code/__functions.R")

# CLUSTER ANALYSIS ----
## 1 Method: ----
  # 2000 - 2022 / mice / 0.73 corr

eu_data <- read_csv("../02_intermediary_data/macro_data.csv")

eu_clean <- eu_data %>%
  arrange(year) %>%
  filter(year %in% c(2000, 2008, 2016, 2022))

cor_matrix <- cor(eu_clean[,-c(1,2)], use = "complete.obs")
corrplot(cor_matrix, method = "color", tl.cex = 0.7, tl.col = "black", diag = FALSE)

high_corr_columns <- findCorrelation(cor_matrix, cutoff = 0.73)
eu_filtered <- eu_skinny[, -c(1, high_corr_columns)]

eu_imp <- mice(eu_filtered, method = "pmm", m = 5, maxit = 50)
eu_imputed <- complete(eu_imp)

eu_scaled <- scale(eu_imputed)
eu_final <- cbind(eu_clean[,c(1,2)], eu_scaled)

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
eu_2022 <- eu_final %>% 
  filter(year %in% 2022) %>% 
  select(-1, -2) %>%
  `rownames<-`(., country_names)

distance <- get_dist(eu_2000); fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
fviz_nbclust(eu_2000, kmeans, method = "wss")
fviz_nbclust(eu_2000, kmeans, method = "silhouette")

distance <- get_dist(eu_2008); fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
fviz_nbclust(eu_2008, kmeans, method = "wss")
fviz_nbclust(eu_2008, kmeans, method = "silhouette")
gap_stat <- clusGap(eu_2008, FUN = kmeans, nstart = 25, K.max = 10, B = 50) ;fviz_gap_stat(gap_stat)

distance <- get_dist(eu_2016); fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
fviz_nbclust(eu_2016, kmeans, method = "wss")
fviz_nbclust(eu_2016, kmeans, method = "silhouette")

distance <- get_dist(eu_2022); fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
fviz_nbclust(eu_2022, kmeans, method = "wss")
fviz_nbclust(eu_2022, kmeans, method = "silhouette")

df1 = eu_2008
df2 = eu_2008

hc <- hclust(dist(df1), method = "complete")
clusters <- cutree(hc, k = 5)

#df2$cluster <- clusters
#print(table(df2$cluster))
plot(hc, main = "Hierarchical Clustering Dendrogram", xlab = "Observations", sub = NULL)


kmeans <- kmeans(df1, centers = 3, nstart = 25)
kclusters <- kmeans$cluster

df2$kcluster <- kclusters
fviz_cluster(kmeans, data = df2)


## 2 Method:----
  # 2000 - 2022 / kkm imputation / 0.80 corr
eu_data <- read_csv("../02_intermediary_data/macro_data.csv")

eu_clean <- eu_data %>%
  as.tibble() %>%
  arrange(year) %>%
  filter(year %in% c(2000, 2008, 2016, 2022))

eu_matrix <- as.matrix(eu_clean[, -c(1, 2)])
eu_imputed_matrix <- kNN(eu_matrix, k = 5)
eu_imp <- as.data.frame(eu_imputed_matrix)
imp_cols <- grep("_imp$", names(eu_imp), value = TRUE)
eu_imputed <- eu_imp[, !names(eu_imp) %in% imp_cols]

cor_matrix <- cor(eu_imputed)
corrplot(cor_matrix, method = "color", tl.cex = 0.7, tl.col = "black", diag = FALSE)
high_corr_columns <- findCorrelation(cor_matrix, cutoff = 0.80)
eu_filtered <- eu_imputed[, -high_corr_columns]

eu_scaled <- scale(eu_filtered)
eu_final <- cbind(eu_clean[,c(1,2)], eu_scaled)

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
eu_2022 <- eu_final %>% 
  filter(year %in% 2022) %>% 
  select(-1, -2) %>%
  `rownames<-`(., country_names)

distance <- get_dist(eu_2000); fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
distance <- get_dist(eu_2008); fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
distance <- get_dist(eu_2016); fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
distance <- get_dist(eu_2022); fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

fviz_nbclust(eu_2000, kmeans, method = "wss")
fviz_nbclust(eu_2000, kmeans, method = "silhouette")

fviz_nbclust(eu_2008, kmeans, method = "wss")
fviz_nbclust(eu_2008, kmeans, method = "silhouette")

fviz_nbclust(eu_2016, kmeans, method = "wss")
fviz_nbclust(eu_2016, kmeans, method = "silhouette")

fviz_nbclust(eu_2022, kmeans, method = "wss")
fviz_nbclust(eu_2022, kmeans, method = "silhouette")
gap_stat <- clusGap(eu_2022, FUN = kmeans, nstart = 25, K.max = 10, B = 50); fviz_gap_stat(gap_stat)

df1 = eu_2022
df2 = eu_2022

hc <- hclust(dist(df1), method = "complete")
clusters <- cutree(hc, k = 5)
plot(hc, main = "Hierarchical Clustering Dendrogram", xlab = "Observations", sub = NULL)

kmeans <- kmeans(df1, centers = 5, nstart = 50); kclusters <- kmeans$cluster
df2$kcluster <- kclusters
fviz_cluster(kmeans, data = df2)

# DESCPRITIVE STATISTICS ----
  #group_by(Cluster) %>%
  #summarise_all("mean")