
# UNWRANGLING ----
convert_chr_num <- function(df) {
  df <- df %>%
    mutate(across(starts_with("year"):last_col(), as.numeric))
  return(df)
}

na.share <- function(df) {
  missing_percentage <- colMeans(is.na(df)) * 100
  result <- data.frame(Share_NAs = missing_percentage)
  return(result)
}

# CLUSTERING ----
eu_countries <- c( "Portugal", "Spain", "France", "Italy", "Malta", "Cyprus", "Greece",
                   "Austria", "Croatia", "Slovenia", "Slovak Republic", "Hungary",
                   "Bulgaria", "Romania", "Poland", "Germany", "Netherlands", "Belgium",
                   "Luxembourg", "Denmark", "Sweden", "Finland", "Estonia", "Latvia",
                   "Lithuania", "Czechia", "Ireland", "Slovakia")

# I saved here the cluster assignments of the analysis so that findings can be reproducible
# Otherwise every time one runs the code, the assignment Core = 2, Southern = 1 and Periphery = 3 changes
k00 <- c(2, 2, 1, 1, 1, 1, 2, 1, 2, 2, 2, 1, 1, 2, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 2)
k11 <- c(1, 1, 3, 3, 3, 3, 1, 3, 1, 1, 1, 2, 3, 2, 2, 3, 3, 1, 3, 1, 3, 2, 3, 3, 3, 2, 1)
k21 <- c(1, 1, 3, 3, 3, 3, 1, 3, 1, 1, 1, 2, 3, 3, 2, 3, 3, 1, 3, 1, 3, 3, 3, 3, 3, 2, 1)
