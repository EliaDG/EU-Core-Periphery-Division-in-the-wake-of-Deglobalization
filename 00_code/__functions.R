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

drop_na_columns <- function(df, threshold = 0.25) {
  na_threshold <- nrow(df) * threshold
  columns_to_drop <- names(df)[colSums(is.na(df)) > na_threshold]
  data_clean <- df[, !(names(df) %in% columns_to_drop)]
  return(data_clean)
}
