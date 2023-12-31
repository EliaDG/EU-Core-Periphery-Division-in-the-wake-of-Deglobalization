convert_chr_num <- function(df) {
  df <- df %>%
    mutate(across(starts_with("year"):last_col(), as.numeric))
  return(df)
}

na_share <- function(df) {
  missing_percentage <- colMeans(is.na(df)) * 100
  result <- data.frame(MissingPercentage = missing_percentage)
  return(result)
}
