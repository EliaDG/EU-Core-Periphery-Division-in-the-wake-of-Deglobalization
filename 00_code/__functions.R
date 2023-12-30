convert_chr_num <- function(df) {
  df <- df %>%
    mutate(across(starts_with("year"):last_col(), as.numeric))
  return(df)
}
