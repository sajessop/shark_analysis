rename.column <- function(df, match_name, match_rename, distance) {
  all.columns <- names(df)
  
  filtered.columns <-
    all.columns[!(all.columns %in% common.columns)]
  filtered.columns.lower <- lapply(filtered.columns, tolower)
  matched.idx <-
    amatch(match_name, filtered.columns.lower, maxDist = distance)
  
  if (is.na(matched.idx))
    stopifnot(matched.idx > 0)
  
  matched.string <- filtered.columns.lower[[matched.idx]]
  df <- df %>% rename(!!sym(match_rename) := matched.string)
  return (df)
}