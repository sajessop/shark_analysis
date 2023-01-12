rename.column <- function(df, match_name, match_rename, distance) {
  all.columns <- names(df)
  
  filtered.columns <-
    all.columns[!(all.columns %in% common.columns)]
  
  # put to lowercase to get closer matches
  filtered.columns.lower <- lapply(filtered.columns, tolower)
  
  # find the index of the closest match, quit if no match found as probably an error
  matched.idx <-
    amatch(match_name, filtered.columns.lower, maxDist = distance, nomatch=-1)
  
  if (matched.idx < 0)
  {
    print("No Match Found")
    stopifnot(matched.idx > 0)
  }
  
  matched.string <- filtered.columns[[matched.idx]]

  # if exact match, do nothing
  print(match_name)
  if (stringdist(match_rename, matched.string) > 0)
  {

    df <- df %>% rename(!!sym(match_rename) := matched.string)
    print(sprintf("Succesfully changed %s to %s",matched.string, match_rename))
  }
  else
   print(paste("No column name change for", deparse(substitute(df))))
  return (df)
}