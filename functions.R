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





rename.entry <- function(pattern, data, renamed_to, dist){
  
  rowSums(afind(data, pattern)$distance <= dist)
  
}


mutate.escape <- function(df) {
  ret <- df %>% mutate(
    Alt.species = case_when(
      str_detect(Escape, "(?i)startfish") ~ "seven legged startfish",
      str_detect(Escape, "(?i)squid") ~ "squid",
      str_detect(Escape, "(?i)cuttlefish") ~ "cuttlefish",
      str_detect(Escape, "(?i)unidentifiable|unknown|UNKNONW") ~ "unknown fish",
      str_detect(Escape, "(?i)seal") ~ "sea lion",
      str_detect(Escape, "(?i)bait|^school") ~ "baitfish",
      str_detect(Escape, "(?i)commernat") ~ "commorant",
      str_detect(Escape, "(?i)garnard") ~ "gurnard",
      str_detect(Escape, "Aplysia punctata") ~ "sea hare",
      str_detect(Escape, "(?i)haul|dark|stops") ~ "no haul",
      str_detect(Escape, "(?i)no fish") ~ "no fish",
      str_detect(Escape, "^\\d|\\<|reef") ~ "",
      is.na(Escape) ~ "",
      TRUE ~ as.character(Escape)
    )
  )
}

mutate.observation <- function(df) {
  ret <- df %>% mutate.escape() %>% mutate(observation = Escape,)
}

boss.function <- function(df){
  tempReturn <- list()
  
  df <-
    rename.column(dummy.GN[[i]], "escape", "Escape", 6)
  ummy.GN[[i]] <-
    rename.column(dummy.GN[[i]], "max.n", "MaxN", 6)
  dummy.GN[[i]] <-
    rename.column(dummy.GN[[i]], "interaction", "Interaction", 10)
  dummy.GN[[i]] <-
    rename.column(dummy.GN[[i]], "method", "Method", 10)
  
  dummy.GN[[i]]$Interaction <-
    factor(dummy.GN[[i]]$Interaction, c(1:12), interaction.factors)
  dummy.GN[[i]]$Number <- as.integer(dummy.GN[[i]]$Number)
  dummy.GN[[i]]$Depth <- as.integer(dummy.GN[[i]]$Depth)
  dummy.GN[[i]]$Escape <- as.character(dummy.GN[[i]]$Escape)
  
  
  if ("Time (mins)" %in% colnames(dummy.GN[[i]])) {
    dummy.GN[[i]] <- dummy.GN[[i]] %>%
      rename("Time (mins)" = "Time..mins.",
             "Period time (mins)" = "Period.time..mins.")
  }
  
  if (!'Position' %in% names(dummy.GN[[i]])
  )
    dummy.GN[[i]]$Position <- NA
  
  Video.net.interaction[[i]] <- dummy.GN[[i]]%>%
    filter(is.na(MaxN))
  
  Video.net.interaction[[i]] %>%
    dplyr::select(all_of(interaction.names)) %>%
    mutate.escape() %>%
    mutate(
      Escape = ifelse(Escape %in% drop.for.inter, "", Escape),
      No.haul = Alt.species == "no haul",
      No.fish = Alt.species == "no fish",
      for.com.sp = ifelse(No.haul == TRUE |
                            No.fish == TRUE, NA, as.character(Alt.species)),
      Species.cleaned = paste0(for.com.sp, Species),
    )
  
  append(tempReturn, Video.net.interaction[[i]])
  
  
  Video.net.maxN[[i]] <- dummy.GN[[i]]%>%
    dplyr::select(all_of(video.net.names))
  
  append(tempReturn, Video.net.maxN[[i]])
  
  # fuzzy matching for when camera stopped
  look <- c("no haul","before haul", "end","CAMERA STOPS")
  
  Video.net.obs[[i]] <- dummy.GN[[i]]
  Video.net.obs[[i]]$Camera.stopped <-
    rowSums(afind(Video.net.obs[[i]]$Escape, look)$distance <= 0)
  
  Video.net.obs[[i]]$Got.dark <-
    grepl("dark", Video.net.obs[[i]])
  
  Video.net.obs[[i]] <-
    dummy.GN[[i]] %>%
    mutate.escape() %>%
    mutate(observation = Alt.species,
           observation = ifelse(observation %in% DROP, "", observation)) %>%
    dplyr::select(all_of(c(
      Video.net.obs.names, "Got.dark", "Camera.stopped"
    )))
  
  append(tempReturn,Video.net.obs[[i]])
  
  res<- myFunc()
}