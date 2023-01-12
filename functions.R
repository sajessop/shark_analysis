rename.column <- function(df, match_name, match_rename, distance) {
  all.columns <- names(df)

  filtered.columns <-
    all.columns[!(all.columns %in% common.columns)]

  # put to lowercase to get closer matches
  filtered.columns.lower <- lapply(filtered.columns, tolower)

  # find the index of the closest match, quit if no match found as probably an error
  matched.idx <-
    amatch(match_name, filtered.columns.lower, maxDist = distance, nomatch = -1)

  if (matched.idx < 0) {
    print("No Match Found")
    stopifnot(matched.idx > 0)
  }

  matched.string <- filtered.columns[[matched.idx]]

  # if exact match, do nothing
  print(match_name)
  if (stringdist(match_rename, matched.string) > 0) {
    df <- df %>% rename(!!sym(match_rename) := matched.string)
    print(sprintf("Succesfully changed %s to %s", matched.string, match_rename))
  } else {
    print(paste("No column name change for", deparse(substitute(df))))
  }
  return(df)
}





rename.entry <- function(pattern, data, renamed_to, dist) {
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

boss.function <- function(df) {
  res <- list()

  df <-
    rename.column(df, "escape", "Escape", 6)
  df <-
    rename.column(df, "max.n", "MaxN", 6)
  df <-
    rename.column(df, "interaction", "Interaction", 10)
  df <-
    rename.column(df, "method", "Method", 10)

  df$Interaction <- factor(df$Interaction, c(1:12), interaction.factors)
  df$Number <- as.integer(df$Number)
  df$Depth <- as.integer(df$Depth)
  df$Escape <- as.character(df$Escape)


  if ("Time (mins)" %in% colnames(df)) {
    df <- df %>%
      rename(
        "Time (mins)" = "Time..mins.",
        "Period time (mins)" = "Period.time..mins."
      )
  }

  if (!"Position" %in% names(df)
  ) {
    df$Position <- NA
  }

  interaction <- df %>%
    filter(is.na(MaxN)) %>%
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
  
  maxN <- df %>%
    dplyr::select(all_of(video.net.names))
  TRUE ~ as.character(Escape)
  
  # fuzzy matching for when camera stopped
  look <- c("no haul", "before haul", "end", "CAMERA STOPS")
  match.distance <- 0
  
  obs <-
    df %>%
    mutate.escape() %>%
    mutate(
      Camera.stopped = rowSums(afind(Escape, look)$distance <= match.distance),
      Got.dark = grepl("dark", Escape),
      observation = Alt.species,
      observation = ifelse(observation %in% DROP, "", observation)
    ) %>%
    dplyr::select(all_of(c(
      Video.net.obs.names, "Got.dark", "Camera.stopped"
    )))
  
  res <- list(interaction, maxN, obs)

  return(res)
}
