
source("data_cleaning_functions.R")
source("data_cleaning_constants.R")
 #1. read in  data
  #1.1. gillnet
  setwd(
    'C:/Users/S.Jesso/OneDrive - Department of Primary Industries And Regional Development/Final_EMobs/Outputs16-01-23/Gillnet'
  )
  filenames <- list.files(pattern = '*.csv')
  dummy.GN <- lapply(filenames, read.csv, skip = 4)
  #1.2. longline
  setwd(
    'C:/Users/S.Jesso/OneDrive - Department of Primary Industries And Regional Development/Final_EMobs/Outputs16-01-23/Longline'
  )
  filenames <- list.files(pattern = '*.csv')
  dummy.LL <- lapply(filenames, read.csv, skip = 4)
  
  #2. put data in standard format
  #2.1. gillnet
  Video.net.interaction = Video.net.maxN = Video.net.obs = vector('list', length(dummy.GN))
  
  for (i in 1:length(dummy.GN))
  {
    dummy.GN[[i]] <- RenameColumn(dummy.GN[[i]])
    #REVIEW:Fix for some dfs in dummy.GN missing position column
    if (!'Position' %in% names(dummy.GN[[i]]))
      dummy.GN[[i]]$Position = NA
    Video.net.interaction[[i]] <- dummy.GN[[i]] %>%
      filter(is.na(MaxN)) %>%
      dplyr::select(all_of(interaction.names)) %>%
      CategoriseComment() %>%
      mutate(
        Number = ifelse(Number == 'AD', NA, Number),
        No.haul = Alt.species == "no haul",
        No.fish = Alt.species == "no fish",
        Species=ApplySpecies(Species, Alt.species),
        Escape = ifelse(
          grepl("^\\d|^\\<", Escape),
          gsub("([0-9]+).*$", "\\1", Escape),
          ''),
        Interaction = AssignInteractions(Interaction)
      ) 
    
    Video.net.maxN[[i]] <- dummy.GN[[i]] %>%
      RenameColumn() %>%
      dplyr::select(all_of(maxn.names)) %>%
      CategoriseComment() %>%
      mutate(
        Species=ApplySpecies(Species, Alt.species)
      )
    
    Video.net.obs[[i]] = dummy.GN[[i]] %>%
      RenameColumn() %>%
      CategoriseComment() %>%
      mutate(
        observation = Alt.species,
        No.haul = Alt.species == "no haul",
        No.fish = Alt.species == "no fish",
        code = Code
      ) %>%
      dplyr::select(all_of(observation.names))
  }
  
  Video.net.interaction <- do.call(rbind, Video.net.interaction) %>%
    filter(!Species %in% c("", " ") &
             No.haul == FALSE & No.fish == FALSE)
  Video.net.maxN = do.call(rbind, Video.net.maxN) %>%
    filter(!is.na(MaxN))
  Video.net.obs = do.call(rbind, Video.net.obs) %>%
    filter(!observation == '')
  
  #2.2. longline
  Video.longline.interaction = Video.longline.maxN = Video.longline.obs =
    vector('list', length(dummy.LL))
  for (i in 1:length(dummy.LL))
  {
    #Call RenameColumn function to deal with spelling errors and make names same as Jacks import
    dummy.LL[[i]] <- RenameColumn(dummy.LL[[i]])
    
    #REVIEW:Fix for some dfs in dummy.LL missing position column
    if (!'Position' %in% names(dummy.LL[[i]]))
      dummy.LL[[i]]$Position = NA
    
    Video.longline.interaction[[i]] <-  dummy.LL[[i]] %>%
      filter(is.na(MaxN)) %>%
      dplyr::select(all_of(interaction.names)) %>%
      CategoriseComment() %>%
      mutate(
        Number = ifelse(Number == 'AD', NA, Number),
        No.haul = Alt.species == "no haul",
        No.fish = Alt.species == "no fish",
        Species=ApplySpecies(Species, Alt.species),
        Escape = ifelse(
          grepl("\\d", Escape),
          gsub("([0-9]+).*$", "\\1", Escape),
          ''
        ),
        Interaction = AssignInteractions(Interaction)
      )
    
    Video.longline.maxN[[i]] = dummy.LL[[i]] %>%
      RenameColumn() %>%
      dplyr::select(all_of(maxn.names)) %>%
      CategoriseComment() %>%
      mutate(
        Species=ApplySpecies(Species, Alt.species)
      )
    
    Video.longline.obs[[i]] = dummy.LL[[i]] %>%
      RenameColumn() %>%
      CategoriseComment() %>%
      mutate(
        observation = Alt.species,
        No.haul = Alt.species == "no haul",
        No.fish = Alt.species == "no fish",
        code = Code
      ) %>%
      dplyr::select(all_of(observation.names))
  }
  Video.longline.interaction = do.call(rbind, Video.longline.interaction) %>%
    filter(!Species %in% c(" ", "") &
             No.haul == FALSE & No.fish == FALSE)
  Video.longline.maxN = do.call(rbind, Video.longline.maxN) %>%
    filter(!is.na(MaxN)) %>%
    rename(Max.N = MaxN)
  Video.longline.obs = do.call(rbind, Video.longline.obs) %>%
    filter(!observation == '') %>%
    rename(Observation = observation,
           optcode = OpCode)