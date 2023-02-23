library(tidyverse)
library(rlang)
library(MASS)
library(janitor)
library(lazyeval)

options(stringsAsFactors = FALSE,dplyr.summarise.inform = FALSE) 

source("data_cleaning_functions.R")
source("data_cleaning_constants.R")

# Ref sheet for CAAB code matching
setwd('//fish.wa.gov.au/Data/Production Databases/Shark/ParksAustralia_2019')
ref <- read.delim("CodeMatchingPA19.txt", sep = "\t") %>% 
  unite(taxa, GENUS, SPECIES, sep = " ", remove = FALSE, na.rm = TRUE) %>% 
  mutate(
    refCode = as.integer(CAAB.CODE),
    taxa = str_trim(taxa, side = "left")) %>% 
  dplyr::select(taxa, refCode)
Event.Mes.data.dump <- 'Abbey_Sarah'


###########################-------------Underwater----------------###################################
if(Event.Mes.data.dump=='Abbey_Sarah')
{
  #1. read in  data
  #1.1. gillnet
  setwd(
    'C:/Users/S.Jesso/OneDrive - Department of Primary Industries And Regional Development/Final_EMobs/EMOutputs/Gillnet'
  )
  #setwd('//fish.wa.gov.au/Data/Production Databases/Shark/ParksAustralia_2019/EMOutputs/Gillnet')
  filenames <- list.files(pattern = '*.csv')
  dummy.GN <- lapply(filenames, read.csv, skip = 4)
  #1.2. longline
  #setwd('//fish.wa.gov.au/Data/Production Databases/Shark/ParksAustralia_2019/EMOutputs/Longline')
  setwd(
    'C:/Users/S.Jesso/OneDrive - Department of Primary Industries And Regional Development/Final_EMobs/EMOutputs/Longline'
  )
  filenames <- list.files(pattern = '*.csv')
  dummy.LL <- lapply(filenames, read.csv, skip = 4)
  
  #2. put data in standard format
  #2.1. gillnet
  Video.net.interaction = Video.net.maxN = Video.net.obs = vector('list', length(dummy.GN))
  
  for (i in 1:length(dummy.GN))
  {
    dummy.GN[[i]] <- RenameColumn(dummy.GN[[i]])
    if (!'Position' %in% names(dummy.GN[[i]]))
      dummy.GN[[i]]$Position = NA
    Video.net.interaction[[i]] <- dummy.GN[[i]] %>%
      OrigUW() %>% 
      filter(is.na(MaxN)) %>%
      CategoriseComment() %>%
      CategoriseEscape() %>% 
      dplyr::select(all_of(interaction.names)) %>%
      mutate(
        Number = ifelse(Number == 'AD', NA, Number),
        No.haul = Alt.species == "no haul",
        No.fish = Alt.species == "no fish",
        Species = ApplySpecies(Species, Alt.species),
        Method = "Gillnet",
        Interaction = AssignInteractions(Interaction)
      ) %>% 
      filter(Alt.species == "")
    
    Video.net.maxN[[i]] <- dummy.GN[[i]] %>%
      RenameColumn() %>%
      OrigUW() %>% 
      CategoriseComment() %>%
      dplyr::select(all_of(maxn.names)) %>%
      mutate(
        Species=ApplySpecies(Species, Alt.species),
        Method = "Gillnet",
      )
    
    Video.net.obs[[i]] = dummy.GN[[i]] %>%
      RenameColumn() %>%
      OrigUW() %>% 
      CategoriseComment() %>%
      mutate(
        observation = Alt.species,
        No.haul = Alt.species == "no haul",
        No.fish = Alt.species == "no fish",
        code = Code,
        Method = "Gillnet",
        Species=ApplySpecies(Species, Alt.species)
      ) %>%
      dplyr::select(all_of(observation.names))
  }
  
  Video.net.interaction <- do.call(rbind, Video.net.interaction) %>%
    MatchCAABFUN() %>% 
    filter(!Species %in% c("", " ") &
             No.haul == FALSE & No.fish == FALSE)
  Video.net.maxN = do.call(rbind, Video.net.maxN) %>%
    MatchCAABFUN() %>% 
    filter(!is.na(MaxN))
  Video.net.obs = do.call(rbind, Video.net.obs) %>%
    MatchCAABFUN() %>% 
    filter(!observation == '') %>% 
    mutate(code = Code)
  
  #2.2. longline
  Video.longline.interaction = Video.longline.maxN = Video.longline.obs =
    vector('list', length(dummy.LL))
  for (i in 1:length(dummy.LL))
  {
    dummy.LL[[i]] <- RenameColumn(dummy.LL[[i]])
    if (!'Position' %in% names(dummy.LL[[i]]))
      dummy.LL[[i]]$Position = NA
    Video.longline.interaction[[i]] <-  dummy.LL[[i]] %>%
      OrigUW() %>% 
      filter(is.na(MaxN)) %>%
      CategoriseComment() %>%
      CategoriseEscape() %>% 
      dplyr::select(all_of(interaction.names)) %>%
      mutate(
        Number = ifelse(Number == 'AD', NA, Number),
        No.haul = Alt.species == "no haul",
        No.fish = Alt.species == "no fish",
        Species=ApplySpecies(Species, Alt.species),
        Method = "Longline",
        Interaction = AssignInteractions(Interaction)
      ) %>% 
      filter(Alt.species == "")
    
    Video.longline.maxN[[i]] = dummy.LL[[i]] %>%
      RenameColumn() %>%
      OrigUW() %>% 
      CategoriseComment() %>%
      dplyr::select(all_of(maxn.names)) %>%
      mutate(
        Method = "Longline",
        Species=ApplySpecies(Species, Alt.species)
      )
    
    Video.longline.obs[[i]] = dummy.LL[[i]] %>%
      RenameColumn() %>%
      OrigUW() %>% 
      CategoriseComment() %>%
      mutate(
        observation = Alt.species,
        No.haul = Alt.species == "no haul",
        No.fish = Alt.species == "no fish",
        code = Code,
        Method = "Longline",
        Species=ApplySpecies(Species, Alt.species)
      ) %>%
      dplyr::select(all_of(observation.names))
  }
  Video.longline.interaction = do.call(rbind, Video.longline.interaction) %>%
    MatchCAABFUN() %>%
    filter(!Species %in% c(" ", "") &
             No.haul == FALSE & No.fish == FALSE)
  Video.longline.maxN = do.call(rbind, Video.longline.maxN) %>%
    MatchCAABFUN() %>%
    filter(!is.na(MaxN)) %>%
    rename(Max.N = MaxN)
  Video.longline.obs = do.call(rbind, Video.longline.obs) %>%
    MatchCAABFUN() %>%
    filter(!observation == '') %>%
    rename(Observation = observation,
           optcode = OpCode) %>% 
    mutate(code = Code)
  
  
}

###########################-------------Deck2----------------###################################
#setwd('//fish.wa.gov.au/Data/Production Databases/Shark/ParksAustralia_2019/EMOutputs/Deck2')
setwd(
  'C:/Users/S.Jesso/OneDrive - Department of Primary Industries And Regional Development/Final_EMobs/Deck/OutputDeck2/23-01-23'
)
# Read in data
dummy.d2 <- list()  
deck2filenames <- dir(pattern="*.csv")

for (i in 1:length(deck2filenames))
{
  dummy.d2[[i]] <- read.csv(deck2filenames[i], skip=4)
}
# Clean with User Defined functions
Deck.2.fish <-  Deck.2.obs <-  vector('list', length(dummy.d2))
for (i in 1:length(dummy.d2))
{
  Deck.2.fish[[i]] <- dummy.d2[[i]] %>% 
    DeckTwoColumns() %>% 
    RemoveWhitespace(hooklocation, hookloc.and.comments) %>% 
    rename(`hook distance to float/weight`=hooklocation) %>% 
    HookLocation() %>% 
    CategoriseGaffed() %>%
    CategoriseDropout() %>% 
    separate("Curtin opcode", c("Region", "DPIRD code", "Position"), sep="_", remove=FALSE) %>% 
    CategoriseRegion() %>% 
    mutate(Position = "Deck#2",
           Species=ApplySpecies(Species, Alt.species)) %>% 
    dplyr::select(all_of(deck.2.fish.names)) 
}

Video.camera2.deck <- do.call(rbind, Deck.2.fish)

###########################-------------Deck1----------------###################################
#setwd('//fish.wa.gov.au/Data/Production Databases/Shark/ParksAustralia_2019/EMOutputs/Deck1')
setwd(
  'C:/Users/S.Jesso/OneDrive - Department of Primary Industries And Regional Development/Final_EMobs/EMoutputs/Deck1'
)
# Read in data
dummy.d1 <- list()  
deck1filenames <- dir(pattern="*.csv")
for (i in 1:length(deck1filenames))
{
  dummy.d1[[i]] <- read.csv(deck1filenames[i], skip=4)
}
Deck.1.fish <-  Deck.1.habitat <-  vector('list', length(dummy.d1))
for (i in 1:length(dummy.d1))
{
  Deck.1.fish[[i]] <- dummy.d1[[i]] %>%
    DeckOneColumns() %>%
    separate("curtin opcode", c("Region", "DIPRD code", "Position"), sep="_", remove=FALSE) %>%
    OrigD1() %>% 
    CategoriseRegion() %>% 
    CategoriseCondition(original.condition, condition) %>% 
    CategoriseRetained() %>% 
    CategoriseMeshed() %>% 
    mutate(Position = "Deck#1",
           Species=ApplySpecies(Species, Alt.species)) %>%
    filter(is.na(`Percentage cover`)) %>% 
    dplyr::select(all_of(deck.1.fish.names))
  
  Deck.1.habitat[[i]] <- dummy.d1[[i]] %>%
    DeckOneColumns() %>%
    separate("curtin opcode", c("Region", "DIPRD code", "Position"), sep="_", remove=FALSE) %>%
    CategoriseRegion() %>% 
    mutate(
      `Curtin opcode` = `curtin opcode`
    ) %>% 
    CategoriseMeshed() %>% 
    filter(!is.na(`Percentage cover`)) %>%
    dplyr::select(all_of(deck.1.habitat.names))
}
Deck.1.fish <- do.call(rbind, Deck.1.fish)
Video.camera1.deck=Deck.1.fish

Video.habitat.deck <- do.call(rbind, Deck.1.habitat)


###########################-------------Subsurface----------------###################################
#setwd('//fish.wa.gov.au/Data/Production Databases/Shark/ParksAustralia_2019/EMOutputs/Subsurface')
setwd(
  'C:/Users/S.Jesso/OneDrive - Department of Primary Industries And Regional Development/Final_EMobs/EMoutputs/Subsurface'
)
# Read in data
dummy.ss <- list()  
ssfilenames <- dir(pattern="*.csv")
for (i in 1:length(ssfilenames))
{
  dummy.ss[[i]] <- read.csv(ssfilenames[i], skip=4)
}
SS.fish <- vector('list', length(dummy.ss))
for (i in 1:length(dummy.ss)){
  SS.fish[[i]] <- dummy.ss[[i]] %>%
    SSColumns() %>% 
    mutate(Region = str_extract(`DPIRD code`, "[^_]+")) %>%
    OrigSS() %>% 
    CategoriseSSDropout() %>% 
    CategoriseSSGaffed() %>% 
    CategoriseCondition(original.condition, `Dropout condition`) %>% 
    mutate(Species=ApplySpecies(Species, Alt.species)) %>% 
    dplyr::select(all_of(subsurface.names))  
  print(i)
}
SS.fish <- do.call(rbind, SS.fish)
Video.subsurface=SS.fish

