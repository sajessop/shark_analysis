
# Functions used in data_cleaning_PA.Rmd

### fucntion names are defined LikeThis
# Assign Interactions Function
AssignInteractions <- function(numerical.interaction){
  
  case_when(numerical.interaction==1 ~'Swim Past',
            numerical.interaction==2 ~'Swim Through',
            numerical.interaction==3 ~'Attracted',
            numerical.interaction==4 ~'Bounce Off',
            numerical.interaction==5 ~'Avoid',
            numerical.interaction==6 ~'Caught-Gilled/hooked',
            numerical.interaction==7 ~'Caught-bagged',
            numerical.interaction==8 ~'Escape',
            numerical.interaction==9 ~'Feeding',
            numerical.interaction==10 ~'Predated on',
            numerical.interaction==11 ~'Bait feeding',
            numerical.interaction==12 ~'Caught while predating',
            TRUE~"Missing data")
}


# Rename Column Function (underwater)
RenameColumn <- function(df) {
  new.names = case_when(
            str_detect(names(df),"(?i)escape|esape") ~"Escape",
            str_detect(names(df),"(?i)max") ~"MaxN",
            str_detect(names(df),"(?i)interact") ~ "Interaction",
            str_detect(names(df),"(?i)method") ~ "Method",
            names(df)=="Time..mins."~"Time (mins)",
            names(df)=="Period.time..mins."~"Period time (mins)",
            TRUE~as.character(names(df))
  )
  old.names <- names(df)
  names(df)[names(df)==old.names] <- new.names
  
  return(df)
}


# Categorise Comment Function (Underwater)
## Using regex notation to capture variation in comments
CategoriseComment <- function(df) {
  ret <- df %>% mutate(
    Alt.species = case_when(
      str_detect(Escape, "(?i)startfish") ~ "seven legged starfish",
      str_detect(Escape, "(?i)squid") ~ "squid",
      str_detect(Escape, "(?i)cuttle") ~ "cuttlefish",
      str_detect(Escape, "(?i)unidentifiable|unknown|UNKNONW|unsure") ~ "unknown fish",
      str_detect(Escape, "(?i)seal") ~ "sea lion",
      str_detect(Escape, "(?i)bird|sear") ~ "bird",
      str_detect(Escape, "(?i)bait|^school") ~ "baitfish",
      str_detect(Escape, "(?i)commernat|comorant") ~ "commorant",
      str_detect(Escape, "(?i)garnard") ~ "gurnard",
      str_detect(Escape, "Aplysia punctata") ~ "sea hare",
      str_detect(Escape, "(?i)haul|dark|stops") ~ "no haul",
      str_detect(Escape, "(?i)no fish") ~ "no fish",
      str_detect(Escape, "^\\d|\\<|reef") ~ "",
      is.na(Escape) ~ "",
      Escape %in% drop2 ~ '',
      TRUE ~ as.character(Escape)))
  return(ret)
}

# Apply species
ApplySpecies <- function(common.species, alternative.species){
  Species = case_when(
    common.species == "" ~ as.character(alternative.species),
    is.na(common.species) ~ as.character(alternative.species),
    TRUE ~ as.character(common.species)
  )
  return(Species)
}



# Rename Deck Column
## Using regex notation to capture variation in column names
DeckTwoColumns <- function(df) {
  new.names =
    case_when(
    str_detect(names(df), "(?i)^prox|^dist|^mesh|^near|^hook")~"hooklocation",
    str_detect(names(df), "(?i)^drop")~"dropout",
    str_detect(names(df), "(?i)^gaff")~"gaffed",
    names(df)=="OpCode"~"Curtin opcode",
    str_detect(names(df), "Period.time.")~"Period time (mins)",
    TRUE~as.character(names(df)))
  old.names <- names(df)
  names(df)[names(df)==old.names] <- new.names
  
  return(df)
}


## Remove Whitespace
## input dataframe, col in which you want to rm whitespace, new col name
RemoveWhitespace <- function(df, varnam, outname) {
  passvar <- rlang::enquo(varnam)
  df <- df %>%
    dplyr::mutate(fnoutput = gsub("\\s+", "",!!passvar)) %>%
    rename({{outname}} := fnoutput)
  return(df)
}

# Categorise Hook Location
## Using regex notation to capture variation in comments
HookLocation <- function(df) {
  ret <- df %>% mutate(
    `hook distance to float/weight` = case_when(
      hookloc.and.comments %in% deck.2.observations~"",
      hookloc.and.comments==""~"",
      is.na(hookloc.and.comments)~"",
      str_detect(hookloc.and.comments, "(?i)^1w") ~ "1w",
      str_detect(hookloc.and.comments, "(?i)^2w") ~ "2w",
      str_detect(hookloc.and.comments, "(?i)^3w") ~ "3w",
      str_detect(hookloc.and.comments, "(?i)^4w") ~ "4w",
      str_detect(hookloc.and.comments, "(?i)^1f") ~ "1f",
      str_detect(hookloc.and.comments, "(?i)^2f") ~ "2f",
      str_detect(hookloc.and.comments, "(?i)^3f") ~ "3f",
      str_detect(hookloc.and.comments, "(?i)^4f") ~ "4f",
      TRUE ~ "ERROR"
    )
  )
  return(ret)
}

# Categorise Gaffed
CategoriseGaffed <- function(df) {
  df %>% mutate(binary.gaffed = ifelse(str_detect(gaffed, "(?i)^y"), TRUE, FALSE),
                gaffed = binary.gaffed)
}
# Categorise Dropout
CategoriseDropout <- function(df) {
  ret <- df %>% mutate(Alt.species = case_when(
      str_detect(dropout, "(?i)bird|shear") ~ "bird",
      str_detect(dropout, "(?i)cuttle") ~ "cuttlefish",
      str_detect(dropout, "(?i)cray") ~ "crayfish",
      str_detect(dropout, "(?i)unknown") ~ "unknown fish",
      str_detect(dropout, "(?i)cuttle") ~ "cuttlefish",
      str_detect(dropout, "sea hare") ~ "sea hare",
      str_detect(hookloc.and.comments, "seahare") ~ "sea hare",
      str_detect(dropout, "^sealion") ~ "sea lion",
      str_detect(dropout, "^blue manner") ~ "crab",
      TRUE ~ ""),
    depredated=case_when(dropout == "depredated" ~ TRUE,
                         hookloc.and.comments =="depredated" ~ TRUE,
                         TRUE ~ FALSE),
    binary.dropout = ifelse(str_detect(dropout, "(?i)^y"), TRUE, FALSE),
    dropout = binary.dropout
  )
  return(ret)
}

# Rename Deck 1 Column
## Using regex notation to capture variation in column names
DeckOneColumns <- function(df) {
  new.names =
    case_when(
      str_detect(names(df), "(?i)^condition|^alive")~"condition",
      str_detect(names(df), "(?i)^reta")~"retained",
      str_detect(names(df), "(?i)^bag|^mesh|cover")~"meshed",
      names(df)=="OpCode"~"curtin opcode",
      names(df)=="Depth"~"Depth (M)",
      names(df)=="Number"~"number",
      str_detect(names(df), "Period.time.")~"Period time (mins)",
      TRUE~as.character(names(df)))
  old.names <- names(df)
  names(df)[names(df)==old.names] <- new.names
  
  return(df)
}

# Define percent cover
## Make a new col named percent cover that includes all numerical values in the meshed col

# Define condition
## ^a=alive, ^y=alive
## ^d=dead, ^n=dead
CategoriseCondition <- function(df){
  ret <- df %>% mutate(categorised.condition = case_when(
    str_detect(condition, "(?i)^a|^y") ~ "alive",
    str_detect(condition, "(?i)^d|n") ~ "dead",
    TRUE ~ as.character(condition)),
    condition = categorised.condition
  )
  return(ret)
}

#Define retained
## ^y=yes, ^n=no
CategoriseRetained <- function(df){
  ret <- df %>% mutate(categorise.retained = case_when(
    str_detect(retained, "(?i)^y") ~ "yes",
    str_detect(retained, "(?i)^n") ~ "no",
    TRUE ~ as.character(NA)),
    retained = categorise.retained
  )
  return(ret)
}

# Categorise meshed
# Two layers of functions as there is lots of stuff in meshed col
# Create ref col to store original meshed data 
OrigMesh <- function(df){
  ret <- df %>% mutate(
    original.meshed = meshed
  )
  return(ret)
}

# create just bagged/gilled in "meshed"
ActualMeshed <- function(df){
  ret <- df %>% mutate(
    meshed = case_when(
      str_detect(meshed, "(?i)illed") ~ "gilled",
      str_detect(meshed, "(?i)bagg") ~ "bagged",
      meshed == "G" ~ "gilled",
      meshed == "B" ~ "bagged")
  )
  return(ret)
}

# create alt.species
AltSpecies <- function(df, varnam){
  ret <- df %>% mutate(
    Alt.species = case_when(
      str_detect({{varnam}}, "(?i)unknown") ~ "unknown fish",
      str_detect({{varnam}}, "(?i)shell|mollusc") ~ "shell",
      str_detect({{varnam}}, "(?i)crab") ~ "crab",
      str_detect({{varnam}}, "(?i)bird") ~ "bird",
      {{varnam}} == "crayfish" ~ "crayfish",
      TRUE ~ as.character(NA))  
  )
  return(ret)
}


# create depredated
Depredate <- function(df){
  ret <- df %>% mutate(
    depredated = ifelse(str_detect(original.meshed, "(?i)depredate"), TRUE, as.character(NA))
  )
}

# Create habitat
Habitat <- function(df){
  ret <- df %>% mutate(
    `Percentage cover` = ifelse(str_detect(original.meshed, "\\d"), as.character(original.meshed), as.character(NA))
  )
}

# 2 layer function
CategoriseMeshed <- function(df){
  ret <- df %>% 
    OrigMesh() %>% 
    ActualMeshed() %>% 
    AltSpecies(original.meshed) %>% 
    Depredate() %>% 
    Habitat() %>% 
   filter(!original.meshed %in% c("jack gets stingray barb", "craypot"))
   return(ret)
}

# Categorise Region
CategoriseRegion <- function(df){
  ret <- df %>% mutate(
    Region = case_when(
      str_detect(Region, "(?i)lance") ~ "Lancelin",
      str_detect(Region, "(?i)peac") ~ "Peacefulbay",
      TRUE ~ as.character(Region)
    )
  )
  return(ret)
}
# define comments



# Subsurface
# Rename SS Column
## Using regex notation to capture variation in column names
SSColumns <- function(df) {
  new.names =
    case_when(
      str_detect(names(df), "(?i)gaffed")~"Gaffed",
      str_detect(names(df), "(?i)condition")~"Dropout condition",
      str_detect(names(df), "(?i)drop")~"Drop out",
      names(df)=="OpCode"~"DPIRD code",
      str_detect(names(df), "Period.time.")~"Period time (mins)",
      TRUE~as.character(names(df)))
  old.names <- names(df)
  names(df)[names(df)==old.names] <- new.names
  
  return(df)
}

# Another two layer function
OrigGaff <- function(df){
  ret <- df %>% mutate(
    original.gaffed = Gaffed
  )
  return(ret)
}

ActualGaffed <- function(df){
  ret <- df %>% mutate(
    Gaffed = case_when(
      str_detect(Gaffed, "(?i)^y") ~ "yes",
      str_detect(Gaffed, "(?i)^n") ~ "no",
      TRUE ~ as.character(NA)
    )
  )
  return(ret)
}
# Categorise interaction
# from gaffed column
CategoriseInteraction <- function(df){
  ret <- df %>% 
    mutate(
      Interaction = str_extract(Gaffed, "^[0-9]+$")
    )
  return(ret)
}

# 2 layer function
# CategoriseGaffedSS <- function(df){
#   ret <- df %>% 
#     OrigGaff() %>% 
#     ActualGaffed %>% 
#     CategoriseInteraction() %>% 
#     
# }


