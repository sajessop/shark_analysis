
  ## Constants used in data_cleaning_PA
  ### Variables are defined like.this
# Underwater  
  # Start with column names that we want
  ## Interaction df
interaction.names <-  c(
  "OpCode",
  "Frame",
  "Time (mins)",
  "Period time (mins)",
  "Period",
  "TapeReader",
  "Depth",
  "Comment",
  "Method",
  "Position",
  "Family",
  "Genus" ,
  "Species" ,
  "Code" ,
  "Number",
  "Interaction",
  "Escape",
  "Alt.species"
)
## MaxN df
maxn.names <-  c(
  "OpCode",
  "Frame",
  "Time (mins)",
  "Period time (mins)",
  "Period",
  "TapeReader",
  "Depth",
  "Family",
  "Genus" ,
  "Species" ,
  "Code",
  "MaxN",
  "Escape",
  "Method",
  "Alt.species"
)
## Observation df
observation.names <-  c(
  "OpCode",
  "Frame",
  "Time (mins)",
  "Period time (mins)",
  "Period",
  "TapeReader",
  "Depth",
  "Number",
  "observation",
  "code",
  "Method",
  "Alt.species"
)


# Here you can add comments (from escape column) that we don't want to include in a df
## Two different sets. Use in various places
## drop1
drop1 <- c(
  "Camera Onboard",
  "example of swell conditions for fis",
  "looking at camera",
  "no fish",
  "No Fish Seen",
  "reef structure 20cm squre rippedup",
  "Retrevial begins",
  "Retrevial Started",
  "retrival start",
  "ripped up macro algae",
  "rock dislodged",
  "rocks dislodged",
  'end-no haul',
  "end no haul",
  "end before haul",
  'CAMERA STOPS',
  "end",
  "ended before haul",
  "finish before haul",
  "no haul",
  " no haul",
  "?",
  "snoode broke on haul 170"
)
## drop2
drop2 <-  c(
  "Camera Onboard",
  "example of swell conditions for fis",
  "looking at camera",
  "reef structure 20cm squre rippedup",
  "Retrevial begins","Retrevial Started",
  "retrival start",
  "ripped up macro algae",
  "rock dislodged",
  "rocks dislodged",
  "?",
  "snoode broke on haul 170",
  "snoode broke on haul 170mins later",
  "line cut",
  "attacked camera",
  "attracted to float",
  "attacks float",
  "snoode broke on haul",
  "attrached to flaot",
  "Attacks camera",
  "snoode broke during retreival"
)

# Deck 1
## Column names that we want
deck.1.fish.names <-
  c(
    "curtin opcode",
    "Region",
    "DIPRD code",
    "Position",
    "Frame",
    "Period",
    "Period time (mins)",
    "TapeReader",
    "Depth (M)",
    "Method",
    "Family",
    "Genus",
    "Species",
    "Code",
    "number",
    "condition",
    "retained",
    "meshed"
  )
deck.1.habitat.names <-
  c(
    "Curtin opcode",
    "Region",
    "DIPRD code",
    "Position",
    "Frame",
    "Period",
    "Period time (mins)",
    "TapeReader",
    "Depth (M)",
    "Method",
    "Family",
    "Genus",
    "Species",
    "Code",
    "Percentage cover"
  )
deck.1.observations.names <- 
  c(
    "RegionDIPRDcodePosition",
    "Region",
    "DIPRD code",
    "Position",
    "Frame",
    "Period",
    "Period time (mins)",
    "TapeReader",
    "Depth (M)",
    "Method",
    "Family",
    "Genus",
    "Species",
    "Code",
    "number",
    "condition",
    "retained",
    "meshed"
  )
# Deck 2
deck.2.fish.names <-
  c(
    "Curtin opcode",
    "Region",
    "DPIRD code",
    "Position",
    "Frame",
    "Period",
    "Period time (mins)",
    "TapeReader",
    "Depth",
    "Method",
    "Family",
    "Genus",
    "Species",
    "Code",
    "hook distance to float/weight",
    "dropout",
    "gaffed",
    "Filename",
    "hookloc.and.comments",
    "Alt.species",
    "depredated"
  )
deck.2.observations.names <-
  c(
    "Curtin opcode",
    "Region",
    "DPIRD code",
    "Position",
    "Frame",
    "Period",
    "Period time (mins)",
    "TapeReader",
    "Depth",
    "Method",
    "Family",
    "Genus",
    "Species",
    "Code",
    "Number",
    "Stage",
    "Activity",
    "comment"
  )
deck.2.observations <- 
  c(
    "depredated",
    "gilled",
    "Bagged",
    "cameratimedout",
    "2",
    "caughtfeedingonSargentBaker",
    "bagged",
    "taggedandreleased",
    "Gilled",
    "4mingapbetweenvidsdidn'tseefishondec",
    "2W&F",
    "birdpacificgull",
    "seahare"
  )
