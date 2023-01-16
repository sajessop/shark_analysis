
  ## Constants used in data_cleaning_PA
  ### Variables are defined like.this
  
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
  "Escape"
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
  "Escape"
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
  "code"
)

# All the possible ways column names have been spelled wrong
## Escape
wrong.escape <- c(
  'escape.time',
  'escape',
  'Escape.time',
  "Esape"
)
## MaxN
wrong.maxn <- c(
  'max.n',
  'Max.N',
  'maxn',
  'Maxn',
  'MAXn'
)
## Interaction
wrong.interaction <- c(
  'Interactino',
  'interaction',
  'Interactions'
)
## Method
wrong.method <- c(
  "method",
  "method.",
  "mETHOD"
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
  "Attacks camera"
)


