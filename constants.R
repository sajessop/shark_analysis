interaction.names <-  c(
  "OpCode",
  "Frame",
  "Time..mins.",
  "Period.time..mins.",
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

video.net.names <-  c(
  "OpCode",
  "Frame",
  "Time..mins.",
  "Period.time..mins.",
  "Period",
  "TapeReader",
  "Depth",
  "Family",
  "Genus" ,
  "Species" ,
  "Code",
  "MaxN"
)

Video.net.obs.names  <-  c(
  "OpCode",
  "Frame",
  "Time..mins.",
  "Period.time..mins.",
  "Period",
  "TapeReader",
  "Depth",
  "Number",
  "observation",
  "Code"
)

DROP <- c(
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

drop.for.inter <-  c(
  "Camera Onboard",
  "example of swell conditions for fis",
  "looking at camera",
  "reef structure 20cm squre rippedup",
  "Retrevial begins",
  "Retrevial Started",
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

common.columns <- c("temp")

interaction.factors <- as.factor(
  c(
    "Swim Past",
    "Swim Through",
    "Attracted",
    "Bounce Off",
    "Avoid",
    "Caught-Gilled/hooked",
    "Caught-bagged",
    "Escape",
    "Feeding",
    "Predated On",
    "Bait feeding",
    "Caught while predating"
  )
)