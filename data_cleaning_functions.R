
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


# Rename Column Function
RenameColumn <- function(df) {
  new.names <- case_when(names(df)%in%wrong.escape~"Escape",
            names(df)%in%wrong.maxn~"MaxN",
            names(df)%in%wrong.interaction~"Interaction",
            names(df)%in%wrong.method~"Method",
            names(df)=="Time..mins."~"Time (mins)",
            names(df)=="Period.time..mins."~"Period time (mins)",
            TRUE~as.character(names(df))
  )
  old.names <- names(df)
  names(df)[names(df)==old.names] <- new.names
  
  return(df)
}

# Categorise Comment Function
