                # SCRIPT FOR ANALYSING AND REPORTING PARKS AUSTRALIA PROJECT 2019
#to do: 
#   Observer data:
#       catch rates by gear, and by gear.hook.combo
#       catch composition
#   TEPS. Make sure there are no duplications between TEPS dataframe (i.e. observer data)
#         and all the cameras
#         Missing: For all TEP graphs, add number of shots observed (LL and GN)

#   Video:
#       Analyse: TEPS (stand alone paper with GHATF observer data?), catch rates (use Hook.combos); video
#       'Current. TEPS Analyses' must combined subsurface, underwater & deck cameras and observer data and must
#           display what camera picked up the interaction


#     use 'explr.dat.entry' to check data entry errors

#MISSING: get number of hours of footage for different cameras form Jack
#         get field of view deck camera 1 Tim and Nils for habitat recons from Jack
#         Add Video.subsurface to TEPS analysis
#         Drop out rates: Overall and for each species By:
#                         1. number of fish caught
#                         2. Km of net or number of hooks
#         re run all (including mapping) once all data entered


rm(list=ls(all=TRUE))
library(tidyverse)
library(dplyr)
library("readxl")
library(ggrepel)
library(ggpubr)
library(rlang)
library(MASS)
library(ggmosaic)
library(Hmisc)
library(gridExtra)
library(stringr)
library(extdplyr)
library(ggpubr)
library(ggridges)
library(vegan)
library(pairwiseAdonis)
library(overlapping)
library(tweedie)
library(mgcv)
library(emmeans)  #for model predictions
library(doParallel)
library(flextable)
library(webshot)

options(stringsAsFactors = FALSE,dplyr.summarise.inform = FALSE) 

#--------- DATA ------------
if(!exists('handl_OneDrive')) source('C:/Users/myb/OneDrive - Department of Primary Industries and Regional Development/Matias/Analyses/SOURCE_SCRIPTS/Git_other/handl_OneDrive.R')

#1. Sharks data base
User="Matias"
if(User=="Matias") source(handl_OneDrive('Analyses/SOURCE_SCRIPTS/Git_other/Source_Shark_bio.R'))


#2. Species list
All.species.names=read.csv(handl_OneDrive("Data/Species.code.csv"))


#3. PA - TEPS interactions recorded by Observers
setwd('M:/Agency Data/Draft Publications/Braccini/2019-20_Parks Australia Project/Fieldwork/Data')
TEPS <- read_excel("TEPS interactions.xlsx", sheet = "Sheet1",skip = 1)


#4. PA - number of hook combinations used in PA project
Hook.combos <- read_excel("Hook count.xlsx", sheet = "Sheet1",skip = 0)


#5. PA - underwater video
setwd(handl_OneDrive("Parks Australia/2019_project/Data/cameras"))

  #net
file.name.GN="Gillnet_Data_3_11_2020_Clean.xlsx"
Video.net.interaction <- read_excel(file.name.GN, sheet = "Interactions")
Video.net.maxN <- read_excel(file.name.GN, sheet = "MaxN")
Video.net.obs <- read_excel(file.name.GN, sheet = "Observation") 

  #longline
file.name.LL="Longline_Data_3_11_2020_Clean.xlsx"
Video.longline.interaction <- read_excel(file.name.LL, sheet = "Interactions")
Video.longline.maxN <- read_excel(file.name.LL, sheet = "MaxN")
Video.longline.obs <- read_excel(file.name.LL, sheet = "Observations")

  #habitat 
#note: CATAMI code used to determine habitat types
file.name.habitat="Gillnet_longline habitat.xlsx"
Video.habitat<- read_excel(file.name.habitat, sheet = "gillnet habitat")


#6. PA - deck camera 1 (points to measuring board)
file.name.habitat.deck="15_01_2020_Deck 1 habitat and fish.xlsx"
Video.habitat.deck<- read_excel(file.name.habitat.deck, sheet = "Habitat")
Video.camera1.deck<- read_excel(file.name.habitat.deck, sheet = "Deck 1 fish landed")
Video.camera1.deck_extra.records<- read_excel(file.name.habitat.deck, sheet = "extra records")


#7. PA - deck camera 2 (points to roller)
file.name.camera2.deck="20_01_2020_Deck 2.xlsx"
Video.camera2.deck<- read_excel(file.name.camera2.deck, sheet = "Deck 2")
Video.camera2.deck_observations<- read_excel(file.name.camera2.deck, sheet = "Other observations")


#8. PA - subsurface camera   
file.name.subsurface="05_02_2021_subSurface.xlsx"
Video.subsurface<- read_excel(file.name.subsurface, sheet = "ALL Dot Point Measurements")
Video.subsurface.comments<- read_excel(file.name.subsurface, sheet = "comment")

source(handl_OneDrive("Analyses/SOURCE_SCRIPTS/Git_Population.dynamics/fn.fig.R"))
Do.tiff="YES"
Do.jpeg="NO"

source(handl_OneDrive("Analyses/SOURCE_SCRIPTS/Git_other/Smart_par.R"))


#---------CONTROL SECTION------------
explr.dat.entry=TRUE
do.len_len=FALSE
do.Historic=FALSE

distance.roller.spreader.Anthony=4.3  #in metres
distance.roller.spreader.Tim=NA  #in metres
distance.roller.spreader.Nils=NA  #in metres

#mesh.deep.Anthony=3.3                   #in metres  (20 meshes of 6.5 inch, source: Jeff Cooke)

metres.observed=5 # average metres observed underwater


hours.underwater.ll='172'  #total number of hours of longline underwater footage   #update from Jack
hours.underwater.gn='571'  #total number of hours of gillnet underwater footage
hours.subsurface.ll='6.15'  #total number of hours of subsurface footage longline
hours.subsurface.gn='20'  #total number of hours of subsurface footage gillnet
hours.deck1.ll='9.25'  #total number of hours of deck camera 1 longline footage
hours.deck1.gn='22.25'  #total number of hours of deck camera 1 gillnet footage
hours.deck2.ll='7.45'  #total number of hours of deck camera 2 longline footage
hours.deck2.gn='17.25'  #total number of hours of deck camera 2 gillnet footage

No.good.water.column=c('PA0059','PA0061') # data sheets no good for looking at Composition around weight or float

Main.species=c("Dusky shark","Whiskery shark","Gummy shark","Sandbar shark",
               "Queen snapper","Blue groper","West Australian dhufish","Pink snapper")
names(Main.species)=c(18003,17003,17001,18007,
                      377004,384002,320000,353001)
Length.weight=data.frame(common_name=Main.species,
                         a_weight=c(1.23e-5,1.63e-5,1.36e-6,6e-6,
                                    1.627539e-05,1.858271e-05,2.451526e-05,1.637477e-05),
                         b_weight=c(2.855,2.733,3.224263,2.9698,
                                    2.924,3.034411,2.91928,2.9251),
                         a_FL.to.TL=c(1.2062,1.0700731,1.0952846,1.1256996,
                                   rep(NA,4)),
                         b_FL.to.TL=c(1.9133,7.818082,3.8499,6.182718,
                                   rep(NA,4))) #Norris et al 2016 & Smallwood et al 2018 for scalefish

Shark.palette=c("firebrick4","red",'yellow')
Teleost.palette=c('chartreuse2','forestgreen','darkgreen')
  
#---------Define TEPS------------
TEPS_Shark.rays=c(37008001,37010003,37035001,37035002)
TEPS_marine.mam=4.1e+07:4.115e+07
TEPS_seabirds=4.0e+07:4.09e+07
TEPS.codes=c(TEPS_Shark.rays,TEPS_marine.mam,TEPS_seabirds)
TEPS.names=data.frame(Name=c('Grey nurse shark','White shark','Smooth stingray','Black stingray',
                             'Pied cormorant','Seabird','Pacific gull',
                             'Australian sea-lion','Humpback whale'),
                      Code=c(37008001,37010003,37035001,37035002,
                             40048006,40000000,40128014,
                             41131005,41112006),
                      Colr=c("brown1","firebrick4","darkorange","chocolate3",
                             "chartreuse3","forestgreen","darkolivegreen3",
                             "steelblue","deepskyblue2"))
TEPS.cols=TEPS.names$Colr
names(TEPS.cols)=TEPS.names$Name


#---------Manipulate Species names------------
suppressWarnings({
  All.species.names=All.species.names%>%
    mutate(Code=ifelse(Taxa=='Elasmobranch'&!is.na(CAAB_code) & nchar(CAAB_code)==4,
                       as.numeric(paste('3700',CAAB_code,sep='')),
                ifelse(Taxa=='Elasmobranch'&!is.na(CAAB_code) & nchar(CAAB_code)==5,
                       as.numeric(paste('370',CAAB_code,sep='')),
                ifelse(Taxa=='Teleost'&!is.na(CAAB_code),as.numeric(paste('37',CAAB_code,sep='')),
                NA)))) 
})


#---------Manipulate PA hook size, type and snood combinations  ------------
if('no of baited hooks'%in%names(Hook.combos))  Hook.combos<-Hook.combos%>%dplyr::select(-'no of baited hooks')
if('Total deployed'%in%names(Hook.combos))  Hook.combos<-Hook.combos%>%dplyr::select(-'Total deployed')
Hook.combos<-Hook.combos%>%
  rename(sheet_no="Sheet no",
         baiting.time='Baiting time (min)',
         baiting.crew='Baiting staff no')%>%
  filter(!is.na(sheet_no))%>%
  mutate_at(c("C10/W","C12/W","C14/W",
              "Eb10/W","Eb12/W","Eb14/W",
              "C10/M","C12/M","C14/M",
              "Eb10/M","Eb12/M","Eb14/M"), as.numeric)%>%
  data.frame

Hook.combos<-Hook.combos%>%
              mutate(hooks.baited=C10.W+C12.W+C14.W+Eb10.W+Eb12.W+Eb14.W+
                                  C10.M+C12.M+C14.M+Eb10.M+Eb12.M+Eb14.M)
  
#---------Basic manipulation of PA observer data------------
Lactate.for.Taylor=DATA%>%
  filter(!is.na(Lactate) | grepl(paste(c("lacta",'lactate:'),collapse="|"), COMMENTS))%>%
  filter(SPECIES%in%c("TG","BW","TK","PJ") & Method%in%c('LL','GN'))
write.csv(Lactate.for.Taylor,handl_OneDrive('Students/2020_Taylor Grosse/data.for.Taylor/Lactate.for.Taylor.csv'),row.names = F)

# Observer.LL.data.historic=DATA%>%
#               filter(Method=='LL' & Mid.Lat <(-26) & !BOAT%in%c('FLIN','HAM','HOU','NAT') & year<2019)
# names(Observer.LL.data.historic)=tolower(names(Observer.LL.data.historic))

DATA=DATA[grep("PA", DATA$SHEET_NO), ]%>%
      filter(year>=2020)%>%
      rename(IDL=TrunkL)%>%
  dplyr::select(SHEET_NO,LINE_NO,RECORDER,Mid.Lat,Mid.Long,Lat.round,Long.round,zone,date,Day,Month,year,BOAT,BLOCK,SKIPPER,BOTDEPTH,
                Set.time,Haul.time,Set.time.end,Haul.time.end,Set.time.avg,Haul.time.avg,SOAK.TIME,
                BaitSpeciesId,Method,MESH_SIZE,MESH_DROP,NET_LENGTH,
                N.hooks,HookLocation,HookRemoved,HookType,HookSize,WireTrace,
                SPECIES,COMMON_NAME,SCIENTIFIC_NAME,Taxa,RetainedFlag,SEX,Number,TL,FL,PL,IDL,
                BloodFlag,FinClipFlag,MuscleFlag,Lactate,BleedingFlag,BAG_NO,COMMENTS,COMMENTS.hdr)
names(DATA)=tolower(names(DATA))
DATA=DATA%>%
  mutate(hooktype=case_when(hooktype%in%c('Circular','Offset circular')~'circular',
         hooktype=='EZ-baiter kerbed'~'Ezb'),
         hooksize=as.numeric(substr(hooksize,1,2)))


#---------Add total weight for main species to PA observer data------------
DATA=DATA%>%
  left_join(Length.weight,by='common_name')%>%
  mutate(tl=case_when(is.na(tl) & 
                        common_name%in%c('Dusky shark','Whiskery shark',
                                         'Gummy shark','Sandbar shark') ~a_FL.to.TL*fl+b_FL.to.TL,
                      TRUE~tl),
         tw=ifelse(common_name%in%Main.species & !is.na(tl),a_weight*tl^b_weight,NA))



#---------Add number of hook combos to PA observer data------------
DATA=DATA%>% 
  left_join(Hook.combos%>%
              dplyr::select(-c(Date,baiting.time,baiting.crew,Comments)),by='sheet_no')%>%
  mutate(Effort=case_when(method=="GN"~soak.time*net_length,
                          method=="LL"~soak.time*n.hooks),
         Effort.hook.combo=case_when(
           method=="LL" & hooktype=='circular' & hooksize==10 & wiretrace=='Yes' ~soak.time * C10.W,
           method=="LL" & hooktype=='circular' & hooksize==12 & wiretrace=='Yes' ~soak.time * C12.W,
           method=="LL" & hooktype=='circular' & hooksize==14 & wiretrace=='Yes' ~soak.time * C14.W,
           method=="LL" & hooktype=='circular' & hooksize==10 & wiretrace=='No' ~soak.time * C10.M,
           method=="LL" & hooktype=='circular' & hooksize==12 & wiretrace=='No' ~soak.time * C12.M,
           method=="LL" & hooktype=='circular' & hooksize==14 & wiretrace=='No' ~soak.time * C14.M,
           method=="LL" & hooktype=='Ezb' & hooksize==10 & wiretrace=='Yes' ~soak.time * Eb10.W,
           method=="LL" & hooktype=='Ezb' & hooksize==12 & wiretrace=='Yes' ~soak.time * Eb12.W,
           method=="LL" & hooktype=='Ezb' & hooksize==14 & wiretrace=='Yes' ~soak.time * Eb14.W,
           method=="LL" & hooktype=='Ezb' & hooksize==10 & wiretrace=='No' ~soak.time * Eb10.M,
           method=="LL" & hooktype=='Ezb' & hooksize==12 & wiretrace=='No' ~soak.time * Eb12.M,
           method=="LL" & hooktype=='Ezb' & hooksize==14 & wiretrace=='No' ~soak.time * Eb14.M,
           TRUE~NA_real_))

DATA=DATA%>%
        mutate(hooks.deployed=n.hooks)

#---------Explore PA observer data for data issues------------
#note: not needed once all data have been entered
if(explr.dat.entry)
{
  setwd(handl_OneDrive("Analyses/Parks Australia/fix this"))
  file.remove(list.files())
  
  No.times=DATA%>%
    filter(is.na(soak.time))%>%
    distinct(sheet_no,.keep_all = T)
  if(nrow(No.times)>0)write.csv(No.times,'No.times.csv',row.names = F)
  
  No.position=DATA%>%
    filter(is.na(mid.lat) | is.na(mid.long))%>%
    distinct(sheet_no,.keep_all = T)
  if(nrow(No.position)>0)write.csv(No.position,'No.position.csv',row.names = F)
  
  No.species=DATA%>%
    filter(is.na(common_name) | is.na(scientific_name))
  if(nrow(No.species)>0) write.csv(No.species,'No.species.csv',row.names = F)
  
    
  LL.issues=DATA%>%
    filter(method=="LL")%>%
    filter(is.na(hooktype) | is.na(hooksize) | is.na(wiretrace) | n.hooks<1)%>%
    dplyr::select(sheet_no,line_no,species,method,n.hooks,hooktype,hooksize,wiretrace)
  if(nrow(LL.issues)>0) write.csv(LL.issues,'LL.issues.csv',row.names = F)
  
  a=DATA%>%
    filter(method=='GN' & (n.hooks>=0 | !is.na(hooksize) | !is.na(hooktype) | !is.na(wiretrace)))%>%
    dplyr::select(sheet_no,line_no,method,n.hooks,hooksize,hooktype,wiretrace,common_name)
  if(nrow(a)>0) write.csv(a,'GN.with.longline.data.csv',row.names = F)
  
  
  GN.issues=DATA%>%filter(method=="GN")%>%
    filter(is.na(net_length) | net_length<3 | net_length>8)%>%
    distinct(sheet_no,.keep_all = T)
  if(nrow(GN.issues)>0) write.csv(GN.issues,'GN.issues.csv',row.names = F)
  
  a=DATA%>%
    filter(method=='LL' & (net_length>=0 | !is.na(mesh_size)))%>%
    dplyr::select(sheet_no,line_no,net_length,method,mesh_size,common_name)
  if(nrow(a)>0) write.csv(a,'LL.with.gillnet.data.csv',row.names = F)
  
  
  a=subset(DATA,method=="LL")%>%
    dplyr::select(sheet_no,line_no,n.hooks,hooks.deployed)%>%
    mutate(dummy=paste(sheet_no,line_no))%>%
    distinct(dummy,.keep_all = T)%>%
    dplyr::select(-dummy)
  a$keep=a$n.hooks==a$hooks.deployed
  a=subset(a,keep=="FALSE")
  if(nrow(a)>0) write.csv(a,'N_hooks.different_to_hooks.deployed.csv',row.names = F)
  
  #retained issues
  dodgy.retained=c("DM.T","SK.T","WC","PJ","SH","WW","SR","BK","FR")
  a=DATA%>%filter(species%in%dodgy.retained & retainedflag=='Yes')%>%
    dplyr::select(sheet_no,line_no,species,retainedflag)%>%
    arrange(sheet_no,line_no)
  if(nrow(a)>0) write.csv(a,'dodgy_retained.csv',row.names = F)
  
}


# ---------Functions: Multivariate stats  -------------------------------
multivariate.fn=function(d,Terms,Def.sp.term,Transf,Show.term,Group,hndl,
                         MDS.title,Simper.title,Permanova.title,
                         simper.cumsum=0.85)
{
  names(d)=tolower(names(d))
  
  what.species.GN=d%>%
    filter(method=='GN')%>%
    group_by_at(Def.sp.term)%>%
    tally()%>%
    arrange(-n)%>%
    ungroup()%>%
    mutate(Row=1:n(),
           Cumktch=cumsum(n),
           Cumktch=Cumktch/sum(n))
  
  if(Group=='95') what.species.GN=what.species.GN%>%mutate(species2=ifelse(Cumktch<=0.95,species,'Other'))
  if(Group=='Top20') what.species.GN=what.species.GN%>%mutate(species2=ifelse(Row<=20,species,'Other'))
  what.species.GN=what.species.GN%>%
    distinct(species,method,species2)%>%
    filter(!species2=='Other')
  
  what.species.LL=d%>%
    filter(method=='LL')%>%
    group_by_at(Def.sp.term)%>%
    tally()%>%
    arrange(-n)%>%
    ungroup()%>%
    mutate(Row=1:n(),
           Cumktch=cumsum(n),
           Cumktch=Cumktch/sum(n))
  if(Group=='95') what.species.LL=what.species.LL%>%mutate(species2=ifelse(Cumktch<=0.95,species,'Other'))
  if(Group=='Top20') what.species.LL=what.species.LL%>%mutate(species2=ifelse(Row<=20,species,'Other'))
  
  what.species.LL=what.species.LL%>%
    distinct(species,method,species2)%>%
    filter(!species2=='Other')
  
  what.species=rbind(what.species.GN,what.species.LL)
  
  d=d%>%
    left_join(what.species,by=c('species','method'))%>%
    mutate(method.zone=factor(paste(method,zone)))%>%
    filter(!is.na(species2))%>%
    group_by_at(c(Terms,'species2'))%>%
    summarise(livewt.c=sum(livewt.c,na.rm=T))%>%
    spread(species2,livewt.c,fill=0)%>%
    ungroup()
  
  d$ColSum=rowSums(d[-match(Terms,names(d))])
  d=d%>%filter(ColSum>10)
  if(Transf=='proportion') d[-match(c(Terms,'ColSum'),names(d))]=d[-match(c(Terms,'ColSum'),names(d))]/d$ColSum
  d=d%>%
    dplyr::select(-ColSum)%>%
    data.frame
  
  Community <- d[-match(Terms,names(d))]
  if(Transf=='sqrt')Community <- sqrt(Community)
  
  this.nm=colnames(Community) %>% str_replace("^[A-Z]*", "")%>%as.numeric()
  names(Community)=All.species.names%>%
    filter(CAES_Code%in%this.nm)%>%
    distinct(CAES_Code,.keep_all=T)%>%
    arrange(CAES_Code)%>%
    pull(COMMON_NAME)
  
  
  #MDS
  MDS <- metaMDS(comm = Community, distance = "bray",k=2,trymax=100, trace = FALSE, autotransform = FALSE)
  MDS_xy <- data.frame(MDS$points)
  MDS_xy$dummy <- d[,match(Show.term,names(d))]
  
  p=ggplot(MDS_xy, aes(MDS1, MDS2, color = dummy)) +
    geom_point(size=3) +
    theme_bw() +
    annotate(geom="text", x=0.85*max(MDS_xy$MDS1), y=min(MDS_xy$MDS2), 
             label=paste("Stress=",round(MDS$stress,3)))+
    theme(legend.position = "top",
          legend.title = element_blank(),
          legend.text=element_text(size=12),
          axis.text=element_text(size=10),
          axis.title=element_text(size=14))+
    scale_color_manual(values=dis.cols[which(names(dis.cols)%in%levels(d$method.zone))], drop=TRUE)+
    guides(colour = guide_legend(nrow = 1))
  fn.fig(paste(hndl,MDS.title,sep=""),2400,2400)
  print(p)
  dev.off()
  
  
  #Permanova
    # 1. overall significance test
  adon.results<-adonis(formula(paste('Community',Show.term,sep='~')),data=d, method="bray",perm=5e3)
  write.csv(as.data.frame(adon.results$aov.tab),paste(hndl,Permanova.title,'.csv',sep=""))
  
    # 2. multilevel pairwise comparison with adjusted p-values
  adonis.pairwise=pairwise.adonis(Community,d[,match(Show.term,names(d))])
  write.csv(as.data.frame(adonis.pairwise),paste(hndl,Permanova.title,'_pairwise.csv',sep=""),row.names = F)
  
  
  #Simper analysis to identify species that discriminate among groups
  SIMPER <- summary(simper(Community, d%>%pull(Show.term),parallel=7))
  
    #1. display species accounting for group differences
  Get=as.data.frame(str_split(names(SIMPER), "_", simplify = TRUE))%>%
    mutate(V1.method=sub("\\ .*", "", V1),
           V2.method=sub("\\ .*", "", V2),
           V1.zone=sub(".* ", "", V1),
           V2.zone=sub(".* ", "", V2),
           id=1:n())%>%
    filter(!V1.method==V2.method & V1.zone==V2.zone)
  SIMPER=SIMPER[Get$id]
  
  disp.simp=vector('list',length(SIMPER))
  for(n in 1:length(disp.simp))
  {
    disp.simp[[n]]=cbind(d[Show.term],
                         Community[row.names(SIMPER[[n]]%>%filter(cumsum<=simper.cumsum))])%>%
      filter(!!sym(Show.term)%in%str_split(names(SIMPER)[n], "_", simplify = TRUE))%>%
      gather(species,prop,-method.zone,)%>%
      mutate(groups=names(SIMPER)[n])
  }
  disp.simp=do.call(rbind,disp.simp)
  
  dis.cls=unique(disp.simp$species)
  dis.cls=All.species.names%>%
    filter(COMMON_NAME%in%unique(disp.simp$species))
  
  colfunc <- colorRampPalette(Shark.palette)
  n.col.elasmos=colfunc(length(dis.cls$CAES_Code[dis.cls$CAES_Code<50000]))
  names(n.col.elasmos)=dis.cls%>%filter(CAES_Code<50000)%>%pull(COMMON_NAME)
  
  colfunc <- colorRampPalette(Teleost.palette)
  n.col.teleos=colfunc(length(dis.cls$CAES_Code[dis.cls$CAES_Code>=50000]))
  names(n.col.teleos)=dis.cls%>%filter(CAES_Code>=50000)%>%pull(COMMON_NAME)
  
  p=disp.simp%>%
    group_by(method.zone,groups,species)%>%
    summarise(prop=mean(prop))%>%
    mutate(method=sub("\\ .*", "", method.zone),
           zone=sub(".* ", "", method.zone))%>%
    ggplot(aes(x=method,y=prop, fill=species))+
    geom_bar(stat="identity", width = 0.5)+
    facet_wrap(~zone,scales='free_y')+
    ylab("Average proportion")+xlab("Method")+
    scale_fill_manual(values=c(n.col.elasmos,n.col.teleos))+
    theme(legend.position = "top",
          legend.title = element_blank(),
          legend.text=element_text(size=10),
          axis.text=element_text(size=10),
          strip.text.x = element_text(size = 12),
          axis.title=element_text(size=14),
          plot.margin=unit(c(.1,.5,.1,.1),"cm"))+
    guides(fill = guide_legend(nrow = 4))
  
  fn.fig(paste(hndl,Simper.title,sep=""),2400,2400)
  print(p)
  dev.off()
  
  SIMPER.out=SIMPER
  for(s in 1:length(SIMPER.out))
  {
    x=SIMPER[[s]]%>%
            mutate(group=names(SIMPER)[s],
                   species=row.names(SIMPER[[s]]))%>%
            relocate(group,species, .before=average)
    x=x%>%
      filter(cumsum<=simper.cumsum)
    SIMPER.out[[s]]=x 
  }
  write.csv(do.call(rbind,SIMPER.out),paste(hndl,Simper.title,'.csv',sep=""),row.names = F)
}
#---------Functions: CPUE standardisation ------------
cpue.stand.fun=function(d,Formula)
{
  mod=gam(Formula,dat=d,family=tw,method="REML")
  return(list(mod=mod,dat=d))
}
pred.fun=function(mod,biascor,PRED)             
{
  if(biascor=="YES")  #apply bias correction for log transf
  {
    lsm=summary(emmeans(mod, PRED, type="link"))%>%
      mutate(response=exp(emmean)*exp(SE^2/2),
             lower.CL=exp(lower.CL)*exp(SE^2/2),
             upper.CL=exp(upper.CL)*exp(SE^2/2))
  }
  if(biascor=="NO") lsm=summary(emmeans(mod, PRED, type="response"))
  
  lsm$SD=lsm$SE
  
  return(lsm)
}
pred.fun.continuous=function(d,mod,PRED,Formula)
{
  NewD=NULL
  VARS=all.vars(Formula)[-1]
  if(PRED[1]%in%VARS)
  {
    #create new data
    VARS=VARS[!VARS%in%PRED]
    NewD=d%>%pull(PRED)
    if(length(PRED)==1)
    {
      NewD=seq(min(NewD,na.rm=T),max(NewD,na.rm=T))
      if(length(NewD)<50) NewD=seq(min(NewD),max(NewD),length.out = 100)
      NewD=as.data.frame(NewD)
      names(NewD)=PRED
    }else
    {
      NewD$dummy=paste(NewD[,1],NewD[,2])
      NewD=NewD%>%
        distinct(dummy,.keep_all = TRUE)%>%
        dplyr::select(-dummy)
    }
    
    FixedVar=as.data.frame(matrix(NA,nrow=1,ncol=length(VARS)))
    names(FixedVar)=VARS
    for(l in 1:length(VARS))
    {
      vv=d%>%pull(VARS[l])
      
      if(is.factor(vv))
      {
        FF=names(sort(-table(vv)))[1]
        FixedVar[,l]=factor(FF,levels(vv))
      }else
        FixedVar[,l]=mean(vv,na.rm=T)
      
      rm(vv)
    }
    NewD=cbind(NewD,FixedVar)
    
    #predict new data
    a=predict(mod,newdata=NewD,type="response",se.fit=T)
    NewD$Pred=a$fit
    NewD$Pred.SE=a$se.fit
    
    NewD=NewD%>%
      rename(response=Pred,
             SE=Pred.SE)%>%
      mutate(lower.CL=response-1.96*SE,
             upper.CL=response+1.96*SE)%>%
      dplyr::select(all_of(PRED),response,SE,lower.CL,upper.CL)
  }
  return(NewD)  
}

#---------Historic catch analyses (commercial catch and effort) ------------    
HNDL=handl_OneDrive('Analyses/Parks Australia/outputs/')
le.paste=function(x) paste(HNDL,x,sep='')
if(do.Historic)  
{
  do.monthly=FALSE  #incomplete info for teleost, species id issues, etc so drop
  library(fields)
  library(gridExtra)
  library(grid)
  library(data.table)
  library(plotrix)
  source(handl_OneDrive("Analyses/SOURCE_SCRIPTS/Git_other/Plot.Map.R"))
  hndl=handl_OneDrive("Analyses/Parks Australia/outputs/Historic_catch_effort")
  
  # Data
  setwd(handl_OneDrive("Analyses/Data_outs"))
  Data.daily.original=fread("Data.daily.original.csv",data.table=FALSE)
  Data.daily=fread("Data.daily.csv",data.table=FALSE)
  Effort.daily=fread("Effort.daily.csv",data.table=FALSE)
  Data.monthly=fread("Data.monthly.csv",data.table=FALSE)
  Effort.monthly=fread("Effort.monthly.csv",data.table=FALSE)

  List.of.species=Data.daily%>%distinct(SPECIES,SNAME)%>%arrange(SPECIES)
  TARGETS=list(whiskery=17003,gummy=17001,dusky=18003,sandbar=18007) 
  Scalefish.species=188000:599001
  Current.yr="2019-20" 
  
  # Power analysis
  do.power=FALSE
  if(do.power)
  {
    seq.lat=c(-26.83, -26.67, -26.50, -26.33, -26.17, -26.00,
              -27.83, -27.67, -27.50, -27.33, -27.17, -27.00,
              -28.83, -28.67, -28.50, -28.33, -28.17, -28.00,
              -29.83, -29.67, -29.50, -29.33, -29.17, -29.00,
              -30.83, -30.67, -30.50, -30.33, -30.17, -30.00,
              -31.83, -31.67, -31.50, -31.33, -31.17, -31.00,
              -32.83, -32.67, -32.50, -32.33, -32.17, -32.00,
              -33.83, -33.67, -33.50, -33.33, -33.17, -33.00,
              -34.83, -34.67, -34.50, -34.33, -34.17, -34.00,
              -35.83, -35.67, -35.50, -35.33, -35.17, -35.00)
    seq.lon=c(113.83, 113.67, 113.50, 113.33, 113.17, 113.00,
              114.83, 114.67, 114.50, 114.33, 114.17, 114.00,
              115.83, 115.67, 115.50, 115.33, 115.17, 115.00,
              116.83, 116.67, 116.50, 116.33, 116.17, 116.00,
              117.83, 117.67, 117.50, 117.33, 117.17, 117.00,
              118.83, 118.67, 118.50, 118.33, 118.17, 118.00,
              119.83, 119.67, 119.50, 119.33, 119.17, 119.00,
              120.83, 120.67, 120.50, 120.33, 120.17, 120.00,
              121.83, 121.67, 121.50, 121.33, 121.17, 121.00,
              122.83, 122.67, 122.50, 122.33, 122.17, 122.00,
              123.83, 123.67, 123.50, 123.33, 123.17, 123.00,
              124.83, 124.67, 124.50, 124.33, 124.17, 124.00,
              125.83, 125.67, 125.50, 125.33, 125.17, 125.00,
              126.83, 126.67, 126.50, 126.33, 126.17, 126.00,
              127.83, 127.67, 127.50, 127.33, 127.17, 127.00,
              128.83, 128.67, 128.50, 128.33, 128.17, 128.00,
              129.83, 129.67, 129.50, 129.33, 129.17, 129.00)
    
    fn.scale=function(x,MX,scaler) ((x/MX)^0.5)*scaler
    fn.plt=function(dd,Main,titl,MAX)
    {
      with(dd,
           {
             plot(LONG,LAT,pch=19,ylim=Ylim,xlim=Xlim,col="steelblue",cex=fn.scale(mean.cpue,MAX,2.5),main=Main)
             legend("topright",paste(round(MAX,2)),bty='n',cex=1.25,
                    pt.cex=fn.scale(MAX,MAX,2),pch=19,col="steelblue",title=titl)
           })
    }
    fn.img.plt=function(dd,TITL)
    {
      seq.lat1=subset(seq.lat,seq.lat<=Ylim[2] & seq.lat>=Ylim[1])
      seq.lon1=subset(seq.lon,seq.lon<=Xlim[2] & seq.lon>=Xlim[1])
      misn.lat=seq.lat1[which(!seq.lat1%in%unique(dd$lat10.corner))]
      
      misn.lon=seq.lon1[which(!seq.lon1%in%unique(dd$long10.corner))]
      if(length(misn.lat)>0 | length(misn.lon)>0)
      {
        combo=expand.grid(lat10.corner=seq.lat1, long10.corner=seq.lon1)
        dd=combo%>%left_join(dd,by=c("lat10.corner","long10.corner"))
      }
      dd=dd%>%arrange(long10.corner)
      dd=dd[,-match('block10',names(dd))]  
      dd=dd%>%
        spread(lat10.corner,mean.cpue)
      Lon=as.numeric(dd$long10.corner)
      dd=as.matrix(dd[,-1]) 
      LaT=as.numeric(colnames(dd))
      brk<- quantile( c(dd),probs=seq(0,1,.2),na.rm=T)
      YLim=Ylim
      XLim=Xlim
      YLim[1]=YLim[1]-0.5
      YLim[2]=YLim[2]+0.5
      XLim[1]=XLim[1]-0.5
      XLim[2]=XLim[2]+0.5
      image.plot(Lon,LaT,dd, breaks=brk, col=rev(heat.colors(length(brk)-1)), 
                 lab.breaks=names(brk),ylim=YLim,xlim=XLim,ylab="",xlab="")
      legend('topright',TITL,bty='n',cex=.9)
      
    }
    fn.parks.power=function(ktch,efF,do.what)
    {
      ktch=ktch%>%filter(METHOD%in%c('GN','LL') & Estuary=="NO")%>%
        group_by(Same.return.SNo,METHOD,day,FINYEAR,MONTH,BLOCKX,block10,LAT,LONG) %>%
        summarise(LIVEWT.c=sum(LIVEWT.c))%>%data.frame
      efF=efF%>%dplyr::select(-c(block10,LAT,LONG))%>%
        mutate(hook.days=hooks,
               hook.hours=hooks*hours.c)
      d=left_join(ktch,efF,by='Same.return.SNo')%>%
        dplyr::select(Same.return.SNo,METHOD,day,FINYEAR,MONTH,BLOCKX,block10,LAT,LONG,LIVEWT.c,
                      hours.c,shots.c,netlen.c,hooks,Km.Gillnet.Hours.c,hook.hours)%>%
        mutate(Hundred.m.Gillnet.Hours.c=Km.Gillnet.Hours.c*10,
               Hundred.hook.hours=hook.hours/100,
               cpue.hour=ifelse(METHOD=="GN",LIVEWT.c/Hundred.m.Gillnet.Hours.c,   
                                ifelse(METHOD=="LL",LIVEWT.c/Hundred.hook.hours,NA)))%>%
        filter(!cpue.hour=="Inf")
      Ylim=floor(range(d$LAT))
      Xlim=floor(range(d$LONG))
      
      #Overal boxplots and histograms
      d=d%>%group_by(METHOD)%>%mutate(n=n()/nrow(d))  #add data weight
      p=vector('list',5)
      p[[1]]=ggplot(d,aes(x=METHOD,y=cpue.hour))+
        geom_boxplot(varwidth = T,fill="grey60")+ facet_wrap(~ MONTH)+
        coord_cartesian(ylim=c(0, quantile(d$cpue.hour,.99)))+
        ylab("cpue (100 gillnet metres / number of hooks per hour)")
      p[[2]]=ggplot(d,aes(fill=METHOD,x=cpue.hour))+
        geom_density(,col=NA,alpha=.35)+ facet_wrap(~ MONTH)+
        coord_cartesian(ylim=c(0, 5),xlim=c(0,quantile(d$cpue.hour,.99)))+
        xlab("cpue (100 gillnet metres / number of hooks per hour)")
      p[[3]]=ggplot(d,aes(x=METHOD,y=cpue.hour))+
        geom_violin(fill="grey60")+ facet_wrap(~ MONTH)+
        coord_cartesian(ylim=c(0, quantile(d$cpue.hour,.99)))+
        ylab("cpue (100 gillnet metres / number of hooks per hour)")
      
      d.block=d%>%group_by(BLOCKX)%>%
        mutate(n=n()/nrow(d))%>%
        filter(n>0.01)%>%
        data.frame
      p[[4]]=ggplot(d.block,aes(x=METHOD,y=cpue.hour))+
        geom_boxplot(varwidth = T,fill="grey60")+ facet_wrap(~ BLOCKX)+
        coord_cartesian(ylim=c(0, quantile(d$cpue.hour,.999)))+
        ylab("cpue (100 gillnet metres / number of hooks per hour)")
      # p[[5]]=ggplot(d.block,aes(fill=METHOD,x=cpue.hour))+
      #   geom_density(,col=NA,alpha=.35)+ facet_wrap(~ BLOCKX)+
      #   coord_cartesian(ylim=c(0, 5),xlim=c(0,quantile(d$cpue.hour,.99)))+
      #   xlab("cpue (100 gillnet metres / number of hooks per hour)")
      p[[5]]=ggplot(d.block,aes(x=METHOD,y=cpue.hour))+
        geom_violin(fill="grey60")+ facet_wrap(~ BLOCKX)+
        coord_cartesian(ylim=c(0, quantile(d$cpue.hour,.999)))+
        ylab("cpue (100 gillnet metres / number of hooks per hour)")
      print(p)
      
      #Spatial
      #LL vs GN, all years combined
      par(mfcol=c(2,1),mar=c(1,1,2,1),oma=c(2,2,.1,.1),mgp=c(1,.45,0))
      if(do.what=="bubbles")
      {
        agg=d%>%group_by(METHOD,LAT,LONG)%>%summarise(mean.cpue=mean(cpue.hour))%>%data.frame
        fn.plt(dd=subset(agg,METHOD=="GN"),Main="All daily gillnet records",
               titl="kg per 100m gn hour",MAX=max(subset(agg,METHOD=="GN")$mean.cpue))
        fn.plt(dd=subset(agg,METHOD=="LL"),Main="All daily longline records",
               titl="kg per 100 hook hour",MAX=max(subset(agg,METHOD=="LL")$mean.cpue))
      }
      if(do.what=="image")
      {
        agg=d%>%group_by(METHOD,block10)%>%summarise(mean.cpue=mean(cpue.hour))%>%
          mutate(lat10.corner=round(-(abs(as.numeric(substr(block10,1,2))+10*(as.numeric(substr(block10,3,3)))/60)),2),
                 long10.corner=round(100+as.numeric(substr(block10,4,5))+10*(as.numeric(substr(block10,6,6)))/60,2))%>%
          data.frame
        fn.img.plt(dd=agg%>%filter(METHOD=="GN")%>%dplyr::select(-METHOD),TITL="All daily gillnet records")
        fn.img.plt(dd=agg%>%filter(METHOD=="LL")%>%dplyr::select(-METHOD),TITL="All daily longline records")
      }
      
      #LL vs GN, by month
      mnth=1:12
      par(mfcol=c(4,3),mar=c(1,2,2,2),oma=c(2,1,.1,1),mgp=c(1,.45,0))
      if(do.what=="bubbles")
      {
        agg=d%>%group_by(METHOD,MONTH,LAT,LONG)%>%summarise(mean.cpue=mean(cpue.hour))%>%data.frame
        for(m in mnth) 
        {
          if(nrow(subset(agg,METHOD=="GN" & MONTH==mnth[m]))==0) plot.new() else
            fn.plt(dd=subset(agg,METHOD=="GN" & MONTH==mnth[m]),Main=paste("Gillnet, Month=",mnth[m]),
                   titl="kg per 100m gn hour",MAX=quantile(subset(agg,METHOD=="GN")$mean.cpue,.99))
        }
        for(m in mnth) 
        {
          if(nrow(subset(agg,METHOD=="LL" & MONTH==mnth[m]))==0) plot.new() else
            fn.plt(dd=subset(agg,METHOD=="LL" & MONTH==mnth[m]),Main=paste("Longline, Month=",mnth[m]),
                   titl="kg per 100 hook hour",MAX=quantile(subset(agg,METHOD=="LL")$mean.cpue,.99))
        }
      }
      if(do.what=="image")
      {
        agg=d%>%group_by(METHOD,MONTH,block10)%>%summarise(mean.cpue=mean(cpue.hour))%>%
          mutate(lat10.corner=round(-(abs(as.numeric(substr(block10,1,2))+10*(as.numeric(substr(block10,3,3)))/60)),2),
                 long10.corner=round(100+as.numeric(substr(block10,4,5))+10*(as.numeric(substr(block10,6,6)))/60,2))%>%
          data.frame
        for(m in mnth)
        {
          if(nrow(subset(agg,METHOD=="GN" & MONTH==mnth[m]))<=1) plot.new() else
          {
            dd1=agg%>%filter(METHOD=="GN"& MONTH==mnth[m])%>%dplyr::select(-c(METHOD,MONTH))
            fn.img.plt(dd=dd1,TITL=paste("Gillnet, Month=",mnth[m]))
          }
        }
        for(m in mnth)
        {
          if(nrow(subset(agg,METHOD=="LL" & MONTH==mnth[m]))<=1) plot.new() else
          {
            dd1=agg%>%filter(METHOD=="LL"& MONTH==mnth[m])%>%dplyr::select(-c(METHOD,MONTH))
            fn.img.plt(dd=dd1,TITL=paste("Longline, Month=",mnth[m]))
          }
        }
      }
      
      #LL vs GN, by year
      yr=sort(unique(d$FINYEAR))
      par(mfcol=c(4,3),mar=c(1,2,2,2),oma=c(2,1,.1,1),mgp=c(1,.45,0))
      if(do.what=="bubbles")
      {
        agg=d%>%group_by(METHOD,FINYEAR,LAT,LONG)%>%summarise(mean.cpue=mean(cpue.hour))%>%data.frame   
        for(m in 1:length(yr)) 
        {
          if(nrow(subset(agg,METHOD=="GN" & FINYEAR==yr[m]))==0) plot.new() else
            fn.plt(dd=subset(agg,METHOD=="GN" & FINYEAR==yr[m]),Main=paste("Gillnet",yr[m]),
                   titl="kg per 100m gn hour",MAX=quantile(subset(agg,METHOD=="GN")$mean.cpue,.99))
        }
        for(m in 1:length(yr))
        {
          if(nrow(subset(agg,METHOD=="LL" & FINYEAR==yr[m]))==0) plot.new() else
            fn.plt(dd=subset(agg,METHOD=="LL" & FINYEAR==yr[m]),Main=paste("Longline",yr[m]),
                   titl="kg per 100 hook hour",MAX=quantile(subset(agg,METHOD=="LL")$mean.cpue,.99))
        }
      }
      if(do.what=="image")
      {
        agg=d%>%group_by(METHOD,FINYEAR,block10)%>%summarise(mean.cpue=mean(cpue.hour))%>%
          mutate(lat10.corner=round(-(abs(as.numeric(substr(block10,1,2))+10*(as.numeric(substr(block10,3,3)))/60)),2),
                 long10.corner=round(100+as.numeric(substr(block10,4,5))+10*(as.numeric(substr(block10,6,6)))/60,2))%>%
          data.frame
        for(m in 1:length(yr))
        {
          if(nrow(subset(agg,METHOD=="GN" & FINYEAR==yr[m]))<=1) plot.new() else
          {
            dd1=agg%>%filter(METHOD=="GN"& FINYEAR==yr[m])%>%dplyr::select(-c(METHOD,FINYEAR))
            fn.img.plt(dd=dd1,TITL=paste("Gillnet, Finyear=",yr[m]))
          }
        }
        for(m in 1:length(yr))
        {
          if(nrow(subset(agg,METHOD=="LL" & FINYEAR==yr[m]))<=1) plot.new() else
          {
            dd1=agg%>%filter(METHOD=="LL"& FINYEAR==yr[m])%>%dplyr::select(-c(METHOD,FINYEAR))
            fn.img.plt(dd=dd1,TITL=paste("Longline, Finyear=",yr[m]))
          }
        }
        
      }
      
      topmonth.GN=d%>%filter(METHOD=="GN")%>%
        group_by(MONTH)%>%
        summarise(mean.cpue=mean(cpue.hour))%>%
        top_n(2,mean.cpue)%>%
        dplyr::select(MONTH)%>%data.frame
      
      topmonth.LL=d%>%filter(METHOD=="LL")%>%
        group_by(MONTH)%>%
        summarise(mean.cpue=mean(cpue.hour))%>%
        top_n(2,mean.cpue)%>%
        dplyr::select(MONTH)%>%data.frame
      
      #GN
      a=d%>%filter(MONTH%in%topmonth.GN$MONTH & METHOD=="GN")
      a=sort(table(a$block10))
      a=names(a[a>=quantile(a,.975)])
      b=d%>%filter(MONTH%in%topmonth.GN$MONTH & METHOD=="GN" & block10%in%a)
      par(mfcol=c(1,1),mar=c(2,2,2,2),oma=c(2,1,.1,1),mgp=c(1,.45,0))
      boxplot(b$cpue.hour~as.factor(b$block10),col=2,main=paste("Gillnet, month=",paste(topmonth.GN$MONTH,collapse="&"),sep=""),
              las=2,ylab="cpue",xlab='')
      unik.bl=unique(b$block10)
      smart.par(length(unik.bl),c(1,2,2,2),c(2,1,.1,1),c(1,.45,0))
      for(u in 1:length(unik.bl))with(subset(b,block10==unik.bl[u]),hist(cpue.hour,col=2,xlim=c(0,max(b$cpue.hour)),
                                                                         ylab="",cex.main=0.9,main=paste("month=",paste(topmonth.GN$MONTH,collapse="&"),", block=",unik.bl[u],sep="")))
      
      #LL
      par(mfcol=c(1,1),mar=c(2,2,2,2),oma=c(2,1,.1,1),mgp=c(1,.45,0))
      a=d%>%filter(MONTH%in%topmonth.LL$MONTH & METHOD=="LL")
      a=sort(table(a$block10))
      a=names(a[a>=quantile(a,probs=c(.90))])
      b=d%>%filter(MONTH%in%topmonth.LL$MONTH & METHOD=="LL" & block10%in%a)
      boxplot(b$cpue.hour~as.factor(b$block10),col=2,main=paste("Longline, month=",paste(topmonth.LL$MONTH,collapse="&"),sep=""),
              las=2,ylab="cpue",xlab='')
      unik.bl=unique(b$block10)
      smart.par(length(unik.bl),c(1,2,2,2),c(2,1,.1,1),c(1,.45,0))
      for(u in 1:length(unik.bl))with(subset(b,block10==unik.bl[u]),hist(cpue.hour,col=2,xlim=c(0,max(b$cpue.hour)),
                                                                         ylab="",cex.main=0.9,main=paste("month=",paste(topmonth.LL$MONTH,collapse="&"),", block=",unik.bl[u],sep="")))
      
    }
    
    for(i in 1:length(TARGETS))
    {
      pdf(file=paste(hndl,"/Power.analysis/",names(TARGETS)[i],".pdf",sep=""))
      fn.parks.power(ktch=subset(Data.daily,SPECIES%in%TARGETS[[i]]),
                     efF=Effort.daily%>%distinct(Same.return.SNo,.keep_all=T),
                     do.what="image")
      dev.off()
    }
    
  }

  TDGDLF.lat.range=c(-26,-40)

  # Some manipulations
  Data.monthly=Data.monthly%>%
    filter(TYPE.DATA=='monthly')%>%
    filter(LAT<=TDGDLF.lat.range[1] & LAT >=TDGDLF.lat.range[2] & METHOD%in%c("GN","LL") &
             Estuary=="NO")%>%
    mutate(YEAR=as.numeric(substr(FINYEAR,1,4)),
           SNAME=tolower(SNAME),
           SNAME=case_when(SPECIES== 10001~ 'shortfin mako shark',
                           SPECIES== 13000~ 'wobbegong sharks',
                           SPECIES== 17001~ 'gummy shark',
                           SPECIES== 17003~ 'whiskery shark',
                           SPECIES== 17008~ 'school shark',
                           SPECIES== 18001~ 'copper shark',
                           SPECIES== 18003~ 'dusky shark',
                           SPECIES== 18007~ 'sandbar shark',
                           SPECIES== 18021~ 'bull shark',
                           SPECIES== 18023~ 'spinner shark',
                           SPECIES== 18022~ 'tiger shark',
                           SPECIES== 19000~ 'hammerheads',
                           SPECIES== 20000~ 'spurdogs',
                           SPECIES== 22999~ 'Other sharks',
                           SPECIES== 23002~ 'common saw shark',
                           SPECIES== 31000~ 'skates and rays',
                           SPECIES== 258004~ 'bight redfish',
                           SPECIES== 311100~ 'breaksea cod',
                           SPECIES== 311060~ 'grey banded cod',
                           SPECIES== 311152~ '8-bar cod',
                           SPECIES== 320000~ 'West Australian dhufish',
                           SPECIES== 337007~ 'samsonfish',
                           SPECIES== 335001~ 'cobia',
                           SPECIES== 344002~ 'western Australian salmon',
                           SPECIES== 346002~ 'goldband snapper',
                           SPECIES== 346914~ 'ruby snapper',
                           SPECIES== 350000~ 'sweetlip',
                           SPECIES== 351006~ 'blue-lined emperor',
                           SPECIES== 351009~ 'sweetlip emperor',
                           SPECIES== 353001~ 'pink snapper',
                           SPECIES== 353901~ 'mixed breams',
                           SPECIES== 353998~ 'silver bream',
                           SPECIES== 363001~ 'butterfish',
                           SPECIES== 367000~ 'boarfishes',
                           SPECIES== 367003~ 'long-snouted boarfish',
                           SPECIES== 377004~ 'blue morwong',
                           SPECIES== 384002~ 'western blue groper',
                           SPECIES== 384999~ 'baldchin groper',
                           SPECIES== 441018~ 'grey mackerel',
                           SPECIES== 441025~ 'shark mackerel',
                           SPECIES== 599000~ 'Other scalefish',
                           SPECIES== 702003~ 'blue manna',
                           SPECIES== 702009~ 'champagne crab',
                           TRUE~SNAME),
           SNAME=capitalize(SNAME)) 


  Data.daily=Data.daily%>%
        filter(LAT<=TDGDLF.lat.range[1] & LAT >=TDGDLF.lat.range[2] & METHOD%in%c("GN","LL") &
                 Estuary=="NO")%>%
        mutate(YEAR=as.numeric(substr(FINYEAR,1,4)),
               SNAME=tolower(SNAME),
               SNAME=case_when(SPECIES== 10001~ 'shortfin mako shark',
                               SPECIES== 13000~ 'wobbegong sharks',
                               SPECIES== 17001~ 'gummy shark',
                               SPECIES== 17003~ 'whiskery shark',
                               SPECIES== 17008~ 'school shark',
                               SPECIES== 18001~ 'copper shark',
                               SPECIES== 18003~ 'dusky shark',
                               SPECIES== 18007~ 'sandbar shark',
                               SPECIES== 18021~ 'bull shark',
                               SPECIES== 18023~ 'spinner shark',
                               SPECIES== 18022~ 'tiger shark',
                               SPECIES== 19000~ 'hammerheads',
                               SPECIES== 20000~ 'spurdogs',
                               SPECIES== 22999~ 'Other sharks',
                               SPECIES== 23002~ 'common saw shark',
                               SPECIES== 31000~ 'skates and rays',
                               SPECIES== 258004~ 'bight redfish',
                               SPECIES== 311100~ 'breaksea cod',
                               SPECIES== 311060~ 'grey banded cod',
                               SPECIES== 311152~ '8-bar cod',
                               SPECIES== 320000~ 'West Australian dhufish',
                               SPECIES== 337007~ 'samsonfish',
                               SPECIES== 335001~ 'cobia',
                               SPECIES== 344002~ 'western Australian salmon',
                               SPECIES== 346002~ 'goldband snapper',
                               SPECIES== 346914~ 'ruby snapper',
                               SPECIES== 350000~ 'sweetlip',
                               SPECIES== 351006~ 'blue-lined emperor',
                               SPECIES== 351009~ 'sweetlip emperor',
                               SPECIES== 353001~ 'pink snapper',
                               SPECIES== 353901~ 'mixed breams',
                               SPECIES== 353998~ 'silver bream',
                               SPECIES== 363001~ 'butterfish',
                               SPECIES== 367000~ 'boarfishes',
                               SPECIES== 367003~ 'long-snouted boarfish',
                               SPECIES== 377004~ 'blue morwong',
                               SPECIES== 384002~ 'western blue groper',
                               SPECIES== 384999~ 'baldchin groper',
                               SPECIES== 441018~ 'grey mackerel',
                               SPECIES== 441025~ 'shark mackerel',
                               SPECIES== 599000~ 'Other scalefish',
                               SPECIES== 702003~ 'blue manna',
                               SPECIES== 702009~ 'champagne crab',
                               TRUE~SNAME),
               SNAME=capitalize(SNAME))
  
  Data.daily.original=Data.daily.original%>%
                       filter(Same.return.SNo%in%unique(Data.daily$Same.return.SNo))
  
  #1. Number of vessels with gillnet or longline by year
  d=Data.monthly%>%
    distinct(Same.return,.keep_all=T)%>%
    filter(METHOD%in%c('GN',"LL"))%>%
    mutate(This=paste(VESSEL,METHOD,FINYEAR))%>%
    distinct(This,.keep_all=T)%>%
    group_by(YEAR,METHOD,zone)%>%
    tally()%>%
    mutate(group=as.factor(paste(METHOD,zone)),
           Data.set="Monthly")%>%
    filter(YEAR<=2005)
  
  d1=Data.daily%>%
    distinct(Same.return,.keep_all=T)%>%
    filter(METHOD%in%c('GN',"LL"))%>%
    mutate(This=paste(VESSEL,METHOD,FINYEAR))%>%
    distinct(This,.keep_all=T)%>%
    group_by(YEAR,METHOD,zone)%>%
    tally()%>%
    mutate(group=as.factor(paste(METHOD,zone)),
           Data.set="Daily")
  
  colfunc <- colorRampPalette(c("orange","firebrick1"))
  n.col.GN=colfunc(length(grep("GN",unique(d$group))))
  colfunc <- colorRampPalette(c("lightblue3", "dodgerblue4"))
  n.col.LL=colfunc(length(grep("LL",unique(d$group))))
  dis.cols=c(n.col.GN,n.col.LL)
  names(dis.cols)=levels(d$group)
  dis.cols=dis.cols[match(levels(droplevels(d$group)),names(dis.cols))]
  
  fn.fig(le.paste("Historic_catch_effort/Annual number of vessels by method"),2400,1800)
  rbind(d,d1)%>%
    mutate(Data.set=factor(Data.set,levels=c("Monthly","Daily")))%>%
    ggplot(aes(YEAR,n))+
    geom_col(aes(fill=group))+
    facet_wrap(~Data.set,scales='free_x')+
    ylab("Number of Vessels")+xlab("Financial year")+
    scale_fill_manual(values=dis.cols)+
    theme(legend.position = "top",
          legend.title = element_blank(),
          legend.text=element_text(size=12),
          axis.text=element_text(size=10),
          strip.text.x = element_text(size = 12),
          axis.title=element_text(size=14),
          plot.margin=unit(c(.1,.5,.1,.1),"cm"))+
    guides(fill = guide_legend(nrow = 1))
  dev.off()
  
  #2. Number of fishing days with gillnet or longline by year 
  d=Data.monthly%>%
    distinct(Same.return,.keep_all=T)%>%
    filter(METHOD%in%c('GN',"LL"))%>%
    group_by(YEAR,METHOD,zone)%>%
    summarise(Bdays=sum(BDAYS))%>%
    mutate(group=as.factor(paste(METHOD,zone)),
           Data.set="Monthly")%>%
    filter(YEAR<=2005)
  
  d1=Data.daily%>%
    distinct(Same.return,.keep_all=T)%>%
    filter(METHOD%in%c('GN',"LL"))%>%
    group_by(YEAR,METHOD,zone)%>%
    summarise(Bdays=sum(BDAYS))%>%
    mutate(group=as.factor(paste(METHOD,zone)),
           Data.set="Daily")
  
  fn.fig(le.paste("Historic_catch_effort/Annual fishing days by method"),2400,1800)
  rbind(d,d1)%>%
    mutate(Data.set=factor(Data.set,levels=c("Monthly","Daily")))%>%
    ggplot(aes(YEAR,Bdays))+
    geom_col(aes(fill=group))+
    facet_wrap(~Data.set,scales='free_x')+
    ylab("Number of fishing days")+xlab("Financial year")+
    scale_fill_manual(values=dis.cols)+
    theme(legend.position = "top",
          legend.title = element_blank(),
          legend.text=element_text(size=12),
          axis.text=element_text(size=10),
          strip.text.x = element_text(size = 12),
          axis.title=element_text(size=14),
          plot.margin=unit(c(.1,.5,.1,.1),"cm"))+
    guides(fill = guide_legend(nrow = 1))
  dev.off()
  
  
  #3. Catch composition GN and LL
    #3.1. Overall (show 20 top species, aggregate the rest)
  fn.br.plt=function(dd,TOP,yMx,CX.nm,Xmax=0.65)
  {
    dd=dd%>%
      mutate(Prop=Total/sum(dd$Total))%>%
      arrange(-Prop)%>%mutate(new.sp=SNAME)
    if(nrow(dd)>TOP) dd$new.sp[(TOP+1):nrow(dd)]="Other"  
    n.other=length(unique(dd$SNAME))-(length(unique(dd$new.sp))-1)
    dd=dd%>%mutate(colr=ifelse(SPECIES<50000,"steelblue","firebrick"),
                   colr=ifelse(new.sp=="Other","forestgreen",colr),
                   new.sp=ifelse(new.sp=="Other",paste("Other (n=",n.other," species)",sep=""),new.sp))%>%
      group_by(new.sp,colr)%>%
      summarise(Prop=sum(Prop))%>%
      data.frame%>%
      arrange(Prop)
    with(dd,barplot(Prop,horiz = T,col=colr,names.arg=new.sp,cex.names=CX.nm,las=1,xlim=c(0,Xmax)))
    box()
  }
  fn.ktch.comp=function(ktch,what,Min.overlap)
  {
     
    #Overall comparison GN vs LL
    dat=ktch%>%group_by(METHOD,SPECIES,SNAME)%>%
      summarise(Total = sum(LIVEWT.c))
    fn.fig(paste(hndl,"/Catch_composition/Overall_GN_LL_overall",what,sep=""),2400,1600)
    par(mfcol=c(1,2),mar=c(2,10.5,1,.1),oma=c(1,.1,.5,.3),mgp=c(1.5,.5,0),cex.axis=.9)
    fn.br.plt(dd=dat%>%filter(METHOD=='GN'),TOP=20,yMx=1,CX.nm=1)
    mtext(paste("Gillnet (",round(sum(subset(dat,METHOD=='GN')$Total)/1000,0)," tonnes)",sep=""),3,cex=1)
    fn.br.plt(dd=dat%>%filter(METHOD=='LL'),TOP=20,yMx=1,CX.nm=1)
    mtext(paste("Longline (",round(sum(subset(dat,METHOD=='LL')$Total)/1000,0)," tonnes)",sep=""),3,cex=1)
    mtext("Proportion of total catch",1,outer=T,line=0.1,cex=1.2) 
    dev.off()
    
    #By zone comparison GN vs LL
    dat=ktch%>%group_by(METHOD,zone,SPECIES,SNAME)%>%
      summarise(Total = sum(LIVEWT.c))
    CX.NM=.7
    Top=15
    fn.fig(paste(hndl,"/Catch_composition/Overall_GN_LL_by zone",what,sep=""),2400,1600)
    par(mfcol=c(3,2),mar=c(2,7,.1,.1),oma=c(1.1,.1,1.1,.35),mgp=c(1.5,.5,0))
      #GN
    fn.br.plt(dd=dat%>%filter(METHOD=='GN' & zone=="West"),TOP=Top,yMx=1,CX.nm=CX.NM)
    mtext("Gillnet",3,cex=1)
    legend('bottomright',paste("West (",round(sum(subset(dat,METHOD=='GN'& zone=="West")$Total)/1000,1),
                            " tonnes)",sep=""),bty='n')
    fn.br.plt(dd=dat%>%filter(METHOD=='GN' & zone=="Zone1"),TOP=Top,yMx=1,CX.nm=CX.NM)
    legend('bottomright',paste("Zone1 (",round(sum(subset(dat,METHOD=='GN'& zone=="Zone1")$Total)/1000,1),
                            " tonnes)",sep=""),bty='n')
    fn.br.plt(dd=dat%>%filter(METHOD=='GN' & zone=="Zone2"),TOP=Top,yMx=1,CX.nm=CX.NM)
    legend('bottomright',paste("Zone2 (",round(sum(subset(dat,METHOD=='GN'& zone=="Zone2")$Total)/1000,1),
                            " tonnes)",sep=""),bty='n')
    
      #LL
    fn.br.plt(dd=dat%>%filter(METHOD=='LL' & zone=="West"),TOP=Top,yMx=1,CX.nm=CX.NM)
    mtext("Longline",3,cex=1)
    legend('bottomright',paste("West (",round(sum(subset(dat,METHOD=='LL'& zone=="West")$Total)/1000,1),
                            " tonnes)",sep=""),bty='n')
    fn.br.plt(dd=dat%>%filter(METHOD=='LL' & zone=="Zone1"),TOP=Top,yMx=1,CX.nm=CX.NM)
    legend('bottomright',paste("Zone1 (",round(sum(subset(dat,METHOD=='LL'& zone=="Zone1")$Total)/1000,1),
                            " tonnes)",sep=""),bty='n')
    fn.br.plt(dd=dat%>%filter(METHOD=='LL' & zone=="Zone2"),TOP=Top,yMx=1,CX.nm=CX.NM)
    legend('bottomright',paste("Zone2 (",round(sum(subset(dat,METHOD=='LL'& zone=="Zone2")$Total)/1000,1),
                            " tonnes)",sep=""),bty='n')
    mtext("Proportion of total catch",1,outer=T,line=0.1,cex=1.2) 
    dev.off()
    
    
    #By zone comparison GN vs LL without targeting vessel
    Snapper.targeting.vessel=NULL
    if(what=="_Daily")
    {
      Snapper.targeting.vessel=ktch%>%
        filter(SPECIES%in%c(353001) & METHOD=="LL" & zone=="West")%>%
        group_by(VESSEL,SNAME)%>%
        summarise(Tot=sum(LIVEWT.c))%>%
        arrange(-Tot)%>%
        ungroup()%>%
        mutate(Cum.tot=cumsum(Tot),
               Cum.tot.per=100*Cum.tot/sum(Tot))%>%
        filter(Cum.tot.per<95)%>%
        pull(VESSEL)
      dat=ktch%>%
        filter(!VESSEL%in%Snapper.targeting.vessel)%>%
        group_by(METHOD,zone,SPECIES,SNAME)%>%
        summarise(Total = sum(LIVEWT.c))
      CX.NM=.7
      Top=15
      fn.fig(paste(hndl,"/Catch_composition/Overall_GN_LL_by zone_without.3.vessels.accounting for 95 percent.catch",what,sep=""),2400,1600)
      par(mfcol=c(3,2),mar=c(2,7,.1,.1),oma=c(1.1,.1,1.1,.3),mgp=c(1.5,.5,0))
      #GN
      fn.br.plt(dd=dat%>%filter(METHOD=='GN' & zone=="West"),TOP=Top,yMx=1,CX.nm=CX.NM)
      mtext("Gillnet",3,cex=1)
      legend('bottomright',paste("West (",round(sum(subset(dat,METHOD=='GN'& zone=="West")$Total)/1000,1),
                                 " tonnes)",sep=""),bty='n')
      fn.br.plt(dd=dat%>%filter(METHOD=='GN' & zone=="Zone1"),TOP=Top,yMx=1,CX.nm=CX.NM)
      legend('bottomright',paste("Zone1 (",round(sum(subset(dat,METHOD=='GN'& zone=="Zone1")$Total)/1000,1),
                                 " tonnes)",sep=""),bty='n')
      fn.br.plt(dd=dat%>%filter(METHOD=='GN' & zone=="Zone2"),TOP=Top,yMx=1,CX.nm=CX.NM)
      legend('bottomright',paste("Zone2 (",round(sum(subset(dat,METHOD=='GN'& zone=="Zone2")$Total)/1000,1),
                                 " tonnes)",sep=""),bty='n')
      
      #LL
      fn.br.plt(dd=dat%>%filter(METHOD=='LL' & zone=="West"),TOP=Top,yMx=1,CX.nm=CX.NM)
      mtext("Longline",3,cex=1)
      legend('bottomright',paste("West (",round(sum(subset(dat,METHOD=='LL'& zone=="West")$Total)/1000,1),
                                 " tonnes)",sep=""),bty='n')
      fn.br.plt(dd=dat%>%filter(METHOD=='LL' & zone=="Zone1"),TOP=Top,yMx=1,CX.nm=CX.NM)
      legend('bottomright',paste("Zone1 (",round(sum(subset(dat,METHOD=='LL'& zone=="Zone1")$Total)/1000,1),
                                 " tonnes)",sep=""),bty='n')
      fn.br.plt(dd=dat%>%filter(METHOD=='LL' & zone=="Zone2"),TOP=Top,yMx=1,CX.nm=CX.NM)
      legend('bottomright',paste("Zone2 (",round(sum(subset(dat,METHOD=='LL'& zone=="Zone2")$Total)/1000,1),
                                 " tonnes)",sep=""),bty='n')
      mtext("Proportion of total catch",1,outer=T,line=0.1,cex=1.2) 
      dev.off()
    }
     
    
    #By similar block-month comparison GN vs LL
    ktch=ktch%>%mutate(BLK={if("block10" %in% names(.)) block10 else BLOCKX})
    
    dat=ktch%>%group_by(METHOD,BLK,MONTH,FINYEAR,SPECIES,SNAME)%>%
      summarise(Total = sum(LIVEWT.c))%>%mutate(dummy=paste(BLK,MONTH,FINYEAR))
    LL.recrds=dat%>%filter(METHOD=="LL")
    GN.recrds=dat%>%filter(METHOD=="GN")
    same.recs=intersect(LL.recrds$dummy, GN.recrds$dummy)
    dat=dat%>%filter(dummy%in%same.recs)
    TAB=as.data.frame.matrix(with(dat,table(dummy,METHOD)))
    same.recs=TAB%>%mutate(dummy=rownames(TAB))%>%filter(GN>=Min.overlap & LL>=Min.overlap)%>%.$dummy
    dat=dat%>%filter(dummy%in%same.recs)
    
    unIk=sort(unique(dat$dummy))
    
    Top=10
    CX.NM=.7*4/length(unIk)
    fn.fig(paste(hndl,"/Catch_composition/Overall_GN_LL_same_blk_yr_mn",what,sep=""),2400,1600)
    par(mfcol=c(length(unIk),2),mar=c(2,7,.1,.1),oma=c(1,.1,1.1,.4),mgp=c(1.5,.5,0))
    #GN    
    for(u in 1:length(unIk))
    {
      fn.br.plt(dd=dat%>%filter(METHOD=='GN' & dummy==unIk[u]),TOP=Top,yMx=1,CX.nm=CX.NM,Xmax=0.8)
      if(u==1)mtext("Gillnet",3,cex=1)
      legend('bottomright',paste(unIk[u]," (",round(sum(subset(dat,METHOD=='GN'& dummy==unIk[u])$Total)/1000,1),
                              " tonnes)",sep=""),bty='n',cex=.8)
    }
    #LL
    for(u in 1:length(unIk))
    {
      fn.br.plt(dd=dat%>%filter(METHOD=='LL' & dummy==unIk[u]),TOP=Top,yMx=1,CX.nm=CX.NM,Xmax=0.8)
      if(u==1)mtext("Longline",3,cex=1)
      legend('bottomright',paste(unIk[u]," (",round(sum(subset(dat,METHOD=='LL'& dummy==unIk[u])$Total)/1000,1),
                              " tonnes)",sep=""),bty='n',cex=.8)
    }
    mtext("Proportion of total catch",1,outer=T,line=0,cex=1) 
    dev.off()
    
    return(list(Snapper.targeting.vessel=Snapper.targeting.vessel,
                same.BLK.MONTH.FINYEAR=unIk))
  }
      #daily
  out.daily=fn.ktch.comp(ktch=Data.daily%>%filter(METHOD%in%c('GN',"LL"))%>%
                  dplyr::select(Same.return.SNo,VESSEL,FINYEAR,METHOD,zone,block10,BLOCKX,SPECIES,SNAME,LIVEWT.c,MONTH),
               what="_Daily",
               Min.overlap=10)
      #monthly
  if(do.monthly) out.monthly=fn.ktch.comp(ktch=Data.monthly%>%filter(METHOD%in%c('GN',"LL"))%>%
                    dplyr::select(Same.return,VESSEL,FINYEAR,METHOD,zone,BLOCKX,SPECIES,SNAME,LIVEWT.c,MONTH),
                           what="_Monthly",
                           Min.overlap=18)
              
    #3.2. Multivariate stats
      #Monthly
  if(do.monthly)
  {
    system.time({
      Out.multi_monthly=multivariate.fn(d=Data.monthly%>%filter(YEAR<=2005 & !zone=='Zone1'),
                                        Terms=c('method.zone','finyear'),
                                        Def.sp.term=c('method','species'),
                                        Transf='proportion',
                                        Show.term='method.zone',
                                        Group='Top20',
                                        hndl,
                                        MDS.title="/Catch_composition/MDS_monthly",
                                        Simper.title="/Catch_composition/Simper_monthly",
                                        Permanova.title="/Catch_composition/Permanova_monthly")
    })
  }

      #Daily
  system.time({
    Out.multi_daily=multivariate.fn(d=Data.daily%>%
                                      filter(!zone=='Zone1'),
                                   Terms=c('method.zone','finyear','month'),
                                   Def.sp.term=c('method','species'),
                                   Transf='proportion',
                                   Show.term='method.zone',
                                   Group='Top20',
                                   hndl,
                                   MDS.title="/Catch_composition/MDS_daily",
                                   Simper.title="/Catch_composition/Simper_daily",
                                   Permanova.title="/Catch_composition/Permanova_daily")
  }) 
  
      #Daily without vessels targeting snapper
  system.time({
    Out.multi_daily_no.snapper.targeting.vessels=multivariate.fn(d=Data.daily%>%
                                                                   filter(!zone=='Zone1' &
                                                                            !VESSEL%in%out.daily$Snapper.targeting.vessel),
                                                                 Terms=c('method.zone','finyear','month'),
                                                                 Def.sp.term=c('method','species'),
                                                                 Transf='proportion',
                                                                 Show.term='method.zone',
                                                                 Group='Top20',
                                                                 hndl,
                                                                 MDS.title="/Catch_composition/MDS_daily_no.snapper.targeting.vessels",
                                                                 Simper.title="/Catch_composition/Simper_daily_no.snapper.targeting.vessels",
                                                                 Permanova.title="/Catch_composition/Permanova_daily_no.snapper.targeting.vessels")
  }) 
  
      #Daily comparable records
  system.time({
    Out.multi_daily_comparable.records=multivariate.fn(d=Data.daily%>%
                                                         mutate(BLK={if("block10" %in% names(.)) block10 else BLOCKX},
                                                                dummy=paste(BLK,MONTH,FINYEAR))%>%
                                                         filter(!zone=='Zone1' &
                                                                  dummy%in%out.daily$same.BLK.MONTH.FINYEAR),
                                                       Terms=c('method.zone','finyear','month'),
                                                       Def.sp.term=c('method','species'),
                                                       Transf='proportion',
                                                       Show.term='method.zone',
                                                       Group='Top20',
                                                       hndl,
                                                       MDS.title="/Catch_composition/MDS_daily_same.blk.mn.yr",
                                                       Simper.title="/Catch_composition/Simper_daily_same.blk.mn.yr",
                                                       Permanova.title="/Catch_composition/Permanova_daily_same.blk.mn.yr")
  })
  
  
  
  #4. Hook characteristics (longline only)
  d=Effort.monthly%>%
    filter(METHOD=="LL")%>%
    dplyr::select(c(Same.return,HOURS.c,HOOKS,SHOTS.c))%>%
    mutate(dat="Monthly")%>%
    distinct(Same.return,.keep_all=T)
  d1=Data.daily.original%>%
    filter(METHOD=="LL")%>%
    dplyr::select(c(DSNo,TSNo,SNo,HOURS,HOOKS,SHOTS,
                    HookSize,HookType))%>%
    mutate(dat="Daily",Same.return.SNo=paste(DSNo,TSNo,SNo))%>%
    distinct(Same.return.SNo,.keep_all=T)
  colnames(d)=tolower(colnames(d))
  colnames(d1)=tolower(colnames(d1))
  d1=d1%>%
    mutate(hours.c=hours,
           shots.c=shots,
           hooksize=as.numeric(hooksize),
           hooktype=case_when(hooktype%in%c("Ezi-Baiter")~"Ezi-baiter",
                              hooktype%in%c("Tuna-Circle","tuna circle")~"Tuna-circle"))
  
  fn.fig(paste(hndl,"/longline only/longline.number of hooks_daily only",sep=""),2400,1600)
  rbind(d%>%dplyr::select(dat,hooks),d1%>%dplyr::select(dat,hooks)) %>% 
    gather(key=dat, value=hooks) %>% 
    filter(dat=="Daily")%>%
    ggplot(aes(x=hooks,fill=dat)) +
    geom_histogram(position="dodge",binwidth=100)+
    scale_fill_manual(values=c("darksalmon", "steelblue"))+
    xlab('Number of hooks per session')+ylab('Frequency')+ guides(fill=guide_legend(title="Data set"))+
    theme_grey(base_size = 22)+
    theme(legend.position="none")
  dev.off()
  
  fn.fig(paste(hndl,"/longline only/longline_gillnet.soak times_daily only",sep=""),2400,1600)
  d1.GN=Data.daily.original%>%
    filter(METHOD=="GN")%>%
    dplyr::select(c(DSNo,TSNo,SNo,HOURS))%>%
    mutate(dat="Daily",Same.return.SNo=paste(DSNo,TSNo,SNo))%>%
    distinct(Same.return.SNo,.keep_all=T)%>%
    mutate(hours.c=HOURS)
  colnames(d1.GN)=tolower(colnames(d1.GN))
  
  rbind(d1%>%dplyr::select(dat,hours.c)%>%mutate(method='LL'),
        d1.GN%>%dplyr::select(dat,hours.c)%>%mutate(method='GN'))%>%
    ggplot(aes(x=hours.c,fill=method)) +
    facet_wrap(~method, scales="free_y")+
    geom_histogram(position="dodge",binwidth=1)+
    scale_fill_manual(values=c("#F8766D", "#00BFC4"))+
    xlab('Soak hours per session')+ylab('Frequency')+ guides(fill=guide_legend(title="Data set"))+
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 16),
          strip.text.x = element_text(size = 12),
          legend.position="none")
  dev.off()
  
  fn.fig(paste(hndl,"/longline only/longline.hook size_daily only",sep=""),2400,1600)
  subset(d1,hooksize>0) %>%
    ggplot(aes(x=hooksize)) + 
    geom_histogram(position="dodge",binwidth=1,fill="darksalmon")+
    xlab('Hook size')+ylab('Frequency')+
    scale_x_continuous(breaks=6:15,
                       labels=6:15)+
    theme_grey(base_size = 22)+
    theme(legend.title=element_blank())
  dev.off()
  
  fn.fig(paste(hndl,"/longline only/longline.hook type_daily only",sep=""),2800,1600)
  ggplot(subset(d1,!is.na(hooktype)|hooktype==''), aes(hooktype)) + geom_bar(fill="darksalmon")+
    theme_grey(base_size = 22)+ylab('Frequency')+xlab('Hook type')
  dev.off()
  
  
  
  #5. Catch rates 
    #5.1 Catch rates by  method for main species
  #note:this has positive and 0 catch records
  fn.catchrate=function(d1,d2)
  {
    main.sp=rbind(d1%>%dplyr::select(species,livewt),d2%>%dplyr::select(species,livewt))%>%
      group_by(species)%>%
      summarise(Tot=sum(livewt,na.rm=T))%>%
      arrange(-Tot)%>%
      mutate(Cum=cumsum(Tot),
             Per=100*Cum/sum(Tot))%>%
      filter(Per<90)%>%pull(species)
    
    main.sp=c(main.sp,c(353001,320000,377004))
    main.sp=unique(main.sp)
    main.sp=as.numeric(names(Main.species))   #just stick to these ones for consistency
    d3=rbind(d1%>%filter(species%in%main.sp),
             d2%>%filter(species%in%main.sp))%>%
      filter(!is.na(cpue)|!cpue==Inf)%>%
      mutate(cpue1=ifelse(method=="GN",cpue*1000,
                   ifelse(method=="LL",cpue*1000,
                   NA)))%>%
      dplyr::select(Same.return.SNo,SNAME,cpue1,zone,method,VESSEL,
                    depthMax,HookType,HookSize)%>%
      spread(SNAME,cpue1,fill=0)%>%
      gather(key = "SNAME", value = "cpue1", -c(zone,method,Same.return.SNo,VESSEL,
                                                depthMax,HookType,HookSize))
    
    d3=d3%>%left_join(d2%>%
                        distinct(SNAME,species)%>%
                        mutate(colr=ifelse(species<50000,"steelblue","firebrick")),
                      by='SNAME')
    d3=subset(d3,!is.infinite(cpue1))
    upper=quantile(d3$cpue1,probs=.99)  
    #By zone
    d3=d3%>%
      mutate(zone=ifelse(zone=='1','zone 1',ifelse(zone=='2','zone 2',zone)),
             method.zone=paste(method,zone))
    
    p=d3%>%
      ggplot(aes(x=SNAME, y=cpue1)) + 
      geom_boxplot(outlier.size = 0.1,aes(fill=colr))+ coord_flip()+
      facet_wrap(~method.zone)+ scale_y_continuous(limits = c(0, upper))+
      scale_fill_manual(values=c("firebrick","steelblue"))+
      ylab("Catch rate per session (kg per 1000 m hour / 1000 hook hour)")+xlab("")+ 
      theme(legend.position="none",
            axis.text = element_text(size = 12),
            strip.text.x = element_text(size = 12))
    print(p)
    
    return(d3)
  }
  
  #Daily
    #a. boxplot of observed cpues
  add.nms=Data.daily%>%distinct(SPECIES,SNAME)%>%distinct(SPECIES,.keep_all=T)
  Data.daily.original=Data.daily.original%>%
                      mutate(HookType=ifelse(HookType%in%c('Tuna-Circle','tuna circle','circle'),'Tuna-circle',
                                    ifelse(HookType%in%c('Ezi-Baiter'),'Ezi-baiter',
                                    ifelse(HookType%in%c(""),NA,
                                    HookType))),
                              HookSize=ifelse(HookSize%in%c("",".","0"),NA,HookSize),
                              HookSize=as.numeric(HookSize))
  d1=Data.daily.original%>%
    filter(METHOD=="LL" & !is.na(species) &!is.na(HOOKS) & !is.na(HOURS))%>%
    mutate(Same.return.SNo=paste(DSNo,TSNo,SNo),
           zone=case_when(zone=='*'~'west',
                          TRUE~zone))%>%
    dplyr::select(c(Same.return.SNo,HOURS,HOOKS,VESSEL,
                    livewt,species,zone,depthMax,HookType,HookSize))%>%
    group_by(Same.return.SNo,species,zone,VESSEL,depthMax,HookType,HookSize)%>%
    summarise(livewt=sum(livewt),
              HOURS=max(HOURS),
              HOOKS=max(HOOKS))%>%
    mutate(cpue=livewt/(HOURS*HOOKS))%>%
    left_join(add.nms,by=c("species"='SPECIES'))%>%
    mutate(colr=ifelse(species<50000,"steelblue","firebrick"),
           SNAME=case_when(species==18013~"Spot-tail shark",
                           species==18014~"Blacktip sharks",
                           species==18029~"Lemon shark",
                           species==18026~"Pigeye shark",
                           species==18030~"Grey reef shark",
                           TRUE~SNAME),
           method='LL')%>%
    dplyr::select(-c(HOURS,HOOKS))%>%
    ungroup()
  
  d2=Data.daily.original%>%
    filter(METHOD=="GN" & !is.na(species) &!is.na(NETLEN) & !is.na(HOURS))%>%
    mutate(Same.return.SNo=paste(DSNo,TSNo,SNo),
           zone=case_when(zone=='*'~'west',
                          TRUE~zone))%>%
    dplyr::select(Same.return.SNo,HOURS,VESSEL,
                  NETLEN,livewt,species,zone,depthMax,HookType,HookSize)%>%
    group_by(Same.return.SNo,species,zone,VESSEL,depthMax,HookType,HookSize)%>%
    summarise(livewt=sum(livewt),
              HOURS=max(HOURS),
              NETLEN=max(NETLEN))%>%
    mutate(cpue=livewt/(HOURS*NETLEN))%>%
    left_join(add.nms,by=c("species"='SPECIES'))%>%
    mutate(colr=ifelse(species<50000,"steelblue","firebrick"),
           method='GN')%>%
    dplyr::select(-c(HOURS,NETLEN))%>%
    ungroup()
  
  d1=d1[match(names(d2),names(d1))]
  
  fn.fig(paste(hndl,"/catch_rates/Catch rates by method and zone for main species_daily",sep=""),2800,1800)
  daily.cpue=fn.catchrate(d1,d2)
  dev.off()
  
    #b. standardization
  names(daily.cpue)=tolower(names(daily.cpue))
  daily.cpue=daily.cpue%>%
    filter(!is.na(depthmax) & depthmax<200)%>%
    filter(!zone=='zone 1')
  
  daily.cpue.GN=daily.cpue%>%
                filter(method=='GN')%>%
                mutate(zone=as.factor(zone),
                       method=as.factor(method),
                       vessel=as.factor(vessel))
  
  daily.cpue.LL=daily.cpue%>%
                filter(method=='LL')%>%
                mutate(zone=as.factor(zone),
                       method=as.factor(method),
                       vessel=as.factor(vessel),
                       hooktype=as.factor(hooktype))

      #b.1 fit GAM
  cpue.stand.out.GN=vector('list',length(main.sp))
  names(cpue.stand.out.GN)=main.sp
  cpue.stand.out.LL=cpue.stand.out.GN
  system.time({
    for(m in 1:length(main.sp))
    {
      cpue.stand.out.GN[[m]]=cpue.stand.fun(d=daily.cpue.GN%>%filter(species==main.sp[m]),
                                            Formula=formula(cpue1~zone+s(depthmax)+s(vessel,bs='re')))  
      print(paste(main.sp[m],'GN GAM-------------'))
      
      D=daily.cpue.LL%>%
        filter(species==main.sp[m] & hooksize>11)%>%
        filter(!is.na(hooktype))%>%
        mutate(hooksize=as.factor(hooksize))
      cpue.stand.out.LL[[m]]=cpue.stand.fun(d=D,
                                            Formula=formula(cpue1~zone+s(depthmax)+hooksize+hooktype+s(vessel,bs='re')))  
      print(paste(main.sp[m],'LL GAM-------------'))
    }
  })   #takes 5 minuts
  #gam.check(cpue.stand.out[[m]])
  
      #b.2 export anova table
  for(m in 1:length(main.sp))
  {
    NM=Main.species[match(main.sp[m],names(Main.species))]
    
    #GN
    ft <- as_flextable(cpue.stand.out.GN[[m]]$mod)
    save_as_image(ft, path = paste(hndl,'catch_rates/GAM_Anova',paste(NM,'.GN.png',sep=''),sep='/'))
    save_as_docx(ft,path = paste(hndl,'catch_rates/GAM_Anova/',paste(NM,'.GN.docx',sep=''),sep='/'))
    rm(ft)
    
    #LL
    ft <- as_flextable(cpue.stand.out.LL[[m]]$mod)
    save_as_image(ft, path = paste(hndl,'catch_rates/GAM_Anova',paste(NM,'.LL.png',sep=''),sep='/'))
    save_as_docx(ft,path = paste(hndl,'catch_rates/GAM_Anova/',paste(NM,'.LL.docx',sep=''),sep='/'))
    
    rm(ft,NM)
  }
  
      #b.3 display term effect
  Store.zone.method=vector('list',length(main.sp))
  names(Store.zone.method)=main.sp
  Store.hook.type=Store.hook.size=Store.depth=Store.zone.method
  
  Get.hook.type.Pred=c(18003,353001) #only significant for these species
  Get.hook.size.Pred=c(18007,320000)
  for(m in 1:length(main.sp))
  {
    SP=Main.species[match(main.sp[m],names(Main.species))]
      
    #zone-method effect                        
    GN=pred.fun(mod=cpue.stand.out.GN[[m]]$mod,biascor="NO",PRED=c('zone'))%>%
              mutate(method="GN",
                     species=SP)
    LL=pred.fun(mod=cpue.stand.out.LL[[m]]$mod,biascor="NO",PRED=c('zone'))%>%
              mutate(method="LL",
                     species=SP)
    Store.zone.method[[m]]=rbind(GN,LL)
    
    #depth effect
    GN=pred.fun.continuous(d=cpue.stand.out.GN[[m]]$dat,
                           mod=cpue.stand.out.GN[[m]]$mod,
                           PRED='depthmax',
                           Formula=formula(cpue1~zone+s(depthmax)+s(vessel,bs='re')))%>%
              mutate(method="GN",
                     species=SP)
    LL=pred.fun.continuous(d=cpue.stand.out.LL[[m]]$dat,
                           mod=cpue.stand.out.LL[[m]]$mod,
                           PRED='depthmax',
                           Formula=formula(cpue1~zone+s(depthmax)+hooksize+hooktype+s(vessel,bs='re')))%>%
              mutate(method="LL",
                     species=SP)
    Store.depth[[m]]=rbind(GN,LL)
    
    #Hook type
    if(main.sp[m]%in%Get.hook.type.Pred)
    {
      Store.hook.type[[m]]=pred.fun(mod=cpue.stand.out.LL[[m]]$mod,biascor="NO",PRED=c('hooktype'))%>%
                              mutate(species=SP)
    }
    
    #Hook size
    if(main.sp[m]%in%Get.hook.size.Pred)
    {
      Store.hook.size[[m]]=pred.fun(mod=cpue.stand.out.LL[[m]]$mod,biascor="NO",PRED=c('hooksize'))%>%
                              mutate(species=SP)
    }
    
  }
  
  fn.barplot.cpue=function(d,YLAB,XLAB,cex)
  {
    p=d%>%
      ggplot(aes(x,y,fill=fill))+
      geom_bar(stat="identity")+
      geom_errorbar(aes(x, ymin=lower.CL, ymax=upper.CL),colour='black',width=0.25)+
      facet_wrap(~facet,scales='free',nrow=2)+
      theme(legend.position='none',
            legend.title=element_text(size=cex),
            strip.text.x = element_text(size = cex-2),
            legend.text=element_text(size=cex-2),
            axis.text=element_text(size=cex-4),
            axis.title=element_text(size=cex))+
      ylab(YLAB)+xlab(XLAB)
    print(p)
  }
  #Method-zone
  fn.fig(paste(hndl,"/catch_rates/Standardized_Catch rates_Method.zone_daily",sep=""),2400,1400)
  fn.barplot.cpue(d=do.call(rbind,Store.zone.method)%>%
                    mutate(method.zone=paste(method,zone,sep='-'),
                           species=factor(species,levels=Main.species),
                           x=method.zone,
                           y=response,
                           fill=method.zone,
                           facet=species),
                  YLAB="Standardized catch rate (kg per 1000 m hour / 1000 hook hour)",
                  XLAB='Fishing method and zone',
                  cex=10)
  dev.off()
  
  #Depth
  do.call(rbind,Store.depth)
  
  #Hooksize
  fn.fig(paste(hndl,"/catch_rates/Standardized_Catch rates_hooksize_daily",sep=""),2400,2400)
  fn.barplot.cpue(d=do.call(rbind,Store.hook.size)%>%
                    mutate(species=factor(species,levels=Main.species),
                           x=hooksize,
                           y=response,
                           fill=hooksize,
                           facet=species),
                  YLAB="Standardized catch rate (kg per 1000 m hour / 1000 hook hour)",
                  XLAB='Hook size',
                  cex=14)
  dev.off()
  
  
  #Hooktype
  fn.fig(paste(hndl,"/catch_rates/Standardized_Catch rates_hooktype_daily",sep=""),2400,2400)
  fn.barplot.cpue(d=do.call(rbind,Store.hook.type)%>%
                    mutate(species=factor(species,levels=Main.species),
                           x=hooktype,
                           y=response,
                           fill=hooktype,
                           facet=species),
                  YLAB="Standardized catch rate (kg per 1000 m hour / 1000 hook hour)",
                  XLAB='Hook type',
                  cex=14)
  dev.off()
  
  
  #ACA
  
  #Monthly
  if(do.monthly)
  {
    add.nms=Data.monthly%>%
      distinct(SPECIES,SNAME)%>%
      mutate(SNAME=case_when(SPECIES==18013~"Spot-tail shark",
                             SPECIES==18014~"Blacktip sharks",
                             SPECIES==18029~"Lemon shark",
                             SPECIES==18026~"Pigeye shark",
                             SPECIES==18030~"Grey reef shark",
                             TRUE~SNAME))%>%
      distinct(SPECIES,.keep_all=T)
    
    
    d=Data.monthly%>%left_join(Effort.monthly%>%
                                 mutate(Effort.net=1000*Km.Gillnet.Hours.c,
                                        Effort.hook=BDAYS*HOURS*HOOKS)%>%
                                 group_by(Same.return)%>%
                                 summarise(Effort.net=max(Effort.net),
                                           Effort.hook=max(Effort.hook)),
                               by="Same.return")%>%
      mutate(zone=tolower(zone))
    d1=d%>%
      filter(METHOD=="LL" & !is.na(SPECIES) &!is.na(Effort.hook))%>%
      group_by(Same.return,SPECIES,zone)%>%
      summarise(livewt=sum(LIVEWT.c),
                Effort=max(Effort.hook))%>%
      mutate(cpue=livewt/(Effort))%>%
      left_join(add.nms,by="SPECIES")%>%
      mutate(colr=ifelse(SPECIES<50000,"steelblue","firebrick"),
             method='LL')%>%
      ungroup()%>%
      rename(species=SPECIES,
             Same.return.SNo=Same.return)
    
    d2=d%>%
      filter(METHOD=="GN" & !is.na(SPECIES) &!is.na(Effort.net))%>%
      group_by(Same.return,SPECIES,zone)%>%
      summarise(livewt=sum(LIVEWT.c),
                Effort=max(Effort.net))%>%
      mutate(cpue=livewt/(Effort))%>%
      left_join(add.nms,by="SPECIES")%>%
      mutate(colr=ifelse(SPECIES<50000,"steelblue","firebrick"),
             method='GN')%>%
      ungroup()%>%
      rename(species=SPECIES,
             Same.return.SNo=Same.return)
    
    fn.fig(paste(hndl,"/Catch rates by method and zone for main species_monthly",sep=""),2800,1800)
    fn.catchrate(d1,d2)
    dev.off()
    rm(d,d1,d2)
  }
  

   
  #6. Average catch price per vessel-gear for last five years
  Lst.fiv.yrs=as.numeric(substr(Current.yr,1,4))
  Lst.fiv.yrs=seq(Lst.fiv.yrs-4,Lst.fiv.yrs)
  Lst.fiv.yrs=paste(Lst.fiv.yrs,substr(Lst.fiv.yrs+1,3,4),sep='-')
  a=Data.daily%>%
    filter(FINYEAR%in%Lst.fiv.yrs & METHOD%in%c("GN","LL"))%>%
    group_by(Same.return.SNo,VESSEL,SPECIES,METHOD)%>%
    summarise(Total.ktch = sum(LIVEWT.c))
  setwd(handl_OneDrive("Analyses/Data_outs"))
  PRICES=read.csv("PRICES.csv",stringsAsFactors = F)
  PRICES=PRICES%>%mutate(dolar.per.kg=PRICES[,match('Processor.Weighted.Average.Price',names(PRICES))],
                         dolar.per.kg=as.numeric(gsub("\\$", "", dolar.per.kg)))%>%
    mutate(SPECIES=as.numeric(ASA.Species.Code))
  a=left_join(a,PRICES,by="SPECIES")%>%
    mutate(ktch.price=dolar.per.kg*Total.ktch)%>%
    group_by(Same.return.SNo,VESSEL,METHOD)%>%
    summarise(Catch.value=sum(ktch.price))
  
  # b=Effort.daily%>%filter(finyear%in%Lst.fiv.yrs & method%in%c("GN","LL"))%>%
  #   group_by(Same.return.SNo,vessel,method)%>%
  #   summarise(Effort = ifelse(method=="GN",max(Km.Gillnet.Hours.c),NA))
  
  # d=left_join(a,b,by=c("Same.return.SNo"))%>%
  #   group_by(vessel)%>%
  #   summarise(average.ktch.value=mean(Catch.value,na.rm=T),
  #             min.ktch.value=min(Catch.value,na.rm=T),
  #             max.ktch.value=max(Catch.value,na.rm=T),
  #             average.effort=mean(Effort,na.rm=T))%>%
  #   data.frame
  
  fn.fig(paste(hndl,"/Catch value per session per vessel for last 5 years",sep=""),2800,1600)
   ggplot(a, aes(x=VESSEL, y=Catch.value,colour=METHOD)) + 
    geom_boxplot()+ coord_flip()+
    scale_x_discrete(breaks=d$VESSEL,labels=1:nrow(d))+
    ylab("Catch value ($) per session")+ 
    theme(legend.position="top",
          legend.title=element_blank())
  dev.off()
  
  
  #7. Tim Nicholas request
  do.Tims=FALSE
  if(do.Tims)
  {
    South.WA.lat=c(-36,-25); South.WA.long=c(112,130)
    PLATE=c(.01,.9,.075,.9)
    Yrs=c("2017-18","2018-19")  
    aa= Data.daily.original%>%filter(FINYEAR%in%Yrs) %>%
      mutate(LatDeg=as.numeric(substr(block10,1,2)),
             LatMin=ifelse(is.na(LatMin),10*as.numeric(substr(block10,3,3)),LatMin),
             Lat=-abs(LatDeg+(LatMin/60)),
             LongDeg=ifelse(is.na(LongDeg),100+as.numeric(substr(block10,4,5)),LongDeg), 
             LongMin=ifelse(is.na(LongMin),10*as.numeric(substr(block10,6,6)),LongMin),
             Long=LongDeg+(LongMin/60))%>%
      filter(Lat<=(-26) & Lat>(-36.5)& Long<=(129) & Long >(111.9))
    
    numInt=20
    couleurs=rev(heat.colors(numInt)) 
    tcl.1=.5
    tcl.2=.5
    Long.seq=seq(South.WA.long[1]+1,South.WA.long[2]-1,by=3)
    Lat.seq=c(-26,-28,-30,-32,-34)
    numberLab=5
    colLeg=(rep(c("black",rep("transparent",numberLab-1)),(numInt+1)/numberLab))
    
    #lodged returns
    TAB1=aa %>% filter(FINYEAR%in%Yrs) %>%
      group_by(FINYEAR)%>%
      summarise(Unique_TSNo=n_distinct(TSNo))%>%
      data.frame
    
    TAB2=aa %>% filter(FINYEAR%in%Yrs) %>%
      group_by(FINYEAR,METHOD)%>%
      summarise(Unique_TSNo=n_distinct(TSNo))%>%
      data.frame
    
    #Who's been using hooks?
    TAB3_trips=aa%>%group_by(METHOD,FINYEAR,VESSEL)%>%
      summarise(Trips=n_distinct(TSNo))%>%
      spread(METHOD, Trips,fill=0)%>%
      data.frame
    
    
    bb=aa%>%filter(METHOD=="LL")%>%
      distinct(Same.return.SNo,.keep_all =T) %>%
      dplyr::select(VESSEL,BoatName,MastersName,port,block10,FINYEAR,MONTH,bioregion,Lat,Long,depthMax,
                    NilCatch,species,nfish,livewt,
                    HookSize,HookType,HOOKS,HOURS,nlines,SHOTS)
    
    TAB4=bb%>%group_by(VESSEL,BoatName,MastersName,port)%>%
      summarise(mean.hook.n=mean(HOOKS,na.rm=T),
                mean.hook.size=mean(HookSize,na.rm=T),
                mean.hook.hours=mean(HOURS,na.rm=T))%>%
      replace(is.na(.), "")%>%
      data.frame
    mytheme <- gridExtra::ttheme_default(
      base_size = 10,
      core = list(padding=unit(c(1, 1), "mm"),fg_params=list(cex = .65)),
      colhead = list(fg_params=list(cex = .75)),
      rowhead = list(fg_params=list(cex = .75)))
    
    pdf(file=paste(hndl,"/Parks_Australia_2018-19.effort_catch.pdf",sep=""))
    
    grid.draw(gridExtra::tableGrob(TAB3_trips, theme = mytheme,rows = NULL))
    grid.newpage()
    
    grid.draw(gridExtra::tableGrob(TAB4, theme = mytheme,rows = NULL))
    
    #effort
    b=aa %>% filter(METHOD=="GN") %>%
      mutate(Km.Gillnet.Hours=HOURS*NETLEN/1000)%>%
      filter(Km.Gillnet.Hours>0)%>%
      group_by(Same.return.SNo,FINYEAR, block10) %>%
      summarize(Km.Gillnet.Hours = max(Km.Gillnet.Hours, na.rm = TRUE))%>%
      group_by(FINYEAR, block10) %>%
      summarize(sum = sum(Km.Gillnet.Hours, na.rm = TRUE))%>%
      mutate(LatDeg=as.numeric(substr(block10,1,2)),
             LatMin=10*as.numeric(substr(block10,3,3)),
             Lat=-abs(LatDeg+(LatMin/60)),
             LongDeg=100+as.numeric(substr(block10,4,5)), 
             LongMin=10*as.numeric(substr(block10,6,6)),
             Long=LongDeg+(LongMin/60))%>%
      data.frame
    b=subset(b,select=c(FINYEAR,sum,Lat,Long))
    BREAKS=quantile(b$sum,probs=seq(0,1,1/numInt),na.rm=T)
    
    par(mfrow=c(2,1),mai = c(0.3, 0.4, 0.15, 0.2),oma = c(0.5, 0.4, 0.2, 0.1),mgp=c(.1, 0.15, 0))
    for(y in 1:length(Yrs))
    {
      bb=subset(b,FINYEAR==Yrs[y],select=-FINYEAR)%>%
        arrange(Lat,Long)
      long=sort(unique(bb$Long))
      lat=sort(unique(bb$Lat))      
      Reshaped=as.matrix(reshape(bb,idvar="Long",timevar="Lat",v.names="sum", direction="wide"))	
      Reshaped=Reshaped[order(Reshaped[,1]),]
      Reshaped=Reshaped[,-1]	
      
      a=South.WA.long[1]:South.WA.long[2]
      bx=seq(South.WA.lat[1],South.WA.lat[2],length.out=length(a))
      plotmap(a,bx,PLATE,"transparent",South.WA.long,South.WA.lat)
      image(long,lat,z=Reshaped,xlab="",ylab="",col =couleurs,breaks=BREAKS,axes = FALSE,add=T)			
      axis(side = 1, at =South.WA.long[1]:South.WA.long[2], labels = F, tcl = tcl.1)
      axis(side = 2, at = South.WA.lat[2]:South.WA.lat[1], labels = F,tcl =tcl.1)
      axis(side = 1, at =Long.seq, labels = Long.seq, tcl = .35,las=1,cex.axis=1,padj=-.15)
      axis(side = 2, at = Lat.seq, labels = -Lat.seq,tcl = .35,las=2,cex.axis=1,hadj=1.1)
      if(y==2)color.legend(129.5,South.WA.lat[2],South.WA.long[2],-33,round(BREAKS,0),
                           rect.col=couleurs,gradient="y",col=colLeg,cex=0.85)
      nnn=with(TAB2%>%filter(FINYEAR==Yrs[y]),paste(paste(Yrs[y]," (gillnet returns= ",Unique_TSNo[1],"; longline returns= ",Unique_TSNo[2],")",sep="")))
      mtext(nnn,3,-2)
    }
    mtext(expression(paste("Latitude (",degree,"S)",sep="")),side=2,line=-1,las=3,cex=1.1,outer=T)
    mtext(expression(paste("Longitude (",degree,"E)",sep="")),side=1,line=-.5,cex=1.1,outer=T)
    mtext("Effort (Km.gn.hours)",3,-0.75,outer=T)
    
    #catch
    for(s in 1:length(TARGETS))
    {
      b=aa %>% filter(METHOD=="GN" & species%in%TARGETS[[s]]) %>%
        group_by(FINYEAR, block10) %>%
        summarize(sum = sum(livewt, na.rm = TRUE))%>%
        mutate(LatDeg=as.numeric(substr(block10,1,2)),
               LatMin=10*as.numeric(substr(block10,3,3)),
               Lat=-abs(LatDeg+(LatMin/60)),
               LongDeg=100+as.numeric(substr(block10,4,5)), 
               LongMin=10*as.numeric(substr(block10,6,6)),
               Long=LongDeg+(LongMin/60))%>%
        data.frame
      b=subset(b,select=c(FINYEAR,sum,Lat,Long))
      BREAKS=quantile(b$sum,probs=seq(0,1,1/numInt),na.rm=T)
      
      par(mfrow=c(2,1),mai = c(0.3, 0.4, 0.15, 0.2),oma = c(0.5, 0.4, 0.2, 0.1),mgp=c(.1, 0.15, 0))
      for(y in 1:length(Yrs))
      {
        bb=subset(b,FINYEAR==Yrs[y],select=-FINYEAR)%>%
          arrange(Lat,Long)
        long=sort(unique(bb$Long))
        lat=sort(unique(bb$Lat))      
        Reshaped=as.matrix(reshape(bb,idvar="Long",timevar="Lat",v.names="sum", direction="wide"))	
        Reshaped=Reshaped[order(Reshaped[,1]),]
        Reshaped=Reshaped[,-1]	
        
        a=South.WA.long[1]:South.WA.long[2]
        bx=seq(South.WA.lat[1],South.WA.lat[2],length.out=length(a))
        plotmap(a,bx,PLATE,"transparent",South.WA.long,South.WA.lat)
        image(long,lat,z=Reshaped,xlab="",ylab="",col =couleurs,breaks=BREAKS,axes = FALSE,add=T)			
        axis(side = 1, at =South.WA.long[1]:South.WA.long[2], labels = F, tcl = tcl.1)
        axis(side = 2, at = South.WA.lat[2]:South.WA.lat[1], labels = F,tcl =tcl.1)
        axis(side = 1, at =Long.seq, labels = Long.seq, tcl = .35,las=1,cex.axis=1,padj=-.15)
        axis(side = 2, at = Lat.seq, labels = -Lat.seq,tcl = .35,las=2,cex.axis=1,hadj=1.1)
        if(y==2)color.legend(129.5,South.WA.lat[2],South.WA.long[2],-33,round(BREAKS,0),
                             rect.col=couleurs,gradient="y",col=colLeg,cex=0.85)
        mtext(Yrs[y],3,-2)
      }
      mtext(expression(paste("Latitude (",degree,"S)",sep="")),side=2,line=-1,las=3,cex=1.1,outer=T)
      mtext(expression(paste("Longitude (",degree,"E)",sep="")),side=1,line=-.5,cex=1.1,outer=T)
      mtext(paste(names(TARGETS)[s],"(catch in kg)"),3,-.75,outer=T)
      
      
    }
    
    #all scalefish
    {
      b=aa %>% filter(METHOD=="GN" & species%in%Scalefish.species) %>%
        group_by(FINYEAR, block10) %>%
        summarize(sum = sum(livewt, na.rm = TRUE))%>%
        mutate(LatDeg=as.numeric(substr(block10,1,2)),
               LatMin=10*as.numeric(substr(block10,3,3)),
               Lat=-abs(LatDeg+(LatMin/60)),
               LongDeg=100+as.numeric(substr(block10,4,5)), 
               LongMin=10*as.numeric(substr(block10,6,6)),
               Long=LongDeg+(LongMin/60))%>%
        data.frame
      b=subset(b,select=c(FINYEAR,sum,Lat,Long))
      BREAKS=quantile(b$sum,probs=seq(0,1,1/numInt),na.rm=T)
      
      par(mfrow=c(2,1),mai = c(0.3, 0.4, 0.15, 0.2),oma = c(0.5, 0.4, 0.2, 0.1),mgp=c(.1, 0.15, 0))
      for(y in 1:length(Yrs))
      {
        bb=subset(b,FINYEAR==Yrs[y],select=-FINYEAR)%>%
          arrange(Lat,Long)
        long=sort(unique(bb$Long))
        lat=sort(unique(bb$Lat))      
        Reshaped=as.matrix(reshape(bb,idvar="Long",timevar="Lat",v.names="sum", direction="wide"))	
        Reshaped=Reshaped[order(Reshaped[,1]),]
        Reshaped=Reshaped[,-1]	
        
        a=South.WA.long[1]:South.WA.long[2]
        bx=seq(South.WA.lat[1],South.WA.lat[2],length.out=length(a))
        plotmap(a,bx,PLATE,"transparent",South.WA.long,South.WA.lat)
        image(long,lat,z=Reshaped,xlab="",ylab="",col =couleurs,breaks=BREAKS,axes = FALSE,add=T)			
        axis(side = 1, at =South.WA.long[1]:South.WA.long[2], labels = F, tcl = tcl.1)
        axis(side = 2, at = South.WA.lat[2]:South.WA.lat[1], labels = F,tcl =tcl.1)
        axis(side = 1, at =Long.seq, labels = Long.seq, tcl = .35,las=1,cex.axis=1,padj=-.15)
        axis(side = 2, at = Lat.seq, labels = -Lat.seq,tcl = .35,las=2,cex.axis=1,hadj=1.1)
        if(y==2)color.legend(129.5,South.WA.lat[2],South.WA.long[2],-33,round(BREAKS,0),
                             rect.col=couleurs,gradient="y",col=colLeg,cex=0.85)
        mtext(Yrs[y],3,-2)
      }
      mtext(expression(paste("Latitude (",degree,"S)",sep="")),side=2,line=-1,las=3,cex=1.1,outer=T)
      mtext(expression(paste("Longitude (",degree,"E)",sep="")),side=1,line=-.5,cex=1.1,outer=T)
      mtext("Scalefish (catch in kg)",3,-.75,outer=T)
      
      
    }
    dev.off()
    
  }
  
}
#---------Manipulate PA observer TEPS ------------
TEPS.code_contact=data.frame(contact.code=c('WWC','BFC','WER','WEN','WDNN','WDNF','WDDN','WDDF'),
                             contact.code.meaning=c('Wildlife on/in water, contact with vessel',
                                                    'Bird flying, contact with vessel or gear',
                                                    'Wildlife entangled in ropes',
                                                    'Wildlife entangled/hooked',
                                                    'Wildlife diving for but not feeding from net/longline',
                                                    'Wildlife diving for and feeding from net/longline',
                                                    'Wildlife diving for but not feeding on discards',
                                                    'Wildlife diving for and feeding on discards'),
                             contact.code.to.complete=c('on/in water, contact with vessel',
                                                    'flying, contact with vessel or gear',
                                                    'entangled in ropes',
                                                    'entangled/hooked',
                                                    'diving for but not feeding from net/longline',
                                                    'feeding from net/longline',
                                                    'diving for but not feeding on discards',
                                                    'feeding on discards'))
TEPS.code_behav=data.frame(sighting.behav=c('INT','IRR','DES','ROW'),
                           sighting.behav.meaning=c('Intensively searching',
                                                    'Irregularly searching',
                                                    'desinterested',
                                                    'Roaming wild'))

TEPS<-TEPS%>%
  rename(sheet_no="Sheet #",
         ves.act="Ves act",
         obs.sector="Obs sector",
         common.name="Common name",
         sighting.period="Period",
         sighting.dist="Dist",
         sighting.count.method="Count method",
         sighting.count="Count",
         sighting.behav="Behav",
         gear.type="Gear type",
         hook.type="Hook type",
         hook.size="Hook size",
         snood.type="Snood type",
         contact.code="Contact code",
         contact.count="Contact count",
         disc.prey="Disc prey",
         steam.away="Steam away")%>%
  mutate(contact.code=ifelse(contact.code=="WEN/ WWC" & sheet_no=='PA0036',NA,contact.code))%>%
  data.frame%>%
  filter(!is.na(contact.code))%>%
  left_join(TEPS.code_contact,by='contact.code')%>%
  left_join(TEPS.code_behav,by='sighting.behav')

TEPS=TEPS%>%
  mutate(common.name=tolower(common.name),
         SP.group=case_when(common.name%in%c("grey nurse shark")~"protected sharks",
                            common.name%in%c("australian sealion")~"marine mammals",
                            TRUE~"other stuff"),
         common.name=case_when(common.name=="australian sealion" ~ "Australian sea-lion",
                               TRUE~common.name),
         common.name=capitalize(common.name))

#function for integer axis labels
integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}


#---------Manipulate PA Observer and underwater cameras data ------------ 
#Observer data  
DATA_PA=DATA%>%
  distinct(sheet_no,.keep_all=TRUE)%>%
  dplyr::select(sheet_no,mid.lat,mid.long,date,day,month,year,
                boat,block,skipper,botdepth,set.time,set.time.end,haul.time,haul.time.end,
                set.time.avg,haul.time.avg,soak.time,method,mesh_size,net_length,n.hooks)

#Tabulate retained or discarded
Retained.tabl=DATA%>%
  mutate(species=toupper(species))%>%
  group_by(species,retainedflag)%>%
  filter(!is.na(retainedflag))%>%
  tally()%>%
  ungroup%>% 
  group_by(species) %>% 
  arrange(desc(n)) %>% 
  slice(1) %>% 
  ungroup()%>%
  data.frame%>%
  left_join(All.species.names,by=c('species'='Species'))%>%
  dplyr::select(Code,retainedflag)%>%
  rename(retained=retainedflag)%>%
  filter(!is.na(Code))

#Combine Interaction with Observation data sheets
Video.net.obs=Video.net.obs%>%
                        mutate(Interaction=case_when(!code%in%c(54079901,11183901,11183902)~'Swim Past'),
                               Escape=NA,
                               Method="Gillnet", 
                               Position="Gillnet",
                               Species=tolower(observation))
if(!is.na(match('code',colnames(Video.net.obs)))) Video.net.obs=Video.net.obs%>%rename(Code=code)
cols.vid.inter=colnames(Video.net.interaction)
add=cols.vid.inter[which(!cols.vid.inter%in%colnames(Video.net.obs))]
empty_df = Video.net.obs[,1:length(add)]
colnames(empty_df)=add
empty_df[,]=NA
Video.net.obs=cbind(Video.net.obs,empty_df)
Video.net.obs=Video.net.obs[,colnames(Video.net.interaction)]
Video.net.interaction=rbind(Video.net.interaction,Video.net.obs)


Video.longline.obs=Video.longline.obs%>%
                        mutate(Interaction=ifelse(Activity=='Passing','Swim Past',NA),
                               Escape=NA,
                               Method="longline", 
                               Position="longline",
                               Species=tolower(Observation))
if(!is.na(match('code',colnames(Video.longline.obs))))
{
  if(!is.na(match('Code',colnames(Video.longline.obs))))
  {
    if(is.na(sum(Video.longline.obs$Code))) Video.longline.obs=Video.longline.obs%>%dplyr::select(-Code)
    Video.longline.obs=Video.longline.obs%>%rename(Code=code)
  }
  
}
cols.vid.inter=colnames(Video.longline.interaction)
add=cols.vid.inter[which(!cols.vid.inter%in%colnames(Video.longline.obs))]
empty_df = Video.longline.obs[,1:length(add)]
colnames(empty_df)=add
empty_df[,]=NA
Video.longline.obs=cbind(Video.longline.obs,empty_df)
Video.longline.obs=Video.longline.obs[,colnames(Video.longline.interaction)]
Video.longline.interaction=rbind(Video.longline.interaction,Video.longline.obs)

  #some manipulations                       
Video.net.interaction=Video.net.interaction%>%
              mutate(SP.group=case_when(Code >=3.7e7 & Code<=3.70241e7 ~"Sharks",
                                        Code >3.7025e7 & Code<=3.7041e7 ~"Rays",
                                        Code >=3.7042e7 & Code<=3.7044e7 ~"Chimaeras",
                                        Code >=3.7046e7 & Code<=3.747e7 ~"Scalefish",
                                        Code >=4.1e+07 & Code<=4.115e+07 ~"Marine mammals",
                                        Code >=4.0e+07 & Code<4.1e+07 ~"Seabirds",
                                        Code >=1.2e7 & Code<3.7e7 ~"Invertebrates",
                                        Code >=1.1e7 & Code<1.2e7 ~"Rock/reef structure",
                                        Code >=5.4e7 & Code<5.49e7 ~"Macroalgae",
                                        Code == 10000910 ~"Sponges"),
                     Period=tolower(Period),
                     Depth=as.numeric(gsub("[^0-9.-]", "", Depth)),
                     sheet_no=sapply( strsplit( OpCode, "_" ), "[", 3),
                     Camera=paste("Camera",sapply( strsplit(OpCode, "_" ), "[", 5)),
                     Escape2=ifelse(Escape=='landed',NA,Escape),
                     Interaction2=ifelse(Interaction%in%grep(paste(c("Caught","caught"),collapse="|"),Interaction, value=TRUE) &
                                         !is.na(Escape2),"Escape",Interaction))%>%
              left_join(DATA_PA,by='sheet_no')%>%
              data.frame

Video.net.interaction=Video.net.interaction%>%
                        left_join(Retained.tabl,by='Code')%>%
                        mutate(Retain.group=
                                 ifelse(retained=='Yes' & SP.group%in% c('Sharks','Rays'),"Retained elasmobranch",
                                 ifelse(retained=='No' & SP.group%in% c('Sharks','Rays'),"Discarded elasmobranch",
                                 ifelse(retained=='Yes' & SP.group%in% c('Scalefish'),"Retained scalefish",   
                                 ifelse(retained=='No' & SP.group%in% c('Scalefish'),"Discarded scalefish",
                                 NA)))))


Video.longline.interaction=Video.longline.interaction%>%
              mutate(SP.group=case_when(Code >=3.7e7 & Code<=3.70241e7 ~"Sharks",
                                        Code >3.7025e7 & Code<=3.7041e7 ~"Rays",
                                        Code >=3.7042e7 & Code<=3.7044e7 ~"Chimaeras",
                                        Code >=3.7046e7 & Code<=3.747e7 ~"Scalefish",
                                        Code >=4.1e+07 & Code<=4.115e+07 ~"Marine mammals",
                                        Code >=4.0e+07 & Code<4.1e+07 ~"Seabirds",
                                        Code >=1.2e7 & Code<3.7e7 ~"Invertebrates",
                                        Code >=1.1e7 & Code<1.2e7 ~"Rock/reef structure",
                                        Code >=5.4e7 & Code<5.49e7 ~"Macroalgae",
                                        Code == 10000910 ~"Sponges"),
                     Period=tolower(Period),
                     Depth=as.numeric(gsub("[^0-9.-]", "", Depth)),
                     sheet_no=sapply( strsplit( OpCode, "_" ), "[", 3),
                     Camera=paste("Camera",sapply( strsplit(OpCode, "_" ), "[", 5)),
                     Escape2=ifelse(Escape=='landed',NA,Escape),
                     Interaction2=ifelse(Interaction%in%grep(paste(c("Caught","caught"),collapse="|"),Interaction, value=TRUE) &
                                           !is.na(Escape2),"Escape",Interaction))%>%
              left_join(DATA_PA,by='sheet_no')%>%
              data.frame

Video.longline.interaction=Video.longline.interaction%>%
              left_join(Retained.tabl,by='Code')%>%
                mutate(Retain.group=
                       ifelse(retained=='Yes' & SP.group%in% c('Sharks','Rays'),"Retained elasmobranch",
                       ifelse(retained=='No' & SP.group%in% c('Sharks','Rays'),"Discarded elasmobranch",
                       ifelse(retained=='Yes' & SP.group%in% c('Scalefish'),"Retained scalefish",   
                       ifelse(retained=='No' & SP.group%in% c('Scalefish'),"Discarded scalefish",
                       NA)))))



Video.net.maxN=Video.net.maxN%>%
              mutate(SP.group=case_when(Code >=3.7e7 & Code<=3.70241e7 ~"Sharks",
                                        Code >3.7025e7 & Code<=3.7041e7 ~"Rays",
                                        Code >=3.7042e7 & Code<=3.7044e7 ~"Chimaeras",
                                        Code >=3.7046e7 & Code<=3.747e7 ~"Scalefish",
                                        Code >=4.1e+07 & Code<=4.115e+07 ~"Marine mammals",
                                        Code >=4.0e+07 & Code<4.1e+07 ~"Seabirds",
                                        Code >=1.2e7 & Code<3.7e7 ~"Invertebrates",
                                        Code >=1.1e7 & Code<1.2e7 ~"Rock/reef structure",
                                        Code >=5.4e7 & Code<5.49e7 ~"Macroalgae",
                                        Code == 10000910 ~"Sponges"),
                     Period=tolower(Period),
                     Depth=as.numeric(gsub("[^0-9.-]", "", Depth)),
                     sheet_no=sapply( strsplit( OpCode, "_" ), "[", 3),
                     Camera=paste("Camera",sapply( strsplit(OpCode, "_" ), "[", 5)))%>%
                left_join(DATA_PA,by='sheet_no')%>%
                data.frame

Video.longline.maxN=Video.longline.maxN%>%
                mutate(SP.group=case_when(Code >=3.7e7 & Code<=3.70241e7 ~"Sharks",
                                          Code >3.7025e7 & Code<=3.7041e7 ~"Rays",
                                          Code >=3.7042e7 & Code<=3.7044e7 ~"Chimaeras",
                                          Code >=3.7046e7 & Code<=3.747e7 ~"Scalefish",
                                          Code >=4.1e+07 & Code<=4.115e+07 ~"Marine mammals",
                                          Code >=4.0e+07 & Code<4.1e+07 ~"Seabirds",
                                          Code >=1.2e7 & Code<3.7e7 ~"Invertebrates",
                                          Code >=1.1e7 & Code<1.2e7 ~"Rock/reef structure",
                                          Code >=5.4e7 & Code<5.49e7 ~"Macroalgae",
                                          Code == 10000910 ~"Sponges"),
                       Period=tolower(Period),
                       Depth=as.numeric(gsub("[^0-9.-]", "", Depth)),
                       sheet_no=sapply( strsplit( OpCode, "_" ), "[", 3),
                       Camera=paste("Camera",sapply( strsplit(OpCode, "_" ), "[", 5)))%>%
                left_join(DATA_PA,by='sheet_no')%>%
                data.frame

#Add species names and code to DATA
DATA=DATA%>%
  mutate(species=toupper(species))%>%
  left_join(All.species.names,by=c('species'='Species'))

#Add LL configuration combos
DATA=DATA%>%
  mutate(hooktype=capitalize(hooktype),
         wiretrace=tolower(wiretrace),
         wiretrace=ifelse(wiretrace=='no',"Mono",ifelse(wiretrace=="yes","Wire",NA)),
         LL.config=ifelse(method=="LL",
                          paste(hooktype, hooksize, wiretrace),
                          NA))

#PL for stingrays and eagle rays are DW and create Size variable
DATA=DATA%>%mutate(dw=ifelse(CAES_Code %in% 35000:39100,pl,NA),
                   Size=case_when(taxa=='Elasmobranch'~fl,
                                  taxa=='Teleost'~tl,
                                  CAES_Code %in% 35000:39100 ~ dw))



#---------Observer catch composition, size distribution and cpue ------------ 
# 1. Map of sampling sites
do.map=FALSE
if(do.map)
{
  library(PBSmapping)
  Bathymetry_120=read.table(handl_OneDrive("Data/Mapping/get_data112_120.cgi"))
  Bathymetry_138=read.table(handl_OneDrive("Data/Mapping/get_data120.05_138.cgi"))
  Bathymetry=rbind(Bathymetry_120,Bathymetry_138)
  Bathymetry=Bathymetry%>%filter(V2<=(-26))
  Bathymetry=Bathymetry[order(Bathymetry$V1,Bathymetry$V2),]
  xbat=sort(unique(Bathymetry$V1))
  ybat=sort(unique(Bathymetry$V2)) 
  reshaped=as.matrix(reshape(Bathymetry,idvar="V1",timevar="V2",v.names="V3", direction="wide"))
  
  #Spatial range
  #Lat.range=round(c(min(DATA$mid.lat)-1,max(DATA$mid.lat)+1))
  #Long.range=round(c(min(DATA$mid.long)-1,max(DATA$mid.long)+1))  
  Lat.range=c(-36,-29)
  Long.range=c(113,119)  
  
  seq.Lat=seq(Lat.range[1],Lat.range[2])
  seq.Long=seq(Long.range[1],Long.range[2])
  
  Sites=DATA%>%distinct(sheet_no,mid.lat,mid.long,method)%>%
    mutate(Col=ifelse(method=='GN',"#F8766D", "#00BFC4"))
  #bring in shape file
  data(worldLLhigh)
  
  tiff(file=le.paste("Map site area.tiff"),width = 1600, height = 2400,units = "px", res = 300,compression = "lzw")
  par(mar = c(0, 0, 0, 0),oma=c(0,0,0,0),mgp=c(.1, 0.15, 0))
  
  #plot shots' Sampling site locations 
  plotMap(worldLLhigh, xlim=Long.range,ylim=Lat.range,axes=F,
          col="dark grey",tck = 0.025, tckMinor = 0.0125, xlab="",ylab="")
  points(Sites$mid.long,Sites$mid.lat,col='black',pch=21,bg=Sites$Col,cex=1.25)
  
  #add bathymetry
  contour(xbat, ybat, reshaped[,2:ncol(reshaped)],ylim=plotlat[[i]],xlim=plotlong[[i]], zlim=c(-1,-300),
          nlevels = 3,labcex=1,lty = c(1,2,3),col=c(rep("black",3)),add=T)
  axis(side = 1, at =seq.Long, labels = seq.Long, tcl = .5,las=1,cex.axis=0.9)
  axis(side = 2, at = seq.Lat, labels = -seq.Lat,tcl = .5,las=2,cex.axis=0.9)
  
  mtext("Latitude (?S)",side=2,line=1.25,las=3,cex=1.5)
  mtext("Longitude (?E)",side=1,line=1.25,cex=1.5)
  
  legend('bottomleft',c("Gillnet","Longline"),pch=21,
         pt.bg=c("#F8766D", "#00BFC4"),bty='n',cex=1.25)
  box()
  
  #inset Australia
  par(fig=c(.35,.95,.35,.95), new = T,mgp=c(.1,.4,0),mai=c(.01,01,.01,.01))
  plotMap(worldLLhigh, xlim=c(113,155), ylim=c(-44,-11),col="grey80", axes=F, xlab="", ylab="",
          border="black",bg="white",plt = NULL)
  
  text(122,-24,"Western",col="black",cex=1,font=2.5)
  text(122,-27,"Australia",col="black",cex=1,font=2.5)
  polygon(x=c(Long.range,rev(Long.range)),
          y=c(rep(Lat.range[1],2),rep(Lat.range[2],2)),
          col=rgb(.1, .6, .1, alpha = .4),border = "black")
  dummy.ln=c(152.8,154.6)
  dummy.la=c(-12.03,-10.94)
  polygon(x=c(dummy.ln,rev(dummy.ln)),
          y=c(rep(dummy.la[1],2),rep(dummy.la[2],2)),
          col='white',border = 'white')
  lines(x=c(129,129),y=c(-31.64,-15),lty=2)
  dev.off()
  
}  

# 2. Number of individuals caught by snood configuration 
fn.fig(le.paste("Observer/Barplot Longline configurations"),2400,2000)
DATA%>%
  filter(method=="LL")%>%
  filter(!is.na(hooktype) & !is.na(hooksize) & !is.na(wiretrace))%>%
  group_by(LL.config,hooktype, hooksize, wiretrace)%>%
  tally()%>%
  ggplot(aes(x= LL.config, y=n,fill=wiretrace)) +
  geom_bar(stat="identity", position = "dodge")+
  theme(legend.title=element_text(size=14), 
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ylab("Number of individuals caught")+xlab('Snood configuration')
dev.off()

# 3. Catch composition        
Grouped.species.fn=function(METH)
{
  x=DATA%>%
    filter(method==METH)%>%
    group_by(COMMON_NAME)%>%
    tally()%>%
    ungroup()%>%
    mutate(Prop=n/sum(n))%>%
    arrange(-Prop)%>%
    mutate(CumSum=cumsum(Prop))%>%
    filter(CumSum>.9)%>%
    pull(COMMON_NAME)
  x=subset(x,!x%in%Main.species)
  return(x)
}
Grouped.species.GN=Grouped.species.fn("GN")
Grouped.species.LL=Grouped.species.fn("LL")

fn.brplt=function(d,Grouped.species,CX.nm)
{
  dd=d%>%
    filter(Taxa%in%c('Elasmobranch','Teleost'))%>%
    mutate(new.COMMON_NAME=ifelse(COMMON_NAME%in%Grouped.species & Taxa=='Elasmobranch',
                                  "Other elasmobranchs",
                                  ifelse(COMMON_NAME%in%Grouped.species & Taxa=='Teleost',
                                         "Other scalefish",COMMON_NAME)))%>%
    group_by(new.COMMON_NAME,Taxa)%>%
    tally()%>%
    ungroup()%>%
    mutate(Prop=n/sum(n))%>%
    arrange(-Prop)
  
  n.other.elasmo=d%>%filter(COMMON_NAME%in%Grouped.species & Taxa=='Elasmobranch')%>%pull(COMMON_NAME)
  n.other.elasmo=length(unique(n.other.elasmo))
  n.other.scalies=d%>%filter(COMMON_NAME%in%Grouped.species & Taxa=='Teleost')%>%pull(COMMON_NAME)
  n.other.scalies=length(unique(n.other.scalies))
  
  dd=dd%>%
    mutate(colr=ifelse(Taxa=='Elasmobranch',"steelblue","firebrick"),
           colr=ifelse(new.COMMON_NAME%in%c("Other elasmobranchs","Other scalefish"),"forestgreen",colr),
           new.COMMON_NAME=ifelse(new.COMMON_NAME=="Other elasmobranchs",
                                  paste("Other elasmos (n=",n.other.elasmo," species)",sep=""),
                                  ifelse(new.COMMON_NAME=="Other scalefish",
                                         paste("Other scalefish (n=",n.other.scalies," species)",sep=""),
                                         new.COMMON_NAME)))%>%
    group_by(new.COMMON_NAME,colr)%>%
    summarise(Prop=sum(Prop))%>%
    data.frame%>%
    arrange(Prop)
  with(dd,barplot(Prop,horiz = T,col=colr,names.arg=new.COMMON_NAME,cex.names=CX.nm,las=2))
  box()
}

    #Overall GN vs LL
fn.fig(le.paste("Observer/Catch.comp_GN_LL_overall"),2400,1600)
par(mfcol=c(1,2),mar=c(2,10.5,1,.1),oma=c(1,.1,.5,.3),mgp=c(1.5,.5,0),cex.axis=.9)
fn.brplt(d=DATA%>%
           filter(method=="GN"),
         Grouped.species=Grouped.species.GN,
         CX.nm=.8)
mtext(paste("Gillnet (",sum(subset(DATA,method=='GN')$number)," individuals)",sep=""),3,cex=1)
fn.brplt(d=DATA%>%
           filter(method=="LL"),
         Grouped.species=Grouped.species.LL,
         CX.nm=.8)
mtext(paste("Longline (",sum(subset(DATA,method=='LL')$number)," individuals)",sep=""),3,cex=1)
mtext("Proportion of total catch",1,outer=T,line=0.1,cex=1.2) 
dev.off()

    #by LL configuration 
LL.configs=DATA%>%
  filter(method=="LL")%>%
  filter(!(is.na(hooktype)| is.na(hooksize) | is.na(wiretrace)))%>%
  group_by(LL.config)%>%
  tally()%>%pull(LL.config)
fn.fig(le.paste("Observer/Catch.comp_by_LL.configuration"),2400,1800)
smart.par(length(LL.configs),MAR=c(2,10.5,1,.1),OMA=c(1,.1,.5,.3),MGP=c(1.5,.5,0))
par(cex.axis=.9)
for(i in 1:length(LL.configs))
{
  d=DATA%>%
    filter(method=="LL" & LL.config==LL.configs[i])
  fn.brplt(d=d,Grouped.species=Grouped.species.LL,CX.nm=.7)
  mtext(paste(LL.configs[i]," (",nrow(d)," individuals)",sep=""),3,cex=.6)
  rm(d)
}
mtext("Proportion of total catch",1,outer=T,line=0.1,cex=1.2) 
dev.off()

    #Multivariate stats
UNIK.gear.zone=unique(paste(DATA$method,DATA$zone))
colfunc <- colorRampPalette(c("orange","firebrick1"))
n.col.GN=colfunc(length(grep("GN",UNIK.gear.zone)))
colfunc <- colorRampPalette(c("lightblue3", "dodgerblue4"))
n.col.LL=colfunc(length(grep("LL",UNIK.gear.zone)))
dis.cols=c(n.col.GN,n.col.LL)
names(dis.cols)=UNIK.gear.zone
system.time({
  multivariate.fn(d=DATA%>%
                    mutate(species=CAES_Code,
                           method.zone=paste(method,zone),
                           livewt.c=number)%>%
                    dplyr::select(method,zone,block,month,common_name,species,method.zone,livewt.c),
                  Terms=c('method.zone','block','month'),
                  Def.sp.term=c('method','species'),
                  Transf='proportion',
                  Show.term='method.zone',
                  Group='Top20',
                  hndl=le.paste("Observer"),
                  MDS.title="/Catch.comp_GN_LL_MDS",
                  Simper.title="/Catch.comp_GN_LL_Simper",
                  Permanova.title="/Catch.comp_GN_LL_Permanova")
}) 


# 4. Size distributions

    #GN vs LL for main species
dumi=DATA%>%
  filter(common_name%in%Main.species)%>%
  mutate(size=ifelse(Taxa=="Elasmobranch",fl,
                     ifelse(Taxa=="Teleost",tl,
                            NA)))
Sumri=dumi %>% group_by(common_name,Taxa,method) %>% 
  summarise(n=sum(number),size=median(size,na.rm=T))%>%
  filter(n>2)

fn.fig(le.paste("Observer/Size.density.dist.main.sp_GN_LL"),2400,1600)
dumi%>%
ggplot(aes(x = size, y = common_name,fill=Taxa)) + 
  geom_density_ridges(alpha = .8, color = "white")+
  facet_wrap(~method)+
  scale_fill_manual(values=c("steelblue","firebrick"))+
  ylab('')+
  xlab('Size (cm, FL for elasmobranchs, TL for teleosts)')+
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
   geom_text(data=Sumri,aes(label=paste('n= ',n,', median= ',round(size)," cm",sep='')),
             position=position_nudge(y=-0.1), colour="black", size=2.2)
dev.off()

    #by LL configuration for main species
dumi=DATA%>%
  filter(common_name%in%Main.species)%>%
  filter(LL.config%in%LL.configs)%>%
  mutate(size=ifelse(Taxa=="Elasmobranch",fl,
                     ifelse(Taxa=="Teleost",tl,
                            NA)))
Sumri=dumi %>% group_by(common_name,Taxa,LL.config) %>% 
  summarise(n=sum(number),size=median(size,na.rm=T)) %>%
  filter(n>2)

fn.fig(le.paste("Observer/Size.density.dist.main.sp_by_LL.configuration"),2400,1600)
dumi%>%
  ggplot(aes(x = size, y = common_name,fill=Taxa)) + 
  geom_density_ridges(alpha = .8, color = "white")+
  facet_wrap(~LL.config)+
  scale_fill_manual(values=c("steelblue","firebrick"))+
  ylab('')+
  xlab('Size (cm, FL for elasmobranchs, TL for teleosts)')+
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text=element_text(size=12),
        axis.text=element_text(size=10),
        axis.title=element_text(size=14))+
  geom_text(data=Sumri,aes(label=paste('n= ',n,', median= ',round(size)," cm",sep='')),
            position=position_nudge(y=-0.1), colour="black", size=2)
dev.off()


#Compare overlap between GN and LL
Tabl.sizes=DATA%>%
  filter(!is.na(Size) & !is.na(common_name))%>%
  group_by(common_name,method)%>%
  tally()%>%
  spread(method,n,fill=0)%>%
  filter(GN>10 & LL>10)%>%
  pull(common_name)

Overlap.res=vector('list',length(Tabl.sizes))
for(o in 1:length(Overlap.res))
{
  x <- list(GN=DATA%>%
              filter(common_name==Tabl.sizes[o] & !is.na(Size) & method=='GN')%>%
              pull(Size),
            LL=DATA%>%
              filter(common_name==Tabl.sizes[o] & !is.na(Size) & method=='LL')%>%
              pull(Size))
  out <- overlap(x)
  #final.plot(x,out$OV)
  Overlap.res[[o]]=data.frame(common_name=Tabl.sizes[o],overlap=out$OV)
}
Overlap.res=do.call(rbind,Overlap.res)
fn.fig(le.paste("Observer/Size.density.dist.overlap_GN_LL"),2400,1600)
DATA%>%
  filter(common_name%in%Tabl.sizes & !is.na(Size))%>%
  left_join(Overlap.res,by='common_name')%>%
  mutate(LBL=paste(common_name," (overlap= ",100*round(overlap,2),"%)",sep=''))%>%
  ggplot(aes(x = Size, fill=method)) +
  facet_wrap( ~LBL,scales='free') + 
  geom_density(alpha=.5) +
  ylab('Density')+
  xlab('Size (cm, FL for elasmobranchs, TL for teleosts)')+
  theme(legend.position = "top",
        legend.title = element_blank(),
        strip.text.x = element_text(size = 8),
        legend.text=element_text(size=10),
        axis.text=element_text(size=10),
        axis.title=element_text(size=12))
dev.off()

#Kolmogorov-Smirnov test
KS.result=vector('list',length(Tabl.sizes))
fn.kolmo=function(GN,LL)  Kolmo=ks.test(GN,LL)
for(s in 1:length(KS.result))
{
  KS.result[[s]]=fn.kolmo(GN=DATA%>%
                              filter(common_name==Tabl.sizes[s] & !is.na(Size) & method=='GN')%>%
                              pull(Size),
                          LL=DATA%>%
                              filter(common_name==Tabl.sizes[s] & !is.na(Size) & method=='LL')%>%
                              pull(Size))

}
Tab.Kolmo=as.data.frame(do.call(rbind,KS.result))
Tab.Kolmo=cbind(Species=Tabl.sizes,Tab.Kolmo)
rownames(Tab.Kolmo)=NULL
Tab.Kolmo=data.frame(Species=unlist(Tab.Kolmo$Species),
                     Statistic=round(unlist(Tab.Kolmo$statistic),3),
                     P.value=round(unlist(Tab.Kolmo$p.value),3))
Tab.Kolmo=Tab.Kolmo%>%
            mutate(P.value=ifelse(P.value<0.001,'<0.001',P.value))
write.csv(Tab.Kolmo,le.paste("Observer/Size.Kolmo_Smirnov.csv"),row.names = F)



# 5. CPUE
    #GN vs LL
fn.fig(le.paste("Observer/CPUE.main.sp_GN_LL"),2400,1600)
DATA%>%
            mutate(Effort=ifelse(method=="GN",1000*net_length*soak.time,  #net length in m
                          ifelse(method=="LL",n.hooks*soak.time,
                          NA)),
                   common_name=case_when(!common_name%in%Main.species~'Other',
                                         TRUE~common_name))%>%
            filter(common_name%in%c(Main.species,'Other'))%>%  #add 'other' species to account for all shots
            group_by(sheet_no,method,common_name)%>%
            summarise(N=sum(number),
                      Effort=max(Effort))%>%
            ungroup()%>%
            spread(common_name,N,fill=0)%>%  #add 0 records
            gather(common_name,N,-c(sheet_no,method,Effort))%>%
            filter(!common_name=='Other')%>%
            left_join(DATA%>%distinct(common_name,Taxa),by='common_name')%>%
            mutate(cpue=N/Effort,
                   cpue1=ifelse(method=="GN",cpue*1000,   # (comparable cpue 3000 m GN = 1000 m LL)
                         ifelse(method=="LL",cpue*1000,
                         NA)),
                   colr=ifelse(Taxa=='Teleost',"firebrick",
                               ifelse(Taxa=='Elasmobranch',"steelblue",NA)))%>%
            filter(!is.na(Effort))%>%
            filter(Effort>0)%>%
  ggplot(aes(x=method, y=cpue1)) + 
  geom_boxplot(outlier.size = 1,aes(fill=method))+ coord_flip()+
  facet_wrap(~common_name,scales='free_x')+
  ylab("Catch rate per session (Numbers of individuals per 1000 m hour for GN and 1000 hook hour for LL)")+xlab("")+ 
  theme(legend.position="none",
        axis.text = element_text(size = 12),
        strip.text.x = element_text(size = 12))
dev.off()

    #by LL configuration
Display.these=c("Dusky shark","Gummy shark",
                "West Australian dhufish","Pink snapper") #select what species to display, not enough obs for the other main species
fn.fig(le.paste("Observer/CPUE.main.sp_by_LL.configuration"),2400,1600)
DATA%>%
  filter(LL.config%in%LL.configs)%>%
  mutate(Effort=ifelse(method=="GN",1000*net_length*soak.time,  #net length in m
                       ifelse(method=="LL",n.hooks*soak.time,
                              NA)),
         common_name=case_when(!common_name%in%Main.species~'Other',
                               TRUE~common_name))%>%
  filter(common_name%in%c(Main.species,'Other'))%>%  #add 'other' species to account for all shots
  group_by(sheet_no,LL.config,common_name)%>%
  summarise(N=sum(number),
            Effort=max(Effort))%>%
  ungroup()%>%
  spread(common_name,N,fill=0)%>%  #add 0 records
  gather(common_name,N,-c(sheet_no,LL.config,Effort))%>%
  filter(!common_name=='Other')%>%
  left_join(DATA%>%distinct(common_name,Taxa),by='common_name')%>%
  mutate(cpue=N/Effort,
         cpue1=cpue*1000,
         colr=ifelse(Taxa=='Teleost',"firebrick",
                     ifelse(Taxa=='Elasmobranch',"steelblue",NA)))%>%
  filter(!is.na(Effort))%>%
  filter(Effort>0)%>%
  filter(common_name%in%Display.these)%>%    
  ggplot(aes(x=LL.config, y=cpue1)) + 
  geom_boxplot(outlier.size = 1,aes(fill=LL.config))+ coord_flip()+
  facet_wrap(~common_name,scales='free_x')+
  ylab("Catch rate per session (Numbers of individuals per 1000 hook hour)")+xlab("")+ 
  theme(legend.position="none",
        axis.text = element_text(size = 12),
        strip.text.x = element_text(size = 12))
dev.off()

#---------Observer retained vs discarded ------------ 

#Export table of retained or discarded
Out_table_Retained=DATA%>%
                    filter(!is.na(retainedflag))%>%
                    group_by(common_name,retainedflag,CAES_Code)%>%
                    tally()%>%
                    ungroup%>%
                    spread(retainedflag,n,fill=0)%>%
                    arrange(CAES_Code)
Out_table_disc.size=DATA%>%
                  filter(retainedflag=='No')%>%
                  group_by(common_name)%>%
                  summarise(Discarded.median.size=round(median(Size,na.rm=T)),
                            min.size=round(min(Size,na.rm=T)),
                            max.size=round(max(Size,na.rm=T)))%>%
                  mutate(Discarded.size.range=paste(min.size,max.size,sep='-'))%>%
                  filter(!is.infinite(min.size) | !is.infinite(max.size))%>%
                  dplyr::select(-min.size, -max.size)
Out_table_Retained=Out_table_Retained%>%
                    left_join(Out_table_disc.size,by='common_name')%>%
                    dplyr::select(-CAES_Code)%>%
                    ungroup()%>%
                    filter(!common_name=='')%>%
                    filter(!common_name=='Unidentified')%>%
                    rename(Discarded=No,
                           Retained=Yes)%>%
                    relocate(Retained, .before = Discarded)
write.csv(Out_table_Retained,le.paste("Observer/Retained_species_table.csv"),row.names=F)  
  


#Plot percentage retained and discarded by gear
  #number of individuals
fn.fig(le.paste("Observer/Catch.comp_GN_LLretained_discarded"),2400,1600)
DATA%>%
  filter(!is.na(retainedflag))%>%
  ggplot(aes(x=as.factor(method), fill=as.factor(retainedflag)))+
  geom_bar(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..]), position="dodge" ) +
  geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ),
            stat="count", position=position_dodge(0.9), vjust=-0.5)+
  scale_y_continuous(labels = scales::percent)+
  theme(legend.title=element_text(size=14), 
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  ylab("Percentage")+xlab('Method')+labs(fill='Retained') 
dev.off()

  #number of species
fn.fig(le.paste("Observer/Catch.comp_GN_LLretained_discarded_species"),2400,1600)
DATA%>%
  filter(!is.na(retainedflag))%>%
  mutate(SP.GR=paste(species,method))%>%
  distinct(species,method,retainedflag,SP.GR)%>%
  ggplot(aes(x=as.factor(method), fill=as.factor(retainedflag)))+
  geom_bar(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..]), position="dodge" ) +
  geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ),
            stat="count", position=position_dodge(0.9), vjust=-0.5)+
  scale_y_continuous(labels = scales::percent)+
  theme(legend.title=element_text(size=14), 
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  ylab("Percentage")+xlab('Method')+labs(fill='Retained') 
dev.off()

#---------Lost hooks ------------
Lost.huk=DATA[grep('hook',tolower(DATA$comments.hdr)),]%>%
  distinct(sheet_no,method,n.hooks,comments.hdr)%>%
  mutate(Lost.hk= str_extract(comments.hdr, "[[:digit:]]+"))
uniK=DATA%>%
  distinct(sheet_no,method,n.hooks,comments.hdr)%>%
  mutate(Lost.hk=0)%>%
  filter(method=='LL'& !sheet_no%in%Lost.huk$sheet_no)

Lost.huk=rbind(Lost.huk,uniK)%>%
  filter(method=='LL' & !is.na(n.hooks))%>%
  mutate(Lost.hk=as.numeric(Lost.hk),
         Percent.lost=100*Lost.hk/n.hooks)%>%
  dplyr::select(-comments.hdr)%>%
  filter(n.hooks>0)%>%
  arrange(sheet_no)%>%
  mutate(LOST=ifelse(Lost.hk>0,"Yes","No"))
Lost.huk$Session=1:nrow(Lost.huk)

fn.fig(le.paste("Observer/Lost hooks"),2400,1800)
Lost.huk%>%
  ggplot(aes(Session,n.hooks))+
  geom_col(aes(fill=LOST))+
  geom_text(data = Lost.huk%>%filter(LOST=="Yes"),
            aes(label = paste(Lost.hk,'lost')),angle=90,nudge_y=25)+
  ylab("Number of hooks deployed")+
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text=element_text(size=12),
        axis.text=element_text(size=10),
        axis.title=element_text(size=14))
dev.off()

#---------General tables and plots of PA underwater Video ------------ 
SP.group.levels=c("Invertebrates","Scalefish","Sharks","Rays","Marine mammals","Seabirds")
TEP.groups=c("Marine mammals","Seabirds","Reptiles")

  #1. number of events 

    #1.1. by gear and species group
rbind(Video.longline.interaction%>%dplyr::select(Method,Interaction,Number,SP.group,Species),
      Video.net.interaction%>%dplyr::select(Method,Interaction,Number,SP.group,Species))%>%
  filter(!Species=='birds feeding at surface')%>%
  filter(!SP.group%in%TEP.groups)%>%
  mutate(Number=1,
         Method=capitalize(Method))%>%
  group_by(Method,Interaction,SP.group)%>%
  tally(Number)%>%
  filter(!is.na(Interaction))%>%
  mutate(Interaction=capitalize(tolower(Interaction)),
         SP.group=factor(SP.group,levels=SP.group.levels))%>%
  ggplot(aes(fill=Method, y=n, x=Interaction)) + 
  geom_bar(position="dodge", stat="identity")+
  coord_flip() + scale_y_sqrt()+
  facet_wrap(~SP.group,dir='h',scales='free_x')+ 
  theme(legend.position = "top",
        strip.text = element_text(size = 16),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16))+
  xlab('')+ylab('Number of events')+ guides(color = guide_legend(nrow = 1))
ggsave(le.paste("Video/underwater/Interactions_number.events_sqrt.transf_by.group.tiff"),width = 12,height = 8,compression = "lzw")

    #1.2. by gear
rbind(Video.longline.interaction%>%dplyr::select(Method,Interaction,Number,SP.group,Species),
      Video.net.interaction%>%dplyr::select(Method,Interaction,Number,SP.group,Species))%>%
  filter(!Species=='birds feeding at surface')%>%
  filter(!SP.group%in%TEP.groups)%>%
  mutate(Number=1,
         Method=capitalize(Method))%>%
  group_by(Method,Interaction)%>%
  tally(Number)%>%
  filter(!is.na(Interaction))%>%
  mutate(Interaction=capitalize(tolower(Interaction)))%>%
  ggplot(aes(fill=Method, y=n, x=Interaction)) + 
  geom_bar(position="dodge", stat="identity")+
  coord_flip() + scale_y_sqrt()+
  facet_wrap(~Method,dir='h',scales='free_x')+ 
  theme(legend.position = "top",
        strip.text = element_text(size = 16),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16))+
  xlab('')+ylab('Number of events')+ guides(color = guide_legend(nrow = 1))
ggsave(le.paste("Video/underwater/Interactions_number.events_sqrt.transf.tiff"),width = 12,height = 8,compression = "lzw")



  #2. Number of individuals

    #2.1. by gear and species group
rbind(Video.longline.interaction%>%dplyr::select(Method,Interaction,Number,SP.group,Species),
      Video.net.interaction%>%dplyr::select(Method,Interaction,Number,SP.group,Species))%>%
  filter(!Species=='birds feeding at surface')%>%
  filter(!SP.group%in%TEP.groups)%>%
  mutate(Method=capitalize(Method))%>%
  group_by(Method,Interaction,SP.group)%>%
  summarise(n=sum(Number))%>%
  filter(!is.na(Interaction))%>%
  mutate(Interaction=capitalize(tolower(Interaction)),
         SP.group=factor(SP.group,levels=SP.group.levels))%>%
  ggplot(aes(fill=Method, y=n, x=Interaction)) + 
  geom_bar(position="dodge", stat="identity")+
  coord_flip() + scale_y_sqrt()+
  facet_wrap(~SP.group,dir='h',scales='free_x')+ 
  theme(legend.position = "top",
        strip.text = element_text(size = 16),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16))+
  xlab('')+ylab('Number of individuals')+ guides(color = guide_legend(nrow = 1))
ggsave(le.paste("Video/underwater/Interactions_number.individuals_sqrt.transf_by.group.tiff"),width = 12,height = 8,compression = "lzw")


    #2.2. by gear
rbind(Video.longline.interaction%>%dplyr::select(Method,Interaction,Number,SP.group,Species),
      Video.net.interaction%>%dplyr::select(Method,Interaction,Number,SP.group,Species))%>%
  filter(!Species=='birds feeding at surface')%>%
  filter(!SP.group%in%TEP.groups)%>%
  mutate(Method=capitalize(Method))%>%
  group_by(Method,Interaction)%>%
  summarise(n=sum(Number))%>%
  filter(!is.na(Interaction))%>%
  mutate(Interaction=capitalize(tolower(Interaction)))%>%
  ggplot(aes(fill=Method, y=n, x=Interaction)) + 
  geom_bar(position="dodge", stat="identity")+
  coord_flip() + scale_y_sqrt()+
  facet_wrap(~Method,dir='h',scales='free_x')+ 
  theme(legend.position = "top",
        strip.text = element_text(size = 16),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16))+
  xlab('')+ylab('Number of individuals')+ guides(color = guide_legend(nrow = 1))
ggsave(le.paste("Video/underwater/Interactions_number.individuals_sqrt.transf.tiff"),width = 12,height = 8,compression = "lzw")



  #Export data for Abbey
add.effort=rbind(Video.longline.interaction%>%dplyr::select(sheet_no,Method,Camera),
                 Video.net.interaction%>%dplyr::select(sheet_no,Method,Camera))%>%
                mutate(Method=capitalize(Method))%>%
                group_by(sheet_no,Method,Camera)%>%
                summarise(n=n())%>%
                group_by(sheet_no,Method)%>%
                summarise(n.cameras=n())
Abbey.data.chapter.1_species=rbind(Video.longline.interaction%>%dplyr::select(sheet_no,Method,Interaction,Number,Code,SP.group),
                                   Video.net.interaction%>%dplyr::select(sheet_no,Method,Interaction,Number,Code,SP.group))%>%
                                mutate(Method=capitalize(Method),
                                       Interaction=capitalize(tolower(Interaction)))%>%
                                filter(!SP.group%in%c('Macroalgae','Rock/reef structure'))%>%
                                group_by(sheet_no,Method,Interaction,Code)%>%
                                summarise(Number=sum(Number))%>%
                                spread(Code,Number,fill = 0)%>%
                                data.frame%>%
                                left_join(add.effort,by=c('sheet_no','Method'))

  
Abbey.data.chapter.1_SP.group=rbind(Video.longline.interaction%>%dplyr::select(sheet_no,Method,Interaction,Number,SP.group),
                                    Video.net.interaction%>%dplyr::select(sheet_no,Method,Interaction,Number,SP.group))%>%
                                mutate(Method=capitalize(Method),
                                       Interaction=capitalize(tolower(Interaction)))%>%
                                filter(!SP.group%in%c('Macroalgae','Rock/reef structure'))%>%
                                group_by(sheet_no,Method,Interaction,SP.group)%>%
                                summarise(Number=sum(Number))%>%
                                spread(SP.group,Number,fill = 0)%>%
                                data.frame%>%
                                left_join(add.effort,by=c('sheet_no','Method'))


Abbey.data.chapter.1_Retain.group=rbind(Video.longline.interaction%>%dplyr::select(sheet_no,Method,Interaction,Number,Retain.group),
                                        Video.net.interaction%>%dplyr::select(sheet_no,Method,Interaction,Number,Retain.group))%>%
                                  mutate(Method=capitalize(Method),
                                         Interaction=capitalize(tolower(Interaction)))%>%
                                  group_by(sheet_no,Method,Interaction,Retain.group)%>%
                                  summarise(Number=sum(Number))%>%
                                  filter(!is.na(Retain.group))%>%
                                  spread(Retain.group,Number,fill = 0)%>%
                                  data.frame%>%
                                  left_join(add.effort,by=c('sheet_no','Method'))


write.csv(Abbey.data.chapter.1_species,handl_OneDrive('Analyses/Parks Australia/outputs/Data for Abbey/Abbey.data.chapter.1_species.csv'),row.names = F)
write.csv(Abbey.data.chapter.1_SP.group,handl_OneDrive('Analyses/Parks Australia/outputs/Data for Abbey/Abbey.data.chapter.1_SP.group.csv'),row.names = F)
write.csv(Abbey.data.chapter.1_Retain.group,handl_OneDrive('Analyses/Parks Australia/outputs/Data for Abbey/Abbey.data.chapter.1_Retain.group.csv'),row.names = F)


  #number of events for main target species
these.codes=c(37018003,37017003,37017001,37018007,
              37377004,37384002,37320004,37353001)
dummy=data.frame(Code=these.codes,
                 Names=All.species.names[match(these.codes,All.species.names$Code),"COMMON_NAME"])

colfunc <- colorRampPalette(Shark.palette)
n.col.elasmos=colfunc(length(these.codes[these.codes<37300000]))
colfunc <- colorRampPalette(Teleost.palette)
n.col.teleos=colfunc(length(these.codes[these.codes>37300000]))
d=rbind(Video.longline.interaction%>%dplyr::select(Method,Interaction,Number,Code),
      Video.net.interaction%>%dplyr::select(Method,Interaction,Number,Code))%>%
  mutate(Number=1,
         Method=capitalize(Method))%>%
  filter(Code%in%these.codes)%>%
  left_join(dummy,by='Code')%>%
  group_by(Method,Interaction,Names)%>%
  tally(Number)%>%
  filter(!is.na(Interaction))%>%
  mutate(Interaction=capitalize(tolower(Interaction)),
         Names=factor(Names,levels=dummy%>%arrange(Code)%>%pull(Names)))
dis.cols=c(n.col.elasmos,n.col.teleos)
names(dis.cols)=levels(d$Names)
dis.cols=dis.cols[match(levels(droplevels(d$Names)),names(dis.cols))]
d%>%
  ggplot(aes(fill=Names, y=n, x=Interaction)) + 
  geom_bar(position="stack", stat="identity")+
  coord_flip() +
  facet_wrap(~Method,dir='h',scales='free_x')+ 
  theme(legend.position = "top",
        strip.text = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16))+
  xlab('')+ylab('Number of events')+ guides(color = guide_legend(nrow = 1))+
  scale_fill_manual(values=dis.cols)
ggsave(le.paste("Video/underwater/Interactions_number.events_main.target.tiff"),width = 12,height = 8,compression = "lzw")

do.loli=FALSE
if(do.loli)
{
  #Lollipop graph of % of interaction types by gear 
  df=rbind(Video.longline.interaction%>%dplyr::select(Method,Interaction,Number),
           Video.net.interaction%>%dplyr::select(Method,Interaction,Number))
  df=pct_routine(df, Method, Interaction)
  ggplot(df)+
    geom_linerange(aes(x = Interaction, ymin = 0, ymax = pct, colour = Method), 
                   position = position_dodge(width = 1))+
    geom_point(aes(x = Interaction, y = pct, colour = Method),
               position = position_dodge(width = 1))+
    coord_flip()
  ggsave(le.paste("Video/underwater/Interactions_lollipop.tiff"), 
         width = 12,height = 10,compression = "lzw")
  
  
  #Mosaic plot
  #gillnet
  TAB=table(Video.net.interaction$Interaction)
  lab=c(TAB)
  lab=paste(names(TAB)," (n= ",lab,")",sep='')
  names(lab)=names(TAB)
  
  ggplot(data = Video.net.interaction) +
    geom_mosaic(aes(x = product(SP.group), fill=SP.group), na.rm=TRUE) + 
    labs(x = "",y='', title='Gillnet')+
    facet_wrap(~Interaction,labeller = labeller(Interaction=lab)) + labs(fill = "")
  ggsave(le.paste("Video/underwater/Interactions_mosaic_gillnet.tiff"), 
         width = 12,height = 10,compression = "lzw")
  
  #longline
  TAB=table(Video.longline.interaction$Interaction)
  lab=c(TAB)
  lab=paste(names(TAB)," (n= ",lab,")",sep='')
  names(lab)=names(TAB)
  
  ggplot(data = Video.longline.interaction) +
    geom_mosaic(aes(x = product(SP.group), fill=SP.group), na.rm=TRUE) + 
    labs(x = "",y='', title='Gillnet')+
    facet_wrap(~Interaction,labeller = labeller(Interaction=lab)) + labs(fill = "")
  ggsave(le.paste("Video/underwater/Interactions_mosaic_longline.tiff"), 
         width = 12,height = 10,compression = "lzw")
  
}



#---------Analyse PA Habitat ------------

# 1. Underwater habitat classification
Video.habitat=Video.habitat%>%
                data.frame%>%
                mutate(SHEET_NO=toupper(opcode),
                       Depth_m=Depth..M.,
                       BROAD=capitalize(tolower(BROAD)),
                       MORPHOLOGY=capitalize(MORPHOLOGY),
                       TYPE=capitalize(TYPE),
                       BROAD=case_when(BROAD%in%c('Consolidated','Unconsolidated') ~'Substrate',
                                       TRUE ~ BROAD))%>%
                filter(FieldOfView=='Facing Down')%>%
                filter(!BROAD=='Open Water')%>%
                dplyr::select(SHEET_NO,Depth_m,Collector,BROAD,MORPHOLOGY,TYPE,CODE)%>%
                mutate(Algae.morph=case_when(MORPHOLOGY=='Erect course branching'~'ECB',
                                             MORPHOLOGY=='Erect fine branching'~'EFB',
                                             MORPHOLOGY=='Filamentous/turf'~'FT',
                                             MORPHOLOGY=='Large Canopy forming'~'LCF',
                                             MORPHOLOGY=='Sheet-like/membraneous'~'SLM'))

#Coarse data
do.barplt=T
do.mosaic=F

tiff(le.paste("Video/underwater/Habitats_coarse.records.tiff"),width=1400,height=2000,units="px",res=300,compression="lzw")
par(mfcol=c(3,1),mar=c(1,1,1,.1),oma=c(1.6,9.5,.1,.75),mgp=c(1,.25,0),cex.axis=1.1)     

    #Overall Broad categories
Hab.cols=c('lightskyblue','red4','seagreen',
           'orange3','tomato4','khaki')
names(Hab.cols)=c('Hydroids','Macroalgae','Seagrasses',
                  'Sponges','Stony corals','Substrate')
Tab.broad=sort(table(Video.habitat$BROAD,useNA = 'ifany'))
barplot(100*Tab.broad/sum(Tab.broad),horiz = T,col=Hab.cols[match(names(Tab.broad),names(Hab.cols))],
        las=1,xlim=c(0,100))
box()

    #Substrate categories
colfunc <- colorRampPalette(c('khaki4', "khaki"))
Tab.Substrate=sort(with(subset(Video.habitat,BROAD=='Substrate'),table(MORPHOLOGY,useNA = 'ifany')))

barplot(100*Tab.Substrate/sum(Tab.Substrate),horiz = T,col=colfunc(length(Tab.Substrate)),
        las=1,main='',xlim=c(0,100))
legend('bottomright','Substrate',bty='n',cex=1.5)
if(do.mosaic) mtext('Percentage',1,cex=1.1,line=1.5)
box()

#Algae categories
if(do.barplt)
{
  coul=c('brown','green3',"firebrick2")
  Tab.Algae=with(subset(Video.habitat,BROAD=='Macroalgae' & !is.na(TYPE)),table(MORPHOLOGY,TYPE))
  data_percentage=t(as.matrix(Tab.Algae))
  data_percentage=data_percentage[,match(names(sort(colSums(data_percentage))),colnames(data_percentage))]
  data_percentage <- 100*data_percentage/sum(data_percentage)
  #data_percentage <- apply(data_percentage, 2, function(x){x*100/sum(x,na.rm=T)})
  barplot(data_percentage, col=coul, las=1,main='',horiz = T,xlim=c(0,100))
  legend("bottomright",legend = rownames(data_percentage),cex=1.35,title="Macroalgae",
         fill = coul,bty='n')
  box()
  mtext('Percentage',1,cex=1.1,line=1.4)
}
if(do.mosaic)
{
  Tab.Algae=with(subset(Video.habitat,BROAD=='Macroalgae'),table(Algae.morph,TYPE))
  colnames(Tab.Algae)=rep('',ncol(Tab.Algae))
  id=which(rowSums(Tab.Algae)==0)
  if(length(id>0))Tab.Algae=Tab.Algae[-id,]
  mosaicplot(Tab.Algae,main="",ylab='',xlab='',cex.axis=1.2,
             color=c("brown", "forestgreen", "red"))
  mtext('Macroalgae',1,las=1,line=0.75,cex=1.1)
  
}
dev.off()


# 2. Underwater observations of habitat damage  
Net.damage.underwater=Video.net.interaction%>%
                mutate(Habitat.damage=ifelse(
                            SP.group%in%c("Macro algae","Rock/reef structure","Sponges"),"Damage",
                            "No damage"))%>%
                dplyr::select(sheet_no,Camera,Habitat.damage)
Damage.events=Net.damage.underwater%>%
                group_by(Habitat.damage)%>%tally()
Total.observed.metres=length(unique(Video.net.interaction$OpCode))*metres.observed

text = paste(Damage.events%>%filter(Habitat.damage=='Damage')%>%pull(n),
             "habitat damage events in\n",
             Total.observed.metres,
             "metres of nets observed with \n",
             "  underwater cameras")
p1=ggplot() + 
  annotate("text", x = 4, y = 25, size=7, label = text) + 
  ggtitle(label ="Underwater cameras")+
  theme_void()+
  theme(plot.title = element_text(size = 20, face = "bold"))


# 3. Deck camera Habitat interactions 
Video.habitat.deck=Video.habitat.deck%>%
          data.frame%>%
          mutate(Period=tolower(Period),
                 SHEET_NO=case_when(Period=='gillnet'~str_remove(word(DIPRD.code,1,sep = "\\/"),'GN'),
                                    Period=='longline'~str_remove(word(DIPRD.code,2,sep = "\\/"),'LL')),
                 Percentage.cover=ifelse(Percentage.cover=='<1',1,Percentage.cover),
                 Percentage.cover=as.numeric(Percentage.cover),
                 Species=capitalize(tolower(Species)),
                 Frame_sheet=paste(SHEET_NO,Frame))%>%
        left_join(DATA%>%
                    distinct(sheet_no,.keep_all=T)%>%
                    dplyr::select(sheet_no,net_length),
                  by=c("SHEET_NO"="sheet_no"))%>%
        mutate(net_length=net_length*1000,      #add net length in m
               Species=ifelse(Species=="Radiata","Macroalgae",Species))    #group Eklonia with macroalgae

Video.habitat.deck_GN=Video.habitat.deck%>%
                filter(Period=='gillnet')%>%
                group_by(Frame,SHEET_NO,net_length,Species)%>%
                summarise(Percentage.cover=sum(Percentage.cover))%>%
                group_by(Frame,SHEET_NO,net_length)%>%
                mutate(Total.damage=sum(Percentage.cover))%>%
                spread(Species,Percentage.cover,fill = 0)%>%
                mutate(Dist.roller.spreader=distance.roller.spreader.Anthony)%>%
                data.frame
                
#add 0 habitat damage frames
habitat.sheet_GN=unique(Video.habitat.deck_GN$SHEET_NO)
dummy=vector('list',length(habitat.sheet_GN))
for(l in 1:length(habitat.sheet_GN))
{
  a=subset(Video.habitat.deck_GN,SHEET_NO==habitat.sheet_GN[l])
  n.exp=floor(a$net_length[1]/a$Dist.roller.spreader[1])
  Plus.row=n.exp-nrow(a)
  PLUS=a[1:Plus.row,]
  PLUS[,]=0
  PLUS$SHEET_NO=a$SHEET_NO[1]
  PLUS$Frame='dummy'
  PLUS$net_length=a$net_length[1]
  PLUS$Dist.roller.spreader=a$Dist.roller.spreader[1]
  a$Frame=as.character(a$Frame)
  dummy[[l]]=rbind(a,PLUS)
}
Video.habitat.deck_GN.no.zeros=Video.habitat.deck_GN
Video.habitat.deck_GN=do.call(rbind,dummy)            

#exploratory stuff
fn.hist=function(x,Main) hist(x,col=2,main=Main,xlab="",ylab="")
fn.hab.explr=function(d.no_zeros,d)
{
  par(mfrow=c(5,2),mar=c(2,2,1,.1),oma=c(2,2,3,.1),mgp=c(1.5,.6,0))
  fn.hist(d$Macroalgae,"Macroalgae")
  mtext("With 0s",3,line=1.5,col="steelblue",cex=1.5)
  fn.hist(d.no_zeros$Macroalgae,"Macroalgae")
  mtext("Without 0s",3,line=1.5,col="steelblue",cex=1.5)
  fn.hist(d$Coral,"Coral"); fn.hist(d.no_zeros$Coral,"Coral")
  fn.hist(d$Rock,"Rock"); fn.hist(d.no_zeros$Rock,"Rock")
  fn.hist(d$Seagrass,"Seagrass"); fn.hist(d.no_zeros$Seagrass,"Seagrass")
  fn.hist(d$Sponge,"Sponge"); fn.hist(d.no_zeros$Sponge,"Sponge")
  mtext('Percentage damage',1,outer=T,cex=1.25)
  mtext('Frequency',2,outer=T,cex=1.25,las=3)
  
  par(mfrow=c(1,2),mar=c(2,2,1,.1),oma=c(2,2,3,.1))
  fn.hist(d$Total.damage,"Total.damage")
  mtext("With 0s",3,line=1.5,col="steelblue",cex=1.5)
  fn.hist(d.no_zeros$Total.damage,"Total.damage")
  mtext("Without 0s",3,line=1.5,col="steelblue",cex=1.5)
  mtext('Percentage damage',1,outer=T,cex=1.25)
  mtext('Frequency',2,outer=T,cex=1.25,las=3)
}

pdf(le.paste("Video/deck.cameras/Habitat.damage_explore.pdf"))
fn.hab.explr(d.no_zeros=Video.habitat.deck_GN.no.zeros,d=Video.habitat.deck_GN)
dev.off()


#Pie chart of frame with damage / no damage
p2=Video.habitat.deck_GN%>%
  mutate(Damage=ifelse(Total.damage>0,"Damage","No damage"))%>%
  group_by(Damage)%>%
  tally()%>%
  mutate(Percent=100*round(n/sum(n),2),
         ymax=cumsum(Percent),
         ymin = c(0, head(ymax, n=-1)),
         labelPosition =(ymax + ymin) / 2)%>%
  ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Damage)) +
  geom_rect() +
  geom_text(x=3.5,aes(y=labelPosition, label=paste(Percent,"%")),size = 8,check_overlap = TRUE)+
  scale_fill_manual(values=c("brown2","steelblue"))+ 
  coord_polar(theta="y") + xlim(c(2, 4)) + 
  ggtitle(label ="Deck cameras",
          subtitle = "Percentage of frames (4.3 m of net) with damage")+
  theme_void() +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold",hjust=-920),
        plot.subtitle = element_text(size = 17,face = "italic",hjust=0.3))

#Pie chart of different habitat damage categories
p3=Video.habitat.deck_GN.no.zeros%>%
  dplyr::select(-Frame,-Total.damage,-SHEET_NO,-net_length,-Dist.roller.spreader)%>%
  gather(Species,Percent)%>%
  group_by(Species)%>%
  summarise(Percent=sum(Percent))%>%
  mutate(label=paste(round(100*Percent/sum(Percent),1),'%'),
         ymax=cumsum(Percent),
         ymin = c(0, head(ymax, n=-1)),
         labelPosition =(ymax + ymin) / 2)%>%
  ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Species)) +
  geom_rect() +
  geom_text(x=3.5,aes(y=labelPosition, label=label,angle = 70),size = 4,check_overlap = TRUE)+
  scale_fill_manual(values=c("deepskyblue2","darkorange1","tomato4","forestgreen",'khaki3'))+ 
  coord_polar(theta="y") + xlim(c(2, 4)) +
  ggtitle(label ="",
          subtitle = "Habitat damage categories")+
  theme_void() +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size = 17,face = "italic",hjust=1))

#Histogram of proportion of net damage
p4=Video.habitat.deck_GN.no.zeros%>%
        dplyr::select(-Frame,-Total.damage,-SHEET_NO,-net_length,-Dist.roller.spreader)%>%
        gather(Species,Percent)%>%
        filter(Percent>0)%>%
        mutate(Percent.bin=5*round(Percent/5))%>%
        ggplot( aes(x=Percent.bin)) +
        geom_histogram( fill="red",col="black", alpha=0.9,binwidth = 5)+
        ggtitle(label ="",
                subtitle = "Percentage damage per frame")+
        theme_classic()+
        theme(axis.text=element_text(size=14),
              axis.title=element_text(size=16),
              plot.title = element_text(size = 20, face = "bold"),
              plot.subtitle = element_text(size = 17,face = "italic",hjust=-.15))+
        xlab("Percentage")+
        ylab("Number of frames")

#export these figures
ggarrange(p1, p2, p3, p4, ncol = 1, nrow = 4)
ggsave(le.paste("Video/Habitat.interactions.tiff"),width = 6,height = 10,compression = "lzw")



#---------Dropouts, Gaffing & Position in water column (catch around weight or float) ---------
Video.camera2.deck=Video.camera2.deck%>%
  data.frame%>%
  mutate(Period=tolower(Period),
         SHEET_NO=case_when(Period=='gillnet'~str_remove(word(DPIRD.code,1,sep = "\\/"),'GN'),
                            Period=='longline'~str_remove(word(DPIRD.code,2,sep = "\\/"),'LL')),
         dropout=ifelse(is.na(dropout),'No',dropout))

Video.camera2.deck_observations=Video.camera2.deck_observations%>%  
  data.frame%>%
  mutate(Period=tolower(Period),
         SHEET_NO=case_when(Period=='gillnet'~str_remove(word(DPIRD.code,1,sep = "\\/"),'GN'),
                            Period=='longline'~str_remove(word(DPIRD.code,2,sep = "\\/"),'LL')),
         Activity=ifelse(grepl("feeding",Video.camera2.deck_observations$comment),paste("feeding from",Period),Activity))


Video.subsurface=Video.subsurface%>%  #MISSING: add this data set to calculation of drop out rates
  data.frame%>%
  mutate(Period=tolower(Period),
         SHEET_NO=case_when(Period=='gillnet'~str_remove(word(DPIRD.code,1,sep = "\\/"),'GN'),
                            Period=='longline'~str_remove(word(DPIRD.code,2,sep = "\\/"),'LL')))
Video.subsurface.comments=Video.subsurface.comments%>%  
  data.frame%>%
  mutate(Period=tolower(Period),
         SHEET_NO=case_when(Period=='gillnet'~str_remove(word(DPIRD.code,1,sep = "\\/"),'GN'),
                            Period=='longline'~str_remove(word(DPIRD.code,2,sep = "\\/"),'LL')))


  #1. Dropouts
these.drop.out.sp=c(37017001,37017003,37018001,37018003,37018007,37018023,37019004,
                    37353001,37320004,37384002,37377004,37384039)
names(these.drop.out.sp)=All.species.names[match(these.drop.out.sp,All.species.names$Code),"COMMON_NAME"]
Video.camera2.deck%>%
  filter(Code%in%these.drop.out.sp)%>%
  group_by(Code,dropout,Period)%>%
  tally()%>%
  mutate(dropout=factor(dropout),
         Period=capitalize(Period))%>%
  left_join(All.species.names%>%dplyr::select(COMMON_NAME,Code),by="Code")%>%
  mutate(COMMON_NAME=factor(COMMON_NAME,levels=names(these.drop.out.sp)))%>%
  ggplot(aes(fill=dropout, y=n, x=COMMON_NAME)) + 
  geom_bar(position="stack", stat="identity")+
  coord_flip() +
  facet_wrap(~Period,dir='h',scales='free_x')+ 
  theme(legend.position = "top",
        strip.text = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16))+
  scale_fill_manual(values=c("deepskyblue3","firebrick3"))+
  xlab('')+ylab('Number of events')
ggsave(le.paste("Video/deck.cameras/Drop.out.events.tiff"),width = 10,height = 8,compression = "lzw")


  #2. Gaffing
Video.camera2.deck%>%
  filter(Code%in%these.drop.out.sp)%>%
  mutate(gaffed=case_when(gaffed=='No' ~  'Lost',
                          is.na(gaffed) ~ 'No',
                          TRUE ~ gaffed))%>%    
  group_by(Code,gaffed,Period)%>%
  tally()%>%
  mutate(gaffed=factor(gaffed),
         Period=capitalize(Period))%>%
  left_join(All.species.names%>%dplyr::select(COMMON_NAME,Code),by="Code")%>%
  mutate(COMMON_NAME=factor(COMMON_NAME,levels=names(these.drop.out.sp)))%>%
  ggplot(aes(fill=gaffed, y=n, x=COMMON_NAME)) + 
  geom_bar(position="stack", stat="identity")+
  coord_flip() +
  facet_wrap(~Period,dir='h',scales='free_x')+ 
  theme(legend.position = "top",
        strip.text = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16))+
  scale_fill_manual(values=c("firebrick3","deepskyblue3","orange"))+
  xlab('')+ylab('Number of events')
ggsave(le.paste("Video/deck.cameras/Gaffing.events.tiff"),width = 10,height = 8,compression = "lzw")


  #3. Composition around weight or float
Min.obs.comp.wei.flot=5
Data_water.column=Video.camera2.deck%>%
            filter(!SHEET_NO%in%No.good.water.column)%>%
            mutate(water.column=tolower(hook.distance.to.float.weight))%>%
            filter(!is.na(water.column))
  
these.compo.sp=Data_water.column%>%
  group_by(Code)%>%
  tally()%>%
  filter(n>=Min.obs.comp.wei.flot)%>%
  pull(Code)

Data_water.column=Data_water.column%>%
            group_by(Code,water.column,Period)%>%
            tally()%>%
            mutate(water.column=factor(water.column,levels=c("1w","2w","3w","1f","2f","3f")),
                   Period=capitalize(Period))%>%
            left_join(All.species.names%>%dplyr::select(COMMON_NAME,Code),by="Code")

names(these.compo.sp)=All.species.names[match(these.compo.sp,All.species.names$Code),"COMMON_NAME"]
Data_water.column%>%
  filter(Code%in%these.compo.sp)%>%
  mutate(COMMON_NAME=factor(COMMON_NAME,levels=names(these.compo.sp)))%>%
  ggplot(aes(fill=water.column, y=n, x=COMMON_NAME)) + 
  geom_bar(position="stack", stat="identity")+
  coord_flip() +
  facet_wrap(~Period,dir='h',scales='free_x')+ 
  theme(legend.position = "top",
        strip.text = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        axis.text=element_text(size=16),
        axis.title=element_text(size=18))+
  scale_fill_manual(values=c("orange","firebrick2","firebrick4","lightblue2","deepskyblue2","dodgerblue4"))+
  xlab('')+ylab('Number of events')+ 
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))
ggsave(le.paste("Video/deck.cameras/Weight_float_species.events.tiff"),width = 10,height = 8,compression = "lzw")



#---------Analysis of Deck 1 camera VS observers --------------------------------------------------------------------

# Deck 1  (pointing to deck)                      
Video.camera1=rbind(Video.camera1.deck_extra.records%>%
                      dplyr::select("DIPRD code",Code,Period,number),
                    Video.camera1.deck%>%
                      mutate(Code=ifelse(Genus=="Kyphosus" & Species=="spp",'37361903',Code))%>%
                      dplyr::select("DIPRD code",Code,Period,number))%>%
              data.frame%>%
              mutate(Code=as.numeric(Code),
                     SP.group=case_when(Code >=3.7e7 & Code<=3.70241e7 ~"Sharks",
                                        Code >3.7025e7 & Code<=3.7041e7 ~"Rays",
                                        Code >=3.7042e7 & Code<=3.7044e7 ~"Chimaeras",
                                        Code >=3.7046e7 & Code<=3.747e7 ~"Scalefish",
                                        Code >=4.1e+07 & Code<=4.115e+07 ~"Marine mammals",
                                        Code >=4.0e+07 & Code<4.1e+07 ~"Seabirds",
                                        Code >=1.2e7 & Code<3.7e7 ~"Invertebrates",
                                        Code >=1.1e7 & Code<1.2e7 ~"Rock/reef structure",
                                        Code >=5.4e7 & Code<5.49e7 ~"Macroalgae",
                                        Code == 10000910 ~"Sponges"),
                     Period=tolower(Period),
                     Period=ifelse(Period=='gn','gillnet',
                                   ifelse(Period=='ll','longline',Period)),
                     sheet_no=case_when(Period=='gillnet'~str_remove(word(DIPRD.code,1,sep = "\\/"),'GN'),
                                        Period=='longline'~str_remove(word(DIPRD.code,2,sep = "\\/"),'LL')))%>%
              left_join(DATA_PA,by='sheet_no')%>%
              mutate(Data.set="camera")

#Export data for Abbey
Abbey.data.chapter.2=rbind(Video.camera1%>%
                             rename(Number=number)%>%
                             mutate(Method=capitalize(Period))%>%
                             filter(Code>=3.7e7 & Code<=3.747e7)%>%  #compare only fish and sharks
                             dplyr::select(sheet_no,Method,Data.set,Code,SP.group,Number),
                           DATA%>%
                             filter(sheet_no%in%unique(Video.camera1$sheet_no))%>%
                             left_join(All.species.names%>%
                                         dplyr::select(Species,Code),
                                       by=c('species'='Species'))%>%
                             filter(Code>=3.7e7 & Code<=3.747e7)%>%     #compare only fish and sharks
                             mutate(Number=1,
                                    Data.set='Onboard.observer',
                                    Method=ifelse(method=="GN","Gillnet",
                                                  ifelse(method=="LL","Longline",
                                                         NA)),
                                    SP.group=case_when(Code >=3.7e7 & Code<=3.70241e7 ~"Sharks",
                                                       Code >3.7025e7 & Code<=3.7041e7 ~"Rays",
                                                       Code >=3.7042e7 & Code<=3.7044e7 ~"Chimaeras",
                                                       Code >=3.7046e7 & Code<=3.747e7 ~"Scalefish"))%>%
                             dplyr::select(sheet_no,Method,Data.set,Code,SP.group,Number))


add.effort=DATA%>%
            filter(sheet_no%in%unique(Abbey.data.chapter.2$sheet_no))%>%
            mutate(Effort=ifelse(method=="GN",soak.time*net_length,
                                 ifelse(method=="LL",soak.time*n.hooks,
                                        NA)))%>%
            group_by(sheet_no,method)%>%
            summarise(Effort=max(Effort))%>%
            mutate(Method=ifelse(method=="GN","Gillnet",
                                 ifelse(method=="LL","Longline",
                                        NA)))%>%
            rename(Fishing.effort=Effort)%>%
            dplyr::select(-method)


Abbey.data.chapter.2_species=Abbey.data.chapter.2%>%
            group_by(sheet_no,Method,Data.set,Code)%>%
            summarise(Number=sum(Number))%>%
            spread(Code,Number,fill = 0)%>%
            data.frame%>%
            left_join(add.effort,by=c('sheet_no','Method'))



Abbey.data.chapter.2_SP.group=Abbey.data.chapter.2%>%
              group_by(sheet_no,Method,Data.set,SP.group)%>%
              summarise(Number=sum(Number))%>%
              spread(SP.group,Number,fill = 0)%>%
              data.frame%>%
              left_join(add.effort,by=c('sheet_no','Method'))


Abbey.data.chapter.2_Retain.group=Abbey.data.chapter.2%>%
  left_join(Retained.tabl,by='Code')%>%
  mutate(Retain.group=
           ifelse(retained=='Yes' & SP.group%in% c('Sharks','Rays'),"Retained elasmobranch",
           ifelse(retained=='No' & SP.group%in% c('Sharks','Rays'),"Discarded elasmobranch",
           ifelse(retained=='Yes' & SP.group%in% c('Scalefish'),"Retained scalefish",   
           ifelse(retained=='No' & SP.group%in% c('Scalefish'),"Discarded scalefish",
           NA)))))%>%
  group_by(sheet_no,Method,Data.set,Retain.group)%>%
  summarise(Number=sum(Number))%>%
  filter(!is.na(Retain.group))%>%
  spread(Retain.group,Number,fill = 0)%>%
  data.frame%>%
  left_join(add.effort,by=c('sheet_no','Method'))

write.csv(Abbey.data.chapter.2_species,handl_OneDrive('Analyses/Parks Australia/outputs/Data for Abbey/Abbey.data.chapter.2_species.csv'),row.names = F)
write.csv(Abbey.data.chapter.2_SP.group,handl_OneDrive('Analyses/Parks Australia/outputs/Data for Abbey/Abbey.data.chapter.2_SP.group.csv'),row.names = F)
write.csv(Abbey.data.chapter.2_Retain.group,handl_OneDrive('Analyses/Parks Australia/outputs/Data for Abbey/Abbey.data.chapter.2_Retain.group.csv'),row.names = F)


#---------Analyse Abbey ------------

fn.get.top.1=function(d,N)
{
  d=colSums(d%>%dplyr::select(-sheet_no,-Method,-Interaction,-n.cameras))
  out=names(d[d>N])
}
fn.explr.Abbey.1=function(d,vars)
{
  d1=d[,vars]/d$Eff
  d1%>%
    mutate(sheet_no=d$sheet_no,
           Method=d$Method,
           Interaction=d$Interaction)%>%
    gather("Species","cpue",-sheet_no,-Method,-Interaction)%>%
    filter(cpue>0)%>%
    mutate(log.cpue=log(cpue))%>%
    ggplot(aes(x=Method, y=log.cpue, fill=Interaction)) + 
    geom_boxplot()+
    facet_wrap(~Species, scale="free")
}

fn.get.top.2=function(d,N)
{
  d=colSums(d%>%dplyr::select(-sheet_no,-Method,-Data.set,-Fishing.effort))
  out=names(d[d>N])
}
fn.explr.Abbey.2=function(d,vars)
{
  d1=d[,vars]/d$Eff
  d1%>%
    mutate(sheet_no=d$sheet_no,
           Method=d$Method,
           Data.set=d$Data.set)%>%
    gather("Species","cpue",-sheet_no,-Method,-Data.set)%>%
    filter(cpue>0)%>%
    mutate(log.cpue=log(cpue))%>%
    ggplot(aes(x=Method, y=log.cpue, fill=Data.set)) + 
    geom_boxplot()+
    facet_wrap(~Species, scale="free")
}
pdf(file=handl_OneDrive('Analyses/Parks Australia/outputs/Data for Abbey/Exploraty.pdf'))

fn.explr.Abbey.1(d=Abbey.data.chapter.1_species%>%rename(Eff=n.cameras),
                 vars=fn.get.top.1(d=Abbey.data.chapter.1_species,N=250))

fn.explr.Abbey.1(d=Abbey.data.chapter.1_SP.group%>%rename(Eff=n.cameras),
                 vars=fn.get.top.1(d=Abbey.data.chapter.1_SP.group,N=30))


fn.explr.Abbey.2(d=Abbey.data.chapter.2_species%>%rename(Eff=Fishing.effort),
                 vars=fn.get.top.2(d=Abbey.data.chapter.2_species,N=100))

fn.explr.Abbey.2(d=Abbey.data.chapter.2_SP.group%>%rename(Eff=Fishing.effort),
                 vars=fn.get.top.2(d=Abbey.data.chapter.2_SP.group,N=30))

dev.off()


#---------Analyse PA TEPS ------------

#2. Cameras

# 2.1. Underwater: number of events 
d=rbind(Video.longline.interaction%>%dplyr::select(Method,Interaction,Number,SP.group,Species,Code),
        Video.net.interaction%>%dplyr::select(Method,Interaction,Number,SP.group,Species,Code))%>%
  mutate(Number=1,
         Method=capitalize(Method))%>%
  filter(SP.group%in%TEP.groups | Code%in%TEPS_Shark.rays)%>%
  filter(!Species=='birds feeding at surface')%>%
  left_join(TEPS.names,by="Code")%>%
  group_by(Method,Interaction,Name,Colr,Code)%>%
  tally(Number)%>%
  filter(!is.na(Interaction))%>%
  mutate(Interaction=capitalize(tolower(Interaction)))

d=d%>%
  mutate(Method.hour=ifelse(Method=='Longline',paste(Method,' ( ',hours.underwater.ll,' video hours)',sep=''),
                            ifelse(Method=='Gillnet',paste(Method,' ( ',hours.underwater.gn,' video hours)',sep=''),
                                   NA)),
         Interaction=case_when(Interaction=="Avoid" ~ "Avoidance",
                               Interaction=="Bounce off" ~ "Bounced off",
                               Interaction== "Swim past" ~ "Swam past",
                               Interaction== "Swim through" ~ "Swam through",
                               TRUE~Interaction))  
#gillnet and longline
d%>%
  mutate(Name=factor(Name,levels=TEPS.names$Name))%>%
  ggplot(aes(fill=Name, y=n, x=Interaction)) + 
  geom_bar(position="stack", stat="identity")+
  coord_flip() +
  facet_wrap(~Method.hour,dir='h')+ 
  theme(legend.position = "top",
        strip.text = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        axis.text=element_text(size=18),
        axis.title=element_text(size=20))+
  xlab('')+ylab('Number of events')+ guides(color = guide_legend(nrow = 1))+
  scale_fill_manual(values=TEPS.cols)+
  scale_y_continuous(breaks = integer_breaks())
ggsave(le.paste("TEPS/Interactions_number.events_underwater.tiff"),width = 12,height = 8,compression = "lzw")

#gillnet only
d%>%
  filter(Method=="Gillnet")%>%
  mutate(Name=factor(Name,levels=TEPS.names$Name))%>%
  ggplot(aes(fill=Name, y=n, x=Interaction)) + 
  geom_bar(position="stack", stat="identity")+
  coord_flip() + 
  theme(legend.position = "top",
        strip.text = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        axis.text=element_text(size=18),
        axis.title=element_text(size=20))+
  xlab('')+ylab('Number of events')+ guides(color = guide_legend(nrow = 1))+
  scale_fill_manual(values=TEPS.cols)+
  scale_y_continuous(breaks = integer_breaks())
ggsave(le.paste("TEPS/Interactions_number.events_underwater_gillnet.only.tiff"),width = 12,height = 8,compression = "lzw")


# 2.2. subsurface   
d=Video.subsurface%>%
  filter(Code%in%TEPS.codes)%>%
  left_join(TEPS.names,by="Code")%>%
  mutate(Number=1,
         Period=capitalize(Period),
         Drop.out=capitalize(tolower(Drop.out)),
         Dropout.condition=tolower(Dropout.condition),
         Dropout.condition=case_when(is.na(Dropout.condition)~'',
                                     TRUE~Dropout.condition),
         dummy=case_when(Drop.out=="Yes"~"Dropped out",
                         Drop.out=="No"~"Caught"),
         Drop.out.plot=paste(dummy,Dropout.condition))%>%
  group_by(Period,Drop.out.plot,Name,Colr,Code)%>%
  tally(Number)%>%
  mutate(Method.hour=ifelse(Period=='Longline',paste(Period,' ( ',hours.deck2.ll,' video hours)',sep=''),
                            ifelse(Period=='Gillnet',paste(Period,' ( ',hours.deck2.gn,' video hours)',sep=''),
                                   NA)))%>%
  mutate(Name=factor(Name,levels=TEPS.names$Name))

#gillnet & longline
d%>%
  ggplot(aes(x=Drop.out.plot, y=n,fill=Name)) + 
  geom_bar(position="stack", stat="identity")+
  coord_flip() +
  facet_wrap(~Method.hour,dir='h')+ 
  theme(legend.position = "top",
        strip.text = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        axis.text=element_text(size=18),
        axis.title=element_text(size=20))+
  xlab('')+ylab('Number of events')+ guides(color = guide_legend(nrow = 1))+
  scale_fill_manual(values=TEPS.cols)+scale_y_continuous(breaks = integer_breaks())
ggsave(le.paste("TEPS/Interactions_number.events_subsurface.tiff"),width = 12,height = 8,compression = "lzw")

#gillnet only
d%>%
  filter(Period=="Gillnet")%>%
  ggplot(aes(x=Drop.out.plot, y=n,fill=Name)) + 
  geom_bar(position="stack", stat="identity")+
  coord_flip() +
  theme(legend.position = "top",
        strip.text = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        axis.text=element_text(size=18),
        axis.title=element_text(size=20))+
  xlab('')+ylab('Number of events')+ guides(color = guide_legend(nrow = 1))+
  scale_fill_manual(values=TEPS.cols)+scale_y_continuous(breaks = integer_breaks())
ggsave(le.paste("TEPS/Interactions_number.events_subsurface_gillnet.only.tiff"),width = 12,height = 8,compression = "lzw")



# 2.3.1 Deck 2  (pointing to roller)
TEPS_deck.camera2=rbind(Video.camera2.deck_observations%>%
                          filter(Code%in%TEPS.codes)%>%
                          dplyr::select(Period,Genus,Species,Code,Activity),
                        Video.camera2.deck%>%
                          filter(Code%in%TEPS.codes)%>%
                          mutate(Activity=ifelse(dropout=='Yes' & gaffed=="No","Drop out",
                                                 ifelse(dropout=='Yes' & gaffed=="Yes","Drop out and gaffed",
                                                        NA)))%>%
                          dplyr::select(Period,Genus,Species,Code,Activity))%>%
  filter(!is.na(Activity))%>%
  left_join(TEPS.names,by="Code")%>%
  mutate(Number=1,
         Period=capitalize(Period),
         Activity=capitalize(Activity))%>%
  group_by(Period,Activity,Name,Colr,Code)%>%
  tally(Number)%>%
  mutate(Method.hour=ifelse(Period=='Longline',paste(Period,' ( ',hours.deck2.ll,' video hours)',sep=''),
                            ifelse(Period=='Gillnet',paste(Period,' ( ',hours.deck2.gn,' video hours)',sep=''),
                                   NA)),
         Activity=case_when(Activity%in%c("Feeding from gillnet",
                                          "Feeding from longline")~"Feeding from net/longline",
                            TRUE~Activity))  
d=TEPS_deck.camera2%>%
    mutate(Name=factor(Name,levels=TEPS.names$Name))

#gillnet & longline
d%>%
  ggplot(aes(fill=Name, y=n, x=Activity)) + 
  geom_bar(position="stack", stat="identity")+
  coord_flip() +
  facet_wrap(~Method.hour,dir='h')+ 
  theme(legend.position = "top",
        strip.text = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        axis.text=element_text(size=18),
        axis.title=element_text(size=20))+
  xlab('')+ylab('Number of events')+ guides(color = guide_legend(nrow = 1))+
  scale_fill_manual(values=TEPS.cols)+scale_y_continuous(breaks = integer_breaks())
ggsave(le.paste("TEPS/Interactions_number.events_deck2.tiff"),width = 12,height = 8,compression = "lzw")

#gillnet only
d%>%
  filter(Period=="Gillnet")%>%
  mutate(Activity=ifelse(Activity=="Feeding from net/longline" & Period=="Gillnet",
                         "Feeding from net",Activity))%>%
  ggplot(aes(fill=Name, y=n, x=Activity)) + 
  geom_bar(position="stack", stat="identity")+
  coord_flip() +
  theme(legend.position = "top",
        strip.text = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        axis.text=element_text(size=18),
        axis.title=element_text(size=20))+
  xlab('')+ylab('Number of events')+ guides(color = guide_legend(nrow = 1))+
  scale_fill_manual(values=TEPS.cols)+scale_y_continuous(breaks = integer_breaks())
ggsave(le.paste("TEPS/Interactions_number.events_deck2_gillnet.only.tiff"),width = 12,height = 8,compression = "lzw")



#2. Observer data
fn.tep.observer=function(d,all.gn.shots,all.ll.shots,do.LL) 
{
  d.nets=d%>%filter(gear.type=="GN")
  d.ll=d%>%filter(gear.type=="LL")
  
  
  #Add species contact group
  Drop= paste(c("diving for and"), collapse = "|")
  d.nets=d.nets%>%
    mutate(contact.code.to.complete= str_remove_all(contact.code.to.complete, Drop),
           con.code.sp=paste(capitalize(common.name),contact.code.to.complete),
           con.code.sp=ifelse(con.code.sp=="NA NA",NA,con.code.sp))
  
  d.ll=d.ll%>%
    mutate(contact.code.to.complete= str_remove_all(contact.code.to.complete, Drop),
           con.code.sp=paste(capitalize(common.name),contact.code.to.complete),
           con.code.sp=ifelse(con.code.sp=="NA NA",NA,con.code.sp))
  
  if(do.LL=="YES")
  {
    rbind(d.nets,d.ll)%>%
      group_by(gear.type,contact.code.to.complete,common.name)%>%
      summarise(n=sum(contact.count))%>%
      mutate(common.name=capitalize(common.name),
             contact.code.to.complete=capitalize(contact.code.to.complete),
             gear.type=ifelse(gear.type=="LL",paste("Longline"," (",all.ll.shots, " shots)",sep=''),
                              ifelse(gear.type=="GN",paste("Gillnet"," (",all.gn.shots, " shots)",sep=''),
                                     NA)))%>%
      mutate(common.name=factor(common.name,levels=TEPS.names$Name))%>%
      ggplot(aes(y=n, x=contact.code.to.complete,fill=common.name)) + 
      geom_bar(position="stack", stat="identity")+
      coord_flip() + labs(fill = "") +
      facet_wrap(~gear.type,dir='h')+ 
      theme(legend.position = "top",
            legend.text = element_text(size = 16),
            strip.text = element_text(size = 14),
            axis.text=element_text(size=16),
            axis.title=element_text(size=20))+
      xlab('')+ylab('Number of individuals')+ 
      scale_fill_manual(values=TEPS.cols)+
      #guides(color = guide_legend(nrow = 1))+
      scale_y_continuous(breaks=1:100)
  }else
  {
    d.nets%>%
      mutate(contact.code.to.complete=case_when(contact.code.to.complete=="entangled/hooked" ~ "entangled",
                                                contact.code.to.complete=="feeding from net/longline" ~ "feeding from net",
                                                TRUE ~ contact.code.to.complete))%>%
      group_by(gear.type,contact.code.to.complete,common.name)%>%
      summarise(n=sum(contact.count))%>%
      mutate(common.name=capitalize(common.name),
             contact.code.to.complete=capitalize(contact.code.to.complete),
             gear.type=ifelse(gear.type=="LL",paste("Longline"," (",all.ll.shots, " shots)",sep=''),
                              ifelse(gear.type=="GN",paste("Gillnet"," (",all.gn.shots, " shots)",sep=''),
                                     NA)))%>%
      mutate(common.name=factor(common.name,levels=TEPS.names$Name))%>%
      ggplot(aes(y=n, x=contact.code.to.complete,fill=common.name)) + 
      geom_bar(position="stack", stat="identity")+
      coord_flip() + labs(fill = "")+ 
      theme(legend.position = "top",
            legend.text = element_text(size = 16),
            strip.text = element_text(size = 14),
            axis.text=element_text(size=16),
            axis.title=element_text(size=20))+
      xlab('')+ylab('Number of individuals')+ 
      scale_fill_manual(values=TEPS.cols)+
      #guides(color = guide_legend(nrow = 1))+
      scale_y_continuous(breaks=1:100)
  }
    
}
#gillnet & hook
fn.tep.observer(d=TEPS,
                all.gn.shots=length(DATA%>%
                                      filter(method=="GN")%>%
                                      distinct(sheet_no)%>%
                                      pull(sheet_no)),
                all.ll.shots=length(DATA%>%
                                      filter(method=="LL")%>%
                                      distinct(sheet_no)%>%
                                      pull(sheet_no)),
                do.LL="YES")
ggsave(le.paste("TEPS/Numbers.interactions.by.gear_Observers.tiff"), width = 12,height = 8,compression = "lzw")

#gillnet only
fn.tep.observer(d=TEPS,
                all.gn.shots=length(DATA%>%
                                      filter(method=="GN")%>%
                                      distinct(sheet_no)%>%
                                      pull(sheet_no)),
                all.ll.shots=length(DATA%>%
                                      filter(method=="LL")%>%
                                      distinct(sheet_no)%>%
                                      pull(sheet_no)),
                do.LL="NO")
ggsave(le.paste("TEPS/Numbers.interactions.by.gear_Observers_gillnet.only.tiff"), width = 12,height = 8,compression = "lzw")


do.inter.for.each.shot=FALSE
if(do.inter.for.each.shot)
{
  fn.tep.barplot=function(d,all.gn.shots,all.ll.shots) 
  {
    d.nets=d%>%filter(gear.type=="GN")
    d.ll=d%>%filter(gear.type=="LL")
    
    #add 0 record if no TEPS
    #gillnet
    no.teps=all.gn.shots[which(!all.gn.shots%in%unique(d.nets$sheet_no))]
    if(length(no.teps)>0)
    {
      add.d=d.nets[1:length(no.teps),]
      add.d[,]=NA
      add.d$sheet_no=no.teps
      d.nets=rbind(d.nets,add.d)
      Shots=data.frame(sheet_no=sort(unique(d.nets$sheet_no)))
      Shots$shot=1:nrow(Shots)
      d.nets=d.nets%>%
        left_join(Shots,by="sheet_no")
    }
    #lonline
    no.teps=all.ll.shots[which(!all.ll.shots%in%unique(d.ll$sheet_no))]
    if(length(no.teps)>0)
    {
      add.d=d.ll[1:length(no.teps),]
      add.d[,]=NA
      add.d$sheet_no=no.teps
      d.ll=rbind(d.ll,add.d)
      
      Shots=data.frame(sheet_no=sort(unique(d.ll$sheet_no)))
      Shots$shot=1:nrow(Shots)
      d.ll=d.ll%>%
        left_join(Shots,by="sheet_no")
    }
    
    #Add species contact group
    Drop= paste(c("/hooked","/longline"), collapse = "|")
    d.nets=d.nets%>%
      mutate(contact.code.to.complete= str_remove_all(contact.code.to.complete, Drop),
             con.code.sp=paste(capitalize(common.name),contact.code.to.complete),
             con.code.sp=ifelse(con.code.sp=="NA NA",NA,con.code.sp))
    
    Drop= paste(c("entangled/","net/"), collapse = "|")
    d.ll=d.ll%>%
      mutate(contact.code.to.complete= str_remove_all(contact.code.to.complete, Drop),
             con.code.sp=paste(capitalize(common.name),contact.code.to.complete),
             con.code.sp=ifelse(con.code.sp=="NA NA",NA,con.code.sp))
    
    
    #Plot gillnet
    p1=d.nets%>%
      group_by(shot,gear.type,con.code.sp)%>%
      tally()%>%
      mutate(n=ifelse(is.na(con.code.sp),0,n),)%>%
      ggplot(aes(fill=con.code.sp, y=n, x=shot)) + 
      geom_bar(position="stack", stat="identity")+
      scale_fill_discrete(na.translate=FALSE)+
      ylab('')+xlab('')+ labs(fill = "Gillnet")+
      theme(legend.position="top")+ 
      scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))
    
    
    
    #Plot longlines
    p2=d.ll%>%
      group_by(shot,gear.type,con.code.sp)%>%
      tally()%>%
      mutate(n=ifelse(is.na(con.code.sp),0,n),)%>%
      ggplot(aes(fill=con.code.sp, y=n, x=shot)) + 
      geom_bar(position="stack", stat="identity")+
      scale_fill_discrete(na.translate=FALSE)+
      ylab('')+xlab('')+ labs(fill = "Longline")+
      theme(legend.position="top")+ 
      scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))
    
    
    #export graphs
    infographic=grid.arrange(p1,p2, nrow = 2,ncol=1,heights=c(3,3))
    annotate_figure(infographic,
                    bottom = text_grob("Shot",size = 20),
                    left = text_grob("Number of interactions",rot = 90,size = 20))
    ggsave(le.paste("TEPS/Numbers.interactions.by.gear.tiff"), width = 12,
           height = 10,compression = "lzw")
    
    
    
  }
  fn.tep.barplot(d=TEPS,
                 all.gn.shots=DATA%>%
                   filter(method=="GN")%>%
                   distinct(sheet_no)%>%
                   pull(sheet_no),
                 all.ll.shots=DATA%>%
                   filter(method=="LL")%>%
                   distinct(sheet_no)%>%
                   pull(sheet_no))
}





#3. All TEP interactions togther (remove duplcations, e.g. observer vs Deck 1 or 2)   MISSING!!

