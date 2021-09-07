                # SCRIPT FOR ANALYSING AND REPORTING PARKS AUSTRALIA PROJECT 2019

# use 'explr.dat.entry' to check data entry errors

# to do: 
#   TEPS. Make sure there are no duplications between TEPS dataframe and DATA in #---------Analyse PA TEPS ------------
#   get number of hours of footage for different cameras from Jack
#   get field of view deck camera 1 Tim Goodall  for habitat recons from Jack
#   re run all (including mapping) once all data entered and verified. update which species to consider
#   Analyse PA socio-economic survey 
#   Analyse PA economics of Longline vs Gillnet: finish cost and profig
#   catch rates: consider testing fisher effect (Nils is less experienced)
rm(list=ls(all=TRUE))
library(tidyverse)
library(dplyr)
library(readxl)
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
library(visNetwork)
library(ozmaps)
library(sf)
library(ggpmisc)
library(forecast)
library(performance)

options(stringsAsFactors = FALSE,dplyr.summarise.inform = FALSE) 

#--------- DATA ------------
if(!exists('handl_OneDrive')) source('C:/Users/myb/OneDrive - Department of Primary Industries and Regional Development/Matias/Analyses/SOURCE_SCRIPTS/Git_other/handl_OneDrive.R')

source(handl_OneDrive("Analyses/Population dynamics/Git_Stock.assessments/NextGeneration.R"))

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
Video.habitat.LL<- read_excel(file.name.habitat, sheet = "longline habitat")


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


#9. PA - socio-economic survey
setwd(handl_OneDrive("Parks Australia/2019_project/Data/socio-economics"))
Survey.fishers<- read_excel("Questionnaire_fisher.xlsx", sheet = "questionnaire")
Survey.fishers.metadata<- read_excel("Questionnaire_fisher.xlsx", sheet = "metadata")
Survey.processor<- read_excel("Questionnaire_processor.xlsx", sheet = "questionnaire")
Survey.processor.metadata<- read_excel("Questionnaire_processor.xlsx", sheet = "metadata")
Additional.questions<- read_excel("Additional questions.xlsx", sheet = "data")
Additional.questions.metadata<- read_excel("Additional questions.xlsx", sheet = "metadata")


#10. Ports
Port.loc=read.csv(handl_OneDrive('Analyses/Parks Australia/outputs/Historic_catch_effort/Ports.csv'))


source(handl_OneDrive("Analyses/SOURCE_SCRIPTS/Git_Population.dynamics/fn.fig.R"))
Do.tiff="YES"
Do.jpeg="NO"
source(handl_OneDrive('Analyses/SOURCE_SCRIPTS/Git_other/ggplot.themes.R'))
source(handl_OneDrive("Analyses/SOURCE_SCRIPTS/Git_other/Smart_par.R"))
source(handl_OneDrive("Analyses/Population dynamics/Git_Stock.assessments/NextGeneration.R"))
source(handl_OneDrive("Analyses/Population dynamics/Git_Stock.assessments/SelnCurveDefinitions.R")) #These can be extended by the user



#---------CONTROL SECTION------------
explr.dat.entry=TRUE
do.len_len=FALSE
do.Historic=FALSE

distance.roller.spreader.Anthony=4.3  #in metres
distance.roller.spreader.Tim=NA  #in metres
distance.roller.spreader.Nils=4  #in metres

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
Length.weight.main=data.frame(common_name=Main.species,
                         a_weight=c(1.23e-5,1.63e-5,1.36e-6,6e-6,
                                    1.627539e-05,1.858271e-05,2.451526e-05,1.637477e-05),
                         b_weight=c(2.855,2.733,3.224263,2.9698,
                                    2.924,3.034411,2.91928,2.9251),
                         a_FL.to.TL=c(1.2062,1.0700731,1.0952846,1.1256996,
                                   rep(NA,4)),
                         b_FL.to.TL=c(1.9133,7.818082,3.8499,6.182718,
                                   rep(NA,4))) #Norris et al 2016 & Smallwood et al 2018 for scalefish

Length.weight.discards=data.frame(common_name=c("Buffalo bream","Dusky morwong",
                                                "Port Jackson","Wobbegongs",
                                                "Fiddler ray","Southern eagle ray","Stingrays"),
                                  a_weight=c(2.261e-5,2e-5,
                                             1.630e-7,3.645e-3,
                                             1.3e-6,3.6e-06,2.03e-05),
                                  b_weight=c(3.055,2.902,
                                             3.9,1.767,
                                             3.2097,3.37,3),
                                  a_FL.to.TL=rep(NA,7),
                                  b_FL.to.TL=rep(NA,7))  #Simpfendorfer & McAuley 2003 & Coulson pers com


#from Fishbase and Smallwood et al 2018
Additional.length.weight.economics=
  data.frame(
    common_name=c("Baldchin groper", "Breaksea cod","Boxfishes","Boarfishes","Banded sweep","Coral trout",
                  "Foxfish","Gurnards","Harlequin fish","John Dory", "Knifejaw", "Lings",
                  "Moonlighter","Mulloway", "Parrotfishes","Nannygai","Samson fish","Sea sweep",
                  "Tunas","Skipjack trevally","Leatherjackets",
                  "Common blacktip shark", "Blacktip sharks", "Bronze whaler","Hammerheads","Scalloped hammerhead","Smooth hammerhead",
                  "Spinner shark","Shortfin mako","Pencil shark","Common sawshark","Spotted wobbegong","Western wobbegong","Other sharks"),
    a_weight=c(3.504558e-05,1.242766e-05,6.46e-06,1.85e-05,7.63e-06,2.292184e-05,
               9.75e-06,9.75e-06,1.83e-05,2.29e-05,2.66e-05,1.1e-06,
               5.255e-05,6.3e-06,2.09e-05,1.274212e-05,4.07e-05,7.63e-06,
               2.14e-05,2.51e-05,2.63e-05,
               1.07e-05,1.07e-05,1.04E-05,1.62e-05,7.75E-06,1.62E-05,
               1.13e-06,5.24E-06,2.29E-05,2.00E-07,3.65E-03,2.19E-06,1e-5),
    b_weight=c(2.86,3.099,3.168,2.931,3.136,2.88,
               3,3,2.891,2.89,3,3.37,
               2.82, 3.05,2.96,3,2.67,3.136,
               2.93,2.86,2.85,
               2.9,2.9,2.9,2.721,3.0669,2.7208,
               3.3,3.1407,2.6125,3.4617,1.757,3.23,3),
    a_FL.to.TL=c(rep(NA,21),rep(mean(Length.weight.main$a_FL.to.TL[1:4]),13)), 
    b_FL.to.TL=c(rep(NA,21),rep(mean(Length.weight.main$b_FL.to.TL[1:4]),13))) 


Length.weight=rbind(Length.weight.main,Length.weight.discards,Additional.length.weight.economics)

Shark.palette=c("firebrick4","red",'yellow')
Teleost.palette=c('steelblue','deepskyblue','royalblue4')


cpue.scaler=1000  #display cpue as km gn hour / 1000 hook hour


Min.N.drop.out=10         #minimum sample size for drop out analyses
Min.N.gaff=10              #minimum sample size for gaffing
Min.obs.comp.wei.flot=5   #minimum sample size for comp around weight and floats

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
  dplyr::rename(sheet_no="Sheet no",
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
  dplyr::rename(IDL=TrunkL,
                Rel.cond="RELEASE CONDITION")%>%
          mutate(COMMENTS=ifelse(is.na(COMMENTS),'',COMMENTS),
                COMMENTS=paste(COMMENTS,NewComments))%>%
  dplyr::select(SHEET_NO,LINE_NO,RECORDER,Mid.Lat,Mid.Long,Lat.round,Long.round,zone,date,Day,Month,year,BOAT,BLOCK,SKIPPER,BOTDEPTH,
                Set.time,Haul.time,Set.time.end,Haul.time.end,Set.time.avg,Haul.time.avg,SOAK.TIME,
                BaitSpeciesId,Method,MESH_SIZE,MESH_DROP,NET_LENGTH,
                N.hooks,HookLocation,HookRemoved,HookType,HookSize,WireTrace,Rel.cond,
                SPECIES,COMMON_NAME,SCIENTIFIC_NAME,Taxa,RetainedFlag,SEX,Number,TL,FL,PL,IDL,
                BloodFlag,FinClipFlag,MuscleFlag,Lactate,BleedingFlag,BAG_NO,COMMENTS,COMMENTS.hdr)
names(DATA)=tolower(names(DATA))
DATA=DATA%>%
  mutate(hooktype=case_when(hooktype%in%c('Circular','Offset circular')~'circular',
         hooktype=='EZ-baiter kerbed'~'Ezb'),
         hooksize=as.numeric(substr(hooksize,1,2)),
         skipper=ifelse(skipper%in%c("Tim","TIM"),"Tim Goodall",skipper))


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
        mutate(n.hooks=ifelse(method=='LL' & is.na(n.hooks) & !is.na(hooks.baited),
                              hooks.baited,n.hooks),
               hooks.deployed=n.hooks)

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
  
  check=c("BG.T","QS.T","PS.T",'JE.T',
          "TK","WH","CP","BW","GM")
  a=DATA%>%filter(species%in%check & retainedflag=="No")%>%distinct(sheet_no,line_no,species,retainedflag,fl,tl,comments,comments.hdr)
  if(nrow(a)>0) write.csv(a,'Main.commercial.sp_not.retained.csv',row.names = F)
  
  
  Lat.range=c(-36,-29)
  Long.range=c(113,119)  
  
  seq.Lat=seq(Lat.range[1],Lat.range[2])
  seq.Long=seq(Long.range[1],Long.range[2])
  
  library(PBSmapping)  
  data(worldLLhigh)
  Sites=DATA%>%distinct(sheet_no,mid.lat,mid.long,method)%>%
    mutate(Col=ifelse(method=='GN',"#F8766D", "#00BFC4"))
  tiff(file="Map issues.tiff",width = 1600, height = 2400,units = "px", res = 300,compression = "lzw")
  par(mar = c(0, 0, 0, 0),oma=c(0,0,0,0),mgp=c(.1, 0.15, 0))
  Lat.range=c(-36.5,-29)
  Long.range=c(114,120)
  #plot shots' Sampling site locations 
  plotMap(worldLLhigh, xlim=Long.range,ylim=Lat.range,axes=F,
          col="dark grey",tck = 0.025, tckMinor = 0.0125, xlab="",ylab="")
  points(Sites$mid.long,Sites$mid.lat,col='black',pch=21,bg=Sites$Col,cex=1.25)
  
  # #add bathymetry
  # contour(xbat, ybat, reshaped[,2:ncol(reshaped)],ylim=plotlat[[i]],xlim=plotlong[[i]], zlim=c(-1,-300),
  #         nlevels = 3,labcex=1,lty = c(1,2,3),col=c(rep("grey60",3)),add=T)
  axis(side = 1, at =seq.Long, labels = seq.Long, tcl = .5,las=1,cex.axis=0.9)
  axis(side = 2, at = seq.Lat, labels = -seq.Lat,tcl = .5,las=2,cex.axis=0.9)

  mtext(expression(paste("Latitude (",degree,"S)",sep="")),side=2,line=1.25,las=3,cex=1.5)
  mtext(expression(paste("Longitude (",degree,"E)",sep="")),side=1,line=1.25,cex=1.5)
  legend('topright',c("Gillnet","Longline"),pch=21,
         pt.bg=c("#F8766D", "#00BFC4"),bty='n',cex=1.25)
  box()
  
  aa=subset(Sites,sheet_no%in%c("PA0003","PA0100","PA0128","PA0047"))
  text(aa$mid.long,aa$mid.lat,aa$sheet_no,pos=4,col="black",font=2,srt=-35)
  dev.off()
}


# ---------Functions: Multivariate stats  -------------------------------
multivariate.fn=function(d,Terms,Def.sp.term,Transf,Show.term,Group,hndl,
                         MDS.title,Simper.title,Permanova.title,
                         simper.cumsum=0.85,
                         LGSIZE=12)
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
    mutate(method=factor(method))%>%
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
  
  Community <<- d[-match(Terms,names(d))]
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
  
  if(Show.term=="method.zone")
  {
    MDS_xy=MDS_xy%>%
      mutate(zone=case_when(grepl("West",dummy)~'West',
                            grepl("Zone1",dummy)~'Zone1',
                            grepl("Zone2",dummy)~'Zone2'),
             method=case_when(grepl("GN",dummy)~"Gillnet",
                              grepl("LL",dummy)~"Longline"))
  }
  p=ggplot(MDS_xy, aes(MDS1, MDS2, color = dummy)) +
    geom_point(size=3,aes(color=zone,shape=method)) +
    annotate(geom="text", x=0.85*max(MDS_xy$MDS1), y=min(MDS_xy$MDS2), 
             label=paste("Stress=",round(MDS$stress,3)))+
    theme_PA(leg.siz=14,axs.t.siz=12,axs.T.siz=14)+
    theme(legend.position = "top",
          legend.title = element_blank())+
    guides(colour = guide_legend(nrow = 1))+
    xlab('')+ylab('')
  if(Show.term=="method.zone")
  {
    p=p+scale_color_manual(values=c("West"="#F8766D","Zone1"="#00BFC4","Zone2"="#7CAE00"))+
      scale_shape_manual(values=c("Gillnet"=8,"Longline"=19))
  }
  
  fn.fig(paste(hndl,MDS.title,sep=""),2400,2400)
  print(p)
  dev.off()
  
  
  #Permanova
    # 1. overall significance test
  adon.results<-adonis(formula(paste('Community',Show.term,sep='~')),data=d, method="bray",perm=5e3)
  write.csv(as.data.frame(adon.results$aov.tab),paste(hndl,Permanova.title,'.csv',sep=""))
  
    # 2. multilevel pairwise comparison with adjusted p-values
  #adonis.pairwise=pairwise.adonis(Community,d[,match(Show.term,names(d))])
  dummy=pairwise.adonis2(Community~method.zone,data=d)
  adonis.pairwise=vector('list',(length(dummy)-1))
  for(qq in 2:length(dummy))
  {
    adonis.pairwise[[qq]]=data.frame(Pairs=names(dummy)[[qq]],
                                   P=dummy[[qq]]$`Pr(>F)`[1])
  }
  adonis.pairwise=do.call(rbind,adonis.pairwise)
  write.csv(adonis.pairwise,paste(hndl,Permanova.title,'_pairwise.csv',sep=""),row.names = F)
  
  
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
    theme_PA(str.siz=14,leg.siz=LGSIZE,axs.t.siz=12,axs.T.siz=16)+
    theme(legend.position = "top",
          legend.title = element_blank(),
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
      dplyr::rename(response=Pred,
             SE=Pred.SE)%>%
      mutate(lower.CL=response-1.96*SE,
             upper.CL=response+1.96*SE)%>%
      dplyr::select(all_of(PRED),response,SE,lower.CL,upper.CL)
  }
  return(NewD)  
}
fn.barplot.cpue=function(d,YLAB,XLAB,cex,Rotate=NULL,Relative,NROW=2)
{
  if(Relative=="Yes")d=d%>%
      group_by(facet)%>%
      mutate(lower.CL=lower.CL/mean(y),
             upper.CL=upper.CL/mean(y),
             y=y/mean(y))
  p=d%>%
    ggplot(aes(x,y,fill=fill))+
    geom_bar(stat="identity")+
    geom_errorbar(aes(x, ymin=lower.CL, ymax=upper.CL),colour='black',width=0.25)+
    facet_wrap(~facet,scales='free',nrow=NROW)+
    theme_PA(strx.siz=cex-2,leg.siz=cex,axs.t.siz=cex-2,axs.T.siz=cex+1)+
    theme(legend.position='none')+
    ylab(YLAB)+xlab(XLAB)
  if(!is.null(Rotate)) p=p+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  print(p)
}
fn.continuous.cpue=function(d,YLAB,XLAB,cex,Relative)
{
  if(Relative=="Yes")d=d%>%
      group_by(facet)%>%
      mutate(lower.CL=lower.CL/mean(y),
             upper.CL=upper.CL/mean(y),
             y=y/mean(y))
  p=d%>%
    ggplot(aes(x,y,fill=fill))+
    geom_line()+
    geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL), alpha=0.4)+
    facet_wrap(~facet,scales='free',nrow=2)+
    theme_PA(strx.siz=cex-2,leg.siz=cex-2,axs.t.siz=cex-2,axs.T.siz=cex+1)+
    theme(legend.position='top',
          legend.title=element_blank())+
    ylab(YLAB)+xlab(XLAB)
  print(p)
}
#---------Functions: mapping ------------  
plt.map=function(D,Title,Scaler)
{
  p=ggplot(oz_states) + 
    geom_sf(fill="brown4") + 
    coord_sf(xlim = c(112, 129), ylim = c(-25, -36))+
    geom_point(data = D, mapping = aes(x = lon.port, y = lat.port),
               size=D$Prop*Scaler, colour = grDevices::adjustcolor("steelblue", alpha=0.6))+
    geom_sf_text(data = sites,aes(label = Cityname),color="White",fontface="bold",
                 nudge_x=Nudge$nudge_x,nudge_y=Nudge$nudge_y)+
    ylab("Latitude")+xlab("Longitude")+
    theme(plot.margin=unit(c(.05,.05,.05,.05),"cm"),
          plot.title = element_text(size = 16,vjust = 0,face = 'bold'),
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 14))+
    ggtitle(Title)
  print(p)
}
sites <- st_as_sf(Port.loc%>%filter(Cityname%in%c("Albany","Augusta","Bunbury","Perth","Esperance",
                                                  "Geraldton","Eucla")),
                  coords = c("lon.port", "lat.port"), 
                  crs = 4326, agr = "constant")
Nudge <- sites %>%
  mutate(nudge_x=ifelse(Cityname%in%c("Perth"),0.65,
                        ifelse(Cityname%in%c("Augusta"),0.9,
                               ifelse(Cityname%in%c("Bunbury"),1.1,
                                      ifelse(Cityname%in%c("Geraldton"),1.2,
                                             0)))) , 
         nudge_y=ifelse(Cityname%in%c("Albany","Esperance","Augusta","Eucla"),0.25,0))
oz_states <- ozmaps::ozmap_states

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
  source(handl_OneDrive("Analyses/SOURCE_SCRIPTS/Git_other/get_lat.long.R"))
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
  
  Data.daily=Data.daily%>%
    mutate(Landing.Port=capitalize(tolower(Landing.Port)),
           Landing.Port=case_when(Landing.Port=="Albany town jetty"~"Albany",
                                  TRUE~Landing.Port))
  Data.monthly=Data.monthly%>%
    mutate(Landing.Port=capitalize(tolower(Landing.Port)))
  
  

  #1. Vessel dynamics
    #1.1. Landing port dynamics at its peak and currently
  Data.monthly=Data.monthly%>%
    left_join(Port.loc,by=c("Landing.Port"="Cityname"))
  
  Data.daily=Data.daily%>%
    left_join(Port.loc,by=c("Landing.Port"="Cityname"))
  
  Max.ports.yr=Data.monthly%>%
    filter(YEAR.c<=2005)%>%
    filter(METHOD%in%c('GN',"LL"))%>%
    filter(!is.na(lat.port))%>%
    distinct(Same.return,.keep_all=T)%>%
    group_by(Landing.Port,FINYEAR)%>%
    tally()%>%
    ungroup()%>%
    mutate(n=1)%>%
    group_by(FINYEAR)%>%
    tally()%>%
    filter(n==max(n))
 
  p1=plt.map(D=Data.monthly%>%
               filter(FINYEAR==Max.ports.yr$FINYEAR)%>%
               filter(METHOD%in%c('GN',"LL"))%>%
               filter(!is.na(lat.port))%>%
               distinct(Same.return,.keep_all=T)%>%
               group_by(Landing.Port,lat.port,lon.port)%>%
               tally()%>%
               ungroup()%>%
               mutate(Prop=n/sum(n)),
             Title=paste("Monthly returns (",Max.ports.yr$n," ports in ",Max.ports.yr$FINYEAR,")",sep=''),
             Scaler=150)
  
  D=Data.daily%>%
    filter(FINYEAR==Current.yr)%>%
    filter(METHOD%in%c('GN',"LL"))%>%
    filter(!is.na(lat.port))%>%
    mutate(TSNo=word(Same.return.SNo, 3, 3))%>%
    distinct(TSNo,.keep_all=T)%>%
    group_by(Landing.Port,lat.port,lon.port)%>%
    tally()%>%
    ungroup()%>%
    mutate(Prop=n/sum(n))
  p2=plt.map(D=D,
             Title=paste("Daily logbooks (",nrow(D)," ports in ",Current.yr,")",sep=''),
             Scaler=150)
  Bubble.ref=data.frame(lon.port=c(128,128),
                        lat.port=c(-35.75,-33.75),
                        Prop=c(0.1,0.2))
  p2=p2+geom_point(data = Bubble.ref, mapping = aes(x = lon.port, y = lat.port),
             size=Bubble.ref$Prop*150, colour = grDevices::adjustcolor("steelblue", alpha=0.6))+
    geom_text(data=Bubble.ref,aes(x = lon.port, y = lat.port,label = paste(Prop*100,"%",sep='')),
              fontface="bold",color="white",size=5)
  
  ggarrange(p1, p2, ncol = 1, nrow = 2)
  ggsave(le.paste("Historic_catch_effort/Map_ports.tiff"),width = 7,height = 9,compression = "lzw")
  
  
    #1.2. Number of departing ports and vessels by year
  a=Data.monthly%>%
    filter(METHOD%in%c('GN',"LL"))%>%
    filter(!is.na(lat.port))%>%
    distinct(Same.return,.keep_all=T)%>%
    group_by(Landing.Port,FINYEAR,zone)%>%
    tally()%>%
    ungroup()%>%
    mutate(n=1)%>%
    group_by(FINYEAR,zone)%>%
    tally()%>%
    ungroup()%>%
    mutate(year=as.numeric(substr(FINYEAR,1,4)),
           dat="Monthly returns")%>%
    filter(year<=2005)
  b=Data.daily%>%
    filter(METHOD%in%c('GN',"LL"))%>%
    filter(!is.na(lat.port))%>%
    mutate(TSNo=word(Same.return.SNo, 3, 3))%>%
    distinct(TSNo,.keep_all=T)%>%
    group_by(Landing.Port,FINYEAR,zone)%>%
    tally()%>%
    ungroup()%>%
    mutate(n=1)%>%
    group_by(FINYEAR,zone)%>%
    tally()%>%
    ungroup()%>%
    mutate(year=as.numeric(substr(FINYEAR,1,4)),
           dat="Daily logbooks")
  my.formula <- y ~ x  
  
  p1=rbind(a,b)%>%
    mutate(dat=factor(dat,levels=c("Monthly returns","Daily logbooks")))%>%
    ggplot(aes(x=year,y=n,color=zone))+
    geom_point(size=2.5)+
    geom_smooth(method = "lm",se=F,formula = my.formula)+
    stat_poly_eq(formula = my.formula, 
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
                 label.y = "top",label.x="right",parse = TRUE, size = 5)+
    #facet_wrap(~dat,scales='free')+
    ylab("Number of ports")+xlab("")+
    theme_PA(strx.siz=14,leg.siz=18,axs.t.siz=16,axs.T.siz=20)+
    theme(legend.position = "top",
          legend.title = element_blank(),
          plot.margin=unit(c(.1,.5,.1,.1),"cm"))
    
  d=Data.monthly%>%
    distinct(Same.return,.keep_all=T)%>%
    filter(METHOD%in%c('GN',"LL"))%>%
    mutate(This=paste(VESSEL,FINYEAR))%>%
    distinct(This,.keep_all=T)%>%
    group_by(YEAR,zone)%>%
    tally()%>%
    mutate(group=as.factor(paste(zone)),
           dat="Monthly returns")%>%
    filter(YEAR<=2005)
  
  d1=Data.daily%>%
    distinct(Same.return,.keep_all=T)%>%
    filter(METHOD%in%c('GN',"LL"))%>%
    mutate(This=paste(VESSEL,FINYEAR))%>%
    distinct(This,.keep_all=T)%>%
    group_by(YEAR,zone)%>%
    tally()%>%
    mutate(group=as.factor(paste(zone)),
           dat="Daily logbooks")
  
  ddd=rbind(d,d1)%>%
    mutate(dat=factor(dat,levels=c("Monthly returns","Daily logbooks")))
  p2=ddd%>%
    ggplot(aes(x=YEAR,y=n,color=group))+
    geom_point(size=2.5)+
    geom_smooth(data=subset(ddd, YEAR >= 1980),method = "lm",se=F,formula = my.formula)+
    stat_poly_eq(formula = my.formula, 
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
                 label.y = "top",label.x="right", parse = TRUE, size = 5)+
    #facet_wrap(~dat,scales='free')+
    ylab("Number of vessels")+xlab("Financial year")+
    theme_PA(strx.siz=14,leg.siz=18,axs.t.siz=16,axs.T.siz=20)+
    theme(legend.position = "top",
          legend.title = element_blank(),
          plot.margin=unit(c(.1,.5,.1,.1),"cm"))
  
  ggarrange(p1, p2, ncol = 1, nrow = 2,common.legend=TRUE)   
  ggsave(le.paste("Historic_catch_effort/Annual number of ports and vessel by zone.tiff"),width = 9,height = 10,compression = "lzw")
  
    #1.3. Number of vessels with gillnet or longline by year
  d=Data.monthly%>%
    distinct(Same.return,.keep_all=T)%>%
    filter(METHOD%in%c('GN',"LL"))%>%
    mutate(YEAR=as.numeric(substr(FINYEAR,1,4)),
           This=paste(VESSEL,METHOD,FINYEAR))%>%
    distinct(This,.keep_all=T)%>%
    group_by(YEAR,METHOD,zone)%>%
    tally()%>%
    mutate(group=as.factor(paste(METHOD,zone)),
           Data.set="Monthly")%>%
    filter(YEAR<=2005)
  
  d1=Data.daily%>%
    distinct(Same.return,.keep_all=T)%>%
    filter(METHOD%in%c('GN',"LL"))%>%
    mutate(YEAR=as.numeric(substr(FINYEAR,1,4)),
           This=paste(VESSEL,METHOD,FINYEAR))%>%
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
  
  
  #add catch per vessel
  d2=Data.monthly%>%
    filter(METHOD%in%c('GN',"LL"))%>%
    mutate(YEAR=as.numeric(substr(FINYEAR,1,4)))%>%
    group_by(YEAR,METHOD,zone)%>%
    summarise(Tons=sum(LIVEWT.c)/1000)%>%
    ungroup()%>%
    mutate(group=as.factor(paste(METHOD,zone)),
           Data.set="Monthly")%>%
    filter(YEAR<=2005)
  
  d3=Data.daily%>%
    filter(METHOD%in%c('GN',"LL"))%>%
    mutate(YEAR=as.numeric(substr(FINYEAR,1,4)))%>%
    group_by(YEAR,METHOD,zone)%>%
    summarise(Tons=sum(LIVEWT.c)/1000)%>%
    ungroup()%>%
    mutate(group=as.factor(paste(METHOD,zone)),
           Data.set="Daily")
  
  d2=d2%>%
    left_join(d,by=c('YEAR','METHOD','zone','group','Data.set'))%>%
    mutate(ktch.per.ves=Tons/n)
  
  d3=d3%>%
    left_join(d1,by=c('YEAR','METHOD','zone','group','Data.set'))%>%
    mutate(ktch.per.ves=Tons/n)
  
  
  dd=rbind(d,d1)%>%
    mutate(Data.set=factor(Data.set,levels=c("Monthly","Daily")))
  dd1=rbind(d2,d3)%>%
    mutate(Data.set=factor(Data.set,levels=c("Monthly","Daily")))
  
  
  coeff=0.5
  fn.fig(le.paste("Historic_catch_effort/Annual number of vessels by method"),2400,1800)
  dd%>%
    ggplot(aes(YEAR,n))+
    geom_col(aes(fill=group),alpha=0.65)+
    geom_line(data=dd1, aes(YEAR,ktch.per.ves/coeff,color=group),size=1.25)+
    scale_y_continuous(name = "Number of Vessels",
      sec.axis = sec_axis(~.*coeff, name="Annual catch (tonnes) per vessel"))+
    xlab("Financial year")+
    scale_color_manual(values=dis.cols)+
    scale_fill_manual(values=dis.cols)+
    theme_PA(Ttl.siz=18,Sbt.siz=16,str.siz=12,strx.siz=12,
             cap.siz=10,leg.siz=12,axs.t.siz=10,axs.T.siz=14)+
    theme(legend.position = "top",
          legend.title = element_blank(),
          plot.margin=unit(c(.1,.5,.1,.1),"cm"))+
     guides(fill = guide_legend(nrow = 1),color = guide_legend(nrow = 1))
  dev.off()

  
  #2. Number of fishing days with gillnet or longline by year
    #Total
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
  
  P1=rbind(d,d1)%>%
    mutate(Data.set=factor(Data.set,levels=c("Monthly","Daily")))%>%
    ggplot(aes(YEAR,Bdays))+
    geom_col(aes(fill=group))+
    #facet_wrap(~Data.set,scales='free')+
    ylab("Number of fishing days")+xlab("")+
    scale_fill_manual(values=dis.cols)+
    theme_PA(Ttl.siz=18,Sbt.siz=16,str.siz=12,strx.siz=12,
             cap.siz=10,leg.siz=12,axs.t.siz=10,axs.T.siz=14)+
    theme(legend.position = "top",
          legend.title = element_blank(),
          plot.margin=unit(c(.1,.5,.1,.1),"cm"))+
    guides(fill = guide_legend(nrow = 1))
  
  
    #Average 
  d=Data.monthly%>%
    distinct(Same.return,.keep_all=T)%>%
    filter(METHOD%in%c('GN',"LL"))%>%
    group_by(YEAR,METHOD,zone,VESSEL)%>%
    summarise(BDAYS=sum(BDAYS,na.rm=T))%>%
    group_by(YEAR,METHOD,zone)%>%
    summarise(Bdays.mean=mean(BDAYS,na.rm=T),
              Bdays.sd=sd(BDAYS,na.rm=T))%>%
    mutate(group=as.factor(paste(METHOD,zone)),
           Data.set="Monthly")%>%
    filter(YEAR<=2005)
  
  d1=Data.daily%>%
    distinct(Same.return,.keep_all=T)%>%
    filter(METHOD%in%c('GN',"LL"))%>%
    group_by(YEAR,METHOD,zone,VESSEL)%>%
    summarise(BDAYS=sum(BDAYS,na.rm=T))%>%
    group_by(YEAR,METHOD,zone)%>%
    summarise(Bdays.mean=mean(BDAYS,na.rm=T),
              Bdays.sd=sd(BDAYS,na.rm=T))%>%
    mutate(group=as.factor(paste(METHOD,zone)),
           Data.set="Daily")
  
  P2=rbind(d,d1)%>%
    mutate(Data.set=factor(Data.set,levels=c("Monthly","Daily")))%>%
    ggplot(aes(YEAR,Bdays.mean,color=group))+
    geom_errorbar(aes(x=YEAR, ymin=Bdays.mean-Bdays.sd, ymax=Bdays.mean+Bdays.sd,color=group),alpha=0.6,width=0.25)+
    geom_line(size=1.5,linetype='solid',alpha=0.6)+
    geom_point(size=2)+
    ylab("Average annual number of fishing days")+
    xlab("Financial year")+
    scale_color_manual(values=dis.cols)+
    theme_PA(Ttl.siz=18,Sbt.siz=16,str.siz=12,strx.siz=12,
             cap.siz=10,leg.siz=12,axs.t.siz=10,axs.T.siz=14)+
    theme(legend.position = "top",
          legend.title = element_blank(),
          plot.margin=unit(c(.1,.5,.1,.1),"cm"))+
    guides(fill = guide_legend(nrow = 1))
  

  fn.fig(le.paste("Historic_catch_effort/Annual fishing days by method"),2400,2200)
  ggarrange(P1,P2,ncol=1,common.legend=TRUE)
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
  do.this=FALSE
  if(do.this)
  {
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
  }
  
      #Daily comparable records
  if(do.this)
  {
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
  }
  
  
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
    theme_PA(axs.t.siz=16,axs.T.siz=18)+
    theme(legend.position="none",
          plot.margin=margin(.1,.5,.1,.1, "cm"))
  dev.off()
  
  d1.GN=Data.daily.original%>%
    filter(METHOD=="GN" & NETLEN>100)%>%
    dplyr::select(c(DSNo,TSNo,SNo,HOURS,SHOTS,NETLEN))%>%
    mutate(dat="Daily",Same.return.SNo=paste(DSNo,TSNo,SNo))%>%
    distinct(Same.return.SNo,.keep_all=T)%>%
    mutate(hours.c=HOURS)%>%
    filter(!is.na(SHOTS))
  colnames(d1.GN)=tolower(colnames(d1.GN))
    
  
  ddx=rbind(d1%>%dplyr::select(dat,hours.c,shots)%>%mutate(method='LL'),
        d1.GN%>%dplyr::select(dat,hours.c,shots)%>%mutate(method='GN'))%>%
        mutate(shots=ifelse(shots>2,">2",shots),
               shots=factor(shots,levels=c("1","2",">2")))
  fn.fig(paste(hndl,"/longline only/longline_gillnet.soak times_daily only",sep=""),2400,1600)
  ddx%>%
    ggplot(aes(x=hours.c,fill=method)) +
    facet_wrap(~method, scales="free")+
    geom_histogram(position="dodge",binwidth=1)+
    scale_fill_manual(values=c("#F8766D", "#00BFC4"))+
    xlab('Soaking hours per session')+ylab('Frequency')+ guides(fill=guide_legend(title="Data set"))+
    theme_PA(axs.t.siz=14,str.siz=14,axs.T.siz=16)+
    theme(legend.position="none")+
     xlim(0,quantile(ddx$hours.c,probs=0.999))
  dev.off()
  
  fn.fig(paste(hndl,"/longline only/longline.hook size_daily only",sep=""),2400,1600)
  subset(d1,hooksize>0)%>%
    ggplot(aes(x=hooksize)) + 
    geom_histogram(position="dodge",binwidth=1,fill="darksalmon")+
    xlab('Hook size')+ylab('Frequency')+
    scale_x_continuous(breaks=6:15,
                       labels=6:15)+
    theme_PA(axs.t.siz=16,axs.T.siz=18)+
    theme(legend.position="none",
          legend.title=element_blank())
  dev.off()
  
  fn.fig(paste(hndl,"/longline only/longline.hook type_daily only",sep=""),2800,1600)
  ggplot(subset(d1,!is.na(hooktype)|hooktype==''), aes(hooktype)) + geom_bar(fill="darksalmon")+
    theme_PA(axs.t.siz=16,axs.T.siz=18)+ylab('Frequency')+xlab('Hook type')
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
      mutate(cpue1=ifelse(method=="GN",cpue*cpue.scaler,
                   ifelse(method=="LL",cpue*cpue.scaler,
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
    upper=quantile(d3$cpue1,probs=.995)  
    
    #By methodzone
    d3=d3%>%
      mutate(zone=ifelse(zone=='1','zone 1',ifelse(zone=='2','zone 2',zone)),
             method.zone=paste(method,zone))
    d3=d3%>%
      mutate(SNAME=ifelse(SNAME=="Blue morwong","Queen snapper",
                   ifelse(SNAME=="Western blue groper","Blue groper",
                   SNAME)),
             SNAME=factor(SNAME,levels=c("Dusky shark","Whiskery shark",
                                         "Gummy shark","Sandbar shark",
                                         "Queen snapper","Blue groper",
                                         "West Australian dhufish","Pink snapper")))
    
    p=d3%>%
      ggplot(aes(x=method.zone, y=cpue1)) + 
      geom_boxplot(outlier.size = 0.1,aes(fill=colr))+ 
      coord_flip()+
      #coord_flip(ylim = c(0, upper))+
      facet_wrap(~SNAME,scales='free',nrow=4)+
      scale_fill_manual(values=c(firebrick="firebrick",steelblue="steelblue"))+
      ylab("Catch rate per session (kg per 1000 m hour / 1000 hook hour)")+xlab("")+ 
      theme_PA(strx.siz=12,axs.t.siz=10.75)+
      theme(legend.position="none",
            plot.margin = margin(.1, .5, .1, .1, "cm"))+
      scale_y_sqrt()
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
  
  fn.fig(paste(hndl,"/catch_rates/Catch rates by method and zone for main species_daily_y_sqrt",sep=""),2600,2000)
  daily.cpue=fn.catchrate(d1,d2)
  dev.off()
  
    #b. standardization
  names(daily.cpue)=tolower(names(daily.cpue))
  daily.cpue=daily.cpue%>%
    filter(!is.na(depthmax) & depthmax<200)%>%
    filter(!zone=='zone 1')   #almost no longline records in zone 1

      #b.1 fit GAM
  main.sp=as.numeric(names(Main.species))
  cpue.stand.out.GN=vector('list',length(main.sp))
  names(cpue.stand.out.GN)=main.sp
  cpue.stand.out.LL=cpue.stand.out.GN
  GN.form=formula(cpue1~zone+s(vessel,bs='re'))
  LL.form=formula(cpue1~zone+hooksize+hooktype+s(vessel,bs='re'))
  #GN.form=formula(cpue1~zone+s(depthmax)+s(vessel,bs='re'))
  #LL.form=formula(cpue1~zone+s(depthmax)+hooksize+hooktype+s(vessel,bs='re'))
  
  system.time({
    for(m in 1:length(main.sp))
    {
      print(paste(main.sp[m],'GN GAM-------------'))
      D=daily.cpue%>%
        filter(method=='GN' & species==main.sp[m])%>%
        mutate(zone=as.factor(zone),
               method=as.factor(method),
               vessel=as.factor(vessel))
      
      cpue.stand.out.GN[[m]]=cpue.stand.fun(d=D,Formula=GN.form)  
      
      
      print(paste(main.sp[m],'LL GAM-------------'))
      D=daily.cpue%>%
        filter(method=='LL' & species==main.sp[m] & hooksize>11)%>%
        filter(!is.na(hooktype))%>%
        mutate(zone=as.factor(zone),
               method=as.factor(method),
               vessel=as.factor(vessel),
               hooksize=factor(hooksize),
               hooktype=as.factor(hooktype))
      
      form=LL.form
      if(main.sp[m]==17001) form=formula(cpue1~zone+hooktype+s(vessel,bs='re'))
      cpue.stand.out.LL[[m]]=cpue.stand.fun(d=D,Formula=form)  
      
      
      #Pink snapper without targeting vessels in West coast
      #note: It cannot be run because when removing the three 
      #       targeting vessels (out.daily$Snapper.targeting.vessel),
      #       there are no records left in West coast...
      do.snpr.targ=FALSE
      if(do.snpr.targ)
      {
        if(main.sp[m]==353001)
        {
          D=daily.cpue%>%
            filter(method=='GN' & species==main.sp[m])%>%
            filter(!vessel%in%out.daily$Snapper.targeting.vessel)%>%
            mutate(zone=as.factor(zone),
                   method=as.factor(method),
                   vessel=as.factor(vessel))
          Pink_snapper_not.targeting.GN=cpue.stand.fun(d=D,Formula=GN.form)  
          
          D=daily.cpue%>%
            filter(method=='LL' & species==main.sp[m] & hooksize>11)%>%
            filter(!vessel%in%out.daily$Snapper.targeting.vessel)%>%
            filter(!is.na(hooktype))%>%
            mutate(zone=as.factor(zone),
                   method=as.factor(method),
                   vessel=as.factor(vessel),
                   hooksize=factor(hooksize),
                   hooktype=as.factor(hooktype))
          form=LL.form
          Pink_snapper_not.targeting.LL=cpue.stand.fun(d=D,Formula=form)  
          
        }
      }
    }
  })   #takes 2 minutes
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
  
  Get.hook.type.Pred=c(18003,353001,17003) #only significant for these species
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
    # GN=pred.fun.continuous(d=cpue.stand.out.GN[[m]]$dat,
    #                        mod=cpue.stand.out.GN[[m]]$mod,
    #                        PRED='depthmax',
    #                        Formula=formula(cpue1~zone+s(depthmax)+s(vessel,bs='re')))%>%
    #           mutate(method="GN",
    #                  species=SP)
    # LL=pred.fun.continuous(d=cpue.stand.out.LL[[m]]$dat,
    #                        mod=cpue.stand.out.LL[[m]]$mod,
    #                        PRED='depthmax',
    #                        Formula=formula(cpue1~zone+s(depthmax)+hooksize+hooktype+s(vessel,bs='re')))%>%
    #           mutate(method="LL",
    #                  species=SP)
    # Store.depth[[m]]=rbind(GN,LL)
    
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

  #Method-zone 
  fn.fig(paste(hndl,"/catch_rates/Standardized_Catch rates_Method.zone_daily",sep=""),2600,1600)
  fn.barplot.cpue(d=do.call(rbind,Store.zone.method)%>%
                              mutate(method.zone=paste(method,zone,sep='-'),
                                     species=factor(species,levels=Main.species),
                                     x=method.zone,
                                     y=response,
                                     fill=method.zone,
                                     facet=species),
                            YLAB="Standardized catch rate (kg per 1000 m hour / 1000 hook hour)",
                            XLAB='Fishing method and zone',
                            cex=10.5,
                            Rotate='Yes',
                           Relative="No")
  dev.off()
  
  #Depth 
  # fn.fig(paste(hndl,"/catch_rates/Standardized_Catch rates_depth_daily",sep=""),2400,1400)
  # fn.continuous.cpue(d=do.call(rbind,Store.depth)%>%
  #                                  mutate(species=factor(species,levels=Main.species),
  #                                         x=depthmax,
  #                                         y=response,
  #                                         fill=method,
  #                                         facet=species),
  #                                YLAB="Relative standardized catch rate",
  #                                XLAB='Depth (m)',
  #                                cex=10,
  #                                Relative="Yes")
  # dev.off()
  
  
  #Hooksize
  fn.fig(paste(hndl,"/catch_rates/Standardized_Catch rates_hooksize_daily",sep=""),2000,2400)
  fn.barplot.cpue(d=do.call(rbind,Store.hook.size)%>%
                    mutate(species=factor(species,levels=Main.species),
                           x=hooksize,
                           y=response,
                           fill=hooksize,
                           facet=species),
                  YLAB="Relative standardized catch rate",
                  XLAB='Hook size',
                  cex=16,
                  Relative="Yes")
  dev.off()
  
  
  #Hooktype
  fn.fig(paste(hndl,"/catch_rates/Standardized_Catch rates_hooktype_daily",sep=""),2000,2400)
  fn.barplot.cpue(d=do.call(rbind,Store.hook.type)%>%
                    mutate(species=factor(species,levels=Main.species),
                           x=hooktype,
                           y=response,
                           fill=hooktype,
                           facet=species),
                  YLAB="Relative standardized catch rate",
                  XLAB='Hook type',
                  cex=16,
                  Relative="Yes")
  dev.off()
  
  
  
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
      dplyr::rename(species=SPECIES,
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
    
    fn.fig(paste(hndl,"/Catch rates by method and zone for main species_monthly_y_sqrt",sep=""),2800,1800)
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
  dplyr::rename(sheet_no="Sheet #",
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
  dplyr::rename(retained=retainedflag)%>%
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
    Video.longline.obs=Video.longline.obs%>%dplyr::rename(Code=code)
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
                     Interaction=ifelse(Interaction=="Bounce off","Bounced off",Interaction),
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


#---------Add total weight for main species to PA observer data------------
these.sharks=c('Dusky shark','Whiskery shark','Gummy shark','Sandbar shark',
               "Common blacktip shark", "Blacktip sharks", "Bronze whaler", "Hammerheads",          
               "Scalloped hammerhead", "Smooth hammerhead", "Spinner shark", "Shortfin mako",        
               "Pencil shark", "Common sawshark", "Spotted wobbegong", "Western wobbegong" ,   
               "Other sharks")
DATA=DATA%>%
  left_join(Length.weight,by='common_name')%>%
  mutate(tl=case_when(is.na(tl) & 
                        common_name%in%these.sharks~a_FL.to.TL*fl+b_FL.to.TL,
                      TRUE~tl),
         tl=ifelse(CAES_Code%in%35000:39100 & is.na(tl),dw,tl),
         tw=ifelse(!is.na(a_weight) & !is.na(tl),a_weight*tl^b_weight,NA),
         tw=ifelse(tw>2500,NA,tw))


#---------Observer catch composition ------------ 
# Map of sampling sites
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
  
  fn.fig(le.paste("Map site area"),1600,2400)
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
  
  mtext(expression(paste("Latitude (",degree,"S)",sep="")),side=2,line=1.25,las=3,cex=1.5)
  mtext(expression(paste("Longitude (",degree,"E)",sep="")),side=1,line=1.5,cex=1.5)
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

# 1. Table of number of individuals caught by species and gear
Tab.n.sp.gear=DATA%>%
                group_by(method,common_name,scientific_name,taxa)%>%
                tally()%>%
                ungroup()%>%
                spread(method,n,fill=0)%>%
                mutate(Total=GN+LL)%>%
                arrange(taxa,-Total)%>%
                dplyr::select(-taxa)%>%
                filter(!common_name=='')
write.csv(Tab.n.sp.gear,le.paste("Observer/Observed.number.species.by.gear.csv"),row.names=F)

Tab.n.sp.longline=DATA%>%
  filter(method=='LL')%>%
  group_by(LL.config,common_name,taxa)%>%
  tally()%>%
  ungroup()%>%
  spread(LL.config,n,fill=0)%>%
  mutate(Total = rowSums(across(where(is.numeric))))%>%
  arrange(taxa,-Total)%>%
  dplyr::select(-taxa,-Total)%>%
  filter(!common_name=='')
write.csv(Tab.n.sp.longline,le.paste("Observer/Observed.number.species.by.longline.config.csv"),row.names=F)


# 2. Number of individuals caught by snood configuration 
fn.fig(le.paste("Observer/Barplot Longline configurations"),2400,2000)
DATA%>%
  filter(method=="LL")%>%
  filter(!is.na(hooktype) & !is.na(hooksize) & !is.na(wiretrace))%>%
  group_by(LL.config,hooktype, hooksize, wiretrace)%>%
  tally()%>%
  ggplot(aes(x= LL.config, y=n,fill=wiretrace)) +
  geom_bar(stat="identity", position = "dodge")+
  theme_PA(lgT.siz=14,leg.siz=12,axs.t.siz=12,axs.T.siz=16)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
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
fn.fig(le.paste("Observer/catch_comp/Catch.comp_GN_LL_overall"),2600,1600)
par(mfcol=c(1,2),mar=c(2,10.5,1,.1),oma=c(1,.1,.5,.65),mgp=c(1.5,.5,0),cex.axis=.9)
fn.brplt(d=DATA%>%
           filter(method=="GN"),
         Grouped.species=Grouped.species.GN,
         CX.nm=.8)
mtext(paste("Gillnet (",length(unique(subset(DATA,method=='GN')$sheet_no))," shots, ",
            sum(subset(DATA,method=='GN')$number)," individuals)",sep=""),3,cex=.9)
fn.brplt(d=DATA%>%
           filter(method=="LL"),
         Grouped.species=Grouped.species.LL,
         CX.nm=.8)
mtext(paste("Longline (",length(unique(subset(DATA,method=='LL')$sheet_no))," shots, ",
            sum(subset(DATA,method=='LL')$number)," individuals)",sep=""),3,cex=.9)
mtext("Proportion of total catch",1,outer=T,line=0.1,cex=1.2) 
dev.off()

  #by zone
dis.zoun=c("West","Zone1","Zone2")
fn.fig(le.paste("Observer/catch_comp/Catch.comp_GN_LL_by.zone"),2600,2000)
smart.par(length(dis.zoun)*2,MAR=c(2,10.5,.15,.1),OMA=c(1,.1,1,.3),MGP=c(1.5,.5,0))
par(cex.axis=.9)
for(z in 1:length(dis.zoun))
{
  fn.brplt(d=DATA%>%
             filter(method=="GN" & zone==dis.zoun[z]),
           Grouped.species=Grouped.species.GN,
           CX.nm=.8)
  if(z==1)mtext("Gillnet",3,cex=.9)
  LGN=paste(dis.zoun[z]," (",length(unique(subset(DATA,method=='GN'& zone==dis.zoun[z])$sheet_no))," shots, ",
            sum(subset(DATA,method=='GN'& zone==dis.zoun[z])$number)," individuals)",sep="")
  legend('bottomright',LGN,bty='n')
  
  fn.brplt(d=DATA%>%
             filter(method=="LL" & zone==dis.zoun[z]),
           Grouped.species=Grouped.species.LL,
           CX.nm=.8)
  if(z==1)mtext("Longline",3,cex=.9)
  LGN=paste(dis.zoun[z]," (",length(unique(subset(DATA,method=='LL'& zone==dis.zoun[z])$sheet_no))," shots, ",
            sum(subset(DATA,method=='LL'& zone==dis.zoun[z])$number)," individuals)",sep="")
  legend('bottomright',LGN,bty='n')
}
mtext("Proportion of total catch",1,outer=T,line=0.1,cex=1.2) 
dev.off()

    #by LL configuration 
LL.configs=DATA%>%
  filter(method=="LL")%>%
  filter(!(is.na(hooktype)| is.na(hooksize) | is.na(wiretrace)))%>%
  group_by(LL.config)%>%
  tally()%>%pull(LL.config)
fn.fig(le.paste("Observer/catch_comp/Catch.comp_by_LL.configuration"),2400,1800)
smart.par(length(LL.configs),MAR=c(2,10.5,1,.1),OMA=c(1,.1,.5,.8),MGP=c(1.5,.5,0))
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
multivariate.fn(d=DATA%>%
                    filter(!is.na(zone))%>%
                    filter( !is.na(method))%>%
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
                 MDS.title="/catch_comp/Catch.comp_GN_LL_MDS",
                 Simper.title="/catch_comp/Catch.comp_GN_LL_Simper",
                 Permanova.title="/catch_comp/Catch.comp_GN_LL_Permanova",
                simper.cumsum=0.8,
                LGSIZE=10.5)  
rm(Community)

#---------Observer size distribution ------------ 

    #GN vs LL for main species
dumi=DATA%>%
  filter(common_name%in%Main.species)%>%
  mutate(size=Size)
Sumri=dumi %>% group_by(common_name,Taxa,method) %>% 
  summarise(n=sum(number),size=median(size,na.rm=T))%>%
  filter(n>2)

fn.fig(le.paste("Observer/size/Size.density.dist.main.sp_GN_LL"),2400,1600)
dumi%>%
ggplot(aes(x = size, y = common_name,fill=Taxa)) + 
  geom_density_ridges(alpha = .8, color = "white")+
  facet_wrap(~method)+
  scale_fill_manual(values=c("steelblue","firebrick"))+
  ylab('')+
  xlab('Size (cm, FL for elasmobranchs, TL for teleosts)')+
  theme_PA(leg.siz=13,axs.t.siz=12,axs.T.siz=14)+
  theme(legend.position = "top",
        legend.title = element_blank())+
   geom_text(data=Sumri,aes(label=paste('n= ',n,', median= ',round(size)," cm",sep='')),
             position=position_nudge(y=-0.1), colour="black", size=3)
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

fn.fig(le.paste("Observer/size/Size.density.dist.main.sp_by_LL.configuration"),2400,1600)
dumi%>%
  ggplot(aes(x = size, y = common_name,fill=Taxa)) + 
  geom_density_ridges(alpha = .8, color = "white")+
  facet_wrap(~LL.config)+
  scale_fill_manual(values=c("steelblue","firebrick"))+
  ylab('')+
  xlab('Size (cm, FL for elasmobranchs, TL for teleosts)')+
  theme_PA(leg.siz=12,str.siz=10,axs.t.siz=11,axs.T.siz=13)+
  theme(legend.position = "top",
        legend.title = element_blank())+
    geom_text(data=Sumri,aes(label=paste('n= ',n,', median= ',round(size)," cm",sep='')),
            position=position_nudge(y=-0.1), colour="black", size=2.5)
dev.off()


  #Compare overlap between GN and LL for most commonly caught species
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
  Overlap.res[[o]]=data.frame(common_name=Tabl.sizes[o],
                              overlap=out$OV,
                              N.GN=length(x$GN),
                              N.LL=length(x$LL))
}
Overlap.res=do.call(rbind,Overlap.res)
fn.fig(le.paste("Observer/size/Size.density.dist.overlap_GN_LL"),2700,1600)
DATA%>%
  filter(common_name%in%Tabl.sizes & !is.na(Size))%>%
  left_join(Overlap.res,by='common_name')%>%
  mutate(method=ifelse(method=="GN","Gillnet",
                ifelse(method=="LL","Longline",
                       NA)),
         common_name=ifelse(common_name=="West Australian dhufish","WA dhufish",
                            common_name),
         LBL=paste(common_name," (N=",N.GN," GN, ",N.LL," LL; ","overlap= ",100*round(overlap,2),"%)",sep=''))%>%
  ggplot(aes(x = Size, fill=method)) +
  facet_wrap( ~LBL,scales='free') + 
  geom_density(alpha=.5) +
  ylab('Density')+
  xlab('Size (cm, FL for elasmobranchs, TL for teleosts)')+
  theme_PA(leg.siz=10,strx.siz=8,axs.t.siz=9,axs.T.siz=12)+
  theme(legend.position = "top",
        legend.title = element_blank())
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
write.csv(Tab.Kolmo,le.paste("Observer/size/Size.Kolmo_Smirnov.csv"),row.names = F)

  #Stats
# this.sp.sizes=DATA%>%
#   filter(!is.na(Size))%>%
#   group_by(common_name)%>%
#   tally()%>%
#   filter(n>50)%>%
#   pull(common_name)
this.sp.sizes=Tabl.sizes
fn.glm.size=function(d,Formula,Pred,Anova.title)
{
  d=d%>%
    mutate(method=as.factor(method),
           zone=as.factor(zone))
  
  #rum model
  mod=gam(Formula,dat=d,family=gaussian(link = "identity"))
  
  
  #export anova table
  ft <- as_flextable(mod)
  save_as_image(ft, path = le.paste(paste("Observer/size/ANOVA_",Anova.title,".png",sep='')))
  rm(ft)
  
  if(any(Pred=="method"))
  {
    p=pred.fun(mod=mod,biascor="NO",PRED=Pred)%>%
      mutate(species=NM)
    return(p)
  }else
  {
    a=summary(mod)$p.pv
    a=a[grep(paste(Pred, collapse="|"),names(a))]
    a=subset(a,a<0.05)
    if(length(a)>0)
    {
      #predict term
      aa=Pred[str_detect(paste(names(a),collapse='|'), Pred)]
      p=vector('list',length(aa))
      names(p)=aa
      for(ss in 1:length(p))
      {
        p[[ss]]=pred.fun(mod=mod,biascor="NO",PRED=Pred[str_detect(names(a)[ss], Pred)])%>%
          mutate(species=NM)
      }
      return(p)
    }
    rm(a)
  }

}

Store.zone.method=vector('list',length(this.sp.sizes))
names(Store.zone.method)=this.sp.sizes

    #Gear effect
for(m in 1:length(this.sp.sizes))
{
  NM=this.sp.sizes[m]
  print(paste(NM,'size GLM-------------'))
  
  D=DATA%>%
    filter(!is.na(Size) & common_name ==NM)
  Tab=D%>%
    group_by(method)%>%
    tally()%>%
    mutate(n=ifelse(n>=15,1,0))
  if(sum(Tab$n)>1)
  {
    Store.zone.method[[m]]=fn.glm.size(d=D,
                                       Formula=formula(Size~method+zone),
                                       Pred='method',
                                       Anova.title=paste("Method",NM,sep='_'))
  }
  
}
fn.fig(le.paste("Observer/size/Standardized_Size_Method"),2400,2400)
fn.barplot.cpue(d=do.call(rbind,Store.zone.method)%>%
                           mutate(species=factor(species,levels=this.sp.sizes),
                                  x=method,
                                  y=emmean,
                                  fill=method,
                                  facet=species),
                         YLAB="Relative standardized size",
                         XLAB='Method',
                         cex=14,
                          Relative="Yes",
                NROW=3)
dev.off()

  #Longline configuration effect
Store=vector('list',length(this.sp.sizes))
names(Store)=this.sp.sizes
for(m in 1:length(this.sp.sizes))
{
  NM=this.sp.sizes[m]
  print(paste(NM,'size GLM-------------'))
  
  D=DATA%>%
    filter(!is.na(Size) & common_name ==NM & method=='LL')%>%
    filter(!is.na(hooktype) & !is.na(wiretrace) & !is.na(hooksize))%>%
    mutate(hooktype=as.factor(hooktype),
           wiretrace=as.factor(wiretrace),
           hooksize=factor(hooksize))
  
  Tab=D%>%
    group_by(hooksize)%>%
    tally()%>%
    mutate(n=ifelse(n>=15,1,0))
  if(sum(Tab$n)>1)
  {
    a=fn.glm.size(d=D,  
                  Formula=formula(Size~hooktype+hooksize+wiretrace+zone),
                  Pred=c('hooktype','hooksize','wiretrace'),
                  Anova.title=paste("longine.config",NM,sep='_'))
    if(!is.null(a)) Store[[m]]=a
  }
}

fun.list.out=function(LIS,what)
{
  dummy=vector('list',length(LIS))
  for(s in 1:length(LIS))
  {
    X=match(what,names(LIS[[s]]))
    if(!is.na(X)) dummy[[s]]=LIS[[s]][[X]]
  }
  dummy=dummy[!sapply(dummy,is.null)]
  return(dummy <- do.call(rbind,dummy))
}

    #Hook size
fn.fig(le.paste("Observer/size/Standardized_Size_Hook.size"),2000,1400)
fn.barplot.cpue(d=fun.list.out(LIS=Store,what='hooksize')%>%
                           mutate(species=factor(species,levels=this.sp.sizes),
                                  x=hooksize,
                                  y=emmean,
                                  fill=hooksize,
                                  facet=species),
                         YLAB="Relative standardized size",
                         XLAB='Hook size',
                         cex=14,
                         Relative="Yes")
dev.off()

      #Hook type
fn.fig(le.paste("Observer/size/Standardized_Size_Hook.type"),1400,2400)
fn.barplot.cpue(d=fun.list.out(LIS=Store,what='hooktype')%>%
                           mutate(species=factor(species,levels=this.sp.sizes),
                                  x=hooktype,
                                  y=emmean,
                                  fill=hooktype,
                                  facet=species),
                         YLAB="Relative standardized size",
                         XLAB='Hook type',
                         cex=14,
                         Relative="Yes")
dev.off()


    #Wire trace
fn.fig(le.paste("Observer/size/Standardized_Size_Snood.type"),2000,2000)
fn.barplot.cpue(d=fun.list.out(LIS=Store,what='wiretrace')%>%
                  mutate(species=factor(species,levels=this.sp.sizes),
                         x=wiretrace,
                         y=emmean,
                         fill=wiretrace,
                         facet=species),
                YLAB="Relative standardized size",
                XLAB='Hook type',
                cex=14,
                Relative="Yes")
dev.off()

#---------Observer cpue ------------ 
MeanSE <- function(x)
{
  v <- c(mean(x) - sd(x)/sqrt(length(x)), mean(x), mean(x) + sd(x)/sqrt(length(x)))
  names(v) <- c("ymin", "middle", "ymax")
  v
}
fn.method.zone=function(what,GN.scale,LL.scale)
{
  p=DATA%>%
    filter(!is.na(zone))%>%
    mutate(zone=tolower(zone),
           methodzone=paste(method,zone),
           Effort=ifelse(method=="GN",1000*net_length*soak.time,  #net length in m
                         ifelse(method=="LL",n.hooks*soak.time,
                                NA)),
           common_name=case_when(!common_name%in%what~'Other',
                                 TRUE~common_name))%>%
    filter(common_name%in%c(what,'Other'))%>%  #add 'other' species to account for all shots
    group_by(sheet_no,method,methodzone,common_name)%>%
    summarise(Weight=sum(tw,na.rm=T),
              Effort=max(Effort))%>%
    ungroup()%>%
    spread(common_name,Weight,fill=0)%>%  #add 0 records
    gather(common_name,Weight,-c(sheet_no,method,methodzone,Effort))%>%
    filter(!common_name=='Other')%>%
    left_join(DATA%>%distinct(common_name,Taxa),by='common_name')%>%
    mutate(cpue=Weight/Effort,
           cpue1=ifelse(method=="GN",cpue*GN.scale,   # (comparable cpue 3000 m GN = 1000 m LL)
                 ifelse(method=="LL",cpue*LL.scale,
                 NA)),
           colr=ifelse(Taxa=='Teleost',"firebrick",
                ifelse(Taxa=='Elasmobranch',"steelblue",
                NA)))%>%
    filter(!is.na(Effort))%>%
    filter(Effort>0)%>%
    group_by(common_name,methodzone)%>%
    mutate(Nominal=mean(cpue1),
           SD=sd(cpue1),
           N=n(),
           SE=SD/sqrt(N))%>%
    ungroup()%>%
    ggplot(aes(x=methodzone, y=cpue1)) + 
    geom_boxplot(outlier.size = 1,aes(fill=Taxa),alpha=.3)+ 
    coord_flip()+
    facet_wrap(~common_name,ncol=2,scales='free_x')+
    ylab(paste("Catch rate per session (Kg per ",GN.scale," m hour or ",LL.scale," hook hour)",sep=''))+
    xlab("")+ 
    theme_PA(strx.siz=12,leg.siz=12,axs.t.siz=12,axs.T.siz=14)+
    theme(legend.position="none")+
    scale_fill_manual(values=c(Teleost="firebrick",Elasmobranch="steelblue"))+
    stat_summary(fun.data=MeanSE, geom="errorbar", color="darkgreen",alpha=1,aes(width=0.5))+
    stat_summary(fun=mean, geom="point", shape=21, size=2, color="darkgreen", fill="darkgreen")+
    scale_y_sqrt()
  return(p)
}

#1. Main commercial
    #1.a. By method-zone 
fn.fig(le.paste("Observer/catch_rates/CPUE.main.sp_GN_LL_y_sqrt"),2400,2000)
fn.method.zone(what=Main.species,
               GN.scale=900,
               LL.scale=300)
dev.off()


    #1.b. by LL configuration  
fn.LL.config=function(what,LL.scale)
{
  p=DATA%>%
    filter(LL.config%in%LL.configs)%>%
    mutate(Effort=ifelse(method=="GN",1000*net_length*soak.time,  #net length in m
                         ifelse(method=="LL",n.hooks*soak.time,
                                NA)),
           common_name=case_when(!common_name%in%what~'Other',
                                 TRUE~common_name))%>%
    filter(common_name%in%c(what,'Other'))%>%  #add 'other' species to account for all shots
    group_by(sheet_no,LL.config,common_name)%>%
    summarise(Weight=sum(tw,na.rm=T),
              Effort=max(Effort))%>%
    ungroup()%>%
    spread(common_name,Weight,fill=0)%>%  #add 0 records
    gather(common_name,Weight,-c(sheet_no,LL.config,Effort))%>%
    filter(!common_name=='Other')%>%
    left_join(DATA%>%distinct(common_name,Taxa),by='common_name')%>%
    mutate(cpue=Weight/Effort,
           cpue1=cpue*LL.scale,   # (comparable cpue 3000 m GN = 1000 m LL)
           colr=ifelse(Taxa=='Teleost',"firebrick",
                ifelse(Taxa=='Elasmobranch',"steelblue",NA)))%>%
    filter(!is.na(Effort))%>%
    filter(Effort>0)%>%
    filter(common_name%in%what)%>%
    group_by(common_name,LL.config)%>%
    mutate(Nominal=mean(cpue1),
           SD=sd(cpue1),
           N=n(),
           SE=SD/sqrt(N))%>%
    ungroup()%>%
    ggplot(aes(x=LL.config, y=cpue1)) + 
    geom_boxplot(outlier.size = 1,aes(fill=Taxa),alpha=.3)+ 
    coord_flip()+
    facet_wrap(~common_name,ncol=2,scales='free_x')+
    ylab(paste("Catch rate per session (Kg per ",LL.scale," hook hour)",sep=''))+xlab("")+ 
    theme_PA(strx.siz=12,leg.siz=12,axs.t.siz=12,axs.T.siz=14)+
    theme(legend.position="none")+
    scale_fill_manual(values=c(Teleost="firebrick",Elasmobranch="steelblue"))+
    stat_summary(fun.data=MeanSE, geom="errorbar", color="darkgreen",alpha=1,aes(width=0.5))+
    stat_summary(fun=mean, geom="point", shape=21, size=2, color="darkgreen", fill="darkgreen")+
    scale_y_sqrt()+scale_x_discrete(position = "top")
  return(p)
}

Display.these=DATA%>%     #select what species to display
  filter(common_name%in%Main.species & method=="LL" &
           !is.na(hooksize) &!is.na(hooktype)&!is.na(wiretrace))%>%
  group_by(common_name)%>%
  tally()%>%ungroup()%>%filter(n>=20)%>%pull(common_name)
fn.fig(le.paste("Observer/catch_rates/CPUE.main.sp_LL.configuration_y_sqrt"),2400,2000)
fn.LL.config(what=Display.these,
             LL.scale=300)
dev.off()

  #1.c  Longline catch rate by shot and fisher to inspect learning behavior
fn.ll.cpue.shot=function(what,LL.scale)
{
  p=DATA%>%
    filter(!is.na(zone) & method=="LL")%>%
    mutate(Effort=n.hooks*soak.time,
           common_name=case_when(!common_name%in%what~'Other',
                                 TRUE~common_name))%>%
    filter(common_name%in%c(what,'Other'))%>%  #add 'other' species to account for all shots
    group_by(sheet_no,boat,common_name)%>%
    summarise(Weight=sum(tw,na.rm=T),
              Effort=max(Effort))%>%
    ungroup()%>%
    spread(common_name,Weight,fill=0)%>%  #add 0 records
    group_by(boat)%>%
    mutate(Shot=1:n())%>%
    ungroup()%>%
    gather(common_name,Weight,-c(sheet_no,boat,Shot,Effort))%>%
    filter(!common_name=='Other')%>%
    left_join(DATA%>%distinct(common_name,Taxa),by='common_name')%>%
    mutate(cpue=Weight/Effort,
           cpue1=cpue*LL.scale,
           colr=ifelse(Taxa=='Teleost',"firebrick",
                       ifelse(Taxa=='Elasmobranch',"steelblue",
                              NA)))%>%
    filter(!is.na(Effort))%>%
    filter(Effort>0)%>%
    filter(!is.na(boat))%>%
    mutate(boat=as.character(as.numeric(as.factor(boat))))%>%
    ggplot(aes(x=Shot, y=cpue1,color=boat)) + 
    geom_point(size=2)+
    geom_line(alpha=0.5,linetype = "dashed")+
    facet_wrap(~common_name,scales='free')+
    ylab(paste("Catch rate per session (Kg per ",LL.scale," hook hour)",sep=''))+
    theme_PA(strx.siz=12,leg.siz=12,axs.t.siz=12,axs.T.siz=14)+
    theme(legend.position="top")
  return(p)
}
fn.fig(le.paste("Observer/catch_rates/CPUE.main.sp_by.shot_vessel"),2400,2000)
fn.ll.cpue.shot(what=Main.species,LL.scale=300)
dev.off()

  #1.d. Standardization
do.standard=FALSE
if(do.standard)
{
  fn.reshape.cpue=function(d,Where)
  {
    d=d%>%
      mutate(Effort=ifelse(method=="GN",1000*net_length*soak.time,  #net length in m
                           ifelse(method=="LL",n.hooks*soak.time,
                                  NA)),
             common_name=case_when(!common_name%in%Where~'Other',
                                   TRUE~common_name))%>%
      filter(common_name%in%c(Where,'Other'))%>%  #add 'other' species to account for all shots
      group_by(sheet_no,method,common_name,hooktype, hooksize, wiretrace,mid.lat,mid.long,zone)%>%
      summarise(Weight=sum(tw,na.rm=T),
                Effort=max(Effort))%>%
      ungroup()%>%
      spread(common_name,Weight,fill=0)%>%  #add 0 records
      gather(common_name,Weight,-c(sheet_no,method,Effort,hooktype, hooksize, wiretrace,mid.lat,mid.long,zone))%>%
      filter(!common_name=='Other')%>%
      left_join(d%>%distinct(common_name,Taxa),by='common_name')%>%
      mutate(cpue=Weight/Effort,
             cpue1=ifelse(method=="GN",cpue*cpue.scaler,   # (comparable cpue 3000 m GN = 1000 m LL)
                          ifelse(method=="LL",cpue*cpue.scaler,
                                 NA)),
             colr=ifelse(Taxa=='Teleost',"firebrick",
                         ifelse(Taxa=='Elasmobranch',"steelblue",NA)))%>%
      filter(!is.na(Effort))%>%
      filter(Effort>0)
    return(d)
  }
  cpue.data.GN=fn.reshape.cpue(d=DATA%>%filter(method=="GN"),Where=Main.species)
  cpue.data.LL=fn.reshape.cpue(d=DATA%>%filter(method=="LL"),Where=Main.species)
  
  #1.b.1 fit GAM
  cpue.stand.out.GN=vector('list',length(Main.species))
  names(cpue.stand.out.GN)=Main.species
  cpue.stand.out.LL=cpue.stand.out.GN
  
  GN.form=formula(cpue1~zone)
  LL.form=formula(cpue1~zone+hooktype+hooksize+wiretrace)
  
  system.time({
    for(m in 1:length(Main.species))
    {
      print(paste(Main.species[m],'GN GAM-------------'))
      D=cpue.data.GN%>%
        filter(common_name==Main.species[m])%>%
        mutate(zone=as.factor(zone))
      
      cpue.stand.out.GN[[m]]=cpue.stand.fun(d=D,Formula=GN.form)  
      
      
      print(paste(Main.species[m],'LL GAM-------------'))
      D=cpue.data.LL%>%
        filter(common_name==Main.species[m])%>%
        filter(!is.na(hooktype))%>%
        filter(!is.na(wiretrace))%>%
        filter(!is.na(hooksize))%>%
        mutate(wiretrace=as.factor(wiretrace),
               hooktype=as.factor(hooktype),
               hooksize=factor(hooksize),
               zone=as.factor(zone))
      form=LL.form
      if(nrow(D)>0)cpue.stand.out.LL[[m]]=cpue.stand.fun(d=D,Formula=form)  
      
      
      
    }
  })   #takes 30 secs
  #gam.check(cpue.stand.out[[m]])
  
  
  #1.b.2 export anova table
  fn.wrp=function(NM,ext)le.paste(paste("Observer/catch_rates",paste('GAM_Anova_',NM,ext,sep=''),sep='/'))
  for(m in 1:length(Main.species))
  {
    NM=Main.species[m]
    
    #GN
    print(paste(Main.species[m],'GN GAM Anova-------------'))
    ft <- as_flextable(cpue.stand.out.GN[[m]]$mod)
    save_as_image(ft, path = fn.wrp(NM,'.GN.png'))
    save_as_docx(ft,path =  fn.wrp(NM,'.GN.docx'))
    rm(ft)
    
    #LL
    print(paste(Main.species[m],'LL GAM Anova-------------'))
    if(!is.null(cpue.stand.out.LL[[m]]$mod))
    {
      ft <- as_flextable(cpue.stand.out.LL[[m]]$mod)
      save_as_image(ft, path = fn.wrp(NM,'.LL.png'))
      save_as_docx(ft,path = fn.wrp(NM,'.LL.docx'))
      rm(ft)
    }
    rm(NM)
    
  }
  
  
  #1.b.3 display term effect
  fn.sig.sp=function(mod,Pred,nm)
  {
    if(!is.null(mod))
    {
      a=summary(mod)$p.pv
      a=a[grep(paste(Pred, collapse="|"),names(a))]
      a=subset(a,a<0.05)
      if(length(a)>0) return(nm)
    }
  }
  Get.hook.type.Pred=vector('list',length(Main.species))
  Get.snood.Pred=Get.hook.size.Pred=Get.hook.type.Pred
  for(m in 1:length(Main.species))
  {
    Get.hook.size.Pred[[m]]=fn.sig.sp(mod=cpue.stand.out.LL[[m]]$mod,Pred="hooksize",nm=names(cpue.stand.out.LL)[m])
    Get.hook.type.Pred[[m]]=fn.sig.sp(mod=cpue.stand.out.LL[[m]]$mod,Pred="hooktype",nm=names(cpue.stand.out.LL)[m])
    Get.snood.Pred[[m]]=fn.sig.sp(mod=cpue.stand.out.LL[[m]]$mod,Pred="wiretrace",nm=names(cpue.stand.out.LL)[m])
  }
  Get.hook.size.Pred=unlist(Get.hook.size.Pred[!sapply(Get.hook.size.Pred,is.null)])
  Get.hook.type.Pred=unlist(Get.hook.type.Pred[!sapply(Get.hook.type.Pred,is.null)])
  Get.snood.Pred=unlist(Get.snood.Pred[!sapply(Get.snood.Pred,is.null)])
  
  Store.zone.method=vector('list',length(Main.species))
  names(Store.zone.method)=Main.species
  Store.hook.type=Store.hook.size=Store.hook.snood=Store.zone.method
  
  for(m in 1:length(Main.species))
  {
    SP=Main.species[m]
    
    #zone-method effect                        
    GN=pred.fun(mod=cpue.stand.out.GN[[m]]$mod,biascor="NO",PRED=c('zone'))%>%
      mutate(method="GN",
             species=SP)
    LL=NULL
    if(!is.null(cpue.stand.out.LL[[m]]))
    {
      LL=pred.fun(mod=cpue.stand.out.LL[[m]]$mod,biascor="NO",PRED=c('zone'))%>%
        mutate(method="LL",
               species=SP)
      
      #Hook type
      if(Main.species[m]%in%Get.hook.type.Pred)
      {
        Store.hook.type[[m]]=pred.fun(mod=cpue.stand.out.LL[[m]]$mod,biascor="NO",PRED=c('hooktype'))%>%
          mutate(species=SP)
      }
      
      #Hook size
      if(Main.species[m]%in%Get.hook.size.Pred)
      {
        Store.hook.size[[m]]=pred.fun(mod=cpue.stand.out.LL[[m]]$mod,biascor="NO",PRED=c('hooksize'))%>%
          mutate(species=SP)
      }
      
      #Snood
      if(Main.species[m]%in%Get.snood.Pred)
      {
        Store.hook.snood[[m]]=pred.fun(mod=cpue.stand.out.LL[[m]]$mod,biascor="NO",PRED=c('wiretrace'))%>%
          mutate(species=SP)
      }
      
    }
    Store.zone.method[[m]]=rbind(GN,LL)
  }
  
  #Method-zone
  fn.fig(le.paste("Observer/catch_rates/Standardized_Catch rates_Method.zone"),2600,1600)
  fn.barplot.cpue(d=do.call(rbind,Store.zone.method)%>%
                    mutate(method.zone=paste(method,zone,sep='-'),
                           species=factor(species,levels=Main.species),
                           x=method.zone,
                           y=response,
                           fill=method.zone,
                           facet=species),
                  YLAB="Standardized catch rate (kg per 1000 m hour / 1000 hook hour)",
                  XLAB='Fishing method and zone',
                  cex=10,
                  Rotate="Yes",
                  Relative="No")
  dev.off()
  
  #Hooksize  
  d=do.call(rbind,Store.hook.size)
  if(!is.null(d))
  {
    fn.fig(le.paste("Observer/catch_rates/Standardized_Catch rates_hooksize"),2000,2400)
    fn.barplot.cpue(d=d%>%
                      mutate(species=factor(species,levels=Main.species),
                             x=hooksize,
                             y=response,
                             fill=hooksize,
                             facet=species),
                    YLAB="Relative standardized catch rate",
                    XLAB='Hook size',
                    cex=16,
                    Relative="Yes")
    dev.off()
  }
  
  
  #Hooktype
  d=do.call(rbind,Store.hook.type)
  if(!is.null(d))
  {
    fn.fig(le.paste("Observer/catch_rates/Standardized_Catch rates_hooktype"),2000,2400)
    fn.barplot.cpue(d=d%>%
                      mutate(species=factor(species,levels=Main.species),
                             x=hooktype,
                             y=response,
                             fill=hooktype,
                             facet=species),
                    YLAB="Relative standardized catch rate",
                    XLAB='Hook type',
                    cex=16,
                    Relative="Yes")
    dev.off()
  }
  
  
  #Snood
  d=do.call(rbind,Store.hook.snood)
  if(!is.null(d))
  {
    fn.fig(le.paste("Observer/catch_rates/Standardized_Catch rates_snood"),2000,2400)
    fn.barplot.cpue(d=d%>%
                      mutate(species=factor(species,levels=Main.species),
                             x=wiretrace,
                             y=response,
                             fill=wiretrace,
                             facet=species),
                    YLAB="Relative standardized catch rate",
                    XLAB='Snood type',
                    cex=16,
                    Relative="Yes")
    dev.off()
  }
  
}


#2. Main discarded
Main.discards=DATA%>%
  filter(!is.na(Code))%>%
  group_by(common_name,retainedflag)%>%
  tally()%>%
  filter(retainedflag=="No")%>%
  arrange(-n)%>%
  ungroup()%>%
  filter(!common_name%in%Main.species)%>%
  mutate(Cumsum=cumsum(n)/sum(n))%>%
  filter(Cumsum<.9)%>%
  pull(common_name)

  #2.a. By method-zone
fn.fig(le.paste("Observer/catch_rates/CPUE.main.sp.discarded_GN_LL_y_sqrt"),2400,2000)
fn.method.zone(what=Main.discards,
               GN.scale=900,
               LL.scale=300)
dev.off()

  #2.b. by LL configuration
Display.these=DATA%>%     #select what species to display
  filter(common_name%in%Main.discards & method=="LL" &
           !is.na(hooksize) &!is.na(hooktype)&!is.na(wiretrace))%>%
  group_by(common_name)%>%
  tally()%>%ungroup()%>%filter(n>=20)%>%pull(common_name)
fn.fig(le.paste("Observer/catch_rates/CPUE.main.sp.discarded_LL.configuration_y_sqrt"),2400,2200)
fn.LL.config(what=Display.these,
             LL.scale=300)
dev.off()

  #2.c. Standardization
Do.disc.cpue=FALSE
if(Do.disc.cpue)
{
  # This.disc=DATA%>%
  #   filter(retainedflag=='No')%>%
  #   group_by(common_name)%>%
  #   tally()%>%
  #   filter(n>=20)%>%
  #   filter(!common_name=='')%>%
  #   pull(common_name)
  # This.disc=subset(This.disc,!This.disc%in%c(Main.species,"Stingrays"))
  This.disc=c("Buffalo bream","Dusky morwong","Port Jackson","Southern eagle ray")
  
  cpue.data.GN=fn.reshape.cpue(d=DATA%>%filter(method=="GN"),Where=This.disc)
  cpue.data.LL=fn.reshape.cpue(d=DATA%>%filter(method=="LL"),Where=This.disc)
  
  #5.c.1 fit GAM
  cpue.stand.out.GN=vector('list',length(This.disc))
  names(cpue.stand.out.GN)=This.disc
  cpue.stand.out.LL=cpue.stand.out.GN
  
  GN.form=formula(cpue1~zone)
  LL.form=formula(cpue1~zone+hooktype+hooksize+wiretrace)
  
  system.time({
    for(m in 1:length(This.disc))
    {
      print(paste(This.disc[m],'GN GAM-------------'))
      D=cpue.data.GN%>%
        filter(common_name==This.disc[m])%>%
        mutate(zone=as.factor(zone))
      
      if(!This.disc[m]%in%c("Fiddler ray","Guitarfish & shovelnose rays"))cpue.stand.out.GN[[m]]=cpue.stand.fun(d=D,Formula=GN.form)  
      
      
      print(paste(This.disc[m],'LL GAM-------------'))
      D=cpue.data.LL%>%
        filter(common_name==This.disc[m])%>%
        filter(!is.na(hooktype))%>%
        filter(!is.na(wiretrace))%>%
        filter(!is.na(hooksize))%>%
        mutate(wiretrace=as.factor(wiretrace),
               hooktype=as.factor(hooktype),
               hooksize=factor(hooksize),
               zone=as.factor(zone))
      form=LL.form
      if(This.disc[m]=="Fiddler ray")form=formula(cpue1~hooktype+hooksize+wiretrace)
      if(nrow(D)>0)cpue.stand.out.LL[[m]]=cpue.stand.fun(d=D,Formula=form) 
      
    }
  }) 
  
  #5.b.2 export anova table
  for(m in 1:length(This.disc))
  {
    NM=This.disc[m]
    
    #GN
    print(paste(This.disc[m],'GN GAM Anova-------------'))
    if(!is.null(cpue.stand.out.GN[[m]]$mod))
    {
      ft <- as_flextable(cpue.stand.out.GN[[m]]$mod)
      save_as_image(ft, path = fn.wrp(NM,'.GN.png'))
      save_as_docx(ft,path =  fn.wrp(NM,'.GN.docx'))
      rm(ft)
    }
    
    
    #LL
    print(paste(This.disc[m],'LL GAM Anova-------------'))
    if(!is.null(cpue.stand.out.LL[[m]]$mod))
    {
      ft <- as_flextable(cpue.stand.out.LL[[m]]$mod)
      save_as_image(ft, path = fn.wrp(NM,'.LL.png'))
      save_as_docx(ft,path = fn.wrp(NM,'.LL.docx'))
      rm(ft)
    }
    rm(NM)
    
  }
  
  #5.b.3 display term effect
  Store.zone.method=vector('list',length(This.disc))
  names(Store.zone.method)=This.disc
  Store.hook.type=Store.hook.size=Store.hook.snood=Store.zone.method
  
  Get.hook.type.Pred=vector('list',length(Main.species))
  Get.snood.Pred=Get.hook.size.Pred=Get.hook.type.Pred
  for(m in 1:length(This.disc))
  {
    Get.hook.size.Pred[[m]]=fn.sig.sp(mod=cpue.stand.out.LL[[m]]$mod,Pred="hooksize",nm=names(cpue.stand.out.LL)[m])
    Get.hook.type.Pred[[m]]=fn.sig.sp(mod=cpue.stand.out.LL[[m]]$mod,Pred="hooktype",nm=names(cpue.stand.out.LL)[m])
    Get.snood.Pred[[m]]=fn.sig.sp(mod=cpue.stand.out.LL[[m]]$mod,Pred="wiretrace",nm=names(cpue.stand.out.LL)[m])
  }
  Get.hook.size.Pred=unlist(Get.hook.size.Pred[!sapply(Get.hook.size.Pred,is.null)])
  Get.hook.type.Pred=unlist(Get.hook.type.Pred[!sapply(Get.hook.type.Pred,is.null)])
  Get.snood.Pred=unlist(Get.snood.Pred[!sapply(Get.snood.Pred,is.null)])

  
  for(m in 1:length(This.disc))
  {
    SP=This.disc[m]
    
    #zone-method effect                        
    GN=NULL
    if(!is.null(cpue.stand.out.GN[[m]]$mod))
    {
      GN=pred.fun(mod=cpue.stand.out.GN[[m]]$mod,biascor="NO",PRED=c('zone'))%>%
        mutate(method="GN",
               species=SP)
    }
    
    LL=NULL
    if(!is.null(cpue.stand.out.LL[[m]]))
    {
      if('zone'%in%all.vars(cpue.stand.out.LL[[m]]$mod$formula))  LL=pred.fun(mod=cpue.stand.out.LL[[m]]$mod,biascor="NO",PRED=c('zone'))%>%
          mutate(method="LL",
                 species=SP)
      
      #Hook type
      if(This.disc[m]%in%Get.hook.type.Pred)
      {
        Store.hook.type[[m]]=pred.fun(mod=cpue.stand.out.LL[[m]]$mod,biascor="NO",PRED=c('hooktype'))%>%
          mutate(species=SP)
      }
      
      #Hook size
      if(This.disc[m]%in%Get.hook.size.Pred)
      {
        Store.hook.size[[m]]=pred.fun(mod=cpue.stand.out.LL[[m]]$mod,biascor="NO",PRED=c('hooksize'))%>%
          mutate(species=SP)
      }
      
      #Snood
      if(This.disc[m]%in%Get.snood.Pred)
      {
        Store.hook.snood[[m]]=pred.fun(mod=cpue.stand.out.LL[[m]]$mod,biascor="NO",PRED=c('wiretrace'))%>%
          mutate(species=SP)
      }
      
    }
    Store.zone.method[[m]]=rbind(GN,LL)
  }
  
  #Method-zone
  fn.fig(le.paste("Observer/catch_rates/Standardized_Catch rates_Method.zone_discards"),2600,1600)
  fn.barplot.cpue(d=do.call(rbind,Store.zone.method)%>%
                             mutate(method.zone=paste(method,zone,sep='-'),
                                    species=factor(species,levels=This.disc),
                                    x=method.zone,
                                    y=response,
                                    fill=method.zone,
                                    facet=species),
                           YLAB="Standardized catch rate (kg per 1000 m hour / 1000 hook hour)",
                           XLAB='Fishing method and zone',
                           cex=10,
                           Rotate="Yes",
                           Relative="No")
  dev.off()
  
  #Hooksize
  d=do.call(rbind,Store.hook.size)
  if(!is.null(d))
  {
    fn.fig(le.paste("Observer/catch_rates/Standardized_Catch rates_hooksize_discards"),2000,2400)
    fn.barplot.cpue(d=d%>%
                      mutate(species=factor(species,levels=This.disc),
                             x=hooksize,
                             y=response,
                             fill=hooksize,
                             facet=species),
                   YLAB="Relative standardized catch rate",
                   XLAB='Hook size',
                   cex=16,
                   Relative="Yes")
    dev.off()
  }
  
  #Hooktype
  d=do.call(rbind,Store.hook.type)
  if(!is.null(d))
  {
    fn.fig(le.paste("Observer/catch_rates/Standardized_Catch rates_hooktype_discards"),2000,2400)
    fn.barplot.cpue(d=d%>%
                       mutate(species=factor(species,levels=This.disc),
                              x=hooktype,
                              y=response,
                              fill=hooktype,
                              facet=species),
                    YLAB="Relative standardized catch rate",
                    XLAB='Hook type',
                    cex=16,
                    Relative="Yes")
    dev.off()
    
  }
  
  #Snood
  d=do.call(rbind,Store.hook.snood)
  if(!is.null(d))
  {
    fn.fig(le.paste("Observer/catch_rates/Standardized_Catch rates_snood_discards"),2000,2400)
    fn.barplot.cpue(d=d%>%
                        mutate(species=factor(species,levels=This.disc),
                               x=wiretrace,
                               y=response,
                               fill=wiretrace,
                               facet=species),
                   YLAB="Relative standardized catch rate",
                   XLAB='Snood type',
                   cex=16,
                   Relative="Yes")
    dev.off()
  }
}


#---------Observer retained vs discarded ------------ 

#Export table of retained or discarded numbers, size and release condition by method
fn.tab=function(what)
{
  d=DATA%>%filter(method==what)
  
  Out_table_Retained=d%>%
    #filter(!is.na(retainedflag))%>%
    group_by(common_name,retainedflag,CAES_Code)%>%
    tally()%>%
    ungroup%>%
    spread(retainedflag,n,fill=0)%>%
    rename(unk=`<NA>`)%>%
    mutate(n=No+Yes+unk)%>%
    arrange(-n)%>%
    dplyr::select(-n)
  Out_table_disc.size=d%>%
    filter(retainedflag=='No')%>%
    group_by(common_name)%>%
    summarise(min.size=round(min(Size,na.rm=T)),
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
    dplyr::rename(Discarded=No,
                  Retained=Yes)%>%
    relocate(Retained, .before = Discarded)
  
  Rel.cond=d%>%
    group_by(common_name,rel.cond)%>%
    summarise(N=sum(number))%>%
    filter(!(is.na(rel.cond)|rel.cond==''))%>%
    mutate(rel.cond=paste('Condition',rel.cond,sep='.'))%>%
    spread(rel.cond,N,fill=0)%>%
    mutate(Tot=Condition.0+Condition.1+Condition.2+Condition.3,
           Condition.0=paste(round(100*Condition.0/Tot,1),'%',sep=''),
           Condition.1=paste(round(100*Condition.1/Tot,1),'%',sep=''),
           Condition.2=paste(round(100*Condition.2/Tot,1),'%',sep=''),
           Condition.3=paste(round(100*Condition.3/Tot,1),'%',sep=''))%>%
    dplyr::select(-Tot)
  
  Out_table_Retained=Out_table_Retained%>%
    left_join(Rel.cond,by='common_name')
  
  
  return(Out_table_Retained)
}
out=fn.tab(what="GN")
write.csv(out,le.paste("Observer/retained/Retained_species_table_GN.csv"),row.names=F)  
out=fn.tab(what="LL")
write.csv(out,le.paste("Observer/retained/Retained_species_table_LL.csv"),row.names=F)  



#Plot percentage retained and discarded by gear
  #a. number of individuals
p1=DATA%>%
  filter(!is.na(retainedflag))%>%
  ggplot(aes(x=as.factor(method), fill=as.factor(retainedflag)))+
  geom_bar(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..]), position="dodge" ) +
  geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ),
            stat="count", position=position_dodge(0.9), vjust=-0.5)+
  scale_y_continuous(labels = scales::percent)+
  theme_PA(lgT.siz=14,leg.siz=12,axs.t.siz=12,axs.T.siz=14)+
  ylab("Percentage")+xlab('Method')+labs(fill='Retained') 


p2=DATA%>%
  filter(retainedflag=='No' & !(rel.cond==''|is.na(rel.cond)))%>%
  group_by(rel.cond,method)%>%
  summarise(N=sum(number))%>%
  group_by(method)%>%
  mutate(sum=N/sum(N),
         ymax=cumsum(N),
         ymin=c(0, head(ymax, n=-1)))%>%
  arrange(method)%>%
  ggplot(aes(x=1,y=sum,fill=rel.cond)) + 
  geom_bar(stat="identity",width=2) + 
  coord_polar(theta='y')+
  facet_wrap(~ method)+
  scale_fill_manual(values=c("deepskyblue2",'khaki3',"tomato4","forestgreen"))+
  scale_x_continuous(limits=c(-1,2.5))+
  ggtitle(label ="",
          subtitle = "Release condition for discarded individuals")+
  theme_void() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 16),
        plot.subtitle = element_text(size = 17,face = "bold",hjust=0.25))
  
fn.fig(le.paste("Observer/retained/GN_LLretained_discarded_individuals"),2100,2400)
ggarrange(p1,p2, ncol = 1, heights=c(1.25,1))
dev.off()



    #Stats
fn.glm.retained=function(d,Formula,Pred,cex,XLAB,Rotate=NULL,Anova.title,do.plot)
{
  d=d%>%
    mutate(Retained=ifelse(retainedflag=='Yes',1,
                           ifelse(retainedflag=='No',0,
                                  NA)))%>%
    filter(!is.na(Retained))%>%
    mutate(Gear=ifelse(method=="GN","Gillnet",LL.config))%>%
    filter(!grepl("NA",Gear))%>%
    mutate(Gear=as.factor(Gear),
           method=as.factor(method),
           zone=as.factor(zone))
  
  #rum model
  mod=gam(Formula,dat=d,family = "binomial")
  
  #export anova table
  ft <- as_flextable(mod)
  save_as_image(ft, path = le.paste(paste("Observer/retained/ANOVA_",Anova.title,".png",sep='')))
  rm(ft)
  
  #Plot term effect
  if(do.plot)
  {
    p=pred.fun(mod=mod,biascor="NO",PRED=Pred)%>%
      mutate(x=!!sym(Pred),
             y=prob,
             fill=!!sym(Pred))%>%
      ggplot(aes(x,y,fill=fill))+
      geom_bar(stat="identity")+
      geom_errorbar(aes(x, ymin=lower.CL, ymax=upper.CL),colour='black',width=0.25)+
      theme_PA(strx.siz=cex-2,lgT.siz=cex,leg.siz=cex-2,axs.t.siz=cex-3,axs.T.siz=cex)+
      theme(legend.position='none')+
      ylab("Probability of retaining an individual")+
      xlab(XLAB)
    if(!is.null(Rotate))p=p+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    fn.fig(le.paste(paste("Observer/retained/Standardized_retained_",Pred,sep='')),2400,2000)
    print(p)
    dev.off()
  }
  
}
      #Method effect
fn.glm.retained(d=DATA%>%
                  filter(!is.na(retainedflag)),
                Formula=formula(Retained~method+zone),
                Pred='method',
                cex=18,
                XLAB="Method",
                Anova.title="Method",
                do.plot=T)
      #Longline configuration effect
fn.glm.retained(d=DATA%>%
                  filter(!is.na(retainedflag) & method=='LL')%>%
                  filter(!is.na(hooktype) & !is.na(wiretrace) & !is.na(hooksize))%>%
                  mutate(hooktype=as.factor(hooktype),
                         wiretrace=as.factor(wiretrace),
                         hooksize=factor(hooksize)),
                Formula=formula(Retained~hooktype+hooksize+wiretrace+zone),
                Pred=c('hooksize','hooktype','wiretrace'),
                cex=18,
                XLAB="Longline configuration",
                Anova.title="longine.config",
                do.plot=F)


  #b. number of species
fn.fig(le.paste("Observer/retained/GN_LLretained_discarded_species"),2400,1600)
DATA%>%
  filter(!is.na(retainedflag))%>%
  mutate(SP.GR=paste(species,method))%>%
  distinct(species,method,retainedflag,SP.GR)%>%
  ggplot(aes(x=as.factor(method), fill=as.factor(retainedflag)))+
  geom_bar(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..]), position="dodge" ) +
  geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ),
            stat="count", position=position_dodge(0.9), vjust=-0.5)+
  scale_y_continuous(labels = scales::percent)+
  theme_PA(lgT.siz=14,leg.siz=12,axs.t.siz=12,axs.T.siz=14)+
  ylab("Percentage")+xlab('Method')+labs(fill='Retained') 
dev.off()


#---------Hook baiting, and numbers losts ------------

#1. Lost hooks 
LL.sessions=DATA%>%
            filter(method=='LL')%>%
            distinct(sheet_no)%>%
  arrange(sheet_no)%>%
  mutate(Session=1:n())
  
  
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
               Kept.hk=n.hooks-Lost.hk)%>%
  dplyr::select(-comments.hdr)%>%
  gather("Lost", "Percent", -c(sheet_no,method,n.hooks))%>%
  mutate(Percent=100*Percent/n.hooks)%>%
  filter(n.hooks>0)%>%
  left_join(LL.sessions,by='sheet_no')%>%
  mutate(Lost=ifelse(Lost=='Lost.hk','Lost','Not lost'))%>%
  arrange(Session)

Plost=Lost.huk%>%
  ggplot(aes(Session,Percent,fill=Lost))+
  geom_col()+
  geom_text(data = Lost.huk%>%distinct(Session,.keep_all=T),
            aes(y=110,label = paste('n=',n.hooks)),angle=75,size=2.5)+
  ylim(0,120)+
  ylab("Percentage of hooks lost")+
  theme_PA(leg.siz=12,axs.t.siz=10,axs.T.siz=12)+
  theme(legend.position = "top",
        legend.title = element_blank())+xlab('')


#2. baiting efficiency  
Baiting=DATA%>%
  filter(method=="LL")%>%
  distinct(sheet_no,.keep_all=T)%>%
  mutate(Efficiency=100*n.hooks/hooks.baited,
         Efficiency=ifelse(Efficiency>100,100,Efficiency))%>%
  left_join(LL.sessions,by='sheet_no')
P.baitn.eff=Baiting%>%
              ggplot(aes(Session,Efficiency))+
              geom_point(size=3,color="forestgreen")+
              ylim(0,100)+
              theme_PA(axs.t.siz=10,axs.T.siz=12)+
              theme(legend.position = "top",
                    legend.title = element_blank())+
            ylab("Deployment efficiency")+xlab('')


#3. baiting time   
Baiting.time=Hook.combos[,c('sheet_no','baiting.time','baiting.crew','hooks.baited')]%>%
              filter(!is.na(baiting.time) & !is.na(baiting.crew))%>%
              filter(!baiting.time=="NA" & !is.na(hooks.baited))%>%
  left_join(Baiting%>%distinct(sheet_no,Session),by='sheet_no')%>%
          mutate(baiting.time=as.numeric(baiting.time),
                 baiting.crew=as.numeric(baiting.crew),
                 hook.per.hour_person=60*(hooks.baited/baiting.crew)/baiting.time)
P.baitn.time=Baiting.time%>%
          ggplot(aes(Session,hook.per.hour_person))+
  geom_hline(yintercept = mean(Baiting.time$hook.per.hour_person),
             col="steelblue",alpha=.6,size=2)+
          geom_point(size=3,color="forestgreen")+
          xlim(0,max(Baiting$Session))+
          theme_PA(axs.t.siz=10,axs.T.siz=12)+
          theme(legend.position = "top",
                legend.title = element_blank())+
          ylab("Baited hooks per person-hour")+
          expand_limits(x = 0, y = 0)  


fn.fig(le.paste("Observer/Hook baiting and lost"),2000,2400)
ggarrange(Plost,P.baitn.eff,P.baitn.time,ncol=1,heights=c(1.5,1,1))
dev.off()



#---------Longine selectivity ------------
#function for predicting selectivity
pred.normal.fixed=function(l,k,m,sigma) exp(-((l-k*m)^2)/(2*(sigma)^2))
pred.normal.prop=function(l,m,a1,a2) exp(-(((l- a1*m)^2)/(2*a2*m^2)))
pred.gamma=function(l,m,k,alpha) ((l/((alpha-1)*k*m))^(alpha-1))*exp(alpha-1-(l/(k*m)))
pred.lognormal=function(l,m,m1,mu,sigma) (1/l)*exp(mu+(log(m/m1))-((sigma^2)/2)-((log(l)-mu-log(m/m1))^2)/(2*(sigma^2)))

#function for predicting selectivity 
out.sel=function(d,BEST,Fixed.equal.power)
{
  if(Fixed.equal.power) DAT=d
  id=match(BEST$Model,names(DAT))
  
  #predict selectivity
  #Plotlen
  dat=data.frame(TL.cm=DAT[[id]]$plotlens)
  Pars=d[[id]]$gear.pars
  if(BEST$Model=="norm.loc")
  {
    k=Pars[match("k",rownames(Pars)),1]
    sigma=Pars[match("sigma",rownames(Pars)),1] 
    dat$'10'=pred.normal.fixed(l=dat$TL.cm,k,m=10,sigma)
    dat$'12'=pred.normal.fixed(l=dat$TL.cm,k,m=12,sigma)
    dat$'14'=pred.normal.fixed(l=dat$TL.cm,k,m=14,sigma)
  }
  if(BEST$Model=="norm.sca")
  {
    a1=Pars[match("k1",rownames(Pars)),1]
    a2=Pars[match("k2",rownames(Pars)),1] 
    dat$'10'=pred.normal.prop(l=dat$TL.cm,m=10,a1,a2)
    dat$'12'=pred.normal.prop(l=dat$TL.cm,m=12,a1,a2)
    dat$'14'=pred.normal.prop(l=dat$TL.cm,m=14,a1,a2)
  }
  if(BEST$Model=="gamma")
  {
    k=Pars[match("k",rownames(Pars)),1]
    alpha=Pars[match("alpha",rownames(Pars)),1] 
    dat$'10'=pred.gamma(l=dat$TL.cm,m=10,k,alpha)
    dat$'12'=pred.gamma(l=dat$TL.cm,m=12,k,alpha)
    dat$'14'=pred.gamma(l=dat$TL.cm,m=14,k,alpha)
  }
  if(BEST$Model=="lognorm")
  {
    m1=min(DAT[[id]]$meshsizes)
    mu=Pars[grep('mu1',rownames(Pars)),1]
    sigma=Pars[grep('sigma',rownames(Pars)),1] 
    dat$'10'=pred.lognormal(l=dat$TL.cm,m=10,m1,mu,sigma)
    dat$'12'=pred.lognormal(l=dat$TL.cm,m=12,m1,mu,sigma)
    dat$'14'=pred.lognormal(l=dat$TL.cm,m=14,m1,mu,sigma)
  }
  return(dat)
}

#wrapping function
fn.LL.sel=function(d,Min.sample,Min.hooks,Size.Interval,convrt.this,Fitfunction,
                   Rtype,Min.length,Max.length)
{
  #Table 1. Number of observations
  Table1=d%>%
    filter(!is.na(Size) &!is.na(hooksize) & !is.na(common_name) & !common_name=="Unidentified")%>%
    group_by(CAES_Code,common_name,scientific_name,hooksize)%>%
    tally()%>%
    spread(hooksize,n,fill=0)%>%
    data.frame%>%
    arrange(CAES_Code)%>%
    rename_with(~str_remove(., 'X'))%>%
    dplyr::select(-CAES_Code)%>%
    rename('Common name'=common_name,
           'Scientific name'=scientific_name)
  ft <- flextable(Table1)
  save_as_docx(ft,path = le.paste('Selectivity_LL/Table1.docx'))
  
  #Map
  do.map=FALSE
  if(do.map)
  {
    library("rnaturalearth")
    world <- ne_countries(scale = "medium", returnclass = "sf")
    Map.dat=d%>%distinct(sheet_no,.keep_all=T)
    Long.range=range(Map.dat$mid.long,na.rm=T)
    Lat.range=range(Map.dat$mid.lat,na.rm=T)
    Poly=data.frame(x=c(Long.range,rev(Long.range)),y=c(rep(Lat.range[1],2),rep(Lat.range[2],2)))
    Inset=ggplot(data = world) +
      geom_sf(color = "black", fill = "white") +
      coord_sf(xlim =c(Long.range[1],153) , ylim = c(-43,-11), expand = T)+
      theme(axis.title=element_blank(),
            axis.text=element_blank(),
            axis.ticks=element_blank())+ 
      annotate(geom="text", x=134, y=-24, label="Australia",size=12)+
      geom_polygon(data=Poly,aes(x,y),fill='black',alpha=.35)
    p=ggplot(data = world) +
      geom_sf(color = "black", fill = "grey60") +
      coord_sf(xlim =Long.range , ylim = Lat.range, expand = T) +
      xlab("Longitude") + ylab("Latitude")+
      geom_point(data=Map.dat,aes(x=mid.long, y=mid.lat,size=10),shape=21,alpha=0.4,fill="brown4")+
      theme(legend.title = element_text(size = 12),
            legend.text = element_text(size = 10),
            legend.position = "none",
            legend.key=element_blank(),
            legend.background=element_blank(),
            legend.direction = "vertical",
            legend.box = "horizontal",
            axis.text.x = element_text(size = 16),
            axis.text.y = element_text(size = 15.5),
            axis.title.x = element_text(size = 24),
            axis.title.y = element_text(size = 24),
            plot.margin=unit(c(.1,.1,.1,.1),"cm"))
    p+annotation_custom(ggplotGrob(Inset),xmin = 115, xmax = 119, ymin = -32, ymax = -30.5)
    ggsave(le.paste('Selectivity_LL/Figure 1.tiff'), width = 8,height = 12,dpi = 300, compression = "lzw")
    
  }
 
  #Select species with enough observations
  Tabl=d%>%
    filter(!is.na(Size) & !is.na(common_name))%>%
    group_by(common_name,method)%>%
    tally()%>%
    spread(method,n,fill=0)%>%
    filter(LL>Min.sample)%>%
    pull(common_name)
  d=d%>%filter(common_name%in%Tabl & hooksize%in%c(10,12,14))
  TAB=table(d$common_name,d$hooksize)
  TAB[TAB<Min.sample]=0
  TAB[TAB>=Min.sample]=1
  d=d%>%
    filter(common_name%in%names(which(rowSums(TAB)>=Min.hooks)) & !is.na(hooktype))
  
  #Convert FL to TL
  d=d%>%
    mutate(a_FL.to.TL=ifelse(common_name=="Port Jackson",1.105265,a_FL.to.TL),
           b_FL.to.TL=ifelse(common_name=="Port Jackson",0.4579532,b_FL.to.TL),
           tl=ifelse(common_name=="Port Jackson" & tl>150,NA,tl),
           tl=ifelse(common_name%in% convrt.this & is.na(tl),
                     fl*a_FL.to.TL+b_FL.to.TL,tl))%>%
    filter(!is.na(tl))
  
  #Check increase in total length with hook size
  colfunc <- colorRampPalette(c("red","orange"))
  cols=colfunc(length(unique(d$hooktype)))
  names(cols)=sort(unique(d$hooktype))
  size.vs.hook=d%>%
    group_by(hooksize,hooktype,common_name)%>%
    summarise(Mean=mean(tl),
              SD=sd(tl))%>%
    mutate(Hook=as.character(hooktype))
  size.vs.hook%>%
    mutate(hooksize=ifelse(hooktype=='Circular',hooksize+.15,hooksize))%>%
    ggplot(aes(x=hooksize, y= Mean, colour=Hook)) + 
    geom_errorbar(aes(ymin= Mean-SD, ymax= Mean+SD), width=.1) +
    geom_point(size=3) +
    ylab("Mean total length (+/-SD)") + xlab("Hook size") +
    facet_wrap(vars(common_name), scales = "free_y")+
    scale_color_manual("Hook type",values=cols)+
    theme_PA(Ttl.siz=18,strx.siz=16,leg.siz=16,axs.t.siz=13,axs.T.siz=20)+
    theme(legend.position="top",
          legend.title = element_blank())
  ggsave(le.paste('Selectivity_LL/Figure_S1.tiff'), width = 10,height = 10, dpi = 300, compression = "lzw")
  
  #drop if size not increasing with hook size
  size.vs.hook.sp=size.vs.hook%>%
    dplyr::select(-SD,-Hook)%>%
    mutate(hooksize=paste("Size.",hooksize,sep=''))%>%
    spread(hooksize,Mean)%>%
    mutate(Keep=ifelse(Size.14>Size.12,'Yes',"No"),
           Keep=ifelse(Size.12>Size.10 & Keep=='Yes','Yes',"No"))%>%
    filter(Keep=="Yes")%>%
    pull(common_name)
  #d=d%>%filter(common_name%in%unique(size.vs.hook.sp))
  
  #add size clase
  d1=d%>%
    mutate(size.class=Size.Interval*floor(tl/Size.Interval)+Size.Interval/2)%>%
    dplyr::select(common_name,hooksize,hooktype,size.class,tl,fl,Size)
  
  #Display size frequency
  library(lemon)
  fig2=function(d1)
  {
    cols=colfunc(length(unique(d1$hooksize)))
    p=d1 %>%
      ggplot( aes(x=TL, fill=hooksize)) +
      geom_histogram(binwidth = 5, alpha=0.8,colour='grey40',size=.1)  +
      facet_wrap(vars(common_name), scales = "free")+
      xlab("Total length (cm)")+ ylab("Frequency")+scale_fill_manual(values=cols)+
      theme_PA(strx.siz=18,leg.siz=16,axs.t.siz=16,axs.T.siz=20)+
      theme(legend.position="top",
            legend.title=element_blank())
  }
  fig2(d1=d%>%
         mutate(hooksize=as.factor(paste(hooksize,"/0",sep='')),
                TL=tl)%>%
         filter(common_name%in%sp))
  ggsave(le.paste('Selectivity_LL/Figure 2.tiff'), width = 10,height = 10, dpi = 300, compression = "lzw")
  
  #calculate selectivity by species based on Millar & Holst 1997
  sp=unique(d1$common_name)
  hk.typ=unique(d1$hooktype)
  PlotLens=seq(Min.length+Size.Interval/2,Max.length-Size.Interval/2,by=Size.Interval)
  Store.sel=vector('list',length(sp))
  names(Store.sel)=sp
  Store.tab=Store.sel
  for(i in 1:length(sp))
  {
    Store=vector('list',length(hk.typ))
    names(Store)=hk.typ
    store.tab=Store
    for(h in 1:length(hk.typ))
    {
      #Tabulate observations by mid size class and hook size   
      tab=d1%>%
        filter(common_name==sp[i] & hooktype==hk.typ[h])%>%
        group_by(hooksize,size.class,common_name)%>%
        summarise(n=n())
      Hooksize=as.numeric(unique(tab$hooksize)) 
      tab=tab%>%
        spread(hooksize,n,fill=0)%>%
        data.frame
      
      #Fit Miller model with equal fishing power
      pwr=rep(1,length(Hooksize))
      Equal.power=vector('list',length(Rtype))
      names(Equal.power)=Rtype
      if(Fitfunction=='gillnetfit')
      {
        for(f in 1:length(Equal.power))
        {
          Equal.power[[f]]=gillnetfit(data=as.matrix(tab%>%dplyr::select(-'common_name')),
                                      meshsizes=Hooksize,
                                      type=Rtype[f],
                                      rel=pwr,
                                      plots=c(F,F),
                                      plotlens=PlotLens,
                                      plotlens_age=100,
                                      details=T)
        }
      }
      Store[[h]]=Equal.power
      store.tab[[h]]=tab
    }
    Store.sel[[i]]=Store
    Store.tab[[i]]=store.tab
  }
  
  #Tabulate model fit
  fn.rnd=function(x) sprintf(round(x,2), fmt = '%#.2f')
  Table.mod.fit=vector('list',length(sp))
  names(Table.mod.fit)=sp
  for(s in 1:length(sp))
  {
    dummy=vector('list',length(hk.typ))
    for(h in 1:length(hk.typ))
    {
      Tab=data.frame(Model=names(Store.sel[[s]][[h]]))%>%
        mutate(Equal_Param1=NA,
               Equal_Param2=NA,
               Equal_Deviance=NA,
               hooktype=hk.typ[h])%>%
        relocate(hooktype)
      for(f in 1:length(Rtype))
      {
        Tab$Equal_Deviance[f]=fn.rnd(Store.sel[[s]][[h]][[f]]$fit.stats['model_dev'])
        PaR=fn.rnd(Store.sel[[s]][[h]][[f]]$gear.pars[1:2,'estimate'])
        errOr=fn.rnd(Store.sel[[s]][[h]][[f]]$gear.pars[1:2,'s.e.'])
        Tab$Equal_Param1[f]=paste(PaR[1]," (",errOr[1],")",sep='')
        Tab$Equal_Param2[f]=paste(PaR[2]," (",errOr[2],")",sep='')
        rm(PaR,errOr)
      }
      dummy[[h]]=Tab
    }
    Table.mod.fit[[s]]=do.call(rbind,dummy)
  }
  Table.mod.fit=do.call(rbind,Table.mod.fit)
  Table.mod.fit$Species=sub("*\\.[0-9]", "", rownames(Table.mod.fit))
  Table.mod.fit=Table.mod.fit%>%
    relocate(Species)%>%
    mutate(Model=ifelse(Model=="norm.loc","Normal (fixed spread)",
                        ifelse(Model=="norm.sca","Normal (prop. spread)",
                               ifelse(Model=="gamma","Gamma",
                                      ifelse(Model=="lognorm","Lognormal",
                                             Model)))))%>%
    Table.mod.fit$Species[duplicated(Table.mod.fit$Species)] <- ""
  Table.mod.fit[is.na(Table.mod.fit)] <- ""
  Table.mod.fit[Table.mod.fit=="NaN"] <- ""
  row.names(Table.mod.fit)=NULL
  ft <- flextable(Table.mod.fit)
  save_as_docx(ft,path = le.paste('Selectivity_LL/Table2.docx'))
  
  
  #select best fit
  Best.fit=vector('list',length(sp))
  names(Best.fit)=sp
  for(i in 1:length(sp))
  {
    Store=vector('list',length(hk.typ))
    names(Store)=hk.typ
    for(h in 1:length(hk.typ))
    {
      Tab=data.frame(Model=names(Store.sel[[i]][[h]]),
                     Equal_dev=NA)
      for(f in 1:length(Rtype))
      {
        Tab$Equal_dev[f]=Store.sel[[i]][[h]][[f]]$fit.stats['model_dev']
      }
      Store[[h]]=Tab[which.min(Tab[,2]),]%>%mutate(Fishing.power="Equal.power")%>%rename(Dev=Equal_dev)
    }
    Best.fit[[i]]=Store
  }
  
  
  #Plot fit residuals
  fn.plt.FigS3=function(h)
  {
    for(s in 1:length(sp))
    {
      for(f in 1:length(Store.sel[[s]][[h]]))
      {
        with(Store.sel[[s]][[h]][[f]],
             {
               MAIN=""
               plot.resids(devres,meshsizes,lens,title=MAIN)
             })
        if(s==1)
        {
          MAIN=with(Store.sel[[s]][[h]][[f]],
                    ifelse(type=="norm.loc","Normal (fixed spread)",
                           ifelse(type=="norm.sca","Normal (prop. spread)",
                                  ifelse(type=="gamma","Gamma",
                                         ifelse(type=="lognorm","Lognormal",NA)))))
          mtext(MAIN,3,cex=.95)
        }
      }
      mtext( sp[s],4,cex=1,las=3)
    }
  }
  tiff(file=le.paste("Selectivity_LL/Figure.S3_Ezb.tiff"),width = 2100, height = 2400,units = "px", res = 300, compression = "lzw")    
  par(mfrow=c(length(sp),length(Rtype)),mar=c(1.5,1.2,.2,.3),oma=c(1.75,2,1,1),mgp=c(1,.5,0),las=1)
  fn.plt.FigS3(h=1)
  mtext("Total length (mm)",1,outer=T,line=.35,cex=1.25)
  mtext("Hook size (/0)",2,outer=T,line=.35,cex=1.25,las=3)
  dev.off()
  
  tiff(file=le.paste("Selectivity_LL/Figure.S3_Circular.tiff"),width = 2100, height = 2400,units = "px", res = 300, compression = "lzw")    
  par(mfrow=c(length(sp),length(Rtype)),mar=c(1.5,1.2,.2,.3),oma=c(1.75,2,1,1),mgp=c(1,.5,0),las=1)
  fn.plt.FigS3(h=2)
  mtext("Total length (mm)",1,outer=T,line=.35,cex=1.25)
  mtext("Hook size (/0)",2,outer=T,line=.35,cex=1.25,las=3)
  dev.off()
  
  #Compare observed VS predicted distribution
  Cols.type=c('blue','brown','forestgreen','red')
  names(Cols.type)=Rtype
  fn.freq.obs.pred1=function(Sizes,Obs,dd,hook.names)
  {
    #Calculated expected population frequency
    N.fish.caught=vector('list',length(dd))
    names(N.fish.caught)=names(dd)
    for(f in 1:length(dd))
    {
      Lns=dd[[f]]$lens
      iid=match(Lns,dd[[f]]$plotlens)
      Pred.sel=dd[[f]]$rselect[iid,]
      Total.sel=rowSums(Pred.sel)
      Total.obs=rowSums(Obs)
      Rel.num.in.pop=sapply(Total.obs/Total.sel,function(x) max(x,0.1))
      Rel.prop.in.pop=Rel.num.in.pop/sum(Rel.num.in.pop)
      N.mesh=colSums(Obs)
      Expnd.N.mesh=matrix(rep(N.mesh,each=nrow(Pred.sel)),ncol=ncol(Pred.sel))
      Expnd.Rel.prop.in.pop=matrix(rep(Rel.prop.in.pop,ncol(Pred.sel)),ncol=ncol(Pred.sel))
      Sum.prod=colSums(Pred.sel*Expnd.Rel.prop.in.pop)
      Sum.prod=matrix(rep(Sum.prod,each=nrow(Pred.sel)),ncol=ncol(Pred.sel))
      N.fish.caught[[f]]=(Expnd.N.mesh*Expnd.Rel.prop.in.pop*Pred.sel)/Sum.prod
    }
    
    #Plot by mesh
    n=ncol(Obs)
    for(i in 1:ncol(Obs))
    {
      plot(Sizes,Obs[,i],type='h',ylab='',xlab='', lwd = 4,col=rgb(.5,.5,.5,alpha=.5))
      for(m in 1:length(N.fish.caught))
      {
        lines(Sizes,N.fish.caught[[m]][,i],col=Cols.type[m],lwd=1.5)
      }
      if(hook.names) mtext(paste(substr(names(Obs[i]),2,5),'/0',sep=''),3,cex=.9)
    }
  }
  
  tiff(file=le.paste("Selectivity_LL/Figure.S4_EZb.tiff"),width = 2400, height = 2200,units = "px", res = 300, compression = "lzw")    
  par(mfrow=c(length(sp),length(unique(d$hooksize))),mar=c(1.5,1.2,.2,.65),oma=c(1.5,2.5,.8,1.2),mgp=c(1,.5,0),las=1,xpd=T)
  for(s in 1:length(sp))
  {
    h=1
    if(s==1) HK.nm=TRUE else HK.nm=FALSE
    fn.freq.obs.pred1(Sizes=Store.tab[[s]][[h]]%>%pull(size.class),
                      Obs=Store.tab[[s]][[h]]%>%dplyr::select(-c(size.class,common_name)),
                      dd=Store.sel[[s]][[h]],
                      hook.names=HK.nm)
    if(s==1)legend("topright",names(Cols.type),bty='n',text.col=Cols.type,cex=1.25)
    mtext(sp[s],4,line=.5,cex=1,las=3)
  }
  mtext("Frequency", side = 2,outer=T, line = 1,las=3,cex=1.2)
  mtext("Total length (cm)",1,outer=T,line=.4,cex=1.2)
  dev.off()
  
  tiff(file=le.paste("Selectivity_LL/Figure.S4_Circular.tiff"),width = 2400, height = 2200,units = "px", res = 300, compression = "lzw")    
  par(mfrow=c(length(sp),length(unique(d$hooksize))),mar=c(1.5,1.2,.2,.65),oma=c(1.5,2.5,.8,1.2),mgp=c(1,.5,0),las=1,xpd=T)
  for(s in 1:length(sp))
  {
    h=2
    if(s==1) HK.nm=TRUE else HK.nm=FALSE
    fn.freq.obs.pred1(Sizes=Store.tab[[s]][[h]]%>%pull(size.class),
                      Obs=Store.tab[[s]][[h]]%>%dplyr::select(-c(size.class,common_name)),
                      dd=Store.sel[[s]][[h]],
                      hook.names=HK.nm)
    if(s==1)legend("topright",names(Cols.type),bty='n',text.col=Cols.type,cex=1.25)
    mtext(sp[s],4,line=.5,cex=1,las=3)
  }
  mtext("Frequency", side = 2,outer=T, line = 1,las=3,cex=1.2)
  mtext("Total length (cm)",1,outer=T,line=.4,cex=1.2)
  dev.off()

  #Predict selectivity by species and hook type
  Pred.sel=vector('list',length(sp))
  names(Pred.sel)=sp
  for(s in 1:length(sp))
  {
    Store=vector('list',length(hk.typ))
    names(Store)=hk.typ
    for(h in 1:length(hk.typ))
    {
      dummy=out.sel(d=Store.sel[[s]][[h]],
                    BEST=Best.fit[[s]][[h]],
                    Fixed.equal.power=TRUE)
      
      dummy=dummy%>%
        gather(Hook.size,Selectivity,-TL.cm)%>%
        mutate(Hooktype=hk.typ[h],
               Species=sp[s])
      
      Store[[h]]=dummy
      rm(dummy)
    }
    Pred.sel[[s]]=do.call(rbind,Store)
  }
  Pred.sel=do.call(rbind,Pred.sel)
  
  #Display selectivity
  p.list=vector('list',length(hk.typ))
  for(h in 1:length(hk.typ))
  {
    p.list[[h]]=Pred.sel%>%
      filter(Hooktype==hk.typ[h])%>%
      mutate(Hook.size=as.factor(paste(Hook.size,'/0',sep='')))%>%
      ggplot(aes(x=TL.cm,y=Selectivity,col=Hook.size))+
      geom_line(size=1.15)+
      facet_wrap(~Species,nrow=1)+
      labs(x="",y="",subtitle=hk.typ[h])+
      theme_PA(Sbt.siz=22,strx.siz=20,leg.siz=20,axs.t.siz=16,axs.T.siz=20)+
      theme(legend.position = "top",
            legend.title = element_blank())
  }
  fig=ggarrange(plotlist=p.list,
                nrow = 2,
                common.legend=TRUE)
  annotate_figure(fig,
                  bottom = text_grob("Total length (cm)",size = 20),
                  left = text_grob("Relative selectivity",size = 20,rot = 90))
  ggsave(le.paste("Selectivity_LL/Relative selectivity.tiff"),width = 12,height = 8,compression = "lzw")
}

fn.LL.sel(d=DATA%>%filter(method=='LL' & !is.na(Size)),
          Min.sample=30,  
          Min.hooks=2,
          Size.Interval=5,
          convrt.this=c("Dusky shark","Gummy shark","Port Jackson"),
          Fitfunction='gillnetfit',
          Rtype=c("norm.loc","norm.sca","gamma","lognorm"),
          Min.length=20,
          Max.length=300) 

#---------General tables and plots of PA underwater Video ------------ 
SP.group.levels=c("Invertebrates","Scalefish","Sharks","Rays","Marine mammals","Seabirds")
TEP.groups=c("Marine mammals","Seabirds","Reptiles")

#Mean soak time by gear
DATA%>%
  distinct(sheet_no,.keep_all=T)%>%
  group_by(method)%>%
  summarise(Mean=mean(soak.time,na.rm=T),
            MIN=min(soak.time,na.rm=T),
            MAX=max(soak.time,na.rm=T))%>%
  arrange(method)

  #1. number of events 
do.events=FALSE
if(do.events)
{
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
  ggsave(le.paste("Video/underwater/Interactions_number.events_sqrt.transf_by.group.tiff"),
         width = 12,height = 10,compression = "lzw")
  
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
  
}

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
  theme_PA(strx.siz=17,leg.siz=18,axs.t.siz=14,axs.T.siz=16)+
  theme(legend.position = "top",
        legend.title = element_blank(),
        plot.margin = margin(.3, 1, .3, .3, "cm"))+
  xlab('')+ylab('Number of individuals')+
  guides(color = guide_legend(nrow = 1))
ggsave(le.paste("Video/underwater/Interactions_number.individuals_sqrt.transf_by.group.tiff"),
       width = 12,height = 10,compression = "lzw")

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
  theme_PA(strx.siz=17,leg.siz=18,axs.t.siz=14,axs.T.siz=16)+
  theme(legend.position = "top",
        legend.title = element_blank(),
        plot.margin = margin(.3, 1, .3, .3, "cm"))+
  xlab('')+ylab('Number of individuals')+
  guides(color = guide_legend(nrow = 1))
ggsave(le.paste("Video/underwater/Interactions_number.individuals_sqrt.transf.tiff"),width = 12,height = 8,compression = "lzw")

  #2.3. Main target species only
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
  mutate(Method=capitalize(Method))%>%
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
  theme_PA(strx.siz=17,leg.siz=18,axs.t.siz=14,axs.T.siz=16)+
  theme(legend.position = "top",
        legend.title = element_blank())+
    xlab('')+ylab('Number of individuals')+ guides(color = guide_legend(nrow = 1))+
  scale_fill_manual(values=dis.cols)
ggsave(le.paste("Video/underwater/Interactions_number.individuals_main.target.tiff"),width = 12,
       height = 10,compression = "lzw")


  #3. Export data for Abbey
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


  #4. Lolipop approach
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


  #5. Test differences in interactions by method and species group from underwater camera
fn.mds=function(tav,COM,SUBT)
{
  iid=match(c('Method','SP.group','SPECIES'),names(tav))
  drop.col=colSums(tav[,-iid])
  drop.col=names(drop.col[drop.col==0])
  COM=COM[,-match(drop.col,names(COM))]
  MDS <- metaMDS(comm = COM, distance = "bray",k=2,trymax=100, trace = FALSE, autotransform = FALSE)
  MDS_xy <- data.frame(MDS$points)%>%
    mutate(SP.group= tav$SP.group)
  p=ggplot(MDS_xy, aes(MDS1, MDS2)) +
    geom_point(aes(shape=SP.group, color = SP.group),size=3) +
    theme_bw() +
    annotate(geom="text", x=0.85*max(MDS_xy$MDS1), y=min(MDS_xy$MDS2), 
             label=paste("Stress=",round(MDS$stress,3)))+
    theme_PA(Sbt.siz=18)+
    theme(legend.position = "top",
          legend.title = element_blank())+
    guides(colour = guide_legend(nrow = 1))+
    labs(subtitle =SUBT, y='', x='') 
  return(p)
}
simper.fn=function(COM,tav,SUBT,Sig.pair)
{
  SIMPER <- summary(simper(COM, tav%>%pull(SP.group),parallel=7))
  Get=as.data.frame(str_split(names(SIMPER), "_", simplify = TRUE))%>%
    mutate(id=1:n())
  SIMPER=SIMPER[Get$id]
  
  disp.simp=vector('list',length(SIMPER))
  for(n in 1:length(disp.simp))
  {
    disp.simp[[n]]=cbind(tav['SP.group'],
                         COM[row.names(SIMPER[[n]]%>%filter(cumsum<=simper.cumsum))])%>%
      filter(SP.group%in%str_split(names(SIMPER)[n], "_", simplify = TRUE))%>%
      gather(species,prop,-SP.group,)%>%
      mutate(groups=names(SIMPER)[n])
  }
  disp.simp=do.call(rbind,disp.simp)
  p=disp.simp%>%
    filter(groups%in%str_remove(Sig.pair,"vs_"))%>%
    group_by(SP.group,groups,species)%>%
    summarise(prop=mean(prop))%>%
    ggplot(aes(x=SP.group,y=prop, fill=species))+
    geom_bar(stat="identity", width = 0.5)+
    ylab("Average proportion")+xlab("Group")+
    theme_PA(Ttl.siz=18,Sbt.siz=18,strx.siz=14,
             leg.siz=13,axs.t.siz=16,axs.T.siz=18)+
    theme(legend.position = "top",
          legend.title = element_blank(),
          plot.margin=unit(c(.1,.5,.1,.1),"cm"))+
    guides(fill = guide_legend(nrow = 2))+
    labs(subtitle =SUBT, y='', x='') 
  return(p)
}
fn.test.underwtr=function(what,trans)
{
  d=rbind(Video.longline.interaction%>%dplyr::select(Method,Interaction,Number,SP.group,Genus,Species,Code),
          Video.net.interaction%>%dplyr::select(Method,Interaction,Number,SP.group,Genus,Species,Code))%>%
    filter(!Species=='birds feeding at surface')%>%
    filter(!SP.group%in%TEP.groups)%>%
    filter(!is.na(Interaction))%>%  
    mutate(Method=capitalize(Method),
           Interaction=capitalize(tolower(Interaction)),
           SP.group=factor(SP.group,levels=SP.group.levels))%>%
    left_join(All.species.names%>%
                dplyr::select(COMMON_NAME,Code)%>%
                distinct(Code,.keep_all=T),
              by="Code")%>%
    mutate(SPECIES=paste(Genus,Species))
  if(what=='Events') d=d%>%mutate(Number=1)
  
  #Table of all behaviours by species
  TAB=d%>%
    group_by(Method,SP.group,Interaction,COMMON_NAME,SPECIES)%>%
    summarise(N=sum(Number))%>%
    spread(Interaction,N,fill='')%>%
    arrange(Method,SP.group,COMMON_NAME)
  write.csv(TAB,le.paste("Video/underwater/Table_number.interactions.by.species.csv"),row.names=F)
  
  
  #Multivariate - differences in behaviours between method and species group
  TAB=d%>%
    group_by(Method,SP.group,Interaction,SPECIES)%>%
    summarise(N=sum(Number))%>%
    spread(Interaction,N,fill=0)%>%
    arrange(Method,SP.group)
  id=match(c('Method','SP.group','SPECIES'),names(TAB))
  
  TAB.GN=TAB%>%filter(Method=="Gillnet")
  TAB.LL=TAB%>%filter(Method=="Longline")
  
  Community.GN <<- TAB.GN[-id]
  if(trans=='proportion') Community.GN=Community.GN/rowSums(Community.GN)
  if(trans=='squareroot') Community.GN=Community.GN^(1/2)
  if(trans=='4throot')    Community.GN=Community.GN^(1/4)
  
  Community.LL <<- TAB.LL[-id]
  if(trans=='proportion') Community.LL=Community.LL/rowSums(Community.LL)
  if(trans=='squareroot') Community.LL=Community.LL^(1/2)
  if(trans=='4throot')    Community.LL=Community.LL^(1/4)
  
  #MDS
  p.mdsGN=fn.mds(tav=TAB.GN,COM=Community.GN,SUBT="Gillnet")
  p.mdsLL=fn.mds(tav=TAB.LL,COM=Community.LL,SUBT="Longline")
  
  #Permanova
    # 1. overall significance test
  adon.results.GN<-adonis(formula('Community.GN~SP.group'),data=TAB.GN, method="bray",perm=5e3)
  adon.results.LL<-adonis(formula('Community.LL~SP.group'),data=TAB.LL, method="bray",perm=5e3)
  
    # 2. multilevel pairwise comparison with adjusted p-values
  dummy=pairwise.adonis2(Community.GN~SP.group,data=TAB.GN)
  adonis.pairwise=vector('list',(length(dummy)-1))
  for(qq in 2:length(dummy))
  {
    adonis.pairwise[[qq]]=data.frame(Pairs=names(dummy)[[qq]],
                                     P=dummy[[qq]]$`Pr(>F)`[1])
  }
  adonis.pairwise.GN=do.call(rbind,adonis.pairwise)
  
  dummy=pairwise.adonis2(Community.LL~SP.group,data=TAB.LL)
  adonis.pairwise=vector('list',(length(dummy)-1))
  for(qq in 2:length(dummy))
  {
    adonis.pairwise[[qq]]=data.frame(Pairs=names(dummy)[[qq]],
                                     P=dummy[[qq]]$`Pr(>F)`[1])
  }
  adonis.pairwise.LL=do.call(rbind,adonis.pairwise)

 
  #Simper analysis to identify interactions that discriminate among groups
  simper.GN=simper.fn(COM=Community.GN,
                      tav=TAB.GN,
                      SUBT="Gillnet",
                      Sig.pair=adonis.pairwise.GN%>%filter(P<0.05)%>%pull(Pairs))
  simper.LL=NULL
  if(nrow(adonis.pairwise.LL%>%filter(P<0.05))>0)
  {
    simper.LL=simper.fn(COM=Community.LL,
                        tav=TAB.LL,
                        SUBT="Longline",
                        Sig.pair=adonis.pairwise.LL%>%filter(P<0.05)%>%pull(Pairs))
  }
  
  return(list(p.mdsGN=p.mdsGN,
              p.mdsLL=p.mdsLL,
              Permanova.GN=as.data.frame(adon.results.GN$aov.tab),
              Permanova.LL=as.data.frame(adon.results.LL$aov.tab),
              adonis.pairwise.GN=adonis.pairwise.GN,
              adonis.pairwise.LL=adonis.pairwise.LL,
              simper.GN=simper.GN,
              simper.LL=simper.LL))
}
simper.cumsum=.95
out.under.n=fn.test.underwtr(what='Numbers',trans='squareroot')
rm(Community.GN,Community.LL)
  #export stuff
ggarrange(plotlist=list(out.under.n$p.mdsGN+rremove("xlab")+rremove("ylab"),
                        out.under.n$p.mdsLL+rremove("xlab")+rremove("ylab")),
              ncol = 1,
              common.legend=TRUE)
ggsave(le.paste("Video/underwater/interactions_MDS.tiff"),width = 5,
       height = 8,compression = "lzw")

write.csv(out.under.n$Permanova.GN,le.paste("Video/underwater/interactions_Permanova.GN.csv"))
write.csv(out.under.n$Permanova.LL,le.paste("Video/underwater/interactions_Permanova.LL.csv"))
write.csv(out.under.n$adonis.pairwise.GN,le.paste("Video/underwater/interactions_Permanova_pairwise.GN.csv"),row.names = F)
write.csv(out.under.n$adonis.pairwise.LL,le.paste("Video/underwater/interactions_Permanova_pairwise.LL.csv"),row.names = F)

plist=list(out.under.n$simper.GN,out.under.n$simper.LL)%>%
        discard(is.null)
fig=ggarrange(plotlist=plist,ncol = 1,common.legend=TRUE)
annotate_figure(fig,
                bottom = text_grob("Group",size = 20),
               left = text_grob("Average proportion",size = 20,rot = 90))

ggsave(le.paste("Video/underwater/interactions_simper.tiff"),width = 6,
       height = 8,compression = "lzw")



#fn.test.underwtr(what='Events',trans='squareroot')



#---------Analyse PA Habitat ------------

# 1. Underwater habitat classification
  #gillnets
Video.habitat=Video.habitat%>%
                data.frame%>%
                filter(!BROAD=="Open water")%>%  #remove cameras facing up
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
  #longline
Video.habitat.LL=Video.habitat.LL%>%
                data.frame%>%
                filter(!BROAD=="Open water")%>%  #remove cameras facing up
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

#Overall Broad categories
Hab.cols=c('lightskyblue','red4','seagreen',
           'orange3','tomato4','khaki')
names(Hab.cols)=c('Hydroids','Macroalgae','Seagrasses',
                  'Sponges','Stony corals','Substrate')
coul=c('brown','green3',"firebrick2")
fn.habitat.damg.sub=function(d,TITL)
{
  p1=d%>%
    group_by(BROAD)%>%
    tally()%>%
    mutate(n=100*n/sum(n))%>%
    ggplot(aes(x=n,y=reorder(BROAD,n),fill=BROAD))+
    geom_bar(stat = "identity",colour="black")+
    xlim(0,100)+
    theme_PA(Ttl.siz=18,str.siz=20,strx.siz=20,
             leg.siz=16,axs.t.siz=16,axs.T.siz=18)+
    theme(legend.position = "none",
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5))+
    scale_fill_manual(values=Hab.cols)+
    labs(x="Percentage",y="",title=TITL,subtitle="Habitat type")
  
  
  #Substrate categories
  colfunc <- colorRampPalette(c('khaki4', "khaki"))
  p2=d%>%
    filter(BROAD=='Substrate')%>%
    group_by(MORPHOLOGY)%>%
    tally()%>%
    mutate(n=100*n/sum(n))
  p2=p2%>%
    ggplot(aes(x=n,y=reorder(MORPHOLOGY,n),fill=MORPHOLOGY))+
    geom_bar(stat = "identity",colour="black")+
    xlim(0,100)+
    theme_PA(Ttl.siz=18,str.siz=20,strx.siz=20,
             leg.siz=16,axs.t.siz=16,axs.T.siz=18)+
    theme(legend.position = "none",
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5))+
    scale_fill_manual(values=colfunc(nrow(p2)))+
    labs(x="Percentage",y="",subtitle="Substrate type")
  
  
  #Algae categories
  n=d%>%
    filter(BROAD=='Macroalgae' & !is.na(TYPE))%>%
    group_by(MORPHOLOGY)%>%
    tally()%>%
    rename(N=n)
  p3=d%>%
    filter(BROAD=='Macroalgae' & !is.na(TYPE))%>%
    group_by(MORPHOLOGY,TYPE)%>%
    tally()%>%
    ungroup()%>%
    mutate(n=100*n/sum(n))%>%
    left_join(n,by="MORPHOLOGY")%>%
    ggplot(aes(x=n,y=reorder(MORPHOLOGY,N),fill=TYPE))+
    geom_bar(stat = "identity",colour="black")+
    xlim(0,100)+
    theme_PA(Ttl.siz=18,str.siz=20,strx.siz=20,
             leg.siz=16,axs.t.siz=16,axs.T.siz=18)+
    theme(legend.position = "none",
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5))+
    scale_fill_manual(values=coul)+
    labs(x="Percentage",y="",subtitle="Macroalgae type")
  
  return(list(p1=p1,p2=p2,p3=p3))
}
Out.damg_under_GN=fn.habitat.damg.sub(d=Video.habitat%>%
                                          filter(!BROAD=='Open water'),  #remove 'Open water' which means camera was pointing upwards
                                      TITL="Gillnet")
Out.damg_under_LL=fn.habitat.damg.sub(d=Video.habitat.LL%>%
                                        filter(!BROAD=='Open water'),
                                      TITL="Longline")
ggarrange(Out.damg_under_GN$p1, Out.damg_under_LL$p1,
          Out.damg_under_GN$p2, Out.damg_under_LL$p2,
          Out.damg_under_GN$p3, Out.damg_under_LL$p3,
          ncol = 2, nrow = 3)
ggsave(le.paste("Video/underwater/Habitats_coarse.records.tiff"),width = 10,height = 10,compression = "lzw")


# 2. Underwater observations of habitat damage
  #gillnets  
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
ggplot() + 
  annotate("text", x = 4, y = 25, size=7, label = text) + 
  ggtitle(label ="Underwater cameras")+
  theme_void()+
  theme(plot.title = element_text(size = 20, face = "bold"))
ggsave(le.paste("Video/underwater/Habitat.damage_GN.tiff"),width = 6,height = 10,compression = "lzw")

  #longlines
Longline.damage.underwater=Video.longline.interaction%>%
  mutate(Habitat.damage=ifelse(
    SP.group%in%c("Macro algae","Rock/reef structure","Sponges"),"Damage",
    "No damage"))%>%
  dplyr::select(sheet_no,Camera,Habitat.damage)
Damage.events=Longline.damage.underwater%>%
  group_by(Habitat.damage)%>%tally()
if(!"Damage"%in%Damage.events$Habitat.damage)
{
  Damage.events=rbind(Damage.events,data.frame(Habitat.damage="Damage",n=0))
}
Total.observed.metres=length(unique(Video.longline.interaction$OpCode))*metres.observed
text = paste(Damage.events%>%filter(Habitat.damage=='Damage')%>%pull(n),
             "habitat damage events in\n",
             Total.observed.metres,
             "metres of longline observed with \n",
             "  underwater cameras")
ggplot() + 
  annotate("text", x = 4, y = 25, size=7, label = text) + 
  ggtitle(label ="Underwater cameras")+
  theme_void()+
  theme(plot.title = element_text(size = 20, face = "bold"))
ggsave(le.paste("Video/underwater/Habitat.damage_LL.tiff"),width = 6,height = 10,compression = "lzw")


# 3. Deck camera Habitat interactions (gillnets only)
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

fn.habitat.damg.deck=function(Gear)
{
  d=Video.habitat.deck%>%
    filter(Period==Gear)%>%
    group_by(Frame,SHEET_NO,net_length,Species)%>%
    summarise(Percentage.cover=sum(Percentage.cover))%>%
    group_by(Frame,SHEET_NO,net_length)%>%
    mutate(Total.damage=sum(Percentage.cover))%>%
    spread(Species,Percentage.cover,fill = 0)%>%
    mutate(Dist.roller.spreader=distance.roller.spreader.Anthony)%>%
    data.frame
  
  #add 0 habitat damage frames
  habitat.sheet=unique(d$SHEET_NO)
  dummy=vector('list',length(habitat.sheet))
  for(l in 1:length(habitat.sheet))
  {
    a=subset(d,SHEET_NO==habitat.sheet[l])
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
  d.no.zeros=d
  d=do.call(rbind,dummy)            
  
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
  pdf(le.paste(paste("Video/deck.cameras/Habitat.damage_explore_",Gear,".pdf",sep='')))
  fn.hab.explr(d.no_zeros=d.no.zeros,d=d)
  dev.off()
  
  
  #Pie chart of frame with damage / no damage
  p2=d%>%
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
            subtitle = "Percentage of frames (~4.5 m of net) with damage")+
    theme_void() +
    theme(legend.position = "right",
          legend.title = element_blank(),
          legend.text = element_text(size = 16),
          plot.title = element_text(size = 20, face = "bold",hjust=-920),
          plot.subtitle = element_text(size = 17,face = "italic",hjust=0.3))
  
  #Pie chart of different habitat damage categories
  p3=d.no.zeros%>%
    dplyr::select(-Frame,-Total.damage,-SHEET_NO,-net_length,-Dist.roller.spreader)%>%
    gather(Species,Percent)%>%
    group_by(Species)%>%
    summarise(Percent=sum(Percent))%>%
    mutate(label=paste(round(100*Percent/sum(Percent)),'%',sep=''),
           ymax=cumsum(Percent),
           ymin = c(0, head(ymax, n=-1)),
           labelPosition =(ymax + ymin) / 2)%>%
    ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Species)) +
    geom_rect() +
    geom_text(x=3.5,aes(y=labelPosition, label=label,angle = 0),size = 6,check_overlap = TRUE)+
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
  p4=d.no.zeros%>%
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
  
  return(list(p2=p2, p3=p3, p4=p4))
}
Out.damg_GN=fn.habitat.damg.deck(Gear='gillnet')
ggarrange(Out.damg_GN$p2, Out.damg_GN$p3, Out.damg_GN$p4, ncol = 1, nrow = 3)
ggsave(le.paste("Video/deck.cameras/Habitat.interactions.tiff"),width = 6,height = 10,compression = "lzw")


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


Video.subsurface=Video.subsurface%>%  
  data.frame%>%
  mutate(Period=tolower(Period),
         Drop.out=tolower(Drop.out),
         Dropout.condition=tolower(Dropout.condition),
         dropout=capitalize(Drop.out),
         SHEET_NO=case_when(Period=='gillnet'~str_remove(word(DPIRD.code,1,sep = "\\/"),'GN'),
                            Period=='longline'~str_remove(word(DPIRD.code,2,sep = "\\/"),'LL')))
Video.subsurface.comments=Video.subsurface.comments%>%  
  data.frame%>%
  mutate(Period=tolower(Period),
         SHEET_NO=case_when(Period=='gillnet'~str_remove(word(DPIRD.code,1,sep = "\\/"),'GN'),
                            Period=='longline'~str_remove(word(DPIRD.code,2,sep = "\\/"),'LL')))


  #1. Dropouts

    #1.1 Barplot for deck camera 2 and subsurface camera
fn.plt.dropouts=function(d,TITLE,LegPos,var,MinN)
{
  these.drop.out.sp=table(d$Code)
  these.drop.out.sp=as.numeric(names(these.drop.out.sp[these.drop.out.sp>MinN]))
  names(these.drop.out.sp)=All.species.names[match(these.drop.out.sp,All.species.names$Code),"COMMON_NAME"]
  
  Col.vec=these.drop.out.sp
  Col.vec=ifelse(Col.vec<37049001,"firebrick","steelblue")
  
  d=d%>%
    filter(Code%in%these.drop.out.sp)%>%
    group_by(Code,!!sym(var),Period)%>%
    tally()%>%
    mutate(dummy=factor(!!sym(var)),
           Period=capitalize(Period))%>%
    left_join(All.species.names%>%dplyr::select(COMMON_NAME,Code),by="Code")%>%
    mutate(COMMON_NAME=factor(COMMON_NAME,levels=names(sort(these.drop.out.sp))))
  
  vals=c("#F8766D","#00BFC4","#7CAE00")
  
  p=d%>%
    ggplot(aes(fill=dummy, y=n, x=COMMON_NAME)) + 
    geom_bar(position="stack", stat="identity")+
    coord_flip() +
    facet_wrap(~Period,dir='h',scales='free_x')+ 
    theme_PA(Ttl.siz=18,Sbt.siz=16,str.siz=18,strx.siz=18,
             cap.siz=10,lgT.siz=14,leg.siz=16,axs.t.siz=14,axs.T.siz=16)+
    theme(legend.position = LegPos,
          legend.title = element_blank(),
          axis.text.y = element_text(colour = Col.vec),
          plot.title = element_text(hjust = -.4))+
    xlab('')+ylab('Number of events')+
    scale_fill_manual(values=vals[1:length(unique(d$dummy))])
  
  if(!is.null(TITLE))p=p+ggtitle(TITLE)
  
  print(p)
}

p1=fn.plt.dropouts(d=Video.camera2.deck,
                   TITLE="Deck camera above roller",
                   LegPos="top",
                   var='dropout',
                   MinN=Min.N.drop.out)
p2=fn.plt.dropouts(d=Video.subsurface%>%
                     mutate(dropout=ifelse(dropout=='Gaffed','Yes',dropout)),
                   TITLE="Subsurface camera",
                   LegPos="none",
                   var='dropout',
                   MinN=Min.N.drop.out)
ggarrange(p1, p2, ncol = 1, nrow = 2,heights=c(1.5,1))
ggsave(le.paste("Video/deck.cameras/Drop.out.events.tiff"),width = 10,height = 10,compression = "lzw")


  #1.2 glm for drop out probability by method
fn.glm.dropouts=function(d,var,MinN)
{
  these.drop.out.sp=table(d$Code)
  these.drop.out.sp=as.numeric(names(these.drop.out.sp[these.drop.out.sp>MinN]))
  names(these.drop.out.sp)=All.species.names[match(these.drop.out.sp,All.species.names$Code),"COMMON_NAME"]
  
  Col.vec=these.drop.out.sp
  Col.vec=ifelse(Col.vec<37049001,"firebrick","steelblue")
  
  d=d%>%
    filter(Code%in%these.drop.out.sp)%>%
    group_by(Code,!!sym(var),Period)%>%
    tally()%>%
    mutate(dummy=!!sym(var),
           Period=capitalize(Period))%>%
    left_join(All.species.names%>%dplyr::select(COMMON_NAME,Code),by="Code")%>%
    ungroup()%>%
    dplyr::select(Period,n,dummy,COMMON_NAME)%>%
    spread(dummy,n,fill=0)
  
  d.GN=d%>%filter(Period=="Gillnet")%>%
    mutate(COMMON_NAME=factor(COMMON_NAME))
  GN.glm <- glm( cbind(No, Yes) ~ COMMON_NAME, data = d.GN, family = "binomial")
  
  d.LL=d%>%filter(Period=="Longline")%>%
    mutate(COMMON_NAME=factor(COMMON_NAME))
  LL.glm <- glm( cbind(No, Yes) ~ COMMON_NAME, data = d.LL, family = "binomial")
  
  
  #Predict species
  GN.pred=summary(emmeans(GN.glm, 'COMMON_NAME', type="response"))
  LL.pred=summary(emmeans(LL.glm, 'COMMON_NAME', type="response"))
  
  return(list(GN.glm=GN.glm,LL.glm=LL.glm,GN.pred=GN.pred,LL.pred=LL.pred,
              Col.vec=data.frame(COMMON_NAME=names(Col.vec),Col=Col.vec)))
}
Glm.dropouts=fn.glm.dropouts(d=Video.camera2.deck,
                var='dropout',
                MinN=Min.N.drop.out)

ft <- as_flextable(Glm.dropouts$GN.glm)
save_as_image(ft, path = le.paste("Video/deck.cameras/anovas/drop_outs_GN.png"))
save_as_docx(ft,path = le.paste("Video/deck.cameras/anovas/drop_outs_GN.docx"))

ft <- as_flextable(Glm.dropouts$LL.glm)
save_as_image(ft, path = le.paste("Video/deck.cameras/anovas/drop_outs_LL.png"))
save_as_docx(ft,path = le.paste("Video/deck.cameras/anovas/drop_outs_LL.docx"))

rm(ft)

p=fn.barplot.cpue(d=rbind(Glm.dropouts$GN.pred%>%
                            mutate(method="Gillnet"),
                          Glm.dropouts$LL.pred%>%
                            mutate(method="Longline"))%>%
                    left_join(Glm.dropouts$Col.vec,by='COMMON_NAME')%>%
                    mutate(Group=Col,
                           x=COMMON_NAME,
                           y=1-prob,
                           fill=Group,
                           facet=method,
                           lower.CL=1-asymp.LCL,
                           upper.CL=1-asymp.UCL),
                  YLAB="Drop out probability",
                  XLAB='',
                  cex=12,
                  Rotate='Yes',
                  Relative="No")
fn.fig(le.paste("Video/deck.cameras/Drop.out.events_predictions_deckcam2"),2000,2000)
p+scale_fill_manual(values=c("firebrick","steelblue"))+
  theme(axis.text.x = element_text(angle = 55, vjust = 1, hjust=1))
dev.off()


  #1.3.drop out rate table (drop out numbers per total number of individuals interacting with gear)
TAB=Video.camera2.deck%>%
  left_join(All.species.names%>%dplyr::select(COMMON_NAME,Code),by="Code")%>%
  group_by(COMMON_NAME,dropout,Period)%>%
  tally()%>%
  ungroup()%>%
  mutate(Period=capitalize(Period),
         dropout=ifelse(dropout=='No','Retained','Drop.out'),
         drop.period=paste(dropout,Period,sep='.'))%>%
  dplyr::select(-c(dropout,Period))%>%
  spread(drop.period,n,fill=0)%>%
  mutate(Percent.drop.out.gillnet=round(100*Drop.out.Gillnet/(Retained.Gillnet+Drop.out.Gillnet),1),
         Percent.drop.out.longline=round(100*Drop.out.Longline/(Retained.Longline+Drop.out.Longline),1))%>%
  arrange(COMMON_NAME)%>%
  relocate(COMMON_NAME,Retained.Gillnet,Drop.out.Gillnet,Percent.drop.out.gillnet,
           Retained.Longline,Drop.out.Longline,Percent.drop.out.longline)%>%
  mutate_at(vars(Percent.drop.out.longline), ~replace(., is.nan(.), ""))%>%
  mutate_at(vars(Percent.drop.out.gillnet), ~replace(., is.nan(.), ""))%>%
  mutate(Drop.out.Gillnet=ifelse(Percent.drop.out.gillnet=='','',Drop.out.Gillnet),
         Retained.Gillnet=ifelse(Percent.drop.out.gillnet=='','',Retained.Gillnet),
         Drop.out.Longline=ifelse(Percent.drop.out.longline=='','',Drop.out.Longline),
         Retained.Longline=ifelse(Percent.drop.out.longline=='','',Retained.Longline))
write.csv(TAB,le.paste("Video/deck.cameras/Drop.out.events_table.csv"),row.names = F)

  #1.4 Drop outs fate
p=fn.plt.dropouts(d=Video.subsurface%>%filter(!dropout=='No'),
                TITLE="Subsurface camera",
                LegPos="top",
                var='Dropout.condition',
                MinN=Min.N.drop.out)
p+theme(plot.title=element_text(hjust = -.25))
ggsave(le.paste("Video/deck.cameras/Drop.out.events_fate_subsurface.tiff"),width = 10,height = 8,compression = "lzw")

  #1.5 Drop outs gaffing
p=fn.plt.dropouts(d=Video.subsurface%>%filter(!dropout=='No')%>%
                  mutate(Gaffed=ifelse(dropout=="Yes","Not gaffed",dropout),
                         Gaffed=factor(Gaffed,levels=c("Gaffed","Not gaffed"))),
                TITLE="Subsurface camera",
                LegPos="top",
                var='Gaffed',
                MinN=Min.N.drop.out)
p+theme(plot.title=element_text(hjust = -.25))
ggsave(le.paste("Video/deck.cameras/Drop.out.events_gaffing_subsurface.tiff"),width = 10,height = 8,compression = "lzw")


  #2. Gaffing
p=fn.plt.dropouts(d=Video.camera2.deck%>%
                  mutate(gaffed=case_when(dropout=='Yes' & gaffed=='No' ~  'Lost',
                                          dropout=='Yes' & gaffed=='Yes' ~  'Gaffed',
                                          TRUE ~ gaffed))%>%
                  filter(!is.na(gaffed) & dropout=='Yes'),
                TITLE="Deck camera above roller",
                LegPos="top",
                var='gaffed',
                MinN=Min.N.gaff)
p+theme(plot.title=element_text(hjust = -.25))
ggsave(le.paste("Video/deck.cameras/Gaffing.events_deck.tiff"),width = 10,height = 8,compression = "lzw")


  #3. Composition around weight or float
# Display data
Data_water.column=Video.camera2.deck%>%
            filter(!SHEET_NO%in%No.good.water.column)%>%
            mutate(water.column=tolower(hook.distance.to.float.weight))%>%
            filter(!is.na(water.column))
these.compo.sp=Data_water.column%>%
  group_by(Code)%>%
  tally()%>%
  filter(n>=Min.obs.comp.wei.flot)%>%
  pull(Code)
names(these.compo.sp)=All.species.names[match(these.compo.sp,All.species.names$Code),"COMMON_NAME"]
Col.vec=these.compo.sp
Col.vec=ifelse(Col.vec<37049001,"firebrick","steelblue")
Data_water.column=Data_water.column%>%
  filter(Code%in%these.compo.sp)%>%
  left_join(All.species.names%>%dplyr::select(COMMON_NAME,Code),by="Code")%>%
  mutate(COMMON_NAME=factor(COMMON_NAME,levels=names(these.compo.sp)))
Data_water.column%>%
        group_by(Code,water.column,Period,COMMON_NAME)%>%
        tally()%>%
        mutate(water.column=factor(water.column,levels=c("1w","2w","3w","1f","2f","3f")),
                   Period=capitalize(Period))%>%
        ggplot(aes(fill=water.column, y=n, x=COMMON_NAME)) + 
        geom_bar(position="stack", stat="identity")+
        coord_flip() +
        facet_wrap(~Period,dir='h',scales='free_x')+ 
        theme_PA(str.siz=20,strx.siz=20,
                 leg.siz=18,axs.t.siz=16,axs.T.siz=18)+
        theme(legend.position = "top",
              legend.title = element_blank())+
        scale_fill_manual(values=c("orange","firebrick2","firebrick4","lightblue2","deepskyblue2","dodgerblue4"))+
        xlab('')+ylab('Number of events')+ 
        guides(fill = guide_legend(nrow = 1, byrow = TRUE))
ggsave(le.paste("Video/deck.cameras/Weight_float_species.events.tiff"),width = 10,height = 8,compression = "lzw")

  #fit binomial glm
Data_water.column=Data_water.column%>%
  left_join(DATA%>%
              filter(sheet_no%in%Data_water.column$SHEET_NO)%>%
              distinct(sheet_no,botdepth),
            by=c("SHEET_NO"="sheet_no"))%>%
  mutate(Position=ifelse(grepl('w',water.column),"weight","float"))

glm.dat=Data_water.column%>% 
            mutate(N=1)%>%
            dplyr::select(COMMON_NAME,N,Position,botdepth)%>%
            group_by(COMMON_NAME,Position)%>%
            summarise(N=sum(N))%>%
            spread(Position,N,fill=0)%>%
            mutate(COMMON_NAME=factor(as.character(COMMON_NAME)))
Mod <- glm( cbind(float, weight) ~ COMMON_NAME, data = glm.dat, family = "binomial")  

  #Predict species
Pred=summary(emmeans(Mod, 'COMMON_NAME', type="response"))
p=fn.barplot.cpue(d=Pred%>%
                    left_join(data.frame(COMMON_NAME=names(Col.vec),Col=Col.vec),
                              by='COMMON_NAME')%>%
                    mutate(Group=Col,
                           x=COMMON_NAME,
                           y=1-prob,
                           fill=Group,
                           facet="Longline",
                           lower.CL=1-asymp.LCL,
                           upper.CL=1-asymp.UCL),
                  YLAB="Probability of capture near a weight",
                  XLAB='',
                  cex=18,
                  Rotate='Yes',
                  Relative="No")
fn.fig(le.paste("Video/deck.cameras/Weight_float_species_predictions"),2400,2400)
p+scale_fill_manual(values=c("firebrick","steelblue"))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
dev.off()

  #Export anova table
ft <- as_flextable(Mod)
save_as_image(ft, path = le.paste("Video/deck.cameras/anovas/Weight_float_species.png"))
save_as_docx(ft,path = le.paste("Video/deck.cameras/anovas/Weight_float_species.docx"))



#---------Analysis of Deck 1 camera VS observers --------------------------------------------------------------------

# Deck 1  (pointing to deck)                      
Video.camera1.deck=Video.camera1.deck%>%
  mutate(Code=gsub('\\s+', '',Code),
         Code=as.numeric(Code))%>%
  left_join(All.species.names%>%
              dplyr::select(COMMON_NAME,Code)%>%
              distinct(Code,.keep_all=T),
            by="Code")

Video.camera1=rbind(Video.camera1.deck_extra.records%>%
                      dplyr::select("DIPRD code",Code,Period,number),
                    Video.camera1.deck%>%
                      mutate(Code=ifelse(Genus=="Kyphosus" & Species=="spp",'37361903',Code))%>%
                      dplyr::select("DIPRD code",Code,Period,number))%>%
              data.frame%>%
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
                     Period=ifelse(Period=='gn','gillnet',
                                   ifelse(Period=='ll','longline',Period)),
                     sheet_no=case_when(Period=='gillnet'~str_remove(word(DIPRD.code,1,sep = "\\/"),'GN'),
                                        Period=='longline'~str_remove(word(DIPRD.code,2,sep = "\\/"),'LL')))%>%
              left_join(DATA_PA,by='sheet_no')%>%
              mutate(Data.set="camera")

  #1. Export data for Abbey
Abbey.data.chapter.2=rbind(Video.camera1%>%
                             dplyr::rename(Number=number)%>%
                             mutate(Method=capitalize(Period))%>%
                             filter(Code>=3.7e7 & Code<=3.747e7)%>%  #compare only fish and sharks
                             dplyr::select(sheet_no,Method,Data.set,Code,SP.group,Number),
                           DATA%>%
                             filter(sheet_no%in%unique(Video.camera1$sheet_no))%>%
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
                             dplyr::select(sheet_no,Method,Data.set,Code,SP.group,Number))%>%
                      mutate(Code=as.numeric(Code))

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
            dplyr::rename(Fishing.effort=Effort)%>%
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

#2 Barplot of observer VS camera
TAB=DATA%>%
  filter(!is.na(COMMON_NAME) & !COMMON_NAME=='')%>%
  group_by(retainedflag,COMMON_NAME)%>%
  tally()%>%
  ungroup()
TAB.comm=TAB%>%
          filter(retainedflag=='Yes')%>%
          arrange(-n)%>%
          mutate(Cum=cumsum(n),
                 Percent=100*Cum/sum(n))
TAB.disc=TAB%>%
  filter(retainedflag=='No' & !COMMON_NAME%in%TAB.comm$COMMON_NAME)%>%
  arrange(-n)%>%
  mutate(Cum=cumsum(n),
         Percent=100*Cum/sum(n))

fn.obs_cam.barplot=function(n.shots,Cam,Obs,LGN,TITL)
{
  LGN.LL=paste(' (',n.shots$LL,' shots)',sep='')
  LGN.GN=paste(' (',n.shots$GN,' shots)',sep='')
  
  d=rbind(Cam,Obs)%>%
    mutate(Method.hour=ifelse(method=='Longline',paste(method,LGN.LL,sep=''),
                       ifelse(method=='Gillnet',paste(method,LGN.GN,sep=''),
                       NA)))%>%
    filter(!is.na(method))%>%
    mutate(COMMON_NAME=ifelse(COMMON_NAME%in%This.sp.other,COMMON_NAME,"Other"))
  p=d%>%
    ggplot(aes(x=COMMON_NAME, y=n, fill=Platform)) + 
    geom_bar(position="dodge", stat="identity")+
    coord_flip() +
    facet_wrap(~Method.hour,dir='h',scales='free_x')+ 
    theme_PA(Ttl.siz=20,str.siz=16,strx.siz=16,
             leg.siz=18,axs.t.siz=16,axs.T.siz=18)+
    theme(legend.position = LGN,
          legend.justification='center',
          legend.direction='horizontal',
          legend.title = element_blank())+
    xlab('')+ylab('Number of individuals')+
    scale_y_continuous(breaks = integer_breaks())+
    guides(fill = guide_legend(nrow = 1))+
    scale_x_discrete(position = "top")
  
  if(!is.null(TITL)) p=p+ggtitle(TITL)
  
  print(p)
}
    #Commercial species
This.sp=TAB.comm%>%pull(COMMON_NAME)
This.sp.other=TAB.comm%>%filter(Percent<=95)%>%pull(COMMON_NAME)
p1=fn.obs_cam.barplot(n.shots=DATA%>%
                        filter(sheet_no%in%unique(Video.camera1$sheet_no))%>%
                        distinct(sheet_no,method)%>%
                        group_by(method)%>%
                        tally()%>%
                        spread(method,n),
                      Cam=Video.camera1%>%
                            mutate(Code=as.numeric(Code))%>%
                            left_join(All.species.names%>%
                                        dplyr::select(COMMON_NAME,Code)%>%
                                        distinct(Code,.keep_all=T),
                                      by="Code")%>%
                            mutate(method=ifelse(method=="GN","Gillnet",
                                                 ifelse(method=="LL","Longline",
                                                        NA)))%>%
                            filter(COMMON_NAME%in%This.sp)%>%
                            group_by(COMMON_NAME,method)%>%
                            summarise(n=sum(number))%>%
                            mutate(Platform="Camera"),
                      Obs=DATA%>%
                            filter(sheet_no%in%unique(Video.camera1$sheet_no) & 
                                     COMMON_NAME%in%This.sp)%>%
                            mutate(method=ifelse(method=="GN","Gillnet",
                                                 ifelse(method=="LL","Longline",
                                                        NA)))%>%
                            group_by(COMMON_NAME,method)%>%
                            summarise(n=sum(number))%>%
                            mutate(Platform="Observer"),
                      LGN="top",
                      TITL="Commercial species")

    #Bycatch species
This.sp=TAB.disc%>%pull(COMMON_NAME)
This.sp.other=TAB.disc%>%filter(Percent<=95)%>%pull(COMMON_NAME)
p2=fn.obs_cam.barplot(n.shots=DATA%>%
                        filter(sheet_no%in%unique(Video.camera1$sheet_no))%>%
                        distinct(sheet_no,method)%>%
                        group_by(method)%>%
                        tally()%>%
                        spread(method,n),
                      Cam=Video.camera1%>%
                        mutate(Code=as.numeric(Code))%>%
                        left_join(All.species.names%>%
                                    dplyr::select(COMMON_NAME,Code)%>%
                                    distinct(Code,.keep_all=T),
                                  by="Code")%>%
                        mutate(method=ifelse(method=="GN","Gillnet",
                                             ifelse(method=="LL","Longline",
                                                    NA)))%>%
                        filter(COMMON_NAME%in%This.sp)%>%
                        group_by(COMMON_NAME,method)%>%
                        summarise(n=sum(number))%>%
                        mutate(Platform="Camera"),
                      Obs=DATA%>%
                        filter(sheet_no%in%unique(Video.camera1$sheet_no) & 
                                 COMMON_NAME%in%This.sp)%>%
                        mutate(method=ifelse(method=="GN","Gillnet",
                                             ifelse(method=="LL","Longline",
                                                    NA)))%>%
                        group_by(COMMON_NAME,method)%>%
                        summarise(n=sum(number))%>%
                        mutate(Platform="Observer"),
                      LGN="top",
                      TITL="Discarded species")

fig=ggarrange(p1+rremove("xlab"), p2+rremove("xlab"),
              ncol = 1, nrow = 2,
              common.legend=TRUE)
annotate_figure(fig,bottom = text_grob("Number of individuals",size = 18))
ggsave(le.paste("Camera_v_Observer/Barplot.tiff"),width = 12,height = 12,compression = "lzw")


#3. Statistical comparison
little.fn=function(d,Method,group,Not)
{
  n=d%>%
    filter(method==Method)%>%
    group_by(COMMON_NAME)%>%
    summarise(Tot=sum(n))%>%
    arrange(-Tot)%>%
    mutate(Cumktch=cumsum(Tot)/sum(Tot))
  if(group=='95')
  {
    what.species=n%>%mutate(species2=ifelse(Cumktch<=0.95,COMMON_NAME,'Other'))%>%pull(species2)
  }
  if(group=='Top20')
  {
    what.species=n%>%mutate(Row=1:n(),
                            species2=ifelse(Row<=20,COMMON_NAME,'Other'))%>%pull(species2)
  }
  what.species=unique(what.species)
  dd=d%>%
    filter(method==Method)%>%
    mutate(COMMON_NAME=ifelse(COMMON_NAME%in%what.species,COMMON_NAME,"Other"))%>%
    group_by(COMMON_NAME,sheet_no,Platform)%>%
    summarise(N=sum(n))%>%
    spread(COMMON_NAME,N,fill=0)%>%
    ungroup()
  
  dd$ColSum=rowSums(dd[-match(Not,names(dd))])
  dd[-match(c(Not,'ColSum'),names(dd))]=dd[-match(c(Not,'ColSum'),names(dd))]/dd$ColSum
  dd=dd%>%
    dplyr::select(-ColSum)%>%
    data.frame
  return(dd)
}
D2=function(null,dev) (null-dev)/null
fn.mod=function(dd,SBTL)
{
  #check_overdispersion(Pois.mod)
  #Pois.mod <- glm(Camera ~ Observer * method, family=poisson(link = "identity"), data=d1) #identity link to assume linearity
  #QuasiPois.mod <- glm(Camera ~ Observer * method, family=quasipoisson(link = "identity"), data=d1)
  #NB.mod <- glm.nb(Camera ~ Observer * method, data=d1, link='identity')
  
  mod <- glm(Camera ~ Observer + method, family="gaussian", data=dd)
  D2.mod=D2(mod$null.deviance,mod$deviance)
  
  pred=predict(mod,type='response',se.fit=T)
  Preds=data.frame(Observer=dd$Observer,
                   method=dd$method)%>%
    mutate(Camera=pred$fit,
           Pred.SE=pred$se.fit,
           low=Camera-1.96*Pred.SE,
           high=Camera+1.96*Pred.SE)
  p=dd%>%
    ggplot(aes(Observer,Camera,color=method))+
    geom_jitter(size=2.5)+
    geom_line(aes(Observer,Observer),linetype=2,color='black')+
    geom_line(data=Preds,aes(Observer,Camera))+
    geom_ribbon(data=Preds,aes(ymin = low, ymax = high,fill=method),alpha = 0.2)+
    theme_PA(Ttl.siz=18,Sbt.siz=17,str.siz=16,strx.siz=16,
             leg.siz=18,axs.t.siz=16,axs.T.siz=18)+
    theme(legend.position = "top",
          legend.title = element_blank())+
    xlab("Number of individuals reported by observer")+
    ylab("Number of individuals recorded from camera")+
    labs(subtitle=bquote(.(SBTL)~.("(")*D^2*.('=')*.(round(D2.mod,3))*.(")")))
  
  COEF=summary(mod)
  COEF=as.data.frame(COEF$coefficients)[1:2,1:2]%>%
    rename(SE="Std. Error")%>%
    mutate(low=Estimate-1.96*SE,
           high=Estimate+1.96*SE,
           Coef=c("Intercept","Slope"))
  
  return(list(mod=mod,p=p))
}
lolipop=function(x)
{
  p=x%>%
    ungroup()%>%
    mutate(COMMON_NAME = fct_reorder(COMMON_NAME, N))%>%
    ggplot()+
    geom_linerange(aes(x = COMMON_NAME, ymin = 0, ymax =  Mean, colour = method), 
                   position = position_dodge(width = 0),show.legend=F)+
    geom_hline(yintercept = 0,linetype=2)+
    geom_errorbar(aes(ymin = xmin,ymax = xmax,x=COMMON_NAME),width = .5,show.legend=F)+
    geom_point(aes(x = COMMON_NAME, y = Mean, fill = method),pch=21,size=2.5,
               position = position_dodge(width = 0))+
    geom_text(aes(x = COMMON_NAME, y = Mean,label = paste("n= ",N,sep='')),
              size=4,hjust=0.5, vjust=-0.5)+
    coord_flip()+
    facet_wrap(~method)+
    theme_PA(Ttl.siz=18,Sbt.siz=17,str.siz=16,strx.siz=16,
             leg.siz=14,axs.t.siz=14,axs.T.siz=18)+
    theme(legend.position = "none")+
    ylab("Percentage diference")+xlab('')
  return(p)
}
fn.compare.obs.cam=function(CAM, OBS, GROUP, Terms)
{
  Not=c('sheet_no',Terms)
  d=rbind(CAM%>%
            mutate(Code=as.numeric(Code))%>%
            filter(!is.na(Code))%>%
            left_join(All.species.names%>%
                        dplyr::select(COMMON_NAME,Code)%>%
                        distinct(Code,.keep_all=T),
                      by="Code")%>%
            filter(!is.na(COMMON_NAME))%>%
            group_by(COMMON_NAME,method,sheet_no)%>%
            summarise(n=sum(number))%>%
            mutate(Platform="Camera"),
          OBS%>%
            filter(sheet_no%in%unique(CAM$sheet_no) & 
                     !is.na(COMMON_NAME))%>%
            group_by(COMMON_NAME,method,sheet_no)%>%
            summarise(n=sum(number))%>%
            mutate(Platform="Observer"))%>%
    ungroup()
  
  Efrt=OBS%>%
    filter(sheet_no%in%d$sheet_no)%>%
    distinct(sheet_no,method,Effort)
  
  d=d%>%
    left_join(Efrt,by=c("sheet_no","method"))%>%
    filter(!COMMON_NAME=="Unidentified")
  
  #extract non-match data for review
  out.for.Jack=d%>%
    dplyr::select(-method)%>%
    spread(Platform,n,fill=0)%>%
    mutate(Discrepancy=abs(Camera-Observer))%>%
    filter(Discrepancy>0)
  
  #display discrepancies
  Shts=d%>%
    distinct(sheet_no)%>%
    arrange(sheet_no)%>%
    mutate(Shot=1:n(),
           Shot=factor(Shot,levels=Shot))
  
  my.formula <- y ~ x
  p.ind=d%>%
    spread(Platform,n,fill=0)%>%
    mutate(Discrepancy=abs(Observer-Camera))%>%
    left_join(Shts,by="sheet_no")%>%
    filter(!is.na(method))%>%
    mutate(method=ifelse(method=="GN","Gillnet",
                         ifelse(method=="LL","Longline",
                                NA)))%>%
    ggplot(aes(x=Observer,y=Camera))+
    geom_line(aes(Observer,Observer),size=1.5)+
    geom_jitter(aes(color=COMMON_NAME), size=3)+
    # geom_smooth(method = "lm",se=T,formula = my.formula)+
    #  stat_poly_eq(formula = my.formula, 
    #              aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    #             label.y = "top",label.x="left",parse = TRUE, size = 5) +
    facet_wrap(~ method,ncol=2,scales = "free_x")+
    xlab("Number of individuals reported by observer")+
    ylab("Number of individuals recorded by camera")+
    theme_PA(Ttl.siz=18,Sbt.siz=17,str.siz=16,strx.siz=16,
             leg.siz=11,axs.t.siz=16,axs.T.siz=18)+
    theme(legend.position = "top",
          legend.title = element_blank())+
    xlim(0,NA) 
  
  p.sp=d%>%
    mutate(n=1)%>%
    spread(Platform,n,fill=0)%>%
    mutate(Discrepancy=abs(Observer-Camera))%>%
    left_join(Shts,by="sheet_no")%>%
    mutate(method=ifelse(method=="GN","Gillnet",
                         ifelse(method=="LL","Longline",
                                NA)))%>%
    filter(!is.na(method))%>%
    group_by(Shot,method)%>%
    summarise(Camera=sum(Camera),
              Observer=sum(Observer))%>%
    ggplot(aes(x=Observer,y=Camera))+
    geom_line(aes(Observer,Observer),size=1.5)+
    geom_jitter(aes(color=method), size=3,show.legend = F)+
    facet_wrap(~ method,ncol=2,scales = "free_x")+
    xlab("Number of species reported by observer")+
    ylab("Number of species recorded by camera")+
    theme_PA(Ttl.siz=18,Sbt.siz=17,str.siz=16,strx.siz=16,
             lgT.siz=18,leg.siz=18,axs.t.siz=16,axs.T.siz=18)+
    theme(legend.position = "top")+
    xlim(0,NA)
  
  
  #univariate stats (follow Emery et al 2019) 
  do.cpue=FALSE
  if(do.cpue)
  {
    d=d%>%
      mutate(cpue=n/Effort,
             log.cpue=log(cpue))
    d%>%
      ggplot(aes(n, fill = Platform)) +
      geom_histogram(binwidth=.5, position="dodge")
    
    d%>%
      filter(method=="GN")%>%
      ggplot(aes(log.cpue, color=Platform)) +geom_density()
    
    d%>%
      filter(method=="LL")%>%
      ggplot(aes(log.cpue, color=Platform)) +geom_density()
    
    mod.GN <- glm(log.cpue ~ Platform * COMMON_NAME, family="gaussian", data=d%>%filter(method=="GN"))
    mod.LL <- glm(log.cpue ~ Platform * COMMON_NAME, family="gaussian", data=d%>%filter(method=="LL"))
  }
  zero.ktch.sht=d%>%
    group_by(sheet_no)%>%
    summarise(N=sum(n))%>%
    filter(N==0)%>%
    pull(sheet_no)
  d1=d
  if(length(zero.ktch.sht)>0) d1=d1%>%filter(!sheet_no%in%zero.ktch.sht)
  d1=d1%>%
    spread(Platform,n,fill=0)%>%
    mutate(method=ifelse(method=="GN","Gillnet",
                         ifelse(method=="LL","Longline",
                                NA)))%>%
    filter(!is.na(method))%>%
    mutate(method=factor(method))
  
  Discarded.species=TAB.disc%>%pull(COMMON_NAME)
  mod_all.species=fn.mod(dd=d1,SBTL="All species")
  mod_commercial.species=fn.mod(dd=d1%>%
                                  filter(!COMMON_NAME%in%Discarded.species),
                                SBTL="Commercial species")  
  mod_discarded.species=fn.mod(dd=d1%>%
                                 filter(COMMON_NAME%in%Discarded.species),
                               SBTL="Discarded species")
  
  #Percentage difference by species
  Per.diff=d1%>%
    rowwise()%>%
    mutate(Max=max(c_across(Camera:Observer)))%>%
    mutate(Per.diff=100*(Camera-Observer)/Max)%>%
    group_by(COMMON_NAME,method)%>%
    summarise(Mean=mean(Per.diff),
              SD=sd(Per.diff),
              N=n())%>%
    mutate(SE=SD/sqrt(SD),
           xmin=ifelse(!is.na(SE),Mean-SE,0),
           xmax=ifelse(!is.na(SE),Mean+SE,0))
  Per.dif.com=lolipop(x=Per.diff%>%filter(!COMMON_NAME%in%Discarded.species))
  Per.dif.dis=lolipop(x=Per.diff%>%filter(COMMON_NAME%in%Discarded.species))
  
  
  
  #multivariate
  #1. GN
  dd=little.fn(d=d,Method="GN",group=GROUP,Not=Not)   
  Community <- dd[-match(Not,names(dd))]  
  
  #MDS
  MDS <- metaMDS(comm = Community, distance = "bray",k=2,trymax=100, trace = FALSE, autotransform = FALSE)
  MDS_xy <-data.frame(MDS$points)%>%
    mutate(dummy =dd[,match(Terms,names(dd))],
           name=dd[,match('sheet_no',names(dd))])
  p.GN=MDS_xy%>%
    ggplot(aes(MDS1, MDS2, color = dummy,label =name)) +
    geom_point(size=3) + geom_text_repel(show.legend = F)+
    theme_bw() +
    annotate(geom="text", x=0.85*max(MDS_xy$MDS1), y=min(MDS_xy$MDS2), 
             label=paste("Stress=",round(MDS$stress,3)))+
    theme_PA(Ttl.siz=18,leg.siz=14,axs.t.siz=10,axs.T.siz=14)+
    theme(legend.position = "top",
          legend.justification='right',
          legend.title = element_blank())+
    guides(colour = guide_legend(nrow = 1))+
    ggtitle("Gillnet")+xlab('')+ylab('')
  
  #Permanova
  adon.results<-adonis(formula(paste('Community',Terms,sep='~')),data=dd, method="bray",perm=5e3)
  adon.GN=as.data.frame(adon.results$aov.tab)
  
  
  #2. LL
  dd=little.fn(d=d,Method="LL",group=GROUP,Not=Not)   
  Community <- dd[-match(Not,names(dd))]
  
  #MDS
  MDS <- metaMDS(comm = Community, distance = "bray",k=2,trymax=100, trace = FALSE, autotransform = FALSE)
  MDS_xy <-data.frame(MDS$points)%>%
    mutate(dummy = dd[,match(Terms,names(dd))],
           name=dd[,match('sheet_no',names(dd))])
  p.LL=MDS_xy%>%
    ggplot(aes(MDS1, MDS2, color = dummy,label =name)) +
    geom_point(size=3) + geom_text_repel(show.legend = F)+
    theme_bw() +
    annotate(geom="text", x=0.85*max(MDS_xy$MDS1), y=min(MDS_xy$MDS2), 
             label=paste("Stress=",round(MDS$stress,3)))+
    theme_PA(Ttl.siz=18,leg.siz=12,axs.t.siz=10,axs.T.siz=14)+
    theme(legend.position = "none",
          legend.title = element_blank())+
    guides(colour = guide_legend(nrow = 1))+
    ggtitle("Longline")+xlab('')+ylab('')
  
  #Permanova
  adon.results<-adonis(formula(paste('Community',Terms,sep='~')),data=dd, method="bray",perm=5e3)
  adon.LL=as.data.frame(adon.results$aov.tab)
  
  
  return(list(out.for.Jack=out.for.Jack,
              p.ind=p.ind,p.sp=p.sp,
              mod_all.species=mod_all.species,
              mod_commercial.species=mod_commercial.species,
              mod_discarded.species=mod_discarded.species,
              Per.dif.com=Per.dif.com,
              Per.dif.dis=Per.dif.dis,
              p.GN=p.GN,p.LL=p.LL,
              adon.GN=adon.GN,adon.LL=adon.LL))
}
Out=fn.compare.obs.cam(CAM=Video.camera1,
                       OBS=DATA,
                       GROUP='Top20',
                       Terms='Platform')
  #export data issues
write.csv(Out$out.for.Jack,le.paste("Camera_v_Observer/out.for.Jack.csv"),row.names=F)

#export Raw data comparison
Out$p.ind
ggsave(le.paste("Camera_v_Observer/raw.tiff"),width = 12,height = 10,compression = "lzw")

Out$p.sp
ggsave(le.paste("Camera_v_Observer/raw_species.tiff"),width = 12,height = 10,compression = "lzw")


  #export GLMS
ft <- as_flextable(Out$mod_all.species$mod)
save_as_image(ft, path = le.paste("Camera_v_Observer/GLM_mod_all.species.png"))
ft <- as_flextable(Out$mod_commercial.species$mod)
save_as_image(ft, path = le.paste("Camera_v_Observer/GLM_mod_commercial.species.png"))
ft <- as_flextable(Out$mod_discarded.species$mod)
save_as_image(ft, path = le.paste("Camera_v_Observer/GLM_mod_discarded.species.png"))

fig=ggarrange(Out$mod_all.species$p+rremove("xlab")+rremove("ylab"),
              Out$mod_commercial.species$p+rremove("xlab")+rremove("ylab"),
              Out$mod_discarded.species$p+rremove("xlab")+rremove("ylab"),
              ncol = 1, nrow = 3,
              common.legend=TRUE)
annotate_figure(fig,
                bottom = text_grob("Number of individuals reported by observer",size = 18),
                left = text_grob("Number of individuals recorded by camera",size = 18,rot = 90))
ggsave(le.paste("Camera_v_Observer/GLM_preds.tiff"),width = 8,height = 12,compression = "lzw")


  #export Percentage difference
Out$Per.dif.com
ggsave(le.paste("Camera_v_Observer/Percentage.difference_com.tiff"),width = 10,height = 12,compression = "lzw")

Out$Per.dif.dis
ggsave(le.paste("Camera_v_Observer/Percentage.difference_disc.tiff"),width = 10,height = 12,compression = "lzw")


  #export MDS
fig=ggarrange(Out$p.GN, Out$p.LL,
              ncol = 1, nrow = 2,
              common.legend=TRUE)
ggsave(le.paste("Camera_v_Observer/MDS.tiff"),width = 8,height = 12,compression = "lzw")


  #export Permanovas
write.csv(Out$adon.GN,le.paste("Camera_v_Observer/Permanova_GN.csv"),row.names = F)
write.csv(Out$adon.LL,le.paste("Camera_v_Observer/Permanova_LL.csv"),row.names = F)


#---------Analysis of Deck 2 camera VS observers --------------------------------------------------------------------

# Deck 2  (over roller)                      
Video.camera2=Video.camera2.deck%>%
  mutate(Code=ifelse(Genus=="Kyphosus" & Species=="spp",'37361903',Code))%>%
  dplyr::select(DPIRD.code,Code,Period)%>%
  mutate(number=1)%>%
  data.frame%>%
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
         Period=ifelse(Period=='gn','gillnet',
                       ifelse(Period=='ll','longline',Period)),
         sheet_no=case_when(Period=='gillnet'~str_remove(word(DPIRD.code,1,sep = "\\/"),'GN'),
                            Period=='longline'~str_remove(word(DPIRD.code,2,sep = "\\/"),'LL')))%>%
  left_join(DATA_PA,by='sheet_no')%>%
  mutate(Data.set="camera")


#1 Barplot of observer VS camera
TAB=DATA%>%
  filter(!is.na(COMMON_NAME) & !COMMON_NAME=='')%>%
  group_by(retainedflag,COMMON_NAME)%>%
  tally()%>%
  ungroup()
TAB.comm=TAB%>%
  filter(retainedflag=='Yes')%>%
  arrange(-n)%>%
  mutate(Cum=cumsum(n),
         Percent=100*Cum/sum(n))
TAB.disc=TAB%>%
  filter(retainedflag=='No' & !COMMON_NAME%in%TAB.comm$COMMON_NAME)%>%
  arrange(-n)%>%
  mutate(Cum=cumsum(n),
         Percent=100*Cum/sum(n))


#Commercial species
This.sp=TAB.comm%>%pull(COMMON_NAME)
This.sp.other=TAB.comm%>%filter(Percent<=95)%>%pull(COMMON_NAME)
p1=fn.obs_cam.barplot(n.shots=DATA%>%
                        filter(sheet_no%in%unique(Video.camera2$sheet_no))%>%
                        distinct(sheet_no,method)%>%
                        group_by(method)%>%
                        tally()%>%
                        spread(method,n),
                      Cam=Video.camera2%>%
                        mutate(Code=as.numeric(Code))%>%
                        left_join(All.species.names%>%
                                    dplyr::select(COMMON_NAME,Code)%>%
                                    distinct(Code,.keep_all=T),
                                  by="Code")%>%
                        mutate(method=ifelse(method=="GN","Gillnet",
                                             ifelse(method=="LL","Longline",
                                                    NA)))%>%
                        filter(COMMON_NAME%in%This.sp)%>%
                        group_by(COMMON_NAME,method)%>%
                        summarise(n=sum(number))%>%
                        mutate(Platform="Camera"),
                      Obs=DATA%>%
                        filter(sheet_no%in%unique(Video.camera2$sheet_no) & 
                                 COMMON_NAME%in%This.sp)%>%
                        mutate(method=ifelse(method=="GN","Gillnet",
                                             ifelse(method=="LL","Longline",
                                                    NA)))%>%
                        group_by(COMMON_NAME,method)%>%
                        summarise(n=sum(number))%>%
                        mutate(Platform="Observer"),
                      LGN="top",
                      TITL="Commercial species")

#Bycatch species
This.sp=TAB.disc%>%pull(COMMON_NAME)
This.sp.other=TAB.disc%>%filter(Percent<=95)%>%pull(COMMON_NAME)
p2=fn.obs_cam.barplot(n.shots=DATA%>%
                        filter(sheet_no%in%unique(Video.camera2$sheet_no))%>%
                        distinct(sheet_no,method)%>%
                        group_by(method)%>%
                        tally()%>%
                        spread(method,n),
                      Cam=Video.camera2%>%
                        mutate(Code=as.numeric(Code))%>%
                        left_join(All.species.names%>%
                                    dplyr::select(COMMON_NAME,Code)%>%
                                    distinct(Code,.keep_all=T),
                                  by="Code")%>%
                        mutate(method=ifelse(method=="GN","Gillnet",
                                             ifelse(method=="LL","Longline",
                                                    NA)))%>%
                        filter(COMMON_NAME%in%This.sp)%>%
                        group_by(COMMON_NAME,method)%>%
                        summarise(n=sum(number))%>%
                        mutate(Platform="Camera"),
                      Obs=DATA%>%
                        filter(sheet_no%in%unique(Video.camera2$sheet_no) & 
                                 COMMON_NAME%in%This.sp)%>%
                        mutate(method=ifelse(method=="GN","Gillnet",
                                             ifelse(method=="LL","Longline",
                                                    NA)))%>%
                        group_by(COMMON_NAME,method)%>%
                        summarise(n=sum(number))%>%
                        mutate(Platform="Observer"),
                      LGN="top",
                      TITL="Discarded species")

fig=ggarrange(p1+rremove("xlab"), p2+rremove("xlab"),
              ncol = 1, nrow = 2,
              common.legend=TRUE)
annotate_figure(fig,bottom = text_grob("Number of individuals",size = 18))
ggsave(le.paste("Camera2_v_Observer/Barplot.tiff"),width = 12,height = 12,compression = "lzw")


#2. Statistical comparison
Out=fn.compare.obs.cam(CAM=Video.camera2,
                       OBS=DATA,
                       GROUP='Top20',
                       Terms='Platform')
#export data issues
write.csv(Out$out.for.Jack,le.paste("Camera2_v_Observer/out.for.Jack.csv"),row.names=F)

#export Raw data comparison
Out$p.ind
ggsave(le.paste("Camera2_v_Observer/raw.tiff"),width = 12,height = 10,compression = "lzw")

Out$p.sp
ggsave(le.paste("Camera2_v_Observer/raw_species.tiff"),width = 12,height = 10,compression = "lzw")


#export GLMS
ft <- as_flextable(Out$mod_all.species$mod)
save_as_image(ft, path = le.paste("Camera2_v_Observer/GLM_mod_all.species.png"))
ft <- as_flextable(Out$mod_commercial.species$mod)
save_as_image(ft, path = le.paste("Camera2_v_Observer/GLM_mod_commercial.species.png"))
ft <- as_flextable(Out$mod_discarded.species$mod)
save_as_image(ft, path = le.paste("Camera2_v_Observer/GLM_mod_discarded.species.png"))

fig=ggarrange(Out$mod_all.species$p+rremove("xlab")+rremove("ylab"),
              Out$mod_commercial.species$p+rremove("xlab")+rremove("ylab"),
              Out$mod_discarded.species$p+rremove("xlab")+rremove("ylab"),
              ncol = 1, nrow = 3,
              common.legend=TRUE)
annotate_figure(fig,
                bottom = text_grob("Number of individuals reported by observer",size = 18),
                left = text_grob("Number of individuals recorded by camera",size = 18,rot = 90))
ggsave(le.paste("Camera2_v_Observer/GLM_preds.tiff"),width = 8,height = 12,compression = "lzw")


#export Percentage difference
Out$Per.dif.com
ggsave(le.paste("Camera2_v_Observer/Percentage.difference_com.tiff"),width = 10,height = 12,compression = "lzw")

Out$Per.dif.dis
ggsave(le.paste("Camera2_v_Observer/Percentage.difference_disc.tiff"),width = 10,height = 12,compression = "lzw")


#export MDS
fig=ggarrange(Out$p.GN, Out$p.LL,
              ncol = 1, nrow = 2,
              common.legend=TRUE)
ggsave(le.paste("Camera2_v_Observer/MDS.tiff"),width = 8,height = 12,compression = "lzw")


#export Permanovas
write.csv(Out$adon.GN,le.paste("Camera2_v_Observer/Permanova_GN.csv"),row.names = F)
write.csv(Out$adon.LL,le.paste("Camera2_v_Observer/Permanova_LL.csv"),row.names = F)


#---------Gilled or bagged ------------ 
Tab.gilled.bagged=Video.camera1.deck%>%
                    filter(Period=="Gillnet")%>%
                    group_by(COMMON_NAME,Code,meshed)%>%
                    tally()%>%
                    ungroup()%>%
                    spread(meshed,n,fill=0)%>%
                    arrange(Code)%>%
                    dplyr::select(-Code)%>%
                    filter(!is.na(COMMON_NAME))
write.csv(Tab.gilled.bagged,le.paste("Video/deck.cameras/Tab.gilled.bagged.csv"),row.names=F)



#---------Analyses for Abbey ------------
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
fn.explr.Abbey.1(d=Abbey.data.chapter.1_species%>%dplyr::rename(Eff=n.cameras),
                 vars=fn.get.top.1(d=Abbey.data.chapter.1_species,N=250))
fn.explr.Abbey.1(d=Abbey.data.chapter.1_SP.group%>%dplyr::rename(Eff=n.cameras),
                 vars=fn.get.top.1(d=Abbey.data.chapter.1_SP.group,N=30))
fn.explr.Abbey.2(d=Abbey.data.chapter.2_species%>%dplyr::rename(Eff=Fishing.effort),
                 vars=fn.get.top.2(d=Abbey.data.chapter.2_species,N=100))
fn.explr.Abbey.2(d=Abbey.data.chapter.2_SP.group%>%dplyr::rename(Eff=Fishing.effort),
                 vars=fn.get.top.2(d=Abbey.data.chapter.2_SP.group,N=30))
dev.off()


#---------Analyse PA TEPS ------------

#2. Cameras
#note: number of events by interaction type
fn.TEPS.barplot=function(n.shots,d,hours.LL,hours.GN,show.all.levels=TRUE,LGN,TITL)
{
  if(is.null(hours.LL))
  {
    LGN.LL=paste(' (',n.shots$longline,' shots)',sep='')
    LGN.GN=paste(' (',n.shots$gillnet,' shots)',sep='')
  }else
  {
    LGN.LL=paste(' (',hours.LL,' video hours; ',n.shots$longline,' shots)',sep='')
    LGN.GN=paste(' (',hours.GN,' video hours; ',n.shots$gillnet,' shots)',sep='')
  }
   
  d=d%>%
    mutate(Method.hour=ifelse(Method=='Longline',paste(Method,LGN.LL,sep=''),
                       ifelse(Method=='Gillnet',paste(Method,LGN.GN,sep=''),
                       NA)))
  p=d%>%
    mutate(Name=factor(Name,levels=TEPS.names$Name))%>%
    ggplot(aes(x=Interaction, y=n, fill=Name)) + 
    geom_bar(position="stack", stat="identity")+
    coord_flip() +
    facet_wrap(~Method.hour,dir='h',scales='free_x')+ 
    theme_PA(str.siz=16,strx.siz=16,leg.siz=17,axs.t.siz=16,axs.T.siz=18)+
    theme(legend.position = LGN,
          legend.justification='left',
          legend.direction='horizontal',
          legend.title = element_blank())+
    xlab('')+ylab('Number of events')+
    scale_y_continuous(breaks = integer_breaks())+
    guides(fill = guide_legend(nrow = 2))+
    scale_x_discrete(position = "top")
  
  if(!is.null(TITL)) p=p+ggtitle(TITL)
  if(show.all.levels) p=p+scale_fill_manual(values=TEPS.cols)
  if(!show.all.levels)
  {
    THIS=match(unique(d$Name),names(TEPS.cols))
    p=p+scale_fill_manual(values=TEPS.cols[THIS])
  }
  print(p)
}

# 2.1. Underwater
p1=fn.TEPS.barplot(n.shots=rbind(Video.longline.interaction%>%
                                   dplyr::select(sheet_no,Method,Interaction,Number,SP.group,Species,Code)%>%
                                   mutate(Period='longline'),
                                 Video.net.interaction%>%
                                   dplyr::select(sheet_no,Method,Interaction,Number,SP.group,Species,Code)%>%
                                   mutate(Period='gillnet'))%>%
                     distinct(sheet_no,Period)%>%
                     group_by(Period)%>%
                     tally()%>%
                     spread(Period,n),
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
                     mutate(Interaction=capitalize(tolower(Interaction)))%>%
                     mutate(Interaction=case_when(Interaction=="Avoid" ~ "Avoidance",
                                                  Interaction=="Caught while predating" ~ "Caught while feeding",
                                                  Interaction=="Bounce off" ~ "Bounced off",
                                                  Interaction== "Swim past" ~ "Swam past",
                                                  Interaction== "Swim through" ~ "Swam through",
                                                  TRUE~Interaction)),
                   hours.LL=hours.underwater.ll,
                   hours.GN=hours.underwater.gn,
                   LGN="none",
                   TITL="Underwater cameras")
#ggsave(le.paste("TEPS/Interactions_number.events_underwater.tiff"),width = 12,height = 8,compression = "lzw")


# 2.2. subsurface
p2=fn.TEPS.barplot(n.shots=Video.subsurface%>%
                     distinct(SHEET_NO,Period)%>%
                     group_by(Period)%>%
                     tally()%>%
                     spread(Period,n),
                   d=Video.subsurface%>%
                     filter(Code%in%TEPS.codes)%>%
                     left_join(TEPS.names,by="Code")%>%
                     mutate(Number=1,
                            Method=capitalize(Period),
                            Drop.out=capitalize(tolower(Drop.out)),
                            Dropout.condition=tolower(Dropout.condition),
                            Dropout.condition=case_when(is.na(Dropout.condition)~'',
                                                        TRUE~Dropout.condition),
                            dummy=case_when(Drop.out=="Yes"~"Drop out",
                                            Drop.out=="No"~"Caught"),
                            Interaction=case_when(dummy=="Drop out"~paste(dummy," (",Dropout.condition,")",sep=''),
                                                  dummy=="Caught"~dummy))%>%
                     group_by(Method,Interaction,Name,Colr,Code)%>%
                     tally(Number)%>%
                     mutate(Name=factor(Name,levels=TEPS.names$Name)),
                   hours.LL=hours.subsurface.ll,
                   hours.GN=hours.subsurface.gn,
                   LGN="none",
                   TITL="Subsurface camera")
#ggsave(le.paste("TEPS/Interactions_number.events_subsurface.tiff"),width = 12,height = 8,compression = "lzw")

# 2.3. Deck 1 
p3=fn.TEPS.barplot(n.shots=Video.camera1%>%
                     distinct(sheet_no,Period)%>%
                     group_by(Period)%>%
                     tally()%>%
                     spread(Period,n),
                   d=Video.camera1%>%
                     filter(Code%in%TEPS.codes)%>%
                     mutate(Interaction="Caught",
                            Code=as.numeric(Code))%>%
                     left_join(TEPS.names,by="Code")%>%
                     mutate(Number=1,
                            Method=capitalize(Period))%>%
                     group_by(Method,Interaction,Name,Colr,Code)%>%
                     tally(Number)%>%
                     mutate(Name=factor(Name,levels=TEPS.names$Name)),
                   hours.LL=hours.deck1.ll,
                   hours.GN=hours.deck1.gn,
                   LGN="none",
                   TITL="Deck camera 1")
#ggsave(le.paste("TEPS/Interactions_number.events_deck1.tiff"),width = 12,height = 8,compression = "lzw")

# 2.4. Deck 2  (pointing to roller)
p4=fn.TEPS.barplot(n.shots=rbind(Video.camera2.deck_observations%>%distinct(Period,SHEET_NO),
                                 Video.camera2.deck%>%distinct(Period,SHEET_NO))%>%
                     distinct(SHEET_NO,Period)%>%
                     group_by(Period)%>%
                     tally()%>%
                     spread(Period,n),
                   d=rbind(Video.camera2.deck_observations%>%
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
                            Method=capitalize(Period),
                            Activity=capitalize(Activity))%>%
                     group_by(Method,Activity,Name,Colr,Code)%>%
                     tally(Number)%>%
                     mutate(Interaction=case_when(Activity%in%c("Feeding from gillnet",
                                                                "Feeding from longline")~"Feeding",
                                                  TRUE~Activity))%>%
                     mutate(Name=factor(Name,levels=TEPS.names$Name)),
                   hours.LL=hours.deck2.ll,
                   hours.GN=hours.deck2.gn,
                   LGN="none",
                   TITL="Deck camera 2")
#ggsave(le.paste("TEPS/Interactions_number.events_deck2.tiff"),width = 12,height = 8,compression = "lzw")

#3. Observer data 
#data from TEPS data sheets
d.teps=TEPS%>%
  mutate(Method=ifelse(gear.type=="GN","Gillnet",
                       ifelse(gear.type=="LL","Longline",
                              NA)),
         con.code.sp=paste(capitalize(common.name),contact.code.to.complete),
         con.code.sp=ifelse(con.code.sp=="NA NA",NA,con.code.sp))%>%
  group_by(sheet_no,Method,contact.code.to.complete,common.name)%>%
  summarise(n=sum(contact.count))%>%
  mutate(common.name=capitalize(common.name),
         contact.code.to.complete=capitalize(contact.code.to.complete),
         contact.code.to.complete=case_when(contact.code.to.complete=='Entangled/hooked'~'Caught',
                                            contact.code.to.complete=='Feeding from net/longline'~'Feeding',
                                            TRUE~contact.code.to.complete),
         Name=common.name,
         Interaction=contact.code.to.complete)

#data from biological data sheets
d.data=rbind(DATA%>%
               filter(grepl(paste(tolower(c('bird','sea lion','Smooth',TEPS.names$Name)),collapse='|'),tolower(comments))),
             DATA%>%
               filter(common_name%in%TEPS.names$Name))%>%
  distinct(sheet_no,line_no,method,common_name)%>%
  filter(common_name%in%TEPS.names$Name)%>%
  rename(common.name=common_name,
         Method=method)%>%
  mutate(contact.code.to.complete='Caught',
         n=1,
         Name=common.name,
         Interaction=contact.code.to.complete)%>%
  filter(!sheet_no%in%d.teps$sheet_no)
d.data=d.data[,match(names(d.teps),names(d.data))]%>%
  mutate(Method=ifelse(Method=="GN","Gillnet",
                       ifelse(Method=="LL","Longline",
                              NA)))  

p5=fn.TEPS.barplot(n.shots=DATA%>%
                         distinct(sheet_no,method)%>%
                         mutate(method=ifelse(method=="GN","gillnet",
                                              ifelse(method=="LL","longline",
                                                     NA)))%>%
                         group_by(method)%>%
                         tally()%>%
                         spread(method,n),
                   d=rbind(d.teps,d.data),
                   hours.LL=NULL,
                   hours.GN=NULL,
                   LGN="none",
                   TITL="Onboard observers")
#ggsave(le.paste("TEPS/Numbers.interactions.by.gear_Observers.tiff"), width = 12,height = 8,compression = "lzw")

fig=ggarrange(p1+rremove("xlab"), p2+rremove("xlab"),
              p4+rremove("xlab"), p3+rremove("xlab"), 
              p5+rremove("xlab"),
              ncol = 1, nrow = 5,
              common.legend=TRUE,
              heights=c(1.5,rep(1,4)))
annotate_figure(fig,bottom = text_grob("Number of events",size = 18))
ggsave(le.paste("TEPS/Interactions.tiff"),width = 12,height = 12,compression = "lzw")


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


#---------Analyse PA socio-economic survey ------------
#some manipulations
Survey.fishers=Survey.fishers%>%  
                    data.frame%>%
                    filter(if_any(everything(), ~ !is.na(.)))
Survey.processor=Survey.processor%>%    
                    data.frame%>%
                    filter(if_any(everything(), ~ !is.na(.)))

#amend responses not summing to 100%
Survey.fishers[paste("Q.19.",letters[1:11],sep='')]=100*Survey.fishers[paste("Q.19.",letters[1:11],sep='')]/rowSums(Survey.fishers[paste("Q.19.",letters[1:11],sep='')])
Survey.processor[paste("Q.16.",letters[1:8],sep='')]=100*Survey.processor[paste("Q.16.",letters[1:8],sep='')]/rowSums(Survey.processor[paste("Q.16.",letters[1:8],sep='')])


#1. Display all responses 
fn.plt.hist=function(d,Var)
{
  p=d%>%
    mutate(x=!!sym(Var))%>%
    ggplot(aes(x))+
    geom_histogram(fill="forestgreen")+
    xlab(Var)+ylab("")+
    theme_PA(leg.siz=Cex.res,axs.t.siz=Cex.res,axs.T.siz=Cex.res+4)+
  theme(legend.position = "none")
  print(p)
}
fn.plt.barplt=function(d,Var)
{
  d=d%>%mutate(x=!!sym(Var))%>%group_by(x)%>%tally()%>%filter(!is.na(x))
  p=d%>%
    ggplot(aes(x=x,y=n))+
    geom_bar(stat="identity",aes(fill=x))+
    xlab(Var)+ylab("")+
    theme_PA(leg.siz=Cex.res,axs.t.siz=Cex.res,axs.T.siz=Cex.res+4)+
    theme(legend.position = "none")
  print(p)
}
Cex.res=12

  # 1.1 Fishers questionnaire
Personal.questions.fisher=c("Inteview","Q.1","Q.2","Q.4","Q.5")
Comments.fisher="Q.30"
Fishers.list=list(c("Ownership", "Employment", "Demographics"),
                  "Catch",
                  "Revenue",
                  c("Costs", "Other"),
                  "Bus. Exp. location")
names(Fishers.list)=Fishers.list
for(l in 1:length(Fishers.list))
{
  if(length(Fishers.list[[l]])>1) names(Fishers.list)[l]=paste(Fishers.list[[l]],collapse="_")
}
for(l in 1:length(Fishers.list))
{
  Fishers.list[[l]]=Survey.fishers.metadata%>%
                        filter(Section%in%Fishers.list[[l]])%>%
                        pull(Question_no)
  Fishers.list[[l]]=subset(Fishers.list[[l]],!Fishers.list[[l]]%in%Personal.questions.fisher,Comments.fisher)
}
for(l in 1:length(Fishers.list))
{
  a=Fishers.list[[l]]
  D.lis=vector('list',length(a))
  for(s in 1:length(D.lis))
  {
    dummy=Survey.fishers[,a[s]]
    if(is.character(dummy)|is.logical(dummy))D.lis[[s]]=fn.plt.barplt(d=Survey.fishers,Var=a[s])
    if(is.numeric(dummy))D.lis[[s]]=fn.plt.hist(d=Survey.fishers,Var=a[s])
    rm(dummy)
  }
  optimum.disp=n2mfrow(length(D.lis))
  fig=ggarrange(plotlist=compact(D.lis),
            nrow=optimum.disp[1],
            ncol = optimum.disp[2])
  annotate_figure(fig,left = text_grob("Frequency", rot=90,size=Cex.res+4))
  ggsave(le.paste(paste("Socio-economics/Responses_fishers_",names(Fishers.list)[l],".tiff",sep='')),
         width = 10,height = 10,compression = "lzw")
}


  # 1.2 Processor questionnaire
Personal.questions.proc=c("Inteview","Q.1","Q.2.a")
Comments.proc="Q.26"
Proc.list=list(c("Ownership", "Employment", "Demographics"),
                  "Catch",
                  "Revenue",
                  c("Costs", "Other"),
                  "Bus. Exp. location")
names(Proc.list)=Proc.list
for(l in 1:length(Proc.list))
{
  if(length(Proc.list[[l]])>1) names(Proc.list)[l]=paste(Proc.list[[l]],collapse="_")
}
for(l in 1:length(Proc.list))
{
  Proc.list[[l]]=Survey.processor.metadata%>%
    filter(Section%in%Proc.list[[l]])%>%pull(Question_no)
  Proc.list[[l]]=subset(Proc.list[[l]],!Proc.list[[l]]%in%Personal.questions.proc,Comments.proc)
}
for(l in 1:length(Proc.list))
{
  a=Proc.list[[l]]
  D.lis=vector('list',length(a))
  for(s in 1:length(D.lis))
  {
    dummy=Survey.processor[,a[s]]
    if(is.character(dummy)|is.logical(dummy))D.lis[[s]]=fn.plt.barplt(d=Survey.processor,Var=a[s])
    if(is.numeric(dummy))D.lis[[s]]=fn.plt.hist(d=Survey.processor,Var=a[s])
    rm(dummy)
  }
  optimum.disp=n2mfrow(length(D.lis))
  fig=ggarrange(plotlist=compact(D.lis),
                nrow=optimum.disp[1],
                ncol = optimum.disp[2])
  annotate_figure(fig,left = text_grob("Frequency", rot=90,size=Cex.res+4))
  ggsave(le.paste(paste("Socio-economics/Responses_processors_",names(Proc.list)[l],".tiff",sep='')),
         width = 10,height = 10,compression = "lzw")
}


#2. Plot Network
Export.SN=FALSE   #only export once all data in as it creates dummy files in out folder
if(Export.SN)
{
  setwd(le.paste(paste("Socio-economics","network",sep='/')))
  fn.social.net=function(d1,d2,Other,out,dummy)
  {
    df=d1%>%
      gather(Question_no,value,-Inteview)
    d=d2%>%
      filter(Question_no%in%df$Question_no)%>%
      dplyr::select(-Section)%>%
      mutate(Question=sub(".*? ", "", sub(".* following buyers", "", Question)))
    Other=Other%>%
      rename(Question=2)%>%
      mutate(Question=tolower(Question),
             Question1=case_when(grepl('processor',Question)~"Other fish processor",
                                 TRUE~Question),
             Question="Other")
    
    dat=left_join(df,d,by='Question_no')%>%
      left_join(Other,by=c('Question','Inteview'))%>%
      dplyr::select(-Question_no,-Inteview)%>%
      mutate(Question=ifelse(!is.na(Question1),Question1,Question))%>%
      mutate(value=value/sum(value))%>%
      group_by(Question)%>%
      summarise(value=100*sum(value))%>%
      mutate(Question=ifelse(Question=='Self-consumed',dummy,
                      ifelse(Question=='Direct to local restaurants or other retailers','Local restaurant/retailer',
                      Question)),
             value=ifelse(value==0 & Question==dummy,1e-4,value))%>%
      filter(value>0)%>%
      ungroup()
    
    nodes=data.frame(id = dat$Question,
                     label = dat$Question,
                     value=dat$value/100)
    
    edges <- data.frame(from = dummy, to = dat$Question,
                        width = 15*dat$value/100)%>%
      filter(width>1e-03)
    nodes$value=1
    nodes$font.size=20
    
    Net=visNetwork(nodes, edges, height = "600px", width = "100%")%>% 
      visEdges(arrows = "to")%>%
      visLayout(randomSeed=666)%>%
      visHierarchicalLayout(direction='LR',nodeSpacing = 150)
    
    html_name <- tempfile(tmpdir=le.paste("Socio-economics"),fileext = "net.html")
    visSave(Net, html_name)
    webshot(html_name, zoom = 4, 
            file = le.paste(paste("Socio-economics","network",paste(out,".jpeg",sep=''),sep='/')))
    
    
  }
  #2.1. Catch
    #2.1.1 Fisher
  fn.social.net(d1=Survey.fishers[c('Inteview','Q.9.a','Q.9.b','Q.9.c','Q.9.d','Q.9.e','Q.9.f','Q.9.g')],
                d2=Survey.fishers.metadata,
                Other=Survey.fishers[c('Inteview','Q.9.g.i')],
                out='Fisher_catch',
                dummy='Fisher')
  
    #2.1.2. Processor
  fn.social.net(d1=Survey.processor[c('Inteview','Q.6.a','Q.6.b','Q.6.c','Q.6.d','Q.6.e','Q.6.f','Q.6.g')],
                d2=Survey.processor.metadata,
                Other=Survey.processor[c('Inteview','Q.6.g.i')],
                out='Processor_catch',
                dummy='Local fish processor')
}


#3. Shark body parts
Body.parts=rbind(Survey.fishers[,c('Q.10','Q.14')]%>%
                   dplyr::rename_at(vars(c('Q.10','Q.14')), ~ c("Q1","Q2"))%>%
                    mutate(Dat="Fisher"),
                Survey.processor[,c('Q.7.a','Q.11')]%>%
                  dplyr::rename_at(vars(c('Q.7.a','Q.11')), ~ c("Q1","Q2"))%>%
                        mutate(Dat="Processor"))
fn.body.part=function(Q,Var)
{
  p=Body.parts%>%
    ggplot(aes(!!sym(Q)))+
    geom_bar(aes(fill=!!sym(Q)))+
    facet_wrap(~Dat)+
    xlab(Var)+ylab("Frequency")+
    theme_PA(strx.siz=Cex.res+2,leg.siz=Cex.res+1,axs.t.siz=Cex.res+1,axs.T.siz=Cex.res+4)+
    theme(legend.position = "none")
  print(p)
}
p1=fn.body.part(Q="Q1",
             Var=Survey.fishers.metadata%>%filter(Question_no=='Q.10')%>%pull(Question))
p2=fn.body.part(Q="Q2",
                Var=Survey.fishers.metadata%>%filter(Question_no=='Q.14')%>%pull(Question))
ggarrange(p1, p2, ncol = 1, nrow = 2)
ggsave(le.paste("Socio-economics/Question_body.parts.tiff"),width = 10,height = 10,compression = "lzw")


#4. Average price per species
fn.average.barplt=function(d.f,d.f_m,Dat.f,d.p,d.p_m,Dat.p,REMUV,REMUV.p,XLAB,YLAB,Rotate=NULL,Cex=12)
{
  #fisher
  Mean=d.f%>%summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))
  SD=d.f%>%summarise(across(where(is.numeric), ~ sd(.x, na.rm = TRUE)))
  d.f=data.frame(Q=names(Mean),Mean=unlist(Mean),SD=unlist(SD))
  d.f_m=d.f_m%>%
    mutate(Question=str_remove(Question, REMUV))
  if(any(grepl('\\?',d.f_m$Question))) d.f_m=d.f_m%>%mutate(Question=sub(".*? ", "",Question))
  d.f=left_join(d.f,d.f_m,by=c('Q'="Question_no"))%>%
    mutate(Dat=Dat.f)
  
  #processor
  Mean=d.p%>%summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))
  SD=d.p%>%summarise(across(where(is.numeric), ~ sd(.x, na.rm = TRUE)))
  d.p=data.frame(Q=names(Mean),Mean=unlist(Mean),SD=unlist(SD))
  d.p_m=d.p_m%>%
    mutate(Question=str_remove(Question, REMUV.p))
  if(any(grepl('\\?',d.p_m$Question))) d.p_m=d.p_m%>%mutate(Question=sub(".*? ", "",Question))
  d.p=left_join(d.p,d.p_m,by=c('Q'="Question_no"))%>%
          mutate(Dat=Dat.p)
  if(any(grepl('shark',d.p$Question)))d.p=d.p%>%mutate(Dat=paste(Dat.p,' (',sub(".*_", "", Q),')',sep=''))
  
  
  d=rbind(d.f,d.p)%>%
    mutate(Question=case_when(Question=='Sandbar (thick skin) shark'~'Sandbar shark',
                              Question=='Vessel repairs and maintenance'~'Vessel repairs',
                              Question=='Gear repairs, maintenance and replacement'~'Equipment repairs',
                              Question=='Crew (including skipper; salaries, superannuation, etc.)'~'Crew',
                              grepl('Persons working in processing plant',Question)~'Personnel',
                              grepl('Administration',Question)~'Administration',
                              Question=='Sourcing fish (purchasing from fisher or other processor)'~'Sourcing fish',
                              Question=='Equipment repairs, maintenance and replacement'~'Equipment repairs',
                              TRUE~Question),
           Question=factor(Question,
                           levels=unique(Question)))
  if(any(grepl('shark',d$Question))) d=d%>%mutate(Col=ifelse(grepl(paste(c('shark','Shark'),collapse="|"),Question),"steelblue","firebrick"))
  if(!any(grepl('shark',d$Question))) d=d%>%mutate(Col=Question)
  
  p=d%>%
    ggplot(aes(x=Question,y=Mean,fill=Col))+
    geom_bar(stat="identity")+
    geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.2,
                  position=position_dodge(0.05))+
    facet_wrap(~Dat,ncol=1,scales='free')+
    xlab(XLAB)+ylab(YLAB)+
    theme_PA(strx.siz=Cex+2,axs.t.siz=Cex+2,axs.T.siz=Cex+4)+
    theme(legend.position = "none")
  if(any(grepl('shark',d$Question)))p=p+scale_fill_manual(values=c("firebrick","steelblue"))
  if(!is.null(Rotate)) p=p+theme(axis.text.x = element_text(angle = Rotate, vjust = 1, hjust=1))
  
  print(p)
}
Chunk=c(paste(paste('Q.10',letters[1:11],sep='.'),'retail',sep='_'),paste(paste('Q.10',letters[1:11],sep='.'),'wholesale',sep='_'))
fn.average.barplt(d.f=Survey.fishers[,paste('Q.13',letters[1:11],sep='.')],
                  d.f_m=Survey.fishers.metadata%>%filter(Question_no%in%paste('Q.13',letters[1:11],sep='.')),
                  Dat.f="Fisher",
                  d.p=Survey.processor[,Chunk],
                  d.p_m=Survey.processor.metadata%>%filter(Question_no%in%Chunk),
                  Dat.p="Processor",
                  REMUV="What is the average price you receive per kg of ",
                  REMUV.p="What is the average price you receive per kg of ",
                  XLAB='Price per kg',
                  YLAB="AUD",
                  Rotate=45,
                  Cex=18)
ggsave(le.paste("Socio-economics/Question_price.per.kg.tiff"),width = 14,height = 16,compression = "lzw")


#5. Costs
fn.average.barplt(d.f=Survey.fishers[paste("Q.19.",letters[1:11],sep='')],
                  d.f_m=Survey.fishers.metadata%>%filter(Question_no%in%paste("Q.19.",letters[1:11],sep='')),
                  Dat.f="Fisher",
                  d.p=Survey.processor[paste("Q.16.",letters[1:8],sep='')],
                  d.p_m=Survey.processor.metadata%>%filter(Question_no%in%paste("Q.16.",letters[1:8],sep='')),
                  Dat.p="Processor",
                  REMUV="What percentage of your annual fishing costs were due to?",
                  REMUV.p="What percentage of your annual processing costs were due to?",
                  XLAB='Cost item',
                  YLAB="Percentage of total annual costs",
                  Rotate=55,
                  Cex=14)
ggsave(le.paste("Socio-economics/Question_Costs.tiff"),width = 10,height = 12,compression = "lzw")


#6. Business expenditure location
fn.average.barplt(d.f=Survey.fishers[paste("Q.20.",letters[1:11],sep='')],
                  d.f_m=Survey.fishers.metadata%>%filter(Question_no%in%paste("Q.20.",letters[1:11],sep='')),
                  Dat.f="Fisher",
                  d.p=Survey.processor[paste("Q.17.",letters[1:8],sep='')],
                  d.p_m=Survey.processor.metadata%>%filter(Question_no%in%paste("Q.17.",letters[1:8],sep='')),
                  Dat.p="Processor",
                  REMUV='What percentage of your annual costs is spent within your local government area?',
                  REMUV.p="What percentage of your annual processing costs is spent within your local government area?",
                  XLAB='Cost item',
                  YLAB="Percentage of costs spent within local government area",
                  Rotate=35)
ggsave(le.paste("Socio-economics/Question_Expenditure_location.tiff"),width = 10,height = 10,compression = "lzw")



#7. Proportion of trips at a loss  MISSING
#note: run model with terms season, vessel, region


#8. Map of economic contribution by location


#9. Map of social (FTEs, etc) contribution by location


#10. Economc and social contribution 
#missing: input the calculated GVP, social value, etc
SKLr=50
  #Economic contribution
D=data.frame(lon.port=c(117.8837,115.1579),
             lat.port=c(-35.0269,-34.3166),
             Quantity=c(150000,350000))%>%   #the quantity to plot (GVP, FTEs, etc)
  mutate(Prop=Quantity/sum(Quantity))
p1=plt.map(D=D,
           Title="Economic contributon",
           Scaler=SKLr)
Bubble.ref=data.frame(lon.port=c(128,128),
                      lat.port=c(-35.75,-33.75),
                      Prop=c(0.25,0.50),
                      Text=c(1e5,5e5))
p1=p1+geom_point(data = Bubble.ref, mapping = aes(x = lon.port, y = lat.port),
                 size=Bubble.ref$Prop*SKLr, colour = grDevices::adjustcolor("steelblue", alpha=0.6))+
  geom_text(data=Bubble.ref,aes(x = lon.port, y = lat.port,label = paste('$',Text,sep='')),
            fontface="bold",color="white",size=5)


#Social contribution
D=data.frame(lon.port=c(117.8837,115.1579),
             lat.port=c(-35.0269,-34.3166),
             Quantity=c(5,7))%>%   #the quantity to plot (GVP, FTEs, etc)
  mutate(Prop=Quantity/sum(Quantity))
p2=plt.map(D=D,
           Title='Social contribution',
           Scaler=SKLr)

Bubble.ref=data.frame(lon.port=c(128,128),
                      lat.port=c(-35.75,-33.75),
                      Prop=c(0.25,0.50),
                      Text=c(5,10))
p2=p2+geom_point(data = Bubble.ref, mapping = aes(x = lon.port, y = lat.port),
                 size=Bubble.ref$Prop*SKLr, colour = grDevices::adjustcolor("steelblue", alpha=0.6))+
  geom_text(data=Bubble.ref,aes(x = lon.port, y = lat.port,label = paste(Text,'FTEs')),
            fontface="bold",color="white",size=5)


ggarrange(p1, p2, ncol = 1, nrow = 2)
ggsave(le.paste("Socio-economics/Map_contributions.tiff"),width = 7,height = 10,compression = "lzw")



#---------Analyse PA economics of Longline vs Gillnet ------------

#1. Calculate revenue
  #1.1 get catch weight per trip
trunk.ratio=1.59   #average total weight to trunk ratio from catch and effort returns
fin.to.trunk.ratio=0.05     #adopted by NMFS (1993) fide in Biery & Pauly 2012
Retained.catch=DATA%>%filter(retainedflag=='Yes' & !is.na(boat))%>%
  dplyr::select(date,sheet_no,line_no,method,boat,taxa,species,common_name,
                tl,fl,number,tw,a_weight,b_weight)%>%
  group_by(common_name)%>%
  mutate(tw.dummy=mean(tw,na.rm=T),
         tw=ifelse(is.na(tw),tw.dummy,tw))%>%
  ungroup()%>%
  filter(!is.na(tw))%>%
  arrange(date)%>%
  mutate(weight=ifelse(taxa=='Teleost',tw,
                ifelse(taxa=='Elasmobranch',tw/trunk.ratio,
                NA)),
         common_name.economics=case_when(common_name%in%c(Main.species,"fins")~common_name,
                                         !common_name%in%c(Main.species,"fins") & taxa=='Elasmobranch'~"Other sharks",
                                         !common_name%in%c(Main.species,"fins") & taxa=='Teleost'~"Other scalefish"))%>%
  arrange(date)%>%
  mutate(diff_days=difftime(date,lag(date,n=1),units="days"),
         diff_days=ifelse(is.na(diff_days),0,diff_days),
         delta_days=ifelse(diff_days>1,1,0),
         trip=1+cumsum(delta_days))

  #1.2 add price per species from questionnaires
This=c("Q.4",paste('Q.13',letters[1:11],sep='.'))
oldnames = paste('Q.13',letters[1:11],sep='.')
newnames=c("Gummy shark","Dusky shark","Whiskery shark","Sandbar shark","fins","Other sharks",
           "Pink snapper","West Australian dhufish","Queen snapper","Blue groper","Other scalefish")
d.f=Survey.fishers[,This]%>%
  mutate(boat=case_when(Q.4=="Tracey Lea"~"E35",
                        Q.4=="Elizabeth Maria II"~"E007",
                        Q.4=="Viking Legend"~"F399"))%>%
  rename_at(vars(oldnames), ~ newnames)
d.f=d.f%>%
  dplyr::select(-Q.4)%>%
  gather(common_name.economics,Price.per.kg,-boat)%>%
  mutate(Price.per.kg=ifelse(is.na(Price.per.kg) & common_name.economics=='fins',16.5,
                      ifelse(is.na(Price.per.kg) & common_name.economics=='Other scalefish',5.4,
                      Price.per.kg)))  #added manually as not provided

Retained.catch=Retained.catch%>%left_join(d.f,by=c('common_name.economics','boat'))%>%
  mutate(revenue= Price.per.kg*weight)

  #1.3 add fins and aggregate revenue by trip
Retained.catch.fins=Retained.catch%>%
                      filter(taxa=='Elasmobranch')%>%
                      group_by(boat,method,trip)%>%
                      summarise(weight=sum(weight))%>%
                      ungroup()%>%
                      mutate(weight=weight*fin.to.trunk.ratio,
                             common_name.economics='fins')%>%
                      left_join(d.f,by=c('common_name.economics','boat'))%>%
                      mutate(revenue= Price.per.kg*weight)%>%
              dplyr::select(-c(weight,Price.per.kg))

Revenue=rbind(Retained.catch.fins,
              Retained.catch%>%
                  group_by(boat,method,trip,common_name.economics)%>%
                  summarise(revenue=sum(revenue)))%>%
              group_by(boat,method,trip)%>%
              summarise(revenue=sum(revenue))%>%
              ungroup()
#ACA
#2. Calculate costs
#Knuckey et al page 8: auto longline daily costs (bait, fuel, crew, stores): 2,192
#         they assume 200 fishing days per year
#Additional.questions
#Additional.questions.metadata

#3. Calculate profit
#Profit=Revenue - Cost