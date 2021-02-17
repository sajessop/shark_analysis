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
#         Add Video.subsurface to TEPS analysis

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

options(stringsAsFactors = FALSE,dplyr.summarise.inform = FALSE) 

#--------- DATA ------------

#1. Sharks data base
User="Matias"
if(User=="Matias") source("C:/Matias/Analyses/SOURCE_SCRIPTS/Git_other/Source_Shark_bio.R")


#2. Species list
All.species.names=read.csv("C:/Matias/Data/Species.code.csv")


#3. PA - TEPS interactions recorded by Observers
setwd('M:/Agency Data/Draft Publications/Braccini/2019-20_Parks Australia Project/Fieldwork/Data')
TEPS <- read_excel("TEPS interactions.xlsx", sheet = "Sheet1",skip = 1)


#3. PA - number of hook combinations used in PA project
Hook.combos <- read_excel("Hook count.xlsx", sheet = "Sheet1",skip = 2)


#3. PA - underwater video
setwd('C:/Matias/Parks Australia/2019_project/Data/cameras')

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


#3. PA - deck camera 1 (points to measuring board)
file.name.habitat.deck="15_01_2020_Deck 1 habitat and fish.xlsx"
Video.habitat.deck<- read_excel(file.name.habitat.deck, sheet = "Habitat")
Video.camera1.deck<- read_excel(file.name.habitat.deck, sheet = "Deck 1 fish landed")
Video.camera1.deck_extra.records<- read_excel(file.name.habitat.deck, sheet = "extra records")


#3. PA - deck camera 2 (points to roller)
file.name.camera2.deck="20_01_2020_Deck 2.xlsx"
Video.camera2.deck<- read_excel(file.name.camera2.deck, sheet = "Deck 2")
Video.camera2.deck_observations<- read_excel(file.name.camera2.deck, sheet = "Other observations")


#3. PA - subsurface camera   
file.name.subsurface="05_02_2021_subSurface.xlsx"
Video.subsurface<- read_excel(file.name.subsurface, sheet = "ALL Dot Point Measurements")
Video.subsurface.comments<- read_excel(file.name.subsurface, sheet = "comment")





#---------CONTROL SECTION------------
explr.dat.entry=TRUE
do.len_len=FALSE
do.Historic=FALSE

distance.roller.spreader.Anthony=4.3  #in metres
mesh.deep.Anthony=2                   #in metres        #MISSING: get actual value from Jeff

metres.observed=5 # average metres observed underwater


hours.underwater.ll='xx'  #total number of hours of longline underwater footage   #MISSING: get from Jack
hours.underwater.gn='xx'  #total number of hours of gillnet underwater footage
hours.subsurface='xx'  #total number of hours of subsurface footage
hours.deck2.ll='xx'  #total number of hours of deck camera 2 longline footage
hours.deck2.gn='xx'  #total number of hours of deck camera 2 gillnet footage

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
Hook.combos<-Hook.combos%>%
  rename(sheet_no="Sheet no",
         baiting.time='Baiting time (min)',
         baiting.crew='Baiting staff no',
         hooks.baited='no of baited hooks',
         hooks.deployed='Total deployed')%>%
  filter(!is.na(sheet_no))%>%
  mutate_at(c("C10/W","C12/W","C14/W",
              "Eb10/W","Eb12/W","Eb14/W",
              "C10/M","C12/M","C14/M",
              "Eb10/M","Eb12/M","Eb14/M"), as.numeric)%>%
  data.frame


#---------Basic manipulation of PA observer data------------
DATA=DATA[grep("PA", DATA$SHEET_NO), ]%>%
      filter(year>=2020)%>%
      rename(IDL=TrunkL)%>%
  dplyr::select(SHEET_NO,LINE_NO,Mid.Lat,Mid.Long,Lat.round,Long.round,zone,date,Day,Month,year,BOAT,BLOCK,SKIPPER,BOTDEPTH,
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

#---------Add number of hook combos to PA observer data------------
DATA=DATA%>% 
  left_join(Hook.combos%>%
              dplyr::select(-c(Date,baiting.time,baiting.crew,hooks.baited,Comments)),by='sheet_no')%>%
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



#---------Explore PA observer data------------
if(explr.dat.entry)
{
  setwd('C:/Matias/Analyses/Parks Australia/fix this')
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
    dplyr::select(sheet_no,species,method,line_no,n.hooks,hooktype,hooksize,wiretrace)
  if(nrow(LL.issues)>0) write.csv(LL.issues,'LL.issues.csv',row.names = F)
  
  GN.issues=DATA%>%filter(method=="GN")%>%
    filter(is.na(net_length) | net_length<3 | net_length>8)%>%
    distinct(sheet_no,.keep_all = T)
  if(nrow(GN.issues)>0) write.csv(GN.issues,'GN.issues.csv',row.names = F)
  
  
  a=subset(DATA,method=="LL")%>%
    dplyr::select(sheet_no,line_no,n.hooks,hooks.deployed)%>%
    mutate(dummy=paste(sheet_no,line_no))%>%
    distinct(dummy,.keep_all = T)%>%
    dplyr::select(-dummy)
  a$keep=a$n.hooks==a$hooks.deployed
  a=subset(a,keep=="FALSE")
  if(nrow(a)>0) write.csv(a,'N_hooks.different_to_hooks.deployed.csv',row.names = F)
}

#---------Catch analyses of historic data (commercial catch and effort) ------------    
if(do.Historic)  
{
  library(fields)
  library(gridExtra)
  library(grid)
  library(data.table)
  library(plotrix)
  source("C:/Matias/Analyses/SOURCE_SCRIPTS/Git_other/Plot.Map.R")
  source("C:/Matias/Analyses/SOURCE_SCRIPTS/Git_other/Smart_par.R")
  source("C:/Matias/Analyses/SOURCE_SCRIPTS/Git_Population.dynamics/fn.fig.R")
  Do.tiff="YES"
  Do.jpeg="NO"
  hndl="C:/Matias/Analyses/Parks Australia/outputs/Historic_catch_effort"
  
  setwd('C:/Matias/Analyses/Data_outs')
  Data.daily.original=fread("Data.daily.original.csv",data.table=FALSE)
  Data.daily=fread("Data.daily.csv",data.table=FALSE)
  Effort.daily=fread("Effort.daily.csv",data.table=FALSE)
  Data.monthly=fread("Data.monthly.csv",data.table=FALSE)
  Effort.monthly=fread("Effort.monthly.csv",data.table=FALSE)
  
  List.of.species=Data.daily%>%distinct(SPECIES,SNAME)%>%arrange(SPECIES)
  TARGETS=list(whiskery=17003,gummy=17001,dusky=18003,sandbar=18007) 
  Scalefish.species=188000:599001
  Current.yr="2018-19" 
  
  #Power analysis
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
  
  #Some manipulations
  Data.monthly=Data.monthly%>%
    filter(TYPE.DATA=='monthly')%>%
    mutate(SNAME=tolower(SNAME),
           SNAME=case_when(SPECIES== 10001~ 'shark, shortfin mako',
                           SPECIES== 13000~ 'shark, wobbegong',
                           SPECIES== 17001~ 'shark, gummy',
                           SPECIES== 17003~ 'shark, whiskery',
                           SPECIES== 18001~ 'shark, copper',
                           SPECIES== 18003~ 'shark, dusky',
                           SPECIES== 18021~ 'shark, bull',
                           SPECIES== 23002~ 'shark, common saw',
                           SPECIES== 311060~ 'cod, grey banded',
                           SPECIES== 311152~ 'cod, 8-bar',
                           SPECIES== 320000~ 'west australian dhufish',
                           SPECIES== 335001~ 'cobia',
                           SPECIES== 346002~ 'goldband snapper',
                           SPECIES== 350000~ 'sweetlip',
                           SPECIES== 351006~ 'emperor,blue-lined',
                           SPECIES== 353001~ 'pink snapper',
                           SPECIES== 353901~ 'bream, mixed',
                           SPECIES== 353998~ 'bream, silver',
                           SPECIES== 363001~ 'butterfish',
                           SPECIES== 367000~ 'boarfishes',
                           SPECIES== 367003~ 'long-snouted boarfish',
                           SPECIES== 377004~ 'blue morwong',
                           SPECIES== 384002~ 'western blue groper',
                           SPECIES== 441018~ 'mackerel, grey',
                           SPECIES== 441025~ 'mackerel, shark',
                           SPECIES== 702003~ 'crab, blue manna',
                           SPECIES== 702009~ 'crab, champagne',
                           TRUE~SNAME))
  
  Data.daily=Data.daily%>%
        mutate(SNAME=tolower(SNAME),
               SNAME=case_when(SPECIES== 10001~ 'shark, shortfin mako',
                               SPECIES== 13000~ 'shark, wobbegong',
                               SPECIES== 17001~ 'shark, gummy',
                               SPECIES== 17003~ 'shark, whiskery',
                               SPECIES== 18001~ 'shark, copper',
                               SPECIES== 18003~ 'shark, dusky',
                               SPECIES== 18021~ 'shark, bull',
                               SPECIES== 23002~ 'shark, common saw',
                               SPECIES== 311060~ 'cod, grey banded',
                               SPECIES== 311152~ 'cod, 8-bar',
                               SPECIES== 320000~ 'west australian dhufish',
                               SPECIES== 335001~ 'cobia',
                               SPECIES== 346002~ 'goldband snapper',
                               SPECIES== 350000~ 'sweetlip',
                               SPECIES== 351006~ 'emperor,blue-lined',
                               SPECIES== 353001~ 'pink snapper',
                               SPECIES== 353901~ 'bream, mixed',
                               SPECIES== 353998~ 'bream, silver',
                               SPECIES== 363001~ 'butterfish',
                               SPECIES== 367000~ 'boarfishes',
                               SPECIES== 367003~ 'long-snouted boarfish',
                               SPECIES== 377004~ 'blue morwong',
                               SPECIES== 384002~ 'western blue groper',
                               SPECIES== 441018~ 'mackerel, grey',
                               SPECIES== 441025~ 'mackerel, shark',
                               SPECIES== 702003~ 'crab, blue manna',
                               SPECIES== 702009~ 'crab, champagne',
                               TRUE~SNAME))
  
  
  
  #Catch composition GN and LL
    #Overall (show 20 top species, aggregate the rest)
  fn.br.plt=function(dd,TOP,yMx,CX.nm)
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
    with(dd,barplot(Prop,horiz = T,col=colr,names.arg=new.sp,cex.names=CX.nm,las=2))
    box()
  }
  fn.ktch.comp=function(ktch,what,Min.overlap)
  {
     
    #Overall comparison GN vs LL
    dat=ktch%>%group_by(METHOD,SPECIES,SNAME)%>%
      summarise(Total = sum(LIVEWT.c))
    fn.fig(paste(hndl,"/Catch_composition/Overall_GN_LL_overall",what,sep=""),2400,1600)
    par(mfcol=c(1,2),mar=c(2,9,1,.1),oma=c(1,.1,.5,.3),mgp=c(1.5,.5,0),cex.axis=.9)
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
    fn.fig(paste(hndl,"/Catch_composition/Overall_GN_LL_by blk_yr_mn",what,sep=""),2400,1600)
    par(mfcol=c(length(unIk),2),mar=c(2,7,.1,.1),oma=c(1,.1,1.1,.3),mgp=c(1.5,.5,0))
    #GN    
    for(u in 1:length(unIk))
    {
      fn.br.plt(dd=dat%>%filter(METHOD=='GN' & dummy==unIk[u]),TOP=Top,yMx=1,CX.nm=CX.NM)
      if(u==1)mtext("Gillnet",3,cex=1)
      legend('bottomright',paste(unIk[u]," (",round(sum(subset(dat,METHOD=='GN'& dummy==unIk[u])$Total)/1000,1),
                              " tonnes)",sep=""),bty='n',cex=.8)
    }
    #LL
    for(u in 1:length(unIk))
    {
      fn.br.plt(dd=dat%>%filter(METHOD=='LL' & dummy==unIk[u]),TOP=Top,yMx=1,CX.nm=CX.NM)
      if(u==1)mtext("Longline",3,cex=1)
      legend('bottomright',paste(unIk[u]," (",round(sum(subset(dat,METHOD=='LL'& dummy==unIk[u])$Total)/1000,1),
                              " tonnes)",sep=""),bty='n',cex=.8)
    }
    mtext("Proportion of total catch",1,outer=T,line=0,cex=1) 
    dev.off()
    
  }
    #daily
  fn.ktch.comp(ktch=Data.daily%>%filter(METHOD%in%c('GN',"LL"))%>%
                  dplyr::select(Same.return.SNo,FINYEAR,METHOD,zone,block10,BLOCKX,SPECIES,SNAME,LIVEWT.c,MONTH),
               what="_Daily",
               Min.overlap=10)
    #monthly
  fn.ktch.comp(ktch=Data.monthly%>%filter(METHOD%in%c('GN',"LL"))%>%
                    dplyr::select(Same.return,FINYEAR,METHOD,zone,BLOCKX,SPECIES,SNAME,LIVEWT.c,MONTH),
               what="_Monthly",
               Min.overlap=18)

  #Hook info
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
  d1$hours.c=d1$hours
  d1$shots.c=d1$shots
  
  fn.fig(paste(hndl,"/longline.number of hooks",sep=""),2400,1600)
  rbind(d%>%dplyr::select(dat,hooks),d1%>%dplyr::select(dat,hooks)) %>% 
    gather(key=dat, value=hooks) %>% 
    ggplot(aes(x=hooks,fill=dat)) + 
    geom_histogram(position="dodge",binwidth=100)+
    scale_fill_manual(values=c("darksalmon", "steelblue"))+
    xlab('Number of hooks')+ylab('Frequency')+ guides(fill=guide_legend(title="Data set"))+
    theme_grey(base_size = 22)
  dev.off()
  
  fn.fig(paste(hndl,"/longline.soak times",sep=""),2400,1600)
  rbind(d%>%dplyr::select(dat,hours.c),d1%>%dplyr::select(dat,hours.c)) %>% 
    gather(key=dat, value=hours.c) %>% 
    ggplot(aes(x=hours.c,fill=dat)) + 
    geom_histogram(position="dodge",binwidth=1)+
    scale_fill_manual(values=c("darksalmon", "steelblue"))+
    xlab('Soak hours')+ylab('Frequency')+ guides(fill=guide_legend(title="Data set"))+
    theme_grey(base_size = 22)
  dev.off()
  
  fn.fig(paste(hndl,"/longline.hook size_daily only",sep=""),2400,1600)
  subset(d1,hooksize>0) %>%
    ggplot(aes(x=hooksize)) + 
    geom_histogram(position="dodge",binwidth=1,fill="darksalmon")+
    xlab('Hook size')+ylab('Frequency')+
    scale_x_continuous(breaks=6:15,
                       labels=6:15)+
    theme_grey(base_size = 22)
  dev.off()
  
  fn.fig(paste(hndl,"/longline.hook type_daily only",sep=""),2400,1600)
  ggplot(subset(d1,!is.na(hooktype)), aes(hooktype)) + geom_bar(fill="darksalmon")+
    theme_grey(base_size = 22)+ylab('Frequency')
  dev.off()
  
  
  #Tim Nicholas request
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
  
  #Average catch price per shot
  a= Data.daily%>%
    filter(FINYEAR=="2017-18" & METHOD%in%c("GN","LL"))%>%
    group_by(Same.return.SNo,VESSEL,SPECIES)%>%
    summarize(Total.ktch = sum(LIVEWT.c))
  setwd("C:/Matias/Analyses/Data_outs")
  PRICES=read.csv("PRICES.csv",stringsAsFactors = F)
  PRICES=PRICES%>%mutate(dolar.per.kg=PRICES[,match('Processor.Weighted.Average.Price',names(PRICES))],
                         dolar.per.kg=as.numeric(gsub("\\$", "", dolar.per.kg)))%>%
    mutate(SPECIES=as.numeric(ASA.Species.Code))
  a=left_join(a,PRICES,by="SPECIES")%>%
    mutate(ktch.price=dolar.per.kg*Total.ktch)%>%
    group_by(Same.return.SNo,VESSEL)%>%
    summarize(Catch.value=sum(ktch.price))
  
  b=Effort.daily%>%filter(finyear=="2017-18" & method%in%c("GN","LL"))%>%
    group_by(Same.return.SNo,vessel)%>%
    summarize(Effort = max(Km.Gillnet.Hours.c))
  
  d=left_join(a,b,by=c("Same.return.SNo"))%>%
    group_by(vessel)%>%
    summarise(average.ktch.value=mean(Catch.value,na.rm=T),
              min.ktch.value=min(Catch.value,na.rm=T),
              max.ktch.value=max(Catch.value,na.rm=T),
              average.effort=mean(Effort,na.rm=T))%>%
    data.frame
  write.csv(d,paste(hndl,"/Average catch price per shot.csv",sep=""),row.names = F)
  
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

#---------Build length-length relationships------------     
#using robust regression to deal with outliers
if(do.len_len)
{
  setwd('C:/Matias/Analyses/Parks Australia/outputs/length_length')
  r2ww <- function(x)
  {
    SSe <- sum(x$w*(x$resid)^2)
    observed <- x$resid+x$fitted
    SSt <- sum(x$w*(observed-weighted.mean(observed,x$w))^2)
    value <- 1-SSe/SSt;
    return(value);
  }
  
  ggplotRegression <- function(dat, xvar, yvar,tolerance=.5)
  {
    dat=dat%>%filter(!is.na((!!sym(xvar))) & !is.na((!!sym(yvar))))
    fml <- as.formula(paste(yvar, "~", xvar))
    fit <- rlm(fml, dat)   #robust regression
    d1=dat%>%
      mutate(pred=predict(fit),
             delta=abs(pred-(!!sym(yvar))))%>%
      filter(delta>tolerance*sd(d1$delta))
    p=dat%>%
      ggplot(aes_string(xvar,yvar,label = 'sheet_no'))+geom_point(na.rm= TRUE,colour='black')+
      geom_text_repel(data=  d1,size=3,na.rm= TRUE,segment.alpha=.4)+
      stat_smooth(method = "lm", col = "red")+
      labs(title = paste("Adj.R2= ",round(r2ww(fit),2),
                         "Inter.=",signif(fit$coef[[1]],5),
                         " Slope=",signif(fit$coef[[2]], 5)))+
      theme(plot.title = element_text(size=10))
    
    return(p)
  }
  fn.idl=function(SP)
  {
    d=DATA%>%filter(species==SP)
    TL.FL=ggplotRegression(d, "tl", "fl")
    FL.TL=ggplotRegression(d, "fl", "tl")
    idl.TL=ggplotRegression(d, "idl", "tl")
    idl.FL=ggplotRegression(d, "idl", "fl")
    ggarrange(TL.FL, idl.TL, FL.TL, idl.FL,ncol = 2, nrow = 2)
    ggsave(paste(unique(d$common_name),".tiff",sep=''), width = 8,height = 8, dpi = 300,
           compression = "lzw")
  }
  idl.species=table(DATA$species,1000*round(DATA$idl/1000))
  idl.species=rownames(idl.species)[which(idl.species>5)]
  for(i in 1:length(idl.species)) fn.idl(SP=idl.species[i])
}



#---------Manipulate PA Observer and underwater cameras data ------------ 
#Observer data  
DATA_PA=DATA%>%
  distinct(sheet_no,.keep_all=TRUE)%>%
  dplyr::select(sheet_no,mid.lat,mid.long,date,day,month,year,
                boat,block,skipper,botdepth,set.time,set.time.end,haul.time,haul.time.end,
                set.time.avg,haul.time.avg,soak.time,method,mesh_size,net_length,n.hooks)

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
              mutate(SP.group=case_when(Code >=3.7e7 & Code<=3.7024e7 ~"Sharks",
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
              mutate(SP.group=case_when(Code >=3.7e7 & Code<=3.7024e7 ~"Sharks",
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

Video.net.maxN=Video.net.maxN%>%
              mutate(SP.group=case_when(Code >=3.7e7 & Code<=3.7024e7 ~"Sharks",
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
                mutate(SP.group=case_when(Code >=3.7e7 & Code<=3.7024e7 ~"Sharks",
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


#---------General tables and plots of PA underwater Video ------------ 
HNDL='C:/Matias/Analyses/Parks Australia/outputs/'
le.paste=function(x) paste(HNDL,x,sep='')

SP.group.levels=c("Invertebrates","Scalefish","Sharks","Rays","Marine mammals","Seabirds")
TEP.groups=c("Marine mammals","Seabirds","Reptiles")

  #number of events
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
ggsave(le.paste("Video/underwater/Interactions_number.events_sqrt.transf.tiff"),width = 12,height = 8,compression = "lzw")


  #number of events for main target species
these.codes=c(37017001,37017003,37018001,37018003,37018007,37018023,37019004,
              37353001,37320004,37384002,37377004,37384039)
dummy=data.frame(Code=these.codes,
                 Names=All.species.names[match(these.codes,All.species.names$Code),"COMMON_NAME"])

colfunc <- colorRampPalette(c("orange","firebrick1", "yellow"))
n.col.elasmos=colfunc(length(these.codes[these.codes<37300000]))
colfunc <- colorRampPalette(c("lightblue1", "dodgerblue4"))
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



#---------Dropouts, Gaffing & Composition around weight or float ---------
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
p=Video.camera2.deck%>%
  mutate(water.column=tolower(hook.distance.to.float.weight))%>%
  filter(!is.na(water.column))%>%
  group_by(Code,water.column,Period)%>%
  tally()%>%
  mutate(water.column=factor(water.column),
         Period=capitalize(Period))%>%
  left_join(All.species.names%>%dplyr::select(COMMON_NAME,Code),by="Code")

these.compo.sp=sort(unique(p$Code))
names(these.compo.sp)=All.species.names[match(these.compo.sp,All.species.names$Code),"COMMON_NAME"]


p%>%mutate(COMMON_NAME=factor(COMMON_NAME,levels=names(these.compo.sp)))%>%
  ggplot(aes(fill=water.column, y=n, x=COMMON_NAME)) + 
  geom_bar(position="stack", stat="identity")+
  coord_flip() +
  facet_wrap(~Period,dir='h',scales='free_x')+ 
  theme(legend.position = "top",
        strip.text = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16))+
  scale_fill_manual(values=c("orange","firebrick2","firebrick4",
                             "lightblue2","deepskyblue2","dodgerblue4"))+
  xlab('')+ylab('Number of events')+ guides(color = guide_legend(nrow = 1))
ggsave(le.paste("Video/deck.cameras/Weight_float_species.events.tiff"),width = 10,height = 8,compression = "lzw")


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



# 2.3.1 Deck 1  (pointing to deck)                #MISSING
#       Video.camera1.deck,Video.camera1.deck_extra.records            
#     Note that Video.camera1.deck,Video.camera1.deck_extra.records must be manipulated

# 2.3.2 Deck 2  (pointing to roller)
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
    rbind(d.nets)%>%
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

