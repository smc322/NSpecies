##want to associate data 

wi<- readRDS(file="Data/LTERNitrogenData.rds")
ia<-readRDS(file="Data/IowaNitrogenData.rds")

library(LAGOS)
lagos<-lagos_load(version="1.087.1")

locus<-lagos$locus
epinut<-lagos$epi_nutr

##LTER lakes
summary(wi$lakeid)

#AL    BM    CB    CR    FI    ME    MO    SP    TB    TR    WI  - these abbreviations are for 
# AL = Allequash Lake
# BM = Big Muskellunge Lake
# CB = Bog 27-2 (Crystal Bog)
# CR = Crystal Lake
# FI = Fish Lake
# ME = Lake Mendota
# MO = Lake Monona
# SP = Sparkling Lake
# TB = Bog 12-15 (Trout Bog)
# TR = Trout Lake
# WI = Lake Wingra
# LR = Little Rock
# KE = Kegonsa
# WA = Waubesa


lter<-epinut[epinut$programname=="WI_LTER_NUTRIENTS",]
lakes.limno<-lagos$lakes_limno
lter.names<-merge(lter, lakes.limno, by="lagoslakeid", all.x=T, all.y=F)
lter.names.short<-lter.names[,c(1,95)]
oneeach<-lter.names.short[!duplicated(lter.names.short$lagosname1),]

#LAGOSLAKEIDS:
# lagoslakeid           lagosname1
#             827          LAKE WINGRA
#          906       SPARKLING LAKE
#         2746            FISH LAKE
#         4559          LAKE MONONA
#     4625       ALLEQUASH LAKE
#       4664           TROUT LAKE
#     4722         CRYSTAL LAKE
#     5248 BIG MUSKELLUNGE LAKE
#     5371         LAKE MENDOTA
#     120948            TROUT BOG

#there seems to be no lakeid for crystal bog, so that would have to be a loss?
#how many samples are for cb? less than 5%, and all are NAs for SLOH data, so that's probably not terribly important

lterlakeabbrevs<-c("AL", "BM", "CR", "FI","ME", "MO", "SP", "TB", "TR", "WI")
lterlakelagosids<-c(4625, 5248, 4722, 2746, 5371, 4559, 906, 120948, 4664, 827)

lter.codes<-data.frame(lterlakeabbrevs, lterlakelagosids)
names(lter.codes) = c("lakeid", "lagoslakeid")

wi.lagoslakeid<-merge(wi, lter.codes, by="lakeid", all.x=T, all.y=T)

###geo variables to include -- rowcrop iws, pasture iws, no3 deposition, 


##iowa mess, continue this later
ialakenames<-unique(ia$lakename)
write.csv(ialakenames, file="iowalakenames.csv")

iowalocus<-locus[locus$state_zoneid=="State_13",]

oneofeachlake<-iowalocus[!duplicated(iowalocus$gnis_name),]
write.csv(oneofeachlake, file="lagosiowalakes.csv")


county<-lagos$county
iowacounty<-county[county$county_state=="IA",]

limno<-lagos$lakes_limno
morenames<-merge(iowalocus, limno, by="lagoslakeid", all.x=T, all.y=F)
oneofeachnew<-morenames[!duplicated(morenames$lagosname1),]

blackhawk<-morenames[morenames$county_zoneid=="County_230",]
relevant.bh<-blackhawk[,c(1,3,4,5,6,19,20,21,22,23)]


pow<-morenames[morenames$county_zoneid=="County_254",]



