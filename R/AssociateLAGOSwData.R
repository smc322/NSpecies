##want to associate data from LAGOS to WI and IA data - match lakes with lagoslakeid, then associate with covariates of choice (see details below)

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

###geo variables to include per SMC discussion with EHS on 31 Oct 17 -- NO3 and TN depo, Runoff, Baseflow (all of these at hu8), chla (to indicate how green the lake is, could also add P or secchi in future?), IWS level LULC for urban (sum), wetland (sum), ag (row crop and pasture separate), forest (sum), open water, lake depth, LA:WA, connectivity class.
#mean/median year for WI data is 2000/2001 so used 2000 dep data and 2001 nlcd

hu.8id<-locus[,c(1,13)]

wi.lakeid.hu8<-merge(wi.lagoslakeid, hu.8id, all.x=T, all.y=F)

hu8.chag<-lagos$hu8.chag
hu8.chag.rel<-hu8.chag[,c(1,4, 20, 68, 100)]

wi.chag<-merge(wi.lakeid.hu8, hu8.chag.rel, by="hu8_zoneid", all.x=T, all.y=F)

iws.lulc<-lagos$iws.lulc
iws.lulc.rel<-iws.lulc[,c(52, 54, 56, 58, 60, 64, 66, 68, 74, 76, 78, 80, 156)]
iws.lulc.rel$urban<-iws.lulc.rel$iws_nlcd2001_pct_21+iws.lulc.rel$iws_nlcd2001_pct_22+iws.lulc.rel$iws_nlcd2001_pct_23+iws.lulc.rel$iws_nlcd2001_pct_24
iws.lulc.rel$rowcrop<-iws.lulc.rel$iws_nlcd2001_pct_82
iws.lulc.rel$pasture<-iws.lulc.rel$iws_nlcd2001_pct_81
iws.lulc.rel$forest<-iws.lulc.rel$iws_nlcd2001_pct_41+iws.lulc.rel$iws_nlcd2001_pct_42+iws.lulc.rel$iws_nlcd2001_pct_43
iws.lulc.rel$wetland<-iws.lulc.rel$iws_nlcd2001_pct_90+iws.lulc.rel$iws_nlcd2001_pct_95
iws.lulc.rel$openh20<-iws.lulc.rel$iws_nlcd2001_pct_11
  
iws.lulc.rel.keep<-iws.lulc.rel[,c(13:19)]
iws.lulc.rel.keep.nona<-na.omit(iws.lulc.rel.keep)

wi.chag.lulc<-merge(wi.chag, iws.lulc.rel.keep.nona, by="lagoslakeid", all.x=T, all.y=F)

##note NAs -- trout bog is only ~1ha so is not part of the ~50,000 lakes in the IWS dataset.  Could add HU12 lulc data if needed?

area<-locus[,c(1,6)]
wsarea<-lagos$iws[,c(2,12)]
maxd<-lagos$lakes_limno[,c(1,6)]
connclass<-lagos$lakes.geo[,c(1,31)]

lakewsa<-merge(area, wsarea, by="lagoslakeid", all.x=T, all.y=T)
areadepth<-merge(lakewsa, maxd, by="lagoslakeid", all.x=T, all.y=T)
areadepthconn<-merge(areadepth, connclass, by="lagoslakeid", all.x=T, all.y=T)
areadepthconn$la_wa_ratio<-areadepthconn$lake_area_ha/areadepthconn$iws_ha

chla<-lagos$epi_nutr[,c(2,7,92)]
chla.80<-na.omit(chla[chla$sampleyear>1979,])

annualmed = aggregate(chla.80$chla, chla.80[,c("lagoslakeid", "sampleyear")], FUN=function(x) c(median=median(x)))
lakemed = aggregate(annualmed$x, by=list(annualmed$lagoslakeid), FUN=function(x) c(median=median(x)))
names(lakemed)<-c("lagoslakeid", "median_chla")

areadepthconnchla<-merge(areadepthconn, lakemed, by="lagoslakeid", all.x=T, all.y=T)

local.covars<-areadepthconnchla[,c(1,4,5,6,7)]

wi.chag.lulc.local<-merge(wi.chag.lulc, local.covars, by="lagoslakeid", all.x=T, all.y=F)

##wi.chag.lulc.local is mostly what we want, with a couple of issues --- 4861 NAs for some variables because we don't have a lagoslakeid for crystal bog, and 13,154 NAs for some variables because trout bog is less than 4ha and excluded from IWS calculations for LAGOS (13154 should be sum of trout bog missing data and crystal bog missing data).  otherwise the data should be complete.  save as RDS here and explain issues to Erin and see if she has any other needs.

saveRDS(wi.chag.lulc.local, file="Data/WIwithCovariates_3Nov2017.rds")


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



