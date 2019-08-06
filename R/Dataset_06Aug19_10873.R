#want to make file for Erin with data as follows:
#concurrent observations of either NH4+NO3+TKN or NH4+NO3+TN
#late summer (jul/aug/sept) data 1980 and after
#covariates as discussed by SMC/EHS
#get rid of ridiculous detection limits (above 50 for NH4 and NO3, above 200 for TN and TKN)

###per June/July updates --
##pick a fake DL for data that don't have one (tbd here)
##only pick most recent single observation per lake

###per Dec update --
##want more than one obs per lake because model too flexible per Erin. So same as july but keep 2 obs per  lake or 3 obs per lake. perhaps build in max time btwn obs (5 year?) Erin prefers reps as cols so we still have a single row per lake so melt as needed.


library(LAGOSNE)
library(dplyr)
library(tidyr)
lagos<-lagosne_load(version="1.087.3")

locus<-lagos$locus
epinut<-lagos$epi_nutr

data.tn<-epinut[,c("lagoslakeid", "nh4", "no2no3", "tn", "nh4_detectionlimit", "no2no3_detectionlimit", "tn_detectionlimit", "sampledate")]
data.tkn<-epinut[,c("lagoslakeid", "nh4", "no2no3", "tkn", "nh4_detectionlimit", "no2no3_detectionlimit", "tkn_detectionlimit", "sampledate")]

tn.complete<-data.tn[!with(data.tn, is.na(nh4)|is.na(no2no3)|is.na(tn)),]
tn.complete$tkn=NA
tn.complete$tkn_detectionlimit=NA

##make new col with fake DL. Looked at medians for these data and eyeballed KEW's dot plot for DL's for SoL and picked something mid ballpark - 100 for TN and TKN, and 20 for NO3 and NH4. There is a TON of variation around these, perhaps do some sensitivity analysis in the future.  Do this for TN and TKN seprately in these two datasets, then for NH4 and NO3 once merged below.

for (i in 1:nrow(tn.complete)){
  if (is.na(tn.complete$tn_detectionlimit[i]) == TRUE) {
    tn.complete$tn_dl_all[i] = 100
  } else {
    tn.complete$tn_dl_all[i] = tn.complete$tn_detectionlimit[i]
  }
}

tn.complete$tkn_dl_all=NA

tkn.complete<-data.tkn[!with(data.tkn, is.na(nh4)|is.na(no2no3)|is.na(tkn)),]
tkn.complete$tn=NA
tkn.complete$tn_detectionlimit=NA

for (i in 1:nrow(tkn.complete)){
  if (is.na(tkn.complete$tkn_detectionlimit[i]) == TRUE) {
    tkn.complete$tkn_dl_all[i] = 100
  } else {
    tkn.complete$tkn_dl_all[i] = tkn.complete$tkn_detectionlimit[i]
  }
}

tkn.complete$tn_dl_all=NA

tnortkn<-rbind(tn.complete, tkn.complete)

sampleyearmonths<- tnortkn %>% separate(sampledate, sep="-", into=c("sampleyear", "samplemonth", "sampleday"))

sampleyearmonths$sampleday=NULL

#limit to data 1980 or after (not a lot of data loss) and only in july/aug/sept (lose just under half of data)
data.80<-sampleyearmonths[sampleyearmonths$sampleyear>1979,]
data.80$samplemonth<-as.numeric(data.80$samplemonth)
data.80.jas<-data.80[data.80$samplemonth == 7 | data.80$samplemonth == 8 | data.80$samplemonth == 9,]

#get rid of ridiculous detection limits - Sarah and Emily looked at histograms and determined >50 for NO3/Nh4 and greater than 200 for TN and TKN

nrdl<-filter(data.80.jas, nh4_detectionlimit<=50 |is.na(nh4_detectionlimit))
nrdl2<-filter(nrdl, no2no3_detectionlimit<=50|is.na(no2no3_detectionlimit))
nrdl3<-filter(nrdl2, tn_detectionlimit<=200|is.na(tn_detectionlimit))
nrdl4<-filter(nrdl3, tkn_detectionlimit<=200|is.na(tkn_detectionlimit))

##make new col with fake DL. Looked at medians for these data and eyeballed KEW's dot plot for DL's for SoL and picked something mid ballpark - 100 for TN and TKN, and 20 for NO3 and NH4. There is a TON of variation around these, perhaps do some sensitivity analysis in the future.


for (i in 1:nrow(nrdl4)){
  if (is.na(nrdl4$nh4_detectionlimit[i]) == TRUE) {
    nrdl4$nh4_dl_all[i] = 20
  } else {
    nrdl4$nh4_dl_all[i] = nrdl4$nh4_detectionlimit[i]
  }
}


for (i in 1:nrow(nrdl4)){
  if (is.na(nrdl4$no2no3_detectionlimit[i]) == TRUE) {
    nrdl4$no3_dl_all[i] = 20
  } else {
    nrdl4$no3_dl_all[i] = nrdl4$no2no3_detectionlimit[i]
  }
}


#geopredictors

hu8.chag<-lagos$hu8.chag
hu8.chag.rel<-hu8.chag[,c(1,4, 20, 68, 100)]

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

area<-locus[,c(1,6)]
wsarea<-lagos$iws[,c(2,12)]
maxd<-lagos$lakes_limno[,c(1,8)]
connclass<-lagos$lakes.geo[,c(1,31)]

lakewsa<-merge(area, wsarea, by="lagoslakeid", all.x=T, all.y=T)
areadepth<-merge(lakewsa, maxd, by="lagoslakeid", all.x=T, all.y=T)
areadepthconn<-merge(areadepth, connclass, by="lagoslakeid", all.x=T, all.y=T)
areadepthconn$la_wa_ratio<-areadepthconn$lake_area_ha/areadepthconn$iws_ha


#combine predictors to merge w n
hu.8id<-locus[,c(1,13)]
chag.id<-merge(hu8.chag.rel, hu.8id, by="hu8_zoneid", all.x=T, all.y=T)

chag.lulc<-merge(chag.id, iws.lulc.rel.keep.nona, by="lagoslakeid", all.x=T, all.y=T)

chag.lulc.lake<-merge(chag.lulc, areadepthconn, by="lagoslakeid", all.x=T, all.y=T)

#get rid of hu8id, lake area, watershed area
chag.lulc.lake$iws_ha=NULL
chag.lulc.lake$hu8_zoneid=NULL
names(chag.lulc.lake) <- c("lagoslakeid","hu8_baseflow",   "hu8_no3depo",    "hu8_totalndepo", "hu8_runoff", "urban", "rowcrop", "pasture", "forest", "wetland", "openh20", "lake_area", "maxdepth", "lakeconnection", "la_wa_ratio" )

data.n.geo<-merge(nrdl4, chag.lulc.lake, by="lagoslakeid", all.x=T, all.y=F)
# setwd("/Users/SarahiMac/Documents/LocalGitProjects/NSpecies/NSpecies/Data")
# write.csv(data.n.geo, file="LagosNSpeciesData_10July2018.csv")

#add latlong
ll.l<-lagos$locus[,c(1,4,5)]
data.n.geo.ll<-merge(data.n.geo, ll.l, by="lagoslakeid", all.x=T, all.y=F)


##pick out last two of each lagoslakeid observations
library(data.table)
orderedidyr<-data.n.geo.ll[order( data.n.geo.ll[,1], data.n.geo.ll[,8]),]
pickone<-data.table(orderedidyr)
setkey(pickone, lagoslakeid)
oneobs<-pickone[J(unique(lagoslakeid)), mult="last"]

excludinglast<-anti_join(data.n.geo.ll, oneobs, by = c("lagoslakeid", "nh4", "no2no3", "tn", "tkn", "sampleyear", "samplemonth"))

elordered<-excludinglast[order(excludinglast[,1], excludinglast[,8]),]
picktwo<-data.table(elordered)
setkey(picktwo, lagoslakeid)
twoobs<-picktwo[J(unique(lagoslakeid)), mult="last"]

two.rel<-twoobs[,c(1:15)]

names(two.rel)<-c("lagoslakeid", "nh4_2", "no2no3_2", "tn_2", "nh4_detectionlimit_2", "no2no3_detectionlimit_2", "tn_detectionlimit_2", "sampleyear_2", "samplemonth_2", "tkn_2", "tkn_detectionlimit_2", "tn_dl_all_2", "tkn_dl_all_2", "nh4_dl_all_2", "no3_dl_all_2")


twoperlake<-right_join(oneobs, two.rel, by="lagoslakeid")

setwd("~/Dropbox/Sarah_Work/Manuscripts/2019_NSpecies/Data")
write.csv(twoperlake, file="LagosNSpeciesData_latlong_06Aug19.csv")


