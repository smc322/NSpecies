#want to make file for Erin with data as follows:
#concurrent observations of either NH4+NO3+TKN or NH4+NO3+TN
#late summer (jul/aug/sept) data 1980 and after
#covariates as discussed by SMC/EHS
#get rid of ridiculous detection limits (above 50 for NH4 and NO3, above 200 for TN and TKN)


library(LAGOS)
library(dplyr)
lagos<-lagos_load(version="1.087.1")

locus<-lagos$locus
# laptop version use 
#epinut<-lagos$epi.nutr
#desktop use
epinut<-lagos$epi_nutr

data.tn<-lagos$epi_nutr[,c(2,12,14,19, 63, 65, 71 ,92,93)]

data.tkn<-lagos$epi_nutr[,c(2,12,14,18, 63, 65, 70 ,92,93)]

tn.complete<-data.tn[!with(data.tn, is.na(nh4)|is.na(no2no3)|is.na(tn)),]
tn.complete$tkn=NA
tn.complete$tkn_detectionlimit=NA

tkn.complete<-data.tkn[!with(data.tkn, is.na(nh4)|is.na(no2no3)|is.na(tkn)),]
tkn.complete$tn=NA
tkn.complete$tn_detectionlimit=NA

tnortkn<-rbind(tn.complete, tkn.complete)

#limit to data 1980 or after (not a lot of data loss) and only in july/aug/sept (lose just under half of data)
data.80<-tnortkn[tnortkn$sampleyear>1979,]
data.80.jas<-data.80[data.80$samplemonth == 7 | data.80$samplemonth == 8 | data.80$samplemonth == 9,]

#get rid of ridiculous detection limits - Sarah and Emily looked at histograms and determined >50 for NO3/Nh4 and greater than 200 for TN and TKN

nrdl<-filter(data.80.jas, nh4_detectionlimit<=50 |is.na(nh4_detectionlimit))
nrdl2<-filter(nrdl, no2no3_detectionlimit<=50|is.na(no2no3_detectionlimit))
nrdl3<-filter(nrdl2, tn_detectionlimit<=200|is.na(tn_detectionlimit))
nrdl4<-filter(nrdl3, tkn_detectionlimit<=200|is.na(tkn_detectionlimit))

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
maxd<-lagos$lakes_limno[,c(1,6)]
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
setwd("/Users/SarahiMac/Documents/LocalGitProjects/NSpecies/NSpecies/Data")
write.csv(data.n.geo, file="LagosNSpeciesData_08April2018.csv")

#add latlong
ll.l<-lagos$locus[,c(1,4,5)]
data.n.geo.ll<-merge(data.n.geo, ll.l, by="lagoslakeid", all.x=T, all.y=F)
setwd("~/Dropbox/Sarah_Work/Manuscripts/2018_NSpecies/Data")
write.csv(data.n.geo.ll, file="LagosNSpeciesData_latlong_04June2018.csv")



