## quick and dirty associate either TN or NO3 or NH4 data with lagos covariates to write ASLO abstract

library(LAGOS)
lagos<-lagos_load(version="1.087.1")

locus<-lagos$locus
# laptop version use 
#epinut<-lagos$epi.nutr
#desktop use
epinut<-lagos$epi_nutr

#summarize N data

#laptop
#nitrogen<-lagos$epi.nutr[,c(2,12,14,19,93,94)]
#deskotp
nitrogen<-lagos$epi_nutr[,c(2,12,14,18,19,92,93)]
nitrogen.80<-nitrogen[nitrogen$sampleyear>1979,]
nitrogen.80.ja<-nitrogen.80[nitrogen.80$samplemonth == 7 | nitrogen.80$samplemonth == 8,]
nitrogen.80.jas<-nitrogen.80[nitrogen.80$samplemonth == 7 | nitrogen.80$samplemonth == 8 | nitrogen.80$samplemonth == 9,]

tn<-na.omit(nitrogen.80.jas[,c(1,5,6)])
no3<-na.omit(nitrogen.80.jas[,c(1,3,6)])
nh4<-na.omit(nitrogen.80.jas[,c(1,2,6)])
tkn<-na.omit(nitrogen.80.jas[,c(1,4,6)])


annualmed.tn = aggregate(tn$tn, tn[,c("lagoslakeid", "sampleyear")], FUN=function(x) c(median=median(x)))
lakemed.tn = aggregate(annualmed.tn$x, by=list(annualmed.tn$lagoslakeid), FUN=function(x) c(median=median(x)))
names(lakemed.tn)<-c("lagoslakeid", "median_tn")

annualmed.no3 = aggregate(no3$no2no3, no3[,c("lagoslakeid", "sampleyear")], FUN=function(x) c(median=median(x)))
lakemed.no3 = aggregate(annualmed.no3$x, by=list(annualmed.no3$lagoslakeid), FUN=function(x) c(median=median(x)))
names(lakemed.no3)<-c("lagoslakeid", "median_no3")

annualmed.nh4 = aggregate(nh4$nh4, nh4[,c("lagoslakeid", "sampleyear")], FUN=function(x) c(median=median(x)))
lakemed.nh4 = aggregate(annualmed.nh4$x, by=list(annualmed.nh4$lagoslakeid), FUN=function(x) c(median=median(x)))
names(lakemed.nh4)<-c("lagoslakeid", "median_nh4")

annualmed.tkn = aggregate(tkn$tkn, tkn[,c("lagoslakeid", "sampleyear")], FUN=function(x) c(median=median(x)))
lakemed.tkn = aggregate(annualmed.tkn$x, by=list(annualmed.tkn$lagoslakeid), FUN=function(x) c(median=median(x)))
names(lakemed.tkn)<-c("lagoslakeid", "median_tkn")

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

nh4.covars<-na.omit(merge(chag.lulc.lake, lakemed.nh4, by="lagoslakeid", all.x=F, all.y=T))

no3.covars<-na.omit(merge(chag.lulc.lake, lakemed.no3, by="lagoslakeid", all.x=F, all.y=T))

tn.covars<-na.omit(merge(chag.lulc.lake, lakemed.tn, by="lagoslakeid", all.x=F, all.y=T))

tkn.covars<-na.omit(merge(chag.lulc.lake, lakemed.tkn, by="lagoslakeid", all.x=F, all.y=T))

#save files of each N species with covars

saveRDS(nh4.covars, file="Data/NH4_Covariates_feb2018.rds")
saveRDS(no3.covars, file="Data/NO3_Covariates_feb2018.rds")
saveRDS(tn.covars, file="Data/TN_Covariates_feb2018.rds")
saveRDS(tkn.covars, file="Data/TKN_Covariates_feb2018rds")

#run a quick random forest for each to get predictability and best predictors to make vague comments in abstract

library(randomForest)

nh4rf<-randomForest(median_nh4~hu8_baseflowindex_mean+hu8_dep_no3_2000_mean+hu8_dep_totaln_2000_mean+hu8_runoff_mean+urban+rowcrop+pasture+forest+wetland+openh20+lake_area_ha+maxdepth+la_wa_ratio, data=nh4.covars)

no3rf<-randomForest(median_no3~hu8_baseflowindex_mean+hu8_dep_no3_2000_mean+hu8_dep_totaln_2000_mean+hu8_runoff_mean+urban+rowcrop+pasture+forest+wetland+openh20+lake_area_ha+maxdepth+la_wa_ratio, data=no3.covars)

tnrf<-randomForest(median_tn~hu8_baseflowindex_mean+hu8_dep_no3_2000_mean+hu8_dep_totaln_2000_mean+hu8_runoff_mean+urban+rowcrop+pasture+forest+wetland+openh20+lake_area_ha+maxdepth+la_wa_ratio, data=tn.covars)


##make maps of each N species location 

library(maps)
library(mapdata)

latlong<-lagos$locus[,c(1,4,5)]

nh4.map<-merge(nh4.covars, latlong, by="lagoslakeid", all.x=T, all.y=F)

map("worldHires", "Canada", xlim=c(min(nh4.map$nhd_long, na.rm=TRUE)-1,max(nh4.map$nhd_long, na.rm=TRUE)+1), ylim=c(min(nh4.map$nhd_lat, na.rm=TRUE)-1,max(nh4.map$nhd_lat, na.rm=TRUE)+1), fill=TRUE, col="khaki", lwd=2, bg="lightblue1")
map("worldHires", "USa", add=TRUE,  fill=TRUE, col="khaki", lwd=2)
map("state", boundary = FALSE, add = TRUE)
points(nh4.map$nhd_long, nh4.map$nhd_lat, pch=19, col="black", cex=0.5)  
text(-78, 47, "NH4 - summer data lakes", cex=1.5)


no3.map<-merge(no3.covars, latlong, by="lagoslakeid", all.x=T, all.y=F)

map("worldHires", "Canada", xlim=c(min(no3.map$nhd_long, na.rm=TRUE)-1,max(no3.map$nhd_long, na.rm=TRUE)+1), ylim=c(min(no3.map$nhd_lat, na.rm=TRUE)-1,max(no3.map$nhd_lat, na.rm=TRUE)+1), fill=TRUE, col="khaki", lwd=2, bg="lightblue1")
map("worldHires", "USa", add=TRUE,  fill=TRUE, col="khaki", lwd=2)
map("state", boundary = FALSE, add = TRUE)
points(no3.map$nhd_long, no3.map$nhd_lat, pch=19, col="black", cex=0.5)  
text(-78, 47, "NO3 - summer data lakes", cex=1.5)

tn.map<-merge(tn.covars, latlong, by="lagoslakeid", all.x=T, all.y=F)

map("worldHires", "Canada", xlim=c(min(tn.map$nhd_long, na.rm=TRUE)-1,max(tn.map$nhd_long, na.rm=TRUE)+1), ylim=c(min(tn.map$nhd_lat, na.rm=TRUE)-1,max(tn.map$nhd_lat, na.rm=TRUE)+1), fill=TRUE, col="khaki", lwd=2, bg="lightblue1")
map("worldHires", "USa", add=TRUE,  fill=TRUE, col="khaki", lwd=2)
map("state", boundary = FALSE, add = TRUE)
points(tn.map$nhd_long, tn.map$nhd_lat, pch=19, col="black", cex=0.5)  
text(-78, 47, "TN - summer data lakes", cex=1.5)

tkn.map<-merge(tkn.covars, latlong, by="lagoslakeid", all.x=T, all.y=F)

map("worldHires", "Canada", xlim=c(min(tkn.map$nhd_long, na.rm=TRUE)-1,max(tkn.map$nhd_long, na.rm=TRUE)+1), ylim=c(min(tkn.map$nhd_lat, na.rm=TRUE)-1,max(tkn.map$nhd_lat, na.rm=TRUE)+1), fill=TRUE, col="khaki", lwd=2, bg="lightblue1")
map("worldHires", "USa", add=TRUE,  fill=TRUE, col="khaki", lwd=2)
map("state", boundary = FALSE, add = TRUE)
points(tkn.map$nhd_long, tkn.map$nhd_lat, pch=19, col="black", cex=0.5)  
text(-78, 47, "TKN - summer data lakes", cex=1.5)


###look at detection limit data and distribution for different N species

dls<-lagos$epi_nutr[,c(2,12,14,18,19, 63, 65, 70, 71 ,92,93)]


dl.80<-dls[dls$sampleyear>1979,]
dl.80.jas<-dl.80[dl.80$samplemonth == 7 | dl.80$samplemonth == 8 | dl.80$samplemonth == 9,]

tndl<-na.omit(dl.80.jas[,c(1,5,9, 10)])
no3dl<-na.omit(dl.80.jas[,c(1,3,7, 10)])
nh4dl<-na.omit(dl.80.jas[,c(1,2,6,10)])
tkndl<-na.omit(dl.80.jas[,c(1,4,8,10)])


smallnh4dl<-nh4dl[nh4dl$nh4_detectionlimit<5,]

library(maps)
library(mapdata)

latlong<-lagos$locus[,c(1,4,5)]

nh4.map<-merge(smallnh4dl, latlong, by="lagoslakeid", all.x=T, all.y=F)

map("worldHires", "Canada", xlim=c(min(nh4.map$nhd_long, na.rm=TRUE)-1,max(nh4.map$nhd_long, na.rm=TRUE)+1), ylim=c(min(nh4.map$nhd_lat, na.rm=TRUE)-1,max(nh4.map$nhd_lat, na.rm=TRUE)+1), fill=TRUE, col="khaki", lwd=2, bg="lightblue1")
map("worldHires", "USa", add=TRUE,  fill=TRUE, col="khaki", lwd=2)
map("state", boundary = FALSE, add = TRUE)
points(nh4.map$nhd_long, nh4.map$nhd_lat, pch=19, col="black", cex=0.5)  
text(-78, 47, "NH4 - summer data lakes", cex=1.5)