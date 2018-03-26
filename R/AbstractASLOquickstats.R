## quick and dirty associate either TN or NO3 or NH4 data with lagos covariates to write ASLO abstract

library(LAGOS)
lagos<-lagos_load(version="1.087.1")

locus<-lagos$locus
epinut<-lagos$epi.nutr

#summarize N data

nitrogen<-lagos$epi.nutr[,c(2,12,14,19,93,94)]
nitrogen.80<-nitrogen[nitrogen$sampleyear>1979,]
nitrogen.80.ja<-nitrogen.80[nitrogen.80$samplemonth == 7 | nitrogen.80$samplemonth == 8,]
nitrogen.80.jas<-nitrogen.80[nitrogen.80$samplemonth == 7 | nitrogen.80$samplemonth == 8 | nitrogen.80$samplemonth == 9,]

tn<-na.omit(nitrogen.80.jas[,c(1,4:5)])
no3<-na.omit(nitrogen.80.jas[,c(1,3,5)])
nh4<-na.omit(nitrogen.80.jas[,c(1,2,5)])


annualmed.tn = aggregate(tn$tn, tn[,c("lagoslakeid", "sampleyear")], FUN=function(x) c(median=median(x)))
lakemed.tn = aggregate(annualmed.tn$x, by=list(annualmed.tn$lagoslakeid), FUN=function(x) c(median=median(x)))
names(lakemed.tn)<-c("lagoslakeid", "median_tn")

annualmed.no3 = aggregate(no3$no2no3, no3[,c("lagoslakeid", "sampleyear")], FUN=function(x) c(median=median(x)))
lakemed.no3 = aggregate(annualmed.no3$x, by=list(annualmed.no3$lagoslakeid), FUN=function(x) c(median=median(x)))
names(lakemed.no3)<-c("lagoslakeid", "median_no3")

annualmed.nh4 = aggregate(nh4$nh4, nh4[,c("lagoslakeid", "sampleyear")], FUN=function(x) c(median=median(x)))
lakemed.nh4 = aggregate(annualmed.nh4$x, by=list(annualmed.nh4$lagoslakeid), FUN=function(x) c(median=median(x)))
names(lakemed.nh4)<-c("lagoslakeid", "median_nh4")

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

#save files of each N species with covars

saveRDS(nh4.covars, file="Data/NH4_Covariates_feb2018.rds")
saveRDS(no3.covars, file="Data/NO3_Covariates_feb2018.rds")
saveRDS(tn.covars, file="Data/TN_Covariates_feb2018.rds")


#run a quick random forest for each to get predictability and best predictors to make vague comments in abstract

library(randomForest)

nh4rf<-randomForest(median_nh4~hu8_baseflowindex_mean+hu8_dep_no3_2000_mean+hu8_dep_totaln_2000_mean+hu8_runoff_mean+urban+rowcrop+pasture+forest+wetland+openh20+lake_area_ha+maxdepth+la_wa_ratio, data=nh4.covars)

no3rf<-randomForest(median_no3~hu8_baseflowindex_mean+hu8_dep_no3_2000_mean+hu8_dep_totaln_2000_mean+hu8_runoff_mean+urban+rowcrop+pasture+forest+wetland+openh20+lake_area_ha+maxdepth+la_wa_ratio, data=no3.covars)

tnrf<-randomForest(median_tn~hu8_baseflowindex_mean+hu8_dep_no3_2000_mean+hu8_dep_totaln_2000_mean+hu8_runoff_mean+urban+rowcrop+pasture+forest+wetland+openh20+lake_area_ha+maxdepth+la_wa_ratio, data=tn.covars)
