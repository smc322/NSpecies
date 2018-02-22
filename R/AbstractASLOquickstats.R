## quick and dirty associate either TN or NO3 or NH4 data with lagos covariates to write ASLO abstract

library(LAGOS)
lagos<-lagos_load(version="1.087.1")

locus<-lagos$locus
epinut<-lagos$epi_nutr

chla<-lagos$epi_nutr[,c(2,7,92)]
chla.80<-na.omit(chla[chla$sampleyear>1979,])

annualmed = aggregate(chla.80$chla, chla.80[,c("lagoslakeid", "sampleyear")], FUN=function(x) c(median=median(x)))
lakemed = aggregate(annualmed$x, by=list(annualmed$lagoslakeid), FUN=function(x) c(median=median(x)))
names(lakemed)<-c("lagoslakeid", "median_chla")


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
