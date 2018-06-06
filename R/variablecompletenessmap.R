##make a map of which n variables are observed in which lakes

library(LAGOS)
library(dplyr)
library(maps)
library(mapdata)
lagos<-lagos_load(version="1.087.1")

locus<-lagos$locus
# laptop version use 
#epinut<-lagos$epi.nutr
#desktop use
epinut<-lagos$epi_nutr

data.n<-lagos$epi_nutr[,c(2,12,14, 18, 19, 92,93)]
cords<-locus[,c(1,4,5)]

medians<- data.n %>% group_by(lagoslakeid, sampleyear) %>% summarise(tnmed=median(tn, na.rm=T), tknmed=median(tkn, na.rm=T), no3med=median(no2no3, na.rm=T), nh4med=median(nh4, na.rm=T))
lakemed<- medians %>% group_by(lagoslakeid) %>% summarise(tn=median(tnmed,na.rm=T), tkn=median(tknmed,na.rm=T), no3=median(no3med, na.rm=T), nh4=median(nh4med,na.rm=T))

anyn<-lakemed %>% filter(!(is.na(nh4)&is.na(no3)&is.na(tkn)&is.na(tn)))

ncords<-merge(anyn, cords, by="lagoslakeid", all.x=T, all.y=F)

nh4<- ncords %>% filter(is.na(no3)&is.na(tkn)&is.na(tn))
no3<- ncords %>% filter(is.na(nh4)&is.na(tkn)&is.na(tn))
tkn<- ncords %>% filter(is.na(nh4)&is.na(no3)&is.na(tn))
tn<-ncords %>% filter(is.na(nh4)&is.na(no3)&is.na(tkn))

singlevals<-rbind(nh4, no3, tkn, tn)

multvals<- subset(ncords, !lagoslakeid %in% singlevals$lagoslakeid)

nh4no3<- multvals %>% filter(is.na(tkn)&is.na(tn))
nh4tkn<- multvals %>% filter(is.na(no3)&is.na(tn))
nh4tn<- multvals %>% filter(is.na(no3)&is.na(tkn))
no3tkn<- multvals %>% filter(is.na(nh4)&is.na(tn))
no3tn<- multvals %>% filter(is.na(nh4)&is.na(tkn))
tkntn<- multvals %>% filter(is.na(nh4)&is.na(no3))

double<-rbind(nh4no3, nh4tkn, nh4tn, no3tkn, no3tn, tkntn)
singledouble<-rbind(singlevals, nh4no3, nh4tkn, nh4tn, no3tkn, no3tn, tkntn)

triplequad<- subset(ncords, !lagoslakeid %in% singledouble$lagoslakeid)

quad<-na.omit(ncords)

triple<-subset(triplequad, !lagoslakeid %in% quad$lagoslakeid)


nh4no3tkn<-triplequad %>% filter(is.na(tn))
nh4no3tn<-triplequad %>% filter(is.na(tkn))
nh4tkntn<-triplequad %>% filter(is.na(no3))

disn<-rbind(nh4no3, no3, nh4)


#nono3tkntn

map("worldHires", "Canada", xlim=c(min(ncords$nhd_long, na.rm=TRUE)-1,max(ncords$nhd_long, na.rm=TRUE)+1), ylim=c(min(ncords$nhd_lat, na.rm=TRUE)-1,max(ncords$nhd_lat, na.rm=TRUE)+1), fill=TRUE, col="white", lwd=2, bg="white")
map("worldHires", "USa", add=TRUE,  fill=TRUE, col="white", lwd=2)
map("state", boundary = FALSE, add = TRUE)
points(nh4$nhd_long, nh4$nhd_lat, pch=19, col=rgb(165,0,38,150, max=255), cex=0.5)
points(no3$nhd_long, no3$nhd_lat, pch=19, col=rgb(215,48,39,150, max=255), cex=0.5)
points(tkn$nhd_long, tkn$nhd_lat, pch=19, col=rgb(244,109,67,150, max=255), cex=0.5)
points(tn$nhd_long, tn$nhd_lat, pch=19, col=rgb(253,174,97,150, max=255), cex=0.5)
points(nh4no3$nhd_long, nh4no3$nhd_lat, pch=19, col=rgb(254,224,144,150, max=255), cex=0.5)
points(nh4tkn$nhd_long, nh4tkn$nhd_lat, pch=19, col=rgb(255,255,191,150, max=255), cex=0.5)
points(nh4tn$nhd_long, nh4tn$nhd_lat, pch=19, col=rgb(224,243,248,150, max=255), cex=0.5)
points(no3tkn$nhd_long, no3tkn$nhd_lat, pch=19, col=rgb(171,217,233,150, max=255), cex=0.5)
points(no3tn$nhd_long, no3tn$nhd_lat, pch=19, col=rgb(116,173,209,150, max=255), cex=0.5)
points(tkntn$nhd_long, tkntn$nhd_lat, pch=19, col=rgb(69,117,180,150, max=255), cex=0.5)
points(triplequad$nhd_long, triplequad$nhd_lat, pch=19, col=rgb(69,117,180,150, max=255), cex=0.5)


points(nh4$nhd_long, nh4$nhd_lat, pch=19, col=rgb(178,24,43,150, max=255), cex=0.8)
points(no3$nhd_long, no3$nhd_lat, pch=19, col=rgb(214,96,77,150, max=255), cex=0.8)
points(tkn$nhd_long, tkn$nhd_lat, pch=19, col=rgb(244,165,130,150, max=255), cex=0.8)
points(tn$nhd_long, tn$nhd_lat, pch=19, col=rgb(253,219,199, 150, max=255), cex=0.8)
points(nh4no3$nhd_long, nh4no3$nhd_lat, pch=19, col=rgb(209,229,240,150, max=255), cex=0.8)
points(no3tkn$nhd_long, no3tkn$nhd_lat, pch=19, col=rgb(146,197,222,150, max=255), cex=0.8)
points(nh4no3tkn$nhd_long, nh4no3tkn$nhd_lat, pch=19, col=rgb(67,147,195,150, max=255), cex=0.8)
points(nh4no3tn$nhd_long, nh4no3tn$nhd_lat, pch=19, col=rgb(33,102,172,150, max=255), cex=0.8)


points(singlevals$nhd_long, singlevals$nhd_lat, pch=21, col="black", lwd=.5, bg=rgb(255,255,204,150, max=255), cex=0.7)
points(double$nhd_long, double$nhd_lat, pch=21, col="black", lwd=.5, bg=rgb(161,218,180,150, max=255), cex=0.7)
points(triple$nhd_long, triple$nhd_lat, pch=21, col="black", lwd=.5,  bg=rgb(65,182,196,150, max=255), cex=0.7)
points(quad$nhd_long, quad$nhd_lat, pch=21, col="black", lwd=.5, bg=rgb(34,94,168,150, max=255), cex=0.7)



text(-78, 47, "N Species", cex=1.5)


###lakes that Erin et al actually used in analysis: 
load("Data/newdata_lakeid.RData")