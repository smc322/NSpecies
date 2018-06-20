#n composition plots etc

library(LAGOS)
library(dplyr)
library(maps)
library(mapdata)

library(LAGOS)
library(dplyr)
lagos<-lagos_load(version="1.087.1")

locus<-lagos$locus
# laptop version use 
epinut<-lagos$epi.nutr
#desktop use
#epinut<-lagos$epi_nutr

#desktop
#data.tn<-lagos$epi_nutr[,c(2,12,14,19, 63, 65, 71 ,92,93)]
#laptop
data.tn<-lagos$epi.nutr[,c(2,12,14,19, 63, 65, 71 ,92,93)]

#desktop
#data.tkn<-lagos$epi_nutr[,c(2,12,14,18, 63, 65, 70 ,92,93)]
#laptop
data.tkn<-lagos$epi.nutr[,c(2,12,14,18, 63, 65, 70 ,92,93)]

tn.complete<-data.tn[!with(data.tn, is.na(nh4)|is.na(no2no3)|is.na(tn)),]
tn.complete$tkn=NA
tn.complete$tkn_detectionlimit=NA
tn.complete$dopn=tn.complete$tn-tn.complete$no2no3-tn.complete$nh4

tkn.complete<-data.tkn[!with(data.tkn, is.na(nh4)|is.na(no2no3)|is.na(tkn)),]
tkn.complete$tn=NA
tkn.complete$tn_detectionlimit=NA
tkn.complete$dopn=tkn.complete$tkn-tkn.complete$nh4

tnortkn<-rbind(tn.complete, tkn.complete)

nrdl<-filter(tnortkn, nh4_detectionlimit<=50 |is.na(nh4_detectionlimit))
nrdl2<-filter(nrdl, no2no3_detectionlimit<=50|is.na(no2no3_detectionlimit))
nrdl3<-filter(nrdl2, tn_detectionlimit<=200|is.na(tn_detectionlimit))
nrdl4<-filter(nrdl3, tkn_detectionlimit<=200|is.na(tkn_detectionlimit))

nbpd<-nrdl4[,c(1:4,9,10,12)]

nbpd$tn_calculated = nbpd$tkn + nbpd$no2no3

for (i in 1:nrow(nbpd)){
  if (is.na(nbpd$tn[i]) == TRUE) {
    nbpd$tn_combined[i] = nbpd$tn_calculated[i]
  } else {
    nbpd$tn_combined[i] = nbpd$tn[i]
  }
}

med.n<- nbpd %>% group_by(lagoslakeid, sampleyear) %>% summarise(tnmed=median(tn_combined, na.rm=T), dopnmed=median(dopn, na.rm=T), no3med=median(no2no3, na.rm=T), nh4med=median(nh4, na.rm=T))
lakemed<- med.n %>% group_by(lagoslakeid) %>% summarise(tn=median(tnmed,na.rm=T), dopn=median(dopnmed,na.rm=T), no3=median(no3med, na.rm=T), nh4=median(nh4med,na.rm=T))

lakemed$pctdis<-((lakemed$no3+lakemed$nh4)/lakemed$tn)*100

bpmatrix<-as.matrix(log(lakemed[,c(2:6)]))
library(sfsmisc)
boxplot.matrix(bpmatrix, ylim=c(0,10))

tnmatrix<-as.matrix(log(lakemed[,2]))

png("Figures/TN_BP.png", width=4, height=4, units='in', res=300)
par(mar=c(1,4,1,1))
boxplot.matrix(tnmatrix, yaxt='n')
axis(2, at=c(0,2.30,4.6,6.9,9.2), labels=c("0", "10", "100", "1000", "10,000"))
mtext("log TN (ug/L)", side=2, line=2.5)
dev.off()

pctmatrix<-as.matrix(lakemed[,6])

png("Figures/pct_BP.png", width=4, height=4, units='in', res=300)
par(mar=c(1,4,1,1))
boxplot.matrix(pctmatrix, ylim=c(0,100), yaxt='n')
axis(2, at=c(0,25, 50, 75, 100))
mtext("Percent Inorganic N", side=2, line=2.5)
dev.off()

cords<-lagos$locus[,c(1,4,5)]
lmc<-merge(lakemed, cords, by="lagoslakeid", all.x=T, all.y=F)


#use median data to make maps

get.col.bins.med <- function(slopes, alpha=255) {
  z=slopes
  #  quants<-quantile(z, probs = c(.1, .2, .3, .4, .5, .6, .7, .8, .9), na.rm=T)
  
  #ii <- cut(z, breaks = c(-Inf, quants, Inf), 
  #          include.lowest = TRUE)
  
  quantile<-quantile(z, probs = c(.15, .3, .45, .6, .75, .9), na.rm=T)
  
  ii <- cut(z, breaks = c(-Inf, quantile, Inf), 
            include.lowest = TRUE)
  
 # ii <- cut(z, breaks = c(0,10, 20, 30, 40, 50, 60,Inf), 
     #       include.lowest = TRUE)
  
  #purple blue green
  # levels(ii) <- c(rgb(246,239,247,max=255, alpha=alpha),
  #                 rgb(208,209,230,max=255, alpha=alpha),
  #                 rgb(166,189,219,max=255, alpha=alpha),
  #                 rgb(103,169,207,max=255, alpha=alpha),
  #                 rgb(54,144,192,max=255, alpha=alpha),
  #                 rgb(2,129,138,max=255, alpha=alpha),
  #                 rgb(1,100,80,max=255, alpha=alpha))
  #yellow green blue
  levels(ii) <- c(rgb(255,255,182,max=255, alpha=alpha),
                  rgb(199,233,180,max=255, alpha=alpha),
                  rgb(127,205,187,max=255, alpha=alpha),
                  rgb(65,182,196,max=255, alpha=alpha),
                  rgb(29,145,192,max=255, alpha=alpha),
                  rgb(34,94,168,max=255, alpha=alpha),
                  rgb(12,44,132,max=255, alpha=alpha))
  
  
  ii = as.character(ii)
  ii[is.na(ii)==TRUE] <- rgb(255,255,255,max=255)
  return(ii)
}

pdf("Figures/quantmapTN.pdf",width = 8,height = 5)
#par(mfcol=c(2,2), cex = 1)
par(mar = c(1,0,1,0))

#plot lakemeans
map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = TRUE, col="white", fg="grey30", lwd=1,mar=c(0,0,1,0),oma=c(0,0,0,0))
points(lmc$nhd_long, lmc$nhd_lat, bg = get.col.bins.med(lmc$tn, 150), pch = 21, col = rgb(76, 76, 76, max=255, alpha=150) ,lwd = .3, cex=1)
dev.off()


pdf("Figures/quantmapNH4.pdf",width = 8,height = 5)
#par(mfcol=c(2,2), cex = 1)
par(mar = c(1,0,1,0))

#plot lakemeans
map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = TRUE, col="white", fg="grey30",lwd=1,mar=c(0,0,1,0),oma=c(0,0,0,0))
points(lmc$nhd_long, lmc$nhd_lat, bg = get.col.bins.med(lmc$nh4, 150), pch = 21, col = rgb(76, 76, 76, max=255, alpha=150) ,lwd = .3, cex=1)
dev.off()

pdf("Figures/quantmapNO3.pdf",width = 8,height = 5)
#par(mfcol=c(2,2), cex = 1)
par(mar = c(1,0,1,0))

#plot lakemeans
map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = TRUE, col="white", fg="grey30",lwd=1,mar=c(0,0,1,0),oma=c(0,0,0,0))
points(lmc$nhd_long, lmc$nhd_lat, bg = get.col.bins.med(lmc$no3, 150), pch = 21, col = rgb(76, 76, 76, max=255, alpha=150) ,lwd = .3, cex=1)
dev.off()



pdf("Figures/quantmapDOPN.pdf",width = 8,height = 5)
#par(mfcol=c(2,2), cex = 1)
par(mar = c(1,0,1,0))

#plot lakemeans
map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = TRUE, col="white", fg="grey30",lwd=1,mar=c(0,0,1,0),oma=c(0,0,0,0))
points(lmc$nhd_long, lmc$nhd_lat, bg = get.col.bins.med(lmc$dopn, 150), pch = 21, col = rgb(76, 76, 76, max=255, alpha=150) ,lwd = .3, cex=1)
dev.off()

pdf("Figures/quantmapPCTDIS.pdf",width = 8,height = 5)
#par(mfcol=c(2,2), cex = 1)
par(mar = c(1,0,1,0))

#plot lakemeans
map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = TRUE, col="white", fg="grey30",lwd=1,mar=c(0,0,1,0),oma=c(0,0,0,0))
points(lmc$nhd_long, lmc$nhd_lat, bg = get.col.bins.med(lmc$pctdis, 150), pch = 21, col = rgb(76, 76, 76, max=255, alpha=150) ,lwd = .3, cex=1)
dev.off()


quantile(lmc$tn, probs = c(.15, .3, .45, .6, .75, .9), na.rm=T)

colors =  c(rgb(255,255,182,max=255),
            rgb(199,233,180,max=255),
            rgb(127,205,187,max=255),
            rgb(65,182,196,max=255),
            rgb(29,145,192,max=255),
            rgb(34,94,168,max=255),
            rgb(12,44,132,max=255))

png("Figures/TNleg.png", width=1.5, height=.5, units='in', res=300)
par(mar = c(0,0,0,0))
plot.new()
points(x = seq(from = 0.1, to = 0.9, by = (.8/6)), y= rep(.5,7), pch = 22, cex = 2.75, bg = colors, col="grey30")
#text(x = seq(from = .1, to = .9, by = (.8/9)), y = rep(.23,10), round(quantile(lmcords$l.med, probs = c(0.05, .15, .25, .35, .45, .55, .65, .75, .85, .95), na.rm=T)), cex = .5)
text(x = seq(from = .1, to = .9, by = (.8/6)), y = rep(.2,7), labels=c("360", "500", "644", "822", "1147", "2000", ">2000"), cex = .4)
text(x=.5, y = .85, labels = "Median TN (ug/L)", cex = .5)
dev.off()

quantile(lmc$pctdis, probs = c(.15, .3, .45, .6, .75, .9), na.rm=T)

png("Figures/pctleg.png", width=1.5, height=.5, units='in', res=300)
par(mar = c(0,0,0,0))
plot.new()
points(x = seq(from = 0.1, to = 0.9, by = (.8/6)), y= rep(.5,7), pch = 22, cex = 2.75, bg = colors, col="grey30")
#text(x = seq(from = .1, to = .9, by = (.8/9)), y = rep(.23,10), round(quantile(lmcords$l.med, probs = c(0.05, .15, .25, .35, .45, .55, .65, .75, .85, .95), na.rm=T)), cex = .5)
text(x = seq(from = .1, to = .9, by = (.8/6)), y = rep(.2,7), labels=c("4", "7", "11", "16", "26", "52", ">52"), cex = .4)
#text(x=.5, y = .85, labels = "Median TN", cex = .5)
dev.off()


quantile(lmc$no3, probs = c(.15, .3, .45, .6, .75, .9), na.rm=T)

png("Figures/no3leg.png", width=1.5, height=.5, units='in', res=300)
par(mar = c(0,0,0,0))
plot.new()
points(x = seq(from = 0.1, to = 0.9, by = (.8/6)), y= rep(.5,7), pch = 22, cex = 2.75, bg = colors, col="grey30")
#text(x = seq(from = .1, to = .9, by = (.8/9)), y = rep(.23,10), round(quantile(lmcords$l.med, probs = c(0.05, .15, .25, .35, .45, .55, .65, .75, .85, .95), na.rm=T)), cex = .5)
text(x = seq(from = .1, to = .9, by = (.8/6)), y = rep(.2,7), labels=c("6", "10", "20", "38", "100", "480", ">480"), cex = .4)
text(x=.5, y = .85, labels = "Median NO3 (ug/L)", cex = .5)
dev.off()

quantile(lmc$nh4, probs = c(.15, .3, .45, .6, .75, .9), na.rm=T)

png("Figures/nh4leg.png", width=1.5, height=.5, units='in', res=300)
par(mar = c(0,0,0,0))
plot.new()
points(x = seq(from = 0.1, to = 0.9, by = (.8/6)), y= rep(.5,7), pch = 22, cex = 2.75, bg = colors, col="grey30")
#text(x = seq(from = .1, to = .9, by = (.8/9)), y = rep(.23,10), round(quantile(lmcords$l.med, probs = c(0.05, .15, .25, .35, .45, .55, .65, .75, .85, .95), na.rm=T)), cex = .5)
text(x = seq(from = .1, to = .9, by = (.8/6)), y = rep(.2,7), labels=c("10", "18", "24", "40", "70", "170", ">170"), cex = .4)
text(x=.5, y = .85, labels = "Median NH4 (ug/L)", cex = .5)
dev.off()