##want to make some figures for AOAS early 2019 submission - maps of N concentration vs. measurement method, maps of lulc and other stuff. Erin sent data that went into analysis - dataforplots.Rdata
##also run dataset_07Dec18.R to get data frames for geo data that have all lakes/watersheds in LAGOS NE for depth and land use maps.

library(maps)
analysisdat<-get(load("Data/DataForPlots.Rdata"))

analysisdat$tn_calc_1<-analysisdat$tkn+analysisdat$no2no3
analysisdat$tn_calc_2<-analysisdat$tkn_2+analysisdat$no2no3_2

analysisdat$tnavg<-(analysisdat$tn+analysisdat$tn_2)/2
analysisdat$tncalcavg<-(analysisdat$tn_calc_1+analysisdat$tn_calc_2)/2

tn.map.dat<-na.omit(analysisdat[,c("lagoslakeid", "tnavg")])
names(tn.map.dat)<-c("lagoslakeid", "tn")
tn.map.dat$dattype<-1

tncalc.map.dat<-na.omit(analysisdat[,c("lagoslakeid", "tncalcavg")])
names(tncalc.map.dat)<-c("lagoslakeid", "tn")
tncalc.map.dat$dattype<-2

map.dat<-rbind(tn.map.dat, tncalc.map.dat)
map.dat.cords<-merge(map.dat, ll.l, by="lagoslakeid", all.x=T, all.y=F)

alpha=255
colors<- c(rgb(33, 102, 172,max=255, alpha=alpha),
  rgb(103, 169, 207,max=255, alpha=alpha),
  rgb(209, 229, 240,max=255, alpha=alpha),
  rgb(247, 247, 247,max=255, alpha=alpha),
  rgb(253, 219, 199,max=255, alpha=alpha),
  rgb(239, 138, 98,max=255, alpha=alpha),
  rgb(178, 24, 43,max=255, alpha=alpha))


slopes<-map.dat.cords$tn

get.col.bins.tn <- function(slopes, alpha=255) {
  z=slopes
  x=1/7
   quants<-quantile(z, probs = c(x, 2*x, 3*x, 4*x, 5*x, 6*x), na.rm=T)
  
  ii <- cut(z, breaks = c(-Inf, quants, Inf), 
            include.lowest = TRUE)
  
  #ii <- cut(z, breaks = c(0,250, 500, 750, 1000, 1250, 1500,Inf), 
   #         include.lowest = TRUE)
  
  #purple blue green
  # levels(ii) <- c(rgb(246,239,247,max=255, alpha=alpha),
  #                 rgb(208,209,230,max=255, alpha=alpha),
  #                 rgb(166,189,219,max=255, alpha=alpha),
  #                 rgb(103,169,207,max=255, alpha=alpha),
  #                 rgb(54,144,192,max=255, alpha=alpha),
  #                 rgb(2,129,138,max=255, alpha=alpha),
  #                 rgb(1,100,80,max=255, alpha=alpha))
  #yellow green blue
  levels(ii) <- c(rgb(33, 102, 172,max=255, alpha=alpha),
                  rgb(103, 169, 207,max=255, alpha=alpha),
                  rgb(209, 229, 240,max=255, alpha=alpha),
                  rgb(247, 247, 247,max=255, alpha=alpha),
                  rgb(253, 219, 199,max=255, alpha=alpha),
                  rgb(239, 138, 98,max=255, alpha=alpha),
                  rgb(178, 24, 43,max=255, alpha=alpha))
  
  
  ii = as.character(ii)
  ii[is.na(ii)==TRUE] <- rgb(255,255,255,max=255)
  return(ii)
}

pdf("Figures/MAPS_AOASFigs/Fig1_TNmap.pdf",width = 10,height = 6)
map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = FALSE, col="grey30",lwd=1,mar=c(0,0,1,0),oma=c(0,0,0,0))
points(map.dat.cords$nhd_long, map.dat.cords$nhd_lat, bg = get.col.bins.tn(map.dat.cords$tn, 200), pch = c(21, 23)[map.dat.cords$dattype], col = "black" ,lwd = .5, cex=1.25)

points(x = seq(from = -82, to =-77, by = (5/6)), y= rep(48,7), pch = 22, cex = 4, bg = colors, col="grey30")
text(x = seq(from = -82, to = -77, by = (5/6)), y = rep(47.55,7), labels=c("331", "477", "656", "842", "1153", "1957", ">1957"), cex = .6)
text(x=-79.5, y = 48.55, labels = expression(paste("Median TN (",mu,"g/L)")))

points(x=-81, y=46.6, pch=21, cex=2)
text(-80.8, 46.56, "TN Measured", pos=4)
points(x=-81, y=46, pch=23, cex=2)
text(-80.8, 45.96, "TN Calculated", pos=4)
#text(-97, 49.5, "(A) TP", adj=c(0,0), cex=1.25)
dev.off()


#also want a 2 panel of land use (I think RC) and lake depth to show spatial structure of predictors - make this with all lakes with depth and % RC data in LAGOS, should already be split out in the prev. dataset script to draw from

rc.depth<-na.omit(chag.lulc.lake[,c("lagoslakeid", "rowcrop", "maxdepth")])
rc.d.ll<-merge(rc.depth, ll.l, by="lagoslakeid", all.x=T, all.y=F)

slopes.rc<-rc.d.ll$rowcrop

get.col.bins.rc <- function(slopes.rc, alpha=255) {
  z=slopes.rc
  #x=1/7
  #quants<-quantile(z, probs = c(x, 2*x, 3*x, 4*x, 5*x, 6*x), na.rm=T)
  
  ii <- cut(z, breaks = c(0, 0.5, 1, 10, 20, 40, 60, Inf), 
            include.lowest = TRUE)
  
  #ii <- cut(z, breaks = c(0,250, 500, 750, 1000, 1250, 1500,Inf), 
  #         include.lowest = TRUE)
  
  #purple blue green
  # levels(ii) <- c(rgb(246,239,247,max=255, alpha=alpha),
  #                 rgb(208,209,230,max=255, alpha=alpha),
  #                 rgb(166,189,219,max=255, alpha=alpha),
  #                 rgb(103,169,207,max=255, alpha=alpha),
  #                 rgb(54,144,192,max=255, alpha=alpha),
  #                 rgb(2,129,138,max=255, alpha=alpha),
  #                 rgb(1,100,80,max=255, alpha=alpha))
  #yellow green blue
  levels(ii) <- c(rgb(33, 102, 172,max=255, alpha=alpha),
                  rgb(103, 169, 207,max=255, alpha=alpha),
                  rgb(209, 229, 240,max=255, alpha=alpha),
                  rgb(247, 247, 247,max=255, alpha=alpha),
                  rgb(253, 219, 199,max=255, alpha=alpha),
                  rgb(239, 138, 98,max=255, alpha=alpha),
                  rgb(178, 24, 43,max=255, alpha=alpha))
  
  
  ii = as.character(ii)
  ii[is.na(ii)==TRUE] <- rgb(255,255,255,max=255)
  return(ii)
}

slopes.ld<-rc.d.ll$maxdepth

get.col.bins.ld <- function(slopes.ld, alpha=255) {
  z=slopes.ld
  x=1/7
  quants<-quantile(z, probs = c(x, 2*x, 3*x, 4*x, 5*x, 6*x), na.rm=T)
  
  ii <- cut(z, breaks = c(-Inf, quants, Inf), 
            include.lowest = TRUE)
  
  #ii <- cut(z, breaks = c(0,250, 500, 750, 1000, 1250, 1500,Inf), 
  #         include.lowest = TRUE)
  
  #purple blue green
  # levels(ii) <- c(rgb(246,239,247,max=255, alpha=alpha),
  #                 rgb(208,209,230,max=255, alpha=alpha),
  #                 rgb(166,189,219,max=255, alpha=alpha),
  #                 rgb(103,169,207,max=255, alpha=alpha),
  #                 rgb(54,144,192,max=255, alpha=alpha),
  #                 rgb(2,129,138,max=255, alpha=alpha),
  #                 rgb(1,100,80,max=255, alpha=alpha))
  #yellow green blue
  levels(ii) <- c(rgb(33, 102, 172,max=255, alpha=alpha),
                  rgb(103, 169, 207,max=255, alpha=alpha),
                  rgb(209, 229, 240,max=255, alpha=alpha),
                  rgb(247, 247, 247,max=255, alpha=alpha),
                  rgb(253, 219, 199,max=255, alpha=alpha),
                  rgb(239, 138, 98,max=255, alpha=alpha),
                  rgb(178, 24, 43,max=255, alpha=alpha))
  
  
  ii = as.character(ii)
  ii[is.na(ii)==TRUE] <- rgb(255,255,255,max=255)
  return(ii)
}

pdf("Figures/MAPS_AOASFigs/Fig2_RCLD.pdf",width = 8,height = 10)
par(mfrow=c(2,1), mar=c(0,0,0,0), xpd=NA)
map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = FALSE, col="grey30",lwd=1,mar=c(0,0,1,0),oma=c(0,0,0,0))
points(rc.d.ll$nhd_long, rc.d.ll$nhd_lat, bg = get.col.bins.rc(rc.d.ll$rowcrop, 200), pch = 21, col = "black" ,lwd = .5, cex=1.25)

#points(x = seq(from = -82, to =-77, by = (5/6)), y= rep(48,7), pch = 22, cex = 4, bg = colors, col="grey30")
#text(x = seq(from = -82, to = -77, by = (5/6)), y = rep(47.55,7), labels=c("331", "477", "656", "842", "1153", "1957", ">1957"), cex = .6)
text(x=-79.5, y = 48.55, "% Row Crop Agriculture")

map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = FALSE, col="grey30",lwd=1,mar=c(0,0,1,0),oma=c(0,0,0,0))
points(rc.d.ll$nhd_long, rc.d.ll$nhd_lat, bg = get.col.bins.ld(rc.d.ll$maxdepth, 200), pch = 21, col = "black" ,lwd = .5, cex=1.25)

#points(x = seq(from = -82, to =-77, by = (5/6)), y= rep(48,7), pch = 22, cex = 4, bg = colors, col="grey30")
#text(x = seq(from = -82, to = -77, by = (5/6)), y = rep(47.55,7), labels=c("331", "477", "656", "842", "1153", "1957", ">1957"), cex = .6)
text(x=-79.5, y = 48.55, "Lake Depth (m)")

dev.off()


#want to remake fig 1 with all lakes not just with analysis lakes
data.n<-epinut[,c("lagoslakeid", "tn", "tkn", "no2no3", "sampleyear", "samplemonth")]

data.n$tn_calc<- data.n$tkn + data.n$no2no3
data.n$tn_all = data.n$tn
data.n$tn_all[which(is.na(data.n$tn_all))] = data.n$tn_calc[which(is.na(data.n$tn_all))]

data.n.80<-data.n[data.n$sampleyear>1979,]
data.n.80.jas<-data.n.80[data.n.80$samplemonth == 7 | data.n.80$samplemonth == 8 | data.n.80$samplemonth == 9,]

nkeeps<-na.omit(data.n.80.jas[,c("lagoslakeid", "tn_all", "sampleyear")])

nlymed<- nkeeps %>% group_by(lagoslakeid, sampleyear) %>% summarise(ly.med=median(tn_all))
lakemed<- nlymed %>% group_by(lagoslakeid) %>% summarise(l.med=median(ly.med))

tndir<-na.omit(data.n[,c("lagoslakeid", "tn")])
dirlakes<-unique(tndir$lagoslakeid)

lakemed.dir<-lakemed[lakemed$lagoslakeid %in% dirlakes, ]
lakemed.calc<-lakemed[!lakemed$lagoslakeid %in% dirlakes,]

lakemed.dir$dattype<-1
lakemed.calc$dattype<-2

lakemed.all<-rbind(lakemed.dir, lakemed.calc)
names(lakemed.all)<-c("lagoslakeid", "tn", "dattype")
lakesallmap<-merge(lakemed.all, ll.l, by="lagoslakeid", all.x=T, all.y=F)

pdf("Figures/MAPS_AOASFigs/Fig1_TNmap_alllakes.pdf",width = 10,height = 6)
map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = FALSE, col="grey30",lwd=1,mar=c(0,0,1,0),oma=c(0,0,0,0))
points(lakesallmap$nhd_long, lakesallmap$nhd_lat, bg = get.col.bins.tn(lakesallmap$tn, 200), pch = c(21, 23)[lakesallmap$dattype], col = "black" ,lwd = .5, cex=1)

points(x = seq(from = -82, to =-77, by = (5/6)), y= rep(48,7), pch = 22, cex = 4, bg = colors, col="grey30")
text(x = seq(from = -82, to = -77, by = (5/6)), y = rep(47.55,7), labels=c("331", "477", "656", "842", "1153", "1957", ">1957"), cex = .6)
text(x=-79.5, y = 48.55, labels = expression(paste("Median TN (",mu,"g/L)")))

points(x=-81, y=46.6, pch=21, cex=2)
text(-80.8, 46.56, "TN Measured", pos=4)
points(x=-81, y=46, pch=23, cex=2)
text(-80.8, 45.96, "TN Calculated", pos=4)
#text(-97, 49.5, "(A) TP", adj=c(0,0), cex=1.25)
dev.off()