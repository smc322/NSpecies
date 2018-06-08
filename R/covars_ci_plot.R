#want to create a plot of the coefficient parameters with 95% credible intervals for aslo talk

#data from Erin in May2018 report doc
# Intercept                &3.667(3.347,4.002)&3.546(2.856,4.279)&6.495(6.276,6.699) \\
# hu8baseflow          &-0.066(-0.128,-0.004)& 0.161(0.048,0.276)&-0.041(-0.081,-0.002)\\
# hu8no3depo           &-0.331(-0.750,0.077)&0.422(-0.381,1.307)&-0.062(-0.291,0.208)  \\
# hu8totalndepo       & 0.039(-0.280,0.375)&-0.428(-1.125,0.226)& 0.030(-0.177,0.222)          \\
# hu8runoff               &-0.035(-0.210,0.142)&-0.015(-0.423,0.333)&-0.269(-0.381,-0.169)           \\
# Row crop 		& -0.277(-0.782,0.188) & 2.971(1.952,3.773) &0.067 (-0.251,0.396) 	\\
# Forest		&-0.841(-1.172,-0.476) &-0.944(-1.716,-0.221) &-0.818(-1.036,-0.582) \\
# Wetland              &-1.052(-1.761,-0.266)&-0.535(-1.988,0.973)&0.014(-0.503,0.498)\\
# Lake area 	&-0.113(-0.276,0.048)&0.005(-0.278,0.297)&-0.029(-0.121,0.061)\\
# Max depth		&-0.126(-0.221,-0.033) & 0.147(-0.047,0.348) &-0.198(-0.258,-0.138) \\
# Ratio			&-0.033(-0.493,0.407)&-1.174(-2.077,-0.226)&-0.347(-0.649,-0.060)\\
# DRLakeStream          & 0.336(0.122,0.554)& 0.248(-0.212,0.644)&0.026(-0.118,0.167)     \\
# DRStream                &  0.059(-0.134,0.240)& 0.081(-0.336,0.513)&0.034(-0.104,0.159)   \\

library(arm)

rccoefs<-c(-0.277, 2.971, 0.067)
forcoefs<-c( -0.841, -0.944, -0.818)
wtcoefs<-c(-1.052, -0.535, 0.014)
rcupper<-c(0.188, 3.773, 0.396)
forupper<-c(-0.476, -0.221, -0.582)
wtupper<-c(-0.266, 0.973, 0.498)
rclower<-c(-0.782,1.952, -0.251)
forlower<-c( -1.172, -1.716, -1.036)
wtlower<-c(-1.761, -1.988, -0.503)
names<-c("NH4", "NO3", "OrgN")
sd=0

coefplot(rccoefs, sd, CI=1, rclower, rcupper, varnames="", ylim=c(-3, 4), cex.pts=2, lwd.pts=2, vertical=F)
mtext(names, at=c(1, 2,3), side=1)
coefplot(forcoefs, sd, CI=1, forlower, forupper, add=T, vertical=F, col.pts="red")
coefplot(wtcoefs, sd, CI=1, wtlower, wtupper, add=T, offset=-.1, vertical=F)

#actually ohter way around is better - N species as legend, covars on x axis

nh4lu<-c(-0.277,  -0.841, -1.052)
nh4luu<-c(0.188, -0.476, -0.266)
nh4lul<-c(-0.782,  -1.172, -1.761)

no3lu<-c(2.971, -0.944, -0.535)
no3luu<-c(3.773, -0.221,0.973)
no3lul<-c(1.952, -1.716,-1.988)

onlu<-c(0.067, -0.818, 0.014)
onluu<-c(0.396, -0.582, 0.498)
onlul<-c(-0.251, -1.036, -0.503)

lun<-c("RowCrop", "Forest", "Wetland")




#luplustin

nh4<-c(-0.277,  -0.841, -1.052, -0.126, -0.033)
nh4u<-c(0.188, -0.476, -0.266, -0.033, 0.407)
nh4l<-c(-0.782,  -1.172, -1.761, -0.221, -0.493)

no3<-c(2.971, -0.944, -0.535, 0.147, -1.174)
no3u<-c(3.773, -0.221,0.973, 0.348, -0.226)
no3l<-c(1.952, -1.716,-1.988, -0.047, -2.077)

on<-c(0.067, -0.818, 0.014, -0.198, -0.347)
onu<-c(0.396, -0.582, 0.498, -0.138, -0.060)
onl<-c(-0.251, -1.036, -0.503, -0.258, -0.649)

labs<-c("RowCrop", "Forest", "Wetland", "Depth", "ResTime")

coefplot(nh4, sd, CI=1, nh4u, nh4l, varnames="", ylim=c(-2, 4), cex.pts=2, lwd.pts=2, vertical=F)
mtext(labs, at=c(1, 2,3, 4, 5), side=1)
coefplot(no3, sd, CI=1, no3l, no3u, add=T, vertical=F, col.pts="red")
coefplot(on, sd, CI=1, onl, onu, add=T, offset=-.1, vertical=F)
