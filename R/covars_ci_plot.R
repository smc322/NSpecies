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




#covars on x axis, species by color

nh4<-c(-0.277,  -0.841, -1.052, -0.126, -0.113, -0.033, -0.035, -0.066, -0.331, 0.039, 0.059, 0.336)
nh4u<-c(0.188, -0.476, -0.266, -0.033,  0.048,  0.407,  0.142, -0.004 ,0.077   ,0.375 ,0.240  ,0.554)
nh4l<-c(-0.782,  -1.172, -1.761, -0.221,-0.276, -0.493, -0.210, -0.128, -0.750, -0.280, -0.134, 0.122)

no3<-c(2.971, -0.944, -0.535, 0.147,0.005, -1.174, -0.015, 0.161, 0.422, -0.428,  0.081, 0.248)
no3u<-c(3.773, -0.221,0.973, 0.348,  0.297, -0.226 , 0.333, 0.276 ,1.307  ,0.226 ,0.513 ,0.644)
no3l<-c(1.952, -1.716,-1.988, -0.047,-0.278, -2.077, -0.423, 0.048, -0.381,-1.125,-0.336, -0.212)

on<-c(0.067, -0.818, 0.014, -0.198,   -0.029, -0.347, -0.269, -0.041, -0.062, 0.030, 0.034, 0.026)
onu<-c(0.396, -0.582, 0.498, -0.138,   0.061, -0.060,  -0.169,-0.002  ,0.208 ,0.222  ,0.159 ,0.167)
onl<-c(-0.251, -1.036, -0.503, -0.258, -0.121, -0.649, -0.381, -0.081, -0.291,-0.177, -0.104, -0.118)

#labs<-c("Crop", "For", "Wetl", "Depth", "Area", "ResT", "Runoff", "Basef", "NO3D", "TND", "DRS", "DRL")
labs<-c("Land Use", "Internal Processing", "Hydrology", "Deposition", "Connectivity")

png("Figures/Coefs_All.png", width=10, height=4, units='in', res=300)
#par(xpd=NA)
coefplot(nh4, sd, CI=1, nh4u, nh4l, varnames="", mar=c(0,3,4,1), ylim=c(-2, 4), cex.pts=2, v.axis=F, col.pts=rgb(168,221,181, 255, max=255),lwdInner=3, cex.axis=2, main="", vertical=F)
segments(1, nh4l[1], 1, nh4u[1], lwd=3, rgb(168,221,181, 255, max=255))
segments(2, nh4l[2], 2, nh4u[2], lwd=3, rgb(168,221,181, 255, max=255))
segments(3, nh4l[3], 3, nh4u[3], lwd=3, rgb(168,221,181, 255, max=255))
segments(4, nh4l[4], 4, nh4u[4], lwd=3, rgb(168,221,181, 255, max=255))
segments(5, nh4l[5], 5, nh4u[5], lwd=3, rgb(168,221,181, 255, max=255))
segments(6, nh4l[6], 6, nh4u[6], lwd=3, rgb(168,221,181, 255, max=255))
segments(7, nh4l[7], 7, nh4u[7], lwd=3, rgb(168,221,181, 255, max=255))
segments(8, nh4l[8], 8, nh4u[8], lwd=3, rgb(168,221,181, 255, max=255))
segments(9, nh4l[9], 9, nh4u[9], lwd=3, rgb(168,221,181, 255, max=255))
segments(10, nh4l[10], 10, nh4u[10], lwd=3, rgb(168,221,181, 255, max=255))
segments(11, nh4l[11], 11, nh4u[11], lwd=3, rgb(168,221,181, 255, max=255))
segments(12, nh4l[12], 12, nh4u[12], lwd=3, rgb(168,221,181, 255, max=255))

abline(v=3.5, lty=3)
abline(v=6.5, lty=3)
abline(v=8.5, lty=3)
abline(v=10.5, lty=3)

mtext(labs, at=c(2,5, 7.5, 9.5, 11.5), cex=1.2, side=1, srt=45)
mtext("Coefficient +/- 95% CI", at=1, line=2, cex=1.2, side=2)
axis(2, cex=3, lwd=2)
coefplot(no3, sd, CI=1, no3l, no3u, add=T, offset=-.1, cex.pts=2, vertical=F, col.pts=rgb(44, 127, 184, 255, max=255))
segments(.9, no3l[1], .9, no3u[1], lwd=3, rgb(44, 127, 184, 255, max=255))
segments(1.9, no3l[2], 1.9, no3u[2], lwd=3, rgb(44, 127, 184, 255, max=255))
segments(2.9, no3l[3], 2.9, no3u[3], lwd=3, rgb(44, 127, 184, 255, max=255))
segments(3.9, no3l[4], 3.9, no3u[4], lwd=3, rgb(44, 127, 184, 255, max=255))
segments(4.9, no3l[5], 4.9, no3u[5], lwd=3, rgb(44, 127, 184, 255, max=255))
segments(5.9, no3l[6], 5.9, no3u[6], lwd=3, rgb(44, 127, 184, 255, max=255))
segments(6.9, no3l[7], 6.9, no3u[7], lwd=3, rgb(44, 127, 184, 255, max=255))
segments(7.9, no3l[8], 7.9, no3u[8], lwd=3, rgb(44, 127, 184, 255, max=255))
segments(8.9, no3l[9], 8.9, no3u[9], lwd=3, rgb(44, 127, 184, 255, max=255))
segments(9.9, no3l[10], 9.9, no3u[10], lwd=3, rgb(44, 127, 184, 255, max=255))
segments(10.9, no3l[11], 10.9, no3u[11], lwd=3, rgb(44, 127, 184, 255, max=255))
segments(11.9, no3l[12], 11.9, no3u[12], lwd=3, rgb(44, 127, 184, 255, max=255))


coefplot(on, sd, CI=1, onl, onu, add=T,vertical=F, cex.pts=2, col.pts=rgb(117,107,177, 255, max=255))
segments(1.1, onl[1], 1.1, onu[1], lwd=3, rgb(117,107,177, 255, max=255))
segments(2.1, onl[2], 2.1, onu[2], lwd=3, rgb(117,107,177, 255, max=255))
segments(3.1, onl[3], 3.1, onu[3], lwd=3, rgb(117,107,177, 255, max=255))
segments(4.1, onl[4], 4.1, onu[4], lwd=3, rgb(117,107,177, 255, max=255))
segments(5.1, onl[5], 5.1, onu[5], lwd=3, rgb(117,107,177, 255, max=255))
segments(6.1, onl[6], 6.1, onu[6], lwd=3, rgb(117,107,177, 255, max=255))
segments(7.1, onl[7], 7.1, onu[7], lwd=3, rgb(117,107,177, 255, max=255))
segments(8.1, onl[8], 8.1, onu[8], lwd=3, rgb(117,107,177, 255, max=255))
segments(9.1, onl[9], 9.1, onu[9], lwd=3, rgb(117,107,177, 255, max=255))
segments(10.1, onl[10], 10.1, onu[10], lwd=3, rgb(117,107,177, 255, max=255))
segments(11.1, onl[11], 11.1, onu[11], lwd=3, rgb(117,107,177, 255, max=255))
segments(12.1, onl[12], 12.1, onu[12], lwd=3, rgb(117,107,177, 255, max=255))

legend(11, 4, legend=c("NO3", "NH4", "OrgN"), bty="n", fill=c(rgb(44, 127, 184, 255, max=255), rgb(168,221,181, 255, max=255), rgb(117,107,177, 255, max=255)))

dev.off()

#just land use

nh4<-c(-0.277,  -0.841, -1.052)
nh4u<-c(0.188, -0.476, -0.266)
nh4l<-c(-0.782,  -1.172, -1.761)

no3<-c(2.971, -0.944, -0.535)
no3u<-c(3.773, -0.221,0.973)
no3l<-c(1.952, -1.716,-1.988)

on<-c(0.067, -0.818, 0.014)
onu<-c(0.396, -0.582, 0.498)
onl<-c(-0.251, -1.036, -0.503)

labs<-c("Row Crop", "Forest", "Wetland")

png("Figures/Coefs_LULC.png", width=4, height=4, units='in', res=300)
#par(xpd=NA)
coefplot(nh4, sd, CI=1, nh4u, nh4l, varnames="", mar=c(0,4,4,1), ylim=c(-2, 4), cex.pts=2, v.axis=F, col.pts=rgb(168,221,181, 255, max=255),lwdInner=3, cex.axis=2, main="", vertical=F)
segments(1, nh4l[1], 1, nh4u[1], lwd=3, rgb(168,221,181, 255, max=255))
segments(2, nh4l[2], 2, nh4u[2], lwd=3, rgb(168,221,181, 255, max=255))
segments(3, nh4l[3], 3, nh4u[3], lwd=3, rgb(168,221,181, 255, max=255))

mtext(labs, at=c(1, 2, 3), cex=1.2, side=1)
mtext("Coefficient +/- 95% CI", at=1, line=2, cex=1.2, side=2)
axis(2, cex=3, lwd=2)
coefplot(no3, sd, CI=1, no3l, no3u, add=T, offset=-.1, cex.pts=2, vertical=F, col.pts=rgb(44, 127, 184, 255, max=255))
segments(.9, no3l[1], .9, no3u[1], lwd=3, rgb(44, 127, 184, 255, max=255))
segments(1.9, no3l[2], 1.9, no3u[2], lwd=3, rgb(44, 127, 184, 255, max=255))
segments(2.9, no3l[3], 2.9, no3u[3], lwd=3, rgb(44, 127, 184, 255, max=255))

coefplot(on, sd, CI=1, onl, onu, add=T,vertical=F, cex.pts=2, col.pts=rgb(117,107,177, 255, max=255))
segments(1.1, onl[1], 1.1, onu[1], lwd=3, rgb(117,107,177, 255, max=255))
segments(2.1, onl[2], 2.1, onu[2], lwd=3, rgb(117,107,177, 255, max=255))
segments(3.1, onl[3], 3.1, onu[3], lwd=3, rgb(117,107,177, 255, max=255))

legend(2, 4, legend=c("NO3", "NH4", "OrgN"), bty="n", fill=c(rgb(44, 127, 184, 255, max=255), rgb(168,221,181, 255, max=255), rgb(117,107,177, 255, max=255)))

dev.off()


#just int

nh4<-c(-0.126, -0.113, -0.033)
nh4u<-c(-0.033,  0.048,  0.407)
nh4l<-c(-0.221,-0.276, -0.493)

no3<-c(0.147,0.005, -1.174)
no3u<-c(0.348,  0.297, -0.226)
no3l<-c(-0.047,-0.278, -2.077)

on<-c(-0.198,   -0.029, -0.347)
onu<-c(-0.138,   0.061, -0.060)
onl<-c(-0.258, -0.121, -0.649)


labs<-c("Lake Depth", "Lake Area", "Res Time")

png("Figures/Coefs_Int.png", width=4, height=4, units='in', res=300)
#par(xpd=NA)
coefplot(nh4, sd, CI=1, nh4u, nh4l, varnames="", mar=c(0,4,4,1), ylim=c(-2, 1), cex.pts=2, v.axis=F, col.pts=rgb(168,221,181, 255, max=255),lwdInner=3, cex.axis=2, main="", vertical=F)
segments(1, nh4l[1], 1, nh4u[1], lwd=3, rgb(168,221,181, 255, max=255))
segments(2, nh4l[2], 2, nh4u[2], lwd=3, rgb(168,221,181, 255, max=255))
segments(3, nh4l[3], 3, nh4u[3], lwd=3, rgb(168,221,181, 255, max=255))

mtext(labs, at=c(1, 2, 3), cex=1, side=1)
mtext("Coefficient +/- 95% CI", at=-.5, line=2, cex=1.2, side=2)
axis(2, cex=3, lwd=2)
coefplot(no3, sd, CI=1, no3l, no3u, add=T, offset=-.1, cex.pts=2, vertical=F, col.pts=rgb(44, 127, 184, 255, max=255))
segments(.9, no3l[1], .9, no3u[1], lwd=3, rgb(44, 127, 184, 255, max=255))
segments(1.9, no3l[2], 1.9, no3u[2], lwd=3, rgb(44, 127, 184, 255, max=255))
segments(2.9, no3l[3], 2.9, no3u[3], lwd=3, rgb(44, 127, 184, 255, max=255))

coefplot(on, sd, CI=1, onl, onu, add=T,vertical=F, cex.pts=2, col.pts=rgb(117,107,177, 255, max=255))
segments(1.1, onl[1], 1.1, onu[1], lwd=3, rgb(117,107,177, 255, max=255))
segments(2.1, onl[2], 2.1, onu[2], lwd=3, rgb(117,107,177, 255, max=255))
segments(3.1, onl[3], 3.1, onu[3], lwd=3, rgb(117,107,177, 255, max=255))

legend(.75, -.75, legend=c("NO3", "NH4", "OrgN"), bty="n", fill=c(rgb(44, 127, 184, 255, max=255), rgb(168,221,181, 255, max=255), rgb(117,107,177, 255, max=255)))

dev.off()

#hydro and depo

nh4<-c(-0.035, -0.066, -0.331, 0.039)
nh4u<-c(0.142, -0.004 ,0.077   ,0.375)
nh4l<-c(-0.210, -0.128, -0.750, -0.280)

no3<-c(-0.015, 0.161, 0.422, -0.428)
no3u<-c( 0.333, 0.276 ,1.307  ,0.226)
no3l<-c(-0.423, 0.048, -0.381,-1.125)

on<-c(-0.269, -0.041, -0.062, 0.030)
onu<-c(-0.169,-0.002  ,0.208 ,0.222)
onl<-c(-0.381, -0.081, -0.291,-0.177)


labs<-c("Runoff", "Baseflow", "NO3Dep", "TNDep")

png("Figures/Coefs_Hydep.png", width=4.5, height=4, units='in', res=300)
#par(xpd=NA)
coefplot(nh4, sd, CI=1, nh4u, nh4l, varnames="", mar=c(0,4,4,1), ylim=c(-1.5, 1.5), cex.pts=2, v.axis=F, col.pts=rgb(168,221,181, 255, max=255),lwdInner=3, cex.axis=2, main="", vertical=F)
segments(1, nh4l[1], 1, nh4u[1], lwd=3, rgb(168,221,181, 255, max=255))
segments(2, nh4l[2], 2, nh4u[2], lwd=3, rgb(168,221,181, 255, max=255))
segments(3, nh4l[3], 3, nh4u[3], lwd=3, rgb(168,221,181, 255, max=255))
segments(4, nh4l[4], 4, nh4u[4], lwd=3, rgb(168,221,181, 255, max=255))


mtext(labs, at=c(1, 2, 3, 4), cex=1, side=1)
mtext("Coefficient +/- 95% CI", at=0, line=2, cex=1.2, side=2)
axis(2, cex=3, lwd=2)
coefplot(no3, sd, CI=1, no3l, no3u, add=T, offset=-.1, cex.pts=2, vertical=F, col.pts=rgb(44, 127, 184, 255, max=255))
segments(.9, no3l[1], .9, no3u[1], lwd=3, rgb(44, 127, 184, 255, max=255))
segments(1.9, no3l[2], 1.9, no3u[2], lwd=3, rgb(44, 127, 184, 255, max=255))
segments(2.9, no3l[3], 2.9, no3u[3], lwd=3, rgb(44, 127, 184, 255, max=255))
segments(3.9, no3l[4], 3.9, no3u[4], lwd=3, rgb(44, 127, 184, 255, max=255))

coefplot(on, sd, CI=1, onl, onu, add=T,vertical=F, cex.pts=2, col.pts=rgb(117,107,177, 255, max=255))
segments(1.1, onl[1], 1.1, onu[1], lwd=3, rgb(117,107,177, 255, max=255))
segments(2.1, onl[2], 2.1, onu[2], lwd=3, rgb(117,107,177, 255, max=255))
segments(3.1, onl[3], 3.1, onu[3], lwd=3, rgb(117,107,177, 255, max=255))
segments(4.1, onl[4], 4.1, onu[4], lwd=3, rgb(117,107,177, 255, max=255))

legend(.75, -.5, legend=c("NO3", "NH4", "OrgN"), bty="n", fill=c(rgb(44, 127, 184, 255, max=255), rgb(168,221,181, 255, max=255), rgb(117,107,177, 255, max=255)), cex=.9)

dev.off()

#connectivity

nh4<-c(0.059, 0.336)
nh4u<-c(0.240  ,0.554)
nh4l<-c(-0.134, 0.122)

no3<-c(0.081, 0.248)
no3u<-c(0.513 ,0.644)
no3l<-c(-0.336, -0.212)

on<-c(0.034, 0.026)
onu<-c(0.159 ,0.167)
onl<-c(-0.104, -0.118)
sd=0
labs<-c("DR Stream", "DR Lake")

png("Figures/Coefs_Conn.png", width=4.5, height=4, units='in', res=300)
#par(xpd=NA)
coefplot(nh4, sd, CI=1, nh4u, nh4l, varnames="", mar=c(0,4,4,1), ylim=c(-1, 1), cex.pts=2, v.axis=F, col.pts=rgb(168,221,181, 255, max=255),lwdInner=3, cex.axis=2, main="", vertical=F)
segments(1, nh4l[1], 1, nh4u[1], lwd=3, rgb(168,221,181, 255, max=255))
segments(2, nh4l[2], 2, nh4u[2], lwd=3, rgb(168,221,181, 255, max=255))



mtext(labs, at=c(1, 2), cex=1, side=1)
mtext("Coefficient +/- 95% CI", at=0, line=2, cex=1.2, side=2)
axis(2, cex=3, lwd=2)
coefplot(no3, sd, CI=1, no3l, no3u, add=T, offset=-.1, cex.pts=2, vertical=F, col.pts=rgb(44, 127, 184, 255, max=255))
segments(.9, no3l[1], .9, no3u[1], lwd=3, rgb(44, 127, 184, 255, max=255))
segments(1.9, no3l[2], 1.9, no3u[2], lwd=3, rgb(44, 127, 184, 255, max=255))


coefplot(on, sd, CI=1, onl, onu, add=T,vertical=F, cex.pts=2, col.pts=rgb(117,107,177, 255, max=255))
segments(1.1, onl[1], 1.1, onu[1], lwd=3, rgb(117,107,177, 255, max=255))
segments(2.1, onl[2], 2.1, onu[2], lwd=3, rgb(117,107,177, 255, max=255))


legend(.75, -.3, legend=c("NO3", "NH4", "OrgN"), bty="n", fill=c(rgb(44, 127, 184, 255, max=255), rgb(168,221,181, 255, max=255), rgb(117,107,177, 255, max=255)), cex=.9)

dev.off()

