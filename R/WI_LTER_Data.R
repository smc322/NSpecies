##lter data downloaded from here:
#https://lter.limnology.wisc.edu/dataset/north-temperate-lakes-lter-chemical-limnology-primary-study-lakes-nutrients-ph-and-carbon-19
#note that the tkn and some nitrate variables have "sloh" on the column - those are samples that were analyzed by State Lab of Hygeine, so if there are two different no3no2 or nh4, that's samples analyzed at UW (along with TN) vs at SLOH (along with TKN)

#initial file includes various carbon/nutrient parameters
lterdata<-read.csv('Data/ntl1_v5.csv', header=T)

#subset to include nitrogen data only
nitrogen<-lterdata[,c(1:8,16:18,20,27:29)]

#subset for only tn and tkn columns
tntkn<-nitrogen[,c(1:8,12,15)]

#omit na's to limit to rows that have data for all nitrogen related variables
nitrogen.nona<-na.omit(nitrogen)

#omit na's to limit to rows that have data for TN and TKN
tntkn.nona<-na.omit(tntkn)

#save nitrogen - all N parameters with NAs still included
saveRDS(nitrogen, file="Data/LTERNitrogenData.rds")
