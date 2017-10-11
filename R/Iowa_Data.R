##iowa data from Chris Filstrup (sent 10 October 2017). He says they started sending TKN data to the state in 2010 but he and John prefer TN so they kept measuring it for internal use. Both included in this file along with nitrate, ammonium.

#initial file includes N data - note units on NH4 vs other parameters
iowadata<-read.csv('Data/Iowa nitrogen data.csv', header=T)

#lose about half observations by omitting na's (pre-2010)
iowadata.nona<- na.omit(iowadata)

#save save as RDS - NAs still included
saveRDS(iowadata, file="Data/IowaNitrogenData.rds")
