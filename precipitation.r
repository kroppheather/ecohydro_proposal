library(lubridate)
library(plyr)
#read in precipitation data
datP <- read.csv("c:\\Users\\hkropp\\Google Drive\\proposal\\healy_precip.csv")

dateP <- as.Date(datP$DATE, "%m/%d/%Y")

datP$doy <- yday(dateP)
datP$year <- year(dateP)
datP$leap <- leap_year(datP$year)
datP$grow <- ifelse(datP$leap==TRUE&datP$doy>=122&datP$doy<=274,1,
					ifelse(datP$leap==FALSE&datP$doy>=121&datP$doy<=273,1,0))
#check that all measurements across years are present
yrL <- aggregate(datP$PRCP, by=list(datP$year),FUN="length")
#some years and days in years missing.

#start by focusing only on the May-Sept period
datG <- datP[datP$grow==1,]
#153 is the full dataset
#omit na from precip
datGP <- data.frame(PRCP=datG$PRCP, datG[,7:10])
datGP <- na.omit(datGP)
nG <- aggregate(datG$PRCP, by=list(datG$year),FUN="length")
colnames(nG)<- c("year","n")
#start by looking at the distribution of daily precipitation
#convert from inches to mm
hist(datGP$PRCP*25.4)

#calculate days between rain events
#first only take years with the full observations
datPD <- join(datGP,nG,by=c("year"), type="left" )
datP2 <- datPD[datPD$n==153,]

#get the number of uears
yrPD <- unique(datP2$year)

Pmat <- matrix(datPD$PRCP, byrow=FALSE, ncol=length(yrPD))