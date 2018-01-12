library(lubridate)
library(plyr)
library(seas)
#read in precipitation data
datP <- read.csv("c:\\Users\\hkropp\\Google Drive\\proposal\\healy_precip.csv")

dateP <- as.Date(datP$DATE, "%m/%d/%Y")

datP$doy <- yday(dateP)
datP$year <- year(dateP)
datP$leap <- leap_year(datP$year)
datP$grow <- ifelse(datP$leap==TRUE&datP$doy>=122&datP$doy<=274,1,
					ifelse(datP$leap==FALSE&datP$doy>=121&datP$doy<=273,1,0))
					
datP$dateP <-dateP 					
#check that all measurements across years are present
yrL <- aggregate(datP$PRCP, by=list(datP$year),FUN="length")
#some years and days in years missing.

#start by focusing only on the May-Sept period
datG <- datP[datP$grow==1,]


#start by looking at the distribution of daily precipitation
#convert from inches to mm
test <-hist(datG$PRCP*25.4, prob=TRUE)

#calculate days between rain events
#first only take years with the full observations
DFsub <- data.frame(date=datG$dateP,precip=datG$PRCP*25.4)
Pint<- interarrival(DFsub)
Pint5<- interarrival(DFsub,p.cut=5)
#omit NA from missing data
DryInt <- na.omit(Pint$dry)
WetInt <- na.omit(Pint$wet)
#look at functional precip
DryInt5 <- na.omit(Pint5$dry)
WetInt5 <- na.omit(Pint5$wet)

hist(DryInt, prob=TRUE)
hist(WetInt, prob=TRUE)

test<-hist(DryInt5, prob=TRUE)

#exclude zeros in precip
datPP <- datG$PRCP[datG$PRCP>0]*25.4

#set up histograms
pbreak1 <- c(0,1,5,10,15,20,25,35,60)
pbreak2 <- c(0,1,3,5,7,10,14,21,28)
pbreak3 <- c(0,1,3,5,7,10,14,21,30,65)

pph <- hist(datPP, breaks=pbreak1)
ddh <- hist(DryInt, breaks=pbreak2)
dd5h <- hist(DryInt5, breaks=pbreak3)


#plot of precipitation pulses
wd <- 75
hd <- 35

x1 <- seq(1,length(pbreak1)-1)
x2 <- seq(1,length(pbreak2)-1)
x3 <- seq(1,length(pbreak3)-1)

jpeg("c:\\Users\\hkropp\\Google Drive\\proposal\\precip.jpg", width=2800, height=3200, units="px", quality=100)
layout(matrix(seq(1,3), ncol=1), width=rep(lcm(wd),3),height=rep(lcm(hd),3)) 
	par(mai=c(3,1,0,0))
	plot(c(0,1),c(0,1), xlim=c(0,length(pbreak1)), ylim=c(0,.6),xlab=" ",
		ylab=" ", axes=FALSE, xaxs="i",yaxs="i")
	for(i in 1:length(pph$counts)){
		polygon(c(x1[i]-1,x1[i]-1,x1[i],x1[i]),
			c(0,pph$counts[i]/sum(pph$counts),pph$counts[i]/sum(pph$counts),0),
			col="grey75")	
	}
	axis(1, x1-.5, rep(" ",length(pph$counts)),cex.axis=5, lwd.ticks=7)
	mtext(paste0(pbreak1[-length(pbreak1)]+1,"-",pbreak1[-1]), at= x1-.5,cex=5, side=1,line=5)
	axis(2, seq(0,.6, by=.2), cex.axis=7,lwd.ticks=7, las=2)
	mtext("Daily precipitation (mm)",side=1,cex=7, line=12)
	mtext("Proportion of occurance", side=2, cex=7, line=12)
	par(mai=c(3,1,0,0))
	plot(c(0,1),c(0,1), xlim=c(0,length(pbreak2)+1), ylim=c(0,.4),xlab=" ",
		ylab=" ", axes=FALSE, xaxs="i",yaxs="i")
		for(i in 1:length(ddh$counts)){
	polygon(c(x2[i]-1,x2[i]-1,x2[i],x2[i]),
			c(0,ddh$counts[i]/sum(ddh$counts),ddh$counts[i]/sum(ddh$counts),0),
			col="grey75")	
	}	
	axis(1, x2-.5, rep(" ",length(ddh$counts)),cex.axis=5, lwd.ticks=7)
	mtext(paste0(pbreak2[-length(pbreak2)]+1,"-",pbreak2[-1]), at= x2-.5,cex=5, side=1,line=5)
	axis(2, seq(0,.4, by=.1), cex.axis=7,lwd.ticks=7, las=2)
	mtext("Days between precipitation (>0.3mm)",side=1,cex=7, line=12)
	mtext("Proportion of occurance", side=2, cex=7, line=12)
	par(mai=c(3,1,0,0))
	plot(c(0,1),c(0,1), xlim=c(0,length(pbreak3)+1), ylim=c(0,.4),xlab=" ",
		ylab=" ", axes=FALSE, xaxs="i",yaxs="i")
		for(i in 1:length(dd5h$counts)){
	polygon(c(x3[i]-1,x3[i]-1,x3[i],x3[i]),
			c(0,dd5h$counts[i]/sum(dd5h$counts),dd5h$counts[i]/sum(dd5h$counts),0),
			col="grey75")	
	}
	axis(1, x3-.5, rep(" ",length(dd5h$counts)),cex.axis=5, lwd.ticks=7)
	mtext(paste0(pbreak3[-length(pbreak3)]+1,"-",pbreak3[-1]), at= x3-.5,cex=5, side=1,line=5)
	axis(2, seq(0,.4, by=.1), cex.axis=7,lwd.ticks=7, las=2)
	mtext("Days between precipitation (>5mm)",side=1,cex=7, line=12)
	mtext("Proportion of occurance", side=2, cex=7, line=12)
dev.off()	

