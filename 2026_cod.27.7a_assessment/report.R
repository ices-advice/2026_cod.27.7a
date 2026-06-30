## Prepare plots and tables for report

## Before:
## After:


library(icesTAF)

mkdir("report")

library(FLCore)
library(FLAssess)
library(FLasher)
library(lattice)
library(data.table)
library(ss3sim)
library(r4ss)
library(ss3diags)
sessionInfo()

maindir <- maindir <- '.'

load("output/cod7a_sOSS3.Rdata")
stock<-stk
la <- readVPAFile(taf.data.path("COD7ALA.txt"))
ln <- readVPAFile(taf.data.path("COD7ALN.txt"))
di <- readVPAFile(taf.data.path("COD7ADI.txt"))
dn <- readVPAFile(taf.data.path("COD7ADN.txt"))
lw <- readVPAFile(taf.data.path("COD7ACW.txt"))


dw<-lw
sw<-lw
cn<-dn+ln

Advice<- 0 #advice for intermediate year
TAC<- 165  #TAC for intermediate year (2025)

FMSY<-0.171
FMSY_L<-0.141
FMSY_U<-0.22
FECO<-0.149
BLIM<-9364
BPA<-13012
FPA<-0.23
BTrig<-13012


SavePlot0<-function(plotname,width=6,height=4){
  file <- file.path("report/",paste0('cod7a_sum_',plotname,'.png')) 
  dev.print(png,file,width=width,height=height,units='in',res=300,pointsize=8)
}
#fill the stock object with landings and discard numbers and catchweights/stock weights
stock0 <- stock # keep a copy of the original stock object
#stock <- FLStock(ln)
landings(stock) <- la
discards(stock) <- di
catch(stock) <- la+di


landings.n(stock) <- ln
discards.n(stock)<- dn
catch.n(stock) <- dn+ln
## calculate landings fraction

landingsfraction <- ln/(ln+dn)
## handle NAs in older ages
landingsfraction[as.character(5:6),][is.na(landingsfraction[as.character(5:6),])] <- 1

## handle NAs in younger ages

landingsfraction[as.character(0:1),][is.na(landingsfraction[as.character(0:1),])] <- 0

landings.wt(stock) <- catch.wt(stock)
discards.wt(stock) <- catch.wt(stock)

catch.wt(stock)[(ln+dn)==0] <- 0 # fix divide by zero
stock@harvest@units <- 'f'
#stock@stock.wt<-stock@catch.wt

#stock@harvest<-stock0@harvest
stock@range[6]<-2
stock@range[7]<-4
fbarage <- 2:4

## Remove unwanted ages  # this needs to be done as the Stock object coming from the SS3 has got 11 ages, and the catches, landings and discards only go up to age 6+ 

stock_new <- trim(stock, age = 0:6) 

## Update plus-group data, and recalculate total catch, landings and discards

## Update plus-group data, and recalculate total catch, landings and discards

stock_new@catch.n["6",] <- quantSums(stock@catch.n[as.character(6:10),])

stock_new@landings.n[]  <- stock_new@catch.n * landingsfraction[as.character(0:6)]

stock_new@discards.n[]  <- stock_new@catch.n * (1-landingsfraction[as.character(0:6)])

stock_new@stock.n["6",] <- quantSums(stock@stock.n[as.character(6:10),])
catch(stock_new)    <- computeCatch(stock_new)
landings(stock_new) <- computeLandings(stock_new)
discards(stock_new) <- computeDiscards(stock_new)
stock(stock_new)    <- computeStock(stock_new)

stock<-stock_new
save(stock, file = 'cod7a_sOSS3ages0to6.Rdata')
#  Handy parameters to have for later
years <- stock@range[4]:stock@range[5]
nyears <- length(years)
ages <- stock@range[1]:stock@range[2]
nages <- length(ages)
fbarage <- stock@range[6]:stock@range[7]
rps <- FLPar(Harvest=NA, Catch=NA, Rec=NA, SSB=NA)

save(stock,file="report/cod7a_forecast.Rdata")



p <- apply(landings.n(stock)/catch.n(stock),1,mean,na.rm=T)
p <- c(ifelse(is.na(p),0,p))
# A function to pull out the data for the catch options from the stf
catchoptions <- function(Basis='',SSBint=NULL,TAC=NULL) {
  out <- data.frame(
    Basis
    ,Catch=round(c(landings(stf1)[,nyears+2]+discards(stf1)[,nyears+2]))
    ,Land=round(c(landings(stf1)[,nyears+2]))
    ,Dis=round(c(discards(stf1)[,nyears+2]))
    ,FCatch=round(mean(harvest(stf1)[as.character(fbarage),nyears+2]),5)
    ,FLand=round(mean((harvest(stf1)*landings.n(stf1)/catch.n(stf1))[as.character(fbarage),nyears+2]),5)
    ,FDis=round(mean((harvest(stf1)*discards.n(stf1)/catch.n(stf1))[as.character(fbarage),nyears+2]),5)
    ,SSB=round(c(ssb(stf1)[,nyears+3]),5)
    ,dSSB=paste0(round(100*(round(c(ssb(stf1)[,nyears+3]),5)-SSBint)/SSBint,5),"%")
    ,dTac=paste0(round(100*(round(c(landings(stf1)[,nyears+2]+discards(stf1)[,nyears+2]))-TAC)/TAC,5),"%")
    ,dAdvice=paste0(round(100*(round(c(landings(stf1)[,nyears+2]+discards(stf1)[,nyears+2]))-Advice)/Advice,5),"%")
  )
  names(out) <- paste0(names(out),c(rep(max(years)+2-2000,3),'',rep(max(years)+2-2000,3),max(years)+3-2000,'',''))
  return(out)
}
#set up empty stock object for forecast
stf0 <- stf(stock,nyears=3, wts.nyears=3, fbar.nyears=3)

# geometric mean recruitment, minus last 2 years and set up forecast recruitment function
GM <- round(exp(mean(log(stock.n(stock)[1,ac(2016:2023)]))),0) # updated the geometric mean to shorter time period
stock.n(stf0)[1,nyears+1] <- GM
stock.n(stf0)[1,nyears+2] <- GM
stock.n(stf0)[1,nyears+3] <- GM
srr <- as.FLSR(stf0, model="geomean")
params(srr)['a',] <- GM
params(srr)

#Calculate F status quo, (last 3 years) and do status-quo forecast
#

#Calculate F status quo, (last 3 years) and do status-quo forecast
#
fsq1<-mean(harvest(stf0)[as.character(fbarage),ac(2023)])
fsq2<-mean(harvest(stf0)[as.character(fbarage),ac(2024)])
fsq3<-mean(harvest(stf0)[as.character(fbarage),ac(2025)])
fsq<-(fsq1+fsq2+fsq3)/3
#Summarise the basis for the catch options

ctrl <- fwdControl(data.frame(year=max(years)+1:3,value=c(fsq,fsq,0),quant='f'))
stf1  <- fwd(stf0, control=ctrl,sr=srr)

round(fsq,3) # F status-quo (assumption for intermediate year)
SSB2026 <- round(c(ssb(stf1)[,nyears+1]),0)
SSB2026
SSBint <- round(c(ssb(stf1)[,nyears+2]),0)
SSBint #SSB at the start of the year after the intermediate year (or the end of the intermediate year)
GM # recruitment assumption (geometric mean)
round(c(landings(stf1)[,nyears+1]))+round(c(discards(stf1)[,nyears+1])) # catch intermediate year
round(c(landings(stf1)[,nyears+1])) # landings intermediate year
round(c(discards(stf1)[,nyears+1])) # discards intermediate year
F2026<-mean(harvest(stf1)[as.character(fbarage),nyears])
F2027<-mean(harvest(stf1)[as.character(fbarage),nyears+1])
F2026
F2027
fsq<-mean(harvest(stf1)[as.character(fbarage),nyears+1])

# create management options table
out <- NULL
ctrl <- fwdControl(data.frame(year=max(years)+1:3,value=c(fsq,FMSY_L,0),quant='f'))
stf1  <- fwd(stf0, control=ctrl,sr=srr)
out <- rbind(out,catchoptions('FMSYLower',SSBint,TAC))

fbar(stf1)

ctrl <- fwdControl(data.frame(year=max(years)+1:3,value=c(fsq,FMSY_U,0),quant='f'))
stf1  <- fwd(stf0, control=ctrl,sr=srr)
out <- rbind(out,catchoptions('FMSYUpper',SSBint,TAC))

ctrl <- fwdControl(data.frame(year=max(years)+1:3,value=c(fsq,FMSY,0),quant='f'))
stf1  <- fwd(stf0, control=ctrl,sr=srr)
out <- rbind(out,catchoptions('FMSY',SSBint,TAC))

ctrl <- fwdControl(data.frame(year=max(years)+1:3,value=c(fsq,FMSY/BTrig*SSBint,0),quant='f'))
stf1  <- fwd(stf0, control=ctrl,sr=srr)
out <- rbind(out,catchoptions('scaled FMSY',SSBint,TAC))

ctrl <- fwdControl(data.frame(year=max(years)+1:3,value=c(fsq,FMSY_U/BTrig*SSBint,0),quant='f'))
stf1  <- fwd(stf0, control=ctrl,sr=srr)
out <- rbind(out,catchoptions('scaled FMSY_upper',SSBint,TAC))

ctrl <- fwdControl(data.frame(year=max(years)+1:3,value=c(fsq,FMSY_L/BTrig*SSBint,0),quant='f'))
stf1  <- fwd(stf0, control=ctrl,sr=srr)
out <- rbind(out,catchoptions('scaled FMSY_lower',SSBint,TAC))

ctrl <- fwdControl(data.frame(year=max(years)+1:3,value=c(fsq,0,0),quant='f'))
stf1  <- fwd(stf0, control=ctrl,sr=srr)
#save(stf1,file=file.path(outdir,'cod7a_forecast_stf1.Rdata')) #this is now needed for the new advice request. run once decision for option to take forward has been decided.
out <- rbind(out,catchoptions('F = 0',SSBint,TAC))

ctrl <- fwdControl(data.frame(year=max(years)+1:3,value=c(fsq,FPA,0),quant='f'))
stf1  <- fwd(stf0, control=ctrl,sr=srr)
out <- rbind(out,catchoptions('F = Fpa',SSBint,TAC))


ctrl <- fwdControl(data.frame(year=max(years)+1:3,value=c(fsq,BLIM,0),quant=c('f','ssb_flash','f')))
stf1  <- fwd(stf0, control=ctrl,sr=srr)
out <- rbind(out,catchoptions('Blim',SSBint,TAC))

ctrl <- fwdControl(data.frame(year=max(years)+1:3,value=c(fsq,BPA,0),quant=c('f','ssb_flash','f')))
stf1  <- fwd(stf0, control=ctrl,sr=srr)
out <- rbind(out,catchoptions('Bpa',SSBint,TAC))

ctrl <- fwdControl(data.frame(year=max(years)+1:3,value=c(fsq,BTrig,0),quant=c('f','ssb_flash','f')))
stf1  <- fwd(stf0, control=ctrl,sr=srr)
out <- rbind(out,catchoptions('Btrig',SSBint,TAC))

ctrl <- fwdControl(data.frame(year=max(years)+1:3,value=c(fsq,fsq,0),quant=c('f','f','f')))
stf1  <- fwd(stf0, control=ctrl,sr=srr)
out <- rbind(out,catchoptions('Fsq',SSBint,TAC))

ctrl <- fwdControl(data.frame(year=max(years)+1:3,value=c(fsq,SSBint,0),quant=c('f','ssb_flash','f')))
stf1  <- fwd(stf0, control=ctrl,sr=srr)
out <- rbind(out,catchoptions('SSB2027=SSB2028',SSBint,TAC))

ctrl <- fwdControl(data.frame(year=max(years)+1:3,value=c(fsq,165,0),quant=c('f','catch','f')))
stf1  <- fwd(stf0, control=ctrl,sr=srr)
out <- rbind(out,catchoptions('TAC Rollover',SSBint,TAC))


ctrl <- fwdControl(data.frame(year=max(years)+1:3,value=c(fsq,TAC,0),quant=c('f','catch','f')))
stf1  <- fwd(stf0, control=ctrl,sr=srr)
out <- rbind(out,catchoptions('unavoidable catch',SSBint,TAC))

ctrl <- fwdControl(data.frame(year=max(years)+1:3,value=c(fsq,FECO,0),quant='f'))
stf1  <- fwd(stf0, control=ctrl,sr=srr)
out <- rbind(out,catchoptions('F = FECO',SSBint,TAC))

ctrl <- fwdControl(data.frame(year=max(years)+1:3,value=c(fsq,FECO/BTrig*SSBint,0),quant='f'))
stf1  <- fwd(stf0, control=ctrl,sr=srr)
out <- rbind(out,catchoptions('scaled F = FECO',SSBint,TAC))


out$FLand26[out$FLand27=="NaN"]<-0
out$FDis26[out$FDis27=="NaN"]<-0
#out[,5:10]<-icesRound(out[,5:10])



write.csv(out,'report/Cod7a_ManOpts2.csv',row.names=F)


#### forecast tables

ctrl <- fwdControl(data.frame(year=max(years)+1:3,value=c(fsq,fsq,fsq),quant=c('f','f','f')))
stf1  <- fwd(stf0, control=ctrl, sr=srr) # status-quo forecast

stfin <- function(i){
  out <- data.frame(Age=ages
                    ,N=round(c(stock.n(stf1)[,i]))
                    ,M=c(m(stf1)[,i])
                    ,Mat=c(mat(stf1)[,i])
                    ,PF=c(harvest.spwn(stf1)[,i])
                    ,PM=c(m.spwn(stf1)[,i])
                    ,SWt=round(c(stock.wt(stf1)[,i]),3)
                    ,Sel=round(c(harvest(stf0)[,i]*p),3)
                    ,CWt=round(c(landings.wt(stf1)[,i]),3)
                    ,DSel=round(c(harvest(stf0)[,i]*(1-p)),3)
                    ,DCWt=round(c(discards.wt(stf1)[,i]),3)
  )
  return(out)
}

stfin1 <- stfin(nyears+1)
stfin2 <- stfin(nyears+2)
stfin3 <- stfin(nyears+3)



write.csv(stfin1,'report/cod7a_stfin1.csv',row.names=F)
write.csv(stfin2,'report/cod7a_stf_stfin2.csv',row.names=F)
write.csv(stfin3,'report/cod7a_stf_stfin3.csv',row.names=F)
#output tables
stfout <- function(i){
  out <- data.frame(Age=ages
                    ,F=round(c(harvest(stf1)[,i])*p,3)
                    ,CatchNos=round(c(landings.n(stf1)[,i]))
                    ,Yield=round(c((landings.n(stf1)*landings.wt(stf1))[,i]),0)
                    ,DF=round(c(harvest(stf1)[,i])*(1-p),3)
                    ,DCatchNos=round(c(discards.n(stf1)[,i]))
                    ,DYield=round(c((discards.n(stf1)*discards.wt(stf1))[,i]),0)
                    ,StockNos=round(c(stock.n(stf1)[,i]))
                    ,Biomass=round(c((stock.n(stf1)*stock.wt(stf1))[,i]))
                    ,SSNos=round(c((stock.n(stf1)*mat(stf1))[,i]))
                    ,SSB=round(c((stock.n(stf1)*stock.wt(stf1)*mat(stf1))[,i])),3
  )
  out <- rbind(out,colSums(out))
  nrows <- nrow(out)
  out[nrows,1] <- 'Total'
  out[nrows,2] <- round(mean((harvest(stf1)[,i]*p)[as.character(fbarage)]),3)
  out[nrows,5] <- round(mean((harvest(stf1)[,i]*(1-p))[as.character(fbarage)]),3)
  return(out)
}

stfout1 <- stfout(nyears+1)
stfout2 <- stfout(nyears+2)
stfout3 <- stfout(nyears+3)


write.csv(stfout1,'report/cod7a_stf_stfout1.csv',row.names=F)
write.csv(stfout2,'report/cod7a_stf_stfout2.csv',row.names=F)
write.csv(stfout3,'report/cod7a_stf_stfout3.csv',row.names=F)

#contributions plot

par(mfrow=c(1,2),mar=c(5,8,4,1),cex=0.8)
nrows <- nrow(stfout2)
yield <- stfout2[-nrows,'Yield']
prop <- paste0(round(100*yield/sum(yield)),'%')
labels <- paste(max(years)-ages+2,rep(c('GM','SS'),c(2,nages-2)))
b <- barplot(yield,horiz=T,names=labels,las=1,xlab='Tonnes',main=paste('Landings yield',max(years)+2),xlim=c(0,max(yield, na.rm = T)*1.25))
text(yield,b,prop,adj=-0.2)

ssb <- stfout3[-nrows,'SSB']
prop <- paste0(round(100*ssb/sum(ssb)),'%')
labels <- paste(max(years)-ages+3,rep(c('GM','SS'),c(3,nages-3)))
b <- barplot(ssb,horiz=T,names=labels,las=1,xlab='Tonnes',main=paste('SSB',max(years)+3),xlim=c(0,max(ssb,na.rm = T)*1.25))
text(ssb,b,prop,adj=-0.2)
a <- SavePlot0('stf_contrib',6,3)






