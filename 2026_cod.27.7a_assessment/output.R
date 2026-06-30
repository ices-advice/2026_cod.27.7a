## Extract results of interest, write TAF output tables

## Before:
## After:

library(icesTAF)

library(r4ss)
library(ss3diags)
library(ss3sim)

mkdir("output")

# copy
cp("model/cod7a_sOSS3.Rdata", "output", move = TRUE)


#read in the files
mod1 <- SS_output(dir="model/run") #get the model outputs

#retroModels <- SSgetoutput(dirvec = file.path("model/retrospectives", paste0("retro", 0:-5)))
# retrofiles are currently not possible
# make standard plots
SS_plots(
  mod1,
  uncertainty = TRUE, png = TRUE,
  forecastplot = TRUE, fitrange = TRUE,
  parrows = 5, parcols = 4,
  showpost = TRUE, showprior = TRUE
) ## png allows you to save them as png files in a dedicated folder or use pdf (,pdf=T)

# copy to output folder
cp("model/run/plots", "output")

# creates plenty of plots
# retroSummary <- SSsummarize(retroModels)

#do extra plots ....
ssb<-SSplotTimeseries(mod1,subplot=7)
rec<-SSplotTimeseries(mod1,subplot=11)
SSplotSummaryF(mod1)
SSplotCatch(mod1,subplot=1)
sspar(mfrow = c(2, 2), plot.cex = 0.8)
rtr<-SSplotRunstest(mod1,add=T,verbose = F)
rtr
SSplotJABBAres(mod1, add=T, verbose=F)
sspar(mfrow = c(2, 3), plot.cex = 0.8)
rta<-SSplotRunstest(mod1, add=T, subplots = "age", verbose = F)
rta
SSplotJABBAres(mod1,add=T,subplot = "age", verbose=F)
sspar(mfrow = c(1, 2), plot.cex = 0.8)
#rb = SSplotRetro(retroSummary, add = T, forecast = F, legend = F, verbose = F)
#rf = SSplotRetro(retroSummary, add = T, subplots = "F", ylim = c(0, 0.4), forecast = F,
                 
#                 legendloc = "topleft", legendcex = 0.8, verbose = F)
#sspar(mfrow = c(1, 2), plot.cex = 0.8)
#rb = SSplotRetro(retroSummary, add = T, forecast = T, legend = F, verbose = F, xmin = 2000)
#rf = SSplotRetro(retroSummary, add = T, subplots = "F", ylim = c(0, 0.4), forecast = T, legendloc = "topleft", legendcex = 0.8, verbose = F, xmin = 2000)

#SShcbias(retroSummary, quant = "SSB", verbose = F)
#SShcbias(retroSummary, quant = "F", verbose = F)
#sspar(mfrow=c(2,2),plot.cex=0.8)
#hci=SSplotHCxval(retroSummary,add=T,verbose=F,ylimAdj = 1.3)


#another way to summarize needed for age
#retroC<-SSretroComps(retroModels)

#sspar(mfrow=c(2,2),plot.cex=0.8)
#hcl=SSplotHCxval(retroC,subplots="age",add=T,verbose=F, ylimAdj=1.3, indexselect = c(1,2,3))

