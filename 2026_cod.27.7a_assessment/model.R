# Run analysis, write model results

# Before:
# After:

library(icesTAF)
library(r4ss)
library(ss3om)

mkdir("model")
#------------------------------------------------------------------------------------
# Create the directory structure where the different model runs will be executed
#------------------------------------------------------------------------------------


# copy model executable
cp(taf.boot.path("software", "ss_win.exe"), "model/run")
# copy data files
cp(taf.boot.path("data", "starter.ss"), "model/run")
cp(taf.boot.path("data", "forecast.ss"), "model/run")
cp(taf.boot.path("data", "control.ss"), "model/run")
cp(taf.boot.path("data", "wtatage.ss"), "model/run")
cp(taf.boot.path("data", "Irishcod_dat.txt"), "model/run")

# this is the folder I have to run ASAP from, after I copied the .exe files there.
rootwd <- setwd("model/run")
# run the SS3 model in the model/run folder
system("ss_win.exe")# runs stock synthesis in this folder.
# move back to root folder
setwd(rootwd)

# do the retrospective
# there is currently an issue wit the retrospective, so this gets removed for now...

#retro(masterdir = file.path(rootwd, "model"), oldsubdir = "run", exe = "ss_win", verbose = TRUE)

# read output into FLR and save
stk <- readFLSss3("model/run")
range(stk)["minfbar"] <- 2 #set range to ages 2-4
range(stk)["maxfbar"] <- 4

save(stk, file = 'model/cod7a_sOSS3.Rdata')
