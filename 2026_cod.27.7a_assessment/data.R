# Prepare data, write CSV data tables

# Before:
# After:

library(icesTAF)

library(FLCore)

mkdir("data")

landings <- readVPAFile(taf.data.path("COD7ALA.txt"))
landings.n <- readVPAFile(taf.data.path("COD7ALN.txt"))
discards <- readVPAFile(taf.data.path("COD7ADI.txt"))
discards.n <- readVPAFile(taf.data.path("COD7ADN.txt"))
catch.wt <- readVPAFile(taf.data.path("COD7ACW.txt"))

# convert to cross tab format
la <- flr2taf(landings)
ln <- flr2taf(landings.n)
di <- flr2taf(discards)
dn <- flr2taf(discards.n)
lw <- flr2taf(catch.wt)


# write out cross tab csv files
write.taf(la, dir = "data")
write.taf(ln, dir = "data")
write.taf(di, dir = "data")
write.taf(dn, dir = "data")
write.taf(lw, dir = "data")

