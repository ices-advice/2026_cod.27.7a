library(icesTAF)  #first need to create a new project...
taf.skeleton() #creates the file outline/directories

dir() #check what I created

# now copy all the available/necessary data files into folder bootstrap/initial/data . this can be csv file, text, or other files. then to crate the DATA.bib run the next line, which will create DATA.bib in the right place and then can edit the data.bib file manually. 
# important to run the draft.data command from the bootstrap (top folder) work space.


#once the DATA.bib file has been created, open this and edit it so it gives some extra information.
#create the data 
draft.data(file= TRUE)

draft.software("boot/initial/software/ss_win.exe",file="boot/SOFTWARE.bib")
### edit Data.dir
# everything gets converted into a .csv file. But here it already is in csv files...

#taf.roxygenise(
#files =
#c("")
#)

taf.bootstrap()

taf.bootstrap(software=TRUE)
#cleans/deletes all software and previously downloaded 

taf.bootstrap(taf=TRUE)

#read the lowestoft files with readVPAFile and then transform it into a TAF file with flr2taf.
# finally do write.taf

taf.bootstrap()
sourceAll()