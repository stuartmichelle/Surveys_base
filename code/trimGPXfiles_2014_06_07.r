################################################################
## Trim GPX files only to survey times
## (for plotting in Google Earth or QGIS)
################################################################
# Script to read in gpx files from Philippines 2013 fieldwork
setwd("/Users/mpinsky/Documents/Rutgers/Philippines/2014/Surveys")
source("readGPXGarmin_2014_06_07.R")
source("writeGPXGarmin_2013_06_02.R")
surv = read.csv("GPSSurveys2014.surveyinfo.csv") # data on surveys
options(digits=8)

# convert survey times to GMT
dates = as.character(surv$Date)
datesplit = unlist(strsplit(dates,"/", fixed=T))
startmonth = as.numeric(datesplit[seq(1,length(datesplit), by=3)])
endmonth = startmonth
startday = as.numeric(datesplit[seq(2,length(datesplit), by=3)])
endday = startday
year = 2000+as.numeric(datesplit[seq(3,length(datesplit), by=3)])
starttime = as.character(surv$StartTime)
starttimesplit = unlist(strsplit(starttime, ":", fixed=T))
starthour = as.numeric(starttimesplit[seq(1,length(starttimesplit), by=2)])
startmin = as.numeric(starttimesplit[seq(2,length(starttimesplit), by=2)])
endtime = as.character(surv$EndTime)
endtimesplit = unlist(strsplit(endtime, ":", fixed=T))
endhour = as.numeric(endtimesplit[seq(1,length(endtimesplit), by=2)])
endmin = as.numeric(endtimesplit[seq(2,length(endtimesplit), by=2)])
	
# Convert time to GMT
starthour = starthour - 8
i = starthour < 0; sum(i) # update if crossed midnight
startday[i] = startday[i] - 1
starthour[i] = starthour[i] + 24
i = startday < 1; sum(i) # make sure no days moved to previous month

i = endhour < 0; sum(i)
endhour = endhour - 8
endday[i] = endday[i] - 1
endhour[i] = endhour[i] + 24
i = endday < 1; sum(i) # make sure no days moved to previous month

starttimePX = strptime(paste(startmonth, startday, year, starthour, startmin), tz='GMT', format='%m %d %Y %H %M') # start times in POSIXlt format
endtimePX = strptime(paste(endmonth, endday, year, endhour, endmin), tz='GMT', format='%m %d %Y %H %M') # start times in POSIXlt format

# Read in each GPX file
files = list.files(path = 'gps', pattern="^Track.*gpx")
len = length(files)

for(i in 1:len){
	infile = readGPXGarmin(paste('gps/', files[i], sep=''))
	intimes = strptime(as.character(infile$data$time), tz = 'GMT', format = '%Y-%m-%dT%H:%M:%SZ') # start time of the GPS track in POSIXlt format
	instarttime = intimes[1] # start time for this GPX track
	inendtime = intimes[length(intimes)] # end time for this GPX track
	j = which(starttimePX >= instarttime & endtimePX <= inendtime) # find which survey fits within this GPX track's date & time
	if(length(j) == 0) stop(paste('no matching survey for', files[i]))
	k = which(intimes >= starttimePX[j] & intimes <= endtimePX[j]) # find the GPX points that fit within the survey
	outfile = list(header = infile$header, data = infile$data[k,])
	outfile$data$elev = 0 # set elevation to 0
	writeGPX(filename = paste('gps_trimmed/', files[i], sep=''), outfile = outfile)
}