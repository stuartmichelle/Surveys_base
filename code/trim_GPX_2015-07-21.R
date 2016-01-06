# setwd("/Volumes/Philippines Data Drive/Surveys")

################################################################
## Trim GPX files only to survey times
## (for plotting in Google Earth)
## 2015-01-08: modified to handle 5 different GPSs
################################################################
# Script to read in gpx files from Philippines 2013 fieldwork
source("code/readGPXGarmin_2014_06_07.R")
source("code/writeGPXGarmin_2013_06_02.R")
surv = read.csv("GPSSurveys2015_05.surveyinfo.csv") # data on surveys
options(digits=8)

# trim off lines that have no DiveNum and columns that have no header (export problem from Excel where it includes too much of the sheet)
surv = surv[!is.na(surv$DiveNum), !grepl('^X', names(surv))]

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
endhour = endhour - 8 # eight hour offset
endday[i] = endday[i] - 1
endhour[i] = endhour[i] + 24
i = endday < 1; sum(i) # make sure no days moved to previous month

starttimePX = strptime(paste(startmonth, startday, year, starthour, startmin), tz='GMT', format='%m %d %Y %H %M') # start times in POSIXlt format
endtimePX = strptime(paste(endmonth, endday, year, endhour, endmin), tz='GMT', format='%m %d %Y %H %M') # start times in POSIXlt format

# Split starttimes and endtimes by GPS
starttimePXs = vector('list', length=5)
endtimePXs = vector('list', length=5)
for(i in 1:5){
	starttimePXs[[i]] = starttimePX[surv$GPS == i]
	endtimePXs[[i]] = endtimePX[surv$GPS == i]	
}

# Read in each GPX file
files = vector('list', length=5)
len = numeric(5)
files[[1]] = list.files(path = 'gpx1', pattern="^Track.*gpx")
len[1] = length(files[[1]])
files[[2]] = list.files(path = 'gpx2', pattern="^Track.*gpx")
len[2] = length(files[[2]])
files[[3]] = list.files(path = 'gpx3', pattern="^Track.*gpx")
len[3] = length(files[[3]])
files[[4]] = list.files(path = 'gpx4', pattern="^Track.*gpx")
len[4] = length(files[[4]])
files[[5]] = list.files(path = 'gpx5', pattern="^Track.*gpx")
len[5] = length(files[[5]])

options(warn=1) # print as the occur
for(j in 1:length(files)){ # loop through each GPS
	print(paste('j=', j))
	if(len[j]>0){
		for(i in 1:len[j]){ # loop over each file in this folder
			print(i)
			infile = readGPXGarmin(paste('gpx', j, '/', files[[j]][i], sep=''))
			intimes = strptime(as.character(infile$data$time), tz = 'GMT', format = '%Y-%m-%dT%H:%M:%SZ') # start time of the GPS track in POSIXlt format
			instarttime = intimes[1] # start time for this GPX track
			inendtime = intimes[length(intimes)] # end time for this GPX track
			jj = which(starttimePXs[[j]] >= instarttime & endtimePXs[[j]] <= inendtime) # find which survey from this GPS fits within this GPX track's date & time
			if(length(jj) == 0){ 
				warning(paste('no matching survey for complete gpx', j, '/', files[[j]][i], '. Looking for incomplete GPX track.', sep=''))
				jj = which((starttimePXs[[j]] >= instarttime & starttimePXs[[j]] <= inendtime) | (starttimePXs[[j]] <= instarttime & endtimePXs[[j]] >= inendtime) | (endtimePXs[[j]] >= instarttime & endtimePXs[[j]] <= inendtime)) # find which survey from this GPS starts within this GPX track's date & time, or starts and ends around the GPX track, or ends within this GPX track
				if(length(jj) == 0){
					warning(paste('		no matching survey for incomplete gpx', j, '/', files[[j]][i], sep=''))
				}

			}
			if(length(jj)>0){
				for(ii in 1:length(jj)){
					k = which(intimes >= starttimePXs[[j]][jj[ii]] & intimes <= endtimePXs[[j]][jj[ii]]) # find the GPX points that fit within the survey
					outfile = list(header = infile$header, data = infile$data[k,])
					outfile$data$elev = 0 # set elevation to 0
					filenm = paste('gpx_trimmed/GPS', j, '_', files[[j]][i], sep='')
					if(length(jj)>1){
						parts = unlist(strsplit(files[[j]][i], split='.', fixed=TRUE))
						filenm = paste('gpx_trimmed/GPS', j, '_', parts[1], '_', ii, '.gpx', sep='')
					}
					#print(filenm)
					writeGPX(filename = filenm, outfile = outfile)
				}
			}
		}
	}
}
