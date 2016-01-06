# For 2015 surveys
# 2015-01-09 Modified to handle 5 GPSs. Malin and Michelle.
# 2015-06-03 Modified to handle input from one of any of the gps units. Michelle

#setwd("~/Desktop/2015_01_conv")
#setwd('/Users/mpinsky/Documents/Rutgers/Philippines/2015-05/Surveys/')
setwd('/Volumes/Philippines Data Drive/Surveys/')
################################################################
## Concatentate GPX track files together into one spreadsheet
## (for use later in the script)
################################################################
# Script to read in gpx files from Philippines 2015 fieldwork
source("code/readGPXGarmin_2014_06_07.R")
options(digits=22)

# Create a list to hold files from the 5 gps units
files = vector('list', length=5)
len = numeric(5)

#For each of the gps units, check to see if files exist and if they do, add them to the list
#if (file.exists('/gpx1/^Track.*gpx'))
	files[[1]] = list.files(path = 'gpx1', pattern="^Track.*gpx")
	len[1] = length(files[[1]])
#if (file.exists('/gpx2/^Track.*gpx'))
	files[[2]] = list.files(path = 'gpx2', pattern="^Track.*gpx")
	len[2] = length(files[[2]])
#if (file.exists("/gpx3/NA"))
	files[[3]] = list.files(path = 'gpx3', pattern="^Track.*gpx")
	len[3] = length(files[[3]])
#if (file.exists('/gpx4/^Track.*gpx'))
	files[[4]] = list.files(path = 'gpx4', pattern="^Track.*gpx")
	len[4] = length(files[[4]])
#if (file.exists('/gpx5/^Track.*gpx'))
	files[[5]] = list.files(path = 'gpx5', pattern="^Track.*gpx")
	len[5] = length(files[[5]])


# read in a file to get the column names
if(any(len>0)){
	infolder <- min(which(len>0)) # find the first gpx folder with a file in it
	infile = readGPXGarmin(paste("gpx",infolder, '/', files[[infolder]][1], sep='')) # read in the first file
} else {
	stop('Could not find any gpx files')
}

# create a dataframe to hold all GPS data
latlong = data.frame(GPS = numeric(0)) # will hold all lat/lon data from the GPSs
for(i in 1:length(names(infile$data))) latlong[[names(infile$data)[i]]]=numeric(0) # create the rest of the column names in latlong

# fill the dataframe
#for(j in 1:5){
for(j in 3:3){	
	for(i in 1:len[j]){

		infile = readGPXGarmin(paste('gpx', j, '/', files[[j]][i], sep=''))
		infile$data$GPS = j

		latlong = rbind(latlong, infile$data)
		print(dim(latlong)) # a little progress indicator
	}
}

# Remove duplicates
i = duplicated(latlong)
	sum(i)
latlong = latlong[!i,]
dim(latlong)
	
# Sort the records by time
permut = order(latlong$year, latlong$month, latlong$day, latlong$hour, latlong$min, latlong$sec)
latlong = latlong[permut,]

write.csv(latlong, file=(paste("output/track.concat", Sys.Date(), ".csv", sep="")))

######################################################
## Surveys and Collections: Match times to locations
######################################################

####### Add lat/long to survey data
surv = read.csv('GPSSurveys2015_05.surveyinfo.csv')
data = read.csv('GPSSurveys2015_05.clownfish.csv', stringsAsFactors=FALSE)
concatfiles = list.files(path = 'output', pattern="^track.concat.*.csv") # find all concat files
concatfiles = concatfiles[order(concatfiles, decreasing = TRUE)] # order them by date, with newest one first
latlong = read.csv(paste('output/', concatfiles[1], sep=''), row.names=1) # read in neweset track.concat file


# trim off lines that have no DiveNum and columns that have no header (export problem from Excel where it includes too much of the sheet)
surv = surv[!is.na(surv$DiveNum), !grepl('^X', names(surv))]
data = data[!is.na(data$DiveNum), !grepl('^X', names(data))]

# a small amount of QA/QC
print(paste('List of divers from DiveInfo sheet:', paste(sort(unique(surv$Divers)), collapse='; ')))
print(paste('List of divers from Clownfish sheet:', paste(sort(unique(data$Collector)), collapse='; ')))


# prep columns in data that we will add to
names = names(data)
data$NumFish = NA # total number of fish (dominant species)
data$Sizes = NA # concatenated sizes of fish (dominant species)
data$lat = NA
data$lon = NA

# add survey name and other dive information to data
data = merge(data, surv[,c('DiveNum', 'Date', 'Name', 'Municipality', 'GPS', 'Cover')])

# remove lines that weren't observed anemones
i = data$AnemSpp == ''
if(any(data$Spp[i]!='')) stop('lacking anemone for a sample')
data = data[!i,]
	
# combine multiple samples from the same anemone. store later samples with the first sample
# at the moment, I make not attempt to average size, depth, or location data across multiple visits
# THIS CODE HASN'T BEEN CHECKED YET
dups = data$AnemID[duplicated(data$AnemID) & !is.na(data$AnemID)] # anemone IDs that appear on >1 line
if(length(dups)>0) warning(paste('DUPLICATE ANEMONES HAVE BEEN RECORDED! CHECK CAREFULLY', paste(dups, collapse=', ')))
#if(length(dups)>0){
#	for(i in 1:length(dups)){
#		inds = sort(which(data$AnemID == dups[i]))
#		if(length(inds)<3){
#			# get data from the second visit to the anemone
#			newsizes = data[inds[2], c('Size1', 'Size2', 'Size3', 'Size4', 'Size5', 'Size6', 'Size7')]
#				newsizes = as.numeric(newsizes[!(newsizes == '' | is.na(newsizes))])
#			newcols = data[inds[2], c('Col1', 'Col2', 'Col3', 'Col4', 'Col5', 'Col6', 'Col7')]
#				newcols = newcols[!(newcols == '' | is.na(newcols))]
#			newids = data[inds[2], c('ID1', 'ID2', 'ID3', 'ID4', 'ID5', 'ID6', 'ID7')]
#				newids = as.numeric(newids[!(newids == '' | is.na(newids))])
#
#			# make sure there is data to add
#			if(length(newsizes)>0 | length(newcols)>0 | length(newids)>0){
#		
#				# error checking
#				if(length(newsizes) != length(newcols)){
#					warning(paste('size and color vector lengths do not match, i=', i, '. Filling with NAs.'))
#					newcols = c(newcols, rep(NA, length(newsizes)-length(newcols)))
#				}
#				if(length(newsizes) != length(newids)) {
#					warning(paste('size and ID vector lengths do not match, i=', i, '. Filling with NAs.'))
#					newids = c(newids, rep(NA, length(newsizes)-length(newids)))			
#				}
#					
#				# append to the first anemone record
#				ii = min(which(is.na(data[inds[1], c('Size1', 'Size2', 'Size3', 'Size4', 'Size5', 'Size6', 'Size7')]))) # index of first empty field on the row
#				if(ii<7){ # only have 7 columns to add into, so can only append on simply if at least one cell is free
#					for(j in ii:min(7,ii+length(newsizes)-1)){ # only have 7 columns, so don't go beyond
#						ct = j - ii + 1 # index into newsizes, newcols, newids
#						sznm = paste('Size', j, sep=''); clnm = paste('Col', j, sep=''); idnm = paste('ID', j, sep='');
#						data[inds[1], sznm] = newsizes[ct]
#						data[inds[1], clnm] = newcols[ct]
#						data[inds[1], idnm] = newids[ct]
#		
#					}
#					if(ii+length(newsizes)-1 > 7){ # if need to fill the 7th column with >1 individual
#						data$Size7[inds[1]] = paste(data$Size7[inds[1]], paste(newsizes[(ct+1):length(newsizes)], collapse=','), sep=',') # paste the remaining individuals together
#						data$Col7[inds[1]] = paste(data$Col7[inds[1]], paste(newcols[(ct+1):length(newcols)], collapse=','), sep=',')
#						data$ID7[inds[1]] = paste(data$ID7[inds[1]], paste(newids[(ct+1):length(newids)], collapse=','), sep=',')
#					}
#				} else {
#					stop('need to deal with case of 7 cols already filled')
#				}
#				# remove inds[2] from data
#				data = data[-inds[2],]
#			}			
#		} else {
#			stop('need to deal with case of >2 sampling times')
#		}
#	}
#}

# process data for each anemone
len = nrow(data)
for(i in 1:len){
	#Get date and time information for the anemone
	date = as.character(data$Date[i])
	datesplit = strsplit(date,"/", fixed=T)[[1]]
	month = as.numeric(datesplit[1])
	day = as.numeric(datesplit[2])
	time = as.character(data$ObsTime[i])
	timesplit = strsplit(time, ":", fixed=T)[[1]]
		timesplit = gsub('PM|AM', '', timesplit) # strip out AM/PM
	hour = as.numeric(timesplit[1])
	min = as.numeric(timesplit[2])
	sec = as.numeric(timesplit[3])

	# Convert time to GMT
	hour = hour - 8
	if(hour <0){
		day = day-1
		hour = hour + 24
	}
	if(day == 0){ # only works because we are in January/February
		day == 31
		month = month-1
	}

	# Find the location records that match the date/time stamp (to nearest second)
	latlongindex = which(latlong$month == month & latlong$day == day & latlong$hour == hour & latlong$min == min & latlong$GPS == data$GPS[i])
	i2 = which.min(abs(latlong$sec[latlongindex] - sec))

	# Calculate the lat/long for this time
	if(length(i2)>0){
		data$lat[i] = latlong$lat[latlongindex][i2]
		data$lon[i] = latlong$long[latlongindex][i2]
	}

	# Add the total number of fish (only for dominant spp)
	data$NumFish[i] = sum(c(!is.na(data$Size1[i]), !is.na(data$Size2[i]), !is.na(data$Size3[i]), !is.na(data$Size4[i]), !is.na(data$Size5[i]), !is.na(data$Size6[i])))
	if(!is.na(data$Size7[i])){
		num7 = length(unlist(strsplit(as.character(data$Size7[i]), split=','))) # number of fish listed in Size7
		data$NumFish[i] = data$NumFish[i] + num7
	}

	# Add the size of fish (only for dominant spp)
	temp = c(data$Size1[i], data$Size2[i], data$Size3[i], data$Size4[i], data$Size5[i], data$Size6[i])
	temp = temp[!is.na(temp)]
	temp = paste(temp, collapse=',')
	if(!is.na(data$Size7[i])){ # parse Size7 if needed
		temp = paste(temp, data$Size7[i], sep=',')
	}
	data$Sizes[i] = temp
}


# Sort the data
permut = order(data$DiveNum, data$ObsTime)
data = data[permut,]
row.names(data) = 1:nrow(data)

# Examine the head and tail of the data
head(data[,c('DiveNum', 'ObsTime', 'AnemSpp', 'Spp', 'NumFish', 'Sizes', 'lat', 'lon')])
tail(data[,c('DiveNum', 'ObsTime', 'AnemSpp', 'Spp', 'NumFish', 'Sizes', 'lat', 'lon')])

# Write out anemone data
write.csv(data, file=paste("output/GPSSurvey.anemlatlong", Sys.Date(), ".csv", sep=""))



# Write out for QGIS (has column headers)
data$notes = '' # for a QGIS label
for(i in 1:nrow(data)) {
	if(data$Spp[i] != ''){
		if(!is.na(data$oldAnemID[i]) & !is.na(data$AnemID[i])) {
			data$notes[i] = paste(data$AnemSpp[i], '#', data$oldAnemID[i], '/', data$AnemID[i], ' w/', data$NumFish[i], ' ', data$Spp[i], sep='')
		}
		if(is.na(data$oldAnemID[i]) & !is.na(data$AnemID[i])) {
			data$notes[i] = paste(data$AnemSpp[i], '#', data$AnemID[i], ' w/', data$NumFish[i], ' ', data$Spp[i], sep='')
		}
		if(!is.na(data$oldAnemID[i]) & is.na(data$AnemID[i])) {
			data$notes[i] = paste(data$AnemSpp[i], '#', data$oldAnemID[i], ' w/', data$NumFish[i], ' ', data$Spp[i], sep='')
		}
		if(is.na(data$oldAnemID[i]) & is.na(data$AnemID[i])){
			data$notes[i] = paste(data$AnemSpp[i], ' w/', data$NumFish[i], ' ', data$Spp[i], sep='')
		}
	}
	else data$notes[i] = as.character(data$AnemSpp[i])
}
out = data[,c('lat', 'lon', 'notes', 'Date', 'Name', 'Municipality')]
write.table(out, file=paste("output/GPSSurvey.anemlatlong", Sys.Date(), " for QGIS.csv", sep=""), col.names=TRUE, sep=',', row.names=FALSE, quote=TRUE)
