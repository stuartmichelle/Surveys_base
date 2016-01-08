# For 2014 surveys

setwd("/Users/michelle/Documents/Rutgers/Philippines/Surveys_2014")

################################################################
## Concatentate GPX track files together into one spreadsheet
## (for use later in the script)
################################################################
# Script to read in gpx files from Philippines 2013 fieldwork
source("code/readGPXGarmin_2014_06_07.R")
options(digits=22)


files = list.files(path = 'gpx', pattern="^Track.*gpx")
len = length(files)

data = vector("list", len)
for(i in 1:len){
	infile = readGPXGarmin(paste('gpx/', files[i], sep=''))
	data[i] = list(infile$data)
}

latlong = data.frame(data[[1]])
for(i in 2:len){
	new = data.frame(data[[i]])
	latlong = rbind(latlong, new)
}
dim(latlong)

# Remove duplicates
i = duplicated(latlong)
	sum(i)
latlong = latlong[!i,]
dim(latlong)
	
# Sort the records by time
permut = order(latlong$year, latlong$month, latlong$day, latlong$hour, latlong$min, latlong$sec)
latlong = latlong[permut,]

write.csv(latlong, file=paste("output/track.concat", Sys.Date(), ".csv", sep=""))


######################################################
## Surveys and Collections: Match times to locations
######################################################

####### Add lat/long to survey data
	surv = read.csv("GPSSurveys2014.surveyinfo.csv")
	data = read.csv("GPSSurveys2014.clownfish.csv", stringsAsFactors=FALSE)
	latlong = read.csv('output/track.concat2014-08-01.csv', row.names=1)
		
	names = names(data)
	data$NumFish = NA # total number of fish (dominant species)
	data$Sizes = NA # concatenated sizes of fish (dominant species)
	data$lat = NA
	data$lon = NA
	
	# add survey name
	data = merge(data, surv[,c('DiveNum', 'Date', 'Name', 'Municipality', 'Cover')])

	# remove lines that weren't samples
	i = data$AnemSpp == ''
	if(any(data$Spp[i]!='')) stop('lacking anemone for a sample')
	data = data[!i,]
	
	# combine multiple samples from the same anemone. store later samples with the first sample
	# at the moment, I make not attempt to average size, depth, or location data across multiple visits
	dups = data$AnemID[duplicated(data$AnemID) & !is.na(data$AnemID)] # anemone IDs that appear on >1 line
	if(length(dups)>0){
		for(i in 1:length(dups)){
			inds = sort(which(data$AnemID == dups[i]))
			if(length(inds)<3){
				# get data from the second visit to the anemone
				newsizes = data[inds[2], c('Size1', 'Size2', 'Size3', 'Size4', 'Size5', 'Size6', 'Size7')]
					newsizes = as.numeric(newsizes[!(newsizes == '' | is.na(newsizes))])
				newcols = data[inds[2], c('Col1', 'Col2', 'Col3', 'Col4', 'Col5', 'Col6', 'Col7')]
					newcols = newcols[!(newcols == '' | is.na(newcols))]
				newids = data[inds[2], c('ID1', 'ID2', 'ID3', 'ID4', 'ID5', 'ID6', 'ID7')]
					newids = as.numeric(newids[!(newids == '' | is.na(newids))])

				# make sure there is data to add
				if(length(newsizes)>0 | length(newcols)>0 | length(newids)>0){
			
					# error checking
					if(length(newsizes) != length(newcols)){
						warning(paste('size and color vector lengths do not match, i=', i, '. Filling with NAs.'))
						newcols = c(newcols, rep(NA, length(newsizes)-length(newcols)))
					}
					if(length(newsizes) != length(newids)) {
						warning(paste('size and ID vector lengths do not match, i=', i, '. Filling with NAs.'))
						newids = c(newids, rep(NA, length(newsizes)-length(newids)))			
					}
						
					# append to the first anemone record
					ii = min(which(is.na(data[inds[1], c('Size1', 'Size2', 'Size3', 'Size4', 'Size5', 'Size6', 'Size7')]))) # index of first empty field on the row
					if(ii<7){ # only have 7 columns to add into, so can only append on simply if at least one cell is free
						for(j in ii:min(7,ii+length(newsizes)-1)){ # only have 7 columns, so don't go beyond
							ct = j - ii + 1 # index into newsizes, newcols, newids
							sznm = paste('Size', j, sep=''); clnm = paste('Col', j, sep=''); idnm = paste('ID', j, sep='');
							data[inds[1], sznm] = newsizes[ct]
							data[inds[1], clnm] = newcols[ct]
							data[inds[1], idnm] = newids[ct]
			
						}
						if(ii+length(newsizes)-1 > 7){ # if need to fill the 7th column with >1 individual
							data$Size7[inds[1]] = paste(data$Size7[inds[1]], paste(newsizes[(ct+1):length(newsizes)], collapse=','), sep=',') # paste the remaining individuals together
							data$Col7[inds[1]] = paste(data$Col7[inds[1]], paste(newcols[(ct+1):length(newcols)], collapse=','), sep=',')
							data$ID7[inds[1]] = paste(data$ID7[inds[1]], paste(newids[(ct+1):length(newids)], collapse=','), sep=',')
						}
					} else {
						stop('need to deal with case of 7 cols already filled')
					}
					# remove inds[2] from data
					data = data[-inds[2],]
				}			
			} else {
				stop('need to deal with case of >2 sampling times')
			}
		}
	}
	
	# process data for each anemone
	len = nrow(data)
	for(i in 1:len){
		#Get date and time information for the anemone
		survey = data$DiveNum[i]
		survindex = which(surv$DiveNum == survey)
		date = as.character(surv$Date[survindex])
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
	
		# Find the location records that match the date/time stamp (to nearest second)
		latlongindex = which(latlong$month == month & latlong$day == day & latlong$hour == hour & latlong$min == min)
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
	data$notes = ''
	for(i in 1:nrow(data)){
		if(data$Spp[i] != '') data$notes[i] = paste(data$AnemSpp[i], ' w/', data$NumFish[i], ' ', data$Spp[i])
		else data$notes[i] = as.character(data$AnemSpp[i])
	}
	out = data[,c('lat', 'lon', 'notes', 'Date', 'Name', 'Municipality')]
	write.table(out, file=paste("output/GPSSurvey.anemlatlong", Sys.Date(), " for QGIS.csv", sep=""), col.names=TRUE, sep=',', row.names=FALSE, quote=TRUE)


	
