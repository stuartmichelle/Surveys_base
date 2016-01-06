# Run extract_anemones_2014-06-28.R first to make GPSSurvey.anemlatlong file

#####  Tissue Samples	
	setwd("~/Desktop/2015_01_conv")
	anem = read.csv("output/GPSSurvey.anemlatlong2014-10-29.csv")

		# Find the largest pair of fish on each anemone
	anem$Rank1 = NA # column of the largest individual (ID1 through ID6)
	anem$Rank2 = NA

	k = which(!is.na(anem$Size1)) # anemones with fish
	for(i in k){
		if(anem$Size7[i] != "" & !is.na(anem$Size7[i])){ # If I need to parse fish from the 7th column
			temp = as.numeric(unlist(strsplit(as.character(anem$Size7[i]), split=",", fixed=T)))
		} else {
			temp = NA
		}
		j = sort(c(anem$Size1[i],anem$Size2[i],anem$Size3[i],anem$Size4[i],anem$Size5[i],temp), index.return=T, decreasing=T)$ix
		anem$Rank1[i] = j[1]
		anem$Rank2[i] = j[2]
	}


	
	collections = data.frame(DiveNum = character(0), Date = character(0), ObsTime = character(0), Spp = character(0), Size1 = numeric(0), ID1 = character(0), Col1 = character(0), Notes=character(0), lat = numeric(0), lon = numeric(0), TopTwo = logical(0))
	names = names(collections)
	
	# add all fish that were sampled (ID is not blank)
		# start with Size1/ID1 fish
	k = which(anem$ID1 !="")
	if(length(k)>0){
		x<-subset(anem[k,], select=c(DiveNum, Date, ObsTime, Spp, Size1, ID1, Col1, Notes, lat, lon))
		x$TopTwo = anem$Rank1[k] == 1 | anem$Rank2[k] == 1 # is this one of the two largest on the anemone?
		names(x)= names
		collections = rbind(collections, x)
	}
	dim(collections)
	
		# then Size2/ID2
	k = which(anem$ID2 !="")
	if(length(k)>0){
		x<-subset(anem[k,], select=c(DiveNum, Date, ObsTime, Spp, Size2, ID2, Col2, Notes, lat, lon))
		x$TopTwo = anem$Rank1[k] == 2 | anem$Rank2[k] == 2

		names(x)= names
		collections = rbind(collections, x)
	}
	dim(collections)
	
		# Size3/ID3
	k = which(anem$ID3 !="")
	if(length(k)>0){
		x<-subset(anem[k,], select=c(DiveNum, Date, ObsTime, Spp, Size3, ID3, Col3, Notes, lat, lon))
		x$TopTwo = anem$Rank1[k] == 3 | anem$Rank2[k] == 3

		names(x)= names
		collections = rbind(collections, x)
	}
	dim(collections)
	
		# Size4/ID4
	k = which(anem$ID4 !="")
	if(length(k)>0){
		x<-subset(anem[k,], select=c(DiveNum, Date, ObsTime, Spp, Size4, ID4, Col4, Notes, lat, lon))
		x$TopTwo = anem$Rank1[k] == 4 | anem$Rank2[k] == 4

		names(x)= names
		collections = rbind(collections, x)
	}
	dim(collections)
	
		# Size5/ID5
	k = which(anem$ID5 !="")
	if(length(k)>0){
		x<-subset(anem[k,], select=c(DiveNum, Date, ObsTime, Spp, Size5, ID5, Col5, Notes, lat, lon))
		x$TopTwo = anem$Rank1[k] == 5 | anem$Rank2[k] == 5

		names(x)= names
		collections = rbind(collections, x)
	}
	dim(collections)
	
		# Size6/ID6
	k = which(anem$ID6 !="")
	if(length(k)>0){
		x<-subset(anem[k,], select=c(DiveNum, Date, ObsTime, Spp, Size6, ID6, Col6, Notes, lat, lon))
		x$TopTwo = anem$Rank1[k] == 6 | anem$Rank2[k] == 6
		names(x)= names
		collections = rbind(collections, x)
	}
	dim(collections)
	
		# Size7/ID7: may need to be split
	k = which(anem$ID7 !="")
	if(length(k)>0){
		x<-subset(anem[k,], select=c(DiveNum, Date, ObsTime, Spp, Size7, ID7, Col7, Notes, lat, lon))
		i = sapply(x, is.factor); x[i] = lapply(x[i], as.character) # converts factors to characters
		x$TopTwo = NA
		x2 = x[0,] # to hold the final values from Size7/ID7 (after disaggregating multiple entries on the same line)
		for(i in 1:nrow(x)){ # check each row for multiple entries
			ids = gsub(' ', '', unlist(strsplit(as.character(x$ID7[i]), split=','))) # split apart on comma and remove spaces
			sizes = gsub(' ', '', unlist(strsplit(as.character(x$Size7[i]), split=','))) # split apart on comma
			for(j in 1:length(ids)){ # then check for non-NA, non-blank entries
				if(!is.na(ids[j]) & ids[j] != '' & ids[j] != 'NA'){
					x2 = rbind(x2, x[i,])
					x2$ID7[nrow(x2)] = ids[j]
					x2$Size7[nrow(x2)] = sizes[j]
					x2$TopTwo[nrow(x2)] = anem$Rank1[k][i] == 6+j | anem$Rank2[k][i] == 6+j # not positive this is working, but not needed, I think
				}
			}
		}
		names(x2)= names
		collections = rbind(collections, x2)
	}
	dim(collections)
	


	names(collections) = c("DiveNum", "Date", "Time", "Spp", "Size", "ID", "Col", "Notes", "lat", "lon", "TopTwo")

	collections$Notes = as.character(collections$Notes)
	
	
	# Sort the data
	permut = order(collections$DiveNum, collections$Time)
	collections = collections[permut,]
	row.names(collections) = 1:nrow(collections)
	
	# Examine the data
	collections[,c('DiveNum', 'Date', 'Time', 'lat', 'lon')]
	
	
	# Write out collection data
	write.csv(collections, file=paste("output/Collections", Sys.Date(), ".csv", sep=""), row.names=FALSE)




