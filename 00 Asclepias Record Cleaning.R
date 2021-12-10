### CLEANING DATA FOR "VAGUELY-GEOREFERENCED SPECIMENS" PROJECT
### David Henderson 2019 | Adam B. Smith 2020
### 
### source('C:/Ecology/Drive/Research Active/Vaguely Georeferenced Specimen Records/Code/00 Asclepias Record Cleaning.R')
### source('D:/Ecology/Drive/Research Active/Vaguely Georeferenced Specimen Records/Code/00 Asclepias Record Cleaning.R')
### source('E:/Ecology/Drive/Research Active/Vaguely Georeferenced Specimen Records/Code/00 Asclepias Record Cleaning.R')

### CONTENTS ###
### setup ###
### obtain GADM ###
### create mask raster ###
### obtain species list ###
### preliminary cleaning of records ###
### make preliminary maps of records for each species ###
### flag records for manual inspection based on preliminary maps ###
### make corrections after inspection of preliminary maps ###
### make maps of cleaned records for each species ###
### flag duplicate records ###
### extract climate data to states/provinces and counties ###
### match climate data to records ###

#############
### setup ###
#############
	
	cat(date(), '\n'); flush.console()
	memory.limit(memory.limit() * 2^29)
	rm(list=ls())
	gc()
	options(stringsAsFactors=FALSE)
	
	# drive <- 'C:'
	# drive <- 'D:'
	drive <- 'E:'
	
	setwd(paste0(drive, '/Ecology/Drive/Research Active/Vaguely Georeferenced Specimen Records'))

	### libraries
	library(raster)
	library(dismo)
	library(rgeos)
	library(BIEN)
	library(geosphere)
	library(scales)
	library(fpCompare)
	library(bit64)
	library(data.table)
	library(scales)
	library(terra)

	library(omnibus) # GitHub: adamlilith/omnibus
	library(enmSdm) # GitHub: adamlilith/enmSdm

	### choice variables
		
		# minimum coordinate uncertainty in meters to be considered an "precise" record
		minCoordUncerOrPrecision_m <- 5000
		minCoordPrecisionForceCounty_m <- 2 * minCoordUncerOrPrecision_m
		minCoordPrecisionForceState_m <- 2 * minCoordPrecisionForceCounty_m
		
		# number of decimal digits to which coordinates of inaccurate records must be the same to be considered being at the same location
		proximateDigits <- 3
		
		# using records that were collected starting in...
		startYear <- 1970
		endYear <- 2019
		
		# minimum number of precise records for a species to be included in the analysis
		minNumAccRecords <- 5
	
	### names

		llGbif <- c('decimalLongitude', 'decimalLatitude')

		# designations for accurate, inaccurate, and county records
		accAssigns <- 'certain/precise'
		inaccAssigns <- c('uncertain/precise', 'certain/imprecise', 'uncertain/imprecise')
		adminAssigns <- c('county/precise', 'county/imprecise', 'county-only', 'state/imprecise', 'state-only')
		countyAssigns <- c('county/precise', 'county/imprecise', 'county-only')
		stateAssigns <- c('state/imprecise', 'state-only')
		
	### data objects
	
		# North American spatial polygons
		if (file.exists('./Regions/GADM Ver 3pt6 North America Level 0 WGS84.rda')) load('./Regions/GADM Ver 3pt6 North America Level 0 WGS84.rda')
		if (file.exists('./Regions/GADM Ver 3pt6 North America Level 1 WGS84.rda')) load('./Regions/GADM Ver 3pt6 North America Level 1 WGS84.rda')
		if (file.exists('./Regions/GADM Ver 3pt6 North America Level 2 WGS84.rda')) load('./Regions/GADM Ver 3pt6 North America Level 2 WGS84.rda')
	
		if (file.exists('./Regions/GADM Ver 3pt6 North America Level 0 Albers.rda')) load('./Regions/GADM Ver 3pt6 North America Level 0 Albers.rda')
		if (file.exists('./Regions/GADM Ver 3pt6 North America Level 1 Albers.rda')) load('./Regions/GADM Ver 3pt6 North America Level 1 Albers.rda')
		if (file.exists('./Regions/GADM Ver 3pt6 North America Level 2 Albers.rda')) load('./Regions/GADM Ver 3pt6 North America Level 2 Albers.rda')
	
		# mask raster
		if (file.exists('./Regions/mask_northAmerica.tif')) mask <- raster('./Regions/mask_northAmerica.tif')
		
say('###################')
say('### obtain GADM ###')
say('###################')
	
	### North America level 2
	if (!file.exists('./Regions/GADM Ver 3pt6 North America Level 2 WGS84.rda')) {

		mex <- getData('GADM', country='MEX', level=2, path='C:/ecology/!Scratch')
		usa <- getData('GADM', country='USA', level=2, path='C:/ecology/!Scratch')
		can <- getData('GADM', country='CAN', level=2, path='C:/ecology/!Scratch')

		nam2Sp <- rbind(can, usa, mex)

		# get lakes for removal from other geographies... add small buffer because lake borders don't exactly align
		lakesSp <- nam2Sp[nam2Sp$ENGTYPE_2 == 'Water body', ]
		lakesSpEa <- sp::spTransform(lakesSp, getCRS('albersNA', TRUE))
		lakesSpEa <- gBuffer(lakesSpEa, width=10)
		lakesSp <- sp::spTransform(lakesSp, getCRS('wgs84', TRUE))
		lakesSp <- gUnaryUnion(lakesSp)

		# names of level 1 areas with lakes
		lakesLevel0 <- unique(nam2Sp@data$NAME_0[nam2Sp@data$ENGTYPE_2 == 'Water body'])
		lakesLevel1 <- unique(nam2Sp@data$NAME_1[nam2Sp@data$ENGTYPE_2 == 'Water body'])
		
		# remove lakes
		nam2Sp <- nam2Sp[nam2Sp@data$ENGTYPE_2 != 'Water body', ]

		nam2SpEa <- sp::spTransform(nam2Sp, getCRS('albersNA', TRUE))
		
		save(nam2Sp, file='./Regions/GADM Ver 3pt6 North America Level 2 WGS84.rda')
		save(nam2SpEa, file='./Regions/GADM Ver 3pt6 North America Level 2 Albers.rda')
		
	}
	
	### North America level 1
	if (!file.exists('./Regions/GADM Ver 3pt6 North America Level 1 WGS84.rda')) {

		mex <- getData('GADM', country='MEX', level=1, path='C:/ecology/!Scratch')
		usa <- getData('GADM', country='USA', level=1, path='C:/ecology/!Scratch')
		can <- getData('GADM', country='CAN', level=1, path='C:/ecology/!Scratch')

		nam1Sp <- rbind(can, usa, mex)
		
		for (thisLevel in lakesLevel1) {
		
			thisLargerSp <- nam1Sp[nam1Sp@data$NAME_1 == thisLevel, ]
			df <- thisLargerSp@data
			nam1Sp <- nam1Sp[-which(nam1Sp@data$NAME_1 == thisLevel), ]
			thisLargerSansLakesSp <- gDifference(thisLargerSp, lakesSp)
			thisLargerSansLakesSp <- as(thisLargerSansLakesSp, 'SpatialPolygonsDataFrame')
			projection(thisLargerSansLakesSp) <- projection(nam1Sp)
			thisLargerSansLakesSp@data <- df
			nam1Sp <- rbind(nam1Sp, thisLargerSansLakesSp)
			
		}

		nam1SpEa <- sp::spTransform(nam1Sp, getCRS('albersNA', TRUE))

		save(nam1Sp, file='./Regions/GADM Ver 3pt6 North America Level 1 WGS84.rda')
		save(nam1SpEa, file='./Regions/GADM Ver 3pt6 North America Level 1 Albers.rda')
		
	}
	
	### North America level 0
	if (!file.exists('./Regions/GADM Ver 3pt6 North America Level 0 WGS84.rda')) {

		mex <- getData('GADM', country='MEX', level=0, path='C:/ecology/!Scratch')
		usa <- getData('GADM', country='USA', level=0, path='C:/ecology/!Scratch')
		can <- getData('GADM', country='CAN', level=0, path='C:/ecology/!Scratch')

		nam0Sp <- rbind(can, usa, mex)

		for (thisLevel in lakesLevel0) {
		
			thisLargerSp <- nam0Sp[nam0Sp@data$NAME_0 == thisLevel, ]
			df <- thisLargerSp@data
			nam0Sp <- nam0Sp[-which(nam0Sp@data$NAME_0 == thisLevel), ]
			thisLargerSansLakesSp <- gDifference(thisLargerSp, lakesSp)
			thisLargerSansLakesSp <- as(thisLargerSansLakesSp, 'SpatialPolygonsDataFrame')
			projection(thisLargerSansLakesSp) <- projection(nam1Sp)
			thisLargerSansLakesSp@data <- df
			nam0Sp <- rbind(nam0Sp, thisLargerSansLakesSp)
			
		}

		nam0SpEa <- sp::spTransform(nam0Sp, getCRS('albersNA', TRUE))

		save(nam0Sp, file='./Regions/GADM Ver 3pt6 North America Level 0 WGS84.rda')
		save(nam0SpEa, file='./Regions/GADM Ver 3pt6 North America Level 0 Albers.rda')
		
	}

say('##########################')
say('### create mask raster ###')
say('##########################')

	# 1s in terrestrial cells in North America, NAs elsewhere

	if (!exists('mask')) {
		
		# elevation
		elev <- raster::raster('E:/Ecology/Climate/WorldClim/worldclim_2.1_30arcsec_elevation/wc2.1_30s_elev.tif')

		elev <- crop(elev, nam0Sp)
		mask <- rasterize(nam0Sp, elev) * 0 + 1
		
		# remove Hawaii
		hawaiiSp <- nam1Sp[nam1Sp$NAME_1 == 'Hawaii', ]
		hawaiiSpEa <- sp::spTransform(hawaiiSp, getCRS('albersNA', TRUE))
		hawaiiBufferSpEa <- gBuffer(hawaiiSpEa, width=1000000)
		hawaiiBufferSp <- sp::spTransform(hawaiiBufferSpEa, getCRS('wgs84', TRUE))
		hawaiiBufferRast <- rasterize(hawaiiBufferSp, mask)
		
		hawaiiBufferRast <- calc(hawaiiBufferRast, fun=function(x) ifelse(is.na(x), 1, NA))
		mask <- mask * hawaiiBufferRast
		
		names(mask) <- 'mask'
		writeRaster(mask, './Regions/mask_northAmerica', datatype='INT1U', format='GTiff', overwrite=TRUE)
		
	}

# say('###########################')
# say('### obtain species list ###')
# say('###########################')

	# say('In this part we will keep species with ranges that have >90% of their range within within Canada, Mexico, and/or the US. We will obtain range maps from BIEN.')

	# # get all names from raw records
	# records <- fread('./Data/GBIF Asclepias 2019-11-18/occurrence.txt', header=TRUE)
	# records <- as.data.frame(records)
	
	# # get names of species
	# candidates <- paste(records$genus, records$specificEpithet)
	# candidates <- sort(unique(candidates))
	# bad <- 'Asclepias '
	# if (any(candidates == bad)) candidates <- candidates[-which(candidates == bad)]

	# # use only species in BIEN
	# hasRange <- BIEN_ranges_species(species=candidates, directory = 'C:/ecology/!Scratch', matched = TRUE, match_names_only = FALSE)
	
	# candidates <- hasRange$Species[hasRange[ , 2] == 'Yes']

	# # namSpEa0 <- sp::spTransform(namSp0, getCRS('albersNA', TRUE))
	# # namExtEa <- extent(namSpEa0)
	# # namExtSpEa <- as(namExtEa, 'SpatialPolygons')
	# # projection(namExtSpEa) <- getCRS('albersNA')

	# out <- data.frame()

	# # for each species
	# for (cand in candidates) {
	
		# species <- gsub(cand, pattern='_', replacement='')
		# say(species)
		
		# # get range
		# rangeSp <- shapefile(paste0('C:/ecology/!Scratch/', cand))
		# rangeSpEa <- sp::spTransform(rangeSp, getCRS('albersNA', TRUE))

		# # crop to North America
		# rangeCropSp <- crop(rangeSp, nam0Sp)
		# rangeCropSpEa <- sp::spTransform(rangeSp, getCRS('albersNA', TRUE))
		
		# # range areas
		# rangeArea_km2 <- gArea(rangeSpEa) / 1000^2
		# rangeAreaInNAM_km2 <- gArea(rangeCropSpEa) / 1000^2
		
		# # proportion of range in North America
		# propInNAM <- rangeAreaInNAM_km2 / rangeArea_km2
		
		# out <- rbind(
			# out,
			# data.frame(
				# species = species,
				# rangeArea_km2 = rangeArea_km2,
				# rangeAreaInNAM_km2 = rangeAreaInNAM_km2,
				# proportionInNAM = propInNAM
			# )
		# )
		
	# }
	
	# write.csv(out, './Data/Species with BIEN Ranges and Proportions in North America.csv', row.names=FALSE)

# say('#######################################')
# say('### preliminary cleaning of records ###')
# say('#######################################')

	# say('### load records ###')
	# ###########################

		# records <- fread('./Data/GBIF Asclepias 2019-11-18/occurrence.txt', header=TRUE)
		# records <- as.data.frame(records)

	# say('### force empty strings to NA and rename countries ###')
	# #############################################################
		
		# records$species[records$species == ''] <- NA
		# records$country[records$country == ''] <- NA
		# records$stateProvince[records$stateProvince == ''] <- NA
		# records$county[records$county == ''] <- NA

		# records$country[records$country == 'CA'] <- 'Canada'
		# records$country[records$country == 'US'] <- 'United States'
		# records$country[records$country == 'MX'] <- 'Mexico'

		# records$species <- trim(records$species)
		# records$country <- trim(records$country)
		# records$stateProvince <- trim(records$stateProvince)
		# records$county <- trim(records$county)
		
	# say('### flag records without species names ###')
	# #################################################
		
		# records$cleanBadSpeciesName <- NA
		# bads <- which(is.na(records$specificEpithet) | records$specificEpithet == '')
		# if (length(bads) > 0) records$cleanBadSpeciesName[bads] <- TRUE
		
	# say('### flag non-native species ###')
	# ######################################
	
		# records$cleanIsNonNative <- FALSE
		# if (any(records$species %in% 'Asclepias fruticosa')) records$cleanIsNonNative[records$species == 'Asclepias fruticosa'] <- TRUE
		
	# say('### flag records based on observations ###')
	# #################################################
		
		# records$cleanObservationalRecord <- NA
		# bads <- which(records$basisOfRecord %in% c('HUMAN_OBSERVATION', 'OBSERVATION', 'MACHINE_OBSERVATION'))
		# if (length(bads) > 0) records$cleanObservationalRecord[bads] <- TRUE

	# say('### flag records based on collection year ###')
	# ####################################################
		
		# ## clean collection year
		# noDate <- which(records$eventDate=='' & records$verbatimEventDate != '')
		
		# # try to scrub date from records with a "verbatimEventDate" but no "eventDate"
		# if (length(noDate) > 0) {
			
			# noDateRecords <- records[noDate , c('gbifID', 'verbatimEventDate', 'eventDate')]
			# scrubbedYear <- yearFromDate(noDateRecords$verbatimEventDate, yearLast=TRUE)
			# scrubbedYearAsChar <- as.character(scrubbedYear)
			# uncertainCenturies <- !is.na(scrubbedYearAsChar) & nchar(scrubbedYearAsChar) == 2
			# if (sum(uncertainCenturies) > 0) scrubbedYear[uncertainCenturies] <- NA
			
			# records$year[noDate] <- scrubbedYear
			
		# }
		
		# ### flag records with no collection year or ambiguous collection year
		# records$cleanBadYear <- NA
		
		# bads <- which(is.na(records$year) | records$year > endYear)
		# if (length(bads) > 0) records$cleanBadYear[bads] <- TRUE
		
		# ### flag records collected before start year
		# records$cleanBeforeStartYear <- NA
		# bads <- which(records$year < startYear)
		# if (length(bads) > 0) records$cleanBeforeStartYear[bads] <- TRUE
	
	# say('### flag cultivated records ###')
	# ######################################

		# habitat <- tolower(records$habitat)
		# locality <- tolower(records$locality)
		# verbatimLocality <- tolower(records$verbatimLocality)
		# flags <- c('cultivat', 'grow', 'garden', 'experiment', 'captiv', 'greenhouse', 'arboretum', 'hothouse', 'residential', 'nursery', 'jardin')
		# grownIndex <- integer()
		# for (flag in flags) {
			# grownIndex <- c(grownIndex, which(grepl(flag, x=habitat)))
			# grownIndex <- c(grownIndex, which(grepl(flag, x=locality)))
			# grownIndex <- c(grownIndex, which(grepl(flag, x=verbatimLocality)))
		# }
		# grownIndex <- sort(unique(grownIndex))

		# grownRecords <- records[grownIndex, c('gbifID', 'habitat', 'locality', 'verbatimLocality')]
		# rownames(grownRecords) <- grownIndex
		# write.csv(grownRecords, './Data/Species Record Cleaning/Records Potentially Representing Grown Specimens.csv')

		# records$cleanCultivated <- NA
		# corrections <- read.csv('./Data/Species Record Cleaning/Records Potentially Representing Grown Specimens - Corrections MANUALLY CREATED.csv')
		
		# if (any(corrections$flagAsCultivated)) {
			# badsId <- corrections$gbifID[corrections$flagAsCultivated]
			# bads <- which(as.character(records$gbifID) %in% as.character(badsId))
			# records$cleanCultivated[bads] <- TRUE
		# }
		
	# say('### clean state/province names for ALL records ###')
	# #########################################################

		# # # ### spelling corrections... these aren't caught below; I don't know why
		# # this <- which(records$stateProvince == 'Chinuahua')
		# # if (length(this) > 0) records$stateProvince[this] <- 'Chihuahua'
	
		# ### assign countries/states/counties to records with coordinates with NAs for country/state/county
		# haveCoords <- which(complete.cases(records[ , llGbif]))
		# admins <- raster::extract(nam2Sp, records[haveCoords, llGbif])

		# for (countHaveCoords in seq_along(haveCoords)) {
		
			# if (is.na(records$country[haveCoords[countHaveCoords]])) records$country[haveCoords[countHaveCoords]] <- admins$NAME_0[countHaveCoords]
			# if (is.na(records$stateProvince[haveCoords[countHaveCoords]])) records$stateProvince[haveCoords[countHaveCoords]] <- admins$NAME_1[countHaveCoords]
			# if (is.na(records$county[haveCoords[countHaveCoords]])) records$county[haveCoords[countHaveCoords]] <- admins$NAME_2[countHaveCoords]
		
		# }
		
		# # what state/province names don't match with GADM?
		# gadmStateProv <- nam2Sp@data$NAME_0
		
		# gadmCombined <- paste(gadmStateProv, nam2Sp@data$NAME_1)
		# gadmCombined <- gadmCombined[!duplicated(gadmCombined)]
		# gadmCombined <- sort(gadmCombined)
		# gadmCombined <- tolower(gadmCombined)

		# recordsCombined <- data.frame(
			# country = records$country,
			# stateProvince = records$stateProvince
		# )
		
		# recordsCombined$combined <- paste(recordsCombined$country, recordsCombined$stateProvince)
		# recordsCombined$combined <- tolower(recordsCombined$combined)
		
		# recordsCombined <- recordsCombined[!duplicated(recordsCombined$combined), ]
		# recordsCombined <- recordsCombined[order(recordsCombined$combined), ]
		
		# bads <- data.frame()
		# for (i in 1:nrow(recordsCombined)) {
			# if (!(recordsCombined$combined[i] %in% gadmCombined)) bads <- rbind(bads, recordsCombined[i, ])
		# }
		
		# write.csv(bads, './Data/Species Record Cleaning/Mismatched State or Province vis-a-vis GADM.csv', row.names=FALSE)
		
		# # MAKE CORRECTIONS using crosswalk created manually from previous file
		# corrections <- read.csv('./Data/Species Record Cleaning/Mismatched State or Province vis-a-vis GADM - Corrections MANUALLY CREATED.csv')

		# for (i in 1:nrow(corrections)) {
		
			# recordsIndex <- which((records$country %in% corrections$country[i]) & (records$stateProvince %in% corrections$stateProvince[i]))
			# records$stateProvince[recordsIndex] <- corrections$gadmStateProvince[i]
			
		# }
		
	# say('### clean county names for ALL records ###')
	# #################################################
		
		# # removes extraneous text
		# # x is a character vector
		# removeExtraText <- function(x) {
		
			# bads <- c('(', ')', ' county', ' County', ' COUNTY', 'County of', 'Par.', 'Parish', 'Cty.', 'Cty', 'Co.', 'Municipio', 'Municipality', '[', ']', 'Co ')
			
			# for (bad in bads) x <- gsub(x, pattern=bad, replacement='', fixed=TRUE)
			
			# saints <- c('Ste. ', 'Ste ', 'St. ', 'St ')
			# for (saint in saints) x <- gsub(x, pattern=saint, replacement='Saint ', fixed=TRUE)
			
			# x <- raster::trim(x)
			# x[x == ''] <- NA
			# x
		# }
		
		# records$county <- removeExtraText(records$county)

		# # what state/province names don't match with GADM?
		# gadmCombined <- paste(nam2Sp@data$NAME_1, nam2Sp@data$NAME_2)
		# gadmCombined <- gadmCombined[!duplicated(gadmCombined)]
		# gadmCombined <- sort(gadmCombined)
		# gadmCombined <- tolower(gadmCombined)

		# recordsCombined <- data.frame(
			# stateProvince = records$stateProvince,
			# county = records$county
		# )
		
		# recordsCombined$combined <- paste(recordsCombined$stateProvince, recordsCombined$county)
		# recordsCombined$combined <- tolower(recordsCombined$combined)
		
		# recordsCombined <- recordsCombined[!duplicated(recordsCombined$combined), ]
		# recordsCombined <- recordsCombined[order(recordsCombined$combined), ]
		
		# bads <- data.frame()
		# for (i in 1:nrow(recordsCombined)) {
			# if (!(recordsCombined$combined[i] %in% gadmCombined)) bads <- rbind(bads, recordsCombined[i, ])
		# }
		
		# write.csv(bads, './Data/Species Record Cleaning/Mismatched State-County with GADM.csv', row.names=FALSE)

		# # MAKE CORRECTIONS using corrections created manually from previous file
		# corrections <- read.csv('./Data/Species Record Cleaning/Mismatched State-County with GADM - Corrections MANUALLY CREATED.csv')
		
		# records$notes <- NA

		# for (crosswalkIndex in 1:nrow(corrections)) {

			# recordsIndex <- which((records$stateProvince %in% corrections$stateProvince[crosswalkIndex]) & (records$county %in% corrections$county[crosswalkIndex]))
			# records$stateProvince[recordsIndex] <- corrections$gadmStateProvince[crosswalkIndex]
			# records$county[recordsIndex] <- corrections$gadmCounty[crosswalkIndex]
			
			# # notes
			# records$notes[recordsIndex] <- paste0(records$notes[recordsIndex], '; ', corrections$notes[crosswalkIndex])
				
		# }

		# bads <- which(records$stateProvince %in% c('Hawaii', 'Virgin Islands'))
		# records$cleanNonCoterminiousNorthAmerica <- NA
		# if (length(bads) > 0) records$cleanNonCoterminiousNorthAmerica[bads] <- TRUE
		
	# # # say('### manually geolocate records with NA in county field ###')
	# # # ################################################################
	
		# # # ### get records with NA for state/province and/or county
		# # # naIndices <- which(
			# # # (is.na(records$stateProvince) | is.na(records$county)) &
			# # # (!is.na(records$locality) | !is.na(records$decimalLongitude) | !is.na(records$decimalLatitude) | !is.na(records$municipality) | !is.na(records$habitat))
		# # # )
		
		# # # naRecords <- records[naIndices, c('gbifID', 'locality', 'stateProvince', 'county', 'municipality', 'habitat', 'decimalLongitude', 'decimalLatitude')]
		# # # rownames(naRecords) <- naIndices

		# # # write.csv(naRecords, './Data/Species Record Cleaning/Species Records with NA in State, Province, and-or County.csv')

		# # # # MAKE CORRECTIONS using corrections created manually from previous file
		# # # corrections <- read.csv('./Data/Species Record Cleaning/Species Records with NA in State, Province, and-or County - Corrections MANUALLY CREATED.csv')
		
		# # # usable <- NA
		
		# # # # assign coordinates and state/county from corrections table
		# # # for (crosswalkIndex in 1:nrow(corrections)) {

			# # # recordsIndex <- which(records$gbifID == corrections$gbifID[crosswalkIndex])
			
			# # # # if record was in corrections... (some may not be on account of records excluded above)
			# # # if (length(recordsIndex) > 0) {
				
				# # # # exclude unusable
				# # # if (!corrections$potentiallyUsable[crosswalkIndex]) {
				
					# # # usable[recordsIndex] <- FALSE
				
				# # # } else {
					
					# # # # if there are new corrections coordinates
					# # # if (!is.na(corrections$newLongitude[crosswalkIndex]) & !is.na(corrections$newLatitude[crosswalkIndex])) {
						
						# # # # assign new coordinates
						# # # records$decimalLongitude[recordsIndex] <- corrections$newLongitude[crosswalkIndex]
						# # # records$decimalLatitude[recordsIndex] <- corrections$newLatitude[crosswalkIndex]
						
						# # # # assign new state/county
						# # # if (!is.na(corrections$gadmStateProvince[crosswalkIndex])) records$stateProvince[recordsIndex] <- corrections$gadmStateProvince[crosswalkIndex]
						# # # if (!is.na(corrections$gadmCounty[crosswalkIndex])) records$county[recordsIndex] <- corrections$gadmCounty[crosswalkIndex]
						
						# # # # if state/county NA, get using coordinates
						# # # if (is.na(records$stateProvince[recordsIndex]) | is.na(records$county[recordsIndex])) {
							# # # ext <- raster::extract(nam2Sp, records[recordsIndex, llGbif])
							# # # records$stateProvince[recordsIndex] <- ext$NAME_1[1]
							# # # records$county[recordsIndex] <- ext$NAME_2[1]
						# # # }
					
					# # # # if not new coordinates
					# # # } else {
					
						# # # if (!is.na(corrections$gadmStateProvince[crosswalkIndex])) records$stateProvince[recordsIndex] <- corrections$gadmStateProvince[crosswalkIndex]
						# # # if (!is.na(corrections$gadmCounty[crosswalkIndex])) records$county[recordsIndex] <- corrections$gadmCounty[crosswalkIndex]
						
					# # # }
					
				# # # }
				
				# # # # notes
				# # # records$notes[recordsIndex] <- paste0(records$notes[recordsIndex], ' ', corrections$notes[crosswalkIndex])
				
			# # # } # if record was in corrections
				
		# # # } # for each item in corrections

		# # # records$cleanManuallyAddedCountyIfWasNA <- NA
		# # # records$cleanManuallyAddedCountyIfWasNA[as.character(records$gbifID) %in% as.character(corrections$gbifID)] <- TRUE
		
		# # # bads <- which(!usable)
		# # # records$cleanCouldNotManuallyAddedCountyIfWasNA <- NA
		# # # if (length(bads) > 0) records$cleanCouldNotManuallyAddedCountyIfWasNA[bads] <- TRUE
		
	# # # say('### flag records with geospatial issues ###')
	# # # ##################################################
	
		# # # # NB this is redundant with the "hasGeospatialIssues" column
	
		# # # records$cleanGeoSpatialIssues <- NA
		# # # records$cleanGeoSpatialIssues[records$hasGeospatialIssues] <- TRUE
		
	# say('### assign records to geospatial confidence level ###')
	# ############################################################
		
		# source('./Code/darwinCoreSpatialAssign.r')

		# assigned <- darwinCoreSpatialAssign(
			# darwin = records,
			# geogCounty = nam2Sp,
			# geogState = nam1Sp,
			# geogCountry = nam0Sp,
			# eaProj = getCRS('albersNA'),
			# minCoordUncerOrPrecision_m = minCoordUncerOrPrecision_m,
			# minCoordPrecisionForceCounty_m = minCoordPrecisionForceCounty_m,
			# minCoordPrecisionForceState_m = minCoordPrecisionForceState_m,
			# # calcDistToCentroids = TRUE,
			# calcDistToCentroids = FALSE,
			# coordSystemStringsDegMinSec = c('degrees minutes seconds', 'DMS', 'deg. min. sec.', 'degrÃ©es minutes secondes', 'grados minutos segundos'),
			# coordSystemStringsDegMin = c('degrees minutes', 'deg. sec.', 'degrÃ©es minutes', 'degrÃ©s minutes', 'grados minutos'),
			# coordSystemStringsDeg = c('degrees', 'degree', 'deg.', 'degrÃ©es', 'degrÃ©e', 'grados', 'grado'),
			# countyGeogField = 'NAME_2',
			# stateGeogField = 'NAME_1',
			# countryGeogField = 'NAME_0',
			# verbose = TRUE
		# )

		# records <- cbind(records, assigned)
		
		# records$coordUncerOrPrecision_m <- pmax(records$coordinateUncertaintyInMeters, records$coordPrecision_m, na.rm=TRUE)

	# say('### flag imprecise/county records in areas > San Bernardino county (largest US county outside of Alaska) ###')
	# ###################################################################################################################

		# # area of San Bernardino
		# sanBernSp <- nam2Sp[nam2Sp@data$NAME_1 == 'California' & nam2Sp@data$NAME_2 == 'San Bernardino', ]
		# sanBernSpEa <- sp::spTransform(sanBernSp, getCRS('albersNA', TRUE))
		# areaSanBern_km2 <- gArea(sanBernSpEa) / 1000^2
	
		# cleanAreaTooLarge <- data.frame(cleanAreaTooLarge = records$uncerPrecisionArea_km2 > areaSanBern_km2)
		# at <- which.max(grepl(names(records), pattern='clean'))
		# records <- insertCol(cleanAreaTooLarge, into=records, at=at, before=FALSE)

	# say('### flag records as usable for this analysis or not ###')
	# ##############################################################

		# records$usable <- FALSE
		# records$usable[
			# isFALSENA(records$cleanBadSpeciesName, ifNA=TRUE) &
			# isFALSENA(records$cleanIsNonNative, ifNA=FALSE) &
			# isFALSENA(records$cleanObservationalRecord, ifNA=TRUE) &
			# isFALSENA(records$cleanBadYear, ifNA=TRUE) &
			# isFALSENA(records$cleanBeforeStartYear, ifNA=TRUE) &
			# isFALSENA(records$cleanCultivated, ifNA=TRUE) &
			# isFALSENA(records$cleanNonCoterminiousNorthAmerica, ifNA=TRUE) &
			# # isFALSENA(records$cleanGeoSpatialIssues, ifNA=TRUE) &
			# isFALSENA(records$cleanAreaTooLarge, ifNA=FALSE) &
			# records$recordType != 'unusable'
		# ] <- TRUE
	
	# say('### randomly re-order records in case any subsequent operations depend on order ###')
	# ##########################################################################################
	
		# set.seed(123)
		# records <- records[sample(1:nrow(records), nrow(records)), ]

	# save(records, file='./Data/Asclepias 00 Preliminary Cleaning of Specimen Records.rda')

# say('#########################################################')
# say('### make preliminary maps of records for each species ###')
# say('#########################################################')
	
	# load('./Data/Asclepias 00 Preliminary Cleaning of Specimen Records.rda')
	
	# species <- sort(unique(records$species))
	
	# gadmStateProvinceCounty <- paste(nam2Sp$NAME_1, nam2Sp$NAME_2)
	# gadmStateProvince <- nam1Sp$NAME_1
	
	# for (thisSpecies in species) {

		# say(thisSpecies)

		# if (exists('ext')) rm(ext)
		
		# # precise records
		# accs <- records[which(records$species == thisSpecies & records$usable & records$recordType %in% accAssigns), ]
		
		# if (nrow(accs) >= minNumAccRecords) {
			
			# inaccs <- records[which(records$species == thisSpecies & records$usable & records$recordType %in% inaccAssigns), ]
			# counties <- records[which(records$species == thisSpecies & records$usable & records$recordType %in% countyAssigns), ]
			# states <- records[which(records$species == thisSpecies & records$usable & records$recordType %in% stateAssigns), ]
		
			# if (nrow(accs) > 0) {
				# accsSp <- SpatialPointsDataFrame(accs[ , llGbif], data=accs, proj4=getCRS('wgs84', TRUE))
				# accsSpEa <- sp::spTransform(accsSp, getCRS('albersNA', TRUE))
				# ext <- extent(accsSpEa)
			# } else {
				# accs <- NULL
			# }
			
			# # imprecise records
			# if (nrow(inaccs) > 0) {
			
				# inaccsSp <- SpatialPointsDataFrame(inaccs[ , llGbif], data=inaccs, proj4=getCRS('wgs84', TRUE))
				# inaccsSpEa <- sp::spTransform(inaccsSp, getCRS('albersNA', TRUE))
				# inaccsSpEa <- gBuffer(inaccsSpEa, width=inaccsSpEa$coordinateUncertaintyInMeters, byid=TRUE, quadsegs=8)
				
				# expExt <- extent(inaccsSpEa)
				# ext <- if (exists('ext')) { merge(ext, expExt) } else { extent(expExt) }
				
			# } else {
				# inaccs <- NULL
			# }
			
			# # county records
			# if (nrow(counties) > 0) {
			
				# countiesStateProvinceCounty <- paste(counties$stateFromGeog, counties$countyFromGeog)
				# index <- which(gadmStateProvinceCounty %in% countiesStateProvinceCounty)
				
				# countiesSpEa <- nam2SpEa[index, ]
			
				# expExt <- extent(countiesSpEa)
				# ext <- if (exists('ext')) { merge(ext, expExt) } else { extent(expExt) }

			# } else {
				# counties <- NULL
			# }
			
			# # state records
			# if (nrow(states) > 0) {
			
				# statesStateProvince <- states$stateFromGeog
				# index <- which(gadmStateProvince %in% statesStateProvince)
				
				# statesSpEa <- nam1SpEa[index, ]
			
				# expExt <- extent(statesSpEa)
				# ext <- if (exists('ext')) { merge(ext, expExt) } else { extent(expExt) }

			# } else {
				# states <- NULL
			# }
			
			# extSpEa <- as(ext, 'SpatialPolygons')
			# projection(extSpEa) <- getCRS('albersNA')
			# extSpEa <- gBuffer(extSpEa, width=50000)
			
			# thisNam1SpEa <- crop(nam1SpEa, extent(extSpEa))
			# thisNam2SpEa <- crop(nam2SpEa, extent(extSpEa))

			# numAcc <- if (is.null(accs)) { 0 } else { nrow(accs) }
			# numInacc <- if (is.null(inaccs)) { 0 } else { nrow(inaccs) }
			# numCounty <- if (is.null(counties)) { 0 } else { nrow(counties) }
			# numState <- if (is.null(states)) { 0 } else { nrow(states) }
		
			# dirCreate('./Data/Species Record Cleaning/Record Maps - Preliminary')
			# png(paste0('./Data/Species Record Cleaning/Record Maps - Preliminary/', thisSpecies, '.png'), width=2 * 1333, height=2 * 833)
			
				# par(oma=c(0.5, 0.2, 2, 0.2))
			
				# plot(extSpEa, border=NA)

				# plot(thisNam2SpEa, add=TRUE, border='gray')
				# plot(thisNam1SpEa, add=TRUE, border='gray', lwd=2)
				
				# if (!is.null(states)) plot(statesSpEa, col=alpha('orange', 0.2), add=TRUE)
				# if (!is.null(counties)) plot(countiesSpEa, col=alpha('orange', 0.2), add=TRUE)
				# if (!is.null(inaccs)) plot(inaccsSpEa, col=alpha('orange', 0.2), add=TRUE)
				# if (!is.null(accs)) points(accsSpEa, pch=21, col='black', bg=alpha('green', 0.5), cex=2.4)
				
				# main <- paste0(thisSpecies, '\nAccurate: ', numAcc, ' | Inaccurate: ', numInacc, ' | County: ', numCounty, ' | State: ', numState)
				# title(main=main, sub=date(), cex.main=3, cex.sub=1.4, line=-1)
				
			# dev.off()
		
		# }
		
	# }

# say('####################################################################')
# say('### flag records for manual inspection based on preliminary maps ###')
# say('####################################################################')

	# load('./Data/Asclepias 00 Preliminary Cleaning of Specimen Records.rda')

	# ### subset records
	# inspect <- read.csv('./Data/Species Record Cleaning/Localities of Suspicious Records - Corrections MANUALLY CREATED.csv') # MANUALLY CREATED based on maps!!!
	
	# reexamine <- data.frame()
	# for (i in 1:nrow(inspect)) {
	
		# this <- NULL
	
		# if (!is.na(inspect$county[i])) {
			# this <- records[records$species == inspect$species[i] & records$stateFromGeog == inspect$stateProvince[i] & records$countyFromGeog == inspect$county[i], ]
		# } else if (!is.na(inspect$stateProvince[i])) {
			# this <- records[records$species == inspect$species[i] & records$stateFromGeog == inspect$stateProvince[i], ]
		# } else if (!is.na(inspect$country[i])) {
			# this <- records[records$species == inspect$species[i] & records$country == inspect$country[i], ]
		# } else {
			# say('Skipping: ', paste(inspect[i, ], collapse=' | '))
		# }
		
		# if (nrow(this) > 0) {
			# this$notesOnGeog <- inspect$notes[i]
			# reexamine <- rbind(reexamine, this)
		# }
		
	# }

	# write.csv(reexamine, './Data/Species Record Cleaning/Localities of Suspicious Records - Records.csv')

# say('#############################################################')
# say('### make corrections after inspection of preliminary maps ###')
# say('#############################################################')

	# load('./Data/Asclepias 00 Preliminary Cleaning of Specimen Records.rda')

	# ### subset records
	# correct <- read.csv('./Data/Species Record Cleaning/Localities of Suspicious Records - Records to Correct MANUALLY CREATED.csv') # MANUALLY CREATED based on maps!!!
	
	# for (i in 1:nrow(correct)) {
	
		# rec <- which(records$gbifID == correct$gbifID[i])
		
		# if (correct$newUsable[i]) {
		
			# records$recordType[rec] <- correct$shouldBe[i]
			# if (correct$shouldBe[i] == 'county-only') {
			
				# records$uncerPrecisionArea_km2[rec] <- -Inf
				# records$uncerBasedOn[rec] <- 'county area'
				# records$coordUncer_m[rec] <- 'county area'
				# records$coordPrecision_m[rec] <- 'county area'
				# records$coordUncerOrPrecision_m[rec] <- 'county area'
				
			# }
		
		# } else {
		
			# records$usable[rec] <- correct$newUsable[i]
		
		# }
		
	# }

	# save(records, file='./Data/Asclepias 01 Specimen Records with Cleaning from Inspection of Preliminary Maps.rda')	
		
# say('#####################################################')
# say('### make maps of cleaned records for each species ###')
# say('#####################################################')
	
	# load('./Data/Asclepias 01 Specimen Records with Cleaning from Inspection of Preliminary Maps.rda')
	
	# species <- sort(unique(records$species))
	
	# gadmStateProvinceCounty <- paste(nam2Sp$NAME_1, nam2Sp$NAME_2)
	# gadmStateProvince <- nam1Sp$NAME_1
	
	# for (thisSpecies in species) {

		# say(thisSpecies)

		# if (exists('ext')) rm(ext)
		
		# # precise records
		# accs <- records[which(records$species == thisSpecies & records$usable & records$recordType %in% accAssigns), ]
		
		# if (nrow(accs) >= minNumAccRecords) {
			
			# inaccs <- records[which(records$species == thisSpecies & records$usable & records$recordType %in% inaccAssigns), ]
			# counties <- records[which(records$species == thisSpecies & records$usable & records$recordType %in% countyAssigns), ]
			# states <- records[which(records$species == thisSpecies & records$usable & records$recordType %in% stateAssigns), ]
		
			# if (nrow(accs) > 0) {
				# accsSp <- SpatialPointsDataFrame(accs[ , llGbif], data=accs, proj4=getCRS('wgs84', TRUE))
				# accsSpEa <- sp::spTransform(accsSp, getCRS('albersNA', TRUE))
				# ext <- extent(accsSpEa)
			# } else {
				# accs <- NULL
			# }
			
			# # imprecise records
			# if (nrow(inaccs) > 0) {
			
				# inaccsSp <- SpatialPointsDataFrame(inaccs[ , llGbif], data=inaccs, proj4=getCRS('wgs84', TRUE))
				# inaccsSpEa <- sp::spTransform(inaccsSp, getCRS('albersNA', TRUE))
				# inaccsSpEa <- gBuffer(inaccsSpEa, width=inaccsSpEa$coordinateUncertaintyInMeters, byid=TRUE, quadsegs=8)
				
				# expExt <- extent(inaccsSpEa)
				# ext <- if (exists('ext')) { merge(ext, expExt) } else { extent(expExt) }
				
			# } else {
				# inaccs <- NULL
			# }
			
			# # county records
			# if (nrow(counties) > 0) {
			
				# countiesStateProvinceCounty <- paste(counties$stateFromGeog, counties$countyFromGeog)
				# index <- which(gadmStateProvinceCounty %in% countiesStateProvinceCounty)
				
				# countiesSpEa <- nam2SpEa[index, ]
			
				# expExt <- extent(countiesSpEa)
				# ext <- if (exists('ext')) { merge(ext, expExt) } else { extent(expExt) }

			# } else {
				# counties <- NULL
			# }
			
			# # state records
			# if (nrow(states) > 0) {
			
				# statesStateProvince <- states$stateFromGeog
				# index <- which(gadmStateProvince %in% statesStateProvince)
				
				# statesSpEa <- nam1SpEa[index, ]
			
				# expExt <- extent(statesSpEa)
				# ext <- if (exists('ext')) { merge(ext, expExt) } else { extent(expExt) }

			# } else {
				# states <- NULL
			# }
			
			# extSpEa <- as(ext, 'SpatialPolygons')
			# projection(extSpEa) <- getCRS('albersNA')
			# extSpEa <- gBuffer(extSpEa, width=50000)
			
			# thisNam1SpEa <- crop(nam1SpEa, extent(extSpEa))
			# thisNam2SpEa <- crop(nam2SpEa, extent(extSpEa))

			# numAcc <- if (is.null(accs)) { 0 } else { nrow(accs) }
			# numInacc <- if (is.null(inaccs)) { 0 } else { nrow(inaccs) }
			# numCounty <- if (is.null(counties)) { 0 } else { nrow(counties) }
			# numState <- if (is.null(states)) { 0 } else { nrow(states) }
		
			# dirCreate('./Data/Species Record Cleaning/Record Maps - Corrected but with Duplicates')
			# png(paste0('./Data/Species Record Cleaning/Record Maps - Corrected but with Duplicates/', thisSpecies, '.png'), width=2 * 1333, height=2 * 833)
			
				# par(oma=c(0.5, 0.2, 2, 0.2))
			
				# plot(extSpEa, border=NA)

				# plot(thisNam2SpEa, add=TRUE, border='gray')
				# plot(thisNam1SpEa, add=TRUE, border='gray', lwd=2)
				
				# if (!is.null(states)) plot(statesSpEa, col=alpha('orange', 0.2), add=TRUE)
				# if (!is.null(counties)) plot(countiesSpEa, col=alpha('orange', 0.2), add=TRUE)
				# if (!is.null(inaccs)) plot(inaccsSpEa, col=alpha('orange', 0.2), add=TRUE)
				# if (!is.null(accs)) points(accsSpEa, pch=21, col='black', bg=alpha('green', 0.5), cex=2.4)
				
				# main <- paste0(thisSpecies, '\nAccurate: ', numAcc, ' | Inaccurate: ', numInacc, ' | County: ', numCounty, ' | State: ', numState)
				# title(main=main, sub=date(), cex.main=3, cex.sub=1.4, line=-1)
				
			# dev.off()
		
		# }
		
	# }

# say('##############################')
# say('### flag duplicate records ###')
# say('##############################')

	# load('./Data/Asclepias 01 Specimen Records with Cleaning from Inspection of Preliminary Maps.rda')

	# cleanDuplicateRecord <- data.frame(cleanDuplicateRecord = rep(FALSE, nrow(records)))
	# at <- which.max(grepl(names(records), pattern='clean'))
	# records <- insertCol(cleanDuplicateRecord, into=records, at=at, before=FALSE)

	# recordsNoNaSpecies <- if (any(is.na(records$species))) {
		# records[!is.na(records$species), ]
	# } else {
		# records		
	# }
	
	# recordsNoNaSpecies <- recordsNoNaSpecies[recordsNoNaSpecies$usable, ]
	
	# countyRecords <- recordsNoNaSpecies[recordsNoNaSpecies$recordType %in% adminAssigns, ]
	# inaccRecords <- recordsNoNaSpecies[recordsNoNaSpecies$recordType %in% inaccAssigns, ]
	# accRecords <- recordsNoNaSpecies[recordsNoNaSpecies$recordType %in% accAssigns, ]
	
	# species <- sort(unique(recordsNoNaSpecies$species))

	# ### accurate records

	# # use GADM 10 arcmin as template for determining which records are duplicates
	# # "duplicates" are in same cell
	# elev <- raster::raster('D:/Ecology/Climate/WORLDCLIM Ver 2.1 January 2020/wc2.1_10m_elev/wc2.1_10m_elev.tif')
	
	# for (thisSpecies in species) {

		# thisSpeciesAccs <- accRecords[accRecords$species == thisSpecies, ]
		# # if any accurate records
		# if (nrow(thisSpeciesAccs) > 0) {

			# priority <- order(thisSpeciesAccs$coordUncerOrPrecision_m)
			# thisSpeciesAccsNoDups <- enmSdm::elimCellDups(thisSpeciesAccs, elev, longLat = llGbif, priority = priority)

			# # flag duplicates
			# if (nrow(thisSpeciesAccsNoDups) < nrow(thisSpeciesAccs)) {

				# dupsId <- thisSpeciesAccs$gbifID[!(thisSpeciesAccs$gbifID %in% thisSpeciesAccsNoDups$gbifID)]
				# records$cleanDuplicateRecord[records$gbifID %in% dupsId] <- TRUE
				
			# }
		
		
		# } # if any precise records

	# } # next species

	# ### inaccurate records
	# for (thisSpecies in species) {
	
		# thisSpeciesInaccs <- inaccRecords[inaccRecords$species %in% thisSpecies, ]

		# if (nrow(thisSpeciesInaccs) > 0) {
		
			# # remove records with duplicated coordinates (to a given error) and same coordinate uncertainty (to a given level)
			# dups <- which(
				# duplicated(
					# data.frame(
						# long=round(thisSpeciesInaccs$decimalLongitude, proximateDigits),
						# lat=(round(thisSpeciesInaccs$decimalLatitude, proximateDigits)),
						# coordUncer=round(as.numeric(thisSpeciesInaccs$coordUncerOrPrecision_m) / (10^proximateDigits))
					# )
				# )
			# )

			# if (length(dups) > 0) {
			
				# dupsId <- thisSpeciesInaccs$gbifID[dups]
				# records$cleanDuplicateRecord[records$gbifID %in% dupsId] <- TRUE
			
			# }
			
		# }

	# } # next species
	
	# ### county/state records
	# for (thisSpecies in species) {
	
		# counties <- countyRecords[countyRecords$species %in% thisSpecies, ]

		# if (nrow(counties) > 0) {
		
			# dups <- which(duplicated(paste(counties$stateFromGeog, counties$countyFromGeog)))
			
			# if (length(dups) > 0) {
			
				# dupsId <- counties$gbifID[dups]
				# records$cleanDuplicateRecord[records$gbifID %in% dupsId] <- TRUE
			
			# }
			
		# }
				
	# } # next species

	# records$usable[records$cleanDuplicateRecord] <- FALSE
	
	# save(records, file='./Data/Asclepias 02 Specimen Records with Duplicates Flagged.rda')

# say('#############################################################')
# say('### extract climate data to states/provinces and counties ###')
# say('#############################################################')	

	# # BIOCLIMs
	# clim <- stack(paste0('E:/Ecology/Climate/WorldClim/version_2_1/10_arcmin/historical/wc2.1_10m_bio_', 1:19, '.tif'))
	# names(clim) <- paste0('bio', 1:19)
	
	# # elevation
	# elev <- raster::raster('E:/Ecology/Climate/WorldClim/version_2_1/10_arcmin/wc2.1_10m_elev.tif')

	# clim <- crop(clim, nam0Sp)
	# names(clim) <- paste0('bio', 1:19)
	# elev <- crop(elev, nam0Sp)
	# names(elev) <- 'elevation'
	
	# env <- stack(elev, clim)
	# name <- names(env)
	
	# # fill NA cells near coasts to account for fact that some records may not fall in a cell near a coast
	# for (i in 1:nlayers(env)) {
		# env[[i]] <- focal(env[[i]], w=matrix(1, nrow=3, ncol=3), fun=mean, na.rm=TRUE, NAonly=TRUE)
	# }
	# names(env) <- name
	
	# say('Extracting states/provinces...', post=0)
	# envStateProv <- raster::extract(env, nam1Sp, weights=TRUE, normalizeWeights=TRUE)
	# say('Extracting counties...')
	# envCounty <- raster::extract(env, nam2Sp, weights=TRUE, normalizeWeights=TRUE)
	
	# save(envStateProv, file=paste0('./Data/Environment for North American States & Provinces at Resolution 10 arcmin for Present.rda'))
	# save(envCounty, file=paste0('./Data/Environment for North American Counties at Resolution 10 arcmin for Present.rda'))

# say('#####################################')
# say('### match climate data to records ###')
# say('#####################################')	

	# ### environmental rasters
	# #########################

		# say('Preparing environmental rasters...')
	
		# # BIOCLIMs
		# clim <- stack(paste0('E:/Ecology/Climate/WorldClim/version_2_1/10_arcmin/historical/wc2.1_10m_bio_', 1:19, '.tif'))
		# names(clim) <- paste0('bio', 1:19)
		# clim <- crop(clim, nam0Sp)
		# clim <- clim * mask
		
		# # fill NA cells near coasts to account for fact that some records may not fall in a cell near a coast
		# for (i in 1:nlayers(clim)) {
			# clim[[i]] <- focal(clim[[i]], w=matrix(1, nrow=3, ncol=3), fun=mean, na.rm=TRUE, NAonly=TRUE)
		# }
		# names(clim) <- paste0('bio', 1:19)

		# # PCA on climate (only)
		# climDf <- as.data.frame(clim)
		# nonNas <- which(complete.cases(climDf))
		# climDf <- climDf[nonNas, ]
		
		# pca <- prcomp(climDf, center=TRUE, scale=TRUE)
		# save(pca, file='./Analysis/PCA on North American Climate.rda')
		
		# pcPredictionNoNas <- predict(pca, climDf)
		# colnames(pcPredictionNoNas) <- paste0('pc', 1:19)

		# # predict PCA back to rasters
		# pcPrediction <- as.data.frame(clim)
		# pcPrediction[nonNas, ] <- pcPredictionNoNas
		
		# pcaRasts <- clim * NA
		# for (pc in 1:19) pcaRasts <- setValues(pcaRasts, values=pcPrediction[ , pc], layer=pc)
		# names(pcaRasts) <- paste0('pc', 1:19)
		
		# envPca <- stack(clim, pcaRasts)

	# ### elevation
	
		# elev <- raster('E:/Ecology/Climate/WorldClim/version_2_1/10_arcmin/wc2.1_10m_elev.tif')
		# elev <- crop(elev, nam0Sp)
		# elev <- elev * mask
		
		# # fill NA cells near coasts to account for fact that some records may not fall in a cell near a coast
		# elev <- focal(elev, w=matrix(1, nrow=3, ncol=3), fun=mean, na.rm=TRUE, NAonly=TRUE)

		# names(elev) <- 'elevation'
		# envPca <- stack(elev, envPca)
	
	# ### pre-extracted climate for states and counties
	# #################################################
	
		# say('Preparing environmental data from states/counties...')
	
		# namStateNames <- tolower(nam1Sp$NAME_1)
		# namStateCountyNames <- tolower(paste(nam2Sp$NAME_1, nam2Sp$NAME_2))
	
		# load('./Data/Environment for North American Counties at Resolution 10 arcmin for Present.rda')
		# load('./Data/Environment for North American States & Provinces at Resolution 10 arcmin for Present.rda')
	
		# # add PC axes to counties
		# for (i in seq_along(envCounty)) {
		
			# thisEnv <- envCounty[[i]]
			
			# if (class(thisEnv) != 'matrix') {
				# thisEnv <- rbind(thisEnv)
				# if (!('weight' %in% colnames(thisEnv))) {
					# wgt <- cbind(1)
					# colnames(wgt) <- 'weight'
					# thisEnv <- cbind(thisEnv, wgt)
				# }
			# }
			
			# this <- predict(pca, thisEnv)
			# colnames(this) <- paste0('pc', 1:19)
			# thisEnv <- cbind(thisEnv, this)
			# wgt <- thisEnv[ , 'weight', drop=FALSE]
			# thisEnv <- thisEnv[ , colnames(thisEnv) != 'weight', drop=FALSE]
			# thisEnv <- cbind(thisEnv, wgt)
			# rownames(thisEnv) <- 1:nrow(thisEnv)
			
			# envCounty[[i]] <- thisEnv
		
		# }
	
		# # add PC axes to states
		# for (i in seq_along(envStateProv)) {
		
			# thisEnv <- envStateProv[[i]]
			
			# if (class(thisEnv) != 'matrix') {
				# thisEnv <- rbind(thisEnv)
				# if (!('weight' %in% colnames(thisEnv))) {
					# wgt <- cbind(1)
					# colnames(wgt) <- 'weight'
					# thisEnv <- cbind(thisEnv, wgt)
				# }
			# }
			
			# this <- predict(pca, thisEnv)
			# colnames(this) <- paste0('pc', 1:19)
			# thisEnv <- cbind(thisEnv, this)
			# wgt <- thisEnv[ , 'weight', drop=FALSE]
			# thisEnv <- thisEnv[ , colnames(envStateProv[[i]]) != 'weight', drop=FALSE]
			# thisEnv <- cbind(thisEnv, wgt)
			# rownames(thisEnv) <- 1:nrow(thisEnv)
			
			# envStateProv[[i]] <- thisEnv
		
		# }
	
	# ### setup record data
	# #####################

		# say('Preparing master data structure...')
	
		# # load record data
		# load('./Data/Asclepias 02 Specimen Records with Duplicates Flagged.rda')

		# # create data subsets
		# asclepias <- list()
		# asclepias$meta$dataDescrip <- paste0('This data comprises a download from GBIF of all Ascpelias records (DOI: 10.15468/dl.uddn6o) on 2019-11-18. The search filtered for all records belonging to the genus Asclepias occurring in Canada, the United States, or Mexico. No other filters were applied. A cleaned version of the data set was generated by comparing data in records to the geographies in GADM 3.6 and through analysis of coordinate precision and uncertainty. Records classified as "usable" had: 1) valid species names; 2) had a valid year; 3) were collected from ', startYear, ' to ', endYear, '; 4) did not appear to have been associated with highly managed habitats (gardens, experiments) or similar circumstances (purchased at a store); 5) occur within coterminous North America (including minor outlying islands); 6) have a collection locality centroid that can be located confidently to a particular administrative unit (state/county) or a subsection thereof; 6) for records that can only be located to an administrative unit (i.e., state or county); 7) occur within a state/county that are as large as San Bernardino County, California, or smaller; are not spatial duplicates of other records. Records are divided into three classes: a) "accurate" (have coordinates, coordinate uncertainty < ', minCoordUncerOrPrecision_m, '  m, coordinate precision < ', 2 * minCoordUncerOrPrecision_m, ' m); b) "inaccurate" (have coordinates, coordinate uncertainty >= ', 2 * minCoordUncerOrPrecision_m, ' m but area of uncertainty <= San Bernardino county); and c) "administrative" (can only be located to a county or state as large as or smaller than San Bernardino county; may have coordinates and an associated coordinate uncertainty but coordinate precision is >= ', 2 * minCoordUncerOrPrecision_m, ' m for county records and >= ', 4 * minCoordUncerOrPrecision_m, ' m for state records). Whether or not a record was a duplicate was determined by applying rules specific to the type of record (accurate: same cell; inaccurate: centroids are the same after rounding to ', proximateDigits, ' decimal places and coordinate uncertainty the same after rounding to nearest ', 10^proximateDigits, ' m; county/county: occur in the same state and county for county-level records and same state for state-level records). WORLDCLIM Version 2 Release June 2016 data was associated with each record based on its type by extracting the cell in which the record centroid fell (accurate) or all cells overlapping with the area of uncertainty (inaccurate, administrative). Each record was also associated with principal component axes of a PCA calculated across all 19 BIOCLIM variables across the entity of North America.')
		# asclepias$meta$pca <- pca
		# asclepias$meta$usableSpecies <- data.frame()
		# asclepias$meta$dataStructure <- data.frame(
			# element = c(
				# 'accurateUsableNoDupRecs',
				# 'inaccurateAdminUsableNoDupRecs',
				# 'accurateUsableNoDupEnv',
				# 'inaccurateAdminUsableNoDupEnv'
			# ),
			# description = c(
				# paste0('data frame: accurate, usable records with duplicates removed'),
				# paste0('data.frame: inaccurate and administrative usable records'),
				# paste0('list: climate associated with records in "$accurateUsableNoDupRecs"'),
				# paste0('list: climate associated with records in "$inaccurateAdminUsableNoDupRecs"')
			# )
		# )
			 
		# asclepias$bySpecies <- list()
	
	# ### collate records
	# ###################
		
		# # fields from records to retain
		# fields <- c('gbifID', 'species', 'recordType', 'decimalLongitude', 'decimalLatitude', 'uncerPrecisionArea_km2', 'maxCoordUncerPrecision_m', 'stateFromGeog', 'countyFromGeog', 'year')
		
		# species <- sort(unique(records$species))
		
		# speciesTicker <- 0 # counter for number of species with >= minimum number of accurate records
		
		# # by species
		# for (countSpecies in seq_along(species)) {

			# thisSpecies <- species[countSpecies]
			# say(thisSpecies)
		
			# # data structure for this species
			# thisOut <- list()
			# thisOut$species <- thisSpecies
			# thisOut$accurateUsableNoDupRecs <- data.frame()
			# thisOut$inaccurateAdminUsableNoDupRecs <- data.frame()
			# thisOut$accurateUsableNoDupEnv <- data.frame()
			# thisOut$inaccurateAdminUsableNoDupEnv <- list()
				
			# # accurate records
			# accs <- records[which(records$species == thisSpecies & records$usable & records$recordType %in% accAssigns), fields]
			
			# # if sufficient accurate records
			# if (nrow(accs) >= minNumAccRecords) {
			
				# speciesTicker <- speciesTicker + 1
			
				# # inaccurate/county/state records
				# inaccs <- records[which(records$species == thisSpecies & records$usable & records$recordType %in% inaccAssigns), fields]
				# county <- records[which(records$species == thisSpecies & records$usable & records$recordType %in% countyAssigns), fields]
				# state <- records[which(records$species == thisSpecies & records$usable & records$recordType %in% stateAssigns), fields]
				
				# asclepias$meta$usableSpecies <- rbind(
					# asclepias$meta$usableSpecies,
					# data.frame(
						# species=thisSpecies,
						# numUniqueUsableAccurateRecs = nrow(accs),
						# numUniqueUsableInaccurateRecs = nrow(inaccs),
						# numUniqueUsableAdminRecs = nrow(county) + nrow(state)
					# )
				# )
				
				# # extract environmental data for ACCURATE records
				# accsEnvPca <- as.data.frame(raster::extract(envPca, accs[ , llGbif]))
				# rownames(accsEnvPca) <- paste0('gbifID_', accs$gbifID)
					
				# thisOut$accurateUsableNoDupRecs <- accs
				# thisOut$accurateUsableNoDupEnv <- accsEnvPca

				# # extract environmental data for INACCURATE records
				# if (nrow(inaccs) > 0) {
				
					# inaccsSp <- SpatialPoints(inaccs[ , llGbif], getCRS('wgs84', TRUE))
					# inaccsSpEa <- sp::spTransform(inaccsSp, getCRS('albersNA', TRUE))
					# inaccsSpEaBuffs <- gBuffer(inaccsSpEa, byid=TRUE, width=inaccs$maxCoordUncerPrecision_m)
					# inaccsSpBuffs <- sp::spTransform(inaccsSpEaBuffs, getCRS('wgs84', TRUE))
				
					# inaccsEnvPca <- raster::extract(envPca, inaccsSpBuffs, weights=TRUE, normalizeWeights=TRUE)
					# names(inaccsEnvPca) <- paste0('gbifID_', inaccs$gbifID)
					
					# thisOut$inaccurateAdminUsableNoDupRecs <- rbind(thisOut$inaccurateAdminUsableNoDupRecs, inaccs)
					# thisOut$inaccurateAdminUsableNoDupEnv <- c(thisOut$inaccurateAdminUsableNoDupEnv, inaccsEnvPca)
				
				# }
				
				# # extract environmental data for COUNTY records
				# if (nrow(county) > 0) {
				
					# recStateCountyNames <- tolower(paste(county$stateFromGeog, county$countyFromGeog))
					# these <- which(namStateCountyNames %in% recStateCountyNames)
					# countyEnvPca <- envCounty[these]
					# countyEnvPca <- countyEnvPca[order(na.omit(match(namStateCountyNames, recStateCountyNames)))]
					# # countyEnvPca <- countyEnvPca[na.omit(match(recStateCountyNames, namStateCountyNames))]
					# names(countyEnvPca) <- paste0('gbifID_', county$gbifID)
				
					# thisOut$inaccurateAdminUsableNoDupRecs <- rbind(thisOut$inaccurateAdminUsableNoDupRecs, county)
					# thisOut$inaccurateAdminUsableNoDupEnv <- c(thisOut$inaccurateAdminUsableNoDupEnv, countyEnvPca)
				
				# }
				
				# # extract environmental data for STATE records
				# if (nrow(state) > 0) {
				
					# recStateNames <- tolower(county$stateFromGeog)
					# these <- which(namStateNames %in% recStateNames)
					# stateEnvPca <- envStateProv[these]
					# stateEnvPca <- stateEnvPca[order(na.omit(match(namStateNames, recStateNames)))]
					# names(stateEnvPca) <- paste0('gbifID_', state$gbifID)
				
					# thisOut$inaccurateAdminUsableNoDupRecs <- rbind(thisOut$inaccurateAdminUsableNoDupRecs, state)
					# thisOut$inaccurateAdminUsableNoDupEnv <- c(thisOut$inaccurateAdminUsableNoDupEnv, stateEnvPca)

				# }
				
				# asclepias$bySpecies[[speciesTicker]] <- thisOut
				# names(asclepias$bySpecies)[speciesTicker] <- tolower(gsub(thisSpecies, pattern=' ', replacement='_'))
				
			# } # if >= min number of accurate records	
		
		# } # next species
		
	# save(asclepias, file='./Data/Asclepias 03 Specimen Records and Environmental Data.rda')

say('DONE! ', date(), level=1, deco='%')
