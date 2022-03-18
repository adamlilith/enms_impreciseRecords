### CALCULATE CLIMATE EXPOSURE USING ENMS
### David Henderson 2019 | Adam B. Smith 2020
### 
### source('C:/Ecology/Drive/Research Active/Vaguely Georeferenced Specimen Records/enms_impreciseRecords/03 Calculate Climate Change Exposure Using ENMs.r')
### source('E:/Ecology/Drive/Research Active/Vaguely Georeferenced Specimen Records/enms_impreciseRecords/03 Calculate Climate Change Exposure Using ENMs.r')

### CONTENTS ###
### setup ###
### calculate climate change exposure from ENMs ###
### plot climate change exposure ###
### plot maps of climate change exposure ###
### plot climate change exposure metrics within MCP of accurate occurrences ###

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

	library(cowplot)
	library(ggplot2)
	library(rgeos)
	library(sp)
	library(terra)
	
	# custom (Adam Smith)
	library(omnibus)
	library(enmSdm)

	### constants
	bufferWidth_km <- 300 # buffer around MCP and around points to define area in which to assess climate change exposure

	### names
	llGbif <- c('decimalLongitude', 'decimalLatitude')
	
	### designations for accurate, inaccurate, and county records
	accAssigns <- 'certain/precise'
	inaccAssigns <- c('uncertain/precise', 'certain/imprecise', 'uncertain/imprecise')
	adminAssigns <- c('county/precise', 'county/imprecise', 'county-only', 'state/imprecise', 'state-only')
	countyAssigns <- c('county/precise', 'county/imprecise', 'county-only')
	stateAssigns <- c('state/imprecise', 'state-only')
	
	# add 0-width buffer, unary union, (crop if desired), return SpatVector
	# have to go through this because error often evolve from using spatVector objects in terra
	workVectPoly <- function(vectPoly, width=NULL, cropBy=NULL) {
	
		# vectPoly	spatVector
		# width		if NULL, do not buffer, otherwise a numeric value
		# cropBy	spatVector by which to crop the polygon... leave as NULL to not crop
	
		spPoly <- as(vectPoly, 'Spatial')
		
		if (!(class(spPoly) %in% c('SpatialPoints', 'SpatialPointsDataFrame'))) {
			spPoly <- rgeos::gBuffer(spPoly, width=0)
			spPoly <- gUnaryUnion(spPoly)
		}
		
		if (!is.null(width)) {
			spPoly <- rgeos::gBuffer(spPoly, width=width)
		}
		
		vectPoly <- vect(spPoly)
		
		if (!is.null(cropBy)) {
			vectPoly <- terra::crop(vectPoly, cropBy)
			vectPoly <- workVectPoly(vectPoly)
		}
		
		vectPoly
		
	}

	### given a data frame representing values extracted from a binary raster of interest and from an area raster, calculate total area
	calcBinaryArea <- function(ext, extArea) {
	
		# ext		data frame, extraction from raster of interest
		# extArea	data frame, extraction from area raster
	
		notNa <- which(!is.na(ext$layer))
	
		a <- sum(extArea$layer[notNa] * ext$layer[notNa] * ext$weight[notNa]) / sum(!is.na(ext$layer[notNa]) * ext$weight[notNa])
		
		a

	}
				
	### calculate area masked by a polygon in the present, future, stable area, and area of gain and loss
	calcExpose <- function(currentRast, futRast, areaRast, vectPoly) {
		
		# currentRast		current raster, thresholded
		# futRast			future raster, thresholded
		# areaRast			area with cell values equal to cell area
		# vectPoly			spatVector used to crop the area of interest
		
		# crop rasters to polygon
		vectPolyUnproj <- terra::project(vectPoly, getCRS('wgs84'))
		
		# mask
		maskRastUnproj <- rasterize(vectPolyUnproj, currentRast) * 0 + 1
		currentRast <- currentRast * maskRastUnproj
		futRast <- futRast * maskRastUnproj
		
		# areas of stability and change
		stableRast <- currentRast * futRast
		changeRast <- futRast - currentRast
		gainRast <- app(changeRast, fun=function(x) ifelse(x==1, 1, 0))
		lossRast <- app(changeRast, fun=function(x) ifelse(x==-1, 1, 0))

		# extract stable, loss, gain cells
		areaExt <- terra::extract(areaRast_km2, vectPolyUnproj, weights=TRUE)
		currentExt <- terra::extract(currentRast, vectPolyUnproj, weights=TRUE)
		futExt <- terra::extract(futRast, vectPolyUnproj, weights=TRUE)
		stableExt <- terra::extract(stableRast, vectPolyUnproj, weights=TRUE)
		gainExt <- terra::extract(gainRast, vectPolyUnproj, weights=TRUE)
		lossExt <- terra::extract(lossRast, vectPolyUnproj, weights=TRUE)
		
		names(areaExt) <- names(currentExt) <- names(futExt) <- names(stableExt) <- names(gainExt) <- names(lossExt) <- c('ID', 'layer', 'weight')
		
		# calculate area of each class
		currentArea_km2 <- calcBinaryArea(currentExt, areaExt)
		futArea_km2 <- calcBinaryArea(futExt, areaExt)
		stableArea_km2 <- calcBinaryArea(stableExt, areaExt)
		gainArea_km2 <- calcBinaryArea(gainExt, areaExt)
		lossArea_km2 <- calcBinaryArea(lossExt, areaExt)

		c(currentArea_km2=currentArea_km2, futArea_km2=futArea_km2, stableArea_km2=stableArea_km2, gainArea_km2=gainArea_km2, lossArea_km2=lossArea_km2)

	}

# say('###################################################')
# say('### calculate climate change exposure from ENMs ###')
# say('###################################################')
		
	# # For a given species, RCP, and background definition:
	
	# # Get present and future rasters
	# # Crop to EOO based on precise records
	# # Get threshold based only on precise records
	# # Threshold rasters
	# # Calculate NatureServe climate change exposure metric
	# # Repeat for cases using imprecise records, too, but use EOO and threshold based only on precise records
		
	# ### North American spatial polygons
	# load('./Regions/GADM Ver 3pt6 North America Level 0 WGS84.rda')
	# load('./Regions/GADM Ver 3pt6 North America Level 1 WGS84.rda')
	# load('./Regions/GADM Ver 3pt6 North America Level 2 WGS84.rda')

	# load('./Regions/GADM Ver 3pt6 North America Level 0 Albers.rda')
	# load('./Regions/GADM Ver 3pt6 North America Level 1 Albers.rda')
	# load('./Regions/GADM Ver 3pt6 North America Level 2 Albers.rda')

	# nam0VectSp <- vect(nam0Sp)
	# nam1VectSp <- vect(nam1Sp)
	# nam2VectSp <- vect(nam2Sp)

	# nam0VectEa <- vect(nam0SpEa)
	# nam1VectEa <- vect(nam1SpEa)
	# nam2VectEa <- vect(nam2SpEa)

	# # state/county names
	# stateGeog <- tolower(paste0(nam1Sp$NAME_1))
	# stateCountyGeog <- tolower(paste0(nam2Sp$NAME_1, nam2Sp$NAME_2))

	# # species' records
	# load('./Data/Asclepias 03 Specimen Records and Environmental Data.rda')
	# speciesList <- asclepias$meta$usableSpecies$species

	# # Stephen's ENM rasters
	# currentEnms <- readRDS('./Analysis/ENMs/binary_maps_current_uncropped_Feb4_2021.rds')

	# # holds exposure values
	# exposure <- data.frame()

	# for (countSpecies in seq_along(speciesList)) {

		# species <- speciesList[countSpecies]
		# say(species)

		# ### accurate records
		# ####################
			
			# spp <- tolower(gsub(species, pattern=' ', replacement='_'))
			# accs <- asclepias$bySpecies[[spp]]$accurateUsableNoDupRecs[ , llGbif]
			# names(accs) <- c('x', 'y')
			# accsSpUnproj <- SpatialPoints(accs, getCRS('wgs84', TRUE))
			# accsVectUnproj <- vect(as.matrix(accs), 'points', crs=getCRS('wgs84'))

			# accsSpEa <- sp::spTransform(accsSpUnproj, getCRS('albersNA', TRUE))
			# accsVectEa <- project(accsVectUnproj, getCRS('albersNA'))

			# # centroid of accurate records
			# centsAccsVectEa <- centroids(accsVectEa)

		# ### imprecise records (points with buffers, counties, states)
		# #############################################################

			# # inaccurate records (points with buffers)
			# index <- which(asclepias$bySpecies[[countSpecies]]$inaccurateAdminUsableNoDupRecs$recordType %in% inaccAssigns)
			# if (length(index) > 0) {

				# inaccs <- asclepias$bySpecies[[countSpecies]]$inaccurateAdminUsableNoDupRecs[index, ]
				# inaccsSp <- SpatialPointsDataFrame(inaccs[ , llGbif], data=inaccs, proj4string=getCRS('wgs84', TRUE))
				# inaccsSpEa <- sp::spTransform(inaccsSp, getCRS('albersNA', TRUE))
				# inaccsSpEa <- gBuffer(inaccsSpEa, width=inaccsSpEa$maxCoordUncerPrecision_m, byid=TRUE, quadsegs=12)
				# inaccsSpEa <- as(inaccsSpEa, 'SpatialPolygons')
				# projection(inaccsSpEa) <- getCRS('albersNA')
				
				# inaccsAdminSpEa <- inaccsSpEa
				
			# }

			# # county records
			# index <- which(asclepias$bySpecies[[countSpecies]]$inaccurateAdminUsableNoDupRecs$recordType %in% countyAssigns)
			# if (length(index) > 0) {

				# county <- asclepias$bySpecies[[countSpecies]]$inaccurateAdminUsableNoDupRecs[index, ]
				# stateCountyRecs <- tolower(paste0(county$stateFromGeog, county$countyFromGeog))
				# countySpEa <- nam2SpEa[which(stateCountyGeog %in% stateCountyRecs), ]
				# countySpEa <- as(countySpEa, 'SpatialPolygons')
				
				# inaccsAdminSpEa <- if (exists('inaccsAdminSpEa')) {
					# rbind(inaccsAdminSpEa, countySpEa, makeUniqueIDs=TRUE)
				# } else {
					# countySpEa
				# }
				
			# }
			
			# # state records
			# index <- which(asclepias$bySpecies[[countSpecies]]$inaccurateAdminUsableNoDupRecs$recordType %in% stateAssigns)
			# if (length(index) > 0) {

				# state <- asclepias$bySpecies[[countSpecies]]$inaccurateAdminUsableNoDupRecs[index, ]
				# stateRecs <- tolower(state$stateFromGeog)
				# stateSpEa <- nam1SpEa[stateGeog %in% stateRecs, ]
				
				# inaccsAdminSpEa <- if (exists('inaccsAdminSpEa')) {
					# rbind(inaccsAdminSpEa, stateSpEa, makeUniqueIDs = TRUE)
				# } else {
					# stateSpEa
				# }
				
			# }

			# inaccsAdminVectEa <- vect(inaccsAdminSpEa)

		# #################################################################################
		# ### pre-generate areas of analysis (MCPs with buffers, buffers around points) ###
		# #################################################################################
		
		# ### ACCURATE: MCP
		
			# mcpAccsVectEa <- convexhull(accsVectEa)
			# mcpAccsVectEa <- workVectPoly(mcpAccsVectEa, cropBy=nam0VectEa)

		# ### ACCURATE: buffered points

			# buffAccsVectEa <- workVectPoly(accsVectEa, width=bufferWidth_km * 1000, cropBy=nam0VectEa)
		
		# ### ACCURATE: MCP + buffer
		
			# buffMcpAccsVectEa <- workVectPoly(mcpAccsVectEa, width=bufferWidth_km * 1000, cropBy=nam0VectEa)
		
		# ### ACCURATE + INACCURATE + ADMINISTRATIVE RECORDS: MCP
		
			# mcpAccsInaccsAdminSpEa <- if (exists('inaccsAdminSpEa')) {
				# mcpFromPolygons(inaccsAdminSpEa, accsSpEa)
			# } else {
				# mcpAccsSpEa
			# }
			
			# mcpAccsInaccsAdminVectEa <- vect(mcpAccsInaccsAdminSpEa)
			# mcpAccsInaccsAdminVectEa <- workVectPoly(mcpAccsInaccsAdminVectEa, cropBy=nam0VectEa)

		# ### ACCURATE + INACCURATE + ADMINISTRATIVE RECORDS: buffer

			# centsInaccsAdminVectEa <- centroids(inaccsAdminVectEa)
			# centsAccsInaccsAdminVectEa <- c(accsVectEa, centsInaccsAdminVectEa)
			
			# buffAccsInaccsAdminVectEa <- workVectPoly(centsAccsInaccsAdminVectEa, width=bufferWidth_km * 1000, cropBy=nam0VectEa)
			
		# ### ACCURATE + INACCURATE + ADMINISTRATIVE RECORDS: MCP + buffer
		
			# buffMcpAccsInaccsAdminVectEa <- workVectPoly(mcpAccsInaccsAdminVectEa, width=bufferWidth_km * 1000, cropBy=nam0VectEa)


		# ### cycle through RCP, background definition, RCP, and method for assigning climate to imprecise records
		# ###################################################################################################
		
		# for (rcp in c(45, 85)) {
		
			# # Stephen's ENM rasters
			# futEnms <- readRDS(paste0('./Analysis/ENMs/binary_maps_rcp', rcp, '_uncropped_Feb4_2021.rds'))

			# ### cycle through each method of defining background
			# for (background in c('convexHull', 'buffers')) {
			
				# say('   ', background)
			
				# ### cycle through each method of assigning climate to imprecise records
				# for (assignMethod in c('accurate', 'centroid', 'mean', 'closest', 'farthest')) {
				
					# say('      ', assignMethod)
				
					# ### ENM output from model using precise + imprecise records
					# modelCount <- if (background == 'convexHull') {
						# if (assignMethod == 'accurate') {
							# 1
						# } else if (assignMethod == 'centroid') {
							# 2
						# } else if (assignMethod == 'mean') {
							# 3
						# } else if (assignMethod == 'closest') {
							# 4
						# } else if (assignMethod == 'farthest') {
							# 5
						# }
					# } else if (background == 'buffers') {
						# if (assignMethod == 'accurate') {
							# 7
						# } else if (assignMethod == 'centroid') {
							# 8
						# } else if (assignMethod == 'mean') {
							# 9
						# } else if (assignMethod == 'closest') {
							# 10
						# } else if (assignMethod == 'farthest') {
							# 11
						# }
					# }
				
					# currentRast <- currentEnms[[modelCount]][[countSpecies]]
					# futRast <- futEnms[[modelCount]][[countSpecies]]
				
					# currentRast <- rast(currentRast)
					# futRast <- rast(futRast)
					
					# areaRast_km2 <- terra::area(currentRast, sum=FALSE) / (1000^2)
					
					# ### for each type of focal polygon
					# focalPolyVectNames <- c('mcpAccsVectEa', 'buffAccsVectEa', 'buffMcpAccsVectEa', 'mcpAccsInaccsAdminVectEa', 'buffAccsInaccsAdminVectEa', 'buffMcpAccsInaccsAdminVectEa')

					# thisExposure <- data.frame(
						# species = species,
						# rcp = rcp,
						# background = background,
						# assignMethod = assignMethod,
						# numAccs = nrow(accs),
						# numInaccsAdmin = length(inaccsAdminVectEa)
					# )
					
					# ### cycle through each polygon and extract area for current, future, stable, gain, loss
					# for (countFocalPoly in seq_along(focalPolyVectNames)) {
					
						# vectPolyName <- focalPolyVectNames[countFocalPoly]
						# say('         ', vectPolyName)
					
						# vectPoly <- get(vectPolyName)
						# areas_km2 <- calcExpose(currentRast, futRast, areaRast_km2, vectPoly)
						
						# newExpose <- data.frame(t(areas_km2))
						# newNames <- paste0(sub(vectPolyName, pattern='VectEa', replacement=''), '_', names(newExpose))
						# names(newExpose) <- newNames
						
						# thisExposure <- cbind(thisExposure, newExpose)
						
					# } # next focal polygon
				
					# exposure <- rbind(exposure, thisExposure)
				# } # next method for assigning climate to records
				
			# } # next way to define background region
			
		# } # next RCP
		
		# write.csv(exposure, './Analysis/ENMs/!Climate Change Exposure - Areal Values for Current, Future, Stable, Gain, and Loss Spp 41-44.csv', row.names=FALSE)
				
	# } # next species
	
# say('####################################')
# say('### plot climate change exposure ###')
# say('####################################')	

	# # exposure estimates
	# cce <- read.csv('./Analysis/ENMs/!Climate Change Exposure - Areal Values for Current, Future, Stable, Gain, and Loss.csv')
	
	# # generalization
	# rcps <- c(45, 85)
	# backgrounds <- c('convexHull', 'buffers')
	# assignMethods <- c('centroid', 'mean', 'closest', 'farthest')
	
	# epsilon <- 0.1 # add this value to numerator/denominator of log-ratios to obviate infinite or 0 NaN values
	
	
	# ### for each region in which to calculate climate change exposure
	# regions <- c('mcpAccs', 'mcpAccsInaccsAdmin', 'buffMcpAccs', 'buffAccsInaccsAdmin')
	
	# for (region in regions) {

		# regionNice <- if (region == 'mcpAccs') {
			# 'MCP of precise records'
		# } else if (region == 'mcpAccsInaccsAdmin') {
			# 'MCP of combined records'
		# } else if (region == 'buffMcpAccs') {
			# 'Buffered MCP of precise records'
		# } else if (region == 'buffAccsInaccsAdmin') {
			# 'Buffered MCP of combined records'
		# }

		# # calculate axis limits
		# allXyPlotVals <- c(
			# cce[cce$assignMethod=='accurate', paste0(region, '_futArea_km2')] / cce[cce$assignMethod=='accurate', paste0(region, '_currentArea_km2')],
			# cce[cce$assignMethod!='accurate', paste0(region, '_futArea_km2')] / cce[cce$assignMethod!='accurate', paste0(region, '_currentArea_km2')]
		# )
		
		# xyPlotLims <- range(pretty(allXyPlotVals))
		
		# allRatioVals <- c(
			# cce[cce$assignMethod!='accurate', paste0(region, '_stableArea_km2')] / cce[cce$assignMethod=='accurate', paste0(region, '_stableArea_km2')],
			# cce[cce$assignMethod!='accurate', paste0(region, '_lossArea_km2')] / cce[cce$assignMethod=='accurate', paste0(region, '_lossArea_km2')],
			# cce[cce$assignMethod!='accurate', paste0(region, '_gainArea_km2')] / cce[cce$assignMethod=='accurate', paste0(region, '_gainArea_km2')]
		# )
		
		# allRatioVals <- log10(allRatioVals + epsilon)
		
		# ratioLims <- range(pretty(allRatioVals))

		# ### plot!
		# for (rcp in rcps) {
				
			# for (background in backgrounds) {
						
				# bgNice <- if (background == 'convexHull') {
					# 'buffered MCP'
				# } else if (background == 'buffers') {
					# 'buffered points'
				# }
				
				# plots <- list()
			
				# for (assignMethod in assignMethods) {
				
					# say(region, ' ', rcp, ' ', background, ' ', assignMethod)
				
					# thisAccCce <- cce[cce$assignMethod == 'accurate' & cce$background==background & cce$rcp == rcp, ]
					# thisAccInaccAdminCce <- cce[cce$assignMethod == assignMethod & cce$background==background & cce$rcp == rcp, ]
					
					# n <- nrow(thisAccInaccAdminCce)
					# types <- c('Stable', 'Loss', 'Gain')
					# thisLong <- data.frame(
						# type = factor(rep(types, each=n), levels=types),
						# value = c(
							# log10((thisAccInaccAdminCce[ , paste0(region, '_stableArea_km2')] + epsilon) / (thisAccCce[ , paste0(region, '_stableArea_km2')] + epsilon)),
							# log10((thisAccInaccAdminCce[ , paste0(region, '_lossArea_km2')] + epsilon) / (thisAccCce[ , paste0(region, '_lossArea_km2')] + epsilon)),
							# log10((thisAccInaccAdminCce[ , paste0(region, '_gainArea_km2')] + epsilon) / (thisAccCce[ , paste0(region, '_gainArea_km2')] + epsilon))
						# )
					# )

					# ### ratio of stable, gain, loss area using combined vs just precise records
					# main <- paste0('Suitable area ratios\nwithin ', regionNice, '\n', assignMethod, ' | RCP', rcp / 10, ' | ', bgNice, ' ENMs')

					# plots[[length(plots) + 1]] <- ggplot(data=thisLong) +
						# geom_hline(yintercept=0) +
						# geom_boxplot(aes(x=type, y=value), fill=c('Gain'='darkolivegreen1', 'Loss'='darksalmon', 'Stable'='cornflowerblue')) +
						# ggtitle(main) +
						# ylab('log10(area using precise & imprecise /\narea using only precise)') +
						# xlab('') +
						# ylim(ratioLims[1], ratioLims[2]) +
						# theme(
							# plot.title=element_text(size=7, face='bold'),
							# axis.title=element_text(size=7)
						# )
						
						
				# } # assign methods
				
				# combined <- plot_grid(
					# plots[[1]], plots[[2]],
					# plots[[3]], plots[[4]],
					# align='h', ncol=2, rel_widths=c(1, 1))
				# print(combined)
				
				# file <- paste0('./Analysis/ENMs/Climate Change Exposure within ', capIt(regionNice), ' with Models Using ', capIt(bgNice), ' Background for RCP', rcp, '.png')
				# ggsave(file, width=13, height=17, units='cm', dpi=450)

			# } # backgrounds
			
		# } # RCPs
		
	# } # next region in which to evaluate climate change exposure
			
	
# say('############################################')
# say('### plot maps of climate change exposure ###')
# say('############################################')	

	# # exposure estimates
	# cce <- read.csv('./Analysis/ENMs/!Climate Change Exposure - Areal Values for Current, Future, Stable, Gain, and Loss.csv')
	
	# # generalization
	# # rcps <- c(45, 85)
	# rcps <- c(85)
	# backgrounds <- c('convexHull', 'buffers')
	# assignMethods <- c('accurate', 'centroid', 'mean', 'closest', 'farthest')
	
	# ### North American spatial polygons
	# load('./Regions/GADM Ver 3pt6 North America Level 0 WGS84.rda')
	# load('./Regions/GADM Ver 3pt6 North America Level 1 WGS84.rda')
	# load('./Regions/GADM Ver 3pt6 North America Level 2 WGS84.rda')

	# load('./Regions/GADM Ver 3pt6 North America Level 0 Albers.rda')
	# load('./Regions/GADM Ver 3pt6 North America Level 1 Albers.rda')
	# load('./Regions/GADM Ver 3pt6 North America Level 2 Albers.rda')

	# nam0VectSp <- vect(nam0Sp)
	# nam1VectSp <- vect(nam1Sp)
	# nam2VectSp <- vect(nam2Sp)

	# nam0VectEa <- vect(nam0SpEa)
	# nam1VectEa <- vect(nam1SpEa)
	# nam2VectEa <- vect(nam2SpEa)

	# # state/county names
	# stateGeog <- tolower(paste0(nam1Sp$NAME_1))
	# stateCountyGeog <- tolower(paste0(nam2Sp$NAME_1, nam2Sp$NAME_2))

	# # species' records
	# load('./Data/Asclepias 03 Specimen Records and Environmental Data.rda')
	# speciesList <- asclepias$meta$usableSpecies$species

	# # Stephen's ENM rasters
	# currentEnms <- readRDS('./Analysis/ENMs/binary_maps_current_uncropped_Feb4_2021.rds')

	# ### plot!
	# for (countSpecies in seq_along(speciesList)) {
			
		# species <- speciesList[countSpecies]
		
		# ### accurate records
		# ####################
			
			# spp <- tolower(gsub(species, pattern=' ', replacement='_'))
			# accs <- asclepias$bySpecies[[spp]]$accurateUsableNoDupRecs[ , llGbif]
			# names(accs) <- c('x', 'y')
			# accsSpUnproj <- SpatialPoints(accs, getCRS('wgs84', TRUE))
			# accsVectUnproj <- vect(as.matrix(accs), 'points', crs=getCRS('wgs84'))

			# accsSpEa <- sp::spTransform(accsSpUnproj, getCRS('albersNA', TRUE))
			# accsVectEa <- project(accsVectUnproj, getCRS('albersNA'))

			# # centroid of accurate records
			# centsAccsVectEa <- centroids(accsVectEa)

		# ### imprecise records (points with buffers, counties, states)
		# #############################################################

			# # inaccurate records (points with buffers)
			# index <- which(asclepias$bySpecies[[countSpecies]]$inaccurateAdminUsableNoDupRecs$recordType %in% inaccAssigns)
			# if (length(index) > 0) {

				# inaccs <- asclepias$bySpecies[[countSpecies]]$inaccurateAdminUsableNoDupRecs[index, ]
				# inaccsSp <- SpatialPointsDataFrame(inaccs[ , llGbif], data=inaccs, proj4string=getCRS('wgs84', TRUE))
				# inaccsSpEa <- sp::spTransform(inaccsSp, getCRS('albersNA', TRUE))
				# inaccsSpEa <- gBuffer(inaccsSpEa, width=inaccsSpEa$maxCoordUncerPrecision_m, byid=TRUE, quadsegs=12)
				# inaccsSpEa <- as(inaccsSpEa, 'SpatialPolygons')
				# projection(inaccsSpEa) <- getCRS('albersNA')
				
				# inaccsAdminSpEa <- inaccsSpEa
				
			# }

			# # county records
			# index <- which(asclepias$bySpecies[[countSpecies]]$inaccurateAdminUsableNoDupRecs$recordType %in% countyAssigns)
			# if (length(index) > 0) {

				# county <- asclepias$bySpecies[[countSpecies]]$inaccurateAdminUsableNoDupRecs[index, ]
				# stateCountyRecs <- tolower(paste0(county$stateFromGeog, county$countyFromGeog))
				# countySpEa <- nam2SpEa[which(stateCountyGeog %in% stateCountyRecs), ]
				# countySpEa <- as(countySpEa, 'SpatialPolygons')
				
				# inaccsAdminSpEa <- if (exists('inaccsAdminSpEa')) {
					# rbind(inaccsAdminSpEa, countySpEa, makeUniqueIDs=TRUE)
				# } else {
					# countySpEa
				# }
				
			# }
			
			# # state records
			# index <- which(asclepias$bySpecies[[countSpecies]]$inaccurateAdminUsableNoDupRecs$recordType %in% stateAssigns)
			# if (length(index) > 0) {

				# state <- asclepias$bySpecies[[countSpecies]]$inaccurateAdminUsableNoDupRecs[index, ]
				# stateRecs <- tolower(state$stateFromGeog)
				# stateSpEa <- nam1SpEa[stateGeog %in% stateRecs, ]
				
				# inaccsAdminSpEa <- if (exists('inaccsAdminSpEa')) {
					# rbind(inaccsAdminSpEa, stateSpEa, makeUniqueIDs = TRUE)
				# } else {
					# stateSpEa
				# }
				
			# }

			# inaccsAdminVectEa <- vect(inaccsAdminSpEa)
			# inaccsAdminVectUnproj <- project(inaccsAdminVectEa, getCRS('wgs84'))
			
		# ### pre-generate areas of analysis
		# ##################################
				
			# ### ACCURATE: MCP
			
				# mcpAccsVectEa <- convexhull(accsVectEa)
				# mcpAccsVectEa <- workVectPoly(mcpAccsVectEa, cropBy=nam0VectEa)
				# mcpAccsVectUnproj <- project(mcpAccsVectEa, getCRS('wgs84'))

			# # ### ACCURATE: buffered points

				# # buffAccsVectEa <- workVectPoly(accsVectEa, width=bufferWidth_km * 1000, cropBy=nam0VectEa)
			
			# ### ACCURATE: MCP + buffer
			
				# buffMcpAccsVectEa <- workVectPoly(mcpAccsVectEa, width=bufferWidth_km * 1000, cropBy=nam0VectEa)
				# buffMcpAccsVectUnproj <- project(buffMcpAccsVectEa, getCRS('wgs84'))
			
			# ### ACCURATE + INACCURATE + ADMINISTRATIVE RECORDS: MCP
			
				# mcpAccsInaccsAdminSpEa <- if (exists('inaccsAdminSpEa')) {
					# mcpFromPolygons(inaccsAdminSpEa, accsSpEa)
				# } else {
					# mcpAccsSpEa
				# }
				
				# mcpAccsInaccsAdminVectEa <- vect(mcpAccsInaccsAdminSpEa)
				# mcpAccsInaccsAdminVectEa <- workVectPoly(mcpAccsInaccsAdminVectEa, cropBy=nam0VectEa)
				# mcpAccsInaccsAdminVectUnproj <- project(mcpAccsInaccsAdminVectEa, getCRS('wgs84'))

			# # ### ACCURATE + INACCURATE + ADMINISTRATIVE RECORDS: buffer

				# # centsInaccsAdminVectEa <- centroids(inaccsAdminVectEa)
				# # centsAccsInaccsAdminVectEa <- c(accsVectEa, centsInaccsAdminVectEa)
				
				# # buffAccsInaccsAdminVectEa <- workVectPoly(centsAccsInaccsAdminVectEa, width=bufferWidth_km * 1000, cropBy=nam0VectEa)
				
			# ### ACCURATE + INACCURATE + ADMINISTRATIVE RECORDS: MCP + buffer
			
				# buffMcpAccsInaccsAdminVectEa <- workVectPoly(mcpAccsInaccsAdminVectEa, width=bufferWidth_km * 1000, cropBy=nam0VectEa)
				
				# buffMcpAccsInaccsAdminVectUnproj <- project(buffMcpAccsInaccsAdminVectEa, getCRS('wgs84'))
			
		# for (rcp in rcps) {
				
			# # Stephen's ENM rasters
			# futEnms <- readRDS(paste0('./Analysis/ENMs/binary_maps_rcp', rcp, '_uncropped_Feb4_2021.rds'))
				
			# file <- paste0('./Analysis/ENMs/Maps of Climate Change Exposure for ', species, ' under RCP', rcp, '.png')
			# png(file, width=1920, height=1080, res=120)

			# par(mfrow=c(2, 5), oma=c(1, 1, 3, 1))
				
			# for (background in backgrounds) {
						
				# bgNice <- if (background == 'convexHull') {
					# 'buffered MCP models'
				# } else if (background == 'buffers') {
					# 'buffered points models'
				# }
				
				# plots <- list()
			
				# for (assignMethod in assignMethods) {
				
					# say(species, ' ', rcp, ' ', background, ' ', assignMethod)
					
					# ### ENM output from model using precise + imprecise records
					# modelCount <- if (background == 'convexHull') {
						# if (assignMethod == 'accurate') {
							# 1
						# } else if (assignMethod == 'centroid') {
							# 2
						# } else if (assignMethod == 'mean') {
							# 3
						# } else if (assignMethod == 'closest') {
							# 4
						# } else if (assignMethod == 'farthest') {
							# 5
						# }
					# } else if (background == 'buffers') {
						# if (assignMethod == 'accurate') {
							# 7
						# } else if (assignMethod == 'centroid') {
							# 8
						# } else if (assignMethod == 'mean') {
							# 9
						# } else if (assignMethod == 'closest') {
							# 10
						# } else if (assignMethod == 'farthest') {
							# 11
						# }
					# }
					
					# ### raster and points
					# current <- currentEnms[[modelCount]][[countSpecies]]
					# fut <- futEnms[[modelCount]][[countSpecies]]
					
					# current <- rast(current)						
					# fut <- rast(fut)						
					
					# mask <- terra::rasterize(buffMcpAccsInaccsAdminVectUnproj, current) * 0 + 1
					# mask <- crop(mask, buffMcpAccsInaccsAdminVectUnproj)
					
					# current <- crop(current, mask)
					# fut <- crop(fut, mask)
					
					# current <- current * mask
					# fut <- fut * mask
					
					# currentNA <- app(current, function(x) ifelse(x==0, NA, x))
					# futNA <- app(fut, function(x) ifelse(x==0, NA, x))
					
					# delta <- fut - current
					# sumNA <- futNA + currentNA

					# stable <- sumNA * 0 + 1
					# gain <- app(delta, function(x) ifelse(x == 1, 1, NA))
					# loss <- app(delta, function(x) ifelse(x == -1, 1, NA))
					
					# admins <- crop(nam1VectSp, mask)

					# ### plot
					# main <- paste(assignMethod, '|', bgNice)
					# plot(stable, col=NA, legend=FALSE, main=main)
					# plot(inaccsAdminVectUnproj, border='orange', col=alpha('orange', 0.25), lwd=1.4, add=TRUE)
					# plot(stable, col='gray45', legend=FALSE, add=TRUE)
					# if (!is.na(global(gain, 'sum', na.rm=TRUE)$sum)) plot(gain, col='forestgreen', legend=FALSE, add=TRUE)
					# if (!is.na(global(loss, 'sum', na.rm=TRUE)$sum)) plot(loss, col='firebrick3', legend=FALSE, add=TRUE)
					
					# plot(admins, border='gray', add=TRUE)

					# points(accsVectUnproj, pch=21, bg='orange')
					# plot(inaccsAdminVectUnproj, border='orange', lwd=1.4, add=TRUE)

					# plot(mcpAccsVectUnproj, lwd=1.8, border='blue', add=TRUE)
					# plot(buffMcpAccsVectUnproj, lwd=1.8, border='blue', add=TRUE)
					# plot(mcpAccsInaccsAdminVectUnproj, lwd=1.8, border='blue', add=TRUE)
					# plot(buffMcpAccsInaccsAdminVectUnproj, lwd=1.8, border='blue', add=TRUE)

					# legend('bottomleft', legend=c('stable', 'gain', 'loss', 'occurrence', 'region'), fill=c('gray45', 'forestgreen', 'firebrick3', alpha('orange', 0.25), NA), col=c(NA, NA, NA, 'black', 'blue'), col.bg=c(NA, NA, NA, 'orange', NA), border=c(NA, NA, NA, 'orange', NA), lwd=c(NA, NA, NA, NA, 1.8), pch=c(NA, NA, NA, 21, NA), bty='n', cex=1.2)
				
				# } # assign methods
				
			# } # backgrounds
			
			# main=paste0(species, ' | RCP ', rcp / 10)
			# title(main, outer=TRUE, line=1, cex.main=1.8)
			
			# dev.off()
			
		# } # RCPs
		
	# } # next species

# say('###############################################################################')
# say('### plot climate change exposure metrics within MCP of accurate occurrences ###')
# say('###############################################################################')

	# # exposure estimates
	# cce <- read.csv('./Analysis/ENMs/!Climate Change Exposure - Areal Values for Current, Future, Stable, Gain, and Loss.csv')

	# ### stable area within precise MCP
	# ##################################
		
		# accStablePreciseMcp <- cce$mcpAccs_stableArea_km2[cce$rcp==45 & cce$background=='convexHull' & cce$assignMethod=='accurate']
		# allStablePreciseMcp <- cce$mcpAccs_stableArea_km2[cce$rcp==45 & cce$background=='convexHull' & cce$assignMethod=='closest']

		# accStablePreciseAndImpreciseMcp <- cce$mcpAccsInaccsAdmin_stableArea_km2[cce$rcp==45 & cce$background=='convexHull' & cce$assignMethod=='accurate']
		# allStablePreciseAndImpreciseMcp <- cce$mcpAccsInaccsAdmin_stableArea_km2[cce$rcp==45 & cce$background=='convexHull' & cce$assignMethod=='closest']

		# data <- data.frame(acc=accStablePreciseMcp, all=allStablePreciseMcp)
		
		# lim <- c(0, max(accStablePreciseMcp, allStablePreciseMcp, accStablePreciseAndImpreciseMcp, allStablePreciseAndImpreciseMcp))
		
		# lim <- 25 * round(ceiling(lim / 25))
		
		# stableMcpPrecise <- ggplot(data, aes(x=acc, y=all)) +
			# geom_abline(slope=1, intercept=0, col='gray45') +
			# geom_point(size=2.1, shape=21, bg=alpha('cornflowerblue', 0.6)) +
			# xlim(lim[1], lim[2]) + ylim(lim[1], lim[2]) +
			# labs(title='Stable suitable area', x=bquote('Precise only'~(km^2)), y=bquote('Precise & imprecise'~(km^2))) +
			# theme(axis.text=element_text(size=8), axis.title=element_text(size=11, face='bold'))

		# # print(stable)

	# ### stable area within precise & imprecise MCP
	# ###############################################
		
		# data <- data.frame(acc=accStablePreciseAndImpreciseMcp, all=allStablePreciseAndImpreciseMcp)
		
		# stableMcpPreciseAndImprecise <- ggplot(data, aes(x=acc, y=all)) +
			# geom_abline(slope=1, intercept=0, col='gray45') +
			# geom_point(size=2.1, shape=21, bg=alpha('cornflowerblue', 0.6)) +
			# xlim(lim[1], lim[2]) + ylim(lim[1], lim[2]) +
			# labs(title='Stable suitable area', x=bquote('Precise only'~(km^2)), y=bquote('Precise & imprecise'~(km^2))) +
			# theme(axis.text=element_text(size=8), axis.title=element_text(size=11, face='bold'))

		# # print(stable)

	# ### loss area within precise MCP
	# ################################
		
		# accLossPreciseMcp <- cce$mcpAccs_lossArea_km2[cce$rcp==45 & cce$background=='convexHull' & cce$assignMethod=='accurate']
		# allLossPreciseMcp <- cce$mcpAccs_lossArea_km2[cce$rcp==45 & cce$background=='convexHull' & cce$assignMethod=='closest']

		# accLossPreciseAndImpreciseMcp <- cce$mcpAccsInaccsAdmin_lossArea_km2[cce$rcp==45 & cce$background=='convexHull' & cce$assignMethod=='accurate']
		# allLossPreciseAndImpreciseMcp <- cce$mcpAccsInaccsAdmin_lossArea_km2[cce$rcp==45 & cce$background=='convexHull' & cce$assignMethod=='closest']

		# accGainPreciseMcp <- cce$mcpAccs_gainArea_km2[cce$rcp==45 & cce$background=='convexHull' & cce$assignMethod=='accurate']
		# allGainPreciseMcp <- cce$mcpAccs_gainArea_km2[cce$rcp==45 & cce$background=='convexHull' & cce$assignMethod=='closest']

		# accGainPreciseAndImpreciseMcp <- cce$mcpAccsInaccsAdmin_gainArea_km2[cce$rcp==45 & cce$background=='convexHull' & cce$assignMethod=='accurate']
		# allGainPreciseAndImpreciseMcp <- cce$mcpAccsInaccsAdmin_gainArea_km2[cce$rcp==45 & cce$background=='convexHull' & cce$assignMethod=='closest']

		# data <- data.frame(acc=accLossPreciseMcp, all=allLossPreciseMcp)
		
		# lim <- c(0, max(accLossPreciseMcp, allLossPreciseMcp, accLossPreciseAndImpreciseMcp, allLossPreciseAndImpreciseMcp, accGainPreciseMcp, allGainPreciseMcp, accGainPreciseAndImpreciseMcp, allGainPreciseAndImpreciseMcp))
		# lim <- 25 * round(ceiling(lim / 25))
		
		# lossMcpPrecise <- ggplot(data, aes(x=acc, y=all)) +
			# geom_abline(slope=1, intercept=0, col='gray45') +
			# geom_point(size=2.1, shape=21, bg=alpha('firebrick1', 0.6)) +
			# xlim(lim[1], lim[2]) + ylim(lim[1], lim[2]) +
			# labs(title='Loss in suitable area', x=bquote('Precise only'~(km^2)), y=bquote('Precise & imprecise'~(km^2))) +
			# theme(axis.text=element_text(size=8), axis.title=element_text(size=11, face='bold'))

		# # print(loss)
		
	# ### loss area within precise & imprecise MCP
	# ############################################
		
		# data <- data.frame(acc=accLossPreciseAndImpreciseMcp, all=allLossPreciseAndImpreciseMcp)
		
		# lossMcpPreciseAndImprecise <- ggplot(data, aes(x=acc, y=all)) +
			# geom_abline(slope=1, intercept=0, col='gray45') +
			# geom_point(size=2.1, shape=21, bg=alpha('firebrick1', 0.6)) +
			# xlim(lim[1], lim[2]) + ylim(lim[1], lim[2]) +
			# labs(title='Loss in suitable area', x=bquote('Precise only'~(km^2)), y=bquote('Precise & imprecise'~(km^2))) +
			# theme(axis.text=element_text(size=8), axis.title=element_text(size=11, face='bold'))

		# # print(loss)
		
	# ### gain area within precise MCP
	# ################################
		
		# data <- data.frame(acc=accGainPreciseMcp, all=allGainPreciseMcp)
		
		# gainMcpPrecise <- ggplot(data, aes(x=acc, y=all)) +
			# geom_abline(slope=1, intercept=0, col='gray45') +
			# geom_point(size=2.1, shape=21, bg=alpha('chartreuse', 0.6)) +
			# xlim(lim[1], lim[2]) + ylim(lim[1], lim[2]) +
			# labs(title='Gain in suitable area', x=bquote('Precise only'~(km^2)), y=bquote('Precise & imprecise'~(km^2))) +
			# theme(axis.text=element_text(size=8), axis.title=element_text(size=11, face='bold'))

		# # print(gain)

	# ### gain area within precise & imprecise MCP
	# ############################################
		
		# data <- data.frame(acc=accGainPreciseAndImpreciseMcp, all=allGainPreciseAndImpreciseMcp)
		
		# gainMcpPreciseAndImprecise <- ggplot(data, aes(x=acc, y=all)) +
			# geom_abline(slope=1, intercept=0, col='gray45') +
			# geom_point(size=2.1, shape=21, bg=alpha('chartreuse', 0.6)) +
			# xlim(lim[1], lim[2]) + ylim(lim[1], lim[2]) +
			# labs(title='Gain in suitable area', x=bquote('Precise only'~(km^2)), y=bquote('Precise & imprecise'~(km^2))) +
			# theme(axis.text=element_text(size=8), axis.title=element_text(size=11, face='bold'))

		# # print(gain)

	# # ### net change in area within precise MCP
	# # #########################################
		
		# # accNetChangePreciseMcp <- accGainPreciseMcp - accLossPreciseMcp
		# # allNetChangePreciseMcp <- allGainPreciseMcp - allLossPreciseMcp
		
		# # accNetChangePreciseAndImpreciseMcp <- accGainPreciseAndImpreciseMcp - accLossPreciseAndImpreciseMcp
		# # allNetChangePreciseAndImpreciseMcp <- allGainPreciseAndImpreciseMcp - allLossPreciseAndImpreciseMcp
		
		# # lim <- c(
			# # min(accNetChangePreciseMcp, allNetChangePreciseMcp, accNetChangePreciseAndImpreciseMcp, allNetChangePreciseAndImpreciseMcp),
			# # max(accNetChangePreciseMcp, allNetChangePreciseMcp, accNetChangePreciseAndImpreciseMcp, allNetChangePreciseAndImpreciseMcp)
		# # )
		
		# # lim <- c(20 * round(floor(lim[1] / 20)), 100 * round(ceiling(lim[2] / 100)))
		
		# # data <- data.frame(
			# # acc=accNetChangePreciseMcp,
			# # all=allNetChangePreciseMcp
		# # )
		
		# # netChangeMcpPrecise <- ggplot(data, aes(x=acc, y=all)) +
			# # geom_abline(slope=1, intercept=0, col='gray45') +
			# # geom_point(size=2.1, shape=21, bg=alpha('gray', 0.6)) +
			# # xlim(lim[1], lim[2]) + ylim(lim[1], lim[2]) +
			# # labs(title='Next change in suitable area', x=bquote('Precise only'~(km^2)), y=bquote('Precise & imprecise'~(km^2))) +
			# # theme(axis.text=element_text(size=8), axis.title=element_text(size=11, face='bold'))

	# # ### net change in area within precise & imprecise MCP
	# # #####################################################
		
		# # data <- data.frame(
			# # acc=accNetChangePreciseAndImpreciseMcp,
			# # all=allNetChangePreciseAndImpreciseMcp
		# # )
		
		# # netChangeMcpPreciseAndImprecise <- ggplot(data, aes(x=acc, y=all)) +
			# # geom_abline(slope=1, intercept=0, col='gray45') +
			# # geom_point(size=2.1, shape=21, bg=alpha('gray', 0.6)) +
			# # xlim(lim[1], lim[2]) + ylim(lim[1], lim[2]) +
			# # labs(title='Next change in suitable area', x=bquote('Precise only'~(km^2)), y=bquote('Precise & imprecise'~(km^2))) +
			# # theme(axis.text=element_text(size=8), axis.title=element_text(size=11, face='bold'))

		# # # print(gain)

	# ### composite
	# # main <- plot_grid(stableMcpPrecise, lossMcpPrecise, gainMcpPrecise, netChangeMcpPrecise, stableMcpPreciseAndImprecise, lossMcpPreciseAndImprecise, gainMcpPreciseAndImprecise, netChangeMcpPreciseAndImprecise, labels=c('a)', 'b)', 'c)', 'd)', 'e)', 'f)', 'g)', 'h'), align='h', label_size=14, ncol=4, rel_widths=1)
	# # main <- plot_grid(stableMcpPrecise, lossMcpPrecise, gainMcpPrecise, stableMcpPreciseAndImprecise, lossMcpPreciseAndImprecise, gainMcpPreciseAndImprecise, labels=c('a)', 'b)', 'c)', 'd)', 'e)', 'f)'), align='h', label_size=14, ncol=3, rel_widths=1)
	# main <- plot_grid(stableMcpPrecise, lossMcpPrecise, gainMcpPrecise, labels=c('d)', 'e)', 'f)'), align='h', label_size=14, ncol=3, rel_widths=1)
	
	# print(main)
	
	# ggsave('./Analysis/ENMs/!Climate Change Exposure within MCP of Precise Records.pdf', width=8, height=8/3, units='in')

# say('###################################################################################')
# say('### plot climate change exposure metrics within buffered MCP of all occurrences ###')
# say('###################################################################################')

	# # exposure estimates
	# cce <- read.csv('./Analysis/ENMs/!Climate Change Exposure - Areal Values for Current, Future, Stable, Gain, and Loss.csv')

	# ### current suitable area
	# #########################
		
		# accPreciseMcp <- cce$buffAccsInaccsAdmin_currentArea_km2[cce$rcp==85 & cce$background=='convexHull' & cce$assignMethod=='accurate']
		# allPreciseMcp <- cce$buffAccsInaccsAdmin_currentArea_km2[cce$rcp==85 & cce$background=='convexHull' & cce$assignMethod=='closest']

		# accPreciseAndImpreciseMcp <- cce$buffAccsInaccsAdmin_currentArea_km2[cce$rcp==85 & cce$background=='convexHull' & cce$assignMethod=='accurate']
		# allPreciseAndImpreciseMcp <- cce$buffAccsInaccsAdmin_currentArea_km2[cce$rcp==85 & cce$background=='convexHull' & cce$assignMethod=='closest']

		# data <- data.frame(acc=accPreciseMcp, all=allPreciseMcp)
		
		# lim <- c(0, max(accPreciseMcp, allPreciseMcp, accPreciseAndImpreciseMcp, allPreciseAndImpreciseMcp))
		
		# lim <- 25 * round(ceiling(lim / 25))
		
		# sq <- ggplot(data, aes(x=acc, y=all)) +
			# geom_abline(slope=1, intercept=0, col='gray45') +
			# geom_point(size=2.1, shape=21, bg=alpha('cornflowerblue', 0.6)) +
			# xlim(lim[1], lim[2]) + ylim(lim[1], lim[2]) +
			# labs(title='Current suitable area', x=bquote('Precise only'~(km^2)), y=bquote('Precise & imprecise'~(km^2))) +
			# theme(axis.text=element_text(size=8), axis.title=element_text(size=10, face='bold'))

	# ### future suitable area
	# ########################
		
		# accPreciseMcp <- cce$buffAccsInaccsAdmin_futArea_km2[cce$rcp==85 & cce$background=='convexHull' & cce$assignMethod=='accurate']
		# allPreciseMcp <- cce$buffAccsInaccsAdmin_futArea_km2[cce$rcp==85 & cce$background=='convexHull' & cce$assignMethod=='closest']

		# accPreciseAndImpreciseMcp <- cce$buffAccsInaccsAdmin_futArea_km2[cce$rcp==85 & cce$background=='convexHull' & cce$assignMethod=='accurate']
		# allPreciseAndImpreciseMcp <- cce$buffAccsInaccsAdmin_futArea_km2[cce$rcp==85 & cce$background=='convexHull' & cce$assignMethod=='closest']

		# data <- data.frame(acc=accPreciseMcp, all=allPreciseMcp)
		
		# lim <- c(0, max(accPreciseMcp, allPreciseMcp, accPreciseAndImpreciseMcp, allPreciseAndImpreciseMcp))
		
		# lim <- 25 * round(ceiling(lim / 25))
		
		# fut <- ggplot(data, aes(x=acc, y=all)) +
			# geom_abline(slope=1, intercept=0, col='gray45') +
			# geom_point(size=2.1, shape=21, bg=alpha('cornflowerblue', 0.6)) +
			# xlim(lim[1], lim[2]) + ylim(lim[1], lim[2]) +
			# labs(title='Future suitable area', x=bquote('Precise only'~(km^2)), y=bquote('Precise & imprecise'~(km^2))) +
			# theme(axis.text=element_text(size=8), axis.title=element_text(size=10, face='bold'))

		# # print(stable)

	# ### gain in suitable area
	# #########################
		
		# accPreciseMcp <- cce$mcpAccsInaccsAdmin_gainArea_km2[cce$rcp==85 & cce$background=='convexHull' & cce$assignMethod=='accurate']
		# allPreciseMcp <- cce$mcpAccsInaccsAdmin_gainArea_km2[cce$rcp==85 & cce$background=='convexHull' & cce$assignMethod=='closest']

		# accPreciseAndImpreciseMcp <- cce$mcpAccsInaccsAdmin_gainArea_km2[cce$rcp==85 & cce$background=='convexHull' & cce$assignMethod=='accurate']
		# allPreciseAndImpreciseMcp <- cce$mcpAccsInaccsAdmin_gainArea_km2[cce$rcp==85 & cce$background=='convexHull' & cce$assignMethod=='closest']

		# data <- data.frame(acc=accPreciseMcp, all=allPreciseMcp)
		
		# lim <- c(0, max(accPreciseMcp, allPreciseMcp, accPreciseAndImpreciseMcp, allPreciseAndImpreciseMcp))
		
		# lim <- 10 * round(ceiling(lim / 25))
		
		# gain <- ggplot(data, aes(x=acc, y=all)) +
			# geom_abline(slope=1, intercept=0, col='gray45') +
			# geom_point(size=2.1, shape=21, bg=alpha('cornflowerblue', 0.6)) +
			# xlim(lim[1], lim[2]) + ylim(lim[1], lim[2]) +
			# labs(title='Gain in suitable area', x=bquote('Precise only'~(km^2)), y=bquote('Precise & imprecise'~(km^2))) +
			# theme(axis.text=element_text(size=8), axis.title=element_text(size=10, face='bold'))

		# # print(stable)

	# ### loss in suitable area
	# #########################
		
		# accPreciseMcp <- cce$mcpAccsInaccsAdmin_lossArea_km2[cce$rcp==85 & cce$background=='convexHull' & cce$assignMethod=='accurate']
		# allPreciseMcp <- cce$mcpAccsInaccsAdmin_lossArea_km2[cce$rcp==85 & cce$background=='convexHull' & cce$assignMethod=='closest']

		# accPreciseAndImpreciseMcp <- cce$mcpAccsInaccsAdmin_lossArea_km2[cce$rcp==85 & cce$background=='convexHull' & cce$assignMethod=='accurate']
		# allPreciseAndImpreciseMcp <- cce$mcpAccsInaccsAdmin_lossArea_km2[cce$rcp==85 & cce$background=='convexHull' & cce$assignMethod=='closest']

		# data <- data.frame(acc=accPreciseMcp, all=allPreciseMcp)
		
		# lim <- c(0, max(accPreciseMcp, allPreciseMcp, accPreciseAndImpreciseMcp, allPreciseAndImpreciseMcp))
		
		# lim <- 10 * round(ceiling(lim / 25))
		
		# loss <- ggplot(data, aes(x=acc, y=all)) +
			# geom_abline(slope=1, intercept=0, col='gray45') +
			# geom_point(size=2.1, shape=21, bg=alpha('cornflowerblue', 0.6)) +
			# xlim(lim[1], lim[2]) + ylim(lim[1], lim[2]) +
			# labs(title='Loss in suitable area', x=bquote('Precise only'~(km^2)), y=bquote('Precise & imprecise'~(km^2))) +
			# theme(axis.text=element_text(size=8), axis.title=element_text(size=10, face='bold'))

		# # print(stable)

	# ### composite
	# main <- plot_grid(sq, fut, gain, loss, labels=c('d)', 'e)', 'f)', 'g)'), align='h', label_size=14, ncol=2, rel_widths=1)
	
	# print(main)
	
	# ggsave('./Analysis/ENMs/!Climate Change Exposure within MCP of All Records.pdf', width=5, height=5, units='in')

say('###################################################################################')
say('### plot climate change exposure metrics within buffered MCP of all occurrences ###')
say('###################################################################################')

	# exposure estimates
	cce <- read.csv('./Analysis/ENMs/!Climate Change Exposure - Areal Values for Current, Future, Stable, Gain, and Loss.csv')

	### current suitable area
	#########################
		
		accPreciseMcp <- cce$buffAccsInaccsAdmin_currentArea_km2[cce$rcp==85 & cce$background=='convexHull' & cce$assignMethod=='accurate']
		allPreciseMcp <- cce$buffAccsInaccsAdmin_currentArea_km2[cce$rcp==85 & cce$background=='convexHull' & cce$assignMethod=='closest']

		accPreciseAndImpreciseMcp <- cce$buffAccsInaccsAdmin_currentArea_km2[cce$rcp==85 & cce$background=='convexHull' & cce$assignMethod=='accurate']
		allPreciseAndImpreciseMcp <- cce$buffAccsInaccsAdmin_currentArea_km2[cce$rcp==85 & cce$background=='convexHull' & cce$assignMethod=='closest']

		data <- data.frame(acc=accPreciseMcp, all=allPreciseMcp)
		
		lim <- c(0, max(accPreciseMcp, allPreciseMcp, accPreciseAndImpreciseMcp, allPreciseAndImpreciseMcp))
		
		lim <- 25 * round(ceiling(lim / 25))
		
		sq <- ggplot(data, aes(x=acc, y=all)) +
			geom_abline(slope=1, intercept=0, col='gray45') +
			geom_point(size=2.1, shape=21, bg=alpha('cornflowerblue', 0.6)) +
			xlim(lim[1], lim[2]) + ylim(lim[1], lim[2]) +
			labs(title='Current suitable area', x=bquote('Precise only'~(km^2)), y=bquote('Precise & imprecise'~(km^2))) +
			theme(
				plot.title=element_text(size=10),
				axis.text=element_text(size=8),
				axis.title=element_text(size=10, face='bold')
			)

	### future suitable area
	########################
		
		accPreciseMcp <- cce$buffAccsInaccsAdmin_futArea_km2[cce$rcp==85 & cce$background=='convexHull' & cce$assignMethod=='accurate']
		allPreciseMcp <- cce$buffAccsInaccsAdmin_futArea_km2[cce$rcp==85 & cce$background=='convexHull' & cce$assignMethod=='closest']

		accPreciseAndImpreciseMcp <- cce$buffAccsInaccsAdmin_futArea_km2[cce$rcp==85 & cce$background=='convexHull' & cce$assignMethod=='accurate']
		allPreciseAndImpreciseMcp <- cce$buffAccsInaccsAdmin_futArea_km2[cce$rcp==85 & cce$background=='convexHull' & cce$assignMethod=='closest']

		data <- data.frame(acc=accPreciseMcp, all=allPreciseMcp)
		
		lim <- c(0, max(accPreciseMcp, allPreciseMcp, accPreciseAndImpreciseMcp, allPreciseAndImpreciseMcp))
		
		lim <- 25 * round(ceiling(lim / 25))
		
		fut <- ggplot(data, aes(x=acc, y=all)) +
			geom_abline(slope=1, intercept=0, col='gray45') +
			geom_point(size=2.1, shape=21, bg=alpha('cornflowerblue', 0.6)) +
			xlim(lim[1], lim[2]) + ylim(lim[1], lim[2]) +
			labs(title='Future suitable area', x=bquote('Precise only'~(km^2)), y=bquote('Precise & imprecise'~(km^2))) +
			theme(
				plot.title=element_text(size=10),
				axis.text=element_text(size=8),
				axis.title=element_text(size=10, face='bold')
			)

		# print(stable)

	### composite
	main <- plot_grid(sq, fut, labels=c('a)', 'b)'), label_size = 11, align='h', ncol=2, rel_widths=1)
	
	print(main)
	
	ggsave('./Analysis/ENMs/!Suitable Habitat within MCP of All Records.pdf', width=6, height=3, units='in')




say('DONE! ', date(), level=1, deco='%')
