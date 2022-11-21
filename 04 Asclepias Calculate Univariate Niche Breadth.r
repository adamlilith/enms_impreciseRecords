### CALCULATE UNIVARIATE NICHE BREADTH
### David Henderson 2019 | Adam B. Smith 2020
### 
### source('C:/Ecology/Drive/Research Active/ENMs - Vaguely Georeferenced Specimen Records/enms_impreciseRecords/04 Asclepias Calculate Univariate Niche Breadth.r')
### source('E:/Ecology/Drive/Research Active/ENMs - Vaguely Georeferenced Specimen Records/enms_impreciseRecords/04 Asclepias Calculate Univariate Niche Breadth.r')

### CONTENTS ###
### setup ###
### calculate univariate niche breadth ###

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
	
	setwd(paste0(drive, '/Ecology/Drive/Research Active/ENMs - Vaguely Georeferenced Specimen Records'))

	library(cowplot)
	library(ggplot2)
	library(rgeos)
	library(sp)
	library(terra)
	
	# custom (Adam Smith)
	library(omnibus)
	library(enmSdm)

	### names
	llGbif <- c('decimalLongitude', 'decimalLatitude')
	
	### designations for accurate, inaccurate, and county records
	accAssigns <- 'certain/precise'
	inaccAssigns <- c('uncertain/precise', 'certain/imprecise', 'uncertain/imprecise')
	adminAssigns <- c('county/precise', 'county/imprecise', 'county-only', 'state/imprecise', 'state-only')
	countyAssigns <- c('county/precise', 'county/imprecise', 'county-only')
	stateAssigns <- c('state/imprecise', 'state-only')

say('##########################################')
say('### calculate univariate niche breadth ###')
say('##########################################')

	### generalization
	bios <- paste0('bio', c(1, 12)) # BIOCLIMs used for niche breadth
	
	# quantiles to define niche
	lowerQuant <- 0
	upperQuant <- 1

	### North American spatial polygons

		load('./Regions/GADM Ver 3pt6 North America Level 2 Albers.rda')
		load('./Regions/GADM Ver 3pt6 North America Level 1 Albers.rda')
		load('./Regions/GADM Ver 3pt6 North America Level 0 WGS84.rda')

		# state/county names
		stateGeog <- tolower(paste0(nam1SpEa$NAME_1))
		stateCountyGeog <- tolower(paste0(nam2SpEa$NAME_1, nam2SpEa$NAME_2))

		# species' records
		load('./Data/Asclepias 03 Specimen Records and Environmental Data.rda')
		speciesList <- asclepias$meta$usableSpecies$species
	
	
	### BIOCLIM rasters
	
		mask <- raster('./Regions/mask_northAmerica.tif')

		# BIOCLIMs
		clim <- stack(paste0('E:/Ecology/Climate/WorldClim/version_2_1/10_arcmin/historical/wc2.1_10m_bio_', c(1, 12), '.tif'))
		names(clim) <- paste0('bio', c(1, 12))
		clim <- crop(clim, nam0Sp)
		clim <- clim * mask
		
		# fill NA cells near coasts to account for fact that some records may not fall in a cell near a coast
		for (i in 1:nlayers(clim)) {
			clim[[i]] <- focal(clim[[i]], w=matrix(1, nrow=3, ncol=3), fun=mean, na.rm=TRUE, NAonly=TRUE)
		}
		names(clim) <- paste0('bio', c(1, 12))

	### calculate niche breadth
	remember <- data.frame()
	for (countSpecies in seq_along(speciesList)) {
	
		species <- speciesList[countSpecies]
		say(species)
	
		spp <- tolower(gsub(species, pattern=' ', replacement='_'))
			
		### accurate records
	
			# niche center and breadth in BIO 1 and 12
			accsEnv <- asclepias$bySpecies[[spp]]$accurateUsableNoDupEnv[ , bios]
			accsEnv <- accsEnv[complete.cases(accsEnv), , drop=FALSE]
			accEnvCenterBio <- apply(accsEnv, 2, mean)
			
			accNicheLower <- apply(accsEnv, 2, quantile, lowerQuant)
			accNicheUpper <- apply(accsEnv, 2, quantile, upperQuant)
			
			accNicheBreathBio <- accNicheUpper - accNicheLower

		### imprecise records (points with buffers, counties, states)
		#############################################################

			## closest

				inaccAdminEnv <- asclepias$bySpecies[[countSpecies]]$inaccurateAdminUsableNoDupEnv

				selected <- data.frame()
				for (i in seq_along(inaccAdminEnv)) {
				
					thisEnv <- inaccAdminEnv[[i]][ , bios, drop=FALSE]
					dists <-  abs(thisEnv - accEnvCenterBio)
					
					thisSelected <- data.frame(
						bio1 = thisEnv[which.min(dists[ , 'bio1']), 'bio1'],
						bio12 = thisEnv[which.min(dists[ , 'bio12']), 'bio12']
					)
					
					selected <- rbind(selected, thisSelected)
					
				}
				
				accInaccAdminBio <- rbind(accsEnv, selected)
				accInaccAdminNicheLower <- apply(accInaccAdminBio, 2, quantile, lowerQuant)
				accInaccAdminNicheUpper <- apply(accInaccAdminBio, 2, quantile, upperQuant)
				accInaccAdminNicheBreadth_closest <- accInaccAdminNicheUpper - accInaccAdminNicheLower
				
			## farthest

				inaccAdminEnv <- asclepias$bySpecies[[countSpecies]]$inaccurateAdminUsableNoDupEnv

				selected <- data.frame()
				for (i in seq_along(inaccAdminEnv)) {
				
					thisEnv <- inaccAdminEnv[[i]][ , bios, drop=FALSE]
					dists <-  abs(thisEnv - accEnvCenterBio)
					
					thisSelected <- data.frame(
						bio1 = thisEnv[which.max(dists[ , 'bio1']), 'bio1'],
						bio12 = thisEnv[which.max(dists[ , 'bio12']), 'bio12']
					)
					
					selected <- rbind(selected, thisSelected)
					
				}
				
				accInaccAdminBio <- rbind(accsEnv, selected)
				accInaccAdminNicheLower <- apply(accInaccAdminBio, 2, quantile, lowerQuant)
				accInaccAdminNicheUpper <- apply(accInaccAdminBio, 2, quantile, upperQuant)
				accInaccAdminNicheBreadth_farthest <- accInaccAdminNicheUpper - accInaccAdminNicheLower
				
			## mean

				inaccAdminEnv <- asclepias$bySpecies[[countSpecies]]$inaccurateAdminUsableNoDupEnv

				selected <- data.frame()
				for (i in seq_along(inaccAdminEnv)) {
				
					thisEnv <- inaccAdminEnv[[i]][ , c(bios, 'weight'), drop=FALSE]
					
					thisEnv <- thisEnv[complete.cases(thisEnv), , drop=FALSE]
					thisEnv[ , 'weight'] <- thisEnv[ , 'weight'] / sum(thisEnv[ , 'weight'])
					
					thisSelected <- data.frame(
						bio1 = sum(thisEnv[ , 'bio1'] * thisEnv[ , 'weight']),
						bio12 = sum(thisEnv[ , 'bio12'] * thisEnv[ , 'weight'])
					)
					
					selected <- rbind(selected, thisSelected)
					
				}
				
				accInaccAdminBio <- rbind(accsEnv, selected)
				accInaccAdminNicheLower <- apply(accInaccAdminBio, 2, quantile, lowerQuant)
				accInaccAdminNicheUpper <- apply(accInaccAdminBio, 2, quantile, upperQuant)
				accInaccAdminNicheBreadth_mean <- accInaccAdminNicheUpper - accInaccAdminNicheLower
				
			## centroid

				# inaccurate records (points with buffers)
				index <- which(asclepias$bySpecies[[countSpecies]]$inaccurateAdminUsableNoDupRecs$recordType %in% inaccAssigns)
				if (length(index) > 0) {

					inaccs <- asclepias$bySpecies[[countSpecies]]$inaccurateAdminUsableNoDupRecs[index, ]
					inaccsSp <- SpatialPointsDataFrame(inaccs[ , llGbif], data=inaccs, proj4string=getCRS('wgs84', TRUE))
					inaccsSpEa <- sp::spTransform(inaccsSp, getCRS('albersNA', TRUE))
					inaccsSpEa <- gBuffer(inaccsSpEa, width=inaccsSpEa$maxCoordUncerPrecision_m, byid=TRUE, quadsegs=12)
					inaccsSpEa <- as(inaccsSpEa, 'SpatialPolygons')
					projection(inaccsSpEa) <- getCRS('albersNA')
					
					inaccsAdminSpEa <- inaccsSpEa
					
				}

				# county records
				index <- which(asclepias$bySpecies[[countSpecies]]$inaccurateAdminUsableNoDupRecs$recordType %in% countyAssigns)
				if (length(index) > 0) {

					county <- asclepias$bySpecies[[countSpecies]]$inaccurateAdminUsableNoDupRecs[index, ]
					stateCountyRecs <- tolower(paste0(county$stateFromGeog, county$countyFromGeog))
					countySpEa <- nam2SpEa[which(stateCountyGeog %in% stateCountyRecs), ]
					countySpEa <- as(countySpEa, 'SpatialPolygons')
					
					inaccsAdminSpEa <- if (exists('inaccsAdminSpEa')) {
						rbind(inaccsAdminSpEa, countySpEa, makeUniqueIDs=TRUE)
					} else {
						countySpEa
					}
					
				}
				
				# state records
				index <- which(asclepias$bySpecies[[countSpecies]]$inaccurateAdminUsableNoDupRecs$recordType %in% stateAssigns)
				if (length(index) > 0) {

					state <- asclepias$bySpecies[[countSpecies]]$inaccurateAdminUsableNoDupRecs[index, ]
					stateRecs <- tolower(state$stateFromGeog)
					stateSpEa <- nam1SpEa[which(stateGeog %in% stateRecs), ]
					
					inaccsAdminSpEa <- if (exists('inaccsAdminSpEa')) {
						rbind(inaccsAdminSpEa, stateSpEa, makeUniqueIDs = TRUE)
					} else {
						stateSpEa
					}
					
				}

				inaccsAdminCentsSpEa <- gCentroid(inaccsAdminSpEa, byid=TRUE)
				inaccsAdminCentsSpUnproj <- sp::spTransform(inaccsAdminCentsSpEa, getCRS('wgs84'))
				inaccsAdminCentroidBios <- raster::extract(clim, inaccsAdminCentsSpUnproj)

				accInaccAdminBio <- rbind(accsEnv, inaccsAdminCentroidBios)
				
				accInaccAdminBio <- accInaccAdminBio[complete.cases(accInaccAdminBio), , drop=FALSE]

				accInaccAdminNicheLower <- apply(accInaccAdminBio, 2, quantile, lowerQuant)
				accInaccAdminNicheUpper <- apply(accInaccAdminBio, 2, quantile, upperQuant)
				accInaccAdminNicheBreadth_centroid <- accInaccAdminNicheUpper - accInaccAdminNicheLower

		### remember

			remember <- rbind(
				remember,
				data.frame(
					species = species,
					numAccs = nrow(accsEnv),
					numInaccsAdmin = length(inaccAdminEnv),
					numTotal = nrow(accsEnv) + length(inaccAdminEnv),
					
					nicheBreathBio1_acc = accNicheBreathBio['bio1'],
					nicheBreathBio1_closest = accInaccAdminNicheBreadth_closest['bio1'],
					nicheBreadthBio1_farthest = accInaccAdminNicheBreadth_farthest['bio1'],
					nicheBreadthBio1_mean = accInaccAdminNicheBreadth_mean['bio1'],
					nicheBreadthBio1_centroid = accInaccAdminNicheBreadth_centroid['bio1'],
					
					nicheBreathBio12_acc = accNicheBreathBio['bio12'],
					nicheBreathBio12_closest = accInaccAdminNicheBreadth_closest['bio12'],
					nicheBreadthBio12_farthest = accInaccAdminNicheBreadth_farthest['bio12'],
					nicheBreadthBio12_mean = accInaccAdminNicheBreadth_mean['bio12'],
					nicheBreadthBio12_centroid = accInaccAdminNicheBreadth_centroid['bio12']
					
				)
			)

	} # next species

	dirCreate('./Analysis/Univariate Niche Breadth')
	write.csv(remember, './Analysis/Univariate Niche Breadth/!Univariate Niche Breadth.csv', row.names=FALSE)

	# # # exploratory plots
	# # par(mfrow=c(2, 2))
	# # lims <- c(0, max(remember[ , c('nicheBreathBio1_acc', 'nicheBreathBio1_closest', 'nicheBreadthBio1_farthest', 'nicheBreadthBio1_mean', 'nicheBreadthBio1_centroid')]))
	
	# # plot(remember$nicheBreathBio1_acc, remember$nicheBreathBio1_closest, xlim=lims, ylim=lims, pty='s'); abline(0, 1)
	# # plot(remember$nicheBreathBio1_acc, remember$nicheBreadthBio1_farthest, xlim=lims, ylim=lims, pty='s'); abline(0, 1)
	# # plot(remember$nicheBreathBio1_acc, remember$nicheBreadthBio1_mean, xlim=lims, ylim=lims, pty='s'); abline(0, 1)
	# # plot(remember$nicheBreathBio1_acc, remember$nicheBreadthBio1_centroid, xlim=lims, ylim=lims, pty='s'); abline(0, 1)

	# # x11()
	# # par(mfrow=c(2, 2))
	# # lims <- c(0, max(remember[ , c('nicheBreathBio12_acc', 'nicheBreathBio12_closest', 'nicheBreadthBio12_farthest', 'nicheBreadthBio12_mean', 'nicheBreadthBio12_centroid')]))
	
	# # plot(remember$nicheBreathBio12_acc, remember$nicheBreathBio12_closest, xlim=lims, ylim=lims, pty='s'); abline(0, 1)
	# # plot(remember$nicheBreathBio12_acc, remember$nicheBreadthBio12_farthest, xlim=lims, ylim=lims, pty='s'); abline(0, 1)
	# # plot(remember$nicheBreathBio12_acc, remember$nicheBreadthBio12_mean, xlim=lims, ylim=lims, pty='s'); abline(0, 1)
	# # plot(remember$nicheBreathBio12_acc, remember$nicheBreadthBio12_centroid, xlim=lims, ylim=lims, pty='s'); abline(0, 1)

	# # say('bio1 ', sum(remember$nicheBreathBio1_closest > remember$nicheBreathBio1_acc))
	# # say('bio12 ', sum(remember$nicheBreathBio12_closest > remember$nicheBreathBio12_acc))
	
	# # x11()
	# # plot((remember$nicheBreathBio12_closest - remember$nicheBreathBio12_acc) / remember$nicheBreathBio12_acc)
	
	
say('DONE! ', date(), level=1, deco='%')
