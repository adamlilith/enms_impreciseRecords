### CALCULATE UNIVARIATE NICHE BREADTH
### David Henderson 2019 | Adam B. Smith 2020
### 
### source('E:/Ecology/Drive/Research/Vaguely Georeferenced Specimen Records/Code/07 Virtual Species - Real Species Templates.r')

### CONTENTS ###
### setup ###
### simulate species, model, and assess ###

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
	
	setwd(paste0(drive, '/Ecology/Drive/Research/Vaguely Georeferenced Specimen Records'))

	library(adehabitatHR)
	library(cowplot)
	library(dismo)
	library(ggplot2)
	library(mvtnorm)
	library(raster)
	library(rgeos)
	library(scales)
	library(terra)
	
	# custom (Adam Smith)
	library(enmSdm)
	library(omnibus)
	library(statisfactory)
	
	source('./Code/simAndModelSpecies.r')
	source('E:/Ecology/Drive/R/enmSdm/R/predictEnmSdm.r')
	source('E:/Ecology/Drive/R/enmSdm/R/predictMaxEnt.r')

	# designations for accurate, inaccurate, and county records
	accAssigns <- 'certain/precise'
	inaccAssigns <- c('uncertain/precise', 'certain/imprecise', 'uncertain/imprecise')
	adminAssigns <- c('county/precise', 'county/imprecise', 'county-only', 'state/imprecise', 'state-only')
	countyAssigns <- c('county/precise', 'county/imprecise', 'county-only')
	stateAssigns <- c('state/imprecise', 'state-only')


say('###########################################')
say('### simulate species, model, and assess ###')
say('###########################################')

	### user-defined
	################
	
		repsStart <- 1 # number of times to replicate each species
		repsEnd <- 10 # number of times to replicate each species
		
		# regMult <- 1
		regMult <- c(seq(0.5, 3, by=0.5), 4, 5)
		
		nicheShape <- 1 # niche variance shape parameter
		nicheRate <- 10 # niche variance rate parameter
		
		minViableSuit <- 0.05 # cell must have suitability density >= this value to be retained in "true" niche raster
		
		scratchDir <- paste0(drive, '/Ecology/!Scratch')

	### data
	########
		
		say('Load data...')
		
		# PCA
		load('./Analysis/PCA on North American Climate.rda')

		# state/county environmental data
		load('./Data/Environment for North American Counties at Resolution 10 arcmin for Present.rda')
		# load('./Data/Environment for North American States & Provinces at Resolution 10 arcmin for Present.rda')

		mask <- raster('./Regions/mask_northAmerica.tif')

		# North American spatial polygons
		load('./Regions/GADM Ver 3pt6 North America Level 0 WGS84.rda')
		load('./Regions/GADM Ver 3pt6 North America Level 1 WGS84.rda')
		load('./Regions/GADM Ver 3pt6 North America Level 2 WGS84.rda')
			
		nam2Sp <- nam2Sp[nam2Sp$NAME_1 != 'Hawaii', ]
		nam2SpEa <- sp::spTransform(nam2Sp, getCRS('albersNA', TRUE))
		nam2SpEa$nam2area_km2 <- gArea(nam2SpEa, byid=TRUE) / 1000^2
		nam2Sp$nam2area_km2 <- nam2SpEa$nam2area_km2
		maxArea_km2 <- nam2SpEa$nam2area_km2[nam2SpEa$NAME_1 == 'California' & nam2SpEa$NAME_2 == 'San Bernardino']
	
		# species
		load('./Data/Asclepias 02 Specimen Records with Duplicates Flagged.rda')
		load('./Data/Asclepias 03 Specimen Records and Environmental Data.rda')

		records$designatedType <- NA
		records$designatedType[records$recordType %in% accAssigns] <- 'precise'
		records$designatedType[records$recordType %in% inaccAssigns] <- 'inaccurate'
		records$designatedType[records$recordType %in% adminAssigns] <- 'admin'
		
		speciesMeta <- asclepias$meta$usableSpecies
		speciesMeta$numDuplicateAdmins <- 0
		for (species in speciesMeta$species) {
			speciesMeta$numDuplicateAdmins[speciesMeta$species == species] <- sum(records$designatedType[records$species == species] == 'admin', na.rm=TRUE)
		}
		
		# contemporary climate rasters
		sqRasts <- stack(paste0('E:/Ecology/Climate/WorldClim/version_2_1/10_arcmin/historical/wc2.1_10m_bio_', 1:19, '.tif'))
		sqRasts <- crop(sqRasts, nam0Sp)
		sqRasts <- sqRasts * mask
		names(sqRasts) <- paste0('bio', 1:19)

		# future climate rasters
		futRasts <- stack('E:/Ecology/Climate/WorldClim/version_2_1/10_arcmin/cmip6_2041_2060_ssp245_BCC-CSM2-MR/wc2.1_10m_bioc_BCC-CSM2-MR_ssp245_2041-2060.tif')
		futRasts <- crop(futRasts, nam0Sp)
		futRasts <- futRasts * mask
		names(futRasts) <- paste0('bio', 1:19)
		
		rm(nam0Sp, records); gc()

	### apply PCA to climate rasters
	################################
	
		say('PCA rasters...')
	
		### contemporary rasters
		
			# PCA on climate (only)
			climDf <- as.data.frame(sqRasts)
			nonNas <- which(complete.cases(climDf))
			climDf <- climDf[nonNas, ]
			
			pcPredictionNoNas <- predictEnmSdm(pca, climDf)
			colnames(pcPredictionNoNas) <- paste0('pc', 1:19)

			# predict PCA back to rasters
			pcPrediction <- as.data.frame(sqRasts)
			pcPrediction[nonNas, ] <- pcPredictionNoNas
			
			sqPcaRasts <- sqRasts[[1:3]] * NA
			for (pc in 1:3) sqPcaRasts <- setValues(sqPcaRasts, values=pcPrediction[ , pc], layer=pc)
			names(sqPcaRasts) <- paste0('pc', 1:3)
			
			pcaDf <- as.data.frame(sqPcaRasts)
			pcaDfNoNA <- pcaDf[complete.cases(pcaDf), ]
			pcaDfNoNAMat <- as.matrix(pcaDfNoNA)
			
			conusMex <- nam1Sp[nam1Sp$NAME_0 != 'Canada' & nam1Sp$NAME_1 != 'Alaska' & nam1Sp$NAME_1 != 'Hawaii', ]
			conusMexMask <- crop(mask, conusMex)
			conusMexMask <- rasterize(conusMex, conusMexMask)
			conusMexMask <- conusMexMask * 0 + 1
			sqPcaRastsConusMex <- sqPcaRasts * conusMexMask
			areaConusMexMask_km2 <- area(sqPcaRastsConusMex)
			areaConusMexMask_km2 <- as.data.frame(areaConusMexMask_km2)
			names(areaConusMexMask_km2) <- 'area_km2'
			
			sqPcaDfConusMex <- as.data.frame(sqPcaRastsConusMex)
			names(sqPcaDfConusMex) <- paste0('PC', 1:3)
			sqPcaDfConusMex <- cbind(sqPcaDfConusMex, areaConusMexMask_km2)
			sqPcaDfConusMexNoNA <- sqPcaDfConusMex[complete.cases(sqPcaDfConusMex), ]
			
			rm(sqRasts, conusMexMask, sqPcaDfConusMex, areaConusMexMask_km2); gc()

		### future rasters
				
			# PCA on climate (only)
			climDf <- as.data.frame(futRasts)
			nonNas <- which(complete.cases(climDf))
			climDf <- climDf[nonNas, ]
			
			pcPredictionNoNas <- predictEnmSdm(pca, climDf)
			colnames(pcPredictionNoNas) <- paste0('pc', 1:19)

			# predict PCA back to rasters
			pcPrediction <- as.data.frame(futRasts)
			pcPrediction[nonNas, ] <- pcPredictionNoNas
			
			futPcaRasts <- futRasts[[1:3]] * NA
			for (pc in 1:3) futPcaRasts <- setValues(futPcaRasts, values=pcPrediction[ , pc], layer=pc)
			names(futPcaRasts) <- paste0('pc', 1:3)
			
			rm(futRasts); gc()
			
	### area raster
	###############

		areaRast_km2 <- raster::area(sqPcaRasts[[1]])
		areaRast_km2 <- areaRast_km2 * mask
		areaVect_km2 <- as.vector(areaRast_km2)
		areaVect_km2 <- areaVect_km2[!is.na(areaVect_km2)]
	
	### collate radii of inaccurate records
	#######################################
	
		# used as a population from which to draw radii for any given species
		allInaccRadii_m <- numeric()
		for (countSpp in seq_along(asclepias$bySpecies)) {
			
			whichInaccs <- asclepias$bySpecies[[countSpp]]$inaccurateAdminUsableNoDupRecs$recordType %in% inaccAssigns
			
			if (any(whichInaccs)) {
				radii_m <- asclepias$bySpecies[[countSpp]]$inaccurateAdminUsableNoDupRecs$maxCoordUncerPrecision_m[whichInaccs]
				allInaccRadii_m <- c(allInaccRadii_m, radii_m)
			}
			
		}

	### for each real species
	#########################

		remember <- if (file.exists(paste0('./Analysis/Virtual Species/!Virtual Species Results ', repsStart, '-', repsEnd, '.csv'))) {
			read.csv(paste0('./Analysis/Virtual Species/!Virtual Species Results ', repsStart, '-', repsEnd, '.csv'))
		} else {
			data.frame()
		}

		numSpecies <- nrow(asclepias$meta$usableSpecies)

		if (nrow(remember) > 0) {
			numSpeciesDoes <- max(remember$species)
			if (numSpeciesDoes == numSpecies) {
				say('DONE!')
				stop('We are done!')
			} else {
				startSpecies <- numSpeciesDoes + 1
			}
		} else {
			startSpecies <- 1
		}

		for (countSpecies in startSpecies:numSpecies) {

			numPrecise <- speciesMeta$numUniqueUsableAccurateRecs[countSpecies]
			numInacc <- speciesMeta$numUniqueUsableInaccurateRecs[countSpecies]
			numAdmin <- speciesMeta$numUniqueUsableAdminRecs[countSpecies]
			numDuplicateAdmins <- speciesMeta$numDuplicateAdmins[countSpecies]

			numRecords <- numPrecise + numInacc + numAdmin
			

			rep <- repsStart
			while (rep <= repsEnd) {
			
				# simulate and model species
				thisRemember <- tryCatch(simAndModelSpecies(), error=function(cond) FALSE)
				
				if (!is(thisRemember, 'logical')) {
					remember <- rbind(remember, thisRemember)
					rep <- rep + 1
				}
				say('')
			
				say('')
				say('Mean correlation between truth and errorless ......... ', round(mean(remember$corTruthVsErrorless, na.rm=TRUE), 2))
				say('Mean correlation between truth and precise only ...... ', round(mean(remember$corTruthVsPrecise, na.rm=TRUE), 2))
				say('Mean correlation between truth and precise + vague ... ', round(mean(remember$corTruthVsVague, na.rm=TRUE), 2))

			} # next repeat of species
					
			dirCreate('./Analysis/Virtual Species')
			write.csv(remember, paste0('./Analysis/Virtual Species/!Virtual Species Results ', repsStart, '-', repsEnd, '.csv'), row.names=FALSE)

		} # next species

# say('############')
# say('### plot ###')
# say('############')

	# files <- listFiles('./Analysis/Virtual Species', pattern='.csv')
	# results <- read.csv(files[1])
	# if (length(files) > 1) {
		# for (file in files[2:length(files)]) {
			# results <- merge(
				# results,
				# read.csv(file),
				# all=TRUE
			# )
		# }
	# }

	# results$numVague <- results$numInacc + results$numAdmin

	# # results <- results[order(results$numVague), ]
	# # results <- results[order(results$numPrecise), ]
	
	# # boxplots: 
	# species <- unique(results$species)
	# reshape <- data.frame()
	# for (i in species) {
	
		# n <- sum(results$species == i)
	
		# reshape <- rbind(
			# reshape,
			# data.frame(
				# species = i,
				# type = c(rep('vsErrorless', n), rep('vsPrecise', n), rep('vsVague', n)),
				# delta = c(
					# results$corTruthVsErrorless[results$species==i],
					# results$corTruthVsPrecise[results$species==i],
					# results$corTruthVsVague[results$species==i]
				# )
			# )
		# )
	# }
	
	# reshape$species <- as.factor(reshape$species)
	
	# p <- ggplot(reshape, aes(x=species, y=delta, fill=type)) +
		# geom_boxplot()
		
	# print(p)
	
	# # # compare slopes
	# species <- unique(results$species)
	# reshape <- data.frame()
	# for (i in species) {
	
		# data <- data.frame(
			# corTruthVsErrorless = results$corTruthVsErrorless[results$species == i],
			# corTruthVsPrecise = results$corTruthVsPrecise[results$species == i],
			# corTruthVsVague = results$corTruthVsVague[results$species == i]
		# )
		
		# data <- data[complete.cases(data), ]
		
		# m1 <- lm(logitAdj(0.5 * (corTruthVsPrecise + 1), 0) ~ logitAdj(0.5 * (corTruthVsErrorless + 1)), data=data)
		# m2 <- lm(logitAdj(0.5 * (corTruthVsVague + 1), 0) ~ logitAdj(0.5 * (corTruthVsErrorless + 1)), data=data)
	
		# reshape <- rbind(
			# reshape,
			# data.frame(
				# species = i,
				# numRecords = results$numRecords[results$species == i][1],
				# numPrecise = results$numPrecise[results$species == i][1],
				# numVague = results$numInacc[results$species == i][1] + results$numAdmin[results$species == i][1],
				# slopeErrorlessVsPrecise = coefficients(m1)[2],
				# slopeErrorlessVsVague = coefficients(m2)[2],
				# r2ErrorlessVsPrecise = summary(m1)$adj.r.squared,
				# r2ErrorlessVsVague = summary(m2)$adj.r.squared
			# )
		# )
	# }
	
	# rownames(reshape) <- NULL
	
	# reshape$species <- as.factor(reshape$species)
	
	# lims <- range(pretty(c(reshape$slopeErrorlessVsPrecise, reshape$slopeErrorlessVsVague)))
	# slopes <- ggplot(reshape, aes(x=slopeErrorlessVsPrecise, y=slopeErrorlessVsVague, fill=numPrecise)) +
		# geom_point() +
		# geom_abline(slope = 1, intercept = 0) +
		# xlim(lims[1], lims[2]) + ylim(lims[1], lims[2]) +
		# coord_fixed()
	
	# print(slopes)
	
	
	# lims <- range(pretty(c(reshape$r2ErrorlessVsPrecise, reshape$r2ErrorlessVsVague)))
	# r2 <- ggplot(reshape, aes(x=r2ErrorlessVsPrecise, y=r2ErrorlessVsVague, fill=numPrecise)) +
		# geom_point() +
		# geom_abline(slope = 1, intercept = 0) +
		# xlim(lims[1], lims[2]) + ylim(lims[1], lims[2]) +
		# coord_fixed()
	
	# print(r2)
	
		
say('DONE!', deco='&', level=1)

