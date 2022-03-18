### EFFECT OF INCLUDING IMPRECISE SPECIMENS ON CLIMATE CHANGE EXPOSURE - VIRTUAL SPECIES
### Adam B. Smith | Missouri Botanical Garden | adam.smith@mobot.org | 2021-10
### 
### source('C:/Ecology/Drive/Research Active/Vaguely Georeferenced Specimen Records/enms_impreciseRecords/09 Example of Virtual Species Simulations.r')
### source('E:/Ecology/Drive/Research Active/Vaguely Georeferenced Specimen Records/enms_impreciseRecords/09 Example of Virtual Species Simulations.r')

### CONTENTS ###
### setup ###
### generate example of simulated species ###

#############
### setup ###
#############

	cat(date(), '\n'); flush.console()
	memory.limit(memory.limit() * 2^29)
	# rm(list=ls())
	gc()
	options(stringsAsFactors=FALSE)
	
	# drive <- 'C:'
	# drive <- 'D:'
	drive <- 'E:'
	
	setwd(paste0(drive, '/Ecology/Drive/Research Active/Vaguely Georeferenced Specimen Records'))

	library(adehabitatHR)
	library(cowplot)
	library(dismo)
	library(geometry)
	library(ggplot2)
	library(mvtnorm)
	library(patchwork)
	library(rgeos)
	library(scales)
	library(terra)
	
	# custom (Adam Smith)
	library(enmSdm)
	library(omnibus)
	library(statisfactory)

# say('############################')
# say('### map of US and Mexico ###')
# say('############################')

	# # PCA
	# load('./Analysis/PCA on North American Climate.rda')

	# mask <- raster('./Regions/mask_northAmerica.tif')
	# load('./Regions/GADM Ver 3pt6 North America Level 1 WGS84.rda')
	# load('./Regions/GADM Ver 3pt6 North America Level 2 WGS84.rda')

	# # contemporary climate rasters
	# sqRasts <- stack(paste0('./Data/WorldClim/worldclim_2.1_10arcmin_historical/wc2.1_10m_bio_', 1:19, '.tif'))

	# ### contemporary rasters

	# sqRasts <- sqRasts * mask
	# names(sqRasts) <- paste0('bio', 1:19)

	# # PCA on climate (only)
	# climDf <- as.data.frame(sqRasts)
	# nonNas <- which(complete.cases(climDf))
	# climDf <- climDf[nonNas, ]
	
	# pcPredictionNoNas <- predictEnmSdm(pca, climDf)
	# colnames(pcPredictionNoNas) <- paste0('pc', 1:19)

	# # predict PCA back to rasters
	# pcPrediction <- as.data.frame(sqRasts)
	# pcPrediction[nonNas, ] <- pcPredictionNoNas
	
	# sqPcaRasts <- sqRasts[[1:3]] * NA
	# for (pc in 1:3) sqPcaRasts <- setValues(sqPcaRasts, values=pcPrediction[ , pc], layer=pc)
	# names(sqPcaRasts) <- paste0('pc', 1:3)
	
	# sqPcaDf <- as.data.frame(sqPcaRasts)
	# sqPcaDfNoNA <- sqPcaDf[complete.cases(sqPcaDf), ]
	# sqPcaDfNoNAMat <- as.matrix(sqPcaDfNoNA)

	# # get climate for just Mexico and USA for selecting niche mean value
	# conusMexSp <- nam1Sp[nam1Sp$NAME_0 != 'Canada' & nam1Sp$NAME_1 != 'Alaska' & nam1Sp$NAME_1 != 'Hawaii', ]
	# conusMexMask <- crop(mask, conusMexSp)
	# conusMexMask <- rasterize(conusMexSp, conusMexMask)
	# conusMexMask <- conusMexMask * 0 + 1
	# sqPcaRastsConusMex <- sqPcaRasts * conusMexMask

	# conusMexSp <- vect(conusMexSp)

	# conusMexSpEa <- project(conusMexSp, getCRS('albersNA'))
	# conusMexBuffSpEa <- buffer(conusMexSpEa, width=200000)
	# conusMexBuffSp <- project(conusMexBuffSpEa, getCRS('wgs84'))

	# # plot US & Mexico
	# png('./Analysis/Examples/Example - US & Mexico.png', res=600, width=1600, height=1200)

		# par(mfrow=c(1, 1), oma=c(1, 1, 1, 1), mar=c(0, 0, 0, 0))
		# plot(conusMexBuffSp, border=NA)
		# plot(conusMexMask, col='gray30', add=TRUE, legend=FALSE)
	
	# dev.off()
	
	# ### plot PCs
	# cols <- hcl.colors(100)
	# cols <- rainbow(100, start=5)
	
	# png('./Analysis/Examples/Example - PC1.png', res=600, width=1600, height=1200)

		# par(mfrow=c(1, 1), oma=c(1, 1, 1, 1), mar=c(0, 0, 0, 0))
		# plot(conusMexBuffSp, border=NA)
		# plot(sqPcaRastsConusMex[[1]], col=cols, legend=FALSE, add=TRUE)
	
	# dev.off()
	

	# png('./Analysis/Examples/Example - PC2.png', res=600, width=1600, height=1200)

		# par(mfrow=c(1, 1), oma=c(1, 1, 1, 1), mar=c(0, 0, 0, 0))
		# plot(conusMexBuffSp, border=NA)
		# plot(sqPcaRastsConusMex[[2]], col=cols, legend=FALSE, add=TRUE)
	
	# dev.off()
	

	# png('./Analysis/Examples/Example - PC3.png', res=600, width=1600, height=1200)

		# par(mfrow=c(1, 1), oma=c(1, 1, 1, 1), mar=c(0, 0, 0, 0))
		# plot(conusMexBuffSp, border=NA)
		# plot(sqPcaRastsConusMex[[3]], col=cols, legend=FALSE, add=TRUE)
	
	# dev.off()
	
say('#############################################')
say('### generate example of simulated species ###')
say('#############################################')

	### user-defined
	################
	
		set.seed(20)
		# set.seed(18)
		# set.seed(111111112)
	
		numErrorless <- 80
		numPrecise <- 10
		numImprecise <- 30

		buffSize_km <- 300 # around sites for calibration region
		
		# maxent settings
		regMult <- c(seq(0.5, 3, by=0.5), 4, 5)
		maxEntClasses <- 'lpq'
		
		# bounds for runif() for gamma distribution rate parameter for MVN variance
		minNicheRate <- 7.5
		maxNicheRate <- 30
		
	# ### data
	# ########
		
		# say('Load data...')
		
		# # PCA
		# load('./Analysis/PCA on North American Climate.rda')

		# # state/county environmental data
		# load('./Data/Environment for North American Counties at Resolution 10 arcmin for Present.rda')
		# mask <- raster('./Regions/mask_northAmerica.tif')

		# # North American spatial polygons
		# load('./Regions/GADM Ver 3pt6 North America Level 1 WGS84.rda')
		# load('./Regions/GADM Ver 3pt6 North America Level 2 WGS84.rda')
		# load('./Regions/GADM Ver 3pt6 North America Level 2 Albers.rda')
			
		# nam2Sp <- nam2Sp[nam2Sp$NAME_1 != 'Hawaii', ]
		# nam2SpEa <- nam2SpEa[nam2SpEa$NAME_1 != 'Hawaii', ]
		# nam2SpEa$nam2area_km2 <- gArea(nam2SpEa, byid=TRUE) / 1000^2
		# nam2Sp$nam2area_km2 <- nam2SpEa$nam2area_km2
		# maxArea_km2 <- nam2SpEa$nam2area_km2[nam2SpEa$NAME_1 == 'California' & nam2SpEa$NAME_2 == 'San Bernardino']
	
		# # contemporary climate rasters
		# sqRasts <- stack(paste0('./Data/WorldClim/worldclim_2.1_10arcmin_historical/wc2.1_10m_bio_', 1:19, '.tif'))
		
		# # future climate rasters
		# futScenario <- paste0('ensemble of ', paste(c('BCC-CSM2-MR', 'CNRM-ESM2-1', 'CanESM5', 'IPSL-CM6A-LR', 'MIROC-ES2L'), collapse=', '))
		# futRasts <- stack('./Data/WorldClim/cmip6_ensemble_ssp585_2061-2080/bioclim.tif')
		
		# rm(nam2SpEa); gc()

	# ### apply PCA to climate rasters
	# ################################
	
		# say('PCA rasters...')
	
		# ### contemporary rasters
		
			# sqRasts <- sqRasts * mask
			# names(sqRasts) <- paste0('bio', 1:19)

			# # PCA on climate (only)
			# climDf <- as.data.frame(sqRasts)
			# nonNas <- which(complete.cases(climDf))
			# climDf <- climDf[nonNas, ]
			
			# pcPredictionNoNas <- predictEnmSdm(pca, climDf)
			# colnames(pcPredictionNoNas) <- paste0('pc', 1:19)

			# # predict PCA back to rasters
			# pcPrediction <- as.data.frame(sqRasts)
			# pcPrediction[nonNas, ] <- pcPredictionNoNas
			
			# sqPcaRasts <- sqRasts[[1:3]] * NA
			# for (pc in 1:3) sqPcaRasts <- setValues(sqPcaRasts, values=pcPrediction[ , pc], layer=pc)
			# names(sqPcaRasts) <- paste0('pc', 1:3)
			
			# sqPcaDf <- as.data.frame(sqPcaRasts)
			# sqPcaDfNoNA <- sqPcaDf[complete.cases(sqPcaDf), ]
			# sqPcaDfNoNAMat <- as.matrix(sqPcaDfNoNA)

			# # get climate for just Mexico and USA for selecting niche mean value
			# conusMexSp <- nam1Sp[nam1Sp$NAME_0 != 'Canada' & nam1Sp$NAME_1 != 'Alaska' & nam1Sp$NAME_1 != 'Hawaii', ]
			# conusMexMask <- crop(mask, conusMexSp)
			# conusMexMask <- rasterize(conusMexSp, conusMexMask)
			# conusMexMask <- conusMexMask * 0 + 1
			# sqPcaRastsConusMex <- sqPcaRasts * conusMexMask
			# areaConusMexMask_km2 <- area(sqPcaRastsConusMex)
			# areaConusMexMask_km2 <- as.data.frame(areaConusMexMask_km2)
			# names(areaConusMexMask_km2) <- 'area_km2'
			
			# sqPcaDfConusMex <- as.data.frame(sqPcaRastsConusMex)
			# names(sqPcaDfConusMex) <- paste0('PC', 1:3)
			# sqPcaDfConusMex <- cbind(sqPcaDfConusMex, areaConusMexMask_km2)
			# sqPcaDfConusMexNoNA <- sqPcaDfConusMex[complete.cases(sqPcaDfConusMex), ]
			
			# rm(climDf, conusMexSp, conusMexMask, sqPcaDfConusMex, areaConusMexMask_km2, sqPcaDfNoNA); gc()

		# ### future rasters
				
			# futRasts <- futRasts * mask
			# names(futRasts) <- paste0('bio', 1:19)

			# # PCA on climate (only)
			# climDf <- as.data.frame(futRasts)
			# nonNas <- which(complete.cases(climDf))
			# climDf <- climDf[nonNas, ]
			
			# pcPredictionNoNas <- predictEnmSdm(pca, climDf)
			# colnames(pcPredictionNoNas) <- paste0('pc', 1:19)
			
			# # predict PCA back to rasters
			# pcPrediction <- as.data.frame(futRasts)
			# pcPrediction[nonNas, ] <- pcPredictionNoNas
			
			# futPcaRasts <- futRasts[[1:3]] * NA
			# for (pc in 1:3) futPcaRasts <- setValues(futPcaRasts, values=pcPrediction[ , pc], layer=pc)
			# names(futPcaRasts) <- paste0('pc', 1:3)
			
			# futPcaDf <- as.data.frame(futPcaRasts)
			# futPcaDfNoNA <- futPcaDf[complete.cases(futPcaDf), ]
			# futPcaDfNoNAMat <- as.matrix(futPcaDfNoNA)

			# rm(futRasts, climDf, pcPrediction, futPcaDfNoNA); gc()
			
	# ### area raster
	# ###############

		# areaRast_km2 <- raster::area(sqPcaRasts[[1]])
		# areaRast_km2 <- areaRast_km2 * mask
		# areaVect_km2 <- as.vector(areaRast_km2)
		# areaVect_km2 <- areaVect_km2[!is.na(areaVect_km2)]

	### generate species
	####################

		say('make species', post=0)

		validRast <- FALSE

		count <- 1
		while (!validRast) {

			nicheShape <- 1 # niche variance shape parameter
			nicheRate1 <- runif(1, minNicheRate, maxNicheRate) # niche variance rate parameter
			nicheRate2 <- runif(1, minNicheRate, maxNicheRate) # niche variance rate parameter
			nicheRate3 <- runif(1, minNicheRate, maxNicheRate) # niche variance rate parameter
			
			# # modeling pre-niche with multivariate normal
			# nicheExtremes1 <- range(sqPcaDfConusMexNoNA$PC1)
			# nicheExtremes2 <- range(sqPcaDfConusMexNoNA$PC2)
			# nicheExtremes3 <- range(sqPcaDfConusMexNoNA$PC3)
			
			# nicheRange1 <- diff(nicheExtremes1)
			# nicheRange2 <- diff(nicheExtremes2)
			# nicheRange3 <- diff(nicheExtremes3)
			
			# nicheExtremes1[1] <- nicheExtremes1[1] - 0.1 * nicheRange1
			# nicheExtremes1[2] <- nicheExtremes1[2] + 0.1 * nicheRange1
			
			# nicheExtremes2[1] <- nicheExtremes2[1] - 0.1 * nicheRange2
			# nicheExtremes2[2] <- nicheExtremes2[2] + 0.1 * nicheRange2
			
			# nicheExtremes3[1] <- nicheExtremes3[1] - 0.1 * nicheRange3
			# nicheExtremes3[2] <- nicheExtremes3[2] + 0.1 * nicheRange3
			
			# nicheCenter <- c(
				# runif(1, nicheExtremes1[1], nicheExtremes1[2]),
				# runif(1, nicheExtremes2[1], nicheExtremes2[2]),
				# runif(1, nicheExtremes3[1], nicheExtremes3[2])
			# )
			
			nicheCenter <- c(NA, NA, NA)
			while (any(is.na(nicheCenter))) {
				nicheCenter <- unlist(sqPcaDfConusMexNoNA[sample(1:nrow(sqPcaDfConusMexNoNA), 1, prob=sqPcaDfConusMexNoNA$area_km2), , drop=TRUE])
			}
			
			nicheCenter <- nicheCenter[1:3]
			
			var1 <- rgamma(1, nicheShape, nicheRate1)
			var2 <- rgamma(1, nicheShape, nicheRate2)
			var3 <- rgamma(1, nicheShape, nicheRate3)

			corr1v2 <- rbeta(1, 5, 5) * 2 - 1
			corr1v3 <- rbeta(1, 5, 5) * 2 - 1
			corr2v3 <- rbeta(1, 5, 5) * 2 - 1
			
			corr <- matrix(
				c(var1, corr1v2, corr1v3,
				corr1v2, var2, corr2v3,
				corr1v3, corr2v3, var3),
				nrow=3, byrow=TRUE
			)
			
			densities <- dmvnorm(sqPcaDfNoNAMat, mean=nicheCenter, sigma=corr)
			maxSqDensities <- max(densities)
			densities <- densities / maxSqDensities
			
			densitiesSum <- sum(densities)
			validRast <- !is.na(densitiesSum) & densitiesSum > 0

			count <- count + 1

		} # keep trying to construct valid niche
		count
		
		## burn densities to present raster
			
		sqNicheRast <- mask * NA
		densitiesForRast <- rep(NA, ncell(sqNicheRast))
		nonNa <- which(complete.cases(sqPcaDf))
		densitiesForRast[nonNa] <- densities
		sqNicheRast <- setValues(sqNicheRast, values=densitiesForRast)
		
		# burn densities to future raster
		densities <- dmvnorm(futPcaDfNoNAMat, mean=nicheCenter, sigma=corr)
		densities <- densities / maxSqDensities

		futNicheRast <- mask * NA
		densitiesForRast <- rep(NA, ncell(futNicheRast))
		nonNa <- which(complete.cases(futPcaDf))
		densitiesForRast[nonNa] <- densities
		futNicheRast <- setValues(futNicheRast, values=densitiesForRast)
		
		names(sqNicheRast) <- names(futNicheRast) <- 'niche'
		
	### generate records
	####################
	
		say('| records', post=0)
	
		# errorless
		selectExt <- extent(-119.1299, -112.1632, 32.05502, 37.76023)
		selectRast <- crop(sqNicheRast, selectExt)
		errorlessRecs <- randomPoints(sqNicheRast, numErrorless, prob=TRUE)
		# errorlessRecs <- errorlessRecs[errorlessRecs[ , 'x'] < -112.597 & errorlessRecs[ , 'y'] < 37.76019, ]
		errorlessRecs <- errorlessRecs[1:numErrorless, ]
		preciseRecs <- errorlessRecs[1:numPrecise, , drop=FALSE]
		impreciseErrorlessRecs <- errorlessRecs[(numPrecise + 1):(numPrecise + numImprecise), , drop=FALSE]
	
		# administrative
		impreciseCounties <- extract(nam2Sp, impreciseErrorlessRecs)
		impreciseCounties$point.ID <- NULL
		impreciseCounties <- impreciseCounties[!duplicated(impreciseCounties), , drop=FALSE]
		
		say(nrow(impreciseCounties), ' non-duplicate admin(s)', post=0)
	
	### define calibration regions
	##############################
	
		say('| calib regions', post=0)
	
		# errorless
		errorlessRecsSp <- SpatialPoints(errorlessRecs[ , 1:2], proj4string=getCRS('wgs84', TRUE))
		errorlessRecsSpEa <- sp::spTransform(errorlessRecsSp, getCRS('albersNA', TRUE))

		errorlessRecsVectEa <- vect(errorlessRecsSpEa)
		errorlessMcpVectEa <- convHull(errorlessRecsVectEa)
		errorlessMcpSpEa <- as(errorlessMcpVectEa, 'Spatial')
		errorlessBuffSpEa <- gBuffer(errorlessMcpSpEa, width=buffSize_km * 1000)
		errorlessBuffSp <- sp::spTransform(errorlessBuffSpEa, getCRS('wgs84', TRUE))

		errorlessMcpBuffMask <- rasterize(errorlessBuffSp, sqPcaRasts)
		
		errorlessMaskedSqPcaRasts <- errorlessMcpBuffMask * sqPcaRasts
		names(errorlessMaskedSqPcaRasts) <- names(sqPcaRasts)
		
		errorlessMaskedFutPcaRasts <- errorlessMcpBuffMask * futPcaRasts
		names(errorlessMaskedFutPcaRasts) <- names(sqPcaRasts)
		
		# precise
		preciseRecsSp <- SpatialPoints(preciseRecs[ , 1:2, drop=FALSE], proj4string=getCRS('wgs84', TRUE))
		preciseRecsSpEa <- sp::spTransform(preciseRecsSp, getCRS('albersNA', TRUE))
		preciseRecVectEa <- vect(preciseRecsSpEa)
		preciseMcpVectEa <- convHull(preciseRecVectEa)
		preciseMcpSpEa <- if (numPrecise < 3) {
			SpatialPoints(geom(preciseMcpVectEa)[ , c('x', 'y'), drop=FALSE], getCRS('albersNA', TRUE))
		} else {
			as(preciseMcpVectEa, 'Spatial')
		}
		preciseBuffSpEa <- gBuffer(preciseMcpSpEa, width=buffSize_km * 1000)
		
		preciseBuffSp <- sp::spTransform(preciseBuffSpEa, getCRS('wgs84', TRUE))
		preciseMcpBuffMask <- rasterize(preciseBuffSp, sqPcaRasts)
		
		preciseMaskedSqPcaRasts <- preciseMcpBuffMask * sqPcaRasts
		names(preciseMaskedSqPcaRasts) <- names(sqPcaRasts)
		
		# precise + admin
		adminSp <- nam2Sp[nam2Sp$NAME_1 == impreciseCounties$NAME_1[1] & nam2Sp$NAME_2 == impreciseCounties$NAME_2[1], ]
		if (nrow(impreciseCounties) > 1) {
			for (i in 2:nrow(impreciseCounties)) {
				adminSp <- rbind(
					adminSp,
					nam2Sp[nam2Sp$NAME_1 == impreciseCounties$NAME_1[i] & nam2Sp$NAME_2 == impreciseCounties$NAME_2[i], ]
				)
			}
		}
		
		adminSpEa <- sp::spTransform(adminSp, getCRS('albersNA', TRUE))
		
		impreciseMcpSpEa <- mcpFromPolygons(adminSpEa, preciseRecsSpEa)
		impreciseBuffSpEa <- gBuffer(impreciseMcpSpEa, byid=FALSE, width=buffSize_km * 1000)
		impreciseBuffSp <- sp::spTransform(impreciseBuffSpEa, getCRS('wgs84', TRUE))
		
		impreciseMcpBuffMask <- rasterize(impreciseBuffSp, sqPcaRasts)
		
		impreciseMaskedSqPcaRasts <- impreciseMcpBuffMask * sqPcaRasts
		names(impreciseMaskedSqPcaRasts) <- names(sqPcaRasts)
		
	### plot
	########
	
		par(oma=c(0, 0, 0, 0), mfrow=c(1, 2))

		# present
		cols <- colorRampPalette(c('gray80', 'green', 'forestgreen'))
		cols <- cols(10)
		main <- paste0('SQ ', numErrorless, ' (', nrow(errorlessRecs), ') el ', numPrecise, ' pr')
		plot(trim(errorlessMcpBuffMask), col=NA, legend=FALSE, main=main)
		plot(sqNicheRast, col=cols, legend=FALSE, add=TRUE)
		plot(adminSp, border='orange4', add=TRUE)
		points(errorlessRecsSp, pch='.')
		points(preciseRecsSp, col='red', pch='.')
		
		plot(impreciseMcpSpEa, border='orange', add=TRUE)
		plot(errorlessBuffSp, add=TRUE)
		plot(preciseBuffSp, border='red', add=TRUE)
		plot(impreciseBuffSp, border='orange4', add=TRUE)

		# future delta
		main <- paste0('fut delta')
		plot(trim(errorlessMcpBuffMask), col=NA, legend=FALSE, main=main)
		delta <- sqNicheRast - futNicheRast
		plot(delta, legend=TRUE, add=TRUE)
		points(errorlessRecsSp, pch='.')
		points(preciseRecsSp, col='red', pch='.')
		
		plot(impreciseMcpSpEa, border='orange', add=TRUE)
		plot(errorlessBuffSp, add=TRUE)
		plot(preciseBuffSp, border='red', add=TRUE)
		plot(impreciseBuffSp, border='orange4', add=TRUE)

	### background sites
	#####################
		
		say('| bgs', post=0)
		
		# errorless
		bgInErrorlessBuffTrain <- randomPoints(errorlessMcpBuffMask, 10000)
		bgInErrorlessBuffEval <- randomPoints(errorlessMcpBuffMask, 10000)

		# precise
		bgInPreciseBuff <- if (numPrecise < 5) {
			sampleRast(preciseMcpBuffMask, cellStats(preciseMcpBuffMask, 'sum'), replace=FALSE, prob=FALSE)
		} else {
			randomPoints(preciseMcpBuffMask, 10000)
		}
		
		# precise + imprecise
		bgInImpreciseBuff <- randomPoints(impreciseMcpBuffMask, 10000)

	### environmental data
	######################
	
		say('| env data', post=0)
	
		### errorless: present
		
		# training occurrences and backgrounds
		errorlessRecsEnv <- extract(sqPcaRasts, errorlessRecs)
		errorlessTrainBgEnv <- extract(sqPcaRasts, bgInErrorlessBuffTrain)
		errorlessTrainBgEnv <- errorlessTrainBgEnv[complete.cases(errorlessTrainBgEnv), , drop=FALSE]
		
		errorlessData <- rbind(errorlessRecsEnv, errorlessTrainBgEnv)
		presBg <- c(rep(1, nrow(errorlessRecs)), rep(0, nrow(errorlessTrainBgEnv)))
		errorlessData <- cbind(presBg, errorlessData)
		errorlessData <- as.data.frame(errorlessData)

		# evaluation sites in buffered MCP around errorless sites
		errorlessEvalBgSqEnv <- extract(sqPcaRasts, bgInErrorlessBuffEval)
		ok <- complete.cases(errorlessEvalBgSqEnv)
		bgInErrorlessBuffEval <- bgInErrorlessBuffEval[ok, , drop=FALSE]
		errorlessEvalBgSqEnv <- errorlessEvalBgSqEnv[ok, , drop=FALSE]

		errorlessEvalBgFutEnv <- extract(futPcaRasts, bgInErrorlessBuffEval)
		ok <- complete.cases(errorlessEvalBgFutEnv)
		errorlessEvalBgFutEnv <- errorlessEvalBgFutEnv[ok, , drop=FALSE]

		### precise
		preciseRecsEnv <- extract(sqPcaRasts, preciseRecs)
		preciseBgEnv <- extract(sqPcaRasts, bgInPreciseBuff)
		preciseBgEnv <- preciseBgEnv[complete.cases(preciseBgEnv), , drop=FALSE]

		preciseData <- rbind(preciseRecsEnv, preciseBgEnv)
		presBg <- c(rep(1, nrow(preciseRecs)), rep(0, nrow(preciseBgEnv)))
		preciseData <- cbind(presBg, preciseData)
		preciseData <- as.data.frame(preciseData)

		### precise + imprecise
		adminEnvExt <- extract(sqPcaRasts, adminSp)

		preciseMeanEnv <- colMeans(preciseRecsEnv)
		adminEnv <- data.frame()
		for (i in 1:nrow(adminSp)) {
			thisCountyEnv <- if (!is(adminEnvExt[[i]], 'matrix')) {
				matrix(adminEnvExt[[i]], nrow=1)
			} else {
				adminEnvExt[[i]]
			}
			
			dists <- sqrt(
				rowSums(
					cbind(
						(preciseMeanEnv[1] - thisCountyEnv[ , 1])^2,
						(preciseMeanEnv[2] - thisCountyEnv[ , 2])^2,
						(preciseMeanEnv[3] - thisCountyEnv[ , 3])^2
					)
				)
			)
			
				
			closestIndex <- which.min(dists)
			adminEnv <- rbind(adminEnv, thisCountyEnv[closestIndex, , drop=FALSE])
		}
		
		names(adminEnv) <- names(sqPcaRasts)
		
		impreciseRecsEnv <- rbind(preciseRecsEnv, adminEnv)
		impreciseBgEnv <- extract(sqPcaRasts, bgInImpreciseBuff)
		impreciseBgEnv <- impreciseBgEnv[complete.cases(impreciseBgEnv), , drop=FALSE]
		
		impreciseData <- rbind(impreciseRecsEnv, impreciseBgEnv)
		presBg <- c(rep(1, nrow(preciseRecs) + nrow(adminSp)), rep(0, nrow(impreciseBgEnv)))
		impreciseData <- cbind(presBg, impreciseData)
		impreciseData <- as.data.frame(impreciseData)

	### model
	#########
		
		say('| model', post=0)
		
		errorlessModel <- trainMaxNet(errorlessData, regMult=regMult, classes=maxEntClasses)
		preciseModel <- if (numPrecise >= 5) {
			trainMaxNet(preciseData, regMult=regMult, classes=maxEntClasses)
		} else {
			trainMaxEnt(preciseData, regMult=regMult, classes=maxEntClasses)
		}
		impreciseModel <- trainMaxNet(impreciseData, regMult=regMult, classes=maxEntClasses)

	### predictions to evaluation background sites in buffered MCP around errorless records
	#######################################################################################
		
		say('| predict', post=0)
		
		errorlessPredToSq <- predictEnmSdm(errorlessModel, errorlessEvalBgSqEnv, maxentFun='enmSdm', type='cloglog')
		precisePredToSq <- predictEnmSdm(preciseModel, errorlessEvalBgSqEnv, maxentFun='enmSdm', type='cloglog')
		imprecisePredToSq <- predictEnmSdm(impreciseModel, errorlessEvalBgSqEnv, maxentFun='enmSdm', type='cloglog')

		errorlessPredToFut <- predictEnmSdm(errorlessModel, errorlessEvalBgFutEnv, maxentFun='enmSdm', type='cloglog')
		precisePredToFut <- predictEnmSdm(preciseModel, errorlessEvalBgFutEnv, maxentFun='enmSdm', type='cloglog')
		imprecisePredToFut <- predictEnmSdm(impreciseModel, errorlessEvalBgFutEnv, maxentFun='enmSdm', type='cloglog')

	### evaluate calibration accuracy
	#################################

		say('| calib acc', post=0)

		### vs present

		# truth
		bgNiche <- extract(sqNicheRast, bgInErrorlessBuffEval)
		if (any(is.na(bgNiche))) bgNiche <- bgNiche[!is.na(bgNiche)]
		
		# estimates
		bgNicheTrans <- logitAdj(bgNiche, epsilon = 0.001)
		errorlessPredTrans <- logitAdj(errorlessPredToSq, epsilon = 0.001) + runif(length(errorlessPredToSq) -eps(), eps())
		precisePredTrans <- logitAdj(precisePredToSq, epsilon = 0.001) + runif(length(precisePredToSq) -eps(), eps())
		imprecisePredTrans <- logitAdj(imprecisePredToSq, epsilon = 0.001) + runif(length(imprecisePredToSq) -eps(), eps())

		# comparison
		corTruthVsErrorlessSq <- cor(bgNicheTrans, errorlessPredTrans)
		corTruthVsPreciseSq <- cor(bgNicheTrans, precisePredTrans)
		corTruthVsImpreciseSq <- cor(bgNicheTrans, imprecisePredTrans)
	
		### vs future
		
		# truth
		bgNiche <- extract(futNicheRast, bgInErrorlessBuffEval)
		if (any(is.na(bgNiche))) bgNiche <- bgNiche[!is.na(bgNiche)]
		
		# estimates
		bgNicheTrans <- logitAdj(bgNiche, epsilon = 0.001)
		errorlessPredTrans <- logitAdj(errorlessPredToFut, epsilon = 0.001) + runif(length(errorlessPredToFut) -eps(), eps())
		precisePredTrans <- logitAdj(precisePredToFut, epsilon = 0.001) + runif(length(precisePredToFut) -eps(), eps())
		imprecisePredTrans <- logitAdj(imprecisePredToFut, epsilon = 0.001) + runif(length(imprecisePredToFut) -eps(), eps())

		# comparison
		corTruthVsErrorlessFut <- cor(bgNicheTrans, errorlessPredTrans)
		corTruthVsPreciseFut <- cor(bgNicheTrans, precisePredTrans)
		corTruthVsImpreciseFut <- cor(bgNicheTrans, imprecisePredTrans)
	
	### evaluate EOO
	################

		say('| eoo', post=0)

		### all precise
		eooErrorless_km2 <- gArea(errorlessMcpSpEa) / 1000^2

		### only precise
		eooPrecise_km2 <- gArea(preciseMcpSpEa) / 1000^2
	
		# precise + imprecise
		eooImprecise_km2 <- gArea(impreciseMcpSpEa) / 1000^2
	
	### evaluate climate change exposure
	####################################

		say('| expos', post=0)

		# thresholds
		predPres_errorless <- predictEnmSdm(errorlessModel, errorlessRecsEnv, maxentFun='enmSdm', type='cloglog')
		predPres_precise <- predictEnmSdm(preciseModel, preciseRecsEnv, maxentFun='enmSdm', type='cloglog')
		predPres_imprecise <- predictEnmSdm(impreciseModel, impreciseRecsEnv, maxentFun='enmSdm', type='cloglog')

		thold_errorless <- quantile(predPres_errorless, 0.1)
		thold_precise <- quantile(predPres_precise, 0.1)
		thold_imprecise <- quantile(predPres_imprecise, 0.1)

		# prediction rasters
		sqPredRast_errorless <- predict(errorlessMaskedSqPcaRasts, errorlessModel, clamp=FALSE, type='cloglog')
		sqPredRast_precise <- predict(errorlessMaskedSqPcaRasts, preciseModel, clamp=FALSE, type='cloglog')
		sqPredRast_imprecise <- predict(errorlessMaskedSqPcaRasts, impreciseModel, clamp=FALSE, type='cloglog')
		
		futPredRast_errorless <- predict(errorlessMaskedFutPcaRasts, errorlessModel, clamp=FALSE, type='cloglog')
		futPredRast_precise <- predict(errorlessMaskedFutPcaRasts, preciseModel, clamp=FALSE, type='cloglog')
		futPredRast_imprecise <- predict(errorlessMaskedFutPcaRasts, impreciseModel, clamp=FALSE, type='cloglog')
		
		# threshold rasters
		sqPredRast_errorless <- sqPredRast_errorless >= thold_errorless
		sqPredRast_precise <- sqPredRast_precise >= thold_precise
		sqPredRast_imprecise <- sqPredRast_imprecise >= thold_imprecise
		
		futPredRast_errorless <- futPredRast_errorless >= thold_errorless
		futPredRast_precise <- futPredRast_precise >= thold_precise
		futPredRast_imprecise <- futPredRast_imprecise >= thold_imprecise

		# suitable area
		sqSuitArea_errorless_km2 <- cellStats(areaRast_km2 * sqPredRast_errorless, 'sum')
		sqSuitArea_precise_km2 <- cellStats(areaRast_km2 * sqPredRast_precise, 'sum')
		sqSuitArea_imprecise_km2 <- cellStats(areaRast_km2 * sqPredRast_imprecise, 'sum')
		
		futSuitArea_errorless_km2 <- cellStats(areaRast_km2 * futPredRast_errorless, 'sum')
		futSuitArea_precise_km2 <- cellStats(areaRast_km2 * futPredRast_precise, 'sum')
		futSuitArea_imprecise_km2 <- cellStats(areaRast_km2 * futPredRast_imprecise, 'sum')

		# stable suitable area
		stable_errorless <- futPredRast_errorless * sqPredRast_errorless * areaRast_km2
		stable_precise <- futPredRast_precise * sqPredRast_precise * areaRast_km2
		stable_imprecise <- futPredRast_imprecise * sqPredRast_imprecise * areaRast_km2
		
		stableArea_errorless_km2 <- cellStats(stable_errorless, 'sum')
		stableArea_precise_km2 <- cellStats(stable_precise, 'sum')
		stableArea_imprecise_km2 <- cellStats(stable_imprecise, 'sum')
		
		# gained/lost area
		deltaArea_errorless <- areaRast_km2 * (futPredRast_errorless - sqPredRast_errorless)
		deltaArea_precise <- areaRast_km2 * (futPredRast_precise - sqPredRast_precise)
		deltaArea_imprecise <- areaRast_km2 * (futPredRast_imprecise - sqPredRast_imprecise)
		

		gainFx <- function(x) ifelse(x > 0, x, NA)
		gainArea_errorless <- calc(deltaArea_errorless, fun=gainFx)
		gainArea_precise <- calc(deltaArea_precise, fun=gainFx)
		gainArea_imprecise <- calc(deltaArea_imprecise, fun=gainFx)
		
		gainArea_errorless_km2 <- cellStats(gainArea_errorless, 'sum')
		gainArea_precise_km2 <- cellStats(gainArea_precise, 'sum')
		gainArea_imprecise_km2 <- cellStats(gainArea_imprecise, 'sum')
		
		
		lostFx <- function(x) ifelse(x < 0, -1 * x, NA)
		lostArea_errorless <- calc(deltaArea_errorless, fun=lostFx)
		lostArea_precise <- calc(deltaArea_precise, fun=lostFx)
		lostArea_imprecise <- calc(deltaArea_imprecise, fun=lostFx)
		
		lostArea_errorless_km2 <- cellStats(lostArea_errorless, 'sum')
		lostArea_precise_km2 <- cellStats(lostArea_precise, 'sum')
		lostArea_imprecise_km2 <- cellStats(lostArea_imprecise, 'sum')

	### multivariate niche breadth
	##############################

		errorlessConvHullStats <- if (numPrecise >= 4) {
			convhulln(errorlessRecsEnv, options='FA')
		} else {
			list(area=NA, vol=NA)
		}

		preciseConvHullStats <- if (numPrecise >= 4) {
			convhulln(preciseRecsEnv, options='FA')
		} else {
			list(area=NA, vol=NA)
		}

		impreciseConvHullStats <- if (nrow(impreciseRecsEnv) >= 4) {
			convhulln(impreciseRecsEnv, options='FA')
		} else {
			list(area=NA, vol=NA)
		}
	
	### univariate niche breadth
	############################
	
		### errorless
		# MAT
		x <- extract(sqRasts[[1]], errorlessRecsSp)
		errorlessNicheBreadthMat_range_degC <- diff(range(x))
		errorlessNicheBreadthMat_095_degC <- diff(range(x[x >= quantile(x, 0.025) & x <= quantile(x, 0.975)]))
		errorlessNicheBreadthMat_090_degC <- diff(range(x[x >= quantile(x, 0.05) & x <= quantile(x, 0.95)]))

		# TAP
		x <- extract(sqRasts[[12]], errorlessRecsSp)
		errorlessNicheBreadthTap_range_mm <- diff(range(x))
		errorlessNicheBreadthTap_095_mm <- diff(range(x[x >= quantile(x, 0.025) & x <= quantile(x, 0.975)]))
		errorlessNicheBreadthTap_090_mm <- diff(range(x[x >= quantile(x, 0.05) & x <= quantile(x, 0.95)]))

		### precise
		# MAT
		x <- extract(sqRasts[[1]], preciseRecsSp)
		preciseNicheBreadthMat_range_degC <- diff(range(x))
		preciseNicheBreadthMat_095_degC <- diff(range(x[x >= quantile(x, 0.025) & x <= quantile(x, 0.975)]))
		preciseNicheBreadthMat_090_degC <- diff(range(x[x >= quantile(x, 0.05) & x <= quantile(x, 0.95)]))

		# TAP
		x <- extract(sqRasts[[12]], preciseRecsSp)
		preciseNicheBreadthTap_range_mm <- diff(range(x))
		preciseNicheBreadthTap_095_mm <- diff(range(x[x >= quantile(x, 0.025) & x <= quantile(x, 0.975)]))
		preciseNicheBreadthTap_090_mm <- diff(range(x[x >= quantile(x, 0.05) & x <= quantile(x, 0.95)]))

		### precise + imprecise
		# MAT
		adminEnvExt <- extract(sqRasts[[1]], adminSp)
		preciseRecsEnv <- extract(sqRasts[[1]], preciseRecs)

		preciseMeanEnv <- mean(preciseRecsEnv)
		adminEnv <- numeric()
		for (i in 1:nrow(adminSp)) {
			thisCountyEnv <- adminEnvExt[[i]]
			closestIndex <- which.min(abs(preciseMeanEnv - thisCountyEnv))
			adminEnv <- c(adminEnv, thisCountyEnv[closestIndex])
		}
		
		x <- c(preciseRecsEnv, adminEnv)
		impreciseNicheBreadthMat_range_degC <- diff(range(x))
		impreciseNicheBreadthMat_095_degC <- diff(range(x[x >= quantile(x, 0.025) & x <= quantile(x, 0.975)]))
		impreciseNicheBreadthMat_090_degC <- diff(range(x[x >= quantile(x, 0.05) & x <= quantile(x, 0.95)]))

		# TAP
		adminEnvExt <- extract(sqRasts[[12]], adminSp)
		preciseRecsEnv <- extract(sqRasts[[12]], preciseRecs)

		preciseMeanEnv <- mean(preciseRecsEnv)
		adminEnv <- numeric()
		for (i in 1:nrow(adminSp)) {
			thisCountyEnv <- adminEnvExt[[i]]
			closestIndex <- which.min(abs(preciseMeanEnv - thisCountyEnv))
			adminEnv <- c(adminEnv, thisCountyEnv[closestIndex])
		}
		
		x <- c(preciseRecsEnv, adminEnv)
		impreciseNicheBreadthTap_range_mm <- diff(range(x))
		impreciseNicheBreadthTap_095_mm <- diff(range(x[x >= quantile(x, 0.025) & x <= quantile(x, 0.975)]))
		impreciseNicheBreadthTap_090_mm <- diff(range(x[x >= quantile(x, 0.05) & x <= quantile(x, 0.95)]))

	### remember
	############
	
		say('| remember')
	
		thisRemember <- data.frame(

			futScenario = futScenario,

			numErrorless = numErrorless,
			numPrecise = numPrecise,
			numImprecise = numImprecise,
			numAdmin = nrow(adminSp),
			
			nicheShape = nicheShape,
			nicheRate1 = nicheRate1,
			nicheRate2 = nicheRate2,
			nicheRate3 = nicheRate3,
			minNicheRate = minNicheRate,
			maxNicheRate = maxNicheRate,
			mean1 = nicheCenter[1],
			mean2 = nicheCenter[2],
			mean3 = nicheCenter[3],
			var1 = var1,
			var2 = var2,
			var3 = var3,
			corr1v2 = corr1v2,
			corr1v3 = corr1v3,
			corr2v3 = corr2v3,
			
			buffSize_km = buffSize_km,
			regMult = paste(regMult, collapse=', '),
			maxEntClasses = maxEntClasses,
			
			# univariate niche breadth
			errorlessNicheBreadthMat_range_degC = errorlessNicheBreadthMat_range_degC,
			preciseNicheBreadthMat_range_degC = preciseNicheBreadthMat_range_degC,
			impreciseNicheBreadthMat_range_degC = impreciseNicheBreadthMat_range_degC,

			errorlessNicheBreadthMat_095_degC = errorlessNicheBreadthMat_095_degC,
			preciseNicheBreadthMat_095_degC = preciseNicheBreadthMat_095_degC,
			impreciseNicheBreadthMat_095_degC = impreciseNicheBreadthMat_095_degC,

			errorlessNicheBreadthMat_090_degC = errorlessNicheBreadthMat_090_degC,
			preciseNicheBreadthMat_090_degC = preciseNicheBreadthMat_090_degC,
			impreciseNicheBreadthMat_090_degC = impreciseNicheBreadthMat_090_degC,
			
			errorlessNicheBreadthTap_range_mm = errorlessNicheBreadthTap_range_mm,
			preciseNicheBreadthTap_range_mm = preciseNicheBreadthTap_range_mm,
			impreciseNicheBreadthTap_range_mm = impreciseNicheBreadthTap_range_mm,

			errorlessNicheBreadthTap_095_mm = errorlessNicheBreadthTap_095_mm,
			preciseNicheBreadthTap_095_mm = preciseNicheBreadthTap_095_mm,
			impreciseNicheBreadthTap_095_mm = impreciseNicheBreadthTap_095_mm,

			errorlessNicheBreadthTap_090_mm = errorlessNicheBreadthTap_090_mm,
			preciseNicheBreadthTap_090_mm = preciseNicheBreadthTap_090_mm,
			impreciseNicheBreadthTap_090_mm = impreciseNicheBreadthTap_090_mm,
			
			# multivariate niche breadth
			errorlessConvHullArea = errorlessConvHullStats$area,
			preciseConvHullArea = preciseConvHullStats$area,
			impreciseConvHullArea = impreciseConvHullStats$area,
			
			errorlessConvHullVol = errorlessConvHullStats$vol,
			preciseConvHullVol = preciseConvHullStats$vol,
			impreciseConvHullVol = impreciseConvHullStats$vol,
			
			# ENM calibration
			corTruthVsErrorlessSq = corTruthVsErrorlessSq,
			corTruthVsPreciseSq = corTruthVsPreciseSq,
			corTruthVsImpreciseSq = corTruthVsImpreciseSq,
			
			corTruthVsErrorlessFut = corTruthVsErrorlessFut,
			corTruthVsPreciseFut = corTruthVsPreciseFut,
			corTruthVsImpreciseFut = corTruthVsImpreciseFut,
			
			# EOO
			eooErrorless_km2 = eooErrorless_km2,
			eooPrecise_km2 = eooPrecise_km2,
			eooImprecise_km2 = eooImprecise_km2,
			
			# ENM range and climate exposure
			sqSuitArea_errorless_km2 = sqSuitArea_errorless_km2,
			sqSuitArea_precise_km2 = sqSuitArea_precise_km2,
			sqSuitArea_imprecise_km2 = sqSuitArea_imprecise_km2,
			
			futSuitArea_errorless_km2 = futSuitArea_errorless_km2,
			futSuitArea_precise_km2 = futSuitArea_precise_km2,
			futSuitArea_imprecise_km2 = futSuitArea_imprecise_km2,
			
			stableArea_errorless_km2 = stableArea_errorless_km2,
			stableArea_precise_km2 = stableArea_precise_km2,
			stableArea_imprecise_km2 = stableArea_imprecise_km2,
			
			gainArea_errorless_km2 = gainArea_errorless_km2,
			gainArea_precise_km2 = gainArea_precise_km2,
			gainArea_imprecise_km2 = gainArea_imprecise_km2,
			
			lostArea_errorless_km2 = lostArea_errorless_km2,
			lostArea_precise_km2 = lostArea_precise_km2,
			lostArea_imprecise_km2 = lostArea_imprecise_km2
			
		)
		

	### plot
	########

	### plot
	########

		cols <- colorRampPalette(c('gray40', 'gray80', 'white', 'white'))
		# cols <- hcl.colors(100)
		cols <- cols(100)

		# MCPs
		errorlessMcpSp <- sp::spTransform(errorlessMcpSpEa, getCRS('wgs84', TRUE))
		preciseMcpSp <- sp::spTransform(preciseMcpSpEa, getCRS('wgs84', TRUE))
		impreciseMcpSp <- sp::spTransform(impreciseMcpSpEa, getCRS('wgs84', TRUE))

		plot(errorlessBuffSp)
		surround <- createPlotPoly(errorlessBuffSp)
		dev.off()

		dirCreate('./Analysis/Examples')
		png('./Analysis/Examples/Example - Virtual Species with Present-Day Suitability & All Occurrence Types.png', res=600, width=1600, height=1200)

			par(mfrow=c(1, 1), oma=c(1, 1, 1, 1), mar=c(0, 0, 0, 0))

			# present
			plot(sqNicheRast, ext=surround, col=cols, legend=FALSE, axes=FALSE)
			plot(adminSp, border='orange', lwd=1.2, add=TRUE)
			points(errorlessRecsSp, pch=1, lwd=0.4, cex=0.6)
			points(preciseRecsSp, bg='#1b9e77', pch=21, lwd=0.4, cex=0.6)
			points(impreciseErrorlessRecs, bg='orange', lwd=0.4, pch=21, cex=0.6)

			plot(errorlessMcpSp, lty='dotted', lwd=0.6, add=TRUE)
			plot(preciseMcpSp, border='#1b9e77', lwd=0.6, lty='dotted', add=TRUE)
			plot(impreciseMcpSp, border='orange', lwd=0.6, lty='dotted', add=TRUE)
			plot(errorlessBuffSp, lwd=0.6, add=TRUE)
			
			legend(
				'right',
				inset=-0.6,
				xpd=NA,
				bty='n',
				cex=0.56,
				pt.lwd=0.4,
				legend=c(
					'Omniscient occurrence',
					'Omniscient MCP',
					'Precise occurrence',
					'Precise MCP',
					'Imprecise occurrence',
					'County occurrence',
					'Precise/county MCP',
					'Focal region'
				),
				pch=c(
					1,
					NA,
					21,
					NA,
					21,
					NA,
					NA,
					NA
				),
				pt.bg=c(
					NA,
					NA,
					'#1b9e77',
					NA,
					'orange',
					NA,
					NA,
					NA
				),
				col=c(
					'black',
					'black',
					'black',
					'#1b9e77',
					'black',
					'orange',
					'orange',
					'black'
				),
				lty=c(
					NA,
					'dotted',
					NA,
					'dotted',
					NA,
					'solid',
					'dotted',
					'solid'
				),
				lwd=c(
					NA,
					0.6,
					NA,
					0.6,
					NA,
					0.6,
					0.6,
					0.7
				)
				
			)
			
		dev.off()
			
		png('./Analysis/Examples/Example - Virtual Species with Present-Day Suitability.png', res=600, width=1600, height=1200)

			par(mfrow=c(1, 1), oma=c(1, 1, 1, 1), mar=c(0, 0, 0, 0))

			# present
			plot(sqNicheRast, ext=surround, col=cols, legend=FALSE, axes=FALSE)
			
		dev.off()
			
		png('./Analysis/Examples/Example - Virtual Species with Present-Day Suitability & All Errorless Occurrences.png', res=600, width=1600, height=1200)

			par(mfrow=c(1, 1), oma=c(1, 1, 1, 1), mar=c(0, 0, 0, 0))

			# present
			plot(sqNicheRast, ext=surround, col=cols, legend=FALSE, axes=FALSE)
			points(errorlessRecsSp, pch=1, lwd=0.4, cex=0.6)
			
		dev.off()
			
		png('./Analysis/Examples/Example - Virtual Species with Present-Day Suitability & All Precise Occurrences.png', res=600, width=1600, height=1200)

			par(mfrow=c(1, 1), oma=c(1, 1, 1, 1), mar=c(0, 0, 0, 0))

			# present
			plot(sqNicheRast, ext=surround, col=cols, legend=FALSE, axes=FALSE)
			points(preciseRecsSp, bg='#1b9e77', pch=21, lwd=0.4, cex=0.6)
			
			
		dev.off()
			
		png('./Analysis/Examples/Example - Virtual Species with Present-Day Suitability & All Imprecise Occurrences.png', res=600, width=1600, height=1200)

			par(mfrow=c(1, 1), oma=c(1, 1, 1, 1), mar=c(0, 0, 0, 0))

			# present
			plot(sqNicheRast, ext=surround, col=cols, legend=FALSE, axes=FALSE)
			points(impreciseErrorlessRecs, bg='orange', lwd=0.4, pch=21, cex=0.6)
			
		dev.off()
			
		png('./Analysis/Examples/Example - Virtual Species with Present-Day Suitability & All Imprecise & County Occurrences.png', res=600, width=1600, height=1200)

			par(mfrow=c(1, 1), oma=c(1, 1, 1, 1), mar=c(0, 0, 0, 0))

			# present
			plot(sqNicheRast, ext=surround, col=cols, legend=FALSE, axes=FALSE)
			plot(adminSp, border='orange', lwd=1.2, add=TRUE)
			points(impreciseErrorlessRecs, bg='orange', lwd=0.4, pch=21, cex=0.6)
			
		dev.off()
			
		png('./Analysis/Examples/Example - Virtual Species with Present-Day Suitability & All Precise & County Occurrences.png', res=600, width=1600, height=1200)

			par(mfrow=c(1, 1), oma=c(1, 1, 1, 1), mar=c(0, 0, 0, 0))

			# present
			plot(sqNicheRast, ext=surround, col=cols, legend=FALSE, axes=FALSE)
			plot(adminSp, border='orange', lwd=1.2, add=TRUE)
			points(preciseRecsSp, bg='#1b9e77', pch=21, lwd=0.4, cex=0.6)
			
		dev.off()
			
		png('./Analysis/Examples/Example - Virtual Species with Future Suitability.png', res=600, width=1600, height=1200)

			par(mfrow=c(1, 1), oma=c(1, 1, 1, 1), mar=c(0, 0, 0, 0))

			# present
			plot(futNicheRast, ext=surround, col=cols, legend=FALSE, axes=FALSE)
			# plot(errorlessBuffSp, lwd=0.6, add=TRUE)

		dev.off()
			
		
		png('./Analysis/Examples/Example - Virtual Species with Present Suitability from Errorless Model.png', res=600, width=1600, height=1200)

			par(mfrow=c(1, 1), oma=c(1, 1, 1, 1), mar=c(0, 0, 0, 0))

			# present
			plot(futNicheRast, ext=surround, col='gray75', legend=FALSE, axes=FALSE)
			plot(sqPredRast_errorless, col=c('gray65', 'forestgreen'), legend=FALSE, add=TRUE)
			plot(errorlessBuffSp, lwd=0.6, add=TRUE)
			
		dev.off()
			
		png('./Analysis/Examples/Example - Virtual Species with FUTURE Suitability from Errorless Model.png', res=600, width=1600, height=1200)

			par(mfrow=c(1, 1), oma=c(1, 1, 1, 1), mar=c(0, 0, 0, 0))

			# present
			plot(futNicheRast, ext=surround, col='gray75', legend=FALSE, axes=FALSE)
			plot(futPredRast_errorless, col=c('gray65', 'forestgreen'), legend=FALSE, add=TRUE)
			plot(errorlessBuffSp, lwd=0.6, add=TRUE)
			
		dev.off()
			
		png('./Analysis/Examples/Example - Virtual Species with PRESENT Suitability from Precise Model.png', res=600, width=1600, height=1200)

			par(mfrow=c(1, 1), oma=c(1, 1, 1, 1), mar=c(0, 0, 0, 0))

			# present
			plot(futNicheRast, ext=surround, col='gray75', legend=FALSE, axes=FALSE)
			plot(sqPredRast_precise, col=c('gray65', 'forestgreen'), legend=FALSE, add=TRUE)
			# points(preciseRecsSp, bg='#1b9e77', pch=21, lwd=0.4, cex=0.6)
			plot(errorlessBuffSp, lwd=0.6, add=TRUE)
			
		dev.off()
			
		png('./Analysis/Examples/Example - Virtual Species with FUTURE Suitability from Precise Model.png', res=600, width=1600, height=1200)

			par(mfrow=c(1, 1), oma=c(1, 1, 1, 1), mar=c(0, 0, 0, 0))

			# present
			plot(futNicheRast, ext=surround, col='gray75', legend=FALSE, axes=FALSE)
			plot(futPredRast_precise, col=c('gray65', 'forestgreen'), legend=FALSE, add=TRUE)
			plot(errorlessBuffSp, lwd=0.6, add=TRUE)
			
		dev.off()
			
		png('./Analysis/Examples/Example - Virtual Species with PRESENT Suitability from Imprecise Model.png', res=600, width=1600, height=1200)

			par(mfrow=c(1, 1), oma=c(1, 1, 1, 1), mar=c(0, 0, 0, 0))

			# present
			plot(futNicheRast, ext=surround, col='gray75', legend=FALSE, axes=FALSE)
			plot(sqPredRast_imprecise, col=c('gray65', 'forestgreen'), legend=FALSE, add=TRUE)
			# plot(adminSp, border='orange', lwd=1.2, add=TRUE)
			# points(preciseRecsSp, bg='#1b9e77', pch=21, lwd=0.4, cex=0.6)
			plot(errorlessBuffSp, lwd=0.6, add=TRUE)
			
		dev.off()
			
		png('./Analysis/Examples/Example - Virtual Species with FUTURE Suitability from Imprecise Model.png', res=600, width=1600, height=1200)

			par(mfrow=c(1, 1), oma=c(1, 1, 1, 1), mar=c(0, 0, 0, 0))

			# present
			plot(futNicheRast, ext=surround, col='gray75', legend=FALSE, axes=FALSE)
			plot(futPredRast_imprecise, col=c('gray65', 'forestgreen'), legend=FALSE, add=TRUE)
			plot(errorlessBuffSp, lwd=0.6, add=TRUE)
			
		dev.off()

	### report!
	###########

		say('Precise : errorless present-day suitable area: ', round(thisRemember$sqSuitArea_precise_km2 / thisRemember$sqSuitArea_errorless_km2, 2))
		say('Imprecise : errorless present-day suitable area: ', round(thisRemember$sqSuitArea_imprecise_km2 / thisRemember$sqSuitArea_errorless_km2, 2), post=2)
		
		say('Precise : errorless future suitable area: ', round(thisRemember$futSuitArea_precise_km2 / thisRemember$futSuitArea_errorless_km2, 2))
		say('Imprecise : errorless future suitable area: ', round(thisRemember$futSuitArea_imprecise_km2 / thisRemember$futSuitArea_errorless_km2, 2))
		
		say('Betas:')
		print(errorlessModel$betas)
		print(preciseModel$betas)
		print(impreciseModel$betas)

		
say('DONE!', deco='&', level=1)

