### EFFECT OF INCLUDING IMPRECISE SPECIMENS ON CLIMATE CHANGE EXPOSURE - VIRTUAL SPECIES
### Adam B. Smith | Missouri Botanical Garden | adam.smith@mobot.org | 2021-10
### 
### source('C:/Ecology/Drive/Research/Vaguely Georeferenced Specimen Records/Code/07 Virtual Species - Completely Synthetic.r')
### source('E:/Ecology/Drive/Research/Vaguely Georeferenced Specimen Records/Code/07 Virtual Species - Completely Synthetic.r')

### CONTENTS ###
### setup ###
### calculate ensemble future rasters ###
### generate simulated species ###
### collate results ###
### plots of SDM calibration accuracy ###
### bar plots of EOO accuracy ###
### point plots of EOO accuracy ###

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
	library(geometry)
	library(qgam)
	library(ggplot2)
	library(mvtnorm)
	library(rgeos)
	library(scales)
	library(terra)
	
	# custom (Adam Smith)
	library(enmSdm)
	library(omnibus)
	library(statisfactory)
	
	# source(paste0(drive, '/Ecology/Drive/R/enmSdm/R/predictEnmSdm.r'))
	# source(paste0(drive, '/Ecology/Drive/R/enmSdm/R/predictMaxEnt.r'))
	
	
	ll <- c('longitude', 'latitude')

	# designations for accurate, inaccurate, and county records
	accAssigns <- 'certain/precise'
	inaccAssigns <- c('uncertain/precise', 'certain/imprecise', 'uncertain/imprecise')
	adminAssigns <- c('county/precise', 'county/imprecise', 'county-only', 'state/imprecise', 'state-only')
	countyAssigns <- c('county/precise', 'county/imprecise', 'county-only')
	stateAssigns <- c('state/imprecise', 'state-only')

# say('#########################################')
# say('### calculate ensemble future rasters ###')
# say('#########################################')

	# say('Using airUpThere (production version) to calculate ensemble BIOCLIM variables for 5 ESMs under SSP 585 for 2061-2080.')

	# ff <- list.files('E:/Ecology/Drive/R/airUpThere/R', pattern='.r', full.names=TRUE)
	# for (f in ff) source(f)

	# ff <- list.files('E:/Ecology/Drive/R/airUpThere/data', pattern='.rda', full.names=TRUE)
	# for (f in ff) load(f)

	# esm <- c('BCC-CSM2-MR', 'CNRM-ESM2-1', 'CanESM5', 'IPSL-CM6A-LR', 'MIROC-ES2L')
	# vars <- c('tmin', 'tmax', 'ppt')

	# for (thisVar in vars) {

		# say(thisVar, post=0)

		# if (thisVar == 'tmin' & exists('tmin')) rm(tmin)
		# if (thisVar == 'tmax' & exists('tmax')) rm(tmax)
		# if (thisVar == 'ppt' & exists('ppt')) rm(ppt)

		# if (exists('ens')) rm(ens)

		# for (month in 1:12) {

			# say(month, post=ifelse(month == 12, 1, 0))

			# thisEns <- wcEnsemble(
				# dir=paste0(drive, '/Ecology/Climate/WorldClim'),
				# ver=2.1,
				# res=10,
				# vars=paste0(thisVar, month),
				# esm=esm,
				# ghg=585,
				# period=2070,
				# fun = mean
			# )
			
			# names(thisEns) <- paste0(thisVar, month)
			
			# ens <- if (!exists('ens')) {
				# thisEns
			# } else {
				# c(ens, thisEns)
			# }
			
		# } # next month
		
		# assign(thisVar, ens)
		
	# } # next variable
		
	# tmin <- stack(tmin)
	# tmax <- stack(tmax)
	# ppt <- stack(ppt)
	
	# bc <- biovars(ppt, tmin, tmax)
	
	# dirCreate('./Data/WorldClim/cmip6_ensemble_ssp585_2061-2080')
	# writeRaster(bc, './Data/WorldClim/cmip6_ensemble_ssp585_2061-2080/bioclim.tif', overwrite=TRUE)

say('##################################')
say('### generate simulated species ###')
say('##################################')	

	### user-defined
	################
	
		series <- data.frame()
		for (totalErrorless in c(20, 40, 80, 160, 320)) {
		
			totalPrecise <- seq(5, min(totalErrorless - 5, 30), by=5)
			
			for (thisTotalPrecise in totalPrecise) {
			
				totalImprecise <- 2^(0:16)
				totalImprecise <- totalImprecise[totalImprecise < totalErrorless - max(thisTotalPrecise)]
				totalImprecise <- c(totalImprecise, totalErrorless - thisTotalPrecise)
			
				this <- expand.grid(totalErrorless = totalErrorless, totalPrecise=thisTotalPrecise, totalImprecise=totalImprecise)
				series <- rbind(series, this)
				
			}
			
		}
		
		series <- series[1:25, , drop=FALSE] # sumisu
		# series <- series[26:50, , drop=FALSE] # sumisu
		# series <- series[51:75, , drop=FALSE] # sumisu
		# series <- series[76:100, , drop=FALSE] # sumisu
		# series <- series[101:125, , drop=FALSE] #1
		# series <- series[126:150, , drop=FALSE] #1
		# series <- series[151:175, , drop=FALSE] #1
		# series <- series[176:200, , drop=FALSE] #1
		# series <- series[201:208, , drop=FALSE] # unassigned
		
		repStart <- 1
		repEnd <- 200

		buffSize_km <- 300 # around sites for calibration region
		
		# maxent settings
		regMult <- c(seq(0.5, 3, by=0.5), 4, 5)
		maxEntClasses <- 'lpq'
		
		# bounds for runif() for gamma distribution rate parameter for MVN variance
		minNicheRate <- 7.5
		maxNicheRate <- 30
		
	### data
	########
		
		say('Load data...')
		
		# PCA
		load('./Analysis/PCA on North American Climate.rda')

		# state/county environmental data
		load('./Data/Environment for North American Counties at Resolution 10 arcmin for Present.rda')
		mask <- raster('./Regions/mask_northAmerica.tif')

		# North American spatial polygons
		load('./Regions/GADM Ver 3pt6 North America Level 1 WGS84.rda')
		load('./Regions/GADM Ver 3pt6 North America Level 2 WGS84.rda')
		load('./Regions/GADM Ver 3pt6 North America Level 2 Albers.rda')
			
		nam2Sp <- nam2Sp[nam2Sp$NAME_1 != 'Hawaii', ]
		nam2SpEa <- nam2SpEa[nam2SpEa$NAME_1 != 'Hawaii', ]
		nam2SpEa$nam2area_km2 <- gArea(nam2SpEa, byid=TRUE) / 1000^2
		nam2Sp$nam2area_km2 <- nam2SpEa$nam2area_km2
		maxArea_km2 <- nam2SpEa$nam2area_km2[nam2SpEa$NAME_1 == 'California' & nam2SpEa$NAME_2 == 'San Bernardino']
	
		# contemporary climate rasters
		sqRasts <- if (drive == 'C:') {
			stack(paste0('D:/Ecology/Climate/WORLDCLIM Ver 2.1 January 2020/wc2.1_10m_bio/wc2.1_10m_bio_', 1:19, '.tif'))
		} else if (drive == 'E:') {
			stack(paste0('E:/Ecology/Climate/WorldClim/worldclim_2.1_10arcmin_historical/wc2.1_10m_bio_', 1:19, '.tif'))
		}
		
		# future climate rasters
		futScenario <- paste0('ensemble of ', paste(c('BCC-CSM2-MR', 'CNRM-ESM2-1', 'CanESM5', 'IPSL-CM6A-LR', 'MIROC-ES2L'), collapse=', '))
		futRasts <- stack('./Data/WorldClim/cmip6_ensemble_ssp585_2061-2080/bioclim.tif')
		
		rm(nam2SpEa); gc()

	### apply PCA to climate rasters
	################################
	
		say('PCA rasters...')
	
		### contemporary rasters
		
			sqRasts <- sqRasts * mask
			names(sqRasts) <- paste0('bio', 1:19)

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
			
			sqPcaDf <- as.data.frame(sqPcaRasts)
			sqPcaDfNoNA <- sqPcaDf[complete.cases(sqPcaDf), ]
			sqPcaDfNoNAMat <- as.matrix(sqPcaDfNoNA)

			# get climate for just Mexico and USA for selecting niche mean value
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
			
			rm(climDf, conusMex, conusMexMask, sqPcaDfConusMex, areaConusMexMask_km2, sqPcaDfNoNA); gc()

		### future rasters
				
			futRasts <- futRasts * mask
			names(futRasts) <- paste0('bio', 1:19)

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
			
			futPcaDf <- as.data.frame(futPcaRasts)
			futPcaDfNoNA <- futPcaDf[complete.cases(futPcaDf), ]
			futPcaDfNoNAMat <- as.matrix(futPcaDfNoNA)

			rm(futRasts, climDf, pcPrediction, futPcaDfNoNA); gc()
			
	### area raster
	###############

		areaRast_km2 <- raster::area(sqPcaRasts[[1]])
		areaRast_km2 <- areaRast_km2 * mask
		areaVect_km2 <- as.vector(areaRast_km2)
		areaVect_km2 <- areaVect_km2[!is.na(areaVect_km2)]

	### worker function: simulate species, draw samples, model, evaluate
	####################################################################

		worker <- compiler::cmpfun(function() {
		
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
				errorlessRecs <- randomPoints(sqNicheRast, numErrorless, prob=TRUE)
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
				
			# ### plot
			# ########
			
				# par(oma=c(0, 0, 0, 0), mfrow=c(1, 2))

				# # present
				# cols <- colorRampPalette(c('gray80', 'green', 'forestgreen'))
				# cols <- cols(10)
				# main <- paste0('SQ ', thisNumErrorless, ' (', nrow(errorlessRecs), ') el ', thisNumPrecise, ' pr')
				# plot(trim(errorlessMcpBuffMask), col=NA, legend=FALSE, main=main)
				# plot(sqNicheRast, col=cols, legend=FALSE, add=TRUE)
				# plot(adminSp, border='orange4', add=TRUE)
				# points(errorlessRecsSp, pch='.')
				# points(preciseRecsSp, col='red', pch='.')
				
				# plot(impreciseMcpSpEa, border='orange', add=TRUE)
				# plot(errorlessBuffSp, add=TRUE)
				# plot(preciseBuffSp, border='red', add=TRUE)
				# plot(impreciseBuffSp, border='orange4', add=TRUE)

				# # future delta
				# main <- paste0('fut delta')
				# plot(trim(errorlessMcpBuffMask), col=NA, legend=FALSE, main=main)
				# delta <- sqNicheRast - futNicheRast
				# plot(delta, legend=TRUE, add=TRUE)
				# points(errorlessRecsSp, pch='.')
				# points(preciseRecsSp, col='red', pch='.')
				
				# plot(impreciseMcpSpEa, border='orange', add=TRUE)
				# plot(errorlessBuffSp, add=TRUE)
				# plot(preciseBuffSp, border='red', add=TRUE)
				# plot(impreciseBuffSp, border='orange4', add=TRUE)

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

					rep = rep,
					futScenario = futScenario,

					numErrorless = numErrorless,
					numPrecise = numPrecise,
					numImpreciseErrorless = numImprecise,
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
				
			thisRemember
			
		})

	### MAIN
	########
	
	for (countSeries in 1:nrow(series)) {
	
		numErrorless <- series$totalErrorless[countSeries]
		numPrecise <- series$totalPrecise[countSeries]
		numImprecise <- series$totalImprecise[countSeries]

		# initiate/re-initiate storage
		if (file.exists(paste0('./Analysis/Virtual Species/Virtual Species Synthetic Results - Errorless ', thisNumErrorless, ' Precise ', thisNumPrecise, '.csv'))) {
			remember <- read.csv(paste0('./Analysis/Virtual Species/Virtual Species Synthetic Results - Errorless ', thisNumErrorless, ' Precise ', thisNumPrecise, '.csv'))
			repStart <- max(remember$rep) + 1
		} else {
			remember <- data.frame()
			repStart <- 1
		}
		
		rep <- repStart
		while (rep  <= repEnd) {

			say('[', thisNumErrorless, ' errorless | ', thisNumPrecise, ' precise | rep ', rep, ']', post=0)

			# simulate and model species
			thisRemember <- tryCatch(worker(), error=function(cond) FALSE)
			
			say('')
			if (!is(thisRemember, 'logical')) {

				remember <- rbind(remember, thisRemember, make.row.names=FALSE)
				rep <- rep + 1
			
				say(' SQ truth vs errorless ......... ', round(mean(remember$corTruthVsErrorlessSq, na.rm=TRUE), 2))
				say(' SQ truth vs precise ........... ', round(mean(remember$corTruthVsPreciseSq, na.rm=TRUE), 2))
				say(' SQ truth vs precise + imprecise ... ', round(mean(remember$corTruthVsImpreciseSq, na.rm=TRUE), 2), post=2)

				say(' Future truth vs errorless ......... ', round(mean(remember$corTruthVsErrorlessFut, na.rm=TRUE), 2))
				say(' Future truth vs precise ........... ', round(mean(remember$corTruthVsPreciseFut, na.rm=TRUE), 2))
				say(' Future truth vs precise + imprecise ... ', round(mean(remember$corTruthVsImpreciseFut, na.rm=TRUE), 2), post=2)
				
			}

			write.csv(remember, paste0('./Analysis/Virtual Species/Virtual Species Synthetic Results - Errorless ', thisNumErrorless, ' Precise ', thisNumPrecise, '.csv'), row.names=FALSE)

		} # next rep

		gc()

	} # next set in series

# say('#######################')
# say('### collate results ###')
# say('#######################')

	# # generalization
	# numReps <- 200

	# ### load results
	# files <- listFiles('./Analysis/Virtual Species', pattern='.csv')
	# results <- read.csv(files[1])
	# if (length(files) > 1) {
		# for (file in files[2:length(files)]) {
			# this <- read.csv(file)
			# results <- rbind(results, this)
		# }
	# }
	# # tally reps
	# series <- expand.grid(totalErrorless = 2^(0:12), numPrecise = seq(5, 30, by=5))
	# series$totalErrorless <- series$totalErrorless + series$numPrecise

	# series$n <- NA
	
	# for (i in 1:nrow(series)) {
		# these <- (results$numErrorless == series$totalErrorless[i] & results$numPrecise == series$numPrecise[i])
		# series$n[i] <- sum(these)
	# }
	
	# if (!all(series$n == numReps)) say('Incomplete!!!')
	
	# save(results, file='./Analysis/Virtual Species/!Collated Results.rda')
	
# say('#########################################')
# say('### plots of SDM calibration accuracy ###')
# say('#########################################')

	# # generalization
	# inner <- 0.9 # inner quantile of each box
	
	# errorlessCol <- 'black'
	# preciseCol <- 'black'
	# impreciseCol <- 'black'
	
	# errorlessFill <- 'white'
	# preciseFill <- '#1b9e77' # '#66c2a5'
	# # impreciseFill <- '#d95f02' # '#fc8d62'
	# impreciseFill <- '#fc8d62'
	
	# # calculations
	# lower <- (1 - inner) / 2
	# upper <- 1 - (1 - inner) / 2
	
	# # load data
	# load('./Analysis/Virtual Species/!Collated Results.rda')
	
	# # y-axis limits
	# y <- min(c(
		# quantile(results$corTruthVsErrorlessSq, lower, na.rm=TRUE),
		# quantile(results$corTruthVsPreciseSq, lower, na.rm=TRUE),
		# quantile(results$corTruthVsImpreciseSq, lower, na.rm=TRUE),
		# quantile(results$corTruthVsErrorlessFut, lower, na.rm=TRUE),
		# quantile(results$corTruthVsPreciseFut, lower, na.rm=TRUE),
		# quantile(results$corTruthVsImpreciseFut, lower, na.rm=TRUE)
	# ))
	
	# ylim <- c(roundTo(y, 1E-9, floor), 1)
	# sq <- fut <- list()
	# countPrecise <- 0

	# numPrecises <- sort(unique(results$numPrecise))

	# for (numPrecise in numPrecises) {
	# # for (numPrecise in 5) {
	
		# countPrecise <- countPrecise + 1
		# letterSq <- letters[2 * countPrecise - 1]
		# letterFut <- letters[2 * countPrecise]
	
		# thisResults <- results[results$numPrecise == numPrecise, ]
		# numErrorlesses <- sort(unique(thisResults$numErrorless))

		# ### construct tall data frame
		# yPreciseSq <- na.omit(thisResults$corTruthVsPreciseSq)
		# yPreciseFut <- na.omit(thisResults$corTruthVsPreciseFut)
		
		# tall <- data.frame(
			# data = 'precise',
			# numErrorless = 0,
			# middleSq = median(yPreciseSq),
			# lowerSq = quantile(yPreciseSq, lower),
			# upperSq = quantile(yPreciseSq, upper),
			# middleFut = median(yPreciseFut),
			# lowerFut = quantile(yPreciseFut, lower),
			# upperFut = quantile(yPreciseFut, upper)
		# )
		
		# for (numErrorless in numErrorlesses) {
		
			# yErrorlessSq <- na.omit(thisResults$corTruthVsErrorlessSq[thisResults$numErrorless == numErrorless])
			# yErrorlessFut <- na.omit(thisResults$corTruthVsErrorlessFut[thisResults$numErrorless == numErrorless])
			
			# yImpreciseSq <- na.omit(thisResults$corTruthVsImpreciseSq[thisResults$numErrorless == numErrorless])
			# yImpreciseFut <- na.omit(thisResults$corTruthVsImpreciseFut[thisResults$numErrorless == numErrorless])
			
			# tall <- rbind(
				# tall,
				# data.frame(
					# data = c('errorless', 'precise + imprecise'),
					# numErrorless = numErrorless,
					# middleSq = c(median(yErrorlessSq), median(yImpreciseSq)),
					# lowerSq = c(quantile(yErrorlessSq, lower), quantile(yImpreciseSq, lower)),
					# upperSq = c(quantile(yErrorlessSq, upper), quantile(yImpreciseSq, upper)),
					# middleFut = c(median(yErrorlessFut), median(yImpreciseFut)),
					# lowerFut = c(quantile(yErrorlessFut, lower), quantile(yImpreciseFut, lower)),
					# upperFut = c(quantile(yErrorlessFut, upper), quantile(yImpreciseFut, upper))
				# )
			# )
		
		# } # next number of errorless occurrences
		
		# tall$numImpreciseErrorless <- tall$numErrorless - numPrecise
		# tall$numImpreciseErrorless[tall$data == 'precise'] <- 0
		# tall$numImpreciseErrorless <- as.factor(tall$numImpreciseErrorless)
		
		# tallPrecise <- tall[tall$data=='precise', ]
		
		# ### plot!
		# sq[[countPrecise]] <- ggplot(tall, aes(x=numImpreciseErrorless, y=middleSq, fill=data, color=data)) +
			# geom_boxplot(middle=tall$middleSq, lower=tall$lowerSq, upper=tall$upperSq, size=0.2) +
			# scale_fill_manual(values=c(errorlessFill, preciseFill, impreciseFill)) +
			# scale_color_manual(values=c(errorlessCol, preciseCol, impreciseCol)) +
			# scale_size(guide='none') +
			# coord_cartesian(ylim=ylim) +
			# ggtitle(paste0(letterSq, ') Present: ', numPrecise, ' precise occurrences')) + 
			# xlab('Additional errorless occurrences') +
			# ylab('Calibration accuracy') +
			# theme(
				# text = element_text(size=10),
				# axis.text.x = element_text(angle = 60, vjust = 0.9, hjust=1),
				# legend.position='bottom'
			# )
			
		# fut[[countPrecise]] <- ggplot(tall, aes(x=numImpreciseErrorless, y=middleFut, fill=data)) +
			# geom_boxplot(middle=tall$middleFut, lower=tall$lowerFut, upper=tall$upperFut, size=0.2) +
			# scale_fill_manual(values=c(errorlessFill, preciseFill, impreciseFill)) +
			# scale_color_manual(values=c(errorlessCol, preciseCol, impreciseCol)) +
			# scale_size(guide='none') +
			# coord_cartesian(ylim=ylim) +
			# ggtitle(paste0(letterFut, ') Future: ', numPrecise, ' precise occurrences')) + 
			# xlab('Additional errorless occurrences') +
			# ylab('Calibration accuracy') +
			# theme(
				# text = element_text(size=10),
				# axis.text.x = element_text(angle = 60, vjust = 0.9, hjust=1),
				# legend.position='bottom'
			# )
			
		# # grab legend
		# legend <- get_legend(
			# # create some space to the left of the legend
			# # sq[[countPrecise]] + theme(legend.box.margin = margin(0, 0, 0, 2))
			# sq[[countPrecise]] + theme(legend.title = element_blank(), legend.text=element_text(size=13))
		# )

		# sq[[countPrecise]] <- sq[[countPrecise]] + theme(legend.position='none')		
		# fut[[countPrecise]] <- fut[[countPrecise]] + theme(legend.position='none')		
			
		
	# } # next number of precise occurrences

	# ### plot all results
	# ####################
		
		# plots <- plot_grid(
			# sq[[1]], fut[[1]],
			# sq[[2]], fut[[2]],
			# sq[[3]], fut[[3]],
			# sq[[4]], fut[[4]],
			# sq[[5]], fut[[5]],
			# sq[[6]], fut[[6]],
			# nrow=6
		# )
		# legend <- plot_grid(legend)
		
		# main <- plot_grid(plots, legend, labels=NULL, nrow=2, rel_heights=c(1, 0.05))

		# print(main)
		
		# ggsave(paste0('./Analysis/Virtual Species/!Calibration All Results.pdf'), width=7.5, height=10, units='in')

# say('#################################')
# say('### bar plots of EOO accuracy ###')
# say('#################################')

	# # generalization
	# inner <- 0.9 # inner quantile of each box
	
	# preciseCol <- 'black'
	# impreciseCol <- 'black'
	
	# preciseFill <- '#66c2a5'
	# impreciseFill <- '#fc8d62'
	
	# # calculations
	# lower <- (1 - inner) / 2
	# upper <- 1 - (1 - inner) / 2
	
	# # load data
	# load('./Analysis/Virtual Species/!Collated Results.rda')
	
	# # y-axis limits
	# mins <- min(c(
		# quantile(results$eooPrecise_km2 / results$eooErrorless_km2, lower, na.rm=TRUE),
		# quantile(results$eooImprecise_km2 / results$eooErrorless_km2, lower, na.rm=TRUE)
	# ))
	
	# maxs <- max(c(
		# quantile(results$eooPrecise_km2 / results$eooErrorless_km2, upper, na.rm=TRUE),
		# quantile(results$eooImprecise_km2 / results$eooErrorless_km2, upper, na.rm=TRUE)
	# ))
	
	# ylim <- c(roundTo(mins, 0.05, floor), roundTo(maxs, 0.1, ceiling))
	# if (ylim[1] > 0) ylim[1] <- 0
	# if (ylim[2] < 1) ylim[2] <- 1
	# ybreaks <- seq(0, 1, by=0.25)
	# if (ylim[1] < -0.25) ybreaks <- c(seq(0, ylim[1], by=-0.25), ybreaks)
	# if (ylim[2] > 1) ybreaks <- c(ybreaks, seq(1, ylim[2], by=0.25))
	# ybreaks <- unique(ybreaks)

	# p <- list()
	# countPrecise <- 0

	# numPrecises <- sort(unique(results$numPrecise))

	# for (numPrecise in numPrecises) {
	# # for (numPrecise in 5) {
	
		# countPrecise <- countPrecise + 1
		# letter <- letters[countPrecise]
	
		# thisResults <- results[results$numPrecise == numPrecise, ]
		# numErrorlesses <- sort(unique(thisResults$numErrorless))

		# tall <- data.frame()
		# for (numErrorless in numErrorlesses) {
		
			# errorless <- thisResults$eooErrorless_km2[thisResults$numErrorless == numErrorless]
			# precise <- thisResults$eooPrecise_km2[thisResults$numErrorless == numErrorless]
			
			# precise <- na.omit(precise / errorless)
			
			# tall <- rbind(
				# tall,
				# data.frame(
					# data = c('precise'),
					# numImpreciseErrorless = numErrorless - numPrecise,
					# middle = median(precise),
					# lower = quantile(precise, lower),
					# upper = quantile(precise, upper)
				# )
			# )
		
			# imprecise <- thisResults$eooImprecise_km2[thisResults$numErrorless == numErrorless]
			# imprecise <- na.omit(imprecise / errorless)
			
			# tall <- rbind(
				# tall,
				# data.frame(
					# data = c('precise + imprecise'),
					# numImpreciseErrorless = numErrorless - numPrecise,
					# middle = median(imprecise),
					# lower = quantile(imprecise, lower),
					# upper = quantile(imprecise, upper)
				# )
			# )
		
		# } # next number of errorless occurrences
		
		# tall$numImpreciseErrorless <- as.factor(tall$numImpreciseErrorless)
		
		# ### plot!
		# p[[countPrecise]] <- ggplot(tall, aes(x=numImpreciseErrorless, y=middle, fill=data, color=data)) +
			# geom_boxplot(middle=tall$middle, lower=tall$lower, upper=tall$upper, size=0.2) +
			# scale_fill_manual(values=c(preciseFill, impreciseFill)) +
			# scale_color_manual(values=c(preciseCol, impreciseCol)) +
			# scale_y_continuous(breaks=ybreaks) +
			# scale_size(guide='none') +
			# coord_cartesian(ylim=ylim) +
			# ggtitle(paste0(letter, ') ', numPrecise, ' precise occurrences')) + 
			# xlab('Additional errorless occurrences') +
			# ylab('Ratio of estimated-to-errorless\nextent of occurrence') +
			# theme(
				# text = element_text(size=12),
				# axis.text.x = element_text(angle = 60, vjust = 0.9, hjust=1),
				# legend.position='bottom'
			# )
			
		# # grab legend
		# legend <- get_legend(
			# # create some space to the left of the legend
			# # sq[[countPrecise]] + theme(legend.box.margin = margin(0, 0, 0, 2))
			# p[[countPrecise]] + theme(legend.title = element_blank(), legend.text=element_text(size=13))
		# )

		# p[[countPrecise]] <- p[[countPrecise]] + theme(legend.position='none')		
			
		
	# } # next number of precise occurrences
	
	# ### plot all results
	# ####################

		# plots <- plot_grid(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], p[[6]], nrow=3)
		# legend <- plot_grid(legend)
		
		# main <- plot_grid(plots, legend, labels=NULL, nrow=2, rel_heights=c(1, 0.05))

		# print(main)
		
		# ggsave(paste0('./Analysis/Virtual Species/!EOO Bars All Results.pdf'), width=7.5, height=10, units='in')

# say('###################################')
# say('### point plots of EOO accuracy ###')
# say('###################################')

	# # generalization
	# inner <- 0.9 # inner quantile of each box
	
	# preciseCol <- alpha('#1b9e77', 0.4)
	# impreciseCol <- alpha('#d95f02', 0.4)
	
	# preciseFill <- alpha('#1b9e77', 0.3)
	# impreciseFill <- alpha('#d95f02', 0.3)
	
	# preciseLine <- '#105c45'
	# impreciseLine <- '#924001'
	
	# # calculations
	# lower <- (1 - inner) / 2
	# upper <- 1 - (1 - inner) / 2
	
	# # load data
	# load('./Analysis/Virtual Species/!Collated Results.rda')
	
	# # y-axis limits
	# y <- c(results$eooPrecise_km2 / results$eooErrorless_km2, results$eooImprecise_km2 / results$eooErrorless_km2)
	# ylim <- c(0, max(2, quantile(y, inner)))
	# xlim <- c(0, max(results$numAdmin))
	
	# p <- list()
	# countPrecise <- 0
	# numPrecises <- sort(unique(results$numPrecise))

	# for (numPrecise in numPrecises) {
	# # for (numPrecise in c(5, 10)) {
	
		# say('-------------- number of precise: ', numPrecise)
	
		# countPrecise <- countPrecise + 1
		# letter <- letters[countPrecise]

		# ### collate ratios of estimates-to-errorless
		# thisResults <- results[results$numPrecise == numPrecise, ]

		# numAdmin <- thisResults$numAdmin
		# errorless <- thisResults$eooErrorless_km2
		# precise <- thisResults$eooPrecise_km2
		# imprecise <- thisResults$eooImprecise_km2
		
		# precise <- precise / errorless
		# imprecise <- imprecise / errorless
		
		# tall <- data.frame(
			# numAdmin = rep(numAdmin, 2),
			# data = c(rep('precise', length(precise)), rep('precise + imprecise', length(imprecise))),
			# ratio = c(precise, imprecise)
		# )
		
		# if (any(precise > ylim[2])) {
			# say('Number of precise not shown: ', sum(precise > ylim[2]), pre=2)
			# say('numAdmin values: ', numAdmin[precise > ylim[2]])
			# say('y values: ', precise[precise > ylim[2]])
		# }
		# if (any(imprecise > ylim[2])) {
			# say('Number of imprecise not shown: ', sum(imprecise > ylim[2]), pre=2)
			# say('numAdmin values: ', numAdmin[imprecise > ylim[2]])
			# say('y values: ', imprecise[imprecise > ylim[2]])
		# }
		
		# # calculate quantiles for regression
		# this <- tall[tall$data == 'precise', ]
		# middleQuantPrecise <- qgam(log(ratio) ~ s(numAdmin, bs='cr'), data=this, qu=0.5)
		# lowerQuantPrecise <- qgam(log(ratio) ~ s(numAdmin, bs='cr'), data=this, qu=lower)
		# upperQuantPrecise <- qgam(log(ratio) ~ s(numAdmin, bs='cr'), data=this, qu=upper)
		
		# middleQuantPrecise <- exp(predict(middleQuantPrecise, this, type='response'))
		# lowerQuantPrecise <- exp(predict(lowerQuantPrecise, this, type='response'))
		# upperQuantPrecise <- exp(predict(upperQuantPrecise, this, type='response'))

		# this <- tall[tall$data == 'precise + imprecise', ]
		# middleQuantImprecise <- qgam(log(ratio) ~ s(numAdmin, bs='cr'), data=this, qu=0.5)
		# lowerQuantImprecise <- qgam(log(ratio) ~ s(numAdmin, bs='cr'), data=this, qu=lower)
		# upperQuantImprecise <- qgam(log(ratio) ~ s(numAdmin, bs='cr'), data=this, qu=upper)
		
		# middleQuantImprecise <- exp(predict(middleQuantImprecise, this, type='response'))
		# lowerQuantImprecise <- exp(predict(lowerQuantImprecise, this, type='response'))
		# upperQuantImprecise <- exp(predict(upperQuantImprecise, this, type='response'))

		# middlePrecise <- data.frame(
			# numAdmin = tall$numAdmin,
			# data = rep('precise', length(lowerQuantPrecise)),
			# middle = middleQuantPrecise
		# )
		
		# middleImprecise <- data.frame(
			# numAdmin = tall$numAdmin,
			# data = rep('precise', length(lowerQuantImprecise)),
			# middle = middleQuantImprecise
		# )
		
		# middlePrecise <- middlePrecise[!duplicated(middlePrecise), ]
		# middleImprecise <- middleImprecise[!duplicated(middleImprecise), ]

		# order <- order(tall$numAdmin[tall$data=='precise'])
		# numAdmin <- thisResults$numAdmin[order]
		# quants <- data.frame(
			# numAdmin = c(numAdmin, rev(numAdmin), numAdmin, rev(numAdmin)),
			# ratio = c(lowerQuantPrecise[order], rev(upperQuantPrecise[order]), lowerQuantImprecise[order], rev(upperQuantImprecise[order])),
			# data = c(rep('precise', 2 * length(lowerQuantPrecise)), rep('precise + imprecise', 2 * length(lowerQuantImprecise)))
		# )
		
		# quants <- quants[!duplicated(quants), ]

		# ### plot!
		# p[[countPrecise]] <- ggplot() +
			# geom_point(data=tall, size=0.5, aes(x=numAdmin, y=ratio, shape=data, color=data)) +
			# geom_polygon(data=quants, mapping=aes(x=numAdmin, y=ratio, fill=data)) +
			# scale_fill_manual(values=c('precise'=preciseFill, 'precise + imprecise'=impreciseFill)) +
			# scale_color_manual(values=c('precise'=preciseCol, 'precise + imprecise'=impreciseCol)) +
			# scale_shape_manual(values=c('precise'=2, 'precise + imprecise'=1)) +
			# geom_line(data=middlePrecise, aes(x=numAdmin, y=middle), color=preciseLine, size=1, lineend='round') +
			# geom_line(data=middleImprecise, aes(x=numAdmin, y=middle), color=impreciseLine, size=1, lineend='round') +
			# ggtitle(paste0(letter, ') ', numPrecise, ' precise occurrences')) + 
			# xlab('Additional imprecise occurrences') +
			# ylab('Ratio of estimated-to-errorless\nextent of occurrence') +
			# xlim(xlim[1], xlim[2]) +
			# ylim(ylim[1], ylim[2]) +
			# theme(
				# text = element_text(size=12),
				# axis.text.x = element_text(angle = 60, vjust = 0.9, hjust=1),
				# legend.position='bottom'
			# )
					
		# # grab legend
		# legend <- get_legend(
			# # create some space to the left of the legend
			# p[[countPrecise]] + theme(legend.title = element_blank(), legend.text=element_text(size=13))
		# )

		# p[[countPrecise]] <- p[[countPrecise]] + theme(legend.position='none')		
			
		
	# } # next number of precise occurrences
	
	# ### plot all results
	# ####################

		# plots <- plot_grid(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], p[[6]], nrow=3)
		# legend <- plot_grid(legend)
		
		# main <- plot_grid(plots, legend, labels=NULL, nrow=2, rel_heights=c(1, 0.05))

		# print(main)
		
		# ggsave(paste0('./Analysis/Virtual Species/!EOO Points All Results.pdf'), width=7.5, height=10, units='in')

# say('##################################################')
# say('### plots of univariate niche breadth accuracy ###')
# say('##################################################')

	# # generalization
	# inner <- 0.9 # inner quantile of each box
	
	# preciseCol <- 'black'
	# impreciseCol <- 'black'
	
	# preciseFill <- '#66c2a5'
	# impreciseFill <- '#fc8d62'
	
	# # calculations
	# lower <- (1 - inner) / 2
	# upper <- 1 - (1 - inner) / 2
	
	# # load data
	# load('./Analysis/Virtual Species/!Collated Results.rda')
	
	# # y-axis limits
	# mins <- min(c(
		# quantile(results$preciseNicheBreadthMat_range_degC / results$errorlessNicheBreadthMat_range_degC, lower, na.rm=TRUE),
		# quantile(results$impreciseNicheBreadthMat_range_degC / results$errorlessNicheBreadthMat_range_degC, lower, na.rm=TRUE),
		# quantile(results$preciseNicheBreadthTap_range_mm / results$errorlessNicheBreadthTap_range_mm, lower, na.rm=TRUE),
		# quantile(results$impreciseNicheBreadthTap_range_mm / results$errorlessNicheBreadthTap_range_mm, lower, na.rm=TRUE)
	# ))
	
	# maxs <- max(c(
		# quantile(results$preciseNicheBreadthMat_range_degC / results$errorlessNicheBreadthMat_range_degC, upper, na.rm=TRUE),
		# quantile(results$impreciseNicheBreadthMat_range_degC / results$errorlessNicheBreadthMat_range_degC, upper, na.rm=TRUE),
		# quantile(results$preciseNicheBreadthTap_range_mm / results$errorlessNicheBreadthTap_range_mm, lower, na.rm=TRUE),
		# quantile(results$impreciseNicheBreadthTap_range_mm / results$errorlessNicheBreadthTap_range_mm, lower, na.rm=TRUE)
	# ))
	
	# ylim <- c(roundTo(mins, 0.05, floor), roundTo(maxs, 0.1, ceiling))
	# if (ylim[1] > 0) ylim[1] <- 0
	# if (ylim[2] < 1) ylim[2] <- 1
	# ybreaks <- c(ifelse(ylim[1] < -0.25, ylim[1], 0), seq(0, 1, by=0.25), ifelse(ylim[2] > 1.25, ylim[2], 1))
	# ybreaks <- unique(ybreaks)

	# mat <- tap <- list()
	# countPrecise <- 0

	# numPrecises <- sort(unique(results$numPrecise))

	# for (numPrecise in numPrecises) {
	# # for (numPrecise in 5) {
	
		# countPrecise <- countPrecise + 1
		# letter1 <- letters[2 * countPrecise - 1]
		# letter2 <- letters[2 * countPrecise]
	
		# thisResults <- results[results$numPrecise == numPrecise, ]
		# numErrorlesses <- sort(unique(thisResults$numErrorless))

		# tall <- data.frame()
		# for (numErrorless in numErrorlesses) {
		
			# errorlessMat <- thisResults$errorlessNicheBreadthMat_range_degC[thisResults$numErrorless == numErrorless]
			# errorlessTap <- thisResults$errorlessNicheBreadthTap_range_mm[thisResults$numErrorless == numErrorless]
			
			# preciseMat <- thisResults$preciseNicheBreadthMat_range_degC[thisResults$numErrorless == numErrorless]
			# preciseTap <- thisResults$preciseNicheBreadthTap_range_mm[thisResults$numErrorless == numErrorless]
			
			# impreciseMat <- thisResults$impreciseNicheBreadthMat_range_degC[thisResults$numErrorless == numErrorless]
			# impreciseTap <- thisResults$impreciseNicheBreadthTap_range_mm[thisResults$numErrorless == numErrorless]
			
			# preciseMat <- na.omit(preciseMat / errorlessMat)
			# preciseTap <- na.omit(preciseTap / errorlessTap)

			# impreciseMat <- na.omit(impreciseMat / errorlessMat)
			# impreciseTap <- na.omit(impreciseTap / errorlessTap)
			
			# tall <- rbind(
				# tall,
				# data.frame(
					# data = c('precise', 'precise + imprecise'),
					# numImpreciseErrorless = rep(numErrorless - numPrecise, 2),
					# middleMat = c(median(preciseMat), median(impreciseMat)),
					# lowerMat = c(quantile(preciseMat, lower), quantile(impreciseMat, lower)),
					# upperMat = c(quantile(preciseMat, upper), quantile(impreciseMat, upper)),
					# middleTap = c(median(preciseTap), median(impreciseTap)),
					# lowerTap = c(quantile(preciseTap, lower), quantile(impreciseTap, lower)),
					# upperTap = c(quantile(preciseTap, upper), quantile(impreciseTap, upper))
				# )
			# )
		
		# } # next number of errorless occurrences
		
		# tall$numImpreciseErrorless <- as.factor(tall$numImpreciseErrorless)
		
		# ### plot!
		# mat[[countPrecise]] <- ggplot(tall, aes(x=numImpreciseErrorless, y=middleMat, fill=data, color=data)) +
			# geom_boxplot(middle=tall$middleMat, lower=tall$lowerMat, upper=tall$upperMat, size=0.2) +
			# scale_fill_manual(values=c(preciseFill, impreciseFill)) +
			# scale_color_manual(values=c(preciseCol, impreciseCol)) +
			# scale_y_continuous(breaks=ybreaks) +
			# scale_size(guide='none') +
			# coord_cartesian(ylim=ylim) +
			# ggtitle(paste0(letter1, ') Temperature: ', numPrecise, ' precise occurrences')) + 
			# xlab('Additional errorless occurrences') +
			# ylab('Niche breadth\nratio') +
			# theme(
				# text = element_text(size=10),
				# axis.text.x = element_text(angle = 60, vjust = 0.9, hjust=1),
				# legend.position='bottom'
			# )
			
		# tap[[countPrecise]] <- ggplot(tall, aes(x=numImpreciseErrorless, y=middleTap, fill=data, color=data)) +
			# geom_boxplot(middle=tall$middleTap, lower=tall$lowerTap, upper=tall$upperTap, size=0.2) +
			# scale_fill_manual(values=c(preciseFill, impreciseFill)) +
			# scale_color_manual(values=c(preciseCol, impreciseCol)) +
			# scale_y_continuous(breaks=ybreaks) +
			# scale_size(guide='none') +
			# coord_cartesian(ylim=ylim) +
			# ggtitle(paste0(letter2, ') Precipitation: ', numPrecise, ' precise occurrences')) + 
			# xlab('Additional errorless occurrences') +
			# ylab('Niche breadth\nratio') +
			# theme(
				# text = element_text(size=10),
				# axis.text.x = element_text(angle = 60, vjust = 0.9, hjust=1),
				# legend.position='bottom'
			# )
			
		# # grab legend
		# legend <- get_legend(
			# # create some space to the left of the legend
			# # sq[[countPrecise]] + theme(legend.box.margin = margin(0, 0, 0, 2))
			# mat[[countPrecise]] + theme(legend.title = element_blank(), legend.text=element_text(size=13))
		# )

		# mat[[countPrecise]] <- mat[[countPrecise]] + theme(legend.position='none')		
		# tap[[countPrecise]] <- tap[[countPrecise]] + theme(legend.position='none')		
			
		
	# } # next number of precise occurrences
	
	# ### plot all results
	# ####################

		# plots <- plot_grid(
			# mat[[1]], tap[[1]],
			# mat[[2]], tap[[2]],
			# mat[[3]], tap[[3]],
			# mat[[4]], tap[[4]],
			# mat[[5]], tap[[5]],
			# mat[[6]], tap[[6]],
			# nrow=6
		# )
		# legend <- plot_grid(legend)
		
		# main <- plot_grid(plots, legend, labels=NULL, nrow=2, rel_heights=c(1, 0.05))

		# print(main)
		
		# ggsave(paste0('./Analysis/Virtual Species/!Univariate Niche Breadth All Results.pdf'), width=7.5, height=10, units='in')

# say('####################################################')
# say('### plots of multivariate niche breadth accuracy ###')
# say('####################################################')

	# # generalization
	# inner <- 0.9 # inner quantile of each box
	
	# preciseCol <- 'black'
	# impreciseCol <- 'black'
	
	# preciseFill <- '#66c2a5'
	# impreciseFill <- '#fc8d62'
	
	# # calculations
	# lower <- (1 - inner) / 2
	# upper <- 1 - (1 - inner) / 2
	
	# # load data
	# load('./Analysis/Virtual Species/!Collated Results.rda')
	
	# ### y-axis limits: volume
	# mins <- min(c(
		# quantile(results$preciseConvHullVol / results$errorlessConvHullVol, lower, na.rm=TRUE),
		# quantile(results$impreciseConvHullVol / results$errorlessConvHullVol, lower, na.rm=TRUE)
	# ))
	
	# maxs <- max(c(
		# quantile(results$preciseConvHullVol / results$errorlessConvHullVol, upper, na.rm=TRUE),
		# quantile(results$impreciseConvHullVol / results$errorlessConvHullVol, 1 - (1 - upper) / 2, na.rm=TRUE)
	# ))
	
	# ylim <- c(roundTo(mins, 0.1, floor), roundTo(maxs, 0.25, ceiling))
	# if (ylim[1] > 0) ylim[1] <- 0
	# if (ylim[2] < 1) ylim[2] <- 1
	# ybreaks <- seq(0, 1, by=0.5)
	# if (ylim[1] < -0.5) ybreaks <- c(seq(0, ylim[1], by=-0.5), ybreaks)
	# if (ylim[2] > 1) ybreaks <- c(ybreaks, seq(1, ylim[2], by=0.5))
	# ybreaks <- unique(ybreaks)

	# ylimVol <- ylim
	# ybreaksVol <- ybreaks

	# ### y-axis limits: area
	# mins <- min(c(
		# quantile(results$preciseConvHullArea / results$errorlessConvHullArea, lower, na.rm=TRUE),
		# quantile(results$impreciseConvHullArea / results$errorlessConvHullArea, lower, na.rm=TRUE)
	# ))
	
	# maxs <- max(c(
		# quantile(results$preciseConvHullArea / results$errorlessConvHullArea, upper, na.rm=TRUE),
		# quantile(results$impreciseConvHullArea / results$errorlessConvHullArea, upper, na.rm=TRUE)
	# ))
	
	# ylim <- c(roundTo(mins, 0.1, floor), roundTo(maxs, 0.25, ceiling))
	# if (ylim[1] > 0) ylim[1] <- 0
	# if (ylim[2] < 1) ylim[2] <- 1
	# ybreaks <- seq(0, 1, by=0.25)
	# if (ylim[1] < -0.25) ybreaks <- c(seq(0, ylim[1], by=-0.25), ybreaks)
	# if (ylim[2] > 1) ybreaks <- c(ybreaks, seq(1, ylim[2], by=0.25))
	# ybreaks <- unique(ybreaks)

	# ylimArea <- ylim
	# ybreaksArea <- ybreaks

	# area <- vol <- list()
	# countPrecise <- 0

	# numPrecises <- sort(unique(results$numPrecise))

	# for (numPrecise in numPrecises) {
	# # for (numPrecise in 5) {
	
		# countPrecise <- countPrecise + 1
		# letter <- letters[countPrecise]
	
		# thisResults <- results[results$numPrecise == numPrecise, ]
		# numErrorlesses <- sort(unique(thisResults$numErrorless))

		# ### construct tall data frame
		# preciseVol <- na.omit(thisResults$preciseConvHullVol / thisResults$errorlessConvHullVol)
		# preciseArea <- na.omit(thisResults$preciseConvHullArea / thisResults$errorlessConvHullArea)

		# tall <- data.frame(
			# data = 'precise',
			# numImpreciseErrorless = 0,
			# middleVol = median(preciseVol),
			# lowerVol = quantile(preciseVol, lower),
			# upperVol = quantile(preciseVol, upper),
			# middleArea = median(preciseArea),
			# lowerArea = quantile(preciseArea, lower),
			# upperArea = quantile(preciseArea, upper)
		# )
		
		# for (numErrorless in numErrorlesses) {
		
			# errorlessVol <- thisResults$errorlessConvHullVol[thisResults$numErrorless == numErrorless]
			# errorlessArea <- thisResults$errorlessConvHullArea[thisResults$numErrorless == numErrorless]
			
			# impreciseVol <- thisResults$impreciseConvHullVol[thisResults$numErrorless == numErrorless]
			# impreciseArea <- thisResults$impreciseConvHullArea[thisResults$numErrorless == numErrorless]
			
			# impreciseVol <- na.omit(impreciseVol / errorlessVol)
			# impreciseArea <- na.omit(impreciseArea / errorlessArea)
			
			# tall <- rbind(
				# tall,
				# data.frame(
					# data = c('precise + imprecise'),
					# numImpreciseErrorless = numErrorless - numPrecise,
					# middleVol = median(impreciseVol),
					# lowerVol = quantile(impreciseVol, lower),
					# upperVol = quantile(impreciseVol, upper),
					# middleArea = median(impreciseArea),
					# lowerArea = quantile(impreciseArea, lower),
					# upperArea = quantile(impreciseArea, upper)
				# )
			# )
		
		# } # next number of errorless occurrences
		
		# tall$numImpreciseErrorless <- as.factor(tall$numImpreciseErrorless)
		
		# ### plot!
		# vol[[countPrecise]] <- ggplot(tall, aes(x=numImpreciseErrorless, y=middleVol, fill=data, color=data)) +
			# geom_boxplot(middle=tall$middleVol, lower=tall$lowerVol, upper=tall$upperVol, size=0.2) +
			# scale_fill_manual(values=c(preciseFill, impreciseFill)) +
			# scale_color_manual(values=c(preciseCol, impreciseCol)) +
			# scale_y_continuous(breaks=ybreaksVol) +
			# scale_size(guide='none') +
			# coord_cartesian(ylim=ylimVol) +
			# ggtitle(paste0(letter, ') Volume: ', numPrecise, ' precise occurrences')) + 
			# xlab('Additional errorless occurrences') +
			# ylab('Niche volume\nratio') +
			# theme(
				# text = element_text(size=10),
				# axis.text.x = element_text(angle = 60, vjust = 0.9, hjust=1),
				# legend.position='bottom'
			# )
			
		# area[[countPrecise]] <- ggplot(tall, aes(x=numImpreciseErrorless, y=middleArea, fill=data, color=data)) +
			# geom_boxplot(middle=tall$middleArea, lower=tall$lowerArea, upper=tall$upperArea, size=0.2) +
			# scale_fill_manual(values=c(preciseFill, impreciseFill)) +
			# scale_color_manual(values=c(preciseCol, impreciseCol)) +
			# scale_y_continuous(breaks=ybreaksArea) +
			# scale_size(guide='none') +
			# coord_cartesian(ylim=ylimArea) +
			# ggtitle(paste0(letter, ') Area: ', numPrecise, ' precise occurrences')) + 
			# xlab('Additional errorless occurrences') +
			# ylab('Niche area\nratio') +
			# theme(
				# text = element_text(size=10),
				# axis.text.x = element_text(angle = 60, vjust = 0.9, hjust=1),
				# legend.position='bottom'
			# )
			
		# # grab legend
		# legend <- get_legend(
			# # create some space to the left of the legend
			# # sq[[countPrecise]] + theme(legend.box.margin = margin(0, 0, 0, 2))
			# vol[[countPrecise]] + theme(legend.title = element_blank(), legend.text=element_text(size=13))
		# )

		# vol[[countPrecise]] <- vol[[countPrecise]] + theme(legend.position='none')		
		# area[[countPrecise]] <- area[[countPrecise]] + theme(legend.position='none')		
			
		
	# } # next number of precise occurrences
	
	# ### plot all results
	# ####################
		
		# plots <- plot_grid(
			# vol[[1]], area[[1]],
			# vol[[2]], area[[2]],
			# vol[[3]], area[[3]],
			# vol[[4]], area[[4]],
			# vol[[5]], area[[5]],
			# vol[[6]], area[[6]],
			# nrow=6
		# )
		# legend <- plot_grid(legend)
		
		# main <- plot_grid(plots, legend, labels=NULL, nrow=2, rel_heights=c(1, 0.05))

		# print(main)
		
		# ggsave(paste0('./Analysis/Virtual Species/!Multivariate Niche Breadth All Results.pdf'), width=7.5, height=10, units='in')
			
# say('##############################')
# say('### plots of suitable area ###')
# say('##############################')

	# # generalization
	# inner <- 0.9 # inner quantile of each box
	
	# ylims <- c(-1, 2)
	
	# preciseCol <- 'black'
	# impreciseCol <- 'black'
	
	# preciseFill <- '#66c2a5'
	# impreciseFill <- '#fc8d62'
	
	# # calculations
	# lower <- (1 - inner) / 2
	# upper <- 1 - (1 - inner) / 2
	
	# # load data
	# load('./Analysis/Virtual Species/!Collated Results.rda')

	# # set up
	# sq <- fut <- list()
	# countPrecise <- 0

	# numPrecises <- sort(unique(results$numPrecise))

	# for (numPrecise in numPrecises) {
	# # for (numPrecise in 5) {
	
		# countPrecise <- countPrecise + 1
		# letter1 <- letters[countPrecise * 2 - 1]
		# letter2 <- letters[countPrecise * 2]
	
		# thisResults <- results[results$numPrecise == numPrecise, ]
		# numErrorlesses <- sort(unique(thisResults$numErrorless))

		# ### construct tall data frame
		# sqSuitErrorless <- thisResults$sqSuitArea_errorless_km2
		# sqSuitPrecise <- thisResults$sqSuitArea_precise_km2
		
		# futSuitErrorless <- thisResults$futSuitArea_errorless_km2
		# futSuitPrecise <- thisResults$futSuitArea_precise_km2

		# sqDelta <- log10((sqSuitPrecise + 0.1) / (sqSuitErrorless + 0.1))
		# futDelta <- log10((futSuitPrecise  + 0.1) / (futSuitErrorless + 0.1))

		# tall <- data.frame(
			# data = 'precise',
			# numImpreciseErrorless = 0,
			# lowerSq = quantile(sqDelta, lower),
			# upperSq = quantile(sqDelta, upper),
			# middleSq = median(sqDelta),
			# lowerFut = quantile(futDelta, lower),
			# upperFut = quantile(futDelta, upper),
			# middleFut = median(futDelta)
		# )
		
		# for (numErrorless in numErrorlesses) {

			# sqSuitErrorless <- thisResults$sqSuitArea_errorless_km2[thisResults$numErrorless == numErrorless]
			# sqSuitImprecise <- thisResults$sqSuitArea_imprecise_km2[thisResults$numErrorless == numErrorless]
			
			# futSuitErrorless <- thisResults$futSuitArea_errorless_km2[thisResults$numErrorless == numErrorless]
			# futSuitImprecise <- thisResults$futSuitArea_imprecise_km2[thisResults$numErrorless == numErrorless]

			# sqDelta <- log10((sqSuitImprecise + 0.1) / (sqSuitErrorless + 0.1))
			# futDelta <- log10((futSuitImprecise + 0.1) / (futSuitErrorless + 0.1))
			
			# tall <- rbind(
				# tall,
				# data.frame(
					# data = 'precise + imprecise',
					# numImpreciseErrorless = numErrorless - numPrecise,
					# lowerSq = quantile(sqDelta, lower),
					# upperSq = quantile(sqDelta, upper),
					# middleSq = median(sqDelta),
					# lowerFut = quantile(futDelta, lower),
					# upperFut = quantile(futDelta, upper),
					# middleFut = median(futDelta)
				# )
			# )
		
		
		# } # next number of errorless occurrences
		
		# tall$numImpreciseErrorless <- as.factor(tall$numImpreciseErrorless)
		
		# ### plot!
		# middleNice <- paste0(ifelse(tall$middleSq > 0, '+', ''), sprintf('%.2f', round(tall$middleSq, 2)))
		
		# sq[[countPrecise]] <- ggplot(tall, aes(x=numImpreciseErrorless, y=middleSq, fill=data, color=data)) +
			# geom_boxplot(middle=tall$middleSq, lower=tall$lowerSq, upper=tall$upperSq, size=0.2) +
			# scale_fill_manual(values=c('precise'=preciseFill, 'precise + imprecise'=impreciseFill)) +
			# scale_color_manual(values=c('precise'=preciseCol, 'precise + imprecise'=impreciseCol)) +
			# scale_size(guide='none') +
			# coord_cartesian(ylim=ylims) +
			# ggtitle(paste0(letter1, ') Present area: ', numPrecise, ' precise occurrences')) + 
			# xlab('Additional errorless occurrences') +
			# ylab(expression(atop(log[10]*' ratio', '(estimate / errorless)'))) +
			# geom_text(x=tall$numImpreciseErrorless, y=ylims[2], label=middleNice, size=2, angle=90, hjust='right') +
			# theme(
				# text = element_text(size=10),
				# axis.text.x = element_text(angle = 60, vjust = 0.9, hjust=1),
				# legend.position='bottom'
			# )
			
			
		# middleNice <- paste0(ifelse(tall$middleFut > 0, '+', ''), sprintf('%.2f', round(tall$middleFut, 2)))
		# fut[[countPrecise]] <- ggplot(tall, aes(x=numImpreciseErrorless, y=middleFut, fill=data, color=data)) +
			# geom_boxplot(middle=tall$middleFut, lower=tall$lowerFut, upper=tall$upperFut, size=0.2) +
			# scale_fill_manual(values=c('precise'=preciseFill, 'precise + imprecise'=impreciseFill)) +
			# scale_color_manual(values=c('precise'=preciseCol, 'precise + imprecise'=impreciseCol)) +
			# scale_size(guide='none') +
			# coord_cartesian(ylim=ylims) +
			# ggtitle(paste0(letter2, ') Future area: ', numPrecise, ' precise occurrences')) + 
			# xlab('Additional errorless occurrences') +
			# ylab(expression(atop(log[10]*' ratio', '(estimate / errorless)'))) +
			# geom_text(x=tall$numImpreciseErrorless, y=ylims[2], label=middleNice, size=2, angle=90, hjust='right') +
			# theme(
				# text = element_text(size=10),
				# axis.text.x = element_text(angle = 60, vjust = 0.9, hjust=1),
				# legend.position='bottom'
			# )
			
		# # grab legend
		# legend <- get_legend(
			# # create some space to the left of the legend
			# # sq[[countPrecise]] + theme(legend.box.margin = margin(0, 0, 0, 2))
			# sq[[countPrecise]] + theme(legend.title = element_blank(), legend.text=element_text(size=13))
		# )

		# sq[[countPrecise]] <- sq[[countPrecise]] + theme(legend.position='none')		
		# fut[[countPrecise]] <- fut[[countPrecise]] + theme(legend.position='none')		
			
		
	# } # next number of precise occurrences
	
	# plots <- plot_grid(
		# sq[[1]], fut[[1]],
		# sq[[2]], fut[[2]],
		# sq[[3]], fut[[3]],
		# sq[[4]], fut[[4]],
		# sq[[5]], fut[[5]],
		# sq[[6]], fut[[6]],
		# nrow=6
	# )
	# legend <- plot_grid(legend)
	
	# main <- plot_grid(plots, legend, labels=NULL, nrow=2, rel_heights=c(1, 0.05))

	# print(main)
	
	# ggsave(paste0('./Analysis/Virtual Species/!ENMs Suitable Area All Results.pdf'), width=7.5, height=10, units='in')
		
say('DONE!', deco='&', level=1)

