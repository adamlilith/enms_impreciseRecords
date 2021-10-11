### EFFECT OF INCLUDING IMPRECISE SPECIMENS ON CLIMATE CHANGE EXPOSURE - VIRTUAL SPECIES
### Adam B. Smith | Missouri Botanical Garden | adam.smith@mobot.org | 2021-10
### 
### source('E:/Ecology/Drive/Research/Vaguely Georeferenced Specimen Records/Code/07 Virtual Species - Completely Synthetic.r')

### CONTENTS ###
### setup ###
### generate simulated species ###

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
	library(dismo)
	library(ggplot2)
	library(mvtnorm)
	library(rgeos)
	library(scales)
	library(terra)
	
	# custom (Adam Smith)
	library(enmSdm)
	library(omnibus)
	library(statisfactory)
	
	source('E:/Ecology/Drive/R/enmSdm/R/predictEnmSdm.r')
	source('E:/Ecology/Drive/R/enmSdm/R/predictMaxEnt.r')
	
	
	ll <- c('longitude', 'latitude')

	# designations for accurate, inaccurate, and county records
	accAssigns <- 'certain/precise'
	inaccAssigns <- c('uncertain/precise', 'certain/imprecise', 'uncertain/imprecise')
	adminAssigns <- c('county/precise', 'county/imprecise', 'county-only', 'state/imprecise', 'state-only')
	countyAssigns <- c('county/precise', 'county/imprecise', 'county-only')
	stateAssigns <- c('state/imprecise', 'state-only')


### random draws from multivariate gamma distribution
#####################################################

	# from "lcmix" package at https://rdrr.io/rforge/lcmix/

	# x 	

	# a numeric matrix of which each row represents an observation.
	# shape 	

	# a vector of shape parameters for the marginal distributions of the columns of x. If length(shape) < ncol(x), the elements of shape will be recycled. If length(shape) > ncol(x), the shape vector will be truncated and a warning given.
	# rate 	

	# a vector of rate parameters for the marginal distributions of the columns of x. If length(rate) < ncol(x), the elements of rate will be recycled. If length(rate) > ncol(x), the rate vector will be truncated and a warning given.
	# corr 	

	# the correlation matrix. See Details.
	# log 	

	# logical; if TRUE, density is given as the log-density.
	# n 	

	# number of vectors to simulate.

	rmvgamma <- function(n, shape=1, rate=1, corr=diag(length(shape)))
	{
		## extract parameters, do sanity checks, deal with univariate case
		
		if(!is.matrix(corr) || !isSymmetric(corr))
			stop("'corr' must be a symmetric matrix")
		D = ncol(corr)
		
		Ds = length(shape)
		if(Ds > D)
			warning("'shape' longer than width of 'corr', truncating to fit")
		if(Ds != D)
			shape = rep(shape, length.out=D)
		
		Dr = length(rate)
		if(Dr > D)
			warning("'rate' longer than width of 'corr', truncating to fit")
		if(Dr != D)
			rate = rep(rate, length.out=D)
		
		if(D == 1) rgamma(n, shape, rate)
		
		## generate standard multivariate normal matrix, convert to CDF
		
		Z = rmvnorm(n, cov=corr)
		cdf = pnorm(Z)
		
		## convert to gamma, return
		
		sapply(1:D, function(d) qgamma(cdf[,d], shape[d], rate[d]))
	}


say('##################################')
say('### generate simulated species ###')
say('##################################')	

	### user-defined
	################
		
		# totalErrorless <- c(40, 160, 640, 5120) # 40, 160 done for 5, 10, 20, 30
		# totalErrorless <- totalErrorless[1]
		# numPrecise <- c(5, 6, 7, 8, 9, 10, 12, 14, 16, 18, 20, 30) # done for 40, 160 640 errorless
		
		totalErrorless <- c(40, 160, 640, 5120) # 40 DONE, remainder IP
		totalErrorless <- totalErrorless[4]
		# numPrecise <- 1 # DONE
		# numPrecise <- 2 # IP
		# numPrecise <- 3 # IP
		numPrecise <- 4 # IP
		
		repStart <- 1
		repEnd <- 100

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
		sqRasts <- stack(paste0('E:/Ecology/Climate/WorldClim/version_2_1/10_arcmin/historical/wc2.1_10m_bio_', 1:19, '.tif'))
		
		# future climate rasters
		futScenario <- 'wc2.1_10m_bioc_BCC-CSM2-MR_ssp245_2041-2060'
		futRasts <- stack(paste0('E:/Ecology/Climate/WorldClim/version_2_1/10_arcmin/cmip6_2041_2060_ssp245_BCC-CSM2-MR/', futScenario, '.tif'))
		
		
		rm(records, nam2SpEa); gc()

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
			
			rm(climDf, conusMex, sqRasts, conusMexMask, sqPcaDfConusMex, areaConusMexMask_km2, sqPcaDfNoNA); gc()

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

			rm(nam0Sp, futRasts, climDf, pcPrediction, futPcaDfNoNA); gc()
			
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
					
					# modeling pre-niche with multivariate normal
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
			##########################################
			
				say('| records', post=0)
			
				# precise
				preciseRecs <- randomPoints(sqNicheRast, thisNumPrecise, prob=TRUE)
				vagueErrorlessRecs <- sampleRast(sqNicheRast, thisNumErrorless - thisNumPrecise, prob=TRUE)
				names(preciseRecs) <- names(vagueErrorlessRecs) <- ll
				
				# administrative
				allCountiesAtVagueErrorless <- extract(nam2Sp, vagueErrorlessRecs)
				allCountiesAtVagueErrorless$point.ID <- NULL
				nondupCountiesAtVagueErrorless <- allCountiesAtVagueErrorless[!duplicated(allCountiesAtVagueErrorless), , drop=FALSE]
				
				# errorless
				errorlessRecs <- rbind(preciseRecs, vagueErrorlessRecs)
				errorlessRecs <- elimCellDups(errorlessRecs, sqNicheRast)
			
				say(nrow(errorlessRecs), ' errorless with ', nrow(nondupCountiesAtVagueErrorless), ' non-duplicate admins', post=0)
			
			### define calibration regions
			##############################
			
				say('| calib regions', post=0)
			
				# errorless
				errorlessRecsSp <- SpatialPoints(errorlessRecs[ , 1:2], proj4string=getCRS('wgs84', TRUE))
				errorlessRecsSpEa <- sp::spTransform(errorlessRecsSp, getCRS('albersNA', TRUE))
				errorlessMcpSpEa <- adehabitatHR::mcp(errorlessRecsSpEa, 100, unin='m', unout='km2')
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
				if (thisNumPrecise >= 5) {
					preciseMcpSpEa <- adehabitatHR::mcp(preciseRecsSpEa, 100, unin='m', unout='km2')
					preciseBuffSpEa <- gBuffer(preciseMcpSpEa, width=buffSize_km * 1000)
				} else {
					preciseMcpSpEa <- preciseBuffSpEa <- gBuffer(preciseRecsSpEa, width=buffSize_km * 1000)
				}
				
				preciseBuffSp <- sp::spTransform(preciseBuffSpEa, getCRS('wgs84', TRUE))
				preciseMcpBuffMask <- rasterize(preciseBuffSp, sqPcaRasts)
				
				preciseMaskedSqPcaRasts <- preciseMcpBuffMask * sqPcaRasts
				names(preciseMaskedSqPcaRasts) <- names(sqPcaRasts)
				
				# precise + admin
				adminSp <- nam2Sp[nam2Sp$NAME_1 == nondupCountiesAtVagueErrorless$NAME_1[1] & nam2Sp$NAME_2 == nondupCountiesAtVagueErrorless$NAME_2[1], ]
				if (nrow(nondupCountiesAtVagueErrorless) > 1) {
					for (i in 2:nrow(nondupCountiesAtVagueErrorless)) {
						adminSp <- rbind(
							adminSp,
							nam2Sp[nam2Sp$NAME_1 == nondupCountiesAtVagueErrorless$NAME_1[i] & nam2Sp$NAME_2 == nondupCountiesAtVagueErrorless$NAME_2[i], ]
						)
					}
				}
				
				adminSpEa <- sp::spTransform(adminSp, getCRS('albersNA', TRUE))
				
				vagueMcpSpEa <- mcpFromPolygons(adminSpEa, preciseRecsSpEa)
				vagueBuffSpEa <- gBuffer(vagueMcpSpEa, byid=FALSE, width=buffSize_km * 1000)
				vagueBuffSp <- sp::spTransform(vagueBuffSpEa, getCRS('wgs84', TRUE))
				
				vagueMcpBuffMask <- rasterize(vagueBuffSp, sqPcaRasts)
				
				vagueMaskedSqPcaRasts <- vagueMcpBuffMask * sqPcaRasts
				names(vagueMaskedSqPcaRasts) <- names(sqPcaRasts)
				
			### plot
			########
			
				par(oma=c(0, 0, 0, 0), mfrow=c(1, 2))

				# present
				cols <- colorRampPalette(c('gray80', 'green', 'forestgreen'))
				cols <- cols(10)
				main <- paste0('SQ ', thisNumErrorless, ' (', nrow(errorlessRecs), ') el ', thisNumPrecise, ' pr')
				plot(trim(errorlessMcpBuffMask), col=NA, legend=FALSE, main=main)
				plot(sqNicheRast, col=cols, legend=FALSE, add=TRUE)
				plot(adminSp, border='orange4', add=TRUE)
				points(errorlessRecsSp, pch='.')
				points(preciseRecsSp, col='red', pch='.')
				
				plot(vagueMcpSpEa, border='orange', add=TRUE)
				plot(errorlessBuffSp, add=TRUE)
				plot(preciseBuffSp, border='red', add=TRUE)
				plot(vagueBuffSp, border='orange4', add=TRUE)

				# future delta
				cols <- colorRampPalette(c('forestgreen', 'lightgreen', 'gray80', 'gray80', 'firebrick1', 'firebrick4'))
				cols <- cols(20)
				main <- paste0('fut delta')
				plot(trim(errorlessMcpBuffMask), col=NA, legend=FALSE, main=main)
				plot(sqNicheRast - futNicheRast, breaks=seq(-1, 1, by=0.1), legend=TRUE, add=TRUE)
				points(errorlessRecsSp, pch='.')
				points(preciseRecsSp, col='red', pch='.')
				
				plot(vagueMcpSpEa, border='orange', add=TRUE)
				plot(errorlessBuffSp, add=TRUE)
				plot(preciseBuffSp, border='red', add=TRUE)
				plot(vagueBuffSp, border='orange4', add=TRUE)

			### background sites
			#####################
				
				say('| bgs', post=0)
				
				# errorless
				bgInErrorlessBuffTrain <- randomPoints(errorlessMcpBuffMask, 10000)
				bgInErrorlessBuffEval <- randomPoints(errorlessMcpBuffMask, 10000)

				# precise
				bgInPreciseBuff <- if (thisNumPrecise < 5) {
					sampleRast(preciseMcpBuffMask, cellStats(preciseMcpBuffMask, 'sum'), replace=FALSE, prob=FALSE)
				} else {
					randomPoints(preciseMcpBuffMask, 10000)
				}
				
				# precise + vague
				bgInVagueBuff <- randomPoints(vagueMcpBuffMask, 10000)

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

				### precise + vague
				adminEnvExt <- extract(sqPcaRasts, adminSp)

				preciseMeanEnv <- colMeans(preciseRecsEnv)
				adminEnv <- data.frame()
				for (i in 1:nrow(adminSp)) {
					thisCountyEnv <- if (!is(adminEnvExt[[i]], 'matrix')) {
						matrix(adminEnvExt[[i]], nrow=1)
					} else {
						adminEnvExt[[i]]
					}
					closestIndex <- which.min(sqrt(rowSums((preciseMeanEnv - thisCountyEnv)^2)))
					adminEnv <- rbind(adminEnv, thisCountyEnv[closestIndex, , drop=FALSE])
				}
				
				names(adminEnv) <- names(sqPcaRasts)
				
				vagueRecsEnv <- rbind(preciseRecsEnv, adminEnv)
				vagueBgEnv <- extract(sqPcaRasts, bgInVagueBuff)
				vagueBgEnv <- vagueBgEnv[complete.cases(vagueBgEnv), , drop=FALSE]
				
				vagueData <- rbind(vagueRecsEnv, vagueBgEnv)
				presBg <- c(rep(1, nrow(preciseRecs) + nrow(adminSp)), rep(0, nrow(vagueBgEnv)))
				vagueData <- cbind(presBg, vagueData)
				vagueData <- as.data.frame(vagueData)

			### model
			#########
				
				say('| model', post=0)
				
				errorlessModel <- trainMaxNet(errorlessData, regMult=regMult, classes=maxEntClasses)
				preciseModel <- if (thisNumPrecise >= 5) {
					trainMaxNet(preciseData, regMult=regMult, classes=maxEntClasses)
				} else {
					trainMaxEnt(preciseData, regMult=regMult, classes=maxEntClasses)
				}
				vagueModel <- trainMaxNet(vagueData, regMult=regMult, classes=maxEntClasses)

			### predictions to evaluation background sites in buffered MCP around errorless records
			#######################################################################################
				
				say('| predict', post=0)
				
				errorlessPredToSq <- predictEnmSdm(errorlessModel, errorlessEvalBgSqEnv, maxentFun='enmSdm', type='cloglog')
				precisePredToSq <- predictEnmSdm(preciseModel, errorlessEvalBgSqEnv, maxentFun='enmSdm', type='cloglog')
				vaguePredToSq <- predictEnmSdm(vagueModel, errorlessEvalBgSqEnv, maxentFun='enmSdm', type='cloglog')

				errorlessPredToFut <- predictEnmSdm(errorlessModel, errorlessEvalBgFutEnv, maxentFun='enmSdm', type='cloglog')
				precisePredToFut <- predictEnmSdm(preciseModel, errorlessEvalBgFutEnv, maxentFun='enmSdm', type='cloglog')
				vaguePredToFut <- predictEnmSdm(vagueModel, errorlessEvalBgFutEnv, maxentFun='enmSdm', type='cloglog')

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
				vaguePredTrans <- logitAdj(vaguePredToSq, epsilon = 0.001) + runif(length(vaguePredToSq) -eps(), eps())

				# comparison
				corTruthVsErrorlessSq <- cor(bgNicheTrans, errorlessPredTrans)
				corTruthVsPreciseSq <- cor(bgNicheTrans, precisePredTrans)
				corTruthVsVagueSq <- cor(bgNicheTrans, vaguePredTrans)
			
				### vs future
				
				# truth
				bgNiche <- extract(futNicheRast, bgInErrorlessBuffEval)
				if (any(is.na(bgNiche))) bgNiche <- bgNiche[!is.na(bgNiche)]
				
				# estimates
				bgNicheTrans <- logitAdj(bgNiche, epsilon = 0.001)
				errorlessPredTrans <- logitAdj(errorlessPredToFut, epsilon = 0.001) + runif(length(errorlessPredToFut) -eps(), eps())
				precisePredTrans <- logitAdj(precisePredToFut, epsilon = 0.001) + runif(length(precisePredToFut) -eps(), eps())
				vaguePredTrans <- logitAdj(vaguePredToFut, epsilon = 0.001) + runif(length(vaguePredToFut) -eps(), eps())

				# comparison
				corTruthVsErrorlessFut <- cor(bgNicheTrans, errorlessPredTrans)
				corTruthVsPreciseFut <- cor(bgNicheTrans, precisePredTrans)
				corTruthVsVagueFut <- cor(bgNicheTrans, vaguePredTrans)
			
			### evaluate EOO
			################

				say('| eoo', post=0)

				### all precise
				eooErrorless_km2 <- gArea(errorlessMcpSpEa) / 1000^2

				### only precise
				eooPrecise_km2 <- gArea(preciseMcpSpEa) / 1000^2
			
				# precise + vague
				eooVague_km2 <- gArea(vagueMcpSpEa) / 1000^2
			
			### evaluate climate change exposure
			####################################

				say('| expos', post=0)

				# thresholds
				predPres_errorless <- predictEnmSdm(errorlessModel, errorlessRecsEnv, maxentFun='enmSdm', type='cloglog')
				predPres_precise <- predictEnmSdm(preciseModel, preciseRecsEnv, maxentFun='enmSdm', type='cloglog')
				predPres_vague <- predictEnmSdm(vagueModel, vagueRecsEnv, maxentFun='enmSdm', type='cloglog')

				thold_errorless <- quantile(predPres_errorless, 0.1)
				thold_precise <- quantile(predPres_precise, 0.1)
				thold_vague <- quantile(predPres_vague, 0.1)

				# prediction rasters
				sqPredRast_errorless <- predict(errorlessMaskedSqPcaRasts, errorlessModel, clamp=FALSE, type='cloglog')
				sqPredRast_precise <- predict(errorlessMaskedSqPcaRasts, preciseModel, clamp=FALSE, type='cloglog')
				sqPredRast_vague <- predict(errorlessMaskedSqPcaRasts, vagueModel, clamp=FALSE, type='cloglog')
				
				futPredRast_errorless <- predict(errorlessMaskedFutPcaRasts, errorlessModel, clamp=FALSE, type='cloglog')
				futPredRast_precise <- predict(errorlessMaskedFutPcaRasts, preciseModel, clamp=FALSE, type='cloglog')
				futPredRast_vague <- predict(errorlessMaskedFutPcaRasts, vagueModel, clamp=FALSE, type='cloglog')
				
				# threshold rasters
				sqPredRast_errorless <- sqPredRast_errorless >= thold_errorless
				sqPredRast_precise <- sqPredRast_precise >= thold_precise
				sqPredRast_vague <- sqPredRast_vague >= thold_vague
				
				futPredRast_errorless <- futPredRast_errorless >= thold_errorless
				futPredRast_precise <- futPredRast_precise >= thold_precise
				futPredRast_vague <- futPredRast_vague >= thold_vague

				# suitable area
				sqSuitArea_errorless_km2 <- cellStats(areaRast_km2 * sqPredRast_errorless, 'sum')
				sqSuitArea_precise_km2 <- cellStats(areaRast_km2 * sqPredRast_precise, 'sum')
				sqSuitArea_vague_km2 <- cellStats(areaRast_km2 * sqPredRast_vague, 'sum')
				
				futSuitArea_errorless_km2 <- cellStats(areaRast_km2 * futPredRast_errorless, 'sum')
				futSuitArea_precise_km2 <- cellStats(areaRast_km2 * futPredRast_precise, 'sum')
				futSuitArea_vague_km2 <- cellStats(areaRast_km2 * futPredRast_vague, 'sum')

				# stable suitable area
				stable_errorless <- futPredRast_errorless * sqPredRast_errorless * areaRast_km2
				stable_precise <- futPredRast_precise * sqPredRast_precise * areaRast_km2
				stable_vague <- futPredRast_vague * sqPredRast_vague * areaRast_km2
				
				stableArea_errorless_km2 <- cellStats(stable_errorless, 'sum')
				stableArea_precise_km2 <- cellStats(stable_precise, 'sum')
				stableArea_vague_km2 <- cellStats(stable_vague, 'sum')
				
				# gained/lost area
				deltaArea_errorless <- areaRast_km2 * (futPredRast_errorless - sqPredRast_errorless)
				deltaArea_precise <- areaRast_km2 * (futPredRast_precise - sqPredRast_precise)
				deltaArea_vague <- areaRast_km2 * (futPredRast_vague - sqPredRast_vague)
				

				gainFx <- function(x) ifelse(x > 0, x, NA)
				gainArea_errorless <- calc(deltaArea_errorless, fun=gainFx)
				gainArea_precise <- calc(deltaArea_precise, fun=gainFx)
				gainArea_vague <- calc(deltaArea_vague, fun=gainFx)
				
				gainArea_errorless_km2 <- cellStats(gainArea_errorless, 'sum')
				gainArea_precise_km2 <- cellStats(gainArea_precise, 'sum')
				gainArea_vague_km2 <- cellStats(gainArea_vague, 'sum')
				
				
				lostFx <- function(x) ifelse(x < 0, -1 * x, NA)
				lostArea_errorless <- calc(deltaArea_errorless, fun=lostFx)
				lostArea_precise <- calc(deltaArea_precise, fun=lostFx)
				lostArea_vague <- calc(deltaArea_vague, fun=lostFx)
				
				lostArea_errorless_km2 <- cellStats(lostArea_errorless, 'sum')
				lostArea_precise_km2 <- cellStats(lostArea_precise, 'sum')
				lostArea_vague_km2 <- cellStats(lostArea_vague, 'sum')
			
			### remember
			############
			
				say('| remember')
			
				thisRemember <- data.frame(

					rep = rep,
					futScenario = futScenario,

					numErrorless = thisNumErrorless,
					# numActualErrorless = nrow(errorlessRecs),
					numPrecise = thisNumPrecise,
					numVagueErrorless = thisNumErrorless - thisNumPrecise,
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
					
					corTruthVsErrorlessSq = corTruthVsErrorlessSq,
					corTruthVsPreciseSq = corTruthVsPreciseSq,
					corTruthVsVagueSq = corTruthVsVagueSq,
					
					corTruthVsErrorlessFut = corTruthVsErrorlessFut,
					corTruthVsPreciseFut = corTruthVsPreciseFut,
					corTruthVsVagueFut = corTruthVsVagueFut,
					
					eooErrorless_km2 = eooErrorless_km2,
					eooPrecise_km2 = eooPrecise_km2,
					eooVague_km2 = eooVague_km2,
					
					sqSuitArea_errorless_km2 = sqSuitArea_errorless_km2,
					sqSuitArea_precise_km2 = sqSuitArea_precise_km2,
					sqSuitArea_vague_km2 = sqSuitArea_vague_km2,
					
					futSuitArea_errorless_km2 = futSuitArea_errorless_km2,
					futSuitArea_precise_km2 = futSuitArea_precise_km2,
					futSuitArea_vague_km2 = futSuitArea_vague_km2,
					
					stableArea_errorless_km2 = stableArea_errorless_km2,
					stableArea_precise_km2 = stableArea_precise_km2,
					stableArea_vague_km2 = stableArea_vague_km2,
					
					gainArea_errorless_km2 = gainArea_errorless_km2,
					gainArea_precise_km2 = gainArea_precise_km2,
					gainArea_vague_km2 = gainArea_vague_km2,
					
					lostArea_errorless_km2 = lostArea_errorless_km2,
					lostArea_precise_km2 = lostArea_precise_km2,
					lostArea_vague_km2 = lostArea_vague_km2
					
				)
				
			thisRemember
			
		})
				
	

	### MAIN
	########
	
	for (thisNumErrorless in totalErrorless) {
	
		for (thisNumPrecise in numPrecise) {
	
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
					say(' SQ truth vs precise + vague ... ', round(mean(remember$corTruthVsVagueSq, na.rm=TRUE), 2), post=2)

					say(' Future truth vs errorless ......... ', round(mean(remember$corTruthVsErrorlessFut, na.rm=TRUE), 2))
					say(' Future truth vs precise ........... ', round(mean(remember$corTruthVsPreciseFut, na.rm=TRUE), 2))
					say(' Future truth vs precise + vague ... ', round(mean(remember$corTruthVsVagueFut, na.rm=TRUE), 2), post=2)
					
				}
	
				write.csv(remember, paste0('./Analysis/Virtual Species/Virtual Species Synthetic Results - Errorless ', thisNumErrorless, ' Precise ', thisNumPrecise, '.csv'), row.names=FALSE)

			} # next rep

			gc()

		} # next number of precise
	} # next number of errorless
		
say('#########################')
say('### exploratory plots ###')
say('#########################')

	### load results
	files <- listFiles('./Analysis/Virtual Species', pattern='.csv')
	results <- read.csv(files[1])
	if (length(files) > 1) {
		for (file in files[2:length(files)]) {
			results <- merge(
				results,
				read.csv(file),
				all=TRUE
			)
		}
	}

	results <- results[order(results$numErrorless), ]
	results <- results[order(results$numPrecise), ]
	results$case <- paste(results$numPrecise, 'p', results$numErrorless, 'el')


	### polygon plots
	# plot distribution of inner 95% of values as a polygon... x axis is number of precise, y is correlation coefficient
	
	# calculate inner x% bounds
	inner <- 0.9
	numErrorless <- 40
	
	thisResults <- results[results$numErrorless == numErrorless, ]
	numPrecise <- sort(unique(thisResults$numPrecise))
	quants <- c((1 - inner) / 2, 1 - (1 - inner) / 2)

	polyDf <- centerDf <- data.frame()
	
	for (position in c('lower', 'upper')) {
	
		if (position == 'lower') {
		
			thisNumPrecises <- numPrecise
			quant <- (1 - inner) / 2
			
		} else {
			
			thisNumPrecises <- rev(numPrecise)
			quant <- 1 - (1 - inner) / 2
			
		}
			
		for (thisNumPrecise in thisNumPrecises) {
		
			# polygon circumscribing inner x% of distributions of correlations
			polyDf <- rbind(
				polyDf,
				data.frame(
					numErrorless = numErrorless,
					numPrecise = thisNumPrecise,
					vs = rep(c('vsErrorless', 'vsPrecise', 'vsVague'), each=1),
					position = position,
					sq = c(
						quantile(thisResults$corTruthVsErrorlessSq[thisResults$numPrecise == thisNumPrecise], quant),
						quantile(thisResults$corTruthVsPreciseSq[thisResults$numPrecise == thisNumPrecise], quant),
						quantile(thisResults$corTruthVsVagueSq[thisResults$numPrecise == thisNumPrecise], quant)
					),
					fut = c(
						quantile(thisResults$corTruthVsErrorlessFut[thisResults$numPrecise == thisNumPrecise], quant),
						quantile(thisResults$corTruthVsPreciseFut[thisResults$numPrecise == thisNumPrecise], quant),
						quantile(thisResults$corTruthVsVagueFut[thisResults$numPrecise == thisNumPrecise], quant)
					)
				)
			)
			
			centerDf <- rbind(
				centerDf,
				data.frame(
					vs = c('vsErrorless', 'vsPrecise', 'vsVague'),
					sq = c(
						mean(thisResults$corTruthVsErrorlessSq[thisResults$numPrecise == thisNumPrecise]),
						mean(thisResults$corTruthVsPreciseSq[thisResults$numPrecise == thisNumPrecise]),
						mean(thisResults$corTruthVsVagueSq)
					),
					fut = c(
						mean(thisResults$corTruthVsErrorlessFut[thisResults$numPrecise == thisNumPrecise]),
						mean(thisResults$corTruthVsPreciseFut[thisResults$numPrecise == thisNumPrecise]),
						mean(thisResults$corTruthVsVagueFut[thisResults$numPrecise == thisNumPrecise])
					)
				)
			)
			
		} # next number of precise records
		
	} # next lower/upper

	ymin <- min(polyDf$sq, polyDf$fut)

	p <- ggplot(polyDf) +
		geom_polygon(data=polyDf[polyDf$vs=='vsErrorless', ], aes(x=numPrecise, y=sq), fill='white', color='black') +
		geom_polygon(data=polyDf[polyDf$vs=='vsVague', ], aes(x=numPrecise, y=sq), fill=alpha('darkorange3', 0.6), color='darkorange3', size=1) +
		geom_polygon(data=polyDf[polyDf$vs=='vsPrecise', ], aes(x=numPrecise, y=sq), fill=alpha('forestgreen', 0.5), color='forestgreen', size=1) +
		# scale_color_manual(values=c('black', 'vsPrecise' = '#1b9e77', 'vsVague' = '#d95f02')) +
		# scale_fill_manual(values=c('vsErrorless' = alpha('black', 0.2), 'vsPrecise' = alpha('#1b9e77', 0.8), 'vsVague' = alpha('#d95f02', 0.2))) +
		ylim(roundTo(ymin, 0.05, floor), 1) +
		xlab('Number of precise occurrences') + ylab('Correlation with truth') +
		ggtitle('Present')
		
		
	print(p)
	
	q <- ggplot(polyDf) +
		geom_polygon(data=polyDf[polyDf$vs=='vsErrorless', ], aes(x=numPrecise, y=fut), fill='white', color='black') +
		geom_polygon(data=polyDf[polyDf$vs=='vsVague', ], aes(x=numPrecise, y=fut), fill=alpha('darkorange3', 0.6), color='darkorange3', size=1) +
		geom_polygon(data=polyDf[polyDf$vs=='vsPrecise', ], aes(x=numPrecise, y=fut), fill=alpha('forestgreen', 0.5), color='forestgreen', size=1) +
		# scale_color_manual(values=c('black', 'vsPrecise' = '#1b9e77', 'vsVague' = '#d95f02')) +
		# scale_fill_manual(values=c('vsErrorless' = alpha('black', 0.2), 'vsPrecise' = alpha('#1b9e77', 0.8), 'vsVague' = alpha('#d95f02', 0.2))) +
		ylim(roundTo(ymin, 0.05, floor), 1) +
		xlab('Number of precise occurrences') + ylab('Correlation with truth') +
		ggtitle('Future')
		
		
	print(q)
	
# say('#########################################')
# say('### compare virtual with real species ###')
# say('#########################################')

	# # real species
	# real <- read.csv('./Analysis/Extent of Occurrence/!Extent of Occurrence.csv')
	
	# # virtual
	# files <- listFiles('./Analysis/Virtual Species/Means Drawn from Cells', pattern='.csv')
	# virt <- read.csv(files[1])
	# if (length(files) > 1) {
		# for (file in files[2:length(files)]) {
			# virt <- merge(
				# virt,
				# read.csv(file),
				# all=TRUE
			# )
		# }
	# }

	# df <- data.frame(
		# species = c(rep('real', nrow(real)), rep('virtual', nrow(virt))),
		# preciseEoo_km2 = c(real$accRange_km2, virt$eooPrecise_km2),
		# vagueEoo_km2 = c(real$accInaccAdminRange_km2, virt$eooVague_km2)
	# )
	
	# p <- ggplot(df, aes(x=log10(preciseEoo_km2), fill=species)) +
		# geom_histogram(mapping=stat(preciseEoo_km2 / sum(preciseEoo_km2))) +
		# scale_fill_manual(values=c(alpha('gray20', 0.4), alpha('darkblue', 0.4)))
	
	# print(p)
	

		
say('DONE!', deco='&', level=1)

