#' Simulate and model a virtual species with precise plus imprecise records
#'
#' Output is a data frame with statistics comparing models trained on all occurrences (no error in georeferencing), the subset that are "observed" and precisely georeferenced, and precise plus imprecise records.

simAndModelSpecies <- function() {

	say('species ', countSpecies, ' ', numPrecise, '/', numInacc, '/', numAdmin, '/', numDuplicateAdmins, ' | rep ', rep, ' | ', date(), post=0)

	### generate niche
	##################
	
		say('| generating niche', post=0)
	
		validRast <- FALSE

		while (!validRast) {

			# modeling pre-niche with multivariate normal
			nicheCenter <- c(NA, NA, NA)
			while (any(is.na(nicheCenter))) {
				nicheCenter <- unlist(sqPcaDfConusMexNoNA[sample(1:nrow(sqPcaDfConusMexNoNA), 1, prob=sqPcaDfConusMexNoNA$area_km2), , drop=TRUE])
			}
			
			nicheCenter <- nicheCenter[1:3]
			
			var1 <- rgamma(1, nicheShape, nicheRate)
			var2 <- rgamma(1, nicheShape, nicheRate)
			var3 <- rgamma(1, nicheShape, nicheRate)

			corr1v2 <- runif(1, -1, 1)
			corr1v3 <- runif(1, -1, 1)
			corr2v3 <- runif(1, -1, 1)
			
			corr <- matrix(
				c(var1, corr1v2, corr1v3,
				corr1v2, var2, corr2v3,
				corr1v3, corr2v3, var3),
				nrow=3, byrow=TRUE
			)
			
			nonNa <- which(complete.cases(pcaDf))
			densities <- dmvnorm(pcaDfNoNAMat, mean=nicheCenter, sigma=corr)
			densities <- densities / max(densities)
			
			densitiesSum <- sum(densities)
			highSuitDensities <- sum(densities >= minViableSuit)
			validRast <- !is.na(densitiesSum) & densitiesSum > 0 & highSuitDensities >= numPrecise + numInacc + numDuplicateAdmins

		} # keep trying to construct valid niche
		
		## burn densities to raster
			
		nicheRast <- mask * NA
		densitiesForRast <- rep(NA, ncell(nicheRast))
		densitiesForRast[nonNa] <- densities
		nicheRast <- setValues(nicheRast, values=densitiesForRast)
		nicheMask <- nicheRast >= minViableSuit
		nicheMask <- calc(nicheMask, fun=function(x) ifelse(x == 1, 1, NA))
		nicheRastMasked <- nicheRast * nicheMask
		names(nicheRast) <- names(nicheRastMasked) <- 'niche'
		
		par(mfrow=c(1, 2))
		pts <- sampleRast(nicheRast, cellStats(nicheMask, 'sum'))
		pts <- SpatialPoints(pts, proj4string=getCRS('wgs84', TRUE))
		plot(nam1Sp[!(nam1Sp$NAME_1 %in% c('Alaska', 'Hawaii')) & nam1Sp$NAME_0 %in% c('United States', 'Mexico'), ], col='gray', border='gray', main='niche')
		plot(nicheRastMasked, xpd=NA, legend=FALSE, add=TRUE)

		# plot(pts, col=NA, main='niche')
		# plot(nicheRast, add=TRUE)
		
	### locate "errorless" records
	##############################
	
		say('| locating records', post=0)
	
		# use same number of records as real species
		
		# precise + inaccurate record coordinates
		preciseRecs <- randomPoints(nicheRastMasked, numPrecise, prob=TRUE)
		preciseRecs <- as.data.frame(preciseRecs)
		names(preciseRecs) <- c('longitude', 'latitude')
		if (numInacc  > 0) {
			inaccRecs <- randomPoints(nicheRastMasked, numInacc, prob=TRUE)
			inaccRecs <- as.data.frame(inaccRecs)
			names(inaccRecs) <- c('longitude', 'latitude')
		} else {
			inaccRecs <- data.frame()
		}
		
		# administrative records
		if (numAdmin == 0) {
			adminErrorless <- data.frame()
		} else {
		
			### locate administrative records
			# sample random site in proportion to niche suitability
			# is this a county that has not yet been included? if so, retain site and county
		
			# get candidate errorless administrative points
			adminsErrorlessCandidates <- sampleRast(nicheRastMasked, 100 * numAdmin, replace=TRUE, prob=TRUE)
			adminsErrorlessCandidates <- as.data.frame(adminsErrorlessCandidates)
			names(adminsErrorlessCandidates) <- c('longitude', 'latitude')
		
			# extract state/county of these points
			stateCountyCandidates <- extract(nam2Sp, adminsErrorlessCandidates)
			stateCountyCandidates <- stateCountyCandidates[ , c('NAME_1', 'NAME_2', 'nam2area_km2')]
			names(stateCountyCandidates) <- c('state', 'county', 'nam2area_km2')
			stateCountyCandidates$ID <- 1:nrow(stateCountyCandidates)

			# get the state/county
			stateCountyCandidates <- cbind(stateCountyCandidates, adminsErrorlessCandidates)
			stateCountyCandidatesStateCounty <- stateCountyCandidates[!duplicated(stateCountyCandidates[ , c('state', 'county')]), c('state', 'county')]
			stateCountyCandidates <- stateCountyCandidates[stateCountyCandidates$nam2area_km2 <= maxArea_km2, ]
			
			adminStateCounty <- stateCountyCandidatesStateCounty[1:numAdmin, , drop=FALSE]
			adminErrorless <- stateCountyCandidates[stateCountyCandidates$state %in% adminStateCounty$state & stateCountyCandidates$county %in% adminStateCounty$county, c('state', 'county', 'longitude', 'latitude', 'ID')]
			
			if (nrow(stateCountyCandidatesStateCounty) < numAdmin) {
				say('')
				stop('Too onerous to locate administrative records. Try again.')
			} else {
				
				### remove errorless points until the ratio of them to number of counties is same as number of precise per county
				
				obsAdminRecsPerCounty <- numDuplicateAdmins / numAdmin
				
				# get ratio of errorless administratives to number of counties
				adminErrorlessPerCounty <- nrow(adminErrorless) / nrow(adminStateCounty)
				
				while (adminErrorlessPerCounty > obsAdminRecsPerCounty & nrow(adminErrorless) >= numAdmin) {
				
					dupAdminErrorless <- duplicated(adminErrorless[ , c('state', 'county')])
					ID <- sample(adminErrorless$ID[dupAdminErrorless], 1)
					
					adminErrorless <- adminErrorless[adminErrorless$ID != ID, , drop=FALSE]
					adminErrorlessPerCounty <- nrow(adminErrorless) / nrow(adminStateCounty)

				}
				
				adminErrorless <- adminErrorless[ , c('longitude', 'latitude')]
				
			}
			
		} # if any administrative records
				
		errorlessRecs <- rbind(preciseRecs, inaccRecs, adminErrorless)
		numErrorless <- nrow(errorlessRecs)
		
	### MCPs for calibration region and comparison
	##############################################
		
		say('| MCPs', post=0)

		### errorless buffered region
		errorlessSp <- SpatialPoints(errorlessRecs, proj4string=getCRS('wgs84', TRUE))
		errorlessSpEa <- sp::spTransform(errorlessSp, getCRS('albersNA', TRUE))
		errorlessMcpSpEa <- adehabitatHR::mcp(errorlessSpEa, 100, unin='m', unout='km2')
		errorlessBuffSpEa <- gBuffer(errorlessMcpSpEa, width=300 * 1000)
		errorlessBuffSp <- sp::spTransform(errorlessBuffSpEa, getCRS('wgs84', TRUE))
		
		errorlessMcpBuffMask <- rasterize(errorlessBuffSp, sqPcaRasts)
		
		errorlessMaskedSqPcaRasts <- errorlessMcpBuffMask * sqPcaRasts
		names(errorlessMaskedSqPcaRasts) <- names(sqPcaRasts)
		
		errorlessMaskedFutPcaRasts <- errorlessMcpBuffMask * futPcaRasts
		names(errorlessMaskedFutPcaRasts) <- names(sqPcaRasts)
		
		### precise buffered region
		preciseSp <- SpatialPoints(preciseRecs, proj4string=getCRS('wgs84', TRUE))
		preciseSpEa <- sp::spTransform(preciseSp, getCRS('albersNA', TRUE))
		preciseMcpSpEa <- adehabitatHR::mcp(preciseSpEa, 100, unin='m', unout='km2')
		preciseBuffSpEa <- gBuffer(preciseMcpSpEa, width=300 * 1000)
		preciseBuffSp <- sp::spTransform(preciseBuffSpEa, getCRS('wgs84', TRUE))
		
		preciseMcpBuffMask <- rasterize(preciseBuffSp, sqPcaRasts)
		
		preciseMaskedSqPcaRasts <- preciseMcpBuffMask * sqPcaRasts
		names(preciseMaskedSqPcaRasts) <- names(sqPcaRasts)
		
	### get environment for occurrences and background sites
	########################################################
	
		say('| env', post=0)

		### errorless
		errorlessEnv <- extract(sqPcaRasts, errorlessRecs)

		### precise observed records
		preciseRecs <- errorlessRecs[1:numPrecise, ]
		preciseEnv <- errorlessEnv[1:numPrecise, ]
		preciseMeanEnv <- apply(preciseEnv, 2, mean)
		
		### inaccurate records (records with coordinate uncertainty)
		inaccEnv <- data.frame()
		if (numInacc > 0) {
		
			inaccSp <- SpatialPoints(inaccRecs, proj4string=getCRS('wgs84', TRUE))
			inaccSpEa <- sp::spTransform(inaccSp, getCRS('albersNA', TRUE))
			
			inaccRadii_m <- sample(allInaccRadii_m, numInacc)
			inaccBuffSpEa <- gBuffer(inaccSpEa, width=inaccRadii_m, byid=TRUE)
			inaccBuffSp <- sp::spTransform(inaccBuffSpEa, getCRS('wgs84', TRUE))
			
			inaccEnvAllCells <- extract(sqPcaRasts, inaccBuffSp)
		
			# environment of cell closest to mean of precise records
			inaccEnv <- data.frame()
			for (countInacc in 1:numInacc) {
				closestIndex <- which.min(sqrt(rowSums((preciseMeanEnv - inaccEnvAllCells[[countInacc]])^2)))
				inaccEnv <- rbind(
					inaccEnv,
					inaccEnvAllCells[[countInacc]][closestIndex, ]
				)
			}
			
			names(inaccEnv) <- names(sqPcaRasts)
			
		}
		
		# remember polygons
		if (numInacc > 0) vagueSpEa <- inaccBuffSpEa
		
		### environment of  administrative records
		adminEnv <- data.frame()
		if (numAdmin > 0) {
			for (countAdmin in 1:numAdmin) {

				# environment of cell closest to mean of precise records
				this <- which(adminStateCounty$state[countAdmin] == nam2Sp$NAME_1 & adminStateCounty$county[countAdmin] == nam2Sp$NAME_2)
				thisCountyBio <- envCounty[[this]]
				thisCountyPca <- predict(pca, thisCountyBio)[ , paste0('PC', 1:3), drop=FALSE]
				
				closestIndex <- which.min(sqrt(rowSums((preciseMeanEnv - thisCountyPca)^2)))
				adminEnv <- rbind(
					adminEnv,
					thisCountyPca[closestIndex, ]
				)
			} # next county
			
			names(adminEnv) <- names(sqPcaRasts)

			# remember polygons
			adminsSpEa <- nam2SpEa[nam2SpEa$NAME_1 %in% adminStateCounty$state[1] & nam2SpEa$NAME_2 %in% adminStateCounty$county[1], ]
		
			if (nrow(adminStateCounty) > 1) {
				for (i in 2:nrow(adminStateCounty)) {
					adminsSpEa <- rbind(
						adminsSpEa,
						nam2SpEa[nam2SpEa$NAME_1 %in% adminStateCounty$state[i] & nam2SpEa$NAME_2 %in% adminStateCounty$county[i], ]
					)
				}
			}
		
			adminsSpEa <- as(adminsSpEa, 'SpatialPolygons')
			
			vagueSpEa <- if (numInacc > 0) {
				rbind(vagueSpEa, adminsSpEa, makeUniqueIDs = TRUE)
			} else {
				adminsSpEa
			}

		}
		
	### background sites
	####################

		say('| bg', post=0)

		### errorless
		errorlessBgSites <- sampleRast(errorlessMaskedSqPcaRasts, 10000, prob=FALSE)
		errorlessBgEnv <- extract(errorlessMaskedSqPcaRasts, errorlessBgSites)

		### precise
		preciseBgSites <- sampleRast(preciseMaskedSqPcaRasts, 10000, prob=FALSE)
		preciseBgEnv <- extract(preciseMaskedSqPcaRasts, preciseBgSites)

		### precise + imprecise
		vagueMcpSpEa <- mcpFromPolygons(vagueSpEa, pts=preciseSpEa)
		vagueBuffSpEa <- gBuffer(vagueMcpSpEa, width=300 * 1000)
		vagueBuffSp <- sp::spTransform(vagueBuffSpEa, getCRS('wgs84', TRUE))
		
		vagueMcpBuffMask <- rasterize(vagueBuffSp, sqPcaRasts)
		
		vagueMaskedSqPcaRasts <- vagueMcpBuffMask * sqPcaRasts
		names(vagueMaskedSqPcaRasts) <- names(sqPcaRasts)
		
		vagueBgSites <- sampleRast(vagueMaskedSqPcaRasts, 10000, prob=FALSE)
		vagueBgEnv <- extract(vagueMaskedSqPcaRasts, vagueBgSites)

	### plot
	########
	
		plot(vagueBuffSpEa, border='orange')
		plot(vagueSpEa, border='orange', add=TRUE)
		plot(errorlessBuffSpEa, add=TRUE)
		plot(preciseBuffSpEa, border='red', add=TRUE)
		if (numAdmin > 0) {
			adminErrorlessSp <- SpatialPoints(adminErrorless, proj4string=getCRS('wgs84', TRUE))
			adminErrorlessSpEa <- sp::spTransform(adminErrorlessSp, getCRS('albersNA', TRUE))
			points(adminErrorlessSpEa, pch=3, col='orange')
		}
			
		if (numInacc > 0) {
			points(inaccSpEa, pch=3, col='blue')
			plot(inaccBuffSpEa, border='blue', add=TRUE)
		}

		points(errorlessSpEa, pch='.')
		points(preciseSpEa, col='red')

		legend('topright', legend=c('errorlessRecs', 'precise', 'inaccurate', 'admin'), pch=c(16, 1, 3, 3), col=c('black', 'red', 'blue', 'orange'))
		
	### train models
	################
	
		say('| models', post=0)
	
		### errorless
		presBg <- data.frame(presBg = c(rep(1, nrow(errorlessEnv)), rep(0, nrow(errorlessBgEnv))))		
		data <- rbind(errorlessEnv, errorlessBgEnv)
		data <- cbind(presBg, data)
		data <- as.data.frame(data)
		
		modelErrorless <- trainMaxEnt(data, classes='lpq', regMult=regMult, jackknife=FALSE, scratchDir=scratchDir)
		
		### precise records
		presBg <- data.frame(presBg=c(rep(1, numPrecise), rep(0, nrow(preciseBgEnv))))
		data <- rbind(preciseEnv, preciseBgEnv)
		data <- cbind(presBg, data)
	
		modelPrecise <- trainMaxEnt(data, classes='lpq', regMult=regMult, jackknife=FALSE, scratchDir=scratchDir)

		### precise + imprecise
		presBg <- data.frame(presBg = c(rep(1, numPrecise), rep(1, numInacc), rep(1, numAdmin), rep(0, nrow(vagueBgEnv))))
		vagueEnv <- rbind(preciseEnv, inaccEnv, adminEnv)
		data <- rbind(vagueEnv, vagueBgEnv)
		data <- cbind(presBg, data)
		
		modelVague <- trainMaxEnt(data, classes='lpq', regMult=regMult, jackknife=FALSE, scratchDir=scratchDir)
	
	### evaluate model calibrations
	###############################

	say('| calib', post=0)
	
		### "truth" and model predictions
		bgNiche <- extract(nicheRast, errorlessBgSites)
		errorlessPred <- predictEnmSdm(modelErrorless, errorlessBgEnv, maxentFun='enmSdm')
		precisePred <- predictEnmSdm(modelPrecise, errorlessBgEnv, maxentFun='enmSdm')
		vaguePred <- predictEnmSdm(modelVague, errorlessBgEnv, maxentFun='enmSdm')

		bgNicheTrans <- logitAdj(bgNiche, epsilon = 0.001)
		errorlessPredTrans <- logitAdj(errorlessPred, epsilon = 0.001) + runif(length(errorlessPred) -eps(), eps())
		precisePredTrans <- logitAdj(precisePred, epsilon = 0.001) + runif(length(precisePred) -eps(), eps())
		vaguePredTrans <- logitAdj(vaguePred, epsilon = 0.001) + runif(length(vaguePred) -eps(), eps())

		corTruthVsErrorless <- cor(bgNicheTrans, errorlessPredTrans)
		corTruthVsPrecise <- cor(bgNicheTrans, precisePredTrans)
		corTruthVsVague <- cor(bgNicheTrans, vaguePredTrans)
	
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

		say('| cc expos', post=0)

		# thresholds
		predPres_errorless <- predictEnmSdm(modelErrorless, errorlessEnv, maxentFun='enmSdm')
		predPres_precise <- predictEnmSdm(modelPrecise, preciseEnv, maxentFun='enmSdm')
		predPres_vague <- predictEnmSdm(modelVague, vagueEnv, maxentFun='enmSdm')

		thold_errorless <- quantile(predPres_errorless, 0.1)
		thold_precise <- quantile(predPres_precise, 0.1)
		thold_vague <- quantile(predPres_vague, 0.1)

		# prediction rasters
		sqPredRast_errorless <- predictEnmSdm(errorlessMaskedSqPcaRasts, modelErrorless, maxentFun='enmSdm')
		sqPredRast_precise <- predictEnmSdm(errorlessMaskedSqPcaRasts, modelPrecise, maxentFun='enmSdm')
		sqPredRast_vague <- predictEnmSdm(errorlessMaskedSqPcaRasts, modelVague, maxentFun='enmSdm')
		
		futPredRast_errorless <- predictEnmSdm(errorlessMaskedFutPcaRasts, modelErrorless, maxentFun='enmSdm')
		futPredRast_precise <- predictEnmSdm(errorlessMaskedFutPcaRasts, modelPrecise, maxentFun='enmSdm')
		futPredRast_vague <- predictEnmSdm(errorlessMaskedFutPcaRasts, modelVague, maxentFun='enmSdm')
		
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
	
		thisRemember <- data.frame(
			species = countSpecies,
			rep = rep,
			
			var1 = var1,
			var2 = var2,
			var3 = var3,
			corr1v2 = corr1v2,
			corr1v3 = corr1v3,
			corr2v3 = corr2v3,
			
			numNonDupRecords = numRecords,
			numPrecise = numPrecise,
			numInacc = numInacc,
			numAdmin = numAdmin,
			numErrorless = numErrorless,
			obsDuplicateAdminRecs = numDuplicateAdmins,
			
			corTruthVsErrorless = corTruthVsErrorless,
			corTruthVsPrecise = corTruthVsPrecise,
			corTruthVsVague = corTruthVsVague,
			
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
	
}

	