### ANALYSIS OF RANGE SIZE FOR "VAGUELY-GEOREFERENCED SPECIMENS" PROJECT
### David Henderson 2019 | Adam B. Smith 2020
### 
### source('C:/Ecology/Drive/Research/ENMs - Vaguely Georeferenced Specimen Records/enms_impreciseRecords/10 Example of Methods Using Asclepias.r')
### source('D:/Ecology/Drive/Research/ENMs - Vaguely Georeferenced Specimen Records/enms_impreciseRecords/10 Example of Methods Using Asclepias.r')
### source('E:/Ecology/Drive/Research/ENMs - Vaguely Georeferenced Specimen Records/enms_impreciseRecords/10 Example of Methods Using Asclepias.r')

### CONTENTS ###
### setup ###
### EXAMPLE: plot inaccurate and accurate records in geographic space ###
### EXAMPLE: plot inaccurate and accurate records in environmental space ###

#############
### setup ###
#############

	cat(date(), '\n'); flush.console()
	rm(list=ls())
	gc()
	options(stringsAsFactors=FALSE)
	
	# drive <- 'C:'
	# drive <- 'D:'
	drive <- 'E:'
	
	setwd(paste0(drive, '/Ecology/Drive/Research/ENMs - Vaguely Georeferenced Specimen Records'))

	### libraries
	library(dismo)
	library(enmSdm)
	library(omnibus)
	library(rgeos)
	library(raster)
	library(scales)
	library(sf)
	library(sp)

	# custom (Adam Smith)
	ff <- listFiles(paste0('/Ecology/Drive/R/enmSdmX/R'), pattern='.r')
	for (f in ff) source (f)

	### names

		llGbif <- c('decimalLongitude', 'decimalLatitude')

		# designations for accurate, inaccurate, and county records
		accAssigns <- 'certain/precise'
		inaccAssigns <- c('uncertain/precise', 'certain/imprecise', 'uncertain/imprecise')
		adminAssigns <- c('county/precise', 'county/imprecise', 'county-only', 'state/imprecise', 'state-only')
		countyAssigns <- c('county/precise', 'county/imprecise', 'county-only')
		stateAssigns <- c('state/imprecise', 'state-only')
	
# say('#########################################################################')
# say('### EXAMPLE: plot inaccurate and accurate records in geographic space ###')
# say('#########################################################################')

	# # focalSpecies <- 'Asclepias amplexicaulis'
	# focalSpecies <- 'Asclepias brachystephana'
	
	# ### data objects
	
		# load('./Regions/GADM Ver 3pt6 North America Level 0 Albers.rda')
		# load('./Regions/GADM Ver 3pt6 North America Level 1 Albers.rda')
		# load('./Regions/GADM Ver 3pt6 North America Level 2 Albers.rda')
	
		# # state/county names
		# stateGeog <- tolower(paste0(nam1SpEa$NAME_1))
		# stateCountyGeog <- tolower(paste0(nam2SpEa$NAME_1, nam2SpEa$NAME_2))
	
	# ### strategy: create spatial objects for each of accurate, inaccurate, county, and state records
	# ### calculate joint extent for plotting
	# ### calculate range area of accurate records using MCP
	# ### calculate range area of accurate + inaccurate + administrative records using MCP
	# ### make a map
	
	# load('./Data/Asclepias 03 Specimen Records and Environmental Data.rda')
	
	# species <- asclepias$meta$usableSpecies$species

		# countSpecies <- which(species == focalSpecies)
	
		# thisSpecies <- species[countSpecies]
		# say(thisSpecies)
		
		# if (exists('inaccsAdminSpEa')) rm(inaccsAdminSpEa)
		
		# # accurate records
		# accs <- asclepias$bySpecies[[countSpecies]]$accurateUsableNoDupRecs[ , llGbif]
		# accsSp <- sp::SpatialPoints(accs[ , llGbif], getCRS('wgs84', TRUE))
		# accsSpEa <- sp::spTransform(accsSp, getCRS('albersNA', TRUE))
		
		# ext <- extent(accsSpEa)
		
		# # inaccurate records
		# index <- which(asclepias$bySpecies[[countSpecies]]$inaccurateAdminUsableNoDupRecs$recordType %in% inaccAssigns)
		# if (length(index) > 0) {
		
			# inaccs <- asclepias$bySpecies[[countSpecies]]$inaccurateAdminUsableNoDupRecs[index, ]
			# inaccsSp <- SpatialPointsDataFrame(inaccs[ , llGbif], data=inaccs, proj4string=getCRS('wgs84', TRUE))
			# inaccsSpEa <- sp::spTransform(inaccsSp, getCRS('albersNA', TRUE))
			# inaccsSpEa <- gBuffer(inaccsSpEa, width=inaccsSpEa$maxCoordUncerPrecision_m, byid=TRUE, quadsegs=12)
			# inaccsSpEa <- as(inaccsSpEa, 'SpatialPolygons')
			# projection(inaccsSpEa) <- getCRS('albersNA')
			
			# ext <- merge(ext, extent(inaccsSpEa))
			# inaccsAdminSpEa <- inaccsSpEa
			
		# } else {
		
			# inaccs <- NULL
			
		# }
		
		# # county records
		# index <- which(asclepias$bySpecies[[countSpecies]]$inaccurateAdminUsableNoDupRecs$recordType %in% countyAssigns)
		# if (length(index) > 0) {
		
			# county <- asclepias$bySpecies[[countSpecies]]$inaccurateAdminUsableNoDupRecs[index, ]
			# stateCountyRecs <- tolower(paste0(county$stateFromGeog, county$countyFromGeog))
			# countySpEa <- nam2SpEa[which(stateCountyGeog %in% stateCountyRecs), ]
			# countySpEa <- as(countySpEa, 'SpatialPolygons')
			# projection(countySpEa) <- getCRS('albersNA')
			
			# ext <- merge(ext, extent(countySpEa))
			# inaccsAdminSpEa <- if (exists('inaccsAdminSpEa')) {
				# rbind(inaccsAdminSpEa, countySpEa, makeUniqueIDs = TRUE)
			# } else {
				# countySpEa
			# }
			
		# } else {
		
			# county <- NULL
			
		# }
		
		# # state records
		# index <- which(asclepias$bySpecies[[countSpecies]]$inaccurateAdminUsableNoDupRecs$recordType %in% stateAssigns)
		# if (length(index) > 0) {
		
			# state <- asclepias$bySpecies[[countSpecies]]$inaccurateAdminUsableNoDupRecs[index, ]
			# stateRecs <- tolower(state$stateFromGeog)
			# stateSpEa <- nam1SpEa[stateGeog %in% stateRecs, ]
			# stateSpEa <- as(stateSpEa, 'SpatialPolygons')
			# projection(stateSpEa) <- getCRS('albersNA')
			
			# inaccsAdminSpEa <- if (exists('inaccsAdminSpEa')) {
				# rbind(inaccsAdminSpEa, stateSpEa, makeUniqueIDs = TRUE)
			# } else {
				# stateSpEa
			# }
			
			# ext <- merge(ext, extent(stateSpEa))
			
		# } else {
		
			# state <- NULL
			
		# }
		
		# # extent
		# extSpEa <- as(ext, 'SpatialPolygons')
		# projection(extSpEa) <- getCRS('albersNA')
		# extSpEa <- gBuffer(extSpEa, width=50000)
		
		# # crop basemaps to extent
		# thisNam0SpEa <- crop(nam0SpEa, extent(extSpEa))
		# thisNam1SpEa <- crop(nam1SpEa, extent(extSpEa))
		# # thisNam2SpEa <- crop(nam2SpEa, extent(extSpEa))

		# # # minimum convex polygon for ranges and range size
		# # accRangeSpEa <- adehabitatHR::mcp(accsSpEa, percent=100, unin='m', unout='km2')
		# # accRangeSpEa <- gIntersection(accRangeSpEa, thisNam0SpEa)
		# # accRange_km2 <- areaFromPointsOrPoly(accRangeSpEa)

		# # if (exists('inaccsAdminSpEa')) {
			# # accInaccAdminRangeSpEa <- mcpFromPolygons(inaccsAdminSpEa, accsSpEa)
			# # accInaccAdminRangeSpEa <- gIntersection(accInaccAdminRangeSpEa, thisNam0SpEa)
			# # accInaccAdminRange_km2 <- areaFromPointsOrPoly(accInaccAdminRangeSpEa)
		# # } else {
			# # accInaccAdminRange_km2 <- accRange_km2
		# # }
		
		# ### tallies
		# numAcc <- if (is.null(accs)) { 0 } else { nrow(accs) }
		# numInacc <- if (is.null(inaccs)) { 0 } else { nrow(inaccs) }
		# numCounty <- if (is.null(county)) { 0 } else { nrow(county) }
		# numState <- if (is.null(state)) { 0 } else { nrow(state) }
		
		# ### convert all vectors to sf!
		# ##############################
		
		# accsSpEa <- st_as_sf(accsSpEa)
		# inaccsAdminSpEa <- st_as_sf(inaccsAdminSpEa)
		
		# accsSpEa <- sf::st_union(accsSpEa)
		
		# ### plot imprecise & precise records
		# ####################################
		
		# width <- 2500
		# height <- 3000
		
		# stateBorder <- 'gray10'
		# stateFill <- 'gray90'
		# preciseCex <- 9
		# impreciseCex <- 6.5
		
		# oma <- rep(0, 4) #c(0.5, 0.2, 2, 0.2)
		
		# # dirCreate('./Analysis/')
		# # png(paste0('./Analysis/Example in Geographic Space for ', thisSpecies, ' of Precise & Imprecise Records.png'), width=2 * 1333, height=2 * 833, res=450)
		
			# # par(oma=oma, cex=0.2, cex.main=0.4, cex.sub=0.15)
		
			# # plot(extSpEa, border=NA)
			# # plot(thisNam1SpEa, add=TRUE, border=stateBorder, col=stateFill, lwd=1)
			# # plot(inaccsAdminSpEa, col=alpha('#d95f02', 0.1), border='#d95f02', lwd=1.8, add=TRUE)
			# # plot(accsSpEa, pch=21, col='black', bg=alpha('#1b9e77', 1), cex=preciseCex, add=TRUE)

			# # main <- paste0(thisSpecies, '\nPrecise: ', numAcc, ' | Inaccurate: ', numInacc, ' | County: ', numCounty, ' | State: ', numState)
			# # title(main=main, cex.main=3, line=-1)
			# # title(sub=date(), cex.sub=1.4, line=0.8)
			
		# # dev.off()

		# ### plot precise records & centroid
		# ###################################
		
		# dirCreate('./Analysis/')
		# png(paste0('./Analysis/Example in Geographic Space for ', thisSpecies, ' of Precise Records & Centroid.png'), width=width, height=height, res=450)
		
			# par(oma=oma, cex=0.2, cex.main=0.4, cex.sub=0.15)
		
			# plot(extSpEa, border=NA)
			# plot(thisNam1SpEa, add=TRUE, border=stateBorder, col=stateFill, lwd=1)
			# # plot(inaccsAdminSpEa, col=alpha('#d95f02', 0.1), border='#d95f02', lwd=1.8, add=TRUE)
			# plot(accsSpEa, pch=21, col='black', bg=alpha('#1b9e77', 1), cex=preciseCex, add=TRUE)
		
			# center <- st_centroid(accsSpEa)
			# plot(center, pch=8, col='black', cex=1.8 * preciseCex, add=TRUE)

		# dev.off()

		# ### plot imprecise records & closest points to centroid
		# #######################################################
		
		# png(paste0('./Analysis/Example in Geographic Space for ', thisSpecies, ' of Imprecise Records & Centroid.png'), width=width, height=height, res=450)
		
			# par(oma=oma, cex=0.2, cex.main=0.4, cex.sub=0.15)
		
			# plot(extSpEa, border=NA)
			# plot(thisNam1SpEa, add=TRUE, border=stateBorder, col=stateFill, lwd=1)
			# plot(inaccsAdminSpEa, col=alpha('#d95f02', 0.4), border='#d95f02', lwd=1.8, add=TRUE)
			# # plot(accsSpEa, pch=21, col='black', bg=alpha('#1b9e77', 1), cex=4.5, add=TRUE)
		
			# center <- st_centroid(accsSpEa)
			# plot(center, pch=8, col='black', cex=1.8 * preciseCex, add=TRUE)

			# mcpPointsVectEa <- mcpFromPointsPolys(pts=center, polys=inaccsAdminSpEa, return = 'polyPoints', terra=FALSE)
			# plot(mcpPointsVectEa, pch=16, cex=impreciseCex, add=TRUE)
			
		# dev.off()

		# ### plot MCP
		# ############

		# png(paste0('./Analysis/Example in Geographic Space for ', thisSpecies, ' of MCP.png'), width=width, height=height, res=450)
		
			# par(oma=oma, cex=0.2, cex.main=0.4, cex.sub=0.15)
		
			# plot(extSpEa, border=NA)
			# plot(thisNam1SpEa, add=TRUE, border=stateBorder, col=stateFill, lwd=1)
			# plot(inaccsAdminSpEa, col=alpha('#d95f02', 0.4), border='#d95f02', lwd=1.8, add=TRUE)
		
			# center <- st_centroid(accsSpEa)
			# # plot(center, pch=8, col='black', cex=1.8 * , add=TRUE)

			# mcpPointsVectEa <- mcpFromPointsPolys(pts=accsSpEa, polys=inaccsAdminSpEa, terra=FALSE)
			# plot(mcpPointsVectEa, col=alpha('#7570b3', 0.4), border='#7570b3', add=TRUE)

			# mcpPointsVectEa <- mcpFromPointsPolys(pts=center, polys=inaccsAdminSpEa, return = 'polyPoints', terra=FALSE)
			# plot(mcpPointsVectEa, pch=16, cex=impreciseCex, add=TRUE)
			# plot(accsSpEa, pch=21, col='black', bg=alpha('#1b9e77', 1), cex=preciseCex, add=TRUE)

		# dev.off()

say('############################################################################')
say('### EXAMPLE: plot inaccurate and accurate records in environmental space ###')
say('############################################################################')

	# focalSpecies <- 'Asclepias amplexicaulis'
	focalSpecies <- 'Asclepias brachystephana'
	
	
	pcs <- c('pc1', 'pc2')
	
	load('./Data/Asclepias 03 Specimen Records and Environmental Data.rda')
	
	species <- asclepias$meta$usableSpecies$species

	countSpecies <- which(species == focalSpecies)
	thisSpecies <- species[countSpecies]
	say(thisSpecies)
	
	# accurate records
	accs <- asclepias$bySpecies[[countSpecies]]$accurateUsableNoDupEnv[ , pcs]
	inaccs <- asclepias$bySpecies[[countSpecies]]$inaccurateAdminUsableNoDupEnv
	
	limsPc1 <- range(accs$pc1)
	limsPc2 <- range(accs$pc2)
	
	for (i in seq_along(inaccs)) {
		limsPc1[1] <- min(limsPc1[1], inaccs[[i]][ , 'pc1'])
		limsPc2[1] <- min(limsPc2[1], inaccs[[i]][ , 'pc2'])
 	
		limsPc1[2] <- max(limsPc1[2], inaccs[[i]][ , 'pc1'])
		limsPc2[2] <- max(limsPc2[2], inaccs[[i]][ , 'pc2'])
 	
	}

	### plot settings
	#################

	width <- 2500
	height <- 3000
	
	preciseCex <- 2
	impreciseCex <- 1.3
	cexLabel <- 2
	cexAxis <- 1.2
	
	oma <- rep(0, 4) #c(0.5, 0.2, 2, 0.2)
	mar <- c(4, 5, 0.5, 0.5) 
		
	centroid <- c(mean(accs$pc1), mean(accs$pc2))

	### plot accurate points in environmental space
	###############################################

		png(paste0('./Analysis/Example in 2D Environmental Space for ', thisSpecies, ' with Precise Points & Centroid.png'), width=width, height=height, res=450)
			
			par(oma=oma, mar=mar, cex.lab=cexLabel, cex.axis=cexAxis)
			
			plot(accs, xlim=limsPc1, pch=21, bg='#1b9e77', cex=preciseCex, ylim=limsPc2, xlab='PC1', ylab='PC2')
			points(centroid[1], centroid[2], pch=8, cex=1.8 * preciseCex)
			
		dev.off()

	### plot imprecise points in environmental space
	################################################
	
		png(paste0('./Analysis/Example in 2D Environmental Space for ', thisSpecies, ' with Imprecise Points.png'), width=width, height=height, res=450)
			
			par(oma=oma, mar=mar, cex.lab=cexLabel, cex.axis=cexAxis)
			
			plot(accs, xlim=limsPc1, col=NA, ylim=limsPc2, xlab='PC1', ylab='PC2')
			for (i in seq_along(inaccs)) {
				points(inaccs[[i]][ , pcs], pch=18, col='#d95f02', cex=impreciseCex)
			}
			
			for (i in seq_along(inaccs)) {
				thisInaccs <- inaccs[[i]][ , pcs]
				closest <- which.min(sqrt((centroid[1] - thisInaccs[ , 'pc1'])^2 + (centroid[2] - thisInaccs[ , 'pc2'])^2))
				closestPoint <- c(thisInaccs[closest, 'pc1'], thisInaccs[closest, 'pc2'])
				points(closestPoint[1], closestPoint[2], pch=18, cex=1)
				closestPoint <- matrix(closestPoint, ncol=2, dimnames=list(1, pcs))
			}
			
			points(centroid[1], centroid[2], pch=8, cex=1.8 * preciseCex)
			
		dev.off()

	### plot MCPs in environmental space
	####################################
	
		allPoints <- accs

		png(paste0('./Analysis/Example in 2D Environmental Space for ', thisSpecies, ' with MCPs.png'), width=width, height=height, res=450)
			
			par(oma=oma, mar=mar, cex.lab=cexLabel, cex.axis=cexAxis)
			
			plot(accs, xlim=limsPc1, pch=21, bg='#1b9e77', cex=preciseCex, ylim=limsPc2, xlab='PC1', ylab='PC2')
			for (i in seq_along(inaccs)) {
				points(inaccs[[i]][ , pcs], pch=18, col='#d95f02', cex=impreciseCex)
			}
			
			for (i in seq_along(inaccs)) {
				thisInaccs <- inaccs[[i]][ , pcs]
				closest <- which.min(sqrt((centroid[1] - thisInaccs[ , 'pc1'])^2 + (centroid[2] - thisInaccs[ , 'pc2'])^2))
				closestPoint <- c(thisInaccs[closest, 'pc1'], thisInaccs[closest, 'pc2'])
				points(closestPoint[1], closestPoint[2], pch=18, cex=1)
				closestPoint <- matrix(closestPoint, ncol=2, dimnames=list(1, pcs))
				allPoints <- rbind(allPoints, rbind(closestPoint))
			}
			
			# MCP of all points
			mcp <- chull(allPoints)
			x <- allPoints[mcp, 'pc1']
			y <- allPoints[mcp, 'pc2']
			polygon(x, y, col=alpha('#7570b3', 0.4), border='#7570b3')
			
			# # MCP of accurate
			# mcp <- chull(accs)
			# x <- allPoints[mcp, 'pc1']
			# y <- allPoints[mcp, 'pc2']
			# polygon(x, y, col=alpha('#1b9e77', 0.4), border='#1b9e77')
			
			points(accs, pch=21, bg='#1b9e77', cex=preciseCex)
			
		dev.off()


say('DONE! ', date(), level=1, deco='%')
