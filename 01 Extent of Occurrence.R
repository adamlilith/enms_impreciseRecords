### ANALYSIS OF RANGE SIZE FOR "VAGUELY-GEOREFERENCED SPECIMENS" PROJECT
### David Henderson 2019 | Adam B. Smith 2020
### 
### source('C:/Ecology/Drive/Research/Vaguely Georeferenced Specimen Records/Code/01 Extent of Occurrence.r')
### source('D:/Ecology/Drive/Research/Vaguely Georeferenced Specimen Records/Code/01 Extent of Occurrence.r')
### source('E:/Ecology/Drive/Research/Vaguely Georeferenced Specimen Records/Code/01 Extent of Occurrence.r')

### CONTENTS ###
### setup ###
### calculate EOO of accurate vs accurate-plus-inaccurate records ###
### vulnerability analysis based on extent of occurrence ###
### plot range size results using base R ###
### plot range size results using ggplot2 ###

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

	### libraries
	library(dismo)
	library(rgeos)
	library(scales)

	# custom (Adam Smith)
	library(omnibus)
	library(enmSdm)
	library(legendary)
	# source('./Code/Functions for Calculating Range Size.r')

	### names

		llGbif <- c('decimalLongitude', 'decimalLatitude')

		# designations for accurate, inaccurate, and county records
		accAssigns <- 'certain/precise'
		inaccAssigns <- c('uncertain/precise', 'certain/imprecise', 'uncertain/imprecise')
		adminAssigns <- c('county/precise', 'county/imprecise', 'county-only', 'state/imprecise', 'state-only')
		countyAssigns <- c('county/precise', 'county/imprecise', 'county-only')
		stateAssigns <- c('state/imprecise', 'state-only')
		
# say('#####################################################################')
# say('### calculate EOO of accurate vs accurate-plus-inaccurate records ###')
# say('#####################################################################')
	
	# ### data objects
	
		# load('./Regions/GADM Ver 3pt6 North America Level 0.rda')
		# load('./Regions/GADM Ver 3pt6 North America Level 1.rda')
		# load('./Regions/GADM Ver 3pt6 North America Level 2.rda')
	
		# nam0SpEa <- sp::spTransform(nam0Sp, getCRS('albersNA', TRUE))
		# nam1SpEa <- sp::spTransform(nam1Sp, getCRS('albersNA', TRUE))
		# nam2SpEa <- sp::spTransform(nam2Sp, getCRS('albersNA', TRUE))
	
		# # state/county names
		# stateGeog <- tolower(paste0(nam1Sp$NAME_1))
		# stateCountyGeog <- tolower(paste0(nam2Sp$NAME_1, nam2Sp$NAME_2))
	
	# ### strategy: create spatial objects for each of accurate, inaccurate, county, and state records
	# ### calculate joint extent for plotting
	# ### calculate range area of accurate records using MCP
	# ### calculate range area of accurate + inaccurate + administrative records using MCP
	# ### make a map
	
	# load('./Data/Asclepias 03 Specimen Records and Environmental Data.rda')
	
	# species <- asclepias$meta$usableSpecies$species

	# stats <- data.frame() # for saving range area
	
	# # by species
	# for (countSpecies in seq_along(species)) {
	
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

		# # minimum convex polygon for ranges and range size
		# accRangeSpEa <- adehabitatHR::mcp(accsSpEa, percent=100, unin='m', unout='km2')
		# accRangeSpEa <- gIntersection(accRangeSpEa, thisNam0SpEa)
		# accRange_km2 <- areaFromPointsOrPoly(accRangeSpEa)

		# if (exists('inaccsAdminSpEa')) {
			# accInaccAdminRangeSpEa <- mcpFromPolygons(inaccsAdminSpEa, accsSpEa)
			# accInaccAdminRangeSpEa <- gIntersection(accInaccAdminRangeSpEa, thisNam0SpEa)
			# accInaccAdminRange_km2 <- areaFromPointsOrPoly(accInaccAdminRangeSpEa)
		# } else {
			# accInaccAdminRange_km2 <- accRange_km2
		# }
		
		# ### tallies
		# numAcc <- if (is.null(accs)) { 0 } else { nrow(accs) }
		# numInacc <- if (is.null(inaccs)) { 0 } else { nrow(inaccs) }
		# numCounty <- if (is.null(county)) { 0 } else { nrow(county) }
		# numState <- if (is.null(state)) { 0 } else { nrow(state) }
		
		# stats <- rbind(
			# stats,
			# data.frame(
				# species=thisSpecies,
				# numAccs=nrow(accs),
				# numInaccs=if (!is.null(inaccs)) { nrow(inaccs) } else { 0 },
				# numCounty=if (!is.null(county)) { nrow(county) } else { 0 },
				# numState=if (!is.null(state)) { nrow(state) } else { 0 },
				# accRange_km2=accRange_km2,
				# accInaccAdminRange_km2=accInaccAdminRange_km2,
				# rangeRatio = accInaccAdminRange_km2 / accRange_km2
			# )
		# )
		
		# ### plot
		# dirCreate('./Analysis/Extent of Occurrence')
		# png(paste0('./Analysis/Extent of Occurrence/', thisSpecies, '.png'), width=2 * 1333, height=2 * 833, res=450)
		
			# par(oma=c(0.5, 0.2, 2, 0.2), cex=0.2, cex.main=0.4, cex.sub=0.15)
		
			# plot(extSpEa, border=NA)

			# # plot(thisNam2SpEa, add=TRUE, border='gray', col='gray90')
			# plot(thisNam1SpEa, add=TRUE, border='gray', col='gray90', lwd=1)
			
			# if (!is.null(state)) plot(stateSpEa, col=alpha('#d95f02', 0.2), add=TRUE)
			# if (!is.null(county)) plot(countySpEa, col=alpha('#d95f02', 0.2), add=TRUE)
			# if (!is.null(inaccs)) plot(inaccsSpEa, col=alpha('#d95f02', 0.2), add=TRUE)
			
			# if (exists('inaccsAdminSpEa')) plot(accInaccAdminRangeSpEa, col=alpha('#d95f02', 0.1), border='#d95f02', lwd=1.8, add=TRUE)
			# plot(accRangeSpEa, col=alpha('#1b9e77', 0.25), border='#1b9e77', lwd=1.8, add=TRUE)
			
			# if (!is.null(state)) plot(stateSpEa, col=alpha('#d95f02', 0.2), add=TRUE)
			# if (!is.null(county)) plot(countySpEa, col=alpha('#d95f02', 0.2), add=TRUE)
			# if (!is.null(inaccs)) plot(inaccsSpEa, col=alpha('#d95f02', 0.2), add=TRUE)
			# points(accsSpEa, pch=21, col='black', bg=alpha('#1b9e77', 1), cex=2.5)

			# legend(
				# 'bottomright', inset=0.01,
				# legend=c('Precise record', 'Imprecise record', 'Precise MCP', 'Imprecise + precise MCP'),
				# col=c('black', NA, NA, NA),
				# lwd=c(NA, 0.5, 1.8, 1.8),
				# fill=c(NA, alpha('#d95f02', 0.4), alpha('#1b9e77', 0.25), alpha('#d95f02', 0.1)),
				# border=c(NA, 'black', '#1b9e77', '#d95f02'),
				# pch=c(21, NA, NA, NA),
				# pt.bg=c('#1b9e77', NA, NA, NA),
				# cex=2.8,
				# bty='n',
				# pt.cex=2.6
			# )
			
			# main <- paste0(thisSpecies, '\nPrecise: ', numAcc, ' | Inaccurate: ', numInacc, ' | County: ', numCounty, ' | State: ', numState)
			# title(main=main, cex.main=3, line=-1)
			# title(sub=date(), cex.sub=1.4, line=0.8)
			
		# dev.off()

	# } # next species

	# write.csv(stats, './Analysis/Extent of Occurrence/!Extent of Occurrence.csv', row.names=FALSE)
	
# say('############################################################')
# say('### vulnerability analysis based on extent of occurrence ###')
# say('############################################################')

	# say('What proportion of species would (partially) qualify as "vulnerable" under IUCN standards based on extent of occurrence with/out accurate and administrative records?', breaks=80)
	
	# rangeArea <- read.csv('./Analysis/Range Area.csv')
	
	# iucnEoo_km2 <- 100 # km2
	# say('Number of species with EOO <', iucnEoo_km2, ': ', sum(rangeArea$accRange_km2 < iucnEoo_km2), ' when using just accurate records')
	# say('Number of species with EOO <', iucnEoo_km2, ': ', sum(rangeArea$accInaccAdminRange_km2 < iucnEoo_km2), ' when using accurate + vague records')
	
	
say('DONE! ', date(), level=1, deco='%')
