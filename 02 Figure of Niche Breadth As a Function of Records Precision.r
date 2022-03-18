### NICHE BREADTH FIGURE
### This script creates a figure demonstrating the difference in niche breadth calculated using precise vs precise + imprecise records
### Adam B. Smith | Missouri Botanical Garden | 2020-04
###
### source('C:/Ecology/Drive/Research Active/Vaguely Georeferenced Specimen Records/enms_impreciseRecords/02 Figure of Niche Breadth As a Function of Records Precision.r')
### source('E:/Ecology/Drive/Research Active/Vaguely Georeferenced Specimen Records/enms_impreciseRecords/02 Figure of Niche Breadth As a Function of Records Precision.r')

### CONTENTS ###
### setup ###
### create figure of niche breadth as a function of record precision ###

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
	library(scales)
	library(omnibus) # from GitHub: adamlilith/omnibus

########################################################################
### create figure of niche breadth as a function of record precision ###
########################################################################

	### user-specified values
	
	# quantiles to define niche
	lowerQuant <- 0.025
	upperQuant <- 0.975

	# colors of imprecise records climate distributions
	# fill <- alpha('gray', 0.5) # fill color for polygons
	fill <- alpha('#d95f02', 0.2) # fill color for polygons
	# border <- 'gray30'
	border <- alpha('#d95f02', 0.4)
	
	# color of precise markers
	# preciseCol <- 'chartreuse'
	preciseCol <- '#1b9e77'
	
	# position of niche width indicators
	preciseNicheWidthMarkerY <- 1.05
	preciseNicheWidthMarkerTextY <- 0.92
	allNicheWidthMarkerY <- 1.3
	allNicheWidthMarkerTextY <- 1.17
	
	### start
	# load species/environmental data
	load('./Data/Asclepias 03 Specimen Records and Environmental Data.rda')

	speciesList <- asclepias$meta$usableSpecies$species
	
	dirCreate('./Analysis/Niche Width by Species')
	
	# for each species
	for (species in speciesList) {
	# for (species in 'Asclepias viridis') {

		say(species)
	
		# get data
		this <- asclepias$bySpecies[[tolower(gsub(species, pattern=' ', replacement='_'))]]

		png(paste0('./Analysis/Univariate Niche Breadth/', species, '.png'), width=1600, height=700, res=600)
		
		par(mfrow=c(1, 2), mgp=c(0, 0, 0), oma=c(0.8, 0.6, 1.4, 0.2), mar=rep(0.2, 4), lwd=0.6, tck=-0.02, cex.axis=0.3)

		# for each variable
		for (countVar in 1:2) {
		
			var <- c('bio1', 'bio12')[countVar]
			
			varNice <- c(
				paste('Mean annual temperature (', '\U00B0', 'C)', collapse='', sep=''),
				paste('Mean annual precipitation (mm)')
			)[countVar]
			
			units <- c(
				paste('\U00B0', 'C', collapse='', sep=''),
				'mm'
			)[countVar]
			
			# multiplied by variable values
			rescale <- c(1, 1)[countVar]
			
			# get plot range
			allVarVals <- this$accurateUsableNoDupEnv[ , var]
			for (i in seq_along(this$inaccurateAdminUsableNoDupEnv)) allVarVals <- c(allVarVals, this$inaccurateAdminUsableNoDupEnv[[i]][ , var])
			allVarVals <- rescale * allVarVals
			names(allVarVals) <- NULL
			
			minVal <- min(allVarVals, na.rm=TRUE)
			maxVal <- max(allVarVals, na.rm=TRUE)
			
			preciseMean <- rescale * mean(this$accurateUsableNoDupEnv[ , var], na.rm=TRUE)
			
			# plot
			pretties <- pretty(c(minVal, maxVal))
			plot(0, 0, col='white', bty='n', xaxt='n', yaxt='n', xlim=range(pretties), ylim=c(0, 1), xlab='', ylab='')
			axis(1, at=pretty(c(minVal, maxVal)), lwd=0.6, xpd=NA, line=0, labels=FALSE)
			
			# x-axis labels/values
			text(pretties, y=-0.1, labels=pretties, xpd=NA, cex=0.22)
			text(mean(pretties), y=-0.23, labels=varNice, xpd=NA, cex=0.28)
			
			# y-axis labels
			x <- min(pretties) - 0.07 * (max(pretties) - min(pretties))
			if (var == 'bio1') text(x, y=0.5, labels='Relative density\n(imprecise records)', srt=90, cex=0.28, xpd=NA)
			
			# calculate rescaling value for imprecise density smooths
			densMax <- -Inf
			for (countImprecise in seq_along(this$inaccurateAdminUsableNoDupEnv)) {
				vals <- rescale * this$inaccurateAdminUsableNoDupEnv[[countImprecise]][ , var]
				w <- this$inaccurateAdminUsableNoDupEnv[[countImprecise]][ , 'weight']

				valsWeights <- naOmitMulti(vals, w)
				vals <- valsWeights[[1]]
				w <- valsWeights[[2]]
				w <- w / sum(w)
					
				if (length(vals) > 1) {

					dens <- density(vals, adjust=0.5, weights=w, na.rm=TRUE)
					densMax <- max(densMax, dens$y)
					
				}
				
			}
			
			# plot imprecise values
			for (countImprecise in seq_along(this$inaccurateAdminUsableNoDupEnv)) {

				vals <- rescale * this$inaccurateAdminUsableNoDupEnv[[countImprecise]][ , var]
				
				if (all(!is.na(vals))) {
					
					w <- this$inaccurateAdminUsableNoDupEnv[[countImprecise]][ , 'weight']

					valsWeights <- naOmitMulti(vals, w)
					vals <- valsWeights[[1]]
					w <- valsWeights[[2]]
					w <- w / sum(w)

					if (length(vals) > 1) {

						dens <- density(vals, adjust=0.5, weights=w, na.rm=TRUE)
						x <- c(dens$x[1], dens$x, dens$x[length(dens$x)], dens$x[1])
						y <- c(0, dens$y / densMax, 0, 0)
						polygon(x=x, y=y, col=fill, border=border, lwd=0.4)
						
					} else {
					
						lines(c(vals, vals), c(0, 0.1), col=border, lwd=2, lend=0)
						
					}
					
				}
				
			}
			
			# niche width of precise records
			x <- rescale * this$accurateUsableNoDupEnv[ , var]
			preciseNicheWidth <- range(x, na.rm=TRUE)
			
			# niche width of imprecise records
			allNicheWidth <- preciseNicheWidth
			for (i in seq_along(this$inaccurateAdminUsableNoDupEnv)) {
			
				x <- rescale * this$inaccurateAdminUsableNoDupEnv[[i]][ , var]
			
				if (!all(is.na(x))) {
				
					closestImprecise <- x[which.min(abs(x - preciseMean))]
					
					if (closestImprecise < allNicheWidth[1]) allNicheWidth[1] <- closestImprecise
					if (closestImprecise > allNicheWidth[2]) allNicheWidth[2] <- closestImprecise
					
				}
			
			}
			
			# niche width indicators
			lines(c(preciseNicheWidth[1], preciseNicheWidth[1]), y=c(0, preciseNicheWidthMarkerY), lty='dotted', xpd=NA)
			lines(c(preciseNicheWidth[2], preciseNicheWidth[2]), y=c(0, preciseNicheWidthMarkerY), lty='dotted', xpd=NA)
			
			lines(c(allNicheWidth[1], allNicheWidth[1]), y=c(0, allNicheWidthMarkerY), lty='dotted', xpd=NA)
			lines(c(allNicheWidth[2], allNicheWidth[2]), y=c(0, allNicheWidthMarkerY), lty='dotted', xpd=NA)

			arrows(x0=preciseNicheWidth[1], x1=preciseNicheWidth[2], y0=preciseNicheWidthMarkerY, y1=preciseNicheWidthMarkerY, angle=15, length=0.03, code=3, xpd=NA)
			arrows(x0=allNicheWidth[1], x1=allNicheWidth[2], y0=allNicheWidthMarkerY, y1=allNicheWidthMarkerY, angle=15, length=0.03, code=3, xpd=NA)
			
			preciseWidth <- diff(preciseNicheWidth)
			allWidth <- diff(allNicheWidth)
			
			preciseWidth <- sprintf('%.1f', preciseWidth)
			allWidth <- sprintf('%.1f', allWidth)
			
			text(mean(preciseNicheWidth), preciseNicheWidthMarkerTextY, labels=paste0('Precise records (', preciseWidth, ' ', units, ')'), xpd=NA, pos=3, cex=0.22)
			text(mean(allNicheWidth), allNicheWidthMarkerTextY, labels=paste0('All records (', allWidth, ' ', units, ')'), xpd=NA, pos=3, cex=0.22)
			
			title(sub=date(), cex.sub=0.1, line=-0.02, outer=TRUE)
			title(main=species, cex.main=0.3, line=1.1, outer=TRUE)
			
			# plot precise values
			x <- rescale * this$accurateUsableNoDupEnv[ , var]
			points(x, rep(0, length(x)), pch=21, bg=preciseCol, cex=0.3)
			
		} # next BIOCLIM variable
		
		dev.off()

	} # next species

say('DONE!', level=1, deco='%')
