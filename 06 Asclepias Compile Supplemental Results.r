### CALCULATE UNIVARIATE NICHE BREADTH
### David Henderson 2019 | Adam B. Smith 2020
### 
### source('E:/Ecology/Drive/Research Active/ENMs - Vaguely Georeferenced Specimen Records/enms_impreciseRecords/06 Asclepias Compile Supplemental Results.r')

### CONTENTS ###
### setup ###
### univariate niche breadth ###
### multivariate niche breadth ###
### climate change exposure ###

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
	library(scales)

	# custom (Adam Smith)
	library(omnibus)

# say('################################')
# say('### univariate niche breadth ###')
# say('################################')

	# data <- read.csv('./Analysis/Univariate Niche Breadth/!Univariate Niche Breadth.csv')
	# n <- length(unique(data$species))

	# data$species <- gsub(data$species, pattern='Asclepias', replacement='A.')
	# numRecs <- data$numAccs
	# names(numRecs) <- data$species
	# numRecs <- sort(numRecs, FALSE)
	# data$species <- factor(data$species, levels=names(numRecs))

	# types <- c('closest', 'mean', 'centroid', 'farthest')
	
	# accCol <- 'nicheBreathBio1_acc'
	# levels <- c('nicheBreathBio1_closest', 'nicheBreadthBio1_mean', 'nicheBreadthBio1_centroid', 'nicheBreadthBio1_farthest')
	# variable <- 'MAT'
	# names(levels) <- types

	# mat <- data.frame(
		# species = rep(data$species, length(levels)),
		# type = rep(types, each=n),
		# breadth = (c(data[ , levels[1]], data[ , levels[2]], data[ , levels[3]], data[ , levels[4]]) - data[ , accCol]) / data[ , accCol],
		# var = variable
	# )
	
	# accCol <- 'nicheBreathBio12_acc'
	# levels <- c('nicheBreathBio12_closest', 'nicheBreadthBio12_mean', 'nicheBreadthBio12_centroid', 'nicheBreadthBio12_farthest')
	# variable <- 'MAP'
	# names(levels) <- types

	# map <- data.frame(
		# species = rep(data$species, length(levels)),
		# type = rep(types, each=n),
		# breadth = (c(data[ , levels[1]], data[ , levels[2]], data[ , levels[3]], data[ , levels[4]]) - data[ , accCol]) / data[ , accCol] ,
		# var = variable
	# )
	
	# x <- rbind(mat, map)
	
	# x$type <- factor(x$type, levels=types)
	# x$var <- factor(x$var, levels=c('MAT', 'MAP'))
	
	# p <- ggplot(data=x, aes(x=species, y=breadth, group=type)) +
		# geom_point(aes(col=type, shape=type), size=2.2) +
		# scale_shape_manual(values=c('closest'=16, 'mean'=3, 'centroid'=5, 'farthest'=15)) +
		# scale_y_continuous(labels=scales::percent) +
		# labs(x=NULL, y=paste0('Increase in niche breadth (%)')) +
		# theme(axis.text.y=element_text(face='italic')) +
		# coord_flip() +
		# facet_grid(. ~ var)
		
	# print(p)
	
	# ggsave(paste0('./Analysis/Univariate Niche Breadth/!Increase in Univariate Niche Breadth.pdf'), width=8, height=9, units='in')
	# dev.off()

say('##################################')
say('### multivariate niche breadth ###')
say('##################################')

	data <- read.csv('./Analysis/Multivariate Niche Volume/!pcaVolume_df.csv')
	ancillary <- read.csv('./Analysis/Univariate Niche Breadth/!Univariate Niche Breadth.csv')
	data$species <- ancillary$species
	n <- length(unique(data$species))

	data$species <- gsub(data$species, pattern='Asclepias', replacement='A.')
	numRecs <- ancillary$numAccs
	names(numRecs) <- data$species
	numRecs <- sort(numRecs, FALSE)
	data$species <- factor(data$species, levels=names(numRecs))

	### niche volume

	types <- c('nearest_vol', 'mean_vol', 'centroid_vol', 'farthest_vol')
	accCol <- 'accurate_vol'
	
	levels <- c('nearest_vol', 'mean_vol', 'centroid_vol', 'farthest_vol')
	names(levels) <- types

	x <- data.frame(
		species = rep(data$species, length(levels)),
		type = rep(types, each=n),
		breadth = (c(data[ , levels[1]], data[ , levels[2]], data[ , levels[3]], data[ , levels[4]]) - data[ , accCol]) / data[ , accCol]
	)
	
	x$type <- factor(x$type, levels=types)
	
	p <- ggplot(data=x, aes(x=species, y=breadth, group=type)) +
		geom_point(aes(col=type, shape=type), size=2.2) +
		scale_shape_manual(labels=c('nearest_vol'='Closest', 'mean_vol'='Mean', 'centroid_vol'='Centroid', 'farthest_vol'='Farthest'), values=c('nearest_vol'=16, 'mean_vol'=5, 'centroid_vol'=3, 'farthest_vol'=1)) +
		scale_color_manual(labels=c('nearest_vol'='Closest', 'mean_vol'='Mean', 'centroid_vol'='Centroid', 'farthest_vol'='Farthest'), values=c('nearest_vol'='#e41a1c', 'mean_vol'='#377eb8', 'centroid_vol'='#4daf4a', 'farthest_vol'='#e41a1c')) +
		scale_y_continuous(labels=scales::percent, trans='log10') +
		labs(x=NULL, y=paste0('Increase in niche volume (%)')) +
		theme(axis.text.y=element_text(face='italic')) +
		coord_flip()
		
	print(p)
	
	ggsave(paste0('./Analysis/Multivariate Niche Volume/!Increase in Multivariate Niche Volume.pdf'), width=8, height=9, units='in')
	dev.off()

	### niche surface area

	types <- c('nearest_area', 'mean_area', 'centroid_area', 'farthest_area')
	accCol <- 'accurate_area'
	
	levels <- c('nearest_area', 'mean_area', 'centroid_area', 'farthest_area')
	names(levels) <- types

	x <- data.frame(
		species = rep(data$species, length(levels)),
		type = rep(types, each=n),
		breadth = (c(data[ , levels[1]], data[ , levels[2]], data[ , levels[3]], data[ , levels[4]]) - data[ , accCol]) / data[ , accCol]
	)
	
	x$type <- factor(x$type, levels=types)
	
	p <- ggplot(data=x, aes(x=species, y=breadth, group=type)) +
		geom_point(aes(col=type, shape=type), size=2.2) +
		scale_shape_manual(labels=c('nearest_area'='Closest', 'mean_area'='Mean', 'centroid_area'='Centroid', 'farthest_area'='Farthest'), values=c('nearest_area'=16, 'mean_area'=5, 'centroid_area'=3, 'farthest_area'=1)) +
		scale_color_manual(labels=c('nearest_area'='Closest', 'mean_area'='Mean', 'centroid_area'='Centroid', 'farthest_area'='Farthest'), values=c('nearest_area'='#e41a1c', 'mean_area'='#377eb8', 'centroid_area'='#4daf4a', 'farthest_area'='#e41a1c')) +
		scale_y_continuous(labels=scales::percent, trans='log10') +
		labs(x=NULL, y=paste0('Increase in niche surface area (%)')) +
		theme(axis.text.y=element_text(face='italic')) +
		coord_flip()
		
	print(p)
	
	ggsave(paste0('./Analysis/Multivariate Niche Volume/!Increase in Multivariate Niche Surface Area.pdf'), width=8, height=9, units='in')
	dev.off()

# say('###############################')
# say('### climate change exposure ###')
# say('###############################')

	# data <- read.csv('./Analysis/ENMs/!Climate Change Exposure - Areal Values for Current, Future, Stable, Gain, and Loss.csv')
	# n <- length(unique(data$species))

	# data$species <- gsub(data$species, pattern='Asclepias', replacement='A.')

	# ### within MCP of PRECISE records
	# ################################

		# ### PRESENT-DAY climatically suitable area within MCP of precise records
		# ########################################################################

			# col <- 'mcpAccs_currentArea_km2'

			# thisData <- data[data$assignMethod != 'accurate' & data$rcp==45, ]
			# thisData$accArea_km2 <- NA
			# for (i in 1:nrow(thisData)) {
				
				# accArea_km2 <- data[data$species == thisData$species[i] & data$rcp == thisData$rcp[i] & data$background == thisData$background[i] & data$assignMethod=='accurate', col]
				
				# thisData$accArea_km2[i] <- accArea_km2
				
			# }

			# backgrounds <- c('buffered records', 'buffered MCP')
			# names(backgrounds) <- c('buffers', 'convexHull')
			
			# thisData$assignMethod <- factor(thisData$assignMethod, levels=c('closest', 'mean', 'centroid', 'farthest'))

			# rng <- c(0, max(data[ , col]))

			# p <- ggplot(data=thisData, aes(x=accArea_km2, y=thisData[ , col])) +
				# geom_abline(slope=1, intercept=0) +
				# geom_point(size=2.1, shape=21, bg=alpha('gray40', 0.6)) +
				# labs(x=bquote('Precise only'~(km^2)), y=bquote('Precise & imprecise'~(km^2))) +
				# xlim(rng[1], rng[2]) + ylim(rng[1], rng[2]) +
				# facet_grid(background ~ assignMethod, labeller=labeller(background=backgrounds)) +
				# theme(aspect.ratio=1)

			# print(p)
			
			# ggsave(paste0('./Analysis/ENMs/!Change in PRESENT-DAY Climatically Suitable Area within MCP of Precise Records.pdf'), width=10, height=5, units='in')
			# dev.off()

		# ### STABLE climatically suitable area within MCP of precise records
		# ###################################################################

			# col <- 'mcpAccs_stableArea_km2'

			# thisData <- data[data$assignMethod != 'accurate', ]
			# thisData$accArea_km2 <- NA
			# for (i in 1:nrow(thisData)) {
				
				# accArea_km2 <- data[data$species == thisData$species[i] & data$rcp == thisData$rcp[i] & data$background == thisData$background[i] & data$assignMethod=='accurate', col]
				
				# thisData$accArea_km2[i] <- accArea_km2
				
			# }

			# rcps <- c('RCP 4.5', 'RCP 8.5')
			# names(rcps) <- c(45, 85)
			
			# backgrounds <- c('buffered records', 'buffered MCP')
			# names(backgrounds) <- c('buffers', 'convexHull')
			
			# thisData$assignMethod <- factor(thisData$assignMethod, levels=c('closest', 'mean', 'centroid', 'farthest'))

			# rng <- c(0, max(data[ , col]))

			# p <- ggplot(data=thisData, aes(x=accArea_km2, y=thisData[ , col])) +
				# geom_abline(slope=1, intercept=0) +
				# geom_point(size=2.1, shape=21, bg=alpha('cornflowerblue', 0.6)) +
				# labs(x=bquote('Precise only'~(km^2)), y=bquote('Precise & imprecise'~(km^2))) +
				# xlim(rng[1], rng[2]) + ylim(rng[1], rng[2]) +
				# facet_grid(rcp ~ assignMethod + background, labeller=labeller(rcp=rcps, background=backgrounds)) +
				# theme(aspect.ratio=1)

			# print(p)
			
			# ggsave(paste0('./Analysis/ENMs/!Change in STABLE Climatically Suitable Area within MCP of Precise Records.pdf'), width=10, height=5, units='in')
			# dev.off()

		# ### LOST climatically suitable area within MCP of precise records
		# #################################################################

			# col <- 'mcpAccs_lossArea_km2'

			# thisData <- data[data$assignMethod != 'accurate', ]
			# thisData$accArea_km2 <- NA
			# for (i in 1:nrow(thisData)) {
				
				# accArea_km2 <- data[data$species == thisData$species[i] & data$rcp == thisData$rcp[i] & data$background == thisData$background[i] & data$assignMethod=='accurate', col]
				
				# thisData$accArea_km2[i] <- accArea_km2
				
			# }

			# rcps <- c('RCP 4.5', 'RCP 8.5')
			# names(rcps) <- c(45, 85)
			
			# backgrounds <- c('buffered records', 'buffered MCP')
			# names(backgrounds) <- c('buffers', 'convexHull')
			
			# thisData$assignMethod <- factor(thisData$assignMethod, levels=c('closest', 'mean', 'centroid', 'farthest'))

			# rng <- c(0, max(data[ , col]))

			# p <- ggplot(data=thisData, aes(x=accArea_km2, y=thisData[ , col])) +
				# geom_abline(slope=1, intercept=0) +
				# geom_point(size=2.1, shape=21, bg=alpha('firebrick1', 0.6)) +
				# labs(x=bquote('Precise only'~(km^2)), y=bquote('Precise & imprecise'~(km^2))) +
				# xlim(rng[1], rng[2]) + ylim(rng[1], rng[2]) +
				# facet_grid(rcp ~ assignMethod + background, labeller=labeller(rcp=rcps, background=backgrounds)) +
				# theme(aspect.ratio=1)

			# print(p)
			
			# ggsave(paste0('./Analysis/ENMs/!Change in LOST Climatically Suitable Area within MCP of Precise Records.pdf'), width=10, height=5, units='in')
			# dev.off()

		# ### GAIN climatically suitable area within MCP of precise records
		# #################################################################

			# col <- 'mcpAccs_gainArea_km2'

			# thisData <- data[data$assignMethod != 'accurate', ]
			# thisData$accArea_km2 <- NA
			# for (i in 1:nrow(thisData)) {
				
				# accArea_km2 <- data[data$species == thisData$species[i] & data$rcp == thisData$rcp[i] & data$background == thisData$background[i] & data$assignMethod=='accurate', col]
				
				# thisData$accArea_km2[i] <- accArea_km2
				
			# }

			# rcps <- c('RCP 4.5', 'RCP 8.5')
			# names(rcps) <- c(45, 85)
			
			# backgrounds <- c('buffered records', 'buffered MCP')
			# names(backgrounds) <- c('buffers', 'convexHull')
			
			# thisData$assignMethod <- factor(thisData$assignMethod, levels=c('closest', 'mean', 'centroid', 'farthest'))

			# rng <- c(0, max(data[ , col]))

			# p <- ggplot(data=thisData, aes(x=accArea_km2, y=thisData[ , col])) +
				# geom_abline(slope=1, intercept=0) +
				# geom_point(size=2.1, shape=21, bg=alpha('chartreuse', 0.6)) +
				# labs(x=bquote('Precise only'~(km^2)), y=bquote('Precise & imprecise'~(km^2))) +
				# xlim(rng[1], rng[2]) + ylim(rng[1], rng[2]) +
				# facet_grid(rcp ~ assignMethod + background, labeller=labeller(rcp=rcps, background=backgrounds)) +
				# theme(aspect.ratio=1)

			# print(p)
			
			# ggsave(paste0('./Analysis/ENMs/!Change in GAIN Climatically Suitable Area within MCP of Precise Records.pdf'), width=10, height=5, units='in')
			# dev.off()

	# ### within MCP of PRECISE & IMPRECISE records
	# #############################################

		# ### PRESENT-DAY climatically suitable area within MCP of PRECISE & IMPRECISE records
		# ####################################################################################

			# col <- 'mcpAccsInaccsAdmin_currentArea_km2'

			# thisData <- data[data$assignMethod != 'accurate' & data$rcp==45, ]
			# thisData$accArea_km2 <- NA
			# for (i in 1:nrow(thisData)) {
				
				# accArea_km2 <- data[data$species == thisData$species[i] & data$rcp == thisData$rcp[i] & data$background == thisData$background[i] & data$assignMethod=='accurate', col]
				
				# thisData$accArea_km2[i] <- accArea_km2
				
			# }

			# backgrounds <- c('buffered records', 'buffered MCP')
			# names(backgrounds) <- c('buffers', 'convexHull')
			
			# thisData$assignMethod <- factor(thisData$assignMethod, levels=c('closest', 'mean', 'centroid', 'farthest'))

			# rng <- c(0, max(data[ , col]))

			# p <- ggplot(data=thisData, aes(x=accArea_km2, y=thisData[ , col])) +
				# geom_abline(slope=1, intercept=0) +
				# geom_point(size=2.1, shape=21, bg=alpha('gray40', 0.6)) +
				# labs(x=bquote('Precise only'~(km^2)), y=bquote('Precise & imprecise'~(km^2))) +
				# xlim(rng[1], rng[2]) + ylim(rng[1], rng[2]) +
				# facet_grid(background ~ assignMethod, labeller=labeller(background=backgrounds)) +
				# theme(aspect.ratio=1)

			# print(p)
			
			# ggsave(paste0('./Analysis/ENMs/!Change in PRESENT-DAY Climatically Suitable Area within MCP of Precise & Imprecise Records.pdf'), width=10, height=5, units='in')
			# dev.off()

		# ### STABLE climatically suitable area within MCP of PRECISE & IMPRECISE records
		# ###############################################################################

			# col <- 'mcpAccsInaccsAdmin_stableArea_km2'

			# thisData <- data[data$assignMethod != 'accurate', ]
			# thisData$accArea_km2 <- NA
			# for (i in 1:nrow(thisData)) {
				
				# accArea_km2 <- data[data$species == thisData$species[i] & data$rcp == thisData$rcp[i] & data$background == thisData$background[i] & data$assignMethod=='accurate', col]
				
				# thisData$accArea_km2[i] <- accArea_km2
				
			# }

			# rcps <- c('RCP 4.5', 'RCP 8.5')
			# names(rcps) <- c(45, 85)
			
			# backgrounds <- c('buffered records', 'buffered MCP')
			# names(backgrounds) <- c('buffers', 'convexHull')
			
			# thisData$assignMethod <- factor(thisData$assignMethod, levels=c('closest', 'mean', 'centroid', 'farthest'))

			# rng <- c(0, max(data[ , col]))

			# p <- ggplot(data=thisData, aes(x=accArea_km2, y=thisData[ , col])) +
				# geom_abline(slope=1, intercept=0) +
				# geom_point(size=2.1, shape=21, bg=alpha('cornflowerblue', 0.6)) +
				# labs(x=bquote('Precise only'~(km^2)), y=bquote('Precise & imprecise'~(km^2))) +
				# xlim(rng[1], rng[2]) + ylim(rng[1], rng[2]) +
				# facet_grid(rcp ~ assignMethod + background, labeller=labeller(rcp=rcps, background=backgrounds)) +
				# theme(aspect.ratio=1)

			# print(p)
			
			# ggsave(paste0('./Analysis/ENMs/!Change in STABLE Climatically Suitable Area within MCP of Precise & Imprecise Records.pdf'), width=10, height=5, units='in')
			# dev.off()

		# ### LOST climatically suitable area within MCP of PRECISE & IMPRECISE records
		# #############################################################################

			# col <- 'mcpAccsInaccsAdmin_lossArea_km2'

			# thisData <- data[data$assignMethod != 'accurate', ]
			# thisData$accArea_km2 <- NA
			# for (i in 1:nrow(thisData)) {
				
				# accArea_km2 <- data[data$species == thisData$species[i] & data$rcp == thisData$rcp[i] & data$background == thisData$background[i] & data$assignMethod=='accurate', col]
				
				# thisData$accArea_km2[i] <- accArea_km2
				
			# }

			# rcps <- c('RCP 4.5', 'RCP 8.5')
			# names(rcps) <- c(45, 85)
			
			# backgrounds <- c('buffered records', 'buffered MCP')
			# names(backgrounds) <- c('buffers', 'convexHull')
			
			# thisData$assignMethod <- factor(thisData$assignMethod, levels=c('closest', 'mean', 'centroid', 'farthest'))

			# rng <- c(0, max(data[ , col]))

			# p <- ggplot(data=thisData, aes(x=accArea_km2, y=thisData[ , col])) +
				# geom_abline(slope=1, intercept=0) +
				# geom_point(size=2.1, shape=21, bg=alpha('firebrick1', 0.6)) +
				# labs(x=bquote('Precise only'~(km^2)), y=bquote('Precise & imprecise'~(km^2))) +
				# xlim(rng[1], rng[2]) + ylim(rng[1], rng[2]) +
				# facet_grid(rcp ~ assignMethod + background, labeller=labeller(rcp=rcps, background=backgrounds)) +
				# theme(aspect.ratio=1)

			# print(p)
			
			# ggsave(paste0('./Analysis/ENMs/!Change in LOST Climatically Suitable Area within MCP of Precise & Imprecise Records.pdf'), width=10, height=5, units='in')
			# dev.off()

		# ### GAIN climatically suitable area within MCP of PRECISE & IMPRECISE records
		# #############################################################################

			# col <- 'mcpAccsInaccsAdmin_gainArea_km2'

			# thisData <- data[data$assignMethod != 'accurate', ]
			# thisData$accArea_km2 <- NA
			# for (i in 1:nrow(thisData)) {
				
				# accArea_km2 <- data[data$species == thisData$species[i] & data$rcp == thisData$rcp[i] & data$background == thisData$background[i] & data$assignMethod=='accurate', col]
				
				# thisData$accArea_km2[i] <- accArea_km2
				
			# }

			# rcps <- c('RCP 4.5', 'RCP 8.5')
			# names(rcps) <- c(45, 85)
			
			# backgrounds <- c('buffered records', 'buffered MCP')
			# names(backgrounds) <- c('buffers', 'convexHull')
			
			# thisData$assignMethod <- factor(thisData$assignMethod, levels=c('closest', 'mean', 'centroid', 'farthest'))

			# rng <- c(0, max(data[ , col]))

			# p <- ggplot(data=thisData, aes(x=accArea_km2, y=thisData[ , col])) +
				# geom_abline(slope=1, intercept=0) +
				# geom_point(size=2.1, shape=21, bg=alpha('chartreuse', 0.6)) +
				# labs(x=bquote('Precise only'~(km^2)), y=bquote('Precise & imprecise'~(km^2))) +
				# xlim(rng[1], rng[2]) + ylim(rng[1], rng[2]) +
				# facet_grid(rcp ~ assignMethod + background, labeller=labeller(rcp=rcps, background=backgrounds)) +
				# theme(aspect.ratio=1)

			# print(p)
			
			# ggsave(paste0('./Analysis/ENMs/!Change in GAIN Climatically Suitable Area within MCP of Precise & Imprecise Records.pdf'), width=10, height=5, units='in')
			# dev.off()

say('DONE!', deco='&', level=1)

