### CALCULATE UNIVARIATE NICHE BREADTH
### David Henderson 2019 | Adam B. Smith 2020
### 
### source('C:/Ecology/Drive/Research Active/Vaguely Georeferenced Specimen Records/Code/05 Summarize Results.r')
### source('E:/Ecology/Drive/Research Active/Vaguely Georeferenced Specimen Records/Code/05 Summarize Results.r')

### CONTENTS ###
### setup ###
### tally raw records ###
### tally records ###
### report statistics on change in EOO ###
### report statistics on univariate niche breadth ###
### report statistics on multivariate niche volume ###
### report metrics on change in climate change exposure ###
### plot increase in EOO and niche breadth as function of number of precise records using base R ###
### plot distribution of different types of occurrences across species ###
### statistical analyses of climate change exposure ###


	cat(date(), '\n'); flush.console()
	memory.limit(memory.limit() * 2^29)
	rm(list=ls())
	gc()
	options(stringsAsFactors=FALSE)
	
	# drive <- 'C:'
	# drive <- 'D:'
	drive <- 'E:'
	
	setwd(paste0(drive, '/Ecology/Drive/Research Active/Vaguely Georeferenced Specimen Records'))

	library(cowplot)
	library(ggplot2)
	
	# custom (Adam Smith)
	library(omnibus)

	### names

		llGbif <- c('decimalLongitude', 'decimalLatitude')

		# designations for accurate, inaccurate, and county records
		accAssigns <- 'certain/precise'
		inaccAssigns <- c('uncertain/precise', 'certain/imprecise', 'uncertain/imprecise')
		adminAssigns <- c('county/precise', 'county/imprecise', 'county-only', 'state/imprecise', 'state-only')
		countyAssigns <- c('county/precise', 'county/imprecise', 'county-only')
		stateAssigns <- c('state/imprecise', 'state-only')

# say('#########################')
# say('### tally raw records ###')
# say('#########################')
		
	# records <- fread('./Data/GBIF Asclepias 2019-11-18/occurrence.txt', header=TRUE)
	# records <- as.data.frame(records)

	# N <- nrow(records)

	# n1 <- nrow(records[is.na(records$decimalLongitude) & is.na(records$decimalLatitude) & !is.na(records$country) & !is.na(records$stateProvince) & !is.na(records$county), ])
	# n2 <- nrow(records[!is.na(records$decimalLongitude) & !is.na(records$decimalLatitude) & is.na(records$coordinateUncertaintyInMeters), ])
	
	# threshold_m <- 5000
	# n3 <- nrow(records[!is.na(records$decimalLongitude) & !is.na(records$decimalLatitude) & records$coordinateUncertaintyInMeters %>na% threshold_m, ])
	
	# say('Records lacking coordinates but with country, state/province, county ............ ', 100 * round(n1 / N, 3), '%')
	# say('Records with coordinates but lacking coordinate uncertainty ..................... ', 100 * round(n2 / N, 3), '%')
	# say('Records with coordinate uncertainty >', threshold_m, 'm coordinate uncertainty ... ', 100 * round(n3 / N, 3), '%')
	# say('SUM .............................................................................. ', 100 * round((n1 + n2 + n3) / N, 3), '%')
	
	# n <- nrow(records[!is.na(records$decimalLongitude) & !is.na(records$decimalLatitude) & !is.na(records$coordinatePrecision), ])
	# say('Records with coordinate precision stated ......................................... ', 100 * round(n / N, 2), '%')
	
# say('#####################')
# say('### tally records ###')
# say('#####################')

	# raw <- fread('./Data/GBIF Asclepias 2019-11-18/occurrence.txt', header=TRUE)
	# raw <- as.data.frame(raw)
	
	# raw$stateProvince[raw$stateProvince == ''] <- NA
	# raw$county[raw$county == ''] <- NA
	# raw$locality[raw$locality == ''] <- NA
	# raw$verbatimLocality[raw$verbatimLocality == ''] <- NA
	
	# load('./Data/Asclepias 02 Specimen Records with Duplicates Flagged.rda')
	# load('./Data/Asclepias 03 Specimen Records and Environmental Data.rda')
	
	# ### species name
	# tally <- data.frame(tally='Invalid species name', n=sum(records$cleanBadSpeciesName, na.rm=TRUE))
	
	# ### invalid year
	# tally <- rbind(
		# tally,
		# data.frame(tally='Invalid collection year', n=sum(records$cleanBadYear, na.rm=TRUE))
	# )
	
	# ### missing coordinates
	# tally <- rbind(
		# tally,
		# data.frame(tally='Missing coordinates', n=sum(!complete.cases(raw[ , llGbif])))
	# )
	
	# ### missing coordinates and coordinate uncertainty
	# n <- sum(complete.cases(raw[ , llGbif]) & !complete.cases(raw$coordinateUncertaintyInMeters))
	
	# tally <- rbind(
		# tally,
		# data.frame(tally='Possess coordinates but missing coordinate uncertainty', n=n)
	# )
	
	# ### missing administrative information
	# tally <- rbind(
		# tally,
		# data.frame(tally='Missing state/province', n=sum(is.na(raw$stateProvince)))
	# )
	
	# tally <- rbind(
		# tally,
		# data.frame(tally='Missing county', n=sum(is.na(raw$county)))
	# )

	# ### administrative mismatch
	# tally <- rbind(
		# tally,
		# data.frame(tally='Administrative mismatch', n=sum(!records$adminMatch, na.rm=TRUE))
	# )
	
	# ### cultivated
	# tally <- rbind(
		# tally,
		# data.frame(tally='Cultivated', n=sum(records$cleanCultivated, na.rm=TRUE))
	# )
	
	# # missing locality information
	# tally <- rbind(
		# tally,
		# data.frame(tally='Missing information in locality fields', n=sum(is.na(raw$locality) & is.na(raw$verbatimLocality)))
	# )
	
	# ### observational records
	# tally <- rbind(
		# tally,
		# data.frame(tally='Observational record', n=sum(records$cleanObservationalRecord, na.rm=TRUE))
	# )
	
	# ### outside coterminous North America
	# tally <- rbind(
		# tally,
		# data.frame(tally='Outside coterminous North America', n=sum(records$cleanNonCoterminiousNorthAmerica, na.rm=TRUE))
	# )
	
	# ### uncertainty too large
	# tally <- rbind(
		# tally,
		# data.frame(tally='Area of uncertainty too large*', n=sum(records$cleanAreaTooLarge, na.rm=TRUE))
	# )
	
	# ### collected before 1970
	# tally <- rbind(
		# tally,
		# data.frame(tally='Collected before 1970', n=sum(records$cleanBeforeStartYear, na.rm=TRUE))
	# )
	
	# ### duplicated
	# tally <- rbind(
		# tally,
		# data.frame(tally='Geographic duplicates', n=sum(records$cleanDuplicateRecord, na.rm=TRUE))
	# )
	
	# ### assignments
	# tally <- rbind(
		# tally,
		# data.frame(tally='State-level record', n=sum(records$recordType %in% stateAssigns))
	# )
	
	# tally <- rbind(
		# tally,
		# data.frame(tally='County-level record', n=sum(records$recordType %in% countyAssigns))
	# )

	# tally <- rbind(
		# tally,
		# data.frame(tally='Inaccurate record', n=sum(records$recordType %in% inaccAssigns))
	# )
	
	# tally <- rbind(
		# tally,
		# data.frame(tally='Accurate record', n=sum(records$recordType %in% accAssigns))
	# )
	
	# tally <- rbind(
		# tally,
		# data.frame(tally='Final usable, all species', n=sum(records$usable))
	# )
	
	# tally <- rbind(
		# tally,
		# data.frame(tally='Final accurate, species with >= 5 accurate records', n=sum(asclepias$meta$usableSpecies[ , c('numUniqueUsableAccurateRecs')]))
	# )
	
	# tally <- rbind(
		# tally,
		# data.frame(tally='Final inaccurate, species with >= 5 accurate records', n=sum(asclepias$meta$usableSpecies[ , c('numUniqueUsableInaccurateRecs')]))
	# )
	
	# tally <- rbind(
		# tally,
		# data.frame(tally='Final administrative, species with >= 5 accurate records', n=sum(asclepias$meta$usableSpecies[ , c('numUniqueUsableAdminRecs')]))
	# )
	
	# tally <- rbind(
		# tally,
		# data.frame(tally='Final usable, species with >= 5 accurate records', n=sum(asclepias$meta$usableSpecies[ , c('numUniqueUsableAccurateRecs', 'numUniqueUsableInaccurateRecs', 'numUniqueUsableAdminRecs')]))
	# )
	
	# ### total number of records
	# tally <- rbind(
		# tally,
		# data.frame(tally='Total records', n=nrow(records))
	# )

	# write.csv(tally, './Analysis/Asclepias/Record Tallies.csv', row.names=FALSE)
	
	
	# say('TALLIES across species used in the analysis:')

		# x <- (asclepias$meta$usableSpecies$numUniqueUsableInaccurateRecs + asclepias$meta$usableSpecies$numUniqueUsableAdminRecs) / asclepias$meta$usableSpecies$numUniqueUsableAccurateRecs
		# say('Median ratio of vague-to-accurate records: ', round(median(x), 2), ' (min: ', round(min(x), 2), ' , max: ', round(max(x), 2), ')')
		# say('Mean ratio of vague-to-accurate records: ', round(mean(x), 2), ' (se: ', round(sd(x) / sqrt(length(x)), 2), ')')
			
		# x <- asclepias$meta$usableSpecies$numUniqueUsableAccurateRecs
		# say('Median number of accurate records: ', round(median(x), 2), ' (min: ', round(min(x), 2), ' , max: ', round(max(x), 2), ')')
		# say('Mean number of accurate records: ', round(mean(x), 2), ' (se: ', round(sd(x) / sqrt(length(x)), 2), ')')

		# x <- asclepias$meta$usableSpecies$numUniqueUsableInaccurateRecs
		# say('Median number of inaccurate records: ', round(median(x), 2), ' (min: ', round(min(x), 2), ' , max: ', round(max(x), 2), ')')
		# say('Mean number of inaccurate records: ', round(mean(x), 2), ' (se: ', round(sd(x) / sqrt(length(x)), 2), ')')

		# x <- asclepias$meta$usableSpecies$numUniqueUsableAdminRecs
		# say('Median number of administrative records: ', round(median(x), 2), ' (min: ', round(min(x), 2), ' , max: ', round(max(x), 2), ')')
		# say('Mean number of administrative records: ', round(mean(x), 2), ' (se: ', round(sd(x) / sqrt(length(x)), 2), ')')

		# say('')
		# x <- rowSums(asclepias$meta$usableSpecies[ , c('numUniqueUsableInaccurateRecs', 'numUniqueUsableAdminRecs')]) / rowSums(asclepias$meta$usableSpecies[ , c('numUniqueUsableAccurateRecs', 'numUniqueUsableInaccurateRecs', 'numUniqueUsableAdminRecs')])
		# say('Mean percentage of vague records: ', round(mean(x), 2), ' (min: ', round(min(x), 2), ', max: ', round(max(x), 2), ')')
			
		# x <- rowSums(asclepias$meta$usableSpecies[ , c('numUniqueUsableAdminRecs'), drop=FALSE]) / rowSums(asclepias$meta$usableSpecies[ , c('numUniqueUsableAccurateRecs', 'numUniqueUsableInaccurateRecs', 'numUniqueUsableAdminRecs')])
		# say('Mean percentage of administrative records: ', round(mean(x), 2), ' (min: ', round(min(x), 2), ', max: ', round(max(x), 2), ')')
			
		# x <- rowSums(asclepias$meta$usableSpecies[ , c('numUniqueUsableAccurateRecs'), drop=FALSE]) / rowSums(asclepias$meta$usableSpecies[ , c('numUniqueUsableAccurateRecs', 'numUniqueUsableInaccurateRecs', 'numUniqueUsableAdminRecs')])
		# say('Mean percentage of accurate records: ', round(mean(x), 2), ' (min: ', round(min(x), 2), ', max: ', round(max(x), 2), ')')
			
		# x <- rowSums(asclepias$meta$usableSpecies[ , c('numUniqueUsableInaccurateRecs'), drop=FALSE]) / rowSums(asclepias$meta$usableSpecies[ , c('numUniqueUsableAccurateRecs', 'numUniqueUsableInaccurateRecs', 'numUniqueUsableAdminRecs')])
		# say('Mean percentage of inaccurate records: ', round(mean(x), 2), ' (min: ', round(min(x), 2), ', max: ', round(max(x), 2), ')')

# say('##########################################')
# say('### report statistics on change in EOO ###')
# say('##########################################')

	# # EOO
	# x <- read.csv('./Analysis/Extent of Occurrence/!Extent of Occurrence.csv')
	
	# say('Difference in EOO:', pre=1)

		# acc <- x$accRange_km2
		# all <- x$accInaccAdminRange_km2
		# delta <- (all - acc) / acc
		# rng <- range(delta)
		
		# medianDelta <- round(median(100 * delta), 1)
		# rng <- round(100 * rng, 1)
		
		# wcox <- wilcox.test(acc, all, paired=TRUE, alternative='two.sided')
		# print(wcox)

		# say('Median (range): ', medianDelta, '% (', rng[1], ' to ', rng[2], ').', post=1)
		# say('Number (%) of species experiencing a doubling in EOO: ', sum(delta > 2), ' (', round(100 * sum(delta > 2) / nrow(x), 1), ').')
		# say('Number (%) of species experiencing a tripling in EOO: ', sum(delta > 3), ' (', round(100 * sum(delta > 3) / nrow(x), 1), ').')
		# say('Number (%) of species experiencing a quadrupling in EOO: ', sum(delta > 4), ' (', round(100 * sum(delta > 4) / nrow(x), 1), ').')
		# say('Number (%) of species experiencing a quintupling in EOO: ', sum(delta > 5), ' (', round(100 * sum(delta > 5) / nrow(x), 1), ').', post=2)

# say('#####################################################')
# say('### report statistics on univariate niche breadth ###')
# say('#####################################################')

	# # univariate niche breadth
	# x <- read.csv('./Analysis/Univariate Niche Breadth/!Univariate Niche Breadth.csv')
	
	# say('Difference in MAT niche breadth:', pre=1)

		# acc <- x$nicheBreathBio1_acc
		# all <- x$nicheBreathBio1_closest
		# delta <- (all - acc) / acc
		# rng <- range(delta)
		
		# medianDelta <- round(median(100 * delta), 1)
		# rng <- round(100 * rng, 1)
		
		# say('Median (range): ', medianDelta, '% (', rng[1], ' to ', rng[2], ').')
		# wcox <- wilcox.test(acc, all, paired=TRUE, alternative='two.sided')
		# print(wcox)

	# say('Difference in MAP niche breadth:', pre=1)

		# acc <- x$nicheBreathBio12_acc
		# all <- x$nicheBreathBio12_closest
		# delta <- (all - acc) / acc
		# rng <- range(delta)
		
		# medianDelta <- round(median(100 * delta), 1)
		# rng <- round(100 * rng, 1)
		
		# say('Median (range): ', medianDelta, '% (', rng[1], ' to ', rng[2], ').', post=2)
		# wcox <- wilcox.test(acc, all, paired=TRUE, alternative='two.sided')
		# print(wcox)

# say('######################################################')
# say('### report statistics on multivariate niche volume ###')
# say('######################################################')

	# # multivariate niche volume
	# x <- read.csv('./Analysis/Multivariate Niche Volume/!pcaVolume_df.csv')
	
	# say('Difference in niche volume:', pre=1)

		# acc <- x$accurate_vol
		# all <- x$nearest_vol
		# delta <- (all - acc) / acc
		# rng <- range(delta)
		
		# medianDelta <- round(median(100 * delta), 1)
		# rng <- round(100 * rng, 1)
		
		# say('Median (range): ', medianDelta, '% (', rng[1], ' to ', rng[2], ').', post=2)

		# wcox <- wilcox.test(acc, all, paired=TRUE, alternative='two.sided')
		# print(wcox)

	# say('Difference in area volume:', pre=1)

		# acc <- x$accurate_area
		# all <- x$nearest_area
		# delta <- (all - acc) / acc
		# rng <- range(delta)
		
		# medianDelta <- round(median(100 * delta), 1)
		# rng <- round(100 * rng, 1)
		
		# say('Median (range): ', medianDelta, '% (', rng[1], ' to ', rng[2], ').', post=2)

		# wcox <- wilcox.test(acc, all, paired=TRUE, alternative='two.sided')
		# print(wcox)

# say('###########################################################')
# say('### report metrics on change in climate change exposure ###')
# say('###########################################################')

	# # climate change exposure
	# x <- read.csv('./Analysis/ENMs/!Climate Change Exposure - Areal Values for Current, Future, Stable, Gain, and Loss.csv')
	
	# say('Difference in CURRENT climatically suitable area within EOO of precise records:', pre=1)

		# acc <- x$mcpAccs_currentArea_km2[x$rcp==45 & x$background=='convexHull' & x$assignMethod=='accurate']
		# all <- x$mcpAccs_currentArea_km2[x$rcp==45 & x$background=='convexHull' & x$assignMethod=='closest']
		# delta <- (all - acc) / acc
		# rng <- range(delta)
		
		# medianDelta <- round(median(100 * delta), 1)
		# rng <- round(100 * rng, 1)
		
		# say('Median (range): ', medianDelta, '% (', rng[1], ' to ', rng[2], ').')
		# say('Number of species with an increase (%): ', sum(delta > 0), ' (', round(100 * sum(delta > 0) / 44, 1), '%).')

	# say('Difference in CURRENT climatically suitable area within EOO of precise & imprecise records:', pre=1)

		# acc <- x$mcpAccsInaccsAdmin_currentArea_km2[x$rcp==45 & x$background=='convexHull' & x$assignMethod=='accurate']
		# all <- x$mcpAccsInaccsAdmin_currentArea_km2[x$rcp==45 & x$background=='convexHull' & x$assignMethod=='closest']
		# delta <- (all - acc) / acc
		# rng <- range(delta)
		
		# medianDelta <- round(median(100 * delta), 1)
		# rng <- round(100 * rng, 1)
		
		# say('Median (range): ', medianDelta, '% (', rng[1], ' to ', rng[2], ').')
		# say('Number of species with an increase (%): ', sum(delta > 0), ' (', round(100 * sum(delta > 0) / 44, 1), '%).')

	# say('Difference in FUTURE climatically suitable area within EOO of precise records under RCP4.5:', pre=1)

		# acc <- x$mcpAccs_futArea_km2[x$rcp==45 & x$background=='convexHull' & x$assignMethod=='accurate']
		# all <- x$mcpAccs_futArea_km2[x$rcp==45 & x$background=='convexHull' & x$assignMethod=='closest']
		
		# delta <- (all - acc) / acc
		# rng <- range(delta)
		
		# medianDelta <- round(median(100 * delta), 1)
		# rng <- round(100 * rng, 1)
		
		# say('Median (range): ', medianDelta, '% (', rng[1], ' to ', rng[2], ').')
		
	# say('Difference in FUTURE climatically suitable area within EOO of precise records under RCP8.5:', pre=1)

		# acc <- x$mcpAccs_futArea_km2[x$rcp==85 & x$background=='convexHull' & x$assignMethod=='accurate']
		# all <- x$mcpAccs_futArea_km2[x$rcp==85 & x$background=='convexHull' & x$assignMethod=='closest']
		
		# delta <- (all - acc) / acc
		# rng <- range(delta)
		
		# medianDelta <- round(median(100 * delta), 1)
		# rng <- round(100 * rng, 1)
		
		# say('Median (range): ', medianDelta, '% (', rng[1], ' to ', rng[2], ').')
		
	# say('Difference in FUTURE STABLE climatically suitable area within EOO of precise records:', pre=1)

		# acc <- x$mcpAccs_stableArea_km2[x$rcp==45 & x$background=='convexHull' & x$assignMethod=='accurate']
		# all <- x$mcpAccs_stableArea_km2[x$rcp==45 & x$background=='convexHull' & x$assignMethod=='closest']
		
		# delta <- (all - acc) / acc
		# rng <- range(delta)
		
		# medianDelta <- round(median(100 * delta), 1)
		# rng <- round(100 * rng, 1)
		
		# say('Median (range): ', medianDelta, '% (', rng[1], ' to ', rng[2], ').')
		
	# say('Difference in LOSS of climatically suitable area within EOO of precise records relative to current suitable area:', pre=1)

		# accDelta <- x$mcpAccs_lossArea_km2[x$rcp==45 & x$background=='convexHull' & x$assignMethod=='accurate']
		# accStart <- x$mcpAccs_currentArea_km2[x$rcp==45 & x$background=='convexHull' & x$assignMethod=='accurate']
		# allDelta <- x$mcpAccs_lossArea_km2[x$rcp==45 & x$background=='convexHull' & x$assignMethod=='closest']
		# allStart <- x$mcpAccs_currentArea_km2[x$rcp==45 & x$background=='convexHull' & x$assignMethod=='closest']
		
		# delta <- allDelta / allStart - accDelta / accStart
		# rng <- range(delta)
		
		# medianDelta <- round(median(100 * delta), 1)
		# rng <- round(100 * rng, 1)
		
		# say('Median (range): ', medianDelta, '% (', rng[1], ' to ', rng[2], ').')
		
		# change <- accDelta / accStart
		# medianChange <- round(100 * median(change), 1)
		# rng <- round(100 * range(change), 1)
		# say('Median loss using precise-only: ', medianChange, '% (', rng[1], ' to ', rng[2], '%).')
		
		# change <- allDelta / allStart
		# medianChange <- round(100 * median(change), 1)
		# rng <- round(100 * range(change), 1)
		# say('Median loss using precise + imprecise: ', medianChange, '% (', rng[1], ' to ', rng[2], '%).')

	# say('Difference in GAIN of climatically suitable area within EOO of precise records relative to current suitable area:', pre=1)

		# accDelta <- x$mcpAccs_gainArea_km2[x$rcp==45 & x$background=='convexHull' & x$assignMethod=='accurate']
		# accStart <- x$mcpAccs_currentArea_km2[x$rcp==45 & x$background=='convexHull' & x$assignMethod=='accurate']
		# allDelta <- x$mcpAccs_gainArea_km2[x$rcp==45 & x$background=='convexHull' & x$assignMethod=='closest']
		# allStart <- x$mcpAccs_currentArea_km2[x$rcp==45 & x$background=='convexHull' & x$assignMethod=='closest']
		
		# delta <- allDelta / allStart - accDelta / accStart
		# rng <- range(delta)
		
		# medianDelta <- round(median(100 * delta), 1)
		# rng <- round(100 * rng, 1)
		
		# say('Median (range): ', medianDelta, '% (', rng[1], ' to ', rng[2], ').')
		
		# change <- accDelta / accStart
		# medianChange <- round(100 * median(change), 1)
		# rng <- round(100 * range(change), 1)
		# say('Median gain using precise-only: ', medianChange, '% (', rng[1], ' to ', rng[2], '%).')
		
		# change <- allDelta / allStart
		# medianChange <- round(100 * median(change), 1)
		# rng <- round(100 * range(change), 1)
		# say('Median gain using precise + imprecise: ', medianChange, '% (', rng[1], ' to ', rng[2], '%).', post=1)
		
	# say('NET CHANGE in climatically suitable area within EOO of PRECISE records relative to current suitable area:', pre=1)

		# accCurrent <- x$mcpAccs_currentArea_km2[x$rcp==45 & x$background=='convexHull' & x$assignMethod=='accurate']
		# accGain <- x$mcpAccs_gainArea_km2[x$rcp==45 & x$background=='convexHull' & x$assignMethod=='accurate']
		# accLoss <- x$mcpAccs_lossArea_km2[x$rcp==45 & x$background=='convexHull' & x$assignMethod=='accurate']
		
		# allCurrent <- x$mcpAccs_currentArea_km2[x$rcp==45 & x$background=='convexHull' & x$assignMethod=='closest']
		# allGain <- x$mcpAccs_gainArea_km2[x$rcp==45 & x$background=='convexHull' & x$assignMethod=='closest']
		# allLoss <- x$mcpAccs_lossArea_km2[x$rcp==45 & x$background=='convexHull' & x$assignMethod=='closest']
		
		# accNet <- (accGain - accLoss) / accCurrent
		# allNet <- (allGain - allLoss) / allCurrent

		# accNetMedian <- round(median(100 * accNet))
		# allNetMedian <- round(median(100 * allNet))

		# rngAcc <- round(100 * range(accNet))
		# rngAll <- round(100 * range(allNet))
	
		# say('Median precise-only (range): ', accNetMedian, '% (', rngAcc[1], ' to ', rngAcc[2], ').')
		# say('   Number of species increasing, precise-only (%): ', sum(accNet > 0), ' (', round(100 * sum(accNet > 0) / 44), '%).')
		# say('Median precise & imprecise (range): ', allNetMedian, '% (', rngAll[1], ' to ', rngAll[2], ').', post=1)
		# say('   Number of species increasing, precise & imprecise (%): ', sum(allNet > 0), ' (', round(100 * sum(allNet > 0) / 44), '%).')
	
	# say('NET CHANGE in climatically suitable area within EOO of PRECISE & IMPRECISE records relative to current suitable area:', pre=1)

		# accCurrent <- x$mcpAccsInaccsAdmin_currentArea_km2[x$rcp==45 & x$background=='convexHull' & x$assignMethod=='accurate']
		# accGain <- x$mcpAccsInaccsAdmin_gainArea_km2[x$rcp==45 & x$background=='convexHull' & x$assignMethod=='accurate']
		# accLoss <- x$mcpAccsInaccsAdmin_lossArea_km2[x$rcp==45 & x$background=='convexHull' & x$assignMethod=='accurate']
		
		# allCurrent <- x$mcpAccsInaccsAdmin_currentArea_km2[x$rcp==45 & x$background=='convexHull' & x$assignMethod=='closest']
		# allGain <- x$mcpAccsInaccsAdmin_gainArea_km2[x$rcp==45 & x$background=='convexHull' & x$assignMethod=='closest']
		# allLoss <- x$mcpAccsInaccsAdmin_lossArea_km2[x$rcp==45 & x$background=='convexHull' & x$assignMethod=='closest']
		
		# accNet <- (accGain - accLoss) / accCurrent
		# allNet <- (allGain - allLoss) / allCurrent

		# accNetMedian <- round(median(100 * accNet))
		# allNetMedian <- round(median(100 * allNet))

		# rngAcc <- round(100 * range(accNet))
		# rngAll <- round(100 * range(allNet))
	
		# say('Median precise-only (range): ', accNetMedian, '% (', rngAcc[1], ' to ', rngAcc[2], ').')
		# say('Median precise & imprecise (range): ', allNetMedian, '% (', rngAll[1], ' to ', rngAll[2], ').', post=1)
	
	# say('NatureServe CCVI rankings (Young et al. 2016p. 43+), under RCP4.5 within PRECISE EOO', pre=1)
	# say('https://www.natureserve.org/sites/default/files/guidelines_natureserveclimatechangevulnerabilityindex_r3.02_1_jun_2016.pdf')
	
		# accCurrent <- x$mcpAccsInaccsAdmin_currentArea_km2[x$rcp==45 & x$background=='convexHull' & x$assignMethod=='accurate']
		# accFut <- x$mcpAccsInaccsAdmin_futArea_km2[x$rcp==45 & x$background=='convexHull' & x$assignMethod=='accurate']
		
		# allCurrent <- x$mcpAccsInaccsAdmin_currentArea_km2[x$rcp==45 & x$background=='convexHull' & x$assignMethod=='closest']
		# allFut <- x$mcpAccsInaccsAdmin_futArea_km2[x$rcp==45 & x$background=='convexHull' & x$assignMethod=='closest']
	
		# accNetChange <- (accFut - accCurrent) / accCurrent
		# allNetChange <- (allFut - allCurrent) / allCurrent
	
		# tholdLower <- -0.99
		# tholdUpper <- -0.5
		# say('Number (%) of species in "increased vulnerability" category (50-99% loss), precise-only: ', sum(accNetChange >= tholdLower & accNetChange < tholdUpper), ' (', round(sum(accNetChange >= tholdLower & accNetChange < tholdUpper) / 44), '%).')
		# say('Number (%) of species in "increased vulnerability" category (50-99% loss), precise & imprecise: ', sum(allNetChange >= tholdLower & allNetChange < tholdUpper), ' (', round(sum(allNetChange >= tholdLower & allNetChange < tholdUpper) / 44), '%).', post=2)
	
		# tholdLower <- -0.5
		# tholdUpper <- -0.2
		# say('Number (%) of species in "somewhat increased vulnerability" category (20-50% loss), precise-only: ', sum(accNetChange >= tholdLower & accNetChange < tholdUpper), ' (', round(sum(accNetChange >= tholdLower & accNetChange < tholdUpper) / 44), '%).')
		# say('Number (%) of species in "somewhat increased vulnerability" category (20-50% loss), precise & imprecise: ', sum(allNetChange >= tholdLower & allNetChange < tholdUpper), ' (', round(sum(allNetChange >= tholdLower & allNetChange < tholdUpper) / 44), '%).', post=2)
		
# say('####################################################################################################')
# say('### plot increase in EOO and niche breadth as function of number of precise records using base R ###')
# say('####################################################################################################')

	# eoo <- read.csv('./Analysis/Extent of Occurrence/!Extent of Occurrence.csv')
	# uniNiche <- read.csv('./Analysis/Univariate Niche Breadth/!Univariate Niche Breadth.csv')
	# mvNiche <- read.csv('./Analysis/Multivariate Niche Volume/!pcaVolume_df.csv')
	
	# labX <- -0.19
	# labY <- 0.08
	# lwd <- 0.6
	# cex <- 0.8
	
	# pdf('./Analysis/Number of Precise Records vs EOO and Niche Inflation Base R.pdf', width=8, height=8)
		
		# par(mfrow=c(2, 2), pty='s', lwd=0.5, oma=c(0.2, 2.2, 0.2, 0.2), mar=c(4.4, 2.2, 2, 0), bty='n', cex=0.9, cex.axis=cex, lwd=lwd, mgp=c(2.7, 1, 0))
		
		# ### number of records for each species
		# recs <- eoo[ , c('numAccs', 'numInaccs', 'numCounty', 'numState')]
		# recs$numAdmin <- rowSums(recs[ c('numCounty', 'numState')])
		# recs$numCounty <- recs$numState <- NULL
		# spp <- eoo$species
		# spp <- gsub(spp, pattern='Asclepias ', replacement='A. ')
		# totals <- rowSums(recs)
		# orders <- order(totals)
		# spp <- spp[orders]
		# recs <- recs[orders, ]
		# recs <- t(as.matrix(recs))
		# colnames(recs) <- spp
		# col <- c('gray32', 'gray', 'gray99') # grays
		# barplot(recs, beside=FALSE, horiz=TRUE, col=col, ylab='Species', xlab='Number of precise records', xpd=NA, lwd=lwd, names.arg=rep('', length(spp)))
		
		# text(rep(15, length(spp)), seq(0.7, 52.2, length.out=length(spp)), labels=spp, cex=0.4, xpd=NA, font=3, pos=2)
		# labelFig('a) Number of records', adj=c(labX, labY - 0.03), font=1)
		# legend('bottomright', inset=0.05, legend=c('Precise', 'Imprecise (coord. uncertainty)', 'Imprecise (geopolitical units)'), fill=col, bty='n', cex=cex)

		# ### number of precise records vs EOO
		# xlim <- c(0, roundTo(max(eoo$numAccs), 50, ceiling))
		# delta <-100 * (eoo$accInaccAdminRange_km2 - eoo$accRange_km2) / eoo$accRange_km2
		# plot(eoo$numAccs, delta, xlab='Number of precise records', ylab='Increase in EOO (%)', col='black', bg='gray40', pch=24, xpd=NA, xlim=xlim, lwd=lwd)
		# labelFig('b) Increase in extent of occurrence', adj=c(labX, labY - 0.03), font=1)
		# # points(eoo$numAccs, eoo$rangeRatio, col='black', bg='cornflowerblue', pch=2)

		# ### number of precise records vs MAT and MAP uniNiche breadth
		# colMat <- 'firebrick1'
		# colMap <- 'cornflowerblue'

		# deltaMat <- 100 * (uniNiche$nicheBreathBio1_closest - uniNiche$nicheBreathBio1_acc) / uniNiche$nicheBreathBio1_acc
		# deltaMap <- 100 * (uniNiche$nicheBreathBio12_closest - uniNiche$nicheBreathBio12_acc) / uniNiche$nicheBreathBio12_acc

		# ylim <- c(0, max(deltaMat, deltaMap))
		
		# plot(uniNiche$numAccs, deltaMat, xlab='Number of precise records', ylab='Increase in niche breadth (%)', col=NA, bg=colMat, pch=21, xlim=xlim, ylim=ylim, lwd=lwd, xpd=NA)
		# points(uniNiche$numAccs, deltaMap, col='black', bg=colMap, pch=25) # MAP
		# points(uniNiche$numAccs, deltaMat, pch=1) # MAT
		# labelFig('c) Increase in univariate niche breadth', adj=c(labX, labY), font=1)
		# legend('topright', inset=0.15, legend=c('MAT', 'MAP'), pt.bg=c(colMat, colMap), pch=c(21, 25), box.lwd=0.2, cex=0.9, bg=alpha('white', 0.5))
		
		# # number of precise records vs multivariate niche breadth
		# delta <- 100 * (mvNiche$nearest - mvNiche$accurate) / mvNiche$accurate
		# plot(uniNiche$numAccs, delta, xlab='Number of precise records', ylab='Increase in niche volume (%)', col='black', bg='purple', pch=21, lwd=lwd, xlim=xlim, xpd=NA)
		# labelFig('d) Increase in multivariate niche volume', adj=c(labX, labY), font=1)
		
	# dev.off()

# say('##########################################################################')
# say('### plot distribution of different types of occurrences across species ###')
# say('##########################################################################')

	# ### barplots of number of records by species
	# eoo <- read.csv('./Analysis/Extent of Occurrence/!Extent of Occurrence.csv')
	
	# n <- nrow(eoo)

	# x <- data.frame(
		# species = rep(eoo$species, 3),
		# type = rep(c('precise', 'imprecise (coord. uncertainty)', 'imprecise (geopolitical units)'), each=n),
		# numRecs = c(eoo$numAccs, eoo$numInaccs, eoo$numCounty + eoo$numState)
	# )
	
	# x$species <- gsub(x$species, pattern='Asclepias', replacement='A.')
	# numRecs <- eoo$numAccs
	# names(numRecs) <- gsub(eoo$species, pattern='Asclepias', replacement='A.')
	# numRecs <- sort(numRecs, FALSE)
	# x$species <- factor(x$species, levels=names(numRecs))
	
	# recs <- ggplot(data=x, aes(x=species, y=numRecs, fill=type)) +
		# geom_bar(stat='identity') +
		# scale_fill_manual(values=c('precise'='#1b9e77', 'imprecise (coord. uncertainty)'='#d95f02', 'imprecise (geopolitical units)'='darkorange')) +
		# labs(x=NULL, y='Number of occurrences', fill='Occurrence type') +
		# guides(fill = guide_legend(reverse = TRUE)) +
		# theme(
			# legend.position=c(0.9, 0.03),
			# legend.justification=c(0.9, 0.05),
			# legend.title=element_text(size=9),
			# legend.text=element_text(size=8),			
			# axis.text.y=element_text(size=8, face='italic')) +
		# coord_flip()

	# print(recs)

	# ggsave('./Analysis/Number of Records by Species.pdf', width=4, height=5, units='in')
	# dev.off()

# say('#######################################################')
# say('### statistical analyses of climate change exposure ###')
# say('#######################################################')
	
	# say('PRESENT-DAY climatically suitable area within precise MCP', level=2)
	
		# data <- read.csv('./Analysis/ENMs/!Climate Change Exposure - Areal Values for Current, Future, Stable, Gain, and Loss.csv')
		# col <- 'mcpAccs_currentArea_km2'
		
		# x <- data[data$rcp==45 & data$background=='convexHull' & data$assignMethod=='accurate', col]
		# y <- data[data$rcp==45 & data$background=='convexHull' & data$assignMethod=='closest', col]
		
		# wcox <- wilcox.test(x, y, paired=TRUE, alternative='two.sided')
		# print(wcox)
		
	# say('PRESENT-DAY climatically suitable area within precise & imprecise MCP', level=2)
	
		# data <- read.csv('./Analysis/ENMs/!Climate Change Exposure - Areal Values for Current, Future, Stable, Gain, and Loss.csv')
		# col <- 'mcpAccsInaccsAdmin_currentArea_km2'
		
		# x <- data[data$rcp==45 & data$background=='convexHull' & data$assignMethod=='accurate', col]
		# y <- data[data$rcp==45 & data$background=='convexHull' & data$assignMethod=='closest', col]
		
		# wcox <- wilcox.test(x, y, paired=TRUE, alternative='two.sided')
		# print(wcox)
		
	# say('climatically STABLE area within MCP of precise records', level=2)
	
		# data <- read.csv('./Analysis/ENMs/!Climate Change Exposure - Areal Values for Current, Future, Stable, Gain, and Loss.csv')
		# col <- 'mcpAccs_stableArea_km2'
		
		# x <- data[data$rcp==45 & data$background=='convexHull' & data$assignMethod=='accurate', col]
		# y <- data[data$rcp==45 & data$background=='convexHull' & data$assignMethod=='closest', col]
		
		# wcox <- wilcox.test(x, y, paired=TRUE, alternative='two.sided')
		# print(wcox)
		
	# say('climatically STABLE area within MCP of precise & imprecise records', level=2)
	
		# data <- read.csv('./Analysis/ENMs/!Climate Change Exposure - Areal Values for Current, Future, Stable, Gain, and Loss.csv')
		# col <- 'mcpAccsInaccsAdmin_stableArea_km2'
		
		# x <- data[data$rcp==45 & data$background=='convexHull' & data$assignMethod=='accurate', col]
		# y <- data[data$rcp==45 & data$background=='convexHull' & data$assignMethod=='closest', col]
		
		# wcox <- wilcox.test(x, y, paired=TRUE, alternative='two.sided')
		# print(wcox)
		
	# say('LOSS of climatically suitable area within MCP of precise records', level=2)
	
		# data <- read.csv('./Analysis/ENMs/!Climate Change Exposure - Areal Values for Current, Future, Stable, Gain, and Loss.csv')
		# col <- 'mcpAccs_lossArea_km2'
		
		# x <- data[data$rcp==45 & data$background=='convexHull' & data$assignMethod=='accurate', col]
		# y <- data[data$rcp==45 & data$background=='convexHull' & data$assignMethod=='closest', col]
		
		# wcox <- wilcox.test(x, y, paired=TRUE, alternative='two.sided')
		# print(wcox)
		
	# say('LOSS of climatically suitable area within MCP of precise & imprecise records', level=2)
	
		# data <- read.csv('./Analysis/ENMs/!Climate Change Exposure - Areal Values for Current, Future, Stable, Gain, and Loss.csv')
		# col <- 'mcpAccsInaccsAdmin_lossArea_km2'
		
		# x <- data[data$rcp==45 & data$background=='convexHull' & data$assignMethod=='accurate', col]
		# y <- data[data$rcp==45 & data$background=='convexHull' & data$assignMethod=='closest', col]
		
		# wcox <- wilcox.test(x, y, paired=TRUE, alternative='two.sided')
		# print(wcox)
		
	# say('GAIN of climatically suitable area within MCP of precise records', level=2)
	
		# data <- read.csv('./Analysis/ENMs/!Climate Change Exposure - Areal Values for Current, Future, Stable, Gain, and Loss.csv')
		# col <- 'mcpAccs_gainArea_km2'
		
		# x <- data[data$rcp==45 & data$background=='convexHull' & data$assignMethod=='accurate', col]
		# y <- data[data$rcp==45 & data$background=='convexHull' & data$assignMethod=='closest', col]
		
		# wcox <- wilcox.test(x, y, paired=TRUE, alternative='two.sided')
		# print(wcox)

	# say('GAIN of climatically suitable area within MCP of precise & imprecise records', level=2)
	
		# data <- read.csv('./Analysis/ENMs/!Climate Change Exposure - Areal Values for Current, Future, Stable, Gain, and Loss.csv')
		# col <- 'mcpAccsInaccsAdmin_gainArea_km2'
		
		# x <- data[data$rcp==45 & data$background=='convexHull' & data$assignMethod=='accurate', col]
		# y <- data[data$rcp==45 & data$background=='convexHull' & data$assignMethod=='closest', col]
		
		# wcox <- wilcox.test(x, y, paired=TRUE, alternative='two.sided')
		# print(wcox)
		
		
	
say('DONE!', level=1, deco='%')
