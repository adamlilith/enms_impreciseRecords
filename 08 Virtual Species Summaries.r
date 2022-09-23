### EFFECT OF INCLUDING IMPRECISE SPECIMENS ON CLIMATE CHANGE EXPOSURE - VIRTUAL SPECIES
### Adam B. Smith | Missouri Botanical Garden | adam.smith@mobot.org | 2021-10
### 
### source('C:/Ecology/Drive/Research/Vaguely Georeferenced Specimen Records/enms_impreciseRecords/08 Virtual Species Summaries.r')
### source('E:/Ecology/Drive/Research/Vaguely Georeferenced Specimen Records/enms_impreciseRecords/08 Virtual Species Summaries.r')

### CONTENTS ###
### setup ###

### collate results ###

### make composite plot with SUBSET of EOO and niche breadth results - TWO omniscient scenarios ###
### make composite plot with SUBSET of ENM results - TWO omniscient scenarios ###
### make composite plot with SUBSET of ENM results - TWO omniscient scenarios REDUCED ###

### make composite plot of ALL CLIMATE CHANGE EXPOSURE results ###
### make composite plot of ALL ENM calibration accuracy ###

### make composite plot of ALL EOO and NICHE BREADTH results ###
### make plot of model complexity ###

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
	library(patchwork)
	library(rgeos)
	library(scales)
	library(stringr)
	library(terra)
	
	# custom (Adam Smith)
	library(enmSdm)
	library(omnibus)
	library(statisfactory)
	
	ll <- c('longitude', 'latitude')

	# designations for accurate, inaccurate, and county records
	accAssigns <- 'certain/precise'
	inaccAssigns <- c('uncertain/precise', 'certain/imprecise', 'uncertain/imprecise')
	adminAssigns <- c('county/precise', 'county/imprecise', 'county-only', 'state/imprecise', 'state-only')
	countyAssigns <- c('county/precise', 'county/imprecise', 'county-only')
	stateAssigns <- c('state/imprecise', 'state-only')

	# round y-axis upper limit to a nice number
	# quantile used to "zoom" in on results... some values are extremely high/low, so not showing these
	outlierQuant <- 0.999

	### figure captions

	enmCalibPresentCaption <- paste0('Figure S2.1 Calibration accuracy of ENMs against the true present-day probability of occurrence. Violin plots, and gray and green points represent the correlation between predictions from ENMs calibrated using omniscient and precise records (respectively) with the true probability of presence in the present. Green points represent the correlation between predictions from models calibrated using precise plus imprecise records and the true probability of presence. The thick black trend line traces the median trend and thin black lines bound the inner 90% of values. Higher values connote better calibration accuracy of ENMs.')
	
	enmCalibFutureCaption <- paste0('Figure S2.2 Calibration accuracy of ENMs against the true future-day probability of occurrence. Violin plots, and gray and green points represent the correlation between predictions from ENMs calibrated using omniscient and precise records (respectively) with the true probability of presence in the future. Green points represent the correlation between predictions from models calibrated using precise plus imprecise records and the true probability of presence. The thick black trend line traces the median trend and thin black lines bound the inner 90% of values. Higher values connote better calibration accuracy of ENMs.')

	lostCaption <- paste0('Figure S2.3 Climatically suitable area estimated using ecological niche models that is lost due to climate change. Violin plots and green points depict the ratio of lost suitable area estimated using only precise records to suitable area estimated using omniscient records.  Orange points represent the ratio of lost suitable area estimated using precise plus imprecise records to lost suitable area estimated using omniscient records. The thick black trend line traces the median trend and thin black lines bound the inner 90% of values. Ratios were calculated as (estimated value + 1) / (omniscient value + 1). To aid visualization, only the inner ', (1 - 2 * (1 - outlierQuant)) * 100, '% of values across all scenarios are shown.')

	gainCaption <- paste0('Figure S2.4 Climatically suitable area estimated using ecological niche models that is gained due to climate change. Violin plots and green points depict the ratio of gain in suitable area estimated using only precise records to gain in suitable area estimated using omniscient records.  Orange points represent the ratio of gained suitable area estimated using precise plus imprecise records to gained suitable area estimated using omniscient records. The thick black trend line traces the median trend and thin black lines bound the inner 90% of values. Ratios were calculated as (estimated value + 1) / (omniscient value + 1). To aid visualization, only the inner ', (1 - 2 * (1 - outlierQuant)) * 100, '% of values across all scenarios are shown.')
	
	stableCaption <- paste0('Figure S2.5 Climatically suitable area estimated using ecological niche models that remains suitable in time. Violin plots and green points depict the ratio of stable suitable area estimated using only precise records to suitable area estimated using omniscient records.  Orange points represent the ratio of stable suitable area estimated using precise plus imprecise records to stable suitable area estimated using omniscient records. The thick black trend line traces the median trend and thin black lines bound the inner 90% of values. Ratios were calculated as (estimated value + 1) / (omniscient value + 1). To aid visualization, only the inner ', (1 - 2 * (1 - outlierQuant)) * 100, '% of values across all scenarios are shown.')

	sqSuitCaption <- paste0('Figure S2.6 Present-day climatically suitable area estimated using ecological niche models. Violin plots and green points depict the ratio of present-day suitable area estimated using only precise records to suitable area estimated using omniscient records.  Orange points represent the ratio of present-day suitable area estimated using precise plus imprecise records to suitable area estimated using omniscient records. The thick black trend line traces the median trend and thin black lines bound the inner 90% of values. To aid visualization, only the inner ', (1 - 2 * (1 - outlierQuant)) * 100, '% of values across all scenarios are shown.')

	futSuitCaption <- paste0('Figure S2.7 Future climatically suitable area estimated using ecological niche models. Violin plots and green points depict the ratio of future suitable area estimated using only precise records to suitable area estimated using omniscient records.  Orange points represent the ratio of future suitable area estimated using precise plus imprecise records to suitable area estimated using omniscient records. The thick black trend line traces the median trend and thin black lines bound the inner 90% of values. Ratios were calculated as (estimated value + 1) / (omniscient value + 1). To aid visualization, only the inner ', (1 - 2 * (1 - outlierQuant)) * 100, '% of values across all scenarios are shown.')

	matCaption <- paste0('Figure S2.8 Estimates of realized niche breadth in mean annual temperature (MAT). In each panel the violin plot and green points show the ratio of realized niche breadth in MAT estimated using only precise records to the true realized niche breadth obtained from omniscient records. The violin plot encompasses the inner 90% of values and the horizontal line within represents the median value. The points and ratio of niche breadth estimated using precise plus imprecise records to true niche breadth (points). The thick black trend line traces the median trend and thin black lines bound the inner 90% of values. To assist in visualization, the top ', 100 * (1 - outlierQuant), '% of values across all scenarios are not shown.')

	tapCaption <- paste0('Figure S2.9 Estimates of realized niche breadth in total annual precipitation (TAP). In each panel the violin plot and green points show the ratio of realized niche breadth in TAP estimated using only precise records to the true realized niche breadth obtained from omniscient records. The violin plot encompasses the inner 90% of values and the horizontal line within represents the median value. The points and ratio of niche breadth estimated using precise plus imprecise records to true niche breadth (points). The thick black trend line traces the median trend and thin black lines bound the inner 90% of values.')

	volCaption <- paste0('Figure S2.10 Estimates of realized multivariate niche breadth volume. The violin plot and green points show the ratio of realized niche volume estimated using only precise records to the true realized niche volume obtained from omniscient records. The violin plot encompasses the inner 90% of values and the horizontal line within represents the median value. The orange points represent the ratio of niche volume estimated using precise plus imprecise records to true niche breadth. The thick black trend line traces the median trend and thin black lines bound the inner 90% of values. To assist in visualization, the top ', 100 * (1 - outlierQuant), '% of values across all scenarios are not shown.')

	areaCaption <- paste0('Figure S2.11 Estimates of realized multivariate niche surface area. The violin plot and green points show the ratio of realized niche area estimated using only precise records to the true realized niche area obtained from omniscient records. The violin plot encompasses the inner 90% of values and the horizontal line within represents the median value. The orange points represent the ratio of niche area estimated using precise plus imprecise records to true niche breadth. The thick black trend line traces the median trend and thin black lines bound the inner 90% of values. To assist in visualization, the top ', 100 * (1 - outlierQuant), '% of values across all scenarios are not shown.')
	
	eooCaption <- 'Figure S2.12 Estimates of extent of occurrence (EOO) compared to true EOO. The violin plot and green points depict the ratio of EOO estimated using only precise records to the true EOO obtained from omniscient records. The violin plot covers the inner 90% of estimates, and the median is represented by the horizontal bar within it. Orange points represent the ratio of EOO estimated using precise plus imprecise records to true EOO. The thick black trend line traces the median trend and thin black lines bound the inner 90% of values.'

	complexityCaption <- paste0('Figure S2.13 Model complexity measured as the number of non-zero coefficients, excluding the intercept. Violin plots, and gray and green points represent the number of coefficients of models using omniscient and precise records (respectively). Green points represent the number of coefficients of models using precise plus imprecise records. The thick black trend line traces the median trend and thin black lines bound the inner 90% of values. Higher values connote increased model complexity.')

	

	### create series of scenarios for species simulations
	######################################################
	makeSeries <- function() {
	
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
		
		series
		
	}

# say('#######################')
# say('### collate results ###')
# say('#######################')

	# # generalization
	# numReps <- 200

	# ### load results
	# files <- listFiles('./Analysis/Virtual Species/Raw Results', pattern='.csv')
	# results <- read.csv(files[1])
	# if (length(files) > 1) {
		# for (file in files[2:length(files)]) {
			# this <- read.csv(file)
			# results <- rbind(results, this)
		# }
	# }
	
	# # tally reps

	# series <- makeSeries()
	# series$n <- NA
	
	# for (i in 1:nrow(series)) {
		# these <- (results$numErrorless == series$totalErrorless[i] & results$numPrecise == series$totalPrecise[i] & results$numImprecise == series$totalImprecise[i])
		# series$n[i] <- sum(these)
	# }
	
	# if (!all(series$n == numReps)) say('Incomplete!!!')
	
	# print(series)
	
	# save(results, file='./Analysis/Virtual Species/!!Collated Results.rda')

say('###################################################################################################')
say('### make composite plot with SUBSET of EOO and niche breadth results - TWO omniscient scenarios ###')
say('###################################################################################################')
	
	# sets of panels, one per metric (EOO, MAT niche breadth, etc.), four panels per set crossing number of errorless and number of precise

	### generalization
	##################

		# text size
		setSize <- 12 # panel set title
		panelSize <- 9 # panel title
		axisSizeX <- 9 # axes with labels
		axisSizeY <- 9 # axes with labels
		axisNumSize <- 8 # numbers on axes
		
		# lines
		medianSize <- 1
		hlineSize <- 0.6 # thickness of horizontal line at 1
		boundaryLineThickness <- 0.4 # thickness of inner quantile boundary's lines
		
		# point size
		pointSize <- 0.25
		
		# colors
		pointCol <- '#d95f02'
		
		impreciseFill <- NA
		impreciseLineCol <- 'black'

		preciseFill <- '#1b9e77'
		precisePointColAlpha <- alpha('#1b9e77', 0.5)

		# quantile to define y-axis upper limit
		outlierQuant <- 0.9975 # quantile above which to remove points for setting y-axis limit

		# quantiles to plot
		lower <- 0.05
		upper <- 0.95
	
		# numErrorlessSelected <- c(320)
		numErrorlessSelected <- c(40, 320)
		# numErrorlessSelected <- c(40)
		
		# numPreciseSelected <- c(5, 30)
		numPreciseSelected <- c(5, 20)

	### preliminary
	###############

		load('./Analysis/Virtual Species/!!Collated Results.rda')
		results <- results[results$numErrorless %in% numErrorlessSelected & results$numPrecise %in% numPreciseSelected, ]

	### extent of occurrence
	########################

		### generalization

		subject <- 'EOO'
		title <- 'a) Extent of occurrence'
		errorless <- 'eooErrorless_km2'
		precise <- 'eooPrecise_km2'
		imprecise <- 'eooImprecise_km2'
		
		ylab <- 'Estimated / omniscient EOO'
		
		### y-axis limits
		these <- c(
			results[ , imprecise] / results[ , errorless],
			results[ , precise] / results[ , errorless]
		)
		ymin <- min(0, these)
		ymax <- max(these)
		ybreaks <- pretty(c(ymin, ymax), 3)
		
		panels <- list()
		count <- 0
		for (thisNumErrorless in numErrorlessSelected) {
		
			# subset number of precise so it's < total errorless
			allowableNumPreciseSelected <- numPreciseSelected[numPreciseSelected < thisNumErrorless]

			# x-axis limits
			these <- results[results$numErrorless == thisNumErrorless, ]
			xmax <- 1.015 * max(these$numAdmin)
			xmin <- 1 - 0.015 * xmax

			for (thisNumPrecise in allowableNumPreciseSelected) {

				say(subject, ' omniscient ', thisNumErrorless, ' precise ', thisNumPrecise)
				count <- count + 1

				# tall imprecise data
				thisResults <- results[results$numErrorless == thisNumErrorless & results$numPrecise == thisNumPrecise, ]
				resp <- thisResults[ , imprecise] / thisResults[ , errorless]
				n <- nrow(thisResults)
				impreciseDf <- data.frame(
					numAdmin = thisResults$numAdmin,
					resp = resp
				)
				
				# calculate quantiles for regression: imprecise
				impreciseDf <- impreciseDf[order(impreciseDf$numAdmin), ]
				k <- if (length(unique(impreciseDf$numAdmin)) <= 6) { 1 } else { -1 }
				middleQuant <- qgam(log(resp) ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=0.5)
				lowerQuant <- qgam(log(resp) ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=lower)
				upperQuant <- qgam(log(resp) ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=upper)
				
				middleQuant <- exp(predict(middleQuant, impreciseDf, type='response'))
				lowerQuant <- exp(predict(lowerQuant, impreciseDf, type='response'))
				upperQuant <- exp(predict(upperQuant, impreciseDf, type='response'))

				middleImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					middle = middleQuant
				)
				
				topBoundaryImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					boundary = upperQuant
				)
				
				bottomBoundaryImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					boundary = lowerQuant
				)

				# frames for violin plot of precise
				preciseDf <- data.frame(
					numAdmin = 0,
					resp = thisResults[ , precise] / thisResults[ , errorless]
				)
				preciseUpper <- quantile(preciseDf$resp, upper, na.rm=TRUE)
				preciseMiddle <- quantile(preciseDf$resp, 0.5, na.rm=TRUE)
				preciseLower <- quantile(preciseDf$resp, lower, na.rm=TRUE)

				# labels and titles
				main <- paste0(thisNumErrorless, ' omniscient, ', thisNumPrecise, ' precise')

				boxPanel <- ggplot(impreciseDf, aes(y=resp)) +
					geom_point(
						data=preciseDf,
						mapping=aes(x=0, y=resp),
						position = position_jitter(w = 0.4, h = 0),
						color = precisePointColAlpha,
						size = pointSize
					) +
					geom_violin(
						data = preciseDf[preciseDf$resp >= quantile(preciseDf$resp, lower, na.rm=TRUE) & preciseDf$resp <= quantile(preciseDf$resp, upper, na.rm=TRUE), ],
						mapping = aes(x=0, y = resp),
						draw_quantiles = 0.5,
						fill = NA
					) +
					geom_hline(yintercept = 1, size=0.4, linetype='solid', col='gray40') +
					scale_x_continuous(breaks=0, labels=c('Pre-\ncise')) +
					scale_y_continuous(breaks=ybreaks) +
					coord_cartesian(xlim=c(-0.55, 0.55), ylim=c(ymin, ymax), expand=FALSE) +
					ggtitle(main) +
					theme(
						plot.title = element_text(hjust = 0, vjust=-1, size=panelSize),
						plot.background = element_blank(),
						panel.grid.major.x = element_blank(),
						panel.grid.minor.x = element_blank(),
						axis.title.x = element_blank(),
						axis.text.x = element_text(angle = 0, hjust=0.5, vjust=0, size=axisSizeX),
						axis.title.y = element_blank(),
						axis.text.y = element_text(size=axisNumSize)
					)

				xbreaks <- pretty(c(1, xmax), 3)
				xbreaks[1] <- 1
					
				imprecisePanel <- ggplot(impreciseDf, aes(x=numAdmin, y=resp)) +
					geom_point(data=impreciseDf, color=pointCol, size=pointSize) +					
					geom_line(data=topBoundaryImprecise, mapping=aes(x=numAdmin, y=boundary), size=boundaryLineThickness) +
					geom_line(data=bottomBoundaryImprecise, mapping=aes(x=numAdmin, y=boundary), size=boundaryLineThickness) +
					geom_line(data=middleImprecise, mapping=aes(x=numAdmin, y=middle), col=impreciseLineCol, size=medianSize) +
					geom_hline(yintercept = 1, size=0.4, linetype='solid', col='gray40') +
					coord_cartesian(xlim=c(xmin, xmax), ylim=c(ymin, ymax), expand=FALSE) +
					xlab('County records') +
					scale_x_continuous(breaks = xbreaks) +
					theme(
						plot.background = element_blank(),
						plot.title = element_blank(),
						axis.title.x = element_text(size=axisSizeX, vjust=6),
						axis.text.x = element_text(size=axisNumSize),
						axis.title.y = element_blank(),
						axis.text.y = element_blank(),
						axis.ticks.y = element_blank()
					)

				boxPanel <- boxPanel + theme(plot.margin = grid::unit(c(0, 0, 0, 0.1), 'cm'))
				imprecisePanel <- imprecisePanel + theme(plot.margin = grid::unit(c(0, 0.2, 0, 0), 'cm'))

				comboPanel <- plot_grid(boxPanel, imprecisePanel, ncol=2, rel_widths=c(0.35, 1), align='h')
				if (length(panels) == 1) comboPanel <- comboPanel +
					geom_text(aes(x=0, y=1, label='Estimated / omniscient EOO', angle=90, vjust=-1, size=axisSizeY))

				panels[[length(panels) + 1]] <- comboPanel
				names(panels)[length(panels)] <- paste0('errorless', thisNumErrorless, '_precise', thisNumPrecise)
				
			} # next number of precise
			
		} # next number of errorless

		eoo <- (panels[[1]] + panels[[3]]) / (panels[[2]] + panels[[4]]) +
			plot_layout(nrow=2) +
			plot_annotation(
				title = title,
				theme=theme(
					plot.title = element_text(size=setSize),
					plot.margin = grid::unit(c(0, 0, 0.1, 0.4), 'in')
				)
			)

	### univariate niche breadth: MAT
	#################################

		### generalization

		subject <- 'MAT'
		title <- 'b) Univariate niche breadth in temperature'
		errorless <- 'errorlessNicheBreadthMat_range_degC'
		precise <- 'preciseNicheBreadthMat_range_degC'
		imprecise <- 'impreciseNicheBreadthMat_range_degC'
		
		ylab <- 'Estimated / omniscient MAT niche breadth'
		
		### y-axis limits
		these <- c(
			results[ , imprecise] / results[ , errorless],
			results[ , precise] / results[ , errorless]
		)
		ymin <- min(0, these)
		ymax <- quantile(these, outlierQuant)
		ybreaks <- pretty(c(ymin, ymax), 3)
		
		panels <- list()
		for (thisNumErrorless in numErrorlessSelected) {
		
			# subset number of precise so it's < total errorless
			allowableNumPreciseSelected <- numPreciseSelected[numPreciseSelected < thisNumErrorless]

			# x-axis limits
			these <- results[results$numErrorless == thisNumErrorless, ]
			xmax <- 1.015 * max(these$numAdmin)
			xmin <- 1 - 0.015 * xmax

			for (thisNumPrecise in allowableNumPreciseSelected) {

				say(subject, ' omniscient ', thisNumErrorless, ' precise ', thisNumPrecise)
				count <- count + 1

				# tall imprecise data
				thisResults <- results[results$numErrorless == thisNumErrorless & results$numPrecise == thisNumPrecise, ]
				resp <- thisResults[ , imprecise] / thisResults[ , errorless]
				n <- nrow(thisResults)
				impreciseDf <- data.frame(
					numAdmin = thisResults$numAdmin,
					resp = resp
				)
				
				# calculate quantiles for regression: imprecise
				impreciseDf <- impreciseDf[order(impreciseDf$numAdmin), ]
				k <- if (length(unique(impreciseDf$numAdmin)) <= 6) { 1 } else { -1 }
				middleQuant <- qgam(log(resp) ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=0.5)
				lowerQuant <- qgam(log(resp) ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=lower)
				upperQuant <- qgam(log(resp) ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=upper)
				
				middleQuant <- exp(predict(middleQuant, impreciseDf, type='response'))
				lowerQuant <- exp(predict(lowerQuant, impreciseDf, type='response'))
				upperQuant <- exp(predict(upperQuant, impreciseDf, type='response'))

				middleImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					middle = middleQuant
				)
				
				topBoundaryImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					boundary = upperQuant
				)
				
				bottomBoundaryImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					boundary = lowerQuant
				)

				# frames for violin plot of precise
				preciseDf <- data.frame(
					numAdmin = 0,
					resp = thisResults[ , precise] / thisResults[ , errorless]
				)
				preciseUpper <- quantile(preciseDf$resp, upper, na.rm=TRUE)
				preciseMiddle <- quantile(preciseDf$resp, 0.5, na.rm=TRUE)
				preciseLower <- quantile(preciseDf$resp, lower, na.rm=TRUE)

				# labels and titles
				main <- paste0(thisNumErrorless, ' omniscient, ', thisNumPrecise, ' precise')

				boxPanel <- ggplot(impreciseDf, aes(y=resp)) +
					geom_point(
						data=preciseDf,
						mapping=aes(x=0, y=resp),
						position = position_jitter(w = 0.4, h = 0),
						color = precisePointColAlpha,
						size = pointSize
					) +
					geom_violin(
						data = preciseDf[preciseDf$resp >= preciseLower & preciseDf$resp <= preciseUpper, , drop=FALSE],
						mapping = aes(x=-0, y = resp),
						draw_quantiles = 0.5,
						fill = NA, fatten=medianSize
					) +
					geom_hline(yintercept = 1, size=0.4, linetype='solid', col='gray40') +
					scale_x_continuous(breaks=0, labels=c('Pre-\ncise')) +
					scale_y_continuous(breaks=ybreaks) +
					coord_cartesian(xlim=c(-0.55, 0.55), ylim=c(ymin, ymax), expand=FALSE) +
					ggtitle(main) +
					theme(
						plot.title = element_text(hjust = 0, vjust=-1, size=panelSize),
						plot.background = element_blank(),
						panel.grid.major.x = element_blank(),
						panel.grid.minor.x = element_blank(),
						axis.title.x = element_blank(),
						axis.text.x = element_text(angle = 0, hjust=0.5, vjust=0, size=axisSizeX),
						axis.title.y = element_blank(),
						axis.text.y = element_text(size=axisNumSize)
					)

				xbreaks <- pretty(c(1, xmax), 3)
				xbreaks[1] <- 1
					
				imprecisePanel <- ggplot(impreciseDf, aes(x=numAdmin, y=resp)) +
					geom_point(data=impreciseDf, color=pointCol, size=pointSize) +					
					geom_line(data=topBoundaryImprecise, mapping=aes(x=numAdmin, y=boundary), size=boundaryLineThickness) +
					geom_line(data=bottomBoundaryImprecise, mapping=aes(x=numAdmin, y=boundary), size=boundaryLineThickness) +
					geom_line(data=middleImprecise, mapping=aes(x=numAdmin, y=middle), col=impreciseLineCol, size=medianSize) +
					geom_hline(yintercept = 1, size=0.4, linetype='solid', col='gray40') +
					coord_cartesian(xlim=c(xmin, xmax), ylim=c(ymin, ymax), expand=FALSE) +
					xlab('County records') +
					scale_x_continuous(breaks = xbreaks) +
					theme(
						plot.background = element_blank(),
						plot.title = element_blank(),
						axis.title.x = element_text(size=axisSizeX, vjust=6),
						axis.text.x = element_text(size=axisNumSize),
						axis.title.y = element_blank(),
						axis.text.y = element_blank(),
						axis.ticks.y = element_blank()
					)

				boxPanel <- boxPanel + theme(plot.margin = grid::unit(c(0, 0, 0, 0.1), 'cm'))
				imprecisePanel <- imprecisePanel + theme(plot.margin = grid::unit(c(0, 0.2, 0, 0), 'cm'))

				comboPanel <- plot_grid(boxPanel, imprecisePanel, ncol=2, rel_widths=c(0.35, 1), align='h')
				if (length(panels) == 1) comboPanel <- comboPanel +
					geom_text(aes(x=0, y=1, label='Estimated / omniscient niche breath', angle=90, vjust=-1, size=axisSizeY))

				panels[[length(panels) + 1]] <- comboPanel
				names(panels)[length(panels)] <- paste0('errorless', thisNumErrorless, '_precise', thisNumPrecise)
				
			} # next number of precise
			
		} # next number of errorless

		mat <- (panels[[1]] + panels[[3]]) / (panels[[2]] + panels[[4]]) +
			plot_layout(nrow=2) +
			plot_annotation(
				title = title,
				theme=theme(
					plot.title = element_text(size=setSize),
					plot.margin = grid::unit(c(0, 0, 0.1, 0.4), 'in')
				)
			)

	### univariate niche breadth: TAP
	#################################

		### generalization

		subject <- 'TAP'
		title <- 'c) Univariate niche breadth in precipitation'
		errorless <- 'errorlessNicheBreadthTap_range_mm'
		precise <- 'preciseNicheBreadthTap_range_mm'
		imprecise <- 'impreciseNicheBreadthTap_range_mm'
		
		ylab <- 'Estimated / omniscient TAP niche breadth'
		
		### y-axis limits
		these <- c(
			results[ , imprecise] / results[ , errorless],
			results[ , precise] / results[ , errorless]
		)
		ymin <- min(0, these)
		ymax <- quantile(these, outlierQuant)
		ybreaks <- pretty(c(ymin, ymax), 3)
		
		panels <- list()
		for (thisNumErrorless in numErrorlessSelected) {
		
			# subset number of precise so it's < total errorless
			allowableNumPreciseSelected <- numPreciseSelected[numPreciseSelected < thisNumErrorless]

			# x-axis limits
			these <- results[results$numErrorless == thisNumErrorless, ]
			xmax <- 1.015 * max(these$numAdmin)
			xmin <- 1 - 0.015 * xmax

			for (thisNumPrecise in allowableNumPreciseSelected) {

				say(subject, ' omniscient ', thisNumErrorless, ' precise ', thisNumPrecise)
				count <- count + 1

				# tall imprecise data
				thisResults <- results[results$numErrorless == thisNumErrorless & results$numPrecise == thisNumPrecise, ]
				resp <- thisResults[ , imprecise] / thisResults[ , errorless]
				n <- nrow(thisResults)
				impreciseDf <- data.frame(
					numAdmin = thisResults$numAdmin,
					resp = resp
				)
				
				# calculate quantiles for regression: imprecise
				impreciseDf <- impreciseDf[order(impreciseDf$numAdmin), ]
				k <- if (length(unique(impreciseDf$numAdmin)) <= 6) { 1 } else { -1 }
				middleQuant <- qgam(log(resp) ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=0.5)
				lowerQuant <- qgam(log(resp) ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=lower)
				upperQuant <- qgam(log(resp) ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=upper)
				
				middleQuant <- exp(predict(middleQuant, impreciseDf, type='response'))
				lowerQuant <- exp(predict(lowerQuant, impreciseDf, type='response'))
				upperQuant <- exp(predict(upperQuant, impreciseDf, type='response'))

				middleImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					middle = middleQuant
				)
				
				topBoundaryImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					boundary = upperQuant
				)
				
				bottomBoundaryImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					boundary = lowerQuant
				)

				# frames for violin plot of precise
				preciseDf <- data.frame(
					numAdmin = 0,
					resp = thisResults[ , precise] / thisResults[ , errorless]
				)
				preciseUpper <- quantile(preciseDf$resp, upper, na.rm=TRUE)
				preciseMiddle <- quantile(preciseDf$resp, 0.5, na.rm=TRUE)
				preciseLower <- quantile(preciseDf$resp, lower, na.rm=TRUE)

				# labels and titles
				main <- paste0(thisNumErrorless, ' omniscient, ', thisNumPrecise, ' precise')

				boxPanel <- ggplot(impreciseDf, aes(y=resp)) +
					geom_point(
						data=preciseDf,
						mapping=aes(x=0, y=resp),
						position = position_jitter(w = 0.4, h = 0),
						color = precisePointColAlpha,
						size = pointSize
					) +
					geom_violin(
						data = preciseDf[preciseDf$resp >= preciseLower & preciseDf$resp <= preciseUpper, , drop=FALSE],
						mapping = aes(x=-0, y = resp),
						draw_quantiles = 0.5,
						fill = NA, fatten=medianSize
					) +
					geom_hline(yintercept = 1, size=0.4, linetype='solid', col='gray40') +
					scale_x_continuous(breaks=0, labels=c('Pre-\ncise')) +
					scale_y_continuous(breaks=ybreaks) +
					coord_cartesian(xlim=c(-0.55, 0.55), ylim=c(ymin, ymax), expand=FALSE) +
					ggtitle(main) +
					theme(
						plot.title = element_text(hjust = 0, vjust=-1, size=panelSize),
						plot.background = element_blank(),
						panel.grid.major.x = element_blank(),
						panel.grid.minor.x = element_blank(),
						axis.title.x = element_blank(),
						axis.text.x = element_text(angle = 0, hjust=0.5, vjust=0, size=axisSizeX),
						axis.title.y = element_blank(),
						axis.text.y = element_text(size=axisNumSize)
					)

				xbreaks <- pretty(c(1, xmax), 3)
				xbreaks[1] <- 1
					
				imprecisePanel <- ggplot(impreciseDf, aes(x=numAdmin, y=resp)) +
					geom_point(data=impreciseDf, color=pointCol, size=pointSize) +					
					geom_line(data=topBoundaryImprecise, mapping=aes(x=numAdmin, y=boundary), size=boundaryLineThickness) +
					geom_line(data=bottomBoundaryImprecise, mapping=aes(x=numAdmin, y=boundary), size=boundaryLineThickness) +
					geom_line(data=middleImprecise, mapping=aes(x=numAdmin, y=middle), col=impreciseLineCol, size=medianSize) +
					geom_hline(yintercept = 1, size=0.4, linetype='solid', col='gray40') +
					coord_cartesian(xlim=c(xmin, xmax), ylim=c(ymin, ymax), expand=FALSE) +
					xlab('County records') +
					scale_x_continuous(breaks = xbreaks) +
					theme(
						plot.background = element_blank(),
						plot.title = element_blank(),
						axis.title.x = element_text(size=axisSizeX, vjust=6),
						axis.text.x = element_text(size=axisNumSize),
						axis.title.y = element_blank(),
						axis.text.y = element_blank(),
						axis.ticks.y = element_blank()
					)

				boxPanel <- boxPanel + theme(plot.margin = grid::unit(c(0, 0, 0, 0.1), 'cm'))
				imprecisePanel <- imprecisePanel + theme(plot.margin = grid::unit(c(0, 0.2, 0, 0), 'cm'))

				comboPanel <- plot_grid(boxPanel, imprecisePanel, ncol=2, rel_widths=c(0.35, 1), align='h')
				if (length(panels) == 1) comboPanel <- comboPanel +
					geom_text(aes(x=0, y=1, label='Estimated / omniscient niche breath', angle=90, vjust=-1, size=axisSizeY))

				panels[[length(panels) + 1]] <- comboPanel
				names(panels)[length(panels)] <- paste0('errorless', thisNumErrorless, '_precise', thisNumPrecise)
				
			} # next number of precise
			
		} # next number of errorless

		tap <- (panels[[1]] + panels[[3]]) / (panels[[2]] + panels[[4]]) +
			plot_layout(nrow=2) +
			plot_annotation(
				title = title,
				theme=theme(
					plot.title = element_text(size=setSize),
					plot.margin = grid::unit(c(0, 0, 0.1, 0.4), 'in')
				)
			)
			
	### multivariate niche breadth: volume
	######################################

		### generalization

		subject <- 'VOLUME'
		title <- 'd) Multivariate niche volume'
		errorless <- 'errorlessConvHullVol'
		precise <- 'preciseConvHullVol'
		imprecise <- 'impreciseConvHullVol'
		
		ylab <- 'Estimated / omniscient niche volume'
		
		### y-axis limits
		these <- c(
			results[ , imprecise] / results[ , errorless],
			results[ , precise] / results[ , errorless]
		)
		ymin <- 0
		ymax <- quantile(these, outlierQuant)
		ybreaks <- pretty(c(ymin, ymax), 3)
		
		panels <- list()
		for (thisNumErrorless in numErrorlessSelected) {
		
			# subset number of precise so it's < total errorless
			allowableNumPreciseSelected <- numPreciseSelected[numPreciseSelected < thisNumErrorless]

			# x-axis limits
			these <- results[results$numErrorless == thisNumErrorless, ]
			xmax <- 1.015 * max(these$numAdmin)
			xmin <- 1 - 0.015 * xmax

			for (thisNumPrecise in allowableNumPreciseSelected) {

				say(subject, ' omniscient ', thisNumErrorless, ' precise ', thisNumPrecise)
				count <- count + 1

				# tall imprecise data
				thisResults <- results[results$numErrorless == thisNumErrorless & results$numPrecise == thisNumPrecise, ]
				resp <- thisResults[ , imprecise] / thisResults[ , errorless]
				n <- nrow(thisResults)
				impreciseDf <- data.frame(
					numAdmin = thisResults$numAdmin,
					resp = resp
				)
				
				# calculate quantiles for regression: imprecise
				impreciseDf <- impreciseDf[order(impreciseDf$numAdmin), ]
				k <- if (length(unique(impreciseDf$numAdmin)) <= 6) { 1 } else { -1 }
				middleQuant <- qgam(resp ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=0.5)
				lowerQuant <- qgam(resp ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=lower)
				upperQuant <- qgam(resp ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=upper)
				
				middleQuant <- (predict(middleQuant, impreciseDf, type='response'))
				lowerQuant <- (predict(lowerQuant, impreciseDf, type='response'))
				upperQuant <- (predict(upperQuant, impreciseDf, type='response'))

				middleImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					middle = middleQuant
				)
				
				topBoundaryImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					boundary = upperQuant
				)
				
				bottomBoundaryImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					boundary = lowerQuant
				)

				# frames for violin plot of precise
				preciseDf <- data.frame(
					numAdmin = 0,
					resp = thisResults[ , precise] / thisResults[ , errorless]
				)
				preciseUpper <- quantile(preciseDf$resp, upper, na.rm=TRUE)
				preciseMiddle <- quantile(preciseDf$resp, 0.5, na.rm=TRUE)
				preciseLower <- quantile(preciseDf$resp, lower, na.rm=TRUE)

				# labels and titles
				main <- paste0(thisNumErrorless, ' omniscient, ', thisNumPrecise, ' precise')

				boxPanel <- ggplot(impreciseDf, aes(y=resp)) +
					geom_point(
						data=preciseDf,
						mapping=aes(x=0, y=resp),
						position = position_jitter(w = 0.4, h = 0),
						color = precisePointColAlpha,
						size = pointSize
					) +
					geom_violin(
						data = preciseDf[preciseDf$resp >= preciseLower & preciseDf$resp <= preciseUpper, , drop=FALSE],
						mapping = aes(x=-0, y = resp),
						draw_quantiles = 0.5,
						fill = NA, fatten=medianSize
					) +
					geom_hline(yintercept = 1, size=0.4, linetype='solid', col='gray40') +
					scale_x_continuous(breaks=0, labels=c('Pre-\ncise')) +
					scale_y_continuous(breaks=ybreaks) +
					coord_cartesian(xlim=c(-0.55, 0.55), ylim=c(ymin, ymax), expand=FALSE) +
					ggtitle(main) +
					theme(
						plot.title = element_text(hjust = 0, vjust=-1, size=panelSize),
						plot.background = element_blank(),
						panel.grid.major.x = element_blank(),
						panel.grid.minor.x = element_blank(),
						axis.title.x = element_blank(),
						axis.text.x = element_text(angle = 0, hjust=0.5, vjust=0, size=axisSizeX),
						axis.title.y = element_blank(),
						axis.text.y = element_text(size=axisNumSize)
					)

				xbreaks <- pretty(c(1, xmax), 3)
				xbreaks[1] <- 1
					
				imprecisePanel <- ggplot(impreciseDf, aes(x=numAdmin, y=resp)) +
					geom_point(data=impreciseDf, color=pointCol, size=pointSize) +					
					geom_line(data=topBoundaryImprecise, mapping=aes(x=numAdmin, y=boundary), size=boundaryLineThickness) +
					geom_line(data=bottomBoundaryImprecise, mapping=aes(x=numAdmin, y=boundary), size=boundaryLineThickness) +
					geom_line(data=middleImprecise, mapping=aes(x=numAdmin, y=middle), col=impreciseLineCol, size=medianSize) +
					geom_hline(yintercept = 1, size=0.4, linetype='solid', col='gray40') +
					coord_cartesian(xlim=c(xmin, xmax), ylim=c(ymin, ymax), expand=FALSE) +
					xlab('County records') +
					scale_x_continuous(breaks = xbreaks) +
					theme(
						plot.background = element_blank(),
						plot.title = element_blank(),
						axis.title.x = element_text(size=axisSizeX, vjust=6),
						axis.text.x = element_text(size=axisNumSize),
						axis.title.y = element_blank(),
						axis.text.y = element_blank(),
						axis.ticks.y = element_blank()
					)

				boxPanel <- boxPanel + theme(plot.margin = grid::unit(c(0, 0, 0, 0.1), 'cm'))
				imprecisePanel <- imprecisePanel + theme(plot.margin = grid::unit(c(0, 0.2, 0, 0), 'cm'))

				comboPanel <- plot_grid(boxPanel, imprecisePanel, ncol=2, rel_widths=c(0.35, 1), align='h')
				if (length(panels) == 1) comboPanel <- comboPanel +
					geom_text(aes(x=0, y=1, label='Estimated / omniscient niche volume', angle=90, vjust=-1, size=axisSizeY))

				panels[[length(panels) + 1]] <- comboPanel
				names(panels)[length(panels)] <- paste0('errorless', thisNumErrorless, '_precise', thisNumPrecise)
				
			} # next number of precise
			
		} # next number of errorless

		vol <- (panels[[1]] + panels[[3]]) / (panels[[2]] + panels[[4]]) +
			plot_layout(nrow=2) +
			plot_annotation(
				title = title,
				theme=theme(
					plot.title = element_text(size=setSize),
					plot.margin = grid::unit(c(0, 0, 0.1, 0.4), 'in')
				)
			)
			
	### multivariate niche breadth: area
	####################################

		### generalization

		subject <- 'AREA'
		title <- 'e) Multivariate niche surface area'
		errorless <- 'errorlessConvHullArea'
		precise <- 'preciseConvHullArea'
		imprecise <- 'impreciseConvHullArea'
		
		ylab <- 'Estimated / omniscient niche area'
		
		### y-axis limits
		these <- c(
			results[ , imprecise] / results[ , errorless],
			results[ , precise] / results[ , errorless]
		)
		ymin <- 0
		ymax <- max(these)
		ybreaks <- pretty(c(ymin, ymax), 3)
		
		panels <- list()
		for (thisNumErrorless in numErrorlessSelected) {
		
			# subset number of precise so it's < total errorless
			allowableNumPreciseSelected <- numPreciseSelected[numPreciseSelected < thisNumErrorless]

			# x-axis limits
			these <- results[results$numErrorless == thisNumErrorless, ]
			xmax <- 1.015 * max(these$numAdmin)
			xmin <- 1 - 0.015 * xmax

			for (thisNumPrecise in allowableNumPreciseSelected) {

				say(subject, ' omniscient ', thisNumErrorless, ' precise ', thisNumPrecise)
				count <- count + 1

				# tall imprecise data
				thisResults <- results[results$numErrorless == thisNumErrorless & results$numPrecise == thisNumPrecise, ]
				resp <- thisResults[ , imprecise] / thisResults[ , errorless]
				n <- nrow(thisResults)
				impreciseDf <- data.frame(
					numAdmin = thisResults$numAdmin,
					resp = resp
				)
				
				# calculate quantiles for regression: imprecise
				impreciseDf <- impreciseDf[order(impreciseDf$numAdmin), ]
				k <- if (length(unique(impreciseDf$numAdmin)) <= 6) { 1 } else { -1 }
				middleQuant <- qgam(log(resp) ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=0.5)
				lowerQuant <- qgam(log(resp) ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=lower)
				upperQuant <- qgam(log(resp) ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=upper)
				
				middleQuant <- exp(predict(middleQuant, impreciseDf, type='response'))
				lowerQuant <- exp(predict(lowerQuant, impreciseDf, type='response'))
				upperQuant <- exp(predict(upperQuant, impreciseDf, type='response'))

				middleImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					middle = middleQuant
				)
				
				topBoundaryImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					boundary = upperQuant
				)
				
				bottomBoundaryImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					boundary = lowerQuant
				)

				# frames for violin plot of precise
				preciseDf <- data.frame(
					numAdmin = 0,
					resp = thisResults[ , precise] / thisResults[ , errorless]
				)
				preciseUpper <- quantile(preciseDf$resp, upper, na.rm=TRUE)
				preciseMiddle <- quantile(preciseDf$resp, 0.5, na.rm=TRUE)
				preciseLower <- quantile(preciseDf$resp, lower, na.rm=TRUE)

				# labels and titles
				main <- paste0(thisNumErrorless, ' omniscient, ', thisNumPrecise, ' precise')

				boxPanel <- ggplot(impreciseDf, aes(y=resp)) +
					geom_point(
						data=preciseDf,
						mapping=aes(x=0, y=resp),
						position = position_jitter(w = 0.4, h = 0),
						color = precisePointColAlpha,
						size = pointSize
					) +
					geom_violin(
						data = preciseDf[preciseDf$resp >= preciseLower & preciseDf$resp <= preciseUpper, , drop=FALSE],
						mapping = aes(x=-0, y = resp),
						draw_quantiles = 0.5,
						fill = NA, fatten=medianSize
					) +
					geom_hline(yintercept = 1, size=0.4, linetype='solid', col='gray40') +
					scale_x_continuous(breaks=0, labels=c('Pre-\ncise')) +
					scale_y_continuous(breaks=ybreaks) +
					coord_cartesian(xlim=c(-0.55, 0.55), ylim=c(ymin, ymax), expand=FALSE) +
					ggtitle(main) +
					theme(
						plot.title = element_text(hjust = 0, vjust=-1, size=panelSize),
						plot.background = element_blank(),
						panel.grid.major.x = element_blank(),
						panel.grid.minor.x = element_blank(),
						axis.title.x = element_blank(),
						axis.text.x = element_text(angle = 0, hjust=0.5, vjust=0, size=axisSizeX),
						axis.title.y = element_blank(),
						axis.text.y = element_text(size=axisNumSize)
					)

				xbreaks <- pretty(c(1, xmax), 3)
				xbreaks[1] <- 1
					
				imprecisePanel <- ggplot(impreciseDf, aes(x=numAdmin, y=resp)) +
					geom_point(data=impreciseDf, color=pointCol, size=pointSize) +					
					geom_line(data=topBoundaryImprecise, mapping=aes(x=numAdmin, y=boundary), size=boundaryLineThickness) +
					geom_line(data=bottomBoundaryImprecise, mapping=aes(x=numAdmin, y=boundary), size=boundaryLineThickness) +
					geom_line(data=middleImprecise, mapping=aes(x=numAdmin, y=middle), col=impreciseLineCol, size=medianSize) +
					geom_hline(yintercept = 1, size=0.4, linetype='solid', col='gray40') +
					coord_cartesian(xlim=c(xmin, xmax), ylim=c(ymin, ymax), expand=FALSE) +
					xlab('County records') +
					scale_x_continuous(breaks = xbreaks) +
					theme(
						plot.background = element_blank(),
						plot.title = element_blank(),
						axis.title.x = element_text(size=axisSizeX, vjust=6),
						axis.text.x = element_text(size=axisNumSize),
						axis.title.y = element_blank(),
						axis.text.y = element_blank(),
						axis.ticks.y = element_blank()
					)

				boxPanel <- boxPanel + theme(plot.margin = grid::unit(c(0, 0, 0, 0.1), 'cm'))
				imprecisePanel <- imprecisePanel + theme(plot.margin = grid::unit(c(0, 0.2, 0, 0), 'cm'))
				
				comboPanel <- plot_grid(boxPanel, imprecisePanel, ncol=2, rel_widths=c(0.35, 1), align='h')
				if (length(panels) == 1) comboPanel <- comboPanel +
					geom_text(aes(x=0, y=1, label='Estimated / omniscient niche area', angle=90, vjust=-1, size=axisSizeY))

				panels[[length(panels) + 1]] <- comboPanel
				names(panels)[length(panels)] <- paste0('errorless', thisNumErrorless, '_precise', thisNumPrecise)
				
			} # next number of precise
			
		} # next number of errorless

		area <- (panels[[1]] + panels[[3]]) / (panels[[2]] + panels[[4]]) +
			plot_layout(nrow=2) +
			plot_annotation(
				title = title,
				theme=theme(
					plot.title = element_text(size=setSize),
					plot.margin = grid::unit(c(0, 0, 0.1, 0.4), 'in')
				)
			)

	### combine
	###########
	
		main <- plot_grid(eoo, NULL, mat, tap, vol, area, ncol=2)
		main <- main + theme(plot.margin = grid::unit(c(0.5, 0.5, 0.5, 0.5), 'in'))
		
		ggsave(plot=main, filename=paste0('./Analysis/Virtual Species/!Selected EOO & Niche Breadth ', paste(numErrorlessSelected, collapse=' '), ' Omniscient & ', paste(numPreciseSelected, collapse=' '), ' Precise.pdf'), width=8.5, height=11, units='in')
		
		say('Displaying only lower ', outlierQuant, 'quantile of points.', level=2, deco='!')
		
say('################################################################################')
say('### make composite plot with SUBSET of ENM results - TWO errorless scenarios ###')
say('################################################################################')
	
	# sets of panels, one per metric (calibration, area gain, etc.), four panels per set crossing number of errorless and number of precise

	### generalization
	##################

		# text size
		setSize <- 12 # panel set title
		panelSize <- 9 # panel title
		axisSizeX <- 9 # axes with labels
		axisSizeY <- 9 # axes with labels
		axisNumSize <- 8 # numbers on axes
		
		# lines
		medianSize <- 1
		hlineSize <- 0.6 # thickness of horizontal line at 1
		boundaryLineThickness <- 0.4 # thickness of inner quantile boundary's lines
		
		# point size
		pointSize <- 0.25
		
		# colors
		imprecisePointCol <- '#d95f02'
		
		impreciseLineCol <- 'black'

		errorlessPointColAlpha <- alpha('gray50', 0.5)

		preciseFill <- '#1b9e77'
		precisePointColAlpha <- alpha('#1b9e77', 0.5)

		# quantile to define y-axis upper limit
		outlierQuant <- 0.9975 # quantile above which to remove points for setting y-axis limit

		# quantiles to plot
		lower <- 0.05
		upper <- 0.95
	
		# numErrorlessSelected <- c(320)
		# numErrorlessSelected <- c(40, 160)
		numErrorlessSelected <- c(40, 320)
		# numErrorlessSelected <- c(40)
		
		# numPreciseSelected <- c(5, 30)
		numPreciseSelected <- c(5, 20)

	### preliminary
	###############

		load('./Analysis/Virtual Species/!!Collated Results.rda')
		results <- results[results$numErrorless %in% numErrorlessSelected & results$numPrecise %in% numPreciseSelected, ]

	### ENM calibration: present
	############################

		### generalization

		subject <- 'ENM CALIBRATION SQ'
		title <- 'a) ENM calibration in the present'
		errorless <- 'corTruthVsErrorlessSq'
		precise <- 'corTruthVsPreciseSq'
		imprecise <- 'corTruthVsImpreciseSq'
	
		ylab <- 'Calibration accuracy'
		
		### y-axis limits
		these <- c(
			results[ , 'corTruthVsErrorlessSq'], results[ , 'corTruthVsPreciseSq'], results[ , 'corTruthVsImpreciseSq'],
			results[ , 'corTruthVsErrorlessFut'], results[ , 'corTruthVsPreciseFut'], results[ , 'corTruthVsImpreciseFut']
		)
		ymin <- min(0, these, na.rm=TRUE)
		ymin <- roundTo(ymin, 0.2, floor)
		ymax <- 1
		ybreaks <- pretty(c(ymin, ymax))
		
		panels <- list()
		count <- 0
		for (thisNumErrorless in numErrorlessSelected) {
		
			# subset number of precise so it's < total errorless
			allowableNumPreciseSelected <- numPreciseSelected[numPreciseSelected < thisNumErrorless]

			# x-axis limits
			these <- results[results$numErrorless == thisNumErrorless, ]
			xmax <- 1.015 * max(these$numAdmin)
			xmin <- 1 - 0.015 * xmax

			for (thisNumPrecise in allowableNumPreciseSelected) {

				say(subject, ' omniscient ', thisNumErrorless, ' precise ', thisNumPrecise)

				# tall imprecise data
				thisResults <- results[results$numErrorless == thisNumErrorless & results$numPrecise == thisNumPrecise, ]
				resp <- thisResults[ , imprecise]
				n <- nrow(thisResults)
				impreciseDf <- data.frame(
					numAdmin = thisResults$numAdmin,
					resp = resp
				)
				
				# calculate quantiles for regression: imprecise
				impreciseDf <- impreciseDf[order(impreciseDf$numAdmin), ]
				k <- if (length(unique(impreciseDf$numAdmin)) <= 6) { 1 } else { -1 }
				middleQuant <- qgam(resp ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=0.5)
				lowerQuant <- qgam(resp ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=lower)
				upperQuant <- qgam(resp ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=upper)
				
				middleQuant <- (predict(middleQuant, impreciseDf))
				lowerQuant <- (predict(lowerQuant, impreciseDf))
				upperQuant <- (predict(upperQuant, impreciseDf))

				middleImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					middle = middleQuant
				)
				
				topBoundaryImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					boundary = upperQuant
				)
				
				bottomBoundaryImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					boundary = lowerQuant
				)

				# frames for violin plot of precise
				errorlessPreciseDf <- data.frame(
					x = rep(c(-0.5, 0.5), each=nrow(thisResults)),
					type = rep(c('errorless', 'precise'), each=nrow(thisResults)),
					resp = c(thisResults[ , errorless], thisResults[ , precise])
				)
				errorlessDf <- data.frame(resp = thisResults[ , errorless])
				preciseDf <- data.frame(resp = thisResults[ , precise])

				errorlessLower <- quantile(errorlessDf$resp, lower, na.rm=TRUE)
				errorlessUpper <- quantile(errorlessDf$resp, upper, na.rm=TRUE)
				preciseLower <- quantile(preciseDf$resp, lower, na.rm=TRUE)
				preciseUpper <- quantile(preciseDf$resp, upper, na.rm=TRUE)

				# labels and titles
				main <- paste0(thisNumErrorless, ' omniscient, ', thisNumPrecise, ' precise')

				boxPanel <- ggplot(errorlessPreciseDf, aes(x=x, y=resp)) +
					geom_point(
						data=errorlessPreciseDf,
						mapping=aes(x=x, y=resp, color=type, group=type),
						position = position_jitter(w = 0.4, h = 0),
						size = pointSize
					) +
					scale_color_manual(values=c('errorless' = errorlessPointColAlpha, 'precise' = precisePointColAlpha)) +
					geom_violin(
						data = errorlessDf[errorlessDf$resp >= errorlessLower & errorlessDf$resp <= errorlessUpper, , drop=FALSE],
						mapping = aes(x=-0.5, y = resp),
						draw_quantiles = 0.5,
						fill = NA, fatten=medianSize
					) +
					geom_violin(
						data = preciseDf[preciseDf$resp >= preciseLower & preciseDf$resp <= preciseUpper, , drop=FALSE],
						mapping = aes(x=0.5, y = resp),
						draw_quantiles = 0.5,
						fill = NA
					) +
					scale_x_continuous(breaks=c(-0.5, 0.5), labels=c('Omni', 'Prec')) +
					scale_y_continuous(breaks=ybreaks) +
					coord_cartesian(xlim=c(-1, 1), ylim=c(ymin, ymax), expand=FALSE) +
					ggtitle(main) +
					theme(
						plot.title = element_text(hjust = 0, vjust=-1, size=panelSize),
						plot.background = element_blank(),
						panel.grid.major.x = element_blank(),
						panel.grid.minor.x = element_blank(),
						axis.title.x = element_blank(),
						axis.text.x = element_text(angle = 90, hjust=0, vjust=0, size=axisSizeX),
						axis.title.y = element_blank(),
						axis.text.y = element_text(size=axisNumSize),
						legend.position='none'
					)

					xbreaks <- pretty(c(1, xmax), 3)
					xbreaks[1] <- 1
					
					imprecisePanel <- ggplot(impreciseDf, aes(x=numAdmin, y=resp)) +
						geom_point(data=impreciseDf, color=imprecisePointCol, size=pointSize) +					
						geom_line(data=topBoundaryImprecise, mapping=aes(x=numAdmin, y=boundary), size=boundaryLineThickness) +
						geom_line(data=bottomBoundaryImprecise, mapping=aes(x=numAdmin, y=boundary), size=boundaryLineThickness) +
						geom_line(data=middleImprecise, mapping=aes(x=numAdmin, y=middle), col=impreciseLineCol, size=medianSize) +
						coord_cartesian(xlim=c(xmin, xmax), ylim=c(ymin, ymax), expand=FALSE) +
						xlab('County records') +
						scale_x_continuous(breaks = xbreaks) +
						theme(
							plot.background = element_blank(),
							plot.title = element_blank(),
							axis.title.x = element_text(size=axisSizeX, vjust=6),
							axis.text.x = element_text(size=axisNumSize),
							axis.title.y = element_blank(),
							axis.text.y = element_blank(),
							axis.ticks.y = element_blank()
						)

				boxPanel <- boxPanel + theme(plot.margin = grid::unit(c(0, 0, 0, 0.1), 'cm'))
				imprecisePanel <- imprecisePanel + theme(plot.margin = grid::unit(c(0, 0.2, 0, 0), 'cm'))

				comboPanel <- plot_grid(boxPanel, imprecisePanel, ncol=2, rel_widths=c(0.5, 1), align='h')
				if (length(panels) == 1) comboPanel <- comboPanel +
					geom_text(aes(x=0, y=1, label='Calibration accuracy', angle=90, vjust=0, size=axisSizeY))

				panels[[length(panels) + 1]] <- comboPanel
				names(panels)[length(panels)] <- paste0('errorless', thisNumErrorless, '_precise', thisNumPrecise)
				
			} # next number of precise
			
		} # next number of errorless

		calibSq <- (panels[[1]] + panels[[3]]) / (panels[[2]] + panels[[4]]) +
			plot_layout(nrow=2) +
			plot_annotation(
				title = title,
				theme=theme(
					plot.title = element_text(size=setSize),
					plot.margin = grid::unit(c(0, 0, 0.2, 0.2), 'in')
				)
			)

	### ENM calibration: future
	############################

		### generalization

		subject <- 'ENM CALIBRATION FUTURE'
		title <- 'b) ENM calibration in the future'
		errorless <- 'corTruthVsErrorlessFut'
		precise <- 'corTruthVsPreciseFut'
		imprecise <- 'corTruthVsImpreciseFut'
	
		ylab <- 'Calibration accuracy'
		
		### y-axis limits
		these <- c(
			results[ , 'corTruthVsErrorlessSq'], results[ , 'corTruthVsPreciseSq'], results[ , 'corTruthVsImpreciseSq'],
			results[ , 'corTruthVsErrorlessFut'], results[ , 'corTruthVsPreciseFut'], results[ , 'corTruthVsImpreciseFut']
		)
		ymin <- min(0, these, na.rm=TRUE)
		ymin <- roundTo(ymin, 0.2, floor)
		ymax <- 1
		ybreaks <- pretty(c(ymin, ymax))
		
		panels <- list()
		count <- 0
		for (thisNumErrorless in numErrorlessSelected) {
		
			# subset number of precise so it's < total errorless
			allowableNumPreciseSelected <- numPreciseSelected[numPreciseSelected < thisNumErrorless]

			# x-axis limits
			these <- results[results$numErrorless == thisNumErrorless, ]
			xmax <- 1.015 * max(these$numAdmin)
			xmin <- 1 - 0.015 * xmax

			for (thisNumPrecise in allowableNumPreciseSelected) {

				say(subject, ' omniscient ', thisNumErrorless, ' precise ', thisNumPrecise)

				# tall imprecise data
				thisResults <- results[results$numErrorless == thisNumErrorless & results$numPrecise == thisNumPrecise, ]
				resp <- thisResults[ , imprecise]
				n <- nrow(thisResults)
				impreciseDf <- data.frame(
					numAdmin = thisResults$numAdmin,
					resp = resp
				)
				
				# calculate quantiles for regression: imprecise
				impreciseDf <- impreciseDf[order(impreciseDf$numAdmin), ]
				k <- if (length(unique(impreciseDf$numAdmin)) <= 6) { 1 } else { -1 }
				middleQuant <- qgam(resp ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=0.5)
				lowerQuant <- qgam(resp ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=lower)
				upperQuant <- qgam(resp ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=upper)
				
				middleQuant <- (predict(middleQuant, impreciseDf))
				lowerQuant <- (predict(lowerQuant, impreciseDf))
				upperQuant <- (predict(upperQuant, impreciseDf))

				middleImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					middle = middleQuant
				)
				
				topBoundaryImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					boundary = upperQuant
				)
				
				bottomBoundaryImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					boundary = lowerQuant
				)

				# frames for violin plot of precise
				errorlessPreciseDf <- data.frame(
					x = rep(c(-0.5, 0.5), each=nrow(thisResults)),
					type = rep(c('errorless', 'precise'), each=nrow(thisResults)),
					resp = c(thisResults[ , errorless], thisResults[ , precise])
				)
				errorlessDf <- data.frame(resp = thisResults[ , errorless])
				preciseDf <- data.frame(resp = thisResults[ , precise])

				errorlessLower <- quantile(errorlessDf$resp, lower, na.rm=TRUE)
				errorlessUpper <- quantile(errorlessDf$resp, upper, na.rm=TRUE)
				preciseLower <- quantile(preciseDf$resp, lower, na.rm=TRUE)
				preciseUpper <- quantile(preciseDf$resp, upper, na.rm=TRUE)

				# labels and titles
				main <- paste0(thisNumErrorless, ' omniscient, ', thisNumPrecise, ' precise')

				boxPanel <- ggplot(errorlessPreciseDf, aes(x=x, y=resp)) +
					geom_point(
						data=errorlessPreciseDf,
						mapping=aes(x=x, y=resp, color=type, group=type),
						position = position_jitter(w = 0.4, h = 0),
						size = pointSize
					) +
					scale_color_manual(values=c('errorless' = errorlessPointColAlpha, 'precise' = precisePointColAlpha)) +
					geom_violin(
						data = errorlessDf[errorlessDf$resp >= errorlessLower & errorlessDf$resp <= errorlessUpper, , drop=FALSE],
						mapping = aes(x=-0.5, y = resp),
						draw_quantiles = 0.5,
						fill = NA
					) +
					geom_violin(
						data = preciseDf[preciseDf$resp >= preciseLower & preciseDf$resp <= preciseUpper, , drop=FALSE],
						mapping = aes(x=0.5, y = resp),
						draw_quantiles = 0.5,
						fill = NA
					) +
					scale_x_continuous(breaks=c(-0.5, 0.5), labels=c('Omni', 'Prec')) +
					scale_y_continuous(breaks=ybreaks) +
					coord_cartesian(xlim=c(-1, 1), ylim=c(ymin, ymax), expand=FALSE) +
					ggtitle(main) +
					theme(
						plot.title = element_text(hjust = 0, vjust=-1, size=panelSize),
						plot.background = element_blank(),
						panel.grid.major.x = element_blank(),
						panel.grid.minor.x = element_blank(),
						axis.title.x = element_blank(),
						axis.text.x = element_text(angle = 90, hjust=0, vjust=0, size=axisSizeX),
						axis.title.y = element_blank(),
						axis.text.y = element_text(size=axisNumSize),
						legend.position='none'
					)

					xbreaks <- pretty(c(1, xmax), 3)
					xbreaks[1] <- 1
					
					imprecisePanel <- ggplot(impreciseDf, aes(x=numAdmin, y=resp)) +
						geom_point(data=impreciseDf, color=imprecisePointCol, size=pointSize) +					
						geom_line(data=topBoundaryImprecise, mapping=aes(x=numAdmin, y=boundary), size=boundaryLineThickness) +
						geom_line(data=bottomBoundaryImprecise, mapping=aes(x=numAdmin, y=boundary), size=boundaryLineThickness) +
						geom_line(data=middleImprecise, mapping=aes(x=numAdmin, y=middle), col=impreciseLineCol, size=medianSize) +
						coord_cartesian(xlim=c(xmin, xmax), ylim=c(ymin, ymax), expand=FALSE) +
						xlab('County records') +
						scale_x_continuous(breaks = xbreaks) +
						theme(
							plot.background = element_blank(),
							plot.title = element_blank(),
							axis.title.x = element_text(size=axisSizeX, vjust=6),
							axis.text.x = element_text(size=axisNumSize),
							axis.title.y = element_blank(),
							axis.text.y = element_blank(),
							axis.ticks.y = element_blank()
						)

				boxPanel <- boxPanel + theme(plot.margin = grid::unit(c(0, 0, 0, 0.1), 'cm'))
				imprecisePanel <- imprecisePanel + theme(plot.margin = grid::unit(c(0, 0.2, 0, 0), 'cm'))

				comboPanel <- plot_grid(boxPanel, imprecisePanel, ncol=2, rel_widths=c(0.5, 1), align='h')
				if (length(panels) == 1) comboPanel <- comboPanel +
					geom_text(aes(x=0, y=1, label='Calibration accuracy', angle=90, vjust=0, size=axisSizeY))

				panels[[length(panels) + 1]] <- comboPanel
				names(panels)[length(panels)] <- paste0('errorless', thisNumErrorless, '_precise', thisNumPrecise)
				
			} # next number of precise
			
		} # next number of errorless

		calibFut <- (panels[[1]] + panels[[3]]) / (panels[[2]] + panels[[4]]) +
			plot_layout(nrow=2) +
			plot_annotation(
				title = title,
				theme=theme(
					plot.title = element_text(size=setSize),
					plot.margin = grid::unit(c(0, 0, 0.2, 0.2), 'in')
				)
			)

	### suitable area: present
	##########################

		### generalization

		subject <- 'SQ SUITABLE AREA'
		title <- 'c) Current climatically suitable area'
		errorless <- 'sqSuitArea_errorless_km2'
		precise <- 'sqSuitArea_precise_km2'
		imprecise <- 'sqSuitArea_imprecise_km2'
		
		ylab <- 'Estimated / omniscient suitable area'
		
		### y-axis limits
		these <- c(
			(results[ , imprecise]) / (results[ , errorless]),
			(results[ , precise]) / (results[ , errorless])
		)
		ys <- quantile(these, c(1 
		- outlierQuant, outlierQuant), na.rm=TRUE)
		ymin <- as.numeric(ys[1])
		ymax <- as.numeric(ys[2])
		
		panels <- list()
		count <- 0
		for (thisNumErrorless in numErrorlessSelected) {
		
			# subset number of precise so it's < total errorless
			allowableNumPreciseSelected <- numPreciseSelected[numPreciseSelected < thisNumErrorless]

			# x-axis limits
			these <- results[results$numErrorless == thisNumErrorless, ]
			xmax <- 1.015 * max(these$numAdmin)
			xmin <- 1 - 0.015 * xmax

			for (thisNumPrecise in allowableNumPreciseSelected) {

				say(subject, ' omniscient ', thisNumErrorless, ' precise ', thisNumPrecise)
				count <- count + 1

				# tall imprecise data
				thisResults <- results[results$numErrorless == thisNumErrorless & results$numPrecise == thisNumPrecise, ]
				resp <- thisResults[ , imprecise] / thisResults[ , errorless]
				n <- nrow(thisResults)
				impreciseDf <- data.frame(
					numAdmin = thisResults$numAdmin,
					resp = resp
				)
				
				# calculate quantiles for regression: imprecise
				impreciseDf <- impreciseDf[order(impreciseDf$numAdmin), ]
				impreciseDf <- impreciseDf[!is.infinite(log(impreciseDf$resp)), ]
				k <- if (length(unique(impreciseDf$numAdmin)) <= 6) { 1 } else { -1 }
				middleQuant <- qgam(log(resp) ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=0.5)
				lowerQuant <- qgam(log(resp) ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=lower)
				upperQuant <- qgam(log(resp) ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=upper)
				
				middleQuant <- exp(predict(middleQuant, impreciseDf, type='response'))
				lowerQuant <- exp(predict(lowerQuant, impreciseDf, type='response'))
				upperQuant <- exp(predict(upperQuant, impreciseDf, type='response'))

				middleImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					middle = middleQuant
				)
				
				topBoundaryImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					boundary = upperQuant
				)
				
				bottomBoundaryImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					boundary = lowerQuant
				)

				# frames for violin plot of precise
				preciseDf <- data.frame(
					numAdmin = 0,
					resp = thisResults[ , precise] / thisResults[ , errorless]
				)
				preciseDf$resp <- log10(preciseDf$resp)
				preciseDf$resp <- 10^(preciseDf$resp)

				# labels and titles
				main <- paste0(thisNumErrorless, ' omniscient, ', thisNumPrecise, ' precise')

				boxPanel <- ggplot(impreciseDf, aes(y=resp)) +
					geom_point(
						data=preciseDf,
						mapping=aes(x=0, y=resp),
						position = position_jitter(w = 0.4, h = 0),
						color = precisePointColAlpha,
						size = pointSize
					) +
					geom_hline(yintercept = 1, size=0.4, linetype='solid', col='gray40') +
					geom_violin(
						data = preciseDf[preciseDf$resp >= quantile(preciseDf$resp, lower, na.rm=TRUE) & preciseDf$resp <= quantile(preciseDf$resp, upper, na.rm=TRUE), ],
						mapping = aes(x=0, y = resp),
						draw_quantiles = 0.5,
						fill = NA
					) +
					scale_x_continuous(breaks=0, labels=c('Prec')) +
					scale_y_log10(limits=c(ymin, ymax), labels=scales::label_number_auto()) +
					# scale_y_continuous(limits=c(ymin, ymax), trans='log10') +
					ggtitle(main) +
					theme(
						plot.title = element_text(hjust = 0, vjust=-1.5, size=panelSize),
						plot.background = element_blank(),
						panel.grid.major.x = element_blank(),
						panel.grid.minor.x = element_blank(),
						axis.title.x = element_blank(),
						axis.text.x = element_text(angle = 0, hjust=0.5, vjust=0),
						axis.title.y = element_blank(),
						axis.text.y = element_text(size=axisNumSize),
					)

				xbreaks <- pretty(c(1, xmax), 3)
				xbreaks[1] <- 1
					
				imprecisePanel <- ggplot(impreciseDf, aes(x=numAdmin, y=resp)) +
					geom_point(data=impreciseDf, color=imprecisePointCol, size=pointSize) +					
					geom_line(data=topBoundaryImprecise, mapping=aes(x=numAdmin, y=boundary), size=boundaryLineThickness) +
					geom_line(data=bottomBoundaryImprecise, mapping=aes(x=numAdmin, y=boundary), size=boundaryLineThickness) +
					geom_line(data=middleImprecise, mapping=aes(x=numAdmin, y=middle), col=impreciseLineCol, size=medianSize) +
					geom_hline(yintercept = 1, size=0.4, linetype='solid', col='gray40') +
					scale_x_continuous(breaks = xbreaks, limits=c(xmin, xmax)) +
					scale_y_log10(limits=c(ymin, ymax), labels=scales::label_number_auto()) +
					# scale_y_continuous(limits=c(ymin, ymax), trans='log10') +
					xlab('County records') +
					theme(
						plot.background = element_blank(),
						plot.title = element_blank(),
						axis.title.x = element_text(size=axisSizeX, vjust=1),
						axis.text.x = element_text(size=axisNumSize),
						axis.title.y = element_blank(),
						axis.text.y = element_blank(),
						axis.ticks.y = element_blank()
					)

				boxPanel <- boxPanel + theme(plot.margin = grid::unit(c(0, 0, 0, 0.1), 'cm'))
				imprecisePanel <- imprecisePanel + theme(plot.margin = grid::unit(c(0, 0.2, 0, 0), 'cm'))

				comboPanel <- plot_grid(boxPanel, imprecisePanel, ncol=2, rel_widths=c(0.55, 1), align='h')
				if (length(panels) == 1) comboPanel <- comboPanel +
					geom_text(aes(x=0, y=1, label='Estimated / omniscient suitable area', angle=90, size=axisSizeY, vjust=0))

				panels[[count]] <- comboPanel
				names(panels)[count] <- paste0('errorless', thisNumErrorless, '_precise', thisNumPrecise)
				
			} # next number of precise
			
		} # next number of errorless

		sqSuit <- (panels[[1]] + panels[[3]]) / (panels[[2]] + panels[[4]]) +
			plot_layout(nrow=2) +
			plot_annotation(
				title = title,
				theme=theme(
					plot.title = element_text(size=setSize),
					plot.margin = grid::unit(c(0, 0, 0.2, 0.2), 'in')
				)
			)

	### suitable area: future
	##########################

		### generalization

		subject <- 'FUTURE SUITABLE AREA'
		title <- 'd) Future climatically suitable area'
		errorless <- 'futSuitArea_errorless_km2'
		precise <- 'futSuitArea_precise_km2'
		imprecise <- 'futSuitArea_imprecise_km2'
		
		ylab <- 'Estimated / omniscient suitable area'
		
		### y-axis limits
		these <- c(
			(results[ , imprecise] + 1) / (results[ , errorless] + 1),
			(results[ , precise] + 1) / (results[ , errorless] + 1)
		)
		ys <- quantile(these, c(1 - outlierQuant, outlierQuant), na.rm=TRUE)
		ymin <- as.numeric(ys[1])
		ymax <- as.numeric(ys[2])
		
		panels <- list()
		count <- 0
		for (thisNumErrorless in numErrorlessSelected) {
		
			# subset number of precise so it's < total errorless
			allowableNumPreciseSelected <- numPreciseSelected[numPreciseSelected < thisNumErrorless]

			# x-axis limits
			these <- results[results$numErrorless == thisNumErrorless, ]
			xmax <- 1.015 * max(these$numAdmin)
			xmin <- 1 - 0.015 * xmax

			for (thisNumPrecise in allowableNumPreciseSelected) {

				say(subject, ' omniscient ', thisNumErrorless, ' precise ', thisNumPrecise)
				count <- count + 1

				# tall imprecise data
				thisResults <- results[results$numErrorless == thisNumErrorless & results$numPrecise == thisNumPrecise, ]
				resp <- ((thisResults[ , imprecise] + 1) / (thisResults[ , errorless] + 1))
				n <- nrow(thisResults)
				impreciseDf <- data.frame(
					numAdmin = thisResults$numAdmin,
					resp = resp
				)
				
				# calculate quantiles for regression: imprecise
				impreciseDf <- impreciseDf[order(impreciseDf$numAdmin), ]
				impreciseDf <- impreciseDf[!is.infinite(impreciseDf$resp), ]
				k <- if (length(unique(impreciseDf$numAdmin)) <= 6) { 1 } else { -1 }
				middleQuant <- qgam(log(resp) ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=0.5)
				lowerQuant <- qgam(log(resp) ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=lower)
				upperQuant <- qgam(log(resp) ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=upper)
				
				middleQuant <- exp(predict(middleQuant, impreciseDf, type='response'))
				lowerQuant <- exp(predict(lowerQuant, impreciseDf, type='response'))
				upperQuant <- exp(predict(upperQuant, impreciseDf, type='response'))

				middleImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					middle = middleQuant
				)
				
				topBoundaryImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					boundary = upperQuant
				)
				
				bottomBoundaryImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					boundary = lowerQuant
				)

				# frames for violin plot of precise
				preciseDf <- data.frame(
					numAdmin = 0,
					resp = ((thisResults[ , precise] + 1) / (thisResults[ , errorless] + 1))
				)

				# labels and titles
				main <- paste0(thisNumErrorless, ' omniscient, ', thisNumPrecise, ' precise')

				boxPanel <- ggplot(impreciseDf, aes(y=resp)) +
					geom_point(
						data=preciseDf,
						mapping=aes(x=0, y=resp),
						position = position_jitter(w = 0.4, h = 0),
						color = precisePointColAlpha,
						size = pointSize
					) +
					geom_hline(yintercept = 1, size=0.4, linetype='solid', col='gray40') +
					geom_violin(
						data = preciseDf[preciseDf$resp >= quantile(preciseDf$resp, lower, na.rm=TRUE) & preciseDf$resp <= quantile(preciseDf$resp, upper, na.rm=TRUE), ],
						mapping = aes(x=0, y = resp),
						draw_quantiles = 0.5,
						fill = NA
					) +
					scale_x_continuous(breaks=0, labels=c('Prec')) +
					scale_y_log10(limits=c(ymin, ymax), labels=scales::label_number_auto()) +
					# scale_y_continuous(limits=c(ymin, ymax), trans='log10') +
					ggtitle(main) +
					theme(
						plot.title = element_text(hjust = 0, vjust=-1.5, size=panelSize),
						plot.background = element_blank(),
						panel.grid.major.x = element_blank(),
						panel.grid.minor.x = element_blank(),
						axis.title.x = element_blank(),
						axis.text.x = element_text(angle = 0, hjust=0.5, vjust=0),
						axis.title.y = element_blank(),
						axis.text.y = element_text(size=axisNumSize),
					)

				xbreaks <- pretty(c(1, xmax), 3)
				xbreaks[1] <- 1
					
				imprecisePanel <- ggplot(impreciseDf, aes(x=numAdmin, y=resp)) +
					geom_point(data=impreciseDf, color=imprecisePointCol, size=pointSize) +					
					geom_line(data=topBoundaryImprecise, mapping=aes(x=numAdmin, y=boundary), size=boundaryLineThickness) +
					geom_line(data=bottomBoundaryImprecise, mapping=aes(x=numAdmin, y=boundary), size=boundaryLineThickness) +
					geom_line(data=middleImprecise, mapping=aes(x=numAdmin, y=middle), col=impreciseLineCol, size=medianSize) +
					geom_hline(yintercept = 1, size=0.4, linetype='solid', col='gray40') +
					scale_x_continuous(breaks = xbreaks, limits=c(xmin, xmax)) +
					scale_y_log10(limits=c(ymin, ymax), labels=scales::label_number_auto()) +
					# scale_y_continuous(limits=c(ymin, ymax), trans='log10') +
					xlab('County records') +
					theme(
						plot.background = element_blank(),
						plot.title = element_blank(),
						axis.title.x = element_text(size=axisSizeX, vjust=1),
						axis.text.x = element_text(size=axisNumSize),
						axis.title.y = element_blank(),
						axis.text.y = element_blank(),
						axis.ticks.y = element_blank()
					)

				boxPanel <- boxPanel + theme(plot.margin = grid::unit(c(0, 0, 0, 0.1), 'cm'))
				imprecisePanel <- imprecisePanel + theme(plot.margin = grid::unit(c(0, 0.2, 0, 0), 'cm'))

				comboPanel <- plot_grid(boxPanel, imprecisePanel, ncol=2, rel_widths=c(0.55, 1), align='h')
				if (length(panels) == 1) comboPanel <- comboPanel +
					geom_text(aes(x=0, y=1, label='Estimated / omniscient suitable area', angle=90, size=axisSizeY, vjust=0))

				panels[[count]] <- comboPanel
				names(panels)[count] <- paste0('errorless', thisNumErrorless, '_precise', thisNumPrecise)
				
			} # next number of precise
			
		} # next number of errorless

		futSuit <- (panels[[1]] + panels[[3]]) / (panels[[2]] + panels[[4]]) +
			plot_layout(nrow=2) +
			plot_annotation(
				title = title,
				theme=theme(
					plot.title = element_text(size=setSize),
					plot.margin = grid::unit(c(0, 0, 0.2, 0.2), 'in')
				)
			)

	### suitable area: lost
	#######################

		### generalization

		subject <- 'LOST SUITABLE AREA'
		title <- 'e) Loss in climatically suitable area'
		errorless <- 'lostArea_errorless_km2'
		precise <- 'lostArea_precise_km2'
		imprecise <- 'lostArea_imprecise_km2'
		
		ylab <- 'Estimated / true loss in suitable area'
		
		### y-axis limits
		these <- c(
			(results[ , imprecise] + 1) / (results[ , errorless] + 1),
			(results[ , precise] + 1) / (results[ , errorless] + 1)
		)
		these <- log10(these)
		ys <- quantile(these, c(1 - outlierQuant, outlierQuant), na.rm=TRUE)
		ymin <- as.numeric(ys[1])
		ymax <- as.numeric(ys[2])
		ymin <- 10^ymin
		ymax <- 10^ymax
		
		panels <- list()
		count <- 0
		for (thisNumErrorless in numErrorlessSelected) {
		
			# subset number of precise so it's < total errorless
			allowableNumPreciseSelected <- numPreciseSelected[numPreciseSelected < thisNumErrorless]

			# x-axis limits
			these <- results[results$numErrorless == thisNumErrorless, ]
			xmax <- 1.015 * max(these$numAdmin)
			xmin <- 1 - 0.015 * xmax

			for (thisNumPrecise in allowableNumPreciseSelected) {

				say(subject, ' omniscient ', thisNumErrorless, ' precise ', thisNumPrecise)
				count <- count + 1

				# tall imprecise data
				thisResults <- results[results$numErrorless == thisNumErrorless & results$numPrecise == thisNumPrecise, ]
				resp <- ((thisResults[ , imprecise] + 1) / (thisResults[ , errorless] + 1))
				n <- nrow(thisResults)
				impreciseDf <- data.frame(
					numAdmin = thisResults$numAdmin,
					resp = resp
				)
				
				# calculate quantiles for regression: imprecise
				impreciseDf <- impreciseDf[order(impreciseDf$numAdmin), ]
				impreciseDf <- impreciseDf[!is.infinite(impreciseDf$resp), ]
				k <- if (length(unique(impreciseDf$numAdmin)) <= 6) { 1 } else { -1 }
				middleQuant <- qgam(log(resp) ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=0.5)
				lowerQuant <- qgam(log(resp) ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=lower)
				upperQuant <- qgam(log(resp) ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=upper)
				
				middleQuant <- exp(predict(middleQuant, impreciseDf, type='response'))
				lowerQuant <- exp(predict(lowerQuant, impreciseDf, type='response'))
				upperQuant <- exp(predict(upperQuant, impreciseDf, type='response'))

				middleImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					middle = middleQuant
				)
				
				topBoundaryImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					boundary = upperQuant
				)
				
				bottomBoundaryImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					boundary = lowerQuant
				)
				
				# frames for violin plot of precise
				preciseDf <- data.frame(
					numAdmin = 0,
					resp = ((thisResults[ , precise] + 1) / (thisResults[ , errorless] + 1))
				)

				# labels and titles
				main <- paste0(thisNumErrorless, ' omniscient, ', thisNumPrecise, ' precise')

				boxPanel <- ggplot(impreciseDf, aes(y=resp)) +
					geom_point(
						data=preciseDf,
						mapping=aes(x=0, y=resp),
						position = position_jitter(w = 0.4, h = 0),
						color = precisePointColAlpha,
						size = pointSize
					) +
					geom_hline(yintercept = 1, size=0.4, linetype='solid', col='gray40') +
					geom_violin(
						data = preciseDf[preciseDf$resp >= quantile(preciseDf$resp, lower, na.rm=TRUE) & preciseDf$resp <= quantile(preciseDf$resp, upper, na.rm=TRUE), ],
						mapping = aes(x=0, y = resp),
						draw_quantiles = 0.5,
						fill = NA
					) +
					scale_x_continuous(breaks=0, labels=c('Prec')) +
					scale_y_log10(limits=c(ymin, ymax), labels=scales::label_number_auto()) +
					# scale_y_continuous(limits=c(ymin, ymax), trans='log10') +
					ggtitle(main) +
					theme(
						plot.title = element_text(hjust = 0, vjust=-1.5, size=panelSize),
						plot.background = element_blank(),
						panel.grid.major.x = element_blank(),
						panel.grid.minor.x = element_blank(),
						axis.title.x = element_blank(),
						axis.text.x = element_text(angle = 0, hjust=0.5, vjust=0),
						axis.title.y = element_blank(),
						axis.text.y = element_text(size=axisNumSize),
					)

				xbreaks <- pretty(c(1, xmax), 3)
				xbreaks[1] <- 1
					
				imprecisePanel <- ggplot(impreciseDf, aes(x=numAdmin, y=resp)) +
					geom_point(data=impreciseDf, color=imprecisePointCol, size=pointSize) +					
					geom_line(data=topBoundaryImprecise, mapping=aes(x=numAdmin, y=boundary), size=boundaryLineThickness) +
					geom_line(data=bottomBoundaryImprecise, mapping=aes(x=numAdmin, y=boundary), size=boundaryLineThickness) +
					geom_line(data=middleImprecise, mapping=aes(x=numAdmin, y=middle), col=impreciseLineCol, size=medianSize) +
					geom_hline(yintercept = 1, size=0.4, linetype='solid', col='gray40') +
					scale_x_continuous(breaks = xbreaks, limits=c(xmin, xmax)) +
					scale_y_log10(limits=c(ymin, ymax), labels=scales::label_number_auto()) +
					# scale_y_continuous(limits=c(ymin, ymax), trans='log10') +
					xlab('County records') +
					theme(
						plot.background = element_blank(),
						plot.title = element_blank(),
						axis.title.x = element_text(size=axisSizeX, vjust=1),
						axis.text.x = element_text(size=axisNumSize),
						axis.title.y = element_blank(),
						axis.text.y = element_blank(),
						axis.ticks.y = element_blank()
					)

				boxPanel <- boxPanel + theme(plot.margin = grid::unit(c(0, 0, 0, 0.1), 'cm'))
				imprecisePanel <- imprecisePanel + theme(plot.margin = grid::unit(c(0, 0.2, 0, 0), 'cm'))

				comboPanel <- plot_grid(boxPanel, imprecisePanel, ncol=2, rel_widths=c(0.55, 1), align='h')
				if (length(panels) == 1) comboPanel <- comboPanel +
					geom_text(aes(x=0, y=1, label='Estimated / omniscient area loss', angle=90, size=axisSizeY, vjust=0))

				panels[[count]] <- comboPanel
				names(panels)[count] <- paste0('errorless', thisNumErrorless, '_precise', thisNumPrecise)
				
			} # next number of precise
			
		} # next number of errorless

		loss <- (panels[[1]] + panels[[3]]) / (panels[[2]] + panels[[4]]) +
			plot_layout(nrow=2) +
			plot_annotation(
				title = title,
				theme=theme(
					plot.title = element_text(size=setSize),
					plot.margin = grid::unit(c(0, 0, 0.2, 0.2), 'in')
				)
			)

	### suitable area: gain
	#######################

		### generalization

		subject <- 'SUITABLE AREA GAIN'
		title <- 'e) Gain in climatically suitable area'
		errorless <- 'gainArea_errorless_km2'
		precise <- 'gainArea_precise_km2'
		imprecise <- 'gainArea_imprecise_km2'
		
		ylab <- 'Estimated / true gain in suitable area'
		
		### y-axis limits
		these <- c(
			(results[ , imprecise] + 1) / (results[ , errorless] + 1),
			(results[ , precise] + 1) / (results[ , errorless] + 1)
		)
		these <- log10(these)
		ys <- quantile(these, c(1 - outlierQuant, outlierQuant), na.rm=TRUE)
		ymin <- as.numeric(ys[1])
		ymax <- as.numeric(ys[2])
		ymin <- 10^ymin
		ymax <- 10^ymax
		
		panels <- list()
		count <- 0
		for (thisNumErrorless in numErrorlessSelected) {
		
			# subset number of precise so it's < total errorless
			allowableNumPreciseSelected <- numPreciseSelected[numPreciseSelected < thisNumErrorless]

			# x-axis limits
			these <- results[results$numErrorless == thisNumErrorless, ]
			xmax <- 1.015 * max(these$numAdmin)
			xmin <- 1 - 0.015 * xmax

			for (thisNumPrecise in allowableNumPreciseSelected) {

				say(subject, ' omniscient ', thisNumErrorless, ' precise ', thisNumPrecise)
				count <- count + 1

				# tall imprecise data
				thisResults <- results[results$numErrorless == thisNumErrorless & results$numPrecise == thisNumPrecise, ]
				resp <- ((thisResults[ , imprecise] + 1) / (thisResults[ , errorless] + 1))
				n <- nrow(thisResults)
				impreciseDf <- data.frame(
					numAdmin = thisResults$numAdmin,
					resp = resp
				)
				
				# calculate quantiles for regression: imprecise
				impreciseDf <- impreciseDf[order(impreciseDf$numAdmin), ]
				impreciseDf <- impreciseDf[!is.infinite(impreciseDf$resp), ]
				k <- if (length(unique(impreciseDf$numAdmin)) <= 6) { 1 } else { -1 }
				middleQuant <- qgam(log(resp) ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=0.5)
				lowerQuant <- qgam(log(resp) ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=lower)
				upperQuant <- qgam(log(resp) ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=upper)
				
				middleQuant <- exp(predict(middleQuant, impreciseDf, type='response'))
				lowerQuant <- exp(predict(lowerQuant, impreciseDf, type='response'))
				upperQuant <- exp(predict(upperQuant, impreciseDf, type='response'))

				middleImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					middle = middleQuant
				)
				
				topBoundaryImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					boundary = upperQuant
				)
				
				bottomBoundaryImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					boundary = lowerQuant
				)

				# frames for violin plot of precise
				preciseDf <- data.frame(
					numAdmin = 0,
					resp = ((thisResults[ , precise] + 1) / (thisResults[ , errorless] + 1))
				)

				# labels and titles
				main <- paste0(thisNumErrorless, ' omniscient, ', thisNumPrecise, ' precise')

				boxPanel <- ggplot(impreciseDf, aes(y=resp)) +
					geom_point(
						data=preciseDf,
						mapping=aes(x=0, y=resp),
						position = position_jitter(w = 0.4, h = 0),
						color = precisePointColAlpha,
						size = pointSize
					) +
					geom_hline(yintercept = 1, size=0.4, linetype='solid', col='gray40') +
					geom_violin(
						data = preciseDf[preciseDf$resp >= quantile(preciseDf$resp, lower, na.rm=TRUE) & preciseDf$resp <= quantile(preciseDf$resp, upper, na.rm=TRUE), ],
						mapping = aes(x=0, y = resp),
						draw_quantiles = 0.5,
						fill = NA
					) +
					scale_x_continuous(breaks=0, labels=c('Prec')) +
					scale_y_log10(limits=c(ymin, ymax), labels=scales::label_number_auto()) +
					# scale_y_continuous(limits=c(ymin, ymax), trans='log10') +
					ggtitle(main) +
					theme(
						plot.title = element_text(hjust = 0, vjust=-1.5, size=panelSize),
						plot.background = element_blank(),
						panel.grid.major.x = element_blank(),
						panel.grid.minor.x = element_blank(),
						axis.title.x = element_blank(),
						axis.text.x = element_text(angle = 0, hjust=0.5, vjust=0),
						axis.title.y = element_blank(),
						axis.text.y = element_text(size=axisNumSize),
					)

				xbreaks <- pretty(c(1, xmax), 3)
				xbreaks[1] <- 1
					
				imprecisePanel <- ggplot(impreciseDf, aes(x=numAdmin, y=resp)) +
					geom_point(data=impreciseDf, color=imprecisePointCol, size=pointSize) +					
					geom_line(data=topBoundaryImprecise, mapping=aes(x=numAdmin, y=boundary), size=boundaryLineThickness) +
					geom_line(data=bottomBoundaryImprecise, mapping=aes(x=numAdmin, y=boundary), size=boundaryLineThickness) +
					geom_line(data=middleImprecise, mapping=aes(x=numAdmin, y=middle), col=impreciseLineCol, size=medianSize) +
					geom_hline(yintercept = 1, size=0.4, linetype='solid', col='gray40') +
					scale_x_continuous(breaks = xbreaks, limits=c(xmin, xmax)) +
					scale_y_log10(limits=c(ymin, ymax), labels=scales::label_number_auto()) +
					# scale_y_continuous(limits=c(ymin, ymax), trans='log10') +
					xlab('County records') +
					theme(
						plot.background = element_blank(),
						plot.title = element_blank(),
						axis.title.x = element_text(size=axisSizeX, vjust=1),
						axis.text.x = element_text(size=axisNumSize),
						axis.title.y = element_blank(),
						axis.text.y = element_blank(),
						axis.ticks.y = element_blank()
					)

				boxPanel <- boxPanel + theme(plot.margin = grid::unit(c(0, 0, 0, 0.1), 'cm'))
				imprecisePanel <- imprecisePanel + theme(plot.margin = grid::unit(c(0, 0.2, 0, 0), 'cm'))

				comboPanel <- plot_grid(boxPanel, imprecisePanel, ncol=2, rel_widths=c(0.55, 1), align='h')
				if (length(panels) == 1) comboPanel <- comboPanel +
					geom_text(aes(x=0, y=1, label='Estimated / omniscient area gain', angle=90, size=axisSizeY, vjust=0))

				panels[[count]] <- comboPanel
				names(panels)[count] <- paste0('errorless', thisNumErrorless, '_precise', thisNumPrecise)
				
			} # next number of precise
			
		} # next number of errorless

		gain <- (panels[[1]] + panels[[3]]) / (panels[[2]] + panels[[4]]) +
			plot_layout(nrow=2) +
			plot_annotation(
				title = title,
				theme=theme(
					plot.title = element_text(size=setSize),
					plot.margin = grid::unit(c(0, 0, 0.2, 0.2), 'in')
				)
			)

	### combine
	###########
	
		# main <- plot_grid(calibSq, calibFut, loss, gain, ncol=2)
		main <- plot_grid(calibSq, calibFut, sqSuit, futSuit, loss, gain, ncol=2)
		main <- main + theme(plot.margin = grid::unit(c(0.5, 0.5, 0.5, 0.5), 'in'))
		
		ggsave(plot=main, filename=paste0('./Analysis/Virtual Species/!Selected ENM Results ', paste(numErrorlessSelected, collapse=' '), ' Omniscient & ', paste(numPreciseSelected, collapse=' '), ' Precise.pdf'), width=8.5, height=11, units='in')

		say('Displaying only lower ', outlierQuant, 'quantile of points.', level=2, deco='!')
				
		
say('#########################################################################################')
say('### make composite plot with SUBSET of ENM results - TWO omniscient scenarios REDUCED ###')
say('#########################################################################################')
	
	# sets of panels, one per metric (calibration, area gain, etc.), four panels per set crossing number of errorless and number of precise

	### generalization
	##################

		# text size
		setSize <- 12 # panel set title
		panelSize <- 9 # panel title
		axisSizeX <- 9 # axes with labels
		axisSizeY <- 9 # axes with labels
		axisNumSize <- 8 # numbers on axes
		
		# lines
		medianSize <- 1
		hlineSize <- 0.6 # thickness of horizontal line at 1
		boundaryLineThickness <- 0.4 # thickness of inner quantile boundary's lines
		
		# point size
		pointSize <- 0.25
		
		# colors
		imprecisePointCol <- '#d95f02'
		
		impreciseLineCol <- 'black'

		errorlessPointColAlpha <- alpha('gray50', 0.5)

		preciseFill <- '#1b9e77'
		precisePointColAlpha <- alpha('#1b9e77', 0.5)

		# quantile to define y-axis upper limit
		outlierQuant <- 0.9975 # quantile above which to remove points for setting y-axis limit

		# quantiles to plot
		lower <- 0.05
		upper <- 0.95
	
		# numErrorlessSelected <- c(320)
		# numErrorlessSelected <- c(40, 160)
		numErrorlessSelected <- c(40, 320)
		# numErrorlessSelected <- c(40)
		
		# numPreciseSelected <- c(5, 30)
		numPreciseSelected <- c(5, 20)

	### preliminary
	###############

		load('./Analysis/Virtual Species/!!Collated Results.rda')
		results <- results[results$numErrorless %in% numErrorlessSelected & results$numPrecise %in% numPreciseSelected, ]

	### ENM calibration: present
	############################

		### generalization

		subject <- 'ENM CALIBRATION SQ'
		title <- 'a) ENM calibration in the present'
		errorless <- 'corTruthVsErrorlessSq'
		precise <- 'corTruthVsPreciseSq'
		imprecise <- 'corTruthVsImpreciseSq'
	
		ylab <- 'Calibration accuracy'
		
		### y-axis limits
		these <- c(
			results[ , 'corTruthVsErrorlessSq'], results[ , 'corTruthVsPreciseSq'], results[ , 'corTruthVsImpreciseSq'],
			results[ , 'corTruthVsErrorlessFut'], results[ , 'corTruthVsPreciseFut'], results[ , 'corTruthVsImpreciseFut']
		)
		ymin <- min(0, these, na.rm=TRUE)
		ymin <- roundTo(ymin, 0.2, floor)
		ymax <- 1
		ybreaks <- pretty(c(ymin, ymax))
		
		panels <- list()
		count <- 0
		for (thisNumErrorless in numErrorlessSelected) {
		
			# subset number of precise so it's < total errorless
			allowableNumPreciseSelected <- numPreciseSelected[numPreciseSelected < thisNumErrorless]

			# x-axis limits
			these <- results[results$numErrorless == thisNumErrorless, ]
			xmax <- 1.015 * max(these$numAdmin)
			xmin <- 1 - 0.015 * xmax

			for (thisNumPrecise in allowableNumPreciseSelected) {

				say(subject, ' omniscient ', thisNumErrorless, ' precise ', thisNumPrecise)

				# tall imprecise data
				thisResults <- results[results$numErrorless == thisNumErrorless & results$numPrecise == thisNumPrecise, ]
				resp <- thisResults[ , imprecise]
				n <- nrow(thisResults)
				impreciseDf <- data.frame(
					numAdmin = thisResults$numAdmin,
					resp = resp
				)
				
				# calculate quantiles for regression: imprecise
				impreciseDf <- impreciseDf[order(impreciseDf$numAdmin), ]
				k <- if (length(unique(impreciseDf$numAdmin)) <= 6) { 1 } else { -1 }
				middleQuant <- qgam(resp ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=0.5)
				lowerQuant <- qgam(resp ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=lower)
				upperQuant <- qgam(resp ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=upper)
				
				middleQuant <- (predict(middleQuant, impreciseDf))
				lowerQuant <- (predict(lowerQuant, impreciseDf))
				upperQuant <- (predict(upperQuant, impreciseDf))

				middleImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					middle = middleQuant
				)
				
				topBoundaryImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					boundary = upperQuant
				)
				
				bottomBoundaryImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					boundary = lowerQuant
				)

				# frames for violin plot of precise
				errorlessPreciseDf <- data.frame(
					x = rep(c(-0.5, 0.5), each=nrow(thisResults)),
					type = rep(c('errorless', 'precise'), each=nrow(thisResults)),
					resp = c(thisResults[ , errorless], thisResults[ , precise])
				)
				errorlessDf <- data.frame(resp = thisResults[ , errorless])
				preciseDf <- data.frame(resp = thisResults[ , precise])

				errorlessLower <- quantile(errorlessDf$resp, lower, na.rm=TRUE)
				errorlessUpper <- quantile(errorlessDf$resp, upper, na.rm=TRUE)
				preciseLower <- quantile(preciseDf$resp, lower, na.rm=TRUE)
				preciseUpper <- quantile(preciseDf$resp, upper, na.rm=TRUE)

				# labels and titles
				main <- paste0(thisNumErrorless, ' omniscient, ', thisNumPrecise, ' precise')

				boxPanel <- ggplot(errorlessPreciseDf, aes(x=x, y=resp)) +
					geom_point(
						data=errorlessPreciseDf,
						mapping=aes(x=x, y=resp, color=type, group=type),
						position = position_jitter(w = 0.4, h = 0),
						size = pointSize
					) +
					scale_color_manual(values=c('errorless' = errorlessPointColAlpha, 'precise' = precisePointColAlpha)) +
					geom_violin(
						data = errorlessDf[errorlessDf$resp >= errorlessLower & errorlessDf$resp <= errorlessUpper, , drop=FALSE],
						mapping = aes(x=-0.5, y = resp),
						draw_quantiles = 0.5,
						fill = NA, fatten=medianSize
					) +
					geom_violin(
						data = preciseDf[preciseDf$resp >= preciseLower & preciseDf$resp <= preciseUpper, , drop=FALSE],
						mapping = aes(x=0.5, y = resp),
						draw_quantiles = 0.5,
						fill = NA
					) +
					scale_x_continuous(breaks=c(-0.5, 0.5), labels=c('Omni', 'Prec')) +
					scale_y_continuous(breaks=ybreaks) +
					coord_cartesian(xlim=c(-1, 1), ylim=c(ymin, ymax), expand=FALSE) +
					ggtitle(main) +
					theme(
						plot.title = element_text(hjust = 0, vjust=-1, size=panelSize),
						plot.background = element_blank(),
						panel.grid.major.x = element_blank(),
						panel.grid.minor.x = element_blank(),
						axis.title.x = element_blank(),
						axis.text.x = element_text(angle = 90, hjust=0, vjust=0, size=axisSizeX),
						axis.title.y = element_blank(),
						axis.text.y = element_text(size=axisNumSize),
						legend.position='none'
					)

					xbreaks <- pretty(c(1, xmax), 3)
					xbreaks[1] <- 1
					
					imprecisePanel <- ggplot(impreciseDf, aes(x=numAdmin, y=resp)) +
						geom_point(data=impreciseDf, color=imprecisePointCol, size=pointSize) +					
						geom_line(data=topBoundaryImprecise, mapping=aes(x=numAdmin, y=boundary), size=boundaryLineThickness) +
						geom_line(data=bottomBoundaryImprecise, mapping=aes(x=numAdmin, y=boundary), size=boundaryLineThickness) +
						geom_line(data=middleImprecise, mapping=aes(x=numAdmin, y=middle), col=impreciseLineCol, size=medianSize) +
						coord_cartesian(xlim=c(xmin, xmax), ylim=c(ymin, ymax), expand=FALSE) +
						xlab('County records') +
						scale_x_continuous(breaks = xbreaks) +
						theme(
							plot.background = element_blank(),
							plot.title = element_blank(),
							axis.title.x = element_text(size=axisSizeX, vjust=6),
							axis.text.x = element_text(size=axisNumSize),
							axis.title.y = element_blank(),
							axis.text.y = element_blank(),
							axis.ticks.y = element_blank()
						)

				boxPanel <- boxPanel + theme(plot.margin = grid::unit(c(0, 0, 0, 0.1), 'cm'))
				imprecisePanel <- imprecisePanel + theme(plot.margin = grid::unit(c(0, 0.2, 0, 0), 'cm'))

				comboPanel <- plot_grid(boxPanel, imprecisePanel, ncol=2, rel_widths=c(0.5, 1), align='h')
				if (length(panels) == 1) comboPanel <- comboPanel +
					geom_text(aes(x=0, y=1, label='Calibration accuracy', angle=90, vjust=0, size=axisSizeY))

				panels[[length(panels) + 1]] <- comboPanel
				names(panels)[length(panels)] <- paste0('errorless', thisNumErrorless, '_precise', thisNumPrecise)
				
			} # next number of precise
			
		} # next number of errorless

		calibSq <- (panels[[1]] + panels[[3]]) / (panels[[2]] + panels[[4]]) +
			plot_layout(nrow=2) +
			plot_annotation(
				title = title,
				theme=theme(
					plot.title = element_text(size=setSize),
					plot.margin = grid::unit(c(0, 0, 0.2, 0.2), 'in')
				)
			)

	### ENM calibration: future
	############################

		### generalization

		subject <- 'ENM CALIBRATION FUTURE'
		title <- 'b) ENM calibration in the future'
		errorless <- 'corTruthVsErrorlessFut'
		precise <- 'corTruthVsPreciseFut'
		imprecise <- 'corTruthVsImpreciseFut'
	
		ylab <- 'Calibration accuracy'
		
		### y-axis limits
		these <- c(
			results[ , 'corTruthVsErrorlessSq'], results[ , 'corTruthVsPreciseSq'], results[ , 'corTruthVsImpreciseSq'],
			results[ , 'corTruthVsErrorlessFut'], results[ , 'corTruthVsPreciseFut'], results[ , 'corTruthVsImpreciseFut']
		)
		ymin <- min(0, these, na.rm=TRUE)
		ymin <- roundTo(ymin, 0.2, floor)
		ymax <- 1
		ybreaks <- pretty(c(ymin, ymax))
		
		panels <- list()
		count <- 0
		for (thisNumErrorless in numErrorlessSelected) {
		
			# subset number of precise so it's < total errorless
			allowableNumPreciseSelected <- numPreciseSelected[numPreciseSelected < thisNumErrorless]

			# x-axis limits
			these <- results[results$numErrorless == thisNumErrorless, ]
			xmax <- 1.015 * max(these$numAdmin)
			xmin <- 1 - 0.015 * xmax

			for (thisNumPrecise in allowableNumPreciseSelected) {

				say(subject, ' omniscient ', thisNumErrorless, ' precise ', thisNumPrecise)

				# tall imprecise data
				thisResults <- results[results$numErrorless == thisNumErrorless & results$numPrecise == thisNumPrecise, ]
				resp <- thisResults[ , imprecise]
				n <- nrow(thisResults)
				impreciseDf <- data.frame(
					numAdmin = thisResults$numAdmin,
					resp = resp
				)
				
				# calculate quantiles for regression: imprecise
				impreciseDf <- impreciseDf[order(impreciseDf$numAdmin), ]
				k <- if (length(unique(impreciseDf$numAdmin)) <= 6) { 1 } else { -1 }
				middleQuant <- qgam(resp ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=0.5)
				lowerQuant <- qgam(resp ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=lower)
				upperQuant <- qgam(resp ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=upper)
				
				middleQuant <- (predict(middleQuant, impreciseDf))
				lowerQuant <- (predict(lowerQuant, impreciseDf))
				upperQuant <- (predict(upperQuant, impreciseDf))

				middleImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					middle = middleQuant
				)
				
				topBoundaryImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					boundary = upperQuant
				)
				
				bottomBoundaryImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					boundary = lowerQuant
				)

				# frames for violin plot of precise
				errorlessPreciseDf <- data.frame(
					x = rep(c(-0.5, 0.5), each=nrow(thisResults)),
					type = rep(c('errorless', 'precise'), each=nrow(thisResults)),
					resp = c(thisResults[ , errorless], thisResults[ , precise])
				)
				errorlessDf <- data.frame(resp = thisResults[ , errorless])
				preciseDf <- data.frame(resp = thisResults[ , precise])

				errorlessLower <- quantile(errorlessDf$resp, lower, na.rm=TRUE)
				errorlessUpper <- quantile(errorlessDf$resp, upper, na.rm=TRUE)
				preciseLower <- quantile(preciseDf$resp, lower, na.rm=TRUE)
				preciseUpper <- quantile(preciseDf$resp, upper, na.rm=TRUE)

				# labels and titles
				main <- paste0(thisNumErrorless, ' omniscient, ', thisNumPrecise, ' precise')

				boxPanel <- ggplot(errorlessPreciseDf, aes(x=x, y=resp)) +
					geom_point(
						data=errorlessPreciseDf,
						mapping=aes(x=x, y=resp, color=type, group=type),
						position = position_jitter(w = 0.4, h = 0),
						size = pointSize
					) +
					scale_color_manual(values=c('errorless' = errorlessPointColAlpha, 'precise' = precisePointColAlpha)) +
					geom_violin(
						data = errorlessDf[errorlessDf$resp >= errorlessLower & errorlessDf$resp <= errorlessUpper, , drop=FALSE],
						mapping = aes(x=-0.5, y = resp),
						draw_quantiles = 0.5,
						fill = NA
					) +
					geom_violin(
						data = preciseDf[preciseDf$resp >= preciseLower & preciseDf$resp <= preciseUpper, , drop=FALSE],
						mapping = aes(x=0.5, y = resp),
						draw_quantiles = 0.5,
						fill = NA
					) +
					scale_x_continuous(breaks=c(-0.5, 0.5), labels=c('Omni', 'Prec')) +
					scale_y_continuous(breaks=ybreaks) +
					coord_cartesian(xlim=c(-1, 1), ylim=c(ymin, ymax), expand=FALSE) +
					ggtitle(main) +
					theme(
						plot.title = element_text(hjust = 0, vjust=-1, size=panelSize),
						plot.background = element_blank(),
						panel.grid.major.x = element_blank(),
						panel.grid.minor.x = element_blank(),
						axis.title.x = element_blank(),
						axis.text.x = element_text(angle = 90, hjust=0, vjust=0, size=axisSizeX),
						axis.title.y = element_blank(),
						axis.text.y = element_text(size=axisNumSize),
						legend.position='none'
					)

					xbreaks <- pretty(c(1, xmax), 3)
					xbreaks[1] <- 1
					
					imprecisePanel <- ggplot(impreciseDf, aes(x=numAdmin, y=resp)) +
						geom_point(data=impreciseDf, color=imprecisePointCol, size=pointSize) +					
						geom_line(data=topBoundaryImprecise, mapping=aes(x=numAdmin, y=boundary), size=boundaryLineThickness) +
						geom_line(data=bottomBoundaryImprecise, mapping=aes(x=numAdmin, y=boundary), size=boundaryLineThickness) +
						geom_line(data=middleImprecise, mapping=aes(x=numAdmin, y=middle), col=impreciseLineCol, size=medianSize) +
						coord_cartesian(xlim=c(xmin, xmax), ylim=c(ymin, ymax), expand=FALSE) +
						xlab('County records') +
						scale_x_continuous(breaks = xbreaks) +
						theme(
							plot.background = element_blank(),
							plot.title = element_blank(),
							axis.title.x = element_text(size=axisSizeX, vjust=6),
							axis.text.x = element_text(size=axisNumSize),
							axis.title.y = element_blank(),
							axis.text.y = element_blank(),
							axis.ticks.y = element_blank()
						)

				boxPanel <- boxPanel + theme(plot.margin = grid::unit(c(0, 0, 0, 0.1), 'cm'))
				imprecisePanel <- imprecisePanel + theme(plot.margin = grid::unit(c(0, 0.2, 0, 0), 'cm'))

				comboPanel <- plot_grid(boxPanel, imprecisePanel, ncol=2, rel_widths=c(0.5, 1), align='h')
				if (length(panels) == 1) comboPanel <- comboPanel +
					geom_text(aes(x=0, y=1, label='Calibration accuracy', angle=90, vjust=0, size=axisSizeY))

				panels[[length(panels) + 1]] <- comboPanel
				names(panels)[length(panels)] <- paste0('errorless', thisNumErrorless, '_precise', thisNumPrecise)
				
			} # next number of precise
			
		} # next number of errorless

		calibFut <- (panels[[1]] + panels[[3]]) / (panels[[2]] + panels[[4]]) +
			plot_layout(nrow=2) +
			plot_annotation(
				title = title,
				theme=theme(
					plot.title = element_text(size=setSize),
					plot.margin = grid::unit(c(0, 0, 0.2, 0.2), 'in')
				)
			)

	### suitable area: present
	##########################

		### generalization

		subject <- 'SQ SUITABLE AREA'
		title <- 'c) Current climatically suitable area'
		errorless <- 'sqSuitArea_errorless_km2'
		precise <- 'sqSuitArea_precise_km2'
		imprecise <- 'sqSuitArea_imprecise_km2'
		
		ylab <- 'Estimated / omniscient suitable area'
		
		### y-axis limits
		these <- c(
			(results[ , imprecise]) / (results[ , errorless]),
			(results[ , precise]) / (results[ , errorless])
		)
		ys <- quantile(these, c(1 
		- outlierQuant, outlierQuant), na.rm=TRUE)
		ymin <- as.numeric(ys[1])
		ymax <- as.numeric(ys[2])
		
		panels <- list()
		count <- 0
		for (thisNumErrorless in numErrorlessSelected) {
		
			# subset number of precise so it's < total errorless
			allowableNumPreciseSelected <- numPreciseSelected[numPreciseSelected < thisNumErrorless]

			# x-axis limits
			these <- results[results$numErrorless == thisNumErrorless, ]
			xmax <- 1.015 * max(these$numAdmin)
			xmin <- 1 - 0.015 * xmax

			for (thisNumPrecise in allowableNumPreciseSelected) {

				say(subject, ' omniscient ', thisNumErrorless, ' precise ', thisNumPrecise)
				count <- count + 1

				# tall imprecise data
				thisResults <- results[results$numErrorless == thisNumErrorless & results$numPrecise == thisNumPrecise, ]
				resp <- thisResults[ , imprecise] / thisResults[ , errorless]
				n <- nrow(thisResults)
				impreciseDf <- data.frame(
					numAdmin = thisResults$numAdmin,
					resp = resp
				)
				
				# calculate quantiles for regression: imprecise
				impreciseDf <- impreciseDf[order(impreciseDf$numAdmin), ]
				impreciseDf <- impreciseDf[!is.infinite(log(impreciseDf$resp)), ]
				k <- if (length(unique(impreciseDf$numAdmin)) <= 6) { 1 } else { -1 }
				middleQuant <- qgam(log(resp) ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=0.5)
				lowerQuant <- qgam(log(resp) ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=lower)
				upperQuant <- qgam(log(resp) ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=upper)
				
				middleQuant <- exp(predict(middleQuant, impreciseDf, type='response'))
				lowerQuant <- exp(predict(lowerQuant, impreciseDf, type='response'))
				upperQuant <- exp(predict(upperQuant, impreciseDf, type='response'))

				middleImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					middle = middleQuant
				)
				
				topBoundaryImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					boundary = upperQuant
				)
				
				bottomBoundaryImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					boundary = lowerQuant
				)

				# frames for violin plot of precise
				preciseDf <- data.frame(
					numAdmin = 0,
					resp = thisResults[ , precise] / thisResults[ , errorless]
				)
				preciseDf$resp <- log10(preciseDf$resp)
				preciseDf$resp <- 10^(preciseDf$resp)

				# labels and titles
				main <- paste0(thisNumErrorless, ' omniscient, ', thisNumPrecise, ' precise')

				boxPanel <- ggplot(impreciseDf, aes(y=resp)) +
					geom_point(
						data=preciseDf,
						mapping=aes(x=0, y=resp),
						position = position_jitter(w = 0.4, h = 0),
						color = precisePointColAlpha,
						size = pointSize
					) +
					geom_hline(yintercept = 1, size=0.4, linetype='solid', col='gray40') +
					geom_violin(
						data = preciseDf[preciseDf$resp >= quantile(preciseDf$resp, lower, na.rm=TRUE) & preciseDf$resp <= quantile(preciseDf$resp, upper, na.rm=TRUE), ],
						mapping = aes(x=0, y = resp),
						draw_quantiles = 0.5,
						fill = NA
					) +
					scale_x_continuous(breaks=0, labels=c('Prec')) +
					scale_y_log10(limits=c(ymin, ymax), labels=scales::label_number_auto()) +
					# scale_y_continuous(limits=c(ymin, ymax), trans='log10') +
					ggtitle(main) +
					theme(
						plot.title = element_text(hjust = 0, vjust=-1.5, size=panelSize),
						plot.background = element_blank(),
						panel.grid.major.x = element_blank(),
						panel.grid.minor.x = element_blank(),
						axis.title.x = element_blank(),
						axis.text.x = element_text(angle = 0, hjust=0.5, vjust=0),
						axis.title.y = element_blank(),
						axis.text.y = element_text(size=axisNumSize),
					)

				xbreaks <- pretty(c(1, xmax), 3)
				xbreaks[1] <- 1
					
				imprecisePanel <- ggplot(impreciseDf, aes(x=numAdmin, y=resp)) +
					geom_point(data=impreciseDf, color=imprecisePointCol, size=pointSize) +					
					geom_line(data=topBoundaryImprecise, mapping=aes(x=numAdmin, y=boundary), size=boundaryLineThickness) +
					geom_line(data=bottomBoundaryImprecise, mapping=aes(x=numAdmin, y=boundary), size=boundaryLineThickness) +
					geom_line(data=middleImprecise, mapping=aes(x=numAdmin, y=middle), col=impreciseLineCol, size=medianSize) +
					geom_hline(yintercept = 1, size=0.4, linetype='solid', col='gray40') +
					scale_x_continuous(breaks = xbreaks, limits=c(xmin, xmax)) +
					scale_y_log10(limits=c(ymin, ymax), labels=scales::label_number_auto()) +
					# scale_y_continuous(limits=c(ymin, ymax), trans='log10') +
					xlab('County records') +
					theme(
						plot.background = element_blank(),
						plot.title = element_blank(),
						axis.title.x = element_text(size=axisSizeX, vjust=1),
						axis.text.x = element_text(size=axisNumSize),
						axis.title.y = element_blank(),
						axis.text.y = element_blank(),
						axis.ticks.y = element_blank()
					)

				boxPanel <- boxPanel + theme(plot.margin = grid::unit(c(0, 0, 0, 0.1), 'cm'))
				imprecisePanel <- imprecisePanel + theme(plot.margin = grid::unit(c(0, 0.2, 0, 0), 'cm'))

				comboPanel <- plot_grid(boxPanel, imprecisePanel, ncol=2, rel_widths=c(0.55, 1), align='h')
				if (length(panels) == 1) comboPanel <- comboPanel +
					geom_text(aes(x=0, y=1, label='Estimated / omniscient suitable area', angle=90, size=axisSizeY, vjust=0))

				panels[[count]] <- comboPanel
				names(panels)[count] <- paste0('errorless', thisNumErrorless, '_precise', thisNumPrecise)
				
			} # next number of precise
			
		} # next number of errorless

		sqSuit <- (panels[[1]] + panels[[3]]) / (panels[[2]] + panels[[4]]) +
			plot_layout(nrow=2) +
			plot_annotation(
				title = title,
				theme=theme(
					plot.title = element_text(size=setSize),
					plot.margin = grid::unit(c(0, 0, 0.2, 0.2), 'in')
				)
			)

	### suitable area: future
	##########################

		### generalization

		subject <- 'FUTURE SUITABLE AREA'
		title <- 'd) Future climatically suitable area'
		errorless <- 'futSuitArea_errorless_km2'
		precise <- 'futSuitArea_precise_km2'
		imprecise <- 'futSuitArea_imprecise_km2'
		
		ylab <- 'Estimated / omniscient suitable area'
		
		### y-axis limits
		these <- c(
			(results[ , imprecise] + 1) / (results[ , errorless] + 1),
			(results[ , precise] + 1) / (results[ , errorless] + 1)
		)
		ys <- quantile(these, c(1 - outlierQuant, outlierQuant), na.rm=TRUE)
		ymin <- as.numeric(ys[1])
		ymax <- as.numeric(ys[2])
		
		panels <- list()
		count <- 0
		for (thisNumErrorless in numErrorlessSelected) {
		
			# subset number of precise so it's < total errorless
			allowableNumPreciseSelected <- numPreciseSelected[numPreciseSelected < thisNumErrorless]

			# x-axis limits
			these <- results[results$numErrorless == thisNumErrorless, ]
			xmax <- 1.015 * max(these$numAdmin)
			xmin <- 1 - 0.015 * xmax

			for (thisNumPrecise in allowableNumPreciseSelected) {

				say(subject, ' omniscient ', thisNumErrorless, ' precise ', thisNumPrecise)
				count <- count + 1

				# tall imprecise data
				thisResults <- results[results$numErrorless == thisNumErrorless & results$numPrecise == thisNumPrecise, ]
				resp <- ((thisResults[ , imprecise] + 1) / (thisResults[ , errorless] + 1))
				n <- nrow(thisResults)
				impreciseDf <- data.frame(
					numAdmin = thisResults$numAdmin,
					resp = resp
				)
				
				# calculate quantiles for regression: imprecise
				impreciseDf <- impreciseDf[order(impreciseDf$numAdmin), ]
				impreciseDf <- impreciseDf[!is.infinite(impreciseDf$resp), ]
				k <- if (length(unique(impreciseDf$numAdmin)) <= 6) { 1 } else { -1 }
				middleQuant <- qgam(log(resp) ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=0.5)
				lowerQuant <- qgam(log(resp) ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=lower)
				upperQuant <- qgam(log(resp) ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=upper)
				
				middleQuant <- exp(predict(middleQuant, impreciseDf, type='response'))
				lowerQuant <- exp(predict(lowerQuant, impreciseDf, type='response'))
				upperQuant <- exp(predict(upperQuant, impreciseDf, type='response'))

				middleImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					middle = middleQuant
				)
				
				topBoundaryImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					boundary = upperQuant
				)
				
				bottomBoundaryImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					boundary = lowerQuant
				)

				# frames for violin plot of precise
				preciseDf <- data.frame(
					numAdmin = 0,
					resp = ((thisResults[ , precise] + 1) / (thisResults[ , errorless] + 1))
				)

				# labels and titles
				main <- paste0(thisNumErrorless, ' omniscient, ', thisNumPrecise, ' precise')

				boxPanel <- ggplot(impreciseDf, aes(y=resp)) +
					geom_point(
						data=preciseDf,
						mapping=aes(x=0, y=resp),
						position = position_jitter(w = 0.4, h = 0),
						color = precisePointColAlpha,
						size = pointSize
					) +
					geom_hline(yintercept = 1, size=0.4, linetype='solid', col='gray40') +
					geom_violin(
						data = preciseDf[preciseDf$resp >= quantile(preciseDf$resp, lower, na.rm=TRUE) & preciseDf$resp <= quantile(preciseDf$resp, upper, na.rm=TRUE), ],
						mapping = aes(x=0, y = resp),
						draw_quantiles = 0.5,
						fill = NA
					) +
					scale_x_continuous(breaks=0, labels=c('Prec')) +
					scale_y_log10(limits=c(ymin, ymax), labels=scales::label_number_auto()) +
					# scale_y_continuous(limits=c(ymin, ymax), trans='log10') +
					ggtitle(main) +
					theme(
						plot.title = element_text(hjust = 0, vjust=-1.5, size=panelSize),
						plot.background = element_blank(),
						panel.grid.major.x = element_blank(),
						panel.grid.minor.x = element_blank(),
						axis.title.x = element_blank(),
						axis.text.x = element_text(angle = 0, hjust=0.5, vjust=0),
						axis.title.y = element_blank(),
						axis.text.y = element_text(size=axisNumSize),
					)

				xbreaks <- pretty(c(1, xmax), 3)
				xbreaks[1] <- 1
					
				imprecisePanel <- ggplot(impreciseDf, aes(x=numAdmin, y=resp)) +
					geom_point(data=impreciseDf, color=imprecisePointCol, size=pointSize) +					
					geom_line(data=topBoundaryImprecise, mapping=aes(x=numAdmin, y=boundary), size=boundaryLineThickness) +
					geom_line(data=bottomBoundaryImprecise, mapping=aes(x=numAdmin, y=boundary), size=boundaryLineThickness) +
					geom_line(data=middleImprecise, mapping=aes(x=numAdmin, y=middle), col=impreciseLineCol, size=medianSize) +
					geom_hline(yintercept = 1, size=0.4, linetype='solid', col='gray40') +
					scale_x_continuous(breaks = xbreaks, limits=c(xmin, xmax)) +
					scale_y_log10(limits=c(ymin, ymax), labels=scales::label_number_auto()) +
					# scale_y_continuous(limits=c(ymin, ymax), trans='log10') +
					xlab('County records') +
					theme(
						plot.background = element_blank(),
						plot.title = element_blank(),
						axis.title.x = element_text(size=axisSizeX, vjust=1),
						axis.text.x = element_text(size=axisNumSize),
						axis.title.y = element_blank(),
						axis.text.y = element_blank(),
						axis.ticks.y = element_blank()
					)

				boxPanel <- boxPanel + theme(plot.margin = grid::unit(c(0, 0, 0, 0.1), 'cm'))
				imprecisePanel <- imprecisePanel + theme(plot.margin = grid::unit(c(0, 0.2, 0, 0), 'cm'))

				comboPanel <- plot_grid(boxPanel, imprecisePanel, ncol=2, rel_widths=c(0.55, 1), align='h')
				if (length(panels) == 1) comboPanel <- comboPanel +
					geom_text(aes(x=0, y=1, label='Estimated / omniscient suitable area', angle=90, size=axisSizeY, vjust=0))

				panels[[count]] <- comboPanel
				names(panels)[count] <- paste0('errorless', thisNumErrorless, '_precise', thisNumPrecise)
				
			} # next number of precise
			
		} # next number of errorless

		futSuit <- (panels[[1]] + panels[[3]]) / (panels[[2]] + panels[[4]]) +
			plot_layout(nrow=2) +
			plot_annotation(
				title = title,
				theme=theme(
					plot.title = element_text(size=setSize),
					plot.margin = grid::unit(c(0, 0, 0.2, 0.2), 'in')
				)
			)
			
	### combine
	###########
	
		# main <- plot_grid(calibSq, calibFut, loss, gain, ncol=2)
		main <- plot_grid(calibSq, calibFut, sqSuit, futSuit, ncol=2)
		main <- main + theme(plot.margin = grid::unit(c(0.5, 0.5, 0.5, 0.5), 'in'))
		
		ggsave(plot=main, filename=paste0('./Analysis/Virtual Species/!Selected ENM Results ', paste(numErrorlessSelected, collapse=' '), ' Omniscient & ', paste(numPreciseSelected, collapse=' '), ' Precise REDUCED.pdf'), width=8.5, height=9, units='in')

		say('Displaying only lower ', outlierQuant, 'quantile of points.', level=2, deco='!')
		
		
say('##################################################################')
say('### make composite plot of ALL CLIMATE CHANGE EXPOSURE results ###')
say('##################################################################')

	# panels, one per scenario, with each column number of errorless, rows number of precise

	### generalization
	##################

		# text size
		panelSize <- 8 # panel title
		axisSizeX <- 8 # axes with labels
		axisSizeY <- 16 # axes with labels
		axisNumSize <- 8 # numbers on axes
		
		# lines
		medianSize <- 1
		hlineSize <- 0.6 # thickness of horizontal line at 1
		boundaryLineThickness <- 0.4 # thickness of inner quantile boundary's lines
		
		# point size
		pointSize <- 0.25
		
		# colors
		pointCol <- '#d95f02'
		imprecisePointCol <- '#d95f02'
		
		impreciseFill <- NA
		impreciseLineCol <- 'black'

		preciseFill <- '#1b9e77'
		precisePointColAlpha <- alpha('#1b9e77', 0.5)

		# quantiles to plot
		lower <- 0.05
		upper <- 0.95
	
	### preliminary
	###############

		load('./Analysis/Virtual Species/!!Collated Results.rda')
		numErrorlessSelected <- sort(unique(results$numErrorless))
		numPreciseSelected <- sort(unique(results$numPrecise))

	### suitable area: current
	#########################

		### generalization

		subject <- 'SQ SUITABLE AREA'
		errorless <- 'sqSuitArea_errorless_km2'
		precise <- 'sqSuitArea_precise_km2'
		imprecise <- 'sqSuitArea_imprecise_km2'
		
		# ylab <- expression(paste0(log[10]*'(Ratio of estimated-to-true EOO)'))
		ylab <- 'Ratio of estimated-to-true suitable area'
		
		plotFileName <- '!Climate Change Exposure - Current Suitable Area - All Results'
		trendsFileName <- '!Trends - Climate Change Exposure - Current Suitable Area'

		caption <- sqSuitCaption

		### y-axis limits
		these <- c(
			(results[ , imprecise] + 1) / (results[ , errorless] + 1),
			(results[ , precise] + 1) / (results[ , errorless] + 1)
		)
		these <- log10(these)
		ys <- quantile(these, c(1 - outlierQuant, outlierQuant), na.rm=TRUE)
		ymin <- as.numeric(ys[1])
		ymax <- as.numeric(ys[2])
		ymin <- 10^ymin
		ymax <- 10^ymax
		
		trends <- data.frame()
		panels <- list()
		count <- 0
		for (thisNumErrorless in numErrorlessSelected) {
		
			# subset number of precise so it's < total errorless
			allowableNumPreciseSelected <- numPreciseSelected[numPreciseSelected < thisNumErrorless]

			# x-axis limits
			these <- results[results$numErrorless == thisNumErrorless, ]
			xmax <- 1.015 * max(these$numAdmin)
			xmin <- 1 - 0.015 * xmax

			for (thisNumPrecise in allowableNumPreciseSelected) {

				say(subject, ' omniscient ', thisNumErrorless, ' precise ', thisNumPrecise)
				count <- count + 1

				# tall imprecise data
				thisResults <- results[results$numErrorless == thisNumErrorless & results$numPrecise == thisNumPrecise, ]
				resp <- ((thisResults[ , imprecise] + 1) / (thisResults[ , errorless] + 1))
				n <- nrow(thisResults)
				impreciseDf <- data.frame(
					numAdmin = thisResults$numAdmin,
					resp = resp
				)
				
				# calculate quantiles for regression: imprecise
				impreciseDf <- impreciseDf[order(impreciseDf$numAdmin), ]
				impreciseDf <- impreciseDf[!is.infinite(impreciseDf$resp), ]
				k <- if (length(unique(impreciseDf$numAdmin)) <= 6) { 1 } else { -1 }
				middleQuant <- qgam(log10(resp) ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=0.5)
				lowerQuant <- qgam(log10(resp) ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=lower)
				upperQuant <- qgam(log10(resp) ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=upper)
				
				middleQuant <- 10^predict(middleQuant, impreciseDf, type='response')
				lowerQuant <- 10^predict(lowerQuant, impreciseDf, type='response')
				upperQuant <- 10^predict(upperQuant, impreciseDf, type='response')

				middleImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					middle = middleQuant
				)
				
				topBoundaryImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					boundary = upperQuant
				)
				
				bottomBoundaryImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					boundary = lowerQuant
				)

				# frames for violin plot of precise
				preciseDf <- data.frame(
					numAdmin = 0,
					resp = (thisResults[ , precise] + 1) / (thisResults[ , errorless] + 1)
				)
				preciseUpper <- quantile(preciseDf$resp, upper, na.rm=TRUE)
				preciseMiddle <- quantile(preciseDf$resp, 0.5, na.rm=TRUE)
				preciseLower <- quantile(preciseDf$resp, lower, na.rm=TRUE)

				# trends
				trends <- rbind(
					trends,
					data.frame(
						numErrorless = thisNumErrorless,
						numPrecise = thisNumPrecise,
						preciseLower = as.numeric(preciseLower),
						preciseMedian = as.numeric(preciseMiddle),
						preciseUpper = as.numeric(preciseUpper),
						propImpreciseGt1 = sum(resp > 1, na.rm=TRUE) / length(na.omit(resp)),
						max95thQuant = max(upperQuant),
						lowerImpreciseQuantAtMaxNumberOfCounties = as.numeric(lowerQuant[which.max(impreciseDf$numAdmin)]),
						medianImpreciseAtMaxNumberOfCounties = as.numeric(middleQuant[which.max(impreciseDf$numAdmin)]),
						upperImpreciseQuantAtMaxNumberOfCounties = as.numeric(upperQuant[which.max(impreciseDf$numAdmin)])
					)
				)

				# labels and titles
				letter <- if (count <= 26) { letters[count] } else { paste0(letters[count - 26], '\'') }
				main <- paste0(letter, ') ', thisNumErrorless, ' omniscient\n& ', thisNumPrecise, ' precise')

				boxPanel <- ggplot(impreciseDf, aes(y=resp)) +
					geom_point(
						data=preciseDf,
						mapping=aes(x=0, y=resp),
						position = position_jitter(w = 0.4, h = 0),
						color = precisePointColAlpha,
						size = pointSize
					) +
					geom_hline(yintercept = 1, size=0.4, linetype='solid', col='gray40') +
					geom_violin(
						data = preciseDf[preciseDf$resp >= quantile(preciseDf$resp, lower, na.rm=TRUE) & preciseDf$resp <= quantile(preciseDf$resp, upper, na.rm=TRUE), ],
						mapping = aes(x=0, y = resp),
						draw_quantiles = 0.5,
						fill = NA
					) +
					scale_x_continuous(breaks=0, labels=c('Prec')) +
					scale_y_log10(limits=c(ymin, ymax), labels=scales::label_number_auto()) +
					# scale_y_continuous(limits=c(ymin, ymax), trans='log10') +
					ggtitle(main) +
					theme(
						plot.title = element_text(hjust = 0, vjust=-1.5, size=panelSize),
						plot.background = element_blank(),
						panel.grid.major.x = element_blank(),
						panel.grid.minor.x = element_blank(),
						axis.title.x = element_blank(),
						axis.text.x = element_text(angle = 0, hjust=0.5, vjust=0),
						axis.title.y = element_blank(),
						axis.text.y = element_text(size=axisNumSize),
					)

				xbreaks <- pretty(c(1, xmax), 3)
				xbreaks[1] <- 1
					
				imprecisePanel <- ggplot(impreciseDf, aes(x=numAdmin, y=resp)) +
					geom_point(data=impreciseDf, color=imprecisePointCol, size=pointSize) +					
					geom_line(data=topBoundaryImprecise, mapping=aes(x=numAdmin, y=boundary), size=boundaryLineThickness) +
					geom_line(data=bottomBoundaryImprecise, mapping=aes(x=numAdmin, y=boundary), size=boundaryLineThickness) +
					geom_line(data=middleImprecise, mapping=aes(x=numAdmin, y=middle), col=impreciseLineCol, size=medianSize) +
					geom_hline(yintercept = 1, size=0.4, linetype='solid', col='gray40') +
					scale_x_continuous(breaks = xbreaks, limits=c(xmin, xmax)) +
					scale_y_log10(limits=c(ymin, ymax), labels=scales::label_number_auto()) +
					# scale_y_continuous(limits=c(ymin, ymax), trans='log10') +
					xlab('County records') +
					theme(
						plot.background = element_blank(),
						plot.title = element_blank(),
						axis.title.x = element_text(size=axisSizeX, vjust=1),
						axis.text.x = element_text(size=axisNumSize),
						axis.title.y = element_blank(),
						axis.text.y = element_blank(),
						axis.ticks.y = element_blank()
					)

				boxPanel <- boxPanel + theme(plot.margin = grid::unit(c(0, 0, 0, 0.1), 'cm'))
				imprecisePanel <- imprecisePanel + theme(plot.margin = grid::unit(c(0, 0.2, 0, 0), 'cm'))

				comboPanel <- plot_grid(boxPanel, imprecisePanel, ncol=2, rel_widths=c(0.55, 1), align='h')
				if (count %in% 1:3) comboPanel <- comboPanel + theme(plot.margin=grid::unit(c(0, 0, 0, 1), 'cm'))
				if (count == 3) comboPanel <- comboPanel +
					geom_text(aes(x=0, y=0, label=ylab, angle=90, size=axisSizeY, vjust=-1))

				top <- 0.5
				right <- 0.5
				bottom <- 0
				left <- 1

				if (count %in% c(1)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(top, 0, 0, right), 'in'))
				if (count %in% c(2:3)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(0, 0, 0, right), 'in'))
				if (count %in% c(4, 10, 16)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(top, 0, 0, 0), 'in'))
				if (count %in% c(22)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(top, right, 0, 0), 'in'))
				if (count %in% c(22:27)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(0, right, 0, 0), 'in'))
				if (count %in% c(27)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(0, right, bottom, 0), 'in'))
				if (count %in% c(9, 15, 21, 27)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(0, 0, bottom, 0), 'in'))

				panels[[count]] <- comboPanel
				names(panels)[count] <- paste0('errorless', thisNumErrorless, '_precise', thisNumPrecise)
				
			} # next number of precise
			
		} # next number of errorless

		main <- panels[[1]] + panels[[2]] + panels[[3]] + plot_spacer() + plot_spacer() + plot_spacer() + # errorless 20
			panels[[4]] + panels[[5]] + panels[[6]] + panels[[7]] + panels[[8]] + panels[[9]] + # errorless 40
			panels[[10]] + panels[[11]] + panels[[12]] + panels[[13]] + panels[[14]] + panels[[15]] + # errorless 80
			panels[[16]] + panels[[17]] + panels[[18]] + panels[[19]] + panels[[20]] + panels[[21]] + # errorless 160
			panels[[22]] + panels[[23]] + panels[[24]] + panels[[25]] + panels[[26]] + panels[[27]] + # errorless 320
			plot_layout(byrow=FALSE, nrow=6)

		main <- main +
			plot_annotation(
				caption = str_wrap(caption, width=105),
				theme=theme(
					plot.caption=element_text(
						size=11,
						hjust=0,
						vjust=5
					)
				)
			)

		ggsave(plot=main, filename=paste0('./Analysis/Virtual Species/', plotFileName, '.pdf'), width=8.5, height=11, units='in')
		write.csv(trends, paste0('./Analysis/Virtual Species/', trendsFileName, '.csv'), row.names=FALSE)
		
	### suitable area: future
	#########################

		### generalization

		subject <- 'FUT SUITABLE AREA'
		errorless <- 'futSuitArea_errorless_km2'
		precise <- 'futSuitArea_precise_km2'
		imprecise <- 'futSuitArea_imprecise_km2'
		
		# ylab <- expression(paste0(log[10]*'(Ratio of estimated-to-true EOO)'))
		ylab <- 'Ratio of estimated-to-true suitable area'
		
		plotFileName <- '!Climate Change Exposure - Future Suitable Area - All Results'
		trendsFileName <- '!Trends - Climate Change Exposure - Future Suitable Area'

		caption <- futSuitCaption

		### y-axis limits
		these <- c(
			(results[ , imprecise] + 1) / (results[ , errorless] + 1),
			(results[ , precise] + 1) / (results[ , errorless] + 1)
		)
		these <- log10(these)
		ys <- quantile(these, c(1 - outlierQuant, outlierQuant), na.rm=TRUE)
		ymin <- as.numeric(ys[1])
		ymax <- as.numeric(ys[2])
		ymin <- 10^ymin
		ymax <- 10^ymax
		
		trends <- data.frame()
		panels <- list()
		count <- 0
		for (thisNumErrorless in numErrorlessSelected) {
		
			# subset number of precise so it's < total errorless
			allowableNumPreciseSelected <- numPreciseSelected[numPreciseSelected < thisNumErrorless]

			# x-axis limits
			these <- results[results$numErrorless == thisNumErrorless, ]
			xmax <- 1.015 * max(these$numAdmin)
			xmin <- 1 - 0.015 * xmax

			for (thisNumPrecise in allowableNumPreciseSelected) {

				say(subject, ' omniscient ', thisNumErrorless, ' precise ', thisNumPrecise)
				count <- count + 1

				# tall imprecise data
				thisResults <- results[results$numErrorless == thisNumErrorless & results$numPrecise == thisNumPrecise, ]
				resp <- ((thisResults[ , imprecise] + 1) / (thisResults[ , errorless] + 1))
				n <- nrow(thisResults)
				impreciseDf <- data.frame(
					numAdmin = thisResults$numAdmin,
					resp = resp
				)
				
				# calculate quantiles for regression: imprecise
				impreciseDf <- impreciseDf[order(impreciseDf$numAdmin), ]
				impreciseDf <- impreciseDf[!is.infinite(impreciseDf$resp), ]
				k <- if (length(unique(impreciseDf$numAdmin)) <= 6) { 1 } else { -1 }
				middleQuant <- qgam(log10(resp) ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=0.5)
				lowerQuant <- qgam(log10(resp) ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=lower)
				upperQuant <- qgam(log10(resp) ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=upper)
				
				middleQuant <- 10^predict(middleQuant, impreciseDf, type='response')
				lowerQuant <- 10^predict(lowerQuant, impreciseDf, type='response')
				upperQuant <- 10^predict(upperQuant, impreciseDf, type='response')

				middleImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					middle = middleQuant
				)
				
				topBoundaryImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					boundary = upperQuant
				)
				
				bottomBoundaryImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					boundary = lowerQuant
				)

				# frames for violin plot of precise
				preciseDf <- data.frame(
					numAdmin = 0,
					resp = (thisResults[ , precise] + 1) / (thisResults[ , errorless] + 1)
				)
				preciseUpper <- quantile(preciseDf$resp, upper, na.rm=TRUE)
				preciseMiddle <- quantile(preciseDf$resp, 0.5, na.rm=TRUE)
				preciseLower <- quantile(preciseDf$resp, lower, na.rm=TRUE)

				# trends
				trends <- rbind(
					trends,
					data.frame(
						numErrorless = thisNumErrorless,
						numPrecise = thisNumPrecise,
						preciseLower = as.numeric(preciseLower),
						preciseMedian = as.numeric(preciseMiddle),
						preciseUpper = as.numeric(preciseUpper),
						propImpreciseGt1 = sum(resp > 1, na.rm=TRUE) / length(na.omit(resp)),
						max95thQuant = max(upperQuant),
						lowerImpreciseQuantAtMaxNumberOfCounties = as.numeric(lowerQuant[which.max(impreciseDf$numAdmin)]),
						medianImpreciseAtMaxNumberOfCounties = as.numeric(middleQuant[which.max(impreciseDf$numAdmin)]),
						upperImpreciseQuantAtMaxNumberOfCounties = as.numeric(upperQuant[which.max(impreciseDf$numAdmin)])
					)
				)

				# labels and titles
				letter <- if (count <= 26) { letters[count] } else { paste0(letters[count - 26], '\'') }
				main <- paste0(letter, ') ', thisNumErrorless, ' omniscient\n& ', thisNumPrecise, ' precise')

				boxPanel <- ggplot(impreciseDf, aes(y=resp)) +
					geom_point(
						data=preciseDf,
						mapping=aes(x=0, y=resp),
						position = position_jitter(w = 0.4, h = 0),
						color = precisePointColAlpha,
						size = pointSize
					) +
					geom_hline(yintercept = 1, size=0.4, linetype='solid', col='gray40') +
					geom_violin(
						data = preciseDf[preciseDf$resp >= quantile(preciseDf$resp, lower, na.rm=TRUE) & preciseDf$resp <= quantile(preciseDf$resp, upper, na.rm=TRUE), ],
						mapping = aes(x=0, y = resp),
						draw_quantiles = 0.5,
						fill = NA
					) +
					scale_x_continuous(breaks=0, labels=c('Prec')) +
					scale_y_log10(limits=c(ymin, ymax), labels=scales::label_number_auto()) +
					# scale_y_continuous(limits=c(ymin, ymax), trans='log10') +
					ggtitle(main) +
					theme(
						plot.title = element_text(hjust = 0, vjust=-1.5, size=panelSize),
						plot.background = element_blank(),
						panel.grid.major.x = element_blank(),
						panel.grid.minor.x = element_blank(),
						axis.title.x = element_blank(),
						axis.text.x = element_text(angle = 0, hjust=0.5, vjust=0),
						axis.title.y = element_blank(),
						axis.text.y = element_text(size=axisNumSize),
					)

				xbreaks <- pretty(c(1, xmax), 3)
				xbreaks[1] <- 1
					
				imprecisePanel <- ggplot(impreciseDf, aes(x=numAdmin, y=resp)) +
					geom_point(data=impreciseDf, color=imprecisePointCol, size=pointSize) +					
					geom_line(data=topBoundaryImprecise, mapping=aes(x=numAdmin, y=boundary), size=boundaryLineThickness) +
					geom_line(data=bottomBoundaryImprecise, mapping=aes(x=numAdmin, y=boundary), size=boundaryLineThickness) +
					geom_line(data=middleImprecise, mapping=aes(x=numAdmin, y=middle), col=impreciseLineCol, size=medianSize) +
					geom_hline(yintercept = 1, size=0.4, linetype='solid', col='gray40') +
					scale_x_continuous(breaks = xbreaks, limits=c(xmin, xmax)) +
					scale_y_log10(limits=c(ymin, ymax), labels=scales::label_number_auto()) +
					# scale_y_continuous(limits=c(ymin, ymax), trans='log10') +
					xlab('County records') +
					theme(
						plot.background = element_blank(),
						plot.title = element_blank(),
						axis.title.x = element_text(size=axisSizeX, vjust=1),
						axis.text.x = element_text(size=axisNumSize),
						axis.title.y = element_blank(),
						axis.text.y = element_blank(),
						axis.ticks.y = element_blank()
					)

				boxPanel <- boxPanel + theme(plot.margin = grid::unit(c(0, 0, 0, 0.1), 'cm'))
				imprecisePanel <- imprecisePanel + theme(plot.margin = grid::unit(c(0, 0.2, 0, 0), 'cm'))

				comboPanel <- plot_grid(boxPanel, imprecisePanel, ncol=2, rel_widths=c(0.55, 1), align='h')
				if (count %in% 1:3) comboPanel <- comboPanel + theme(plot.margin=grid::unit(c(0, 0, 0, 1), 'cm'))
				if (count == 3) comboPanel <- comboPanel +
					geom_text(aes(x=0, y=0, label=ylab, angle=90, size=axisSizeY, vjust=-1))

				top <- 0.5
				right <- 0.5
				bottom <- 0
				left <- 1

				if (count %in% c(1)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(top, 0, 0, right), 'in'))
				if (count %in% c(2:3)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(0, 0, 0, right), 'in'))
				if (count %in% c(4, 10, 16)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(top, 0, 0, 0), 'in'))
				if (count %in% c(22)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(top, right, 0, 0), 'in'))
				if (count %in% c(22:27)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(0, right, 0, 0), 'in'))
				if (count %in% c(27)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(0, right, bottom, 0), 'in'))
				if (count %in% c(9, 15, 21, 27)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(0, 0, bottom, 0), 'in'))

				panels[[count]] <- comboPanel
				names(panels)[count] <- paste0('errorless', thisNumErrorless, '_precise', thisNumPrecise)
				
			} # next number of precise
			
		} # next number of errorless

		main <- panels[[1]] + panels[[2]] + panels[[3]] + plot_spacer() + plot_spacer() + plot_spacer() + # errorless 20
			panels[[4]] + panels[[5]] + panels[[6]] + panels[[7]] + panels[[8]] + panels[[9]] + # errorless 40
			panels[[10]] + panels[[11]] + panels[[12]] + panels[[13]] + panels[[14]] + panels[[15]] + # errorless 80
			panels[[16]] + panels[[17]] + panels[[18]] + panels[[19]] + panels[[20]] + panels[[21]] + # errorless 160
			panels[[22]] + panels[[23]] + panels[[24]] + panels[[25]] + panels[[26]] + panels[[27]] + # errorless 320
			plot_layout(byrow=FALSE, nrow=6)

		main <- main +
			plot_annotation(
				caption = str_wrap(caption, width=105),
				theme=theme(
					plot.caption=element_text(
						size=11,
						hjust=0,
						vjust=5
					)
				)
			)

		ggsave(plot=main, filename=paste0('./Analysis/Virtual Species/', plotFileName, '.pdf'), width=8.5, height=11, units='in')
		write.csv(trends, paste0('./Analysis/Virtual Species/', trendsFileName, '.csv'), row.names=FALSE)
		
	### suitable area: stable
	#########################

		### generalization

		subject <- 'STABLE SUITABLE AREA'
		errorless <- 'stableArea_errorless_km2'
		precise <- 'stableArea_precise_km2'
		imprecise <- 'stableArea_imprecise_km2'
		
		# ylab <- expression(paste0(log[10]*'(Ratio of estimated-to-true EOO)'))
		ylab <- 'Ratio of estimated-to-true stable suitable area'
		
		plotFileName <- '!Climate Change Exposure - Stable Suitable Area - All Results'
		trendsFileName <- '!Trends - Climate Change Exposure - Stable Suitable Area'

		caption <- stableCaption

		### y-axis limits
		these <- c(
			(results[ , imprecise] + 1) / (results[ , errorless] + 1),
			(results[ , precise] + 1) / (results[ , errorless] + 1)
		)
		these <- log10(these)
		ys <- quantile(these, c(1 - outlierQuant, outlierQuant), na.rm=TRUE)
		ymin <- as.numeric(ys[1])
		ymax <- as.numeric(ys[2])
		ymin <- 10^ymin
		ymax <- 10^ymax
		
		trends <- data.frame()
		panels <- list()
		count <- 0
		for (thisNumErrorless in numErrorlessSelected) {
		
			# subset number of precise so it's < total errorless
			allowableNumPreciseSelected <- numPreciseSelected[numPreciseSelected < thisNumErrorless]

			# x-axis limits
			these <- results[results$numErrorless == thisNumErrorless, ]
			xmax <- 1.015 * max(these$numAdmin)
			xmin <- 1 - 0.015 * xmax

			for (thisNumPrecise in allowableNumPreciseSelected) {

				say(subject, ' omniscient ', thisNumErrorless, ' precise ', thisNumPrecise)
				count <- count + 1

				# tall imprecise data
				thisResults <- results[results$numErrorless == thisNumErrorless & results$numPrecise == thisNumPrecise, ]
				resp <- (thisResults[ , imprecise] + 1) / (thisResults[ , errorless] + 1)
				n <- nrow(thisResults)
				impreciseDf <- data.frame(
					numAdmin = thisResults$numAdmin,
					resp = resp
				)
				
				# calculate quantiles for regression: imprecise
				impreciseDf <- impreciseDf[order(impreciseDf$numAdmin), ]
				impreciseDf <- impreciseDf[!is.infinite(impreciseDf$resp), ]
				k <- if (length(unique(impreciseDf$numAdmin)) <= 6) { 1 } else { -1 }
				middleQuant <- qgam(log10(resp) ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=0.5)
				lowerQuant <- qgam(log10(resp) ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=lower)
				upperQuant <- qgam(log10(resp) ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=upper)
				
				middleQuant <- 10^predict(middleQuant, impreciseDf, type='response')
				lowerQuant <- 10^predict(lowerQuant, impreciseDf, type='response')
				upperQuant <- 10^predict(upperQuant, impreciseDf, type='response')

				middleImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					middle = middleQuant
				)
				
				topBoundaryImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					boundary = upperQuant
				)
				
				bottomBoundaryImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					boundary = lowerQuant
				)

				# frames for violin plot of precise
				preciseDf <- data.frame(
					numAdmin = 0,
					resp = (thisResults[ , precise] + 1) / (thisResults[ , errorless] + 1)
				)
				preciseUpper <- quantile(preciseDf$resp, upper, na.rm=TRUE)
				preciseMiddle <- quantile(preciseDf$resp, 0.5, na.rm=TRUE)
				preciseLower <- quantile(preciseDf$resp, lower, na.rm=TRUE)

				# trends
				trends <- rbind(
					trends,
					data.frame(
						numErrorless = thisNumErrorless,
						numPrecise = thisNumPrecise,
						preciseLower = as.numeric(preciseLower),
						preciseMedian = as.numeric(preciseMiddle),
						preciseUpper = as.numeric(preciseUpper),
						propImpreciseGt1 = sum(resp > 1, na.rm=TRUE) / length(na.omit(resp)),
						max95thQuant = max(upperQuant),
						lowerImpreciseQuantAtMaxNumberOfCounties = as.numeric(lowerQuant[which.max(impreciseDf$numAdmin)]),
						medianImpreciseAtMaxNumberOfCounties = as.numeric(middleQuant[which.max(impreciseDf$numAdmin)]),
						upperImpreciseQuantAtMaxNumberOfCounties = as.numeric(upperQuant[which.max(impreciseDf$numAdmin)])
					)
				)

				# labels and titles
				letter <- if (count <= 26) { letters[count] } else { paste0(letters[count - 26], '\'') }
				main <- paste0(letter, ') ', thisNumErrorless, ' omniscient\n& ', thisNumPrecise, ' precise')

				boxPanel <- ggplot(impreciseDf, aes(y=resp)) +
					geom_point(
						data=preciseDf,
						mapping=aes(x=0, y=resp),
						position = position_jitter(w = 0.4, h = 0),
						color = precisePointColAlpha,
						size = pointSize
					) +
					geom_hline(yintercept = 1, size=0.4, linetype='solid', col='gray40') +
					geom_violin(
						data = preciseDf[preciseDf$resp >= quantile(preciseDf$resp, lower, na.rm=TRUE) & preciseDf$resp <= quantile(preciseDf$resp, upper, na.rm=TRUE), ],
						mapping = aes(x=0, y = resp),
						draw_quantiles = 0.5,
						fill = NA
					) +
					scale_x_continuous(breaks=0, labels=c('Prec')) +
					scale_y_log10(limits=c(ymin, ymax), labels=scales::label_number_auto()) +
					# scale_y_continuous(limits=c(ymin, ymax), trans='log10') +
					ggtitle(main) +
					theme(
						plot.title = element_text(hjust = 0, vjust=-1.5, size=panelSize),
						plot.background = element_blank(),
						panel.grid.major.x = element_blank(),
						panel.grid.minor.x = element_blank(),
						axis.title.x = element_blank(),
						axis.text.x = element_text(angle = 0, hjust=0.5, vjust=0),
						axis.title.y = element_blank(),
						axis.text.y = element_text(size=axisNumSize),
					)

				xbreaks <- pretty(c(1, xmax), 3)
				xbreaks[1] <- 1
					
				imprecisePanel <- ggplot(impreciseDf, aes(x=numAdmin, y=resp)) +
					geom_point(data=impreciseDf, color=imprecisePointCol, size=pointSize) +					
					geom_line(data=topBoundaryImprecise, mapping=aes(x=numAdmin, y=boundary), size=boundaryLineThickness) +
					geom_line(data=bottomBoundaryImprecise, mapping=aes(x=numAdmin, y=boundary), size=boundaryLineThickness) +
					geom_line(data=middleImprecise, mapping=aes(x=numAdmin, y=middle), col=impreciseLineCol, size=medianSize) +
					geom_hline(yintercept = 1, size=0.4, linetype='solid', col='gray40') +
					scale_x_continuous(breaks = xbreaks, limits=c(xmin, xmax)) +
					scale_y_log10(limits=c(ymin, ymax), labels=scales::label_number_auto()) +
					# scale_y_continuous(limits=c(ymin, ymax), trans='log10') +
					xlab('County records') +
					theme(
						plot.background = element_blank(),
						plot.title = element_blank(),
						axis.title.x = element_text(size=axisSizeX, vjust=1),
						axis.text.x = element_text(size=axisNumSize),
						axis.title.y = element_blank(),
						axis.text.y = element_blank(),
						axis.ticks.y = element_blank()
					)

				boxPanel <- boxPanel + theme(plot.margin = grid::unit(c(0, 0, 0, 0.1), 'cm'))
				imprecisePanel <- imprecisePanel + theme(plot.margin = grid::unit(c(0, 0.2, 0, 0), 'cm'))

				comboPanel <- plot_grid(boxPanel, imprecisePanel, ncol=2, rel_widths=c(0.55, 1), align='h')
				if (count %in% 1:3) comboPanel <- comboPanel + theme(plot.margin=grid::unit(c(0, 0, 0, 1), 'cm'))
				if (count == 3) comboPanel <- comboPanel +
					geom_text(aes(x=0, y=0, label=ylab, angle=90, size=axisSizeY, vjust=-1))

				top <- 0.5
				right <- 0.5
				bottom <- 0
				left <- 1

				if (count %in% c(1)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(top, 0, 0, right), 'in'))
				if (count %in% c(2:3)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(0, 0, 0, right), 'in'))
				if (count %in% c(4, 10, 16)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(top, 0, 0, 0), 'in'))
				if (count %in% c(22)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(top, right, 0, 0), 'in'))
				if (count %in% c(22:27)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(0, right, 0, 0), 'in'))
				if (count %in% c(27)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(0, right, bottom, 0), 'in'))
				if (count %in% c(9, 15, 21, 27)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(0, 0, bottom, 0), 'in'))

				panels[[count]] <- comboPanel
				names(panels)[count] <- paste0('errorless', thisNumErrorless, '_precise', thisNumPrecise)
				
			} # next number of precise
			
		} # next number of errorless

		main <- panels[[1]] + panels[[2]] + panels[[3]] + plot_spacer() + plot_spacer() + plot_spacer() + # errorless 20
			panels[[4]] + panels[[5]] + panels[[6]] + panels[[7]] + panels[[8]] + panels[[9]] + # errorless 40
			panels[[10]] + panels[[11]] + panels[[12]] + panels[[13]] + panels[[14]] + panels[[15]] + # errorless 80
			panels[[16]] + panels[[17]] + panels[[18]] + panels[[19]] + panels[[20]] + panels[[21]] + # errorless 160
			panels[[22]] + panels[[23]] + panels[[24]] + panels[[25]] + panels[[26]] + panels[[27]] + # errorless 320
			plot_layout(byrow=FALSE, nrow=6)

		main <- main +
			plot_annotation(
				caption = str_wrap(caption, width=105),
				theme=theme(
					plot.caption=element_text(
						size=11,
						hjust=0,
						vjust=5
					)
				)
			)

		ggsave(plot=main, filename=paste0('./Analysis/Virtual Species/', plotFileName, '.pdf'), width=8.5, height=11, units='in')
		write.csv(trends, paste0('./Analysis/Virtual Species/', trendsFileName, '.csv'), row.names=FALSE)
		
	### suitable area: lost
	#######################

		### generalization

		subject <- 'LOST SUITABLE AREA'
		errorless <- 'lostArea_errorless_km2'
		precise <- 'lostArea_precise_km2'
		imprecise <- 'lostArea_imprecise_km2'
		
		# ylab <- expression(paste0(log[10]*'(Ratio of estimated-to-true EOO)'))
		ylab <- 'Ratio of estimated-to-true lost suitable area'
		
		plotFileName <- '!Climate Change Exposure - Lost Suitable Area - All Results'
		trendsFileName <- '!Trends - Climate Change Exposure - Lost Suitable Area'

		caption <- lostCaption

		### y-axis limits
		these <- c(
			(results[ , imprecise] + 1) / (results[ , errorless] + 1),
			(results[ , precise] + 1) / (results[ , errorless] + 1)
		)
		these <- log10(these)
		ys <- quantile(these, c(1 - outlierQuant, outlierQuant), na.rm=TRUE)
		ymin <- as.numeric(ys[1])
		ymax <- as.numeric(ys[2])
		ymin <- 10^ymin
		ymax <- 10^ymax
		
		trends <- data.frame()
		panels <- list()
		count <- 0
		for (thisNumErrorless in numErrorlessSelected) {
		
			# subset number of precise so it's < total errorless
			allowableNumPreciseSelected <- numPreciseSelected[numPreciseSelected < thisNumErrorless]

			# x-axis limits
			these <- results[results$numErrorless == thisNumErrorless, ]
			xmax <- 1.015 * max(these$numAdmin)
			xmin <- 1 - 0.015 * xmax

			for (thisNumPrecise in allowableNumPreciseSelected) {

				say(subject, ' omniscient ', thisNumErrorless, ' precise ', thisNumPrecise)
				count <- count + 1

				# tall imprecise data
				thisResults <- results[results$numErrorless == thisNumErrorless & results$numPrecise == thisNumPrecise, ]
				resp <- ((thisResults[ , imprecise] + 1) / (thisResults[ , errorless] + 1))
				n <- nrow(thisResults)
				impreciseDf <- data.frame(
					numAdmin = thisResults$numAdmin,
					resp = resp
				)
				
				# calculate quantiles for regression: imprecise
				impreciseDf <- impreciseDf[order(impreciseDf$numAdmin), ]
				impreciseDf <- impreciseDf[!is.infinite(impreciseDf$resp), ]
				k <- if (length(unique(impreciseDf$numAdmin)) <= 6) { 1 } else { -1 }
				middleQuant <- qgam(log10(resp) ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=0.5)
				lowerQuant <- qgam(log10(resp) ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=lower)
				upperQuant <- qgam(log10(resp) ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=upper)
				
				middleQuant <- 10^predict(middleQuant, impreciseDf, type='response')
				lowerQuant <- 10^predict(lowerQuant, impreciseDf, type='response')
				upperQuant <- 10^predict(upperQuant, impreciseDf, type='response')

				middleImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					middle = middleQuant
				)
				
				topBoundaryImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					boundary = upperQuant
				)
				
				bottomBoundaryImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					boundary = lowerQuant
				)

				# frames for violin plot of precise
				preciseDf <- data.frame(
					numAdmin = 0,
					resp = (thisResults[ , precise] + 1) / (thisResults[ , errorless] + 1)
				)
				preciseUpper <- quantile(preciseDf$resp, upper, na.rm=TRUE)
				preciseMiddle <- quantile(preciseDf$resp, 0.5, na.rm=TRUE)
				preciseLower <- quantile(preciseDf$resp, lower, na.rm=TRUE)

				# trends
				trends <- rbind(
					trends,
					data.frame(
						numErrorless = thisNumErrorless,
						numPrecise = thisNumPrecise,
						preciseLower = as.numeric(preciseLower),
						preciseMedian = as.numeric(preciseMiddle),
						preciseUpper = as.numeric(preciseUpper),
						propImpreciseGt1 = sum(resp > 1, na.rm=TRUE) / length(na.omit(resp)),
						max95thQuant = max(upperQuant),
						lowerImpreciseQuantAtMaxNumberOfCounties = as.numeric(lowerQuant[which.max(impreciseDf$numAdmin)]),
						medianImpreciseAtMaxNumberOfCounties = as.numeric(middleQuant[which.max(impreciseDf$numAdmin)]),
						upperImpreciseQuantAtMaxNumberOfCounties = as.numeric(upperQuant[which.max(impreciseDf$numAdmin)])
					)
				)

				# labels and titles
				letter <- if (count <= 26) { letters[count] } else { paste0(letters[count - 26], '\'') }
				main <- paste0(letter, ') ', thisNumErrorless, ' omniscient\n& ', thisNumPrecise, ' precise')

				boxPanel <- ggplot(impreciseDf, aes(y=resp)) +
					geom_point(
						data=preciseDf,
						mapping=aes(x=0, y=resp),
						position = position_jitter(w = 0.4, h = 0),
						color = precisePointColAlpha,
						size = pointSize
					) +
					geom_hline(yintercept = 1, size=0.4, linetype='solid', col='gray40') +
					geom_violin(
						data = preciseDf[preciseDf$resp >= quantile(preciseDf$resp, lower, na.rm=TRUE) & preciseDf$resp <= quantile(preciseDf$resp, upper, na.rm=TRUE), ],
						mapping = aes(x=0, y = resp),
						draw_quantiles = 0.5,
						fill = NA
					) +
					scale_x_continuous(breaks=0, labels=c('Prec')) +
					scale_y_log10(limits=c(ymin, ymax), labels=scales::label_number_auto()) +
					# scale_y_continuous(limits=c(ymin, ymax), trans='log10') +
					ggtitle(main) +
					theme(
						plot.title = element_text(hjust = 0, vjust=-1.5, size=panelSize),
						plot.background = element_blank(),
						panel.grid.major.x = element_blank(),
						panel.grid.minor.x = element_blank(),
						axis.title.x = element_blank(),
						axis.text.x = element_text(angle = 0, hjust=0.5, vjust=0),
						axis.title.y = element_blank(),
						axis.text.y = element_text(size=axisNumSize),
					)

				xbreaks <- pretty(c(1, xmax), 3)
				xbreaks[1] <- 1
					
				imprecisePanel <- ggplot(impreciseDf, aes(x=numAdmin, y=resp)) +
					geom_point(data=impreciseDf, color=imprecisePointCol, size=pointSize) +					
					geom_line(data=topBoundaryImprecise, mapping=aes(x=numAdmin, y=boundary), size=boundaryLineThickness) +
					geom_line(data=bottomBoundaryImprecise, mapping=aes(x=numAdmin, y=boundary), size=boundaryLineThickness) +
					geom_line(data=middleImprecise, mapping=aes(x=numAdmin, y=middle), col=impreciseLineCol, size=medianSize) +
					geom_hline(yintercept = 1, size=0.4, linetype='solid', col='gray40') +
					scale_x_continuous(breaks = xbreaks, limits=c(xmin, xmax)) +
					scale_y_log10(limits=c(ymin, ymax), labels=scales::label_number_auto()) +
					# scale_y_continuous(limits=c(ymin, ymax), trans='log10') +
					xlab('County records') +
					theme(
						plot.background = element_blank(),
						plot.title = element_blank(),
						axis.title.x = element_text(size=axisSizeX, vjust=1),
						axis.text.x = element_text(size=axisNumSize),
						axis.title.y = element_blank(),
						axis.text.y = element_blank(),
						axis.ticks.y = element_blank()
					)

				boxPanel <- boxPanel + theme(plot.margin = grid::unit(c(0, 0, 0, 0.1), 'cm'))
				imprecisePanel <- imprecisePanel + theme(plot.margin = grid::unit(c(0, 0.2, 0, 0), 'cm'))

				comboPanel <- plot_grid(boxPanel, imprecisePanel, ncol=2, rel_widths=c(0.55, 1), align='h')
				if (count %in% 1:3) comboPanel <- comboPanel + theme(plot.margin=grid::unit(c(0, 0, 0, 1), 'cm'))
				if (count == 3) comboPanel <- comboPanel +
					geom_text(aes(x=0, y=0, label=ylab, angle=90, size=axisSizeY, vjust=-1))

				top <- 0.5
				right <- 0.5
				bottom <- 0
				left <- 1

				if (count %in% c(1)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(top, 0, 0, right), 'in'))
				if (count %in% c(2:3)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(0, 0, 0, right), 'in'))
				if (count %in% c(4, 10, 16)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(top, 0, 0, 0), 'in'))
				if (count %in% c(22)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(top, right, 0, 0), 'in'))
				if (count %in% c(22:27)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(0, right, 0, 0), 'in'))
				if (count %in% c(27)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(0, right, bottom, 0), 'in'))
				if (count %in% c(9, 15, 21, 27)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(0, 0, bottom, 0), 'in'))

				panels[[count]] <- comboPanel
				names(panels)[count] <- paste0('errorless', thisNumErrorless, '_precise', thisNumPrecise)
				
			} # next number of precise
			
		} # next number of errorless

		main <- panels[[1]] + panels[[2]] + panels[[3]] + plot_spacer() + plot_spacer() + plot_spacer() + # errorless 20
			panels[[4]] + panels[[5]] + panels[[6]] + panels[[7]] + panels[[8]] + panels[[9]] + # errorless 40
			panels[[10]] + panels[[11]] + panels[[12]] + panels[[13]] + panels[[14]] + panels[[15]] + # errorless 80
			panels[[16]] + panels[[17]] + panels[[18]] + panels[[19]] + panels[[20]] + panels[[21]] + # errorless 160
			panels[[22]] + panels[[23]] + panels[[24]] + panels[[25]] + panels[[26]] + panels[[27]] + # errorless 320
			plot_layout(byrow=FALSE, nrow=6)

		main <- main +
			plot_annotation(
				caption = str_wrap(caption, width=105),
				theme=theme(
					plot.caption=element_text(
						size=11,
						hjust=0,
						vjust=5
					)
				)
			)

		ggsave(plot=main, filename=paste0('./Analysis/Virtual Species/', plotFileName, '.pdf'), width=8.5, height=11, units='in')
		write.csv(trends, paste0('./Analysis/Virtual Species/', trendsFileName, '.csv'), row.names=FALSE)
		
	### suitable area: gain
	#######################

		### generalization

		subject <- 'GAINED SUITABLE AREA'
		errorless <- 'gainArea_errorless_km2'
		precise <- 'gainArea_precise_km2'
		imprecise <- 'gainArea_imprecise_km2'
		
		# ylab <- expression(paste0(log[10]*'(Ratio of estimated-to-true EOO)'))
		ylab <- 'Ratio of estimated-to-true gain in suitable area'
		
		plotFileName <- '!Climate Change Exposure - Gained Suitable Area - All Results'
		trendsFileName <- '!Trends - Climate Change Exposure - Gained Suitable Area'

		caption <- gainCaption

		### y-axis limits
		these <- c(
			(results[ , imprecise] + 1) / (results[ , errorless] + 1),
			(results[ , precise] + 1) / (results[ , errorless] + 1)
		)
		these <- log10(these)
		ys <- quantile(these, c(1 - outlierQuant, outlierQuant), na.rm=TRUE)
		ymin <- as.numeric(ys[1])
		ymax <- as.numeric(ys[2])
		ymin <- 10^ymin
		ymax <- 10^ymax
		
		trends <- data.frame()
		panels <- list()
		count <- 0
		for (thisNumErrorless in numErrorlessSelected) {
		
			# subset number of precise so it's < total errorless
			allowableNumPreciseSelected <- numPreciseSelected[numPreciseSelected < thisNumErrorless]

			# x-axis limits
			these <- results[results$numErrorless == thisNumErrorless, ]
			xmax <- 1.015 * max(these$numAdmin)
			xmin <- 1 - 0.015 * xmax

			for (thisNumPrecise in allowableNumPreciseSelected) {

				say(subject, ' omniscient ', thisNumErrorless, ' precise ', thisNumPrecise)
				count <- count + 1

				# tall imprecise data
				thisResults <- results[results$numErrorless == thisNumErrorless & results$numPrecise == thisNumPrecise, ]
				resp <- (thisResults[ , imprecise] + 1) / (thisResults[ , errorless] + 1)
				n <- nrow(thisResults)
				impreciseDf <- data.frame(
					numAdmin = thisResults$numAdmin,
					resp = resp
				)
				
				# calculate quantiles for regression: imprecise
				impreciseDf <- impreciseDf[order(impreciseDf$numAdmin), ]
				impreciseDf <- impreciseDf[!is.infinite(impreciseDf$resp), ]
				k <- if (length(unique(impreciseDf$numAdmin)) <= 6) { 1 } else { -1 }
				middleQuant <- qgam(log10(resp) ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=0.5)
				lowerQuant <- qgam(log10(resp) ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=lower)
				upperQuant <- qgam(log10(resp) ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=upper)
				
				middleQuant <- 10^predict(middleQuant, impreciseDf, type='response')
				lowerQuant <- 10^predict(lowerQuant, impreciseDf, type='response')
				upperQuant <- 10^predict(upperQuant, impreciseDf, type='response')

				middleImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					middle = middleQuant
				)
				
				topBoundaryImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					boundary = upperQuant
				)
				
				bottomBoundaryImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					boundary = lowerQuant
				)

				# frames for violin plot of precise
				preciseDf <- data.frame(
					numAdmin = 0,
					resp = (thisResults[ , precise] + 1) / (thisResults[ , errorless] + 1)
				)
				preciseUpper <- quantile(preciseDf$resp, upper, na.rm=TRUE)
				preciseMiddle <- quantile(preciseDf$resp, 0.5, na.rm=TRUE)
				preciseLower <- quantile(preciseDf$resp, lower, na.rm=TRUE)

				# trends
				trends <- rbind(
					trends,
					data.frame(
						numErrorless = thisNumErrorless,
						numPrecise = thisNumPrecise,
						preciseLower = as.numeric(preciseLower),
						preciseMedian = as.numeric(preciseMiddle),
						preciseUpper = as.numeric(preciseUpper),
						propImpreciseGt1 = sum(resp > 1, na.rm=TRUE) / length(na.omit(resp)),
						max95thQuant = max(upperQuant),
						lowerImpreciseQuantAtMaxNumberOfCounties = as.numeric(lowerQuant[which.max(impreciseDf$numAdmin)]),
						medianImpreciseAtMaxNumberOfCounties = as.numeric(middleQuant[which.max(impreciseDf$numAdmin)]),
						upperImpreciseQuantAtMaxNumberOfCounties = as.numeric(upperQuant[which.max(impreciseDf$numAdmin)])
					)
				)

				# labels and titles
				letter <- if (count <= 26) { letters[count] } else { paste0(letters[count - 26], '\'') }
				main <- paste0(letter, ') ', thisNumErrorless, ' omniscient\n& ', thisNumPrecise, ' precise')

				boxPanel <- ggplot(impreciseDf, aes(y=resp)) +
					geom_point(
						data=preciseDf,
						mapping=aes(x=0, y=resp),
						position = position_jitter(w = 0.4, h = 0),
						color = precisePointColAlpha,
						size = pointSize
					) +
					geom_hline(yintercept = 1, size=0.4, linetype='solid', col='gray40') +
					geom_violin(
						data = preciseDf[preciseDf$resp >= quantile(preciseDf$resp, lower, na.rm=TRUE) & preciseDf$resp <= quantile(preciseDf$resp, upper, na.rm=TRUE), ],
						mapping = aes(x=0, y = resp),
						draw_quantiles = 0.5,
						fill = NA
					) +
					scale_x_continuous(breaks=0, labels=c('Prec')) +
					scale_y_log10(limits=c(ymin, ymax), labels=scales::label_number_auto()) +
					# scale_y_continuous(limits=c(ymin, ymax), trans='log10') +
					ggtitle(main) +
					theme(
						plot.title = element_text(hjust = 0, vjust=-1.5, size=panelSize),
						plot.background = element_blank(),
						panel.grid.major.x = element_blank(),
						panel.grid.minor.x = element_blank(),
						axis.title.x = element_blank(),
						axis.text.x = element_text(angle = 0, hjust=0.5, vjust=0),
						axis.title.y = element_blank(),
						axis.text.y = element_text(size=axisNumSize),
					)

				xbreaks <- pretty(c(1, xmax), 3)
				xbreaks[1] <- 1
					
				imprecisePanel <- ggplot(impreciseDf, aes(x=numAdmin, y=resp)) +
					geom_point(data=impreciseDf, color=imprecisePointCol, size=pointSize) +					
					geom_line(data=topBoundaryImprecise, mapping=aes(x=numAdmin, y=boundary), size=boundaryLineThickness) +
					geom_line(data=bottomBoundaryImprecise, mapping=aes(x=numAdmin, y=boundary), size=boundaryLineThickness) +
					geom_line(data=middleImprecise, mapping=aes(x=numAdmin, y=middle), col=impreciseLineCol, size=medianSize) +
					geom_hline(yintercept = 1, size=0.4, linetype='solid', col='gray40') +
					scale_x_continuous(breaks = xbreaks, limits=c(xmin, xmax)) +
					scale_y_log10(limits=c(ymin, ymax), labels=scales::label_number_auto()) +
					# scale_y_continuous(limits=c(ymin, ymax), trans='log10') +
					xlab('County records') +
					theme(
						plot.background = element_blank(),
						plot.title = element_blank(),
						axis.title.x = element_text(size=axisSizeX, vjust=1),
						axis.text.x = element_text(size=axisNumSize),
						axis.title.y = element_blank(),
						axis.text.y = element_blank(),
						axis.ticks.y = element_blank()
					)

				boxPanel <- boxPanel + theme(plot.margin = grid::unit(c(0, 0, 0, 0.1), 'cm'))
				imprecisePanel <- imprecisePanel + theme(plot.margin = grid::unit(c(0, 0.2, 0, 0), 'cm'))

				comboPanel <- plot_grid(boxPanel, imprecisePanel, ncol=2, rel_widths=c(0.55, 1), align='h')
				if (count %in% 1:3) comboPanel <- comboPanel + theme(plot.margin=grid::unit(c(0, 0, 0, 1), 'cm'))
				if (count == 3) comboPanel <- comboPanel +
					geom_text(aes(x=0, y=0, label=ylab, angle=90, size=axisSizeY, vjust=-1))

				top <- 0.5
				right <- 0.5
				bottom <- 0
				left <- 1

				if (count %in% c(1)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(top, 0, 0, right), 'in'))
				if (count %in% c(2:3)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(0, 0, 0, right), 'in'))
				if (count %in% c(4, 10, 16)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(top, 0, 0, 0), 'in'))
				if (count %in% c(22)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(top, right, 0, 0), 'in'))
				if (count %in% c(22:27)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(0, right, 0, 0), 'in'))
				if (count %in% c(27)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(0, right, bottom, 0), 'in'))
				if (count %in% c(9, 15, 21, 27)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(0, 0, bottom, 0), 'in'))

				panels[[count]] <- comboPanel
				names(panels)[count] <- paste0('errorless', thisNumErrorless, '_precise', thisNumPrecise)
				
			} # next number of precise
			
		} # next number of errorless

		main <- panels[[1]] + panels[[2]] + panels[[3]] + plot_spacer() + plot_spacer() + plot_spacer() + # errorless 20
			panels[[4]] + panels[[5]] + panels[[6]] + panels[[7]] + panels[[8]] + panels[[9]] + # errorless 40
			panels[[10]] + panels[[11]] + panels[[12]] + panels[[13]] + panels[[14]] + panels[[15]] + # errorless 80
			panels[[16]] + panels[[17]] + panels[[18]] + panels[[19]] + panels[[20]] + panels[[21]] + # errorless 160
			panels[[22]] + panels[[23]] + panels[[24]] + panels[[25]] + panels[[26]] + panels[[27]] + # errorless 320
			plot_layout(byrow=FALSE, nrow=6)

		main <- main +
			plot_annotation(
				caption = str_wrap(caption, width=105),
				theme=theme(
					plot.caption=element_text(
						size=11,
						hjust=0,
						vjust=5
					)
				)
			)

		ggsave(plot=main, filename=paste0('./Analysis/Virtual Species/', plotFileName, '.pdf'), width=8.5, height=11, units='in')
		write.csv(trends, paste0('./Analysis/Virtual Species/', trendsFileName, '.csv'), row.names=FALSE)

say('###########################################################')
say('### make composite plot of ALL ENM calibration accuracy ###')
say('###########################################################')

	# panels, one per scenario, with each column number of errorless, rows number of precise

	### generalization
	##################

		# text size
		panelSize <- 8 # panel title
		axisSizeX <- 8 # axes with labels
		axisSizeY <- 16 # axes with labels
		axisNumSize <- 8 # numbers on axes
		
		# lines
		medianSize <- 1
		boundaryLineThickness <- 0.4 # thickness of inner quantile boundary's lines
		
		# point size
		pointSize <- 0.25
		
		# colors
		pointCol <- '#d95f02'
		
		impreciseFill <- NA
		impreciseLineCol <- 'black'

		preciseFill <- '#1b9e77'
		precisePointColAlpha <- alpha('#1b9e77', 0.5)

		errorlessPointColAlpha <- alpha('gray50', 0.5)

		# quantiles to plot
		lower <- 0.05
		upper <- 0.95
	
	### preliminary
	###############

		load('./Analysis/Virtual Species/!!Collated Results.rda')
		numErrorlessSelected <- sort(unique(results$numErrorless))
		numPreciseSelected <- sort(unique(results$numPrecise))

	### ENM calibration: present
	############################

		### generalization

		subject <- 'ENM CALIBRATION PRESENT'
		errorless <- 'corTruthVsErrorlessSq'
		precise <- 'corTruthVsPreciseSq'
		imprecise <- 'corTruthVsImpreciseSq'
		
		ylab <- 'Correlation between predicted and true probability of presence'
		
		plotFileName <- '!ENM Calibration - Present - All Results'
		trendsFileName <- '!Trends - ENM Calibration Present'

		caption <- enmCalibPresentCaption

		### y-axis limits
		these <- c(
			results[ , 'corTruthVsErrorlessSq'], results[ , 'corTruthVsPreciseSq'], results[ , 'corTruthVsImpreciseSq'],
			results[ , 'corTruthVsErrorlessFut'], results[ , 'corTruthVsPreciseFut'], results[ , 'corTruthVsImpreciseFut']
		)
		ymin <- min(0, these, na.rm=TRUE)
		ymin <- roundTo(ymin, 0.2, floor)
		ymax <- 1
		ybreaks <- pretty(c(ymin, ymax))
		
		trends <- data.frame()
		panels <- list()
		count <- 0
		for (thisNumErrorless in numErrorlessSelected) {
		
			# subset number of precise so it's < total errorless
			allowableNumPreciseSelected <- numPreciseSelected[numPreciseSelected < thisNumErrorless]

			# x-axis limits
			these <- results[results$numErrorless == thisNumErrorless, ]
			xmax <- 1.015 * max(these$numAdmin)
			xmin <- 1 - 0.015 * xmax

			for (thisNumPrecise in allowableNumPreciseSelected) {

				say(subject, ' omniscient ', thisNumErrorless, ' precise ', thisNumPrecise)
				count <- count + 1

				# tall imprecise data
				thisResults <- results[results$numErrorless == thisNumErrorless & results$numPrecise == thisNumPrecise, ]
				resp <- thisResults[ , imprecise]
				n <- nrow(thisResults)
				impreciseDf <- data.frame(
					numAdmin = thisResults$numAdmin,
					resp = resp
				)
				
				# calculate quantiles for regression: imprecise
				impreciseDf <- impreciseDf[order(impreciseDf$numAdmin), ]
				k <- if (length(unique(impreciseDf$numAdmin)) <= 6) { 1 } else { -1 }
				middleQuant <- qgam(resp ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=0.5)
				lowerQuant <- qgam(resp ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=lower)
				upperQuant <- qgam(resp ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=upper)

				middleQuant <- predict(middleQuant, impreciseDf, type='response')
				lowerQuant <- predict(lowerQuant, impreciseDf, type='response')
				upperQuant <- predict(upperQuant, impreciseDf, type='response')

				middleImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					middle = middleQuant
				)
				
				topBoundaryImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					boundary = upperQuant
				)
				
				bottomBoundaryImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					boundary = lowerQuant
				)

				# frames for violin plot of precise
				n <- nrow(thisResults)
				errorlessDf <- data.frame(
					x = rep(-0.5, n),
					resp = thisResults[ , errorless]
				)
				preciseDf <- data.frame(
					x = rep(0.5, n),
					resp = thisResults[ , precise]
				)
				errorlessPreciseDf <- rbind(errorlessDf, preciseDf)
				
				errorlessUpper <- quantile(errorlessDf$resp, upper, na.rm=TRUE)
				errorlessMiddle <- quantile(errorlessDf$resp, 0.5, na.rm=TRUE)
				errorlessLower <- quantile(errorlessDf$resp, lower, na.rm=TRUE)

				preciseUpper <- quantile(preciseDf$resp, upper, na.rm=TRUE)
				preciseMiddle <- quantile(preciseDf$resp, 0.5, na.rm=TRUE)
				preciseLower <- quantile(preciseDf$resp, lower, na.rm=TRUE)

				# trends
				trends <- rbind(
					trends,
					data.frame(
						numErrorless = thisNumErrorless,
						numPrecise = thisNumPrecise,
						errorlessLower = as.numeric(errorlessLower),
						errorlessMedian = as.numeric(errorlessMiddle),
						errorlessUpper = as.numeric(errorlessUpper),
						preciseLower = as.numeric(preciseLower),
						preciseMedian = as.numeric(preciseMiddle),
						preciseUpper = as.numeric(preciseUpper),
						lowerImpreciseQuantAtMaxNumberOfCounties = as.numeric(lowerQuant[which.max(impreciseDf$numAdmin)]),
						medianImpreciseAtMaxNumberOfCounties = as.numeric(middleQuant[which.max(impreciseDf$numAdmin)]),
						upperImpreciseQuantAtMaxNumberOfCounties = as.numeric(upperQuant[which.max(impreciseDf$numAdmin)])
					)
				)

				# labels and titles
				letter <- if (count <= 26) { letters[count] } else { paste0(letters[count - 26], '\'') }
				main <- paste0(letter, ') ', thisNumErrorless, ' omniscient\n& ', thisNumPrecise, ' precise')

				boxPanel <- ggplot(errorlessPreciseDf, aes(y=resp)) +
					geom_point(
						data=errorlessDf,
						mapping=aes(x=x, y=resp),
						position = position_jitter(w = 0.4, h = 0),
						color = errorlessPointColAlpha,
						size = pointSize
					) +
					geom_point(
						data=preciseDf,
						mapping=aes(x=x, y=resp),
						position = position_jitter(w = 0.4, h = 0),
						color = precisePointColAlpha,
						size = pointSize
					) +
					geom_violin(
						data = preciseDf[preciseDf$resp >= preciseLower & preciseDf$resp <= preciseUpper, ],
						mapping = aes(x=x, y = resp),
						draw_quantiles = 0.5,
						fill = NA
					) +
					geom_violin(
						data = errorlessDf[errorlessDf$resp >= preciseLower & errorlessDf$resp <= preciseUpper, ],
						mapping = aes(x=x, y = resp),
						draw_quantiles = 0.5,
						fill = NA
					) +
					scale_x_continuous(breaks=c(-0.5, 0.5), labels=c('Omni', 'Prec')) +
					scale_y_continuous(breaks=ybreaks) +
					coord_cartesian(xlim=c(-1, 1), ylim=c(ymin, ymax), expand=FALSE) +
					ggtitle(main) +
					theme(
						plot.title = element_text(hjust = 0, size=panelSize),
						plot.background = element_blank(),
						panel.grid.major.x = element_blank(),
						panel.grid.minor.x = element_blank(),
						axis.title.x = element_blank(),
						axis.text.x = element_text(angle = 90, hjust=1, vjust=0, size=axisNumSize),
						axis.title.y = element_blank(),
						axis.text.y = element_text(size=axisNumSize),
						legend.position='none'
					)

				xbreaks <- pretty(c(1, xmax), 3)
				xbreaks[1] <- 1
					
				imprecisePanel <- ggplot(impreciseDf, aes(x=numAdmin, y=resp)) +
					geom_point(data=impreciseDf, color=pointCol, size=pointSize) +					
					geom_line(data=topBoundaryImprecise, mapping=aes(x=numAdmin, y=boundary), size=boundaryLineThickness) +
					geom_line(data=bottomBoundaryImprecise, mapping=aes(x=numAdmin, y=boundary), size=boundaryLineThickness) +
					geom_line(data=middleImprecise, mapping=aes(x=numAdmin, y=middle), col=impreciseLineCol, size=medianSize) +
					coord_cartesian(xlim=c(xmin, xmax), ylim=c(ymin, ymax), expand=FALSE) +
					xlab('County records') +
					scale_x_continuous(breaks = xbreaks) +
					theme(
						plot.background = element_blank(),
						plot.title = element_blank(),
						axis.title.x = element_text(size=axisSizeX, vjust=7),
						axis.text.x = element_text(size=axisNumSize),
						axis.title.y = element_blank(),
						axis.text.y = element_blank(),
						axis.ticks.y = element_blank()
					)

				boxPanel <- boxPanel + theme(plot.margin = grid::unit(c(0, 0, 0, 0.1), 'cm'))
				imprecisePanel <- imprecisePanel + theme(plot.margin = grid::unit(c(0, 0.2, 0, 0), 'cm'))

				comboPanel <- plot_grid(boxPanel, imprecisePanel, ncol=2, rel_widths=c(0.65, 1), align='h')
				if (count %in% 1:3) comboPanel <- comboPanel + theme(plot.margin=grid::unit(c(0, 0, 0, 1), 'cm'))
				if (count == 3) comboPanel <- comboPanel +
					geom_text(aes(x=0, y=0, label=ylab, angle=90, size=axisSizeY, vjust=-1))

				top <- 0.5
				right <- 0.5
				bottom <- 0
				left <- 1

				if (count %in% c(1)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(top, 0, 0, right), 'in'))
				if (count %in% c(2:3)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(0, 0, 0, right), 'in'))
				if (count %in% c(4, 10, 16)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(top, 0, 0, 0), 'in'))
				if (count %in% c(22)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(top, right, 0, 0), 'in'))
				if (count %in% c(22:27)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(0, right, 0, 0), 'in'))
				if (count %in% c(27)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(0, right, bottom, 0), 'in'))
				if (count %in% c(9, 15, 21, 27)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(0, 0, bottom, 0), 'in'))

				panels[[count]] <- comboPanel
				names(panels)[count] <- paste0('errorless', thisNumErrorless, '_precise', thisNumPrecise)
				
			} # next number of precise
			
		} # next number of errorless

		main <- panels[[1]] + panels[[2]] + panels[[3]] + plot_spacer() + plot_spacer() + plot_spacer() + # errorless 20
			panels[[4]] + panels[[5]] + panels[[6]] + panels[[7]] + panels[[8]] + panels[[9]] + # errorless 40
			panels[[10]] + panels[[11]] + panels[[12]] + panels[[13]] + panels[[14]] + panels[[15]] + # errorless 80
			panels[[16]] + panels[[17]] + panels[[18]] + panels[[19]] + panels[[20]] + panels[[21]] + # errorless 160
			panels[[22]] + panels[[23]] + panels[[24]] + panels[[25]] + panels[[26]] + panels[[27]] + # errorless 320
			plot_layout(byrow=FALSE, nrow=6)

		main <- main +
			plot_annotation(
				caption = str_wrap(caption, width=105),
				theme=theme(
					plot.caption=element_text(
						size=11,
						hjust=0,
						vjust=5
					)
				)
			)

		ggsave(plot=main, filename=paste0('./Analysis/Virtual Species/', plotFileName, '.pdf'), width=8.5, height=11, units='in')
		write.csv(trends, paste0('./Analysis/Virtual Species/', trendsFileName, '.csv'), row.names=FALSE)
		
	### ENM calibration: future
	###########################

		### generalization

		subject <- 'ENM CALIBRATION FUTURE'
		errorless <- 'corTruthVsErrorlessFut'
		precise <- 'corTruthVsPreciseFut'
		imprecise <- 'corTruthVsImpreciseFut'
		
		ylab <- 'Correlation between predicted and true probability of presence'
		
		plotFileName <- '!ENM Calibration - Future - All Results'
		trendsFileName <- '!Trends - ENM Calibration Future'

		caption <- enmCalibFutureCaption

		### y-axis limits
		these <- c(
			results[ , 'corTruthVsErrorlessSq'], results[ , 'corTruthVsPreciseSq'], results[ , 'corTruthVsImpreciseSq'],
			results[ , 'corTruthVsErrorlessFut'], results[ , 'corTruthVsPreciseFut'], results[ , 'corTruthVsImpreciseFut']
		)
		ymin <- min(0, these, na.rm=TRUE)
		ymin <- roundTo(ymin, 0.2, floor)
		ymax <- 1
		ybreaks <- pretty(c(ymin, ymax))
		
		trends <- data.frame()
		panels <- list()
		count <- 0
		for (thisNumErrorless in numErrorlessSelected) {
		
			# subset number of precise so it's < total errorless
			allowableNumPreciseSelected <- numPreciseSelected[numPreciseSelected < thisNumErrorless]

			# x-axis limits
			these <- results[results$numErrorless == thisNumErrorless, ]
			xmax <- 1.015 * max(these$numAdmin)
			xmin <- 1 - 0.015 * xmax

			for (thisNumPrecise in allowableNumPreciseSelected) {

				say(subject, ' omniscient ', thisNumErrorless, ' precise ', thisNumPrecise)
				count <- count + 1

				# tall imprecise data
				thisResults <- results[results$numErrorless == thisNumErrorless & results$numPrecise == thisNumPrecise, ]
				resp <- thisResults[ , imprecise]
				n <- nrow(thisResults)
				impreciseDf <- data.frame(
					numAdmin = thisResults$numAdmin,
					resp = resp
				)
				
				# calculate quantiles for regression: imprecise
				impreciseDf <- impreciseDf[order(impreciseDf$numAdmin), ]
				k <- if (length(unique(impreciseDf$numAdmin)) <= 6) { 1 } else { -1 }
				middleQuant <- qgam(resp ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=0.5)
				lowerQuant <- qgam(resp ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=lower)
				upperQuant <- qgam(resp ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=upper)

				middleQuant <- predict(middleQuant, impreciseDf, type='response')
				lowerQuant <- predict(lowerQuant, impreciseDf, type='response')
				upperQuant <- predict(upperQuant, impreciseDf, type='response')

				middleImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					middle = middleQuant
				)
				
				topBoundaryImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					boundary = upperQuant
				)
				
				bottomBoundaryImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					boundary = lowerQuant
				)

				# frames for violin plot of precise
				n <- nrow(thisResults)
				errorlessDf <- data.frame(
					x = rep(-0.5, n),
					resp = thisResults[ , errorless]
				)
				preciseDf <- data.frame(
					x = rep(0.5, n),
					resp = thisResults[ , precise]
				)
				errorlessPreciseDf <- rbind(errorlessDf, preciseDf)
				
				errorlessUpper <- quantile(errorlessDf$resp, upper, na.rm=TRUE)
				errorlessMiddle <- quantile(errorlessDf$resp, 0.5, na.rm=TRUE)
				errorlessLower <- quantile(errorlessDf$resp, lower, na.rm=TRUE)

				preciseUpper <- quantile(preciseDf$resp, upper, na.rm=TRUE)
				preciseMiddle <- quantile(preciseDf$resp, 0.5, na.rm=TRUE)
				preciseLower <- quantile(preciseDf$resp, lower, na.rm=TRUE)

				# trends
				trends <- rbind(
					trends,
					data.frame(
						numErrorless = thisNumErrorless,
						numPrecise = thisNumPrecise,
						errorlessLower = as.numeric(errorlessLower),
						errorlessMedian = as.numeric(errorlessMiddle),
						errorlessUpper = as.numeric(errorlessUpper),
						preciseLower = as.numeric(preciseLower),
						preciseMedian = as.numeric(preciseMiddle),
						preciseUpper = as.numeric(preciseUpper),
						lowerImpreciseQuantAtMaxNumberOfCounties = as.numeric(lowerQuant[which.max(impreciseDf$numAdmin)]),
						medianImpreciseAtMaxNumberOfCounties = as.numeric(middleQuant[which.max(impreciseDf$numAdmin)]),
						upperImpreciseQuantAtMaxNumberOfCounties = as.numeric(upperQuant[which.max(impreciseDf$numAdmin)])
					)
				)

				# labels and titles
				letter <- if (count <= 26) { letters[count] } else { paste0(letters[count - 26], '\'') }
				main <- paste0(letter, ') ', thisNumErrorless, ' omniscient\n& ', thisNumPrecise, ' precise')

				boxPanel <- ggplot(errorlessPreciseDf, aes(y=resp)) +
					geom_point(
						data=errorlessDf,
						mapping=aes(x=x, y=resp),
						position = position_jitter(w = 0.4, h = 0),
						color = errorlessPointColAlpha,
						size = pointSize
					) +
					geom_point(
						data=preciseDf,
						mapping=aes(x=x, y=resp),
						position = position_jitter(w = 0.4, h = 0),
						color = precisePointColAlpha,
						size = pointSize
					) +
					geom_violin(
						data = preciseDf[preciseDf$resp >= preciseLower & preciseDf$resp <= preciseUpper, ],
						mapping = aes(x=x, y = resp),
						draw_quantiles = 0.5,
						fill = NA
					) +
					geom_violin(
						data = errorlessDf[errorlessDf$resp >= preciseLower & errorlessDf$resp <= preciseUpper, ],
						mapping = aes(x=x, y = resp),
						draw_quantiles = 0.5,
						fill = NA
					) +
					scale_x_continuous(breaks=c(-0.5, 0.5), labels=c('Omni', 'Prec')) +
					scale_y_continuous(breaks=ybreaks) +
					coord_cartesian(xlim=c(-1, 1), ylim=c(ymin, ymax), expand=FALSE) +
					ggtitle(main) +
					theme(
						plot.title = element_text(hjust = 0, size=panelSize),
						plot.background = element_blank(),
						panel.grid.major.x = element_blank(),
						panel.grid.minor.x = element_blank(),
						axis.title.x = element_blank(),
						axis.text.x = element_text(angle = 90, hjust=1, vjust=0, size=axisNumSize),
						axis.title.y = element_blank(),
						axis.text.y = element_text(size=axisNumSize),
						legend.position='none'
					)

				xbreaks <- pretty(c(1, xmax), 3)
				xbreaks[1] <- 1
					
				imprecisePanel <- ggplot(impreciseDf, aes(x=numAdmin, y=resp)) +
					geom_point(data=impreciseDf, color=pointCol, size=pointSize) +					
					geom_line(data=topBoundaryImprecise, mapping=aes(x=numAdmin, y=boundary), size=boundaryLineThickness) +
					geom_line(data=bottomBoundaryImprecise, mapping=aes(x=numAdmin, y=boundary), size=boundaryLineThickness) +
					geom_line(data=middleImprecise, mapping=aes(x=numAdmin, y=middle), col=impreciseLineCol, size=medianSize) +
					coord_cartesian(xlim=c(xmin, xmax), ylim=c(ymin, ymax), expand=FALSE) +
					xlab('County records') +
					scale_x_continuous(breaks = xbreaks) +
					theme(
						plot.background = element_blank(),
						plot.title = element_blank(),
						axis.title.x = element_text(size=axisSizeX, vjust=7),
						axis.text.x = element_text(size=axisNumSize),
						axis.title.y = element_blank(),
						axis.text.y = element_blank(),
						axis.ticks.y = element_blank()
					)

				boxPanel <- boxPanel + theme(plot.margin = grid::unit(c(0, 0, 0, 0.1), 'cm'))
				imprecisePanel <- imprecisePanel + theme(plot.margin = grid::unit(c(0, 0.2, 0, 0), 'cm'))

				comboPanel <- plot_grid(boxPanel, imprecisePanel, ncol=2, rel_widths=c(0.65, 1), align='h')
				if (count %in% 1:3) comboPanel <- comboPanel + theme(plot.margin=grid::unit(c(0, 0, 0, 1), 'cm'))
				if (count == 3) comboPanel <- comboPanel +
					geom_text(aes(x=0, y=0, label=ylab, angle=90, size=axisSizeY, vjust=-1))

				top <- 0.5
				right <- 0.5
				bottom <- 0
				left <- 1

				if (count %in% c(1)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(top, 0, 0, right), 'in'))
				if (count %in% c(2:3)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(0, 0, 0, right), 'in'))
				if (count %in% c(4, 10, 16)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(top, 0, 0, 0), 'in'))
				if (count %in% c(22)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(top, right, 0, 0), 'in'))
				if (count %in% c(22:27)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(0, right, 0, 0), 'in'))
				if (count %in% c(27)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(0, right, bottom, 0), 'in'))
				if (count %in% c(9, 15, 21, 27)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(0, 0, bottom, 0), 'in'))

				panels[[count]] <- comboPanel
				names(panels)[count] <- paste0('errorless', thisNumErrorless, '_precise', thisNumPrecise)
				
			} # next number of precise
			
		} # next number of errorless

		main <- panels[[1]] + panels[[2]] + panels[[3]] + plot_spacer() + plot_spacer() + plot_spacer() + # errorless 20
			panels[[4]] + panels[[5]] + panels[[6]] + panels[[7]] + panels[[8]] + panels[[9]] + # errorless 40
			panels[[10]] + panels[[11]] + panels[[12]] + panels[[13]] + panels[[14]] + panels[[15]] + # errorless 80
			panels[[16]] + panels[[17]] + panels[[18]] + panels[[19]] + panels[[20]] + panels[[21]] + # errorless 160
			panels[[22]] + panels[[23]] + panels[[24]] + panels[[25]] + panels[[26]] + panels[[27]] + # errorless 320
			plot_layout(byrow=FALSE, nrow=6)

		main <- main +
			plot_annotation(
				caption = str_wrap(caption, width=105),
				theme=theme(
					plot.caption=element_text(
						size=11,
						hjust=0,
						vjust=5
					)
				)
			)

		ggsave(plot=main, filename=paste0('./Analysis/Virtual Species/', plotFileName, '.pdf'), width=8.5, height=11, units='in')
		write.csv(trends, paste0('./Analysis/Virtual Species/', trendsFileName, '.csv'), row.names=FALSE)

say('################################################################')
say('### make composite plot of ALL EOO and NICHE BREADTH results ###')
say('################################################################')

	# panels, one per scenario, with each column number of errorless, rows number of precise

	### generalization
	##################

		# text size
		panelSize <- 8 # panel title
		axisSizeX <- 8 # axes with labels
		axisSizeY <- 16 # axes with labels
		axisNumSize <- 8 # numbers on axes
		
		# lines
		medianSize <- 1
		hlineSize <- 0.6 # thickness of horizontal line at 1
		boundaryLineThickness <- 0.4 # thickness of inner quantile boundary's lines
		
		# point size
		pointSize <- 0.25
		
		# colors
		pointCol <- '#d95f02'
		
		impreciseFill <- NA
		impreciseLineCol <- 'black'

		preciseFill <- '#1b9e77'
		precisePointColAlpha <- alpha('#1b9e77', 0.5)

		# quantiles to plot
		lower <- 0.05
		upper <- 0.95
	
	### preliminary
	###############

		load('./Analysis/Virtual Species/!!Collated Results.rda')
		numErrorlessSelected <- sort(unique(results$numErrorless))
		numPreciseSelected <- sort(unique(results$numPrecise))

	### EOO
	#######

		### generalization

		subject <- 'EOO'
		errorless <- 'eooErrorless_km2'
		precise <- 'eooPrecise_km2'
		imprecise <- 'eooImprecise_km2'
		
		# ylab <- expression(paste0(log[10]*'(Ratio of estimated-to-true EOO)'))
		ylab <- 'Ratio of estimated-to-true EOO'
		
		plotFileName <- '!EOO - All Results'
		trendsFileName <- '!Trends - EOO'

		caption <- eooCaption

		### y-axis limits
		these <- c(
			results[ , imprecise] / results[ , errorless],
			results[ , precise] / results[ , errorless]
		)
		ymin <- 0
		# ymax <- quantile(these, outlierQuant), na.rm=TRUE)
		ymax <- max(these)
		
		trends <- data.frame()
		panels <- list()
		count <- 0
		for (thisNumErrorless in numErrorlessSelected) {
		
			# subset number of precise so it's < total errorless
			allowableNumPreciseSelected <- numPreciseSelected[numPreciseSelected < thisNumErrorless]

			# x-axis limits
			these <- results[results$numErrorless == thisNumErrorless, ]
			xmax <- 1.015 * max(these$numAdmin)
			xmin <- 1 - 0.015 * xmax

			for (thisNumPrecise in allowableNumPreciseSelected) {

				say(subject, ' omniscient ', thisNumErrorless, ' precise ', thisNumPrecise)
				count <- count + 1

				# tall imprecise data
				thisResults <- results[results$numErrorless == thisNumErrorless & results$numPrecise == thisNumPrecise, ]
				resp <- thisResults[ , imprecise] / thisResults[ , errorless]
				n <- nrow(thisResults)
				impreciseDf <- data.frame(
					numAdmin = thisResults$numAdmin,
					resp = resp
				)
				
				# calculate quantiles for regression: imprecise
				impreciseDf <- impreciseDf[order(impreciseDf$numAdmin), ]
				impreciseDf <- impreciseDf[!is.infinite(impreciseDf$resp), ]
				k <- if (length(unique(impreciseDf$numAdmin)) <= 6) { 1 } else { -1 }
				middleQuant <- qgam(log(resp) ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=0.5)
				lowerQuant <- qgam(log(resp) ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=lower)
				upperQuant <- qgam(log(resp) ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=upper)
				
				middleQuant <- exp(predict(middleQuant, impreciseDf, type='response'))
				lowerQuant <- exp(predict(lowerQuant, impreciseDf, type='response'))
				upperQuant <- exp(predict(upperQuant, impreciseDf, type='response'))

				middleImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					middle = middleQuant
				)
				
				topBoundaryImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					boundary = upperQuant
				)
				
				bottomBoundaryImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					boundary = lowerQuant
				)

				# frames for violin plot of precise
				preciseDf <- data.frame(
					numAdmin = 0,
					resp = thisResults[ , precise] / thisResults[ , errorless]
				)
				preciseUpper <- quantile(preciseDf$resp, upper, na.rm=TRUE)
				preciseMiddle <- quantile(preciseDf$resp, 0.5, na.rm=TRUE)
				preciseLower <- quantile(preciseDf$resp, lower, na.rm=TRUE)

				# trends
				trends <- rbind(
					trends,
					data.frame(
						numErrorless = thisNumErrorless,
						numPrecise = thisNumPrecise,
						preciseLower = as.numeric(preciseLower),
						preciseMedian = as.numeric(preciseMiddle),
						preciseUpper = as.numeric(preciseUpper),
						propImpreciseGt1 = sum(resp > 1, na.rm=TRUE) / length(na.omit(resp)),
						lowerImpreciseQuantAtMaxNumberOfCounties = as.numeric(lowerQuant[which.max(impreciseDf$numAdmin)]),
						medianImpreciseAtMaxNumberOfCounties = as.numeric(middleQuant[which.max(impreciseDf$numAdmin)]),
						upperImpreciseQuantAtMaxNumberOfCounties = as.numeric(upperQuant[which.max(impreciseDf$numAdmin)])
					)
				)

				# labels and titles
				letter <- if (count <= 26) { letters[count] } else { paste0(letters[count - 26], '\'') }
				main <- paste0(letter, ') ', thisNumErrorless, ' omniscient\n& ', thisNumPrecise, ' precise')

				boxPanel <- ggplot(impreciseDf, aes(y=resp)) +
					geom_point(
						data=preciseDf,
						mapping=aes(x=0, y=resp),
						position = position_jitter(w = 0.4, h = 0),
						color = precisePointColAlpha,
						size = pointSize
					) +
					geom_hline(yintercept = 1, size=0.4, linetype='solid', col='gray40') +
					geom_violin(
						data = preciseDf[preciseDf$resp >= quantile(preciseDf$resp, lower, na.rm=TRUE) & preciseDf$resp <= quantile(preciseDf$resp, upper, na.rm=TRUE), ],
						mapping = aes(x=0, y = resp),
						draw_quantiles = 0.5,
						fill = NA
					) +
					scale_x_continuous(breaks=0, labels=c('Pre-\ncise')) +
					scale_y_continuous(limits=c(ymin, ymax)) +
					# scale_y_log10(
						# limits=c(ymin, ymax),
						# breaks=trans_breaks('log10', function(x) 10^x),
						# labels=trans_format('log10', math_format(10^.x))
					# ) +
					ggtitle(main) +
					theme(
						plot.title = element_text(hjust = 0, vjust=-1.5, size=panelSize),
						plot.background = element_blank(),
						panel.grid.major.x = element_blank(),
						panel.grid.minor.x = element_blank(),
						axis.title.x = element_blank(),
						axis.text.x = element_text(angle = 0, hjust=0.5, vjust=0),
						axis.title.y = element_blank(),
						axis.text.y = element_text(size=axisNumSize),
					)

				xbreaks <- pretty(c(1, xmax), 3)
				xbreaks[1] <- 1
					
				imprecisePanel <- ggplot(impreciseDf, aes(x=numAdmin, y=resp)) +
					geom_point(data=impreciseDf, color=pointCol, size=pointSize) +					
					geom_line(data=topBoundaryImprecise, mapping=aes(x=numAdmin, y=boundary), size=boundaryLineThickness) +
					geom_line(data=bottomBoundaryImprecise, mapping=aes(x=numAdmin, y=boundary), size=boundaryLineThickness) +
					geom_line(data=middleImprecise, mapping=aes(x=numAdmin, y=middle), col=impreciseLineCol, size=medianSize) +
					geom_hline(yintercept = 1, size=0.4, linetype='solid', col='gray40') +
					scale_x_continuous(breaks = xbreaks, limits=c(xmin, xmax)) +
					scale_y_continuous(limits=c(ymin, ymax)) +
					# scale_y_log10(
						# limits=c(ymin, ymax),
						# breaks=trans_breaks('log10', function(x) 10^x),
						# labels=trans_format('log10', math_format(10^.x))
					# ) +
					xlab('County records') +
					theme(
						plot.background = element_blank(),
						plot.title = element_blank(),
						axis.title.x = element_text(size=axisSizeX, vjust=8),
						axis.text.x = element_text(size=axisNumSize),
						axis.title.y = element_blank(),
						axis.text.y = element_blank(),
						axis.ticks.y = element_blank()
					)

				boxPanel <- boxPanel + theme(plot.margin = grid::unit(c(0, 0, 0, 0.1), 'cm'))
				imprecisePanel <- imprecisePanel + theme(plot.margin = grid::unit(c(0, 0.2, 0, 0), 'cm'))

				comboPanel <- plot_grid(boxPanel, imprecisePanel, ncol=2, rel_widths=c(0.55, 1), align='h')
				if (count %in% 1:3) comboPanel <- comboPanel + theme(plot.margin=grid::unit(c(0, 0, 0, 1), 'cm'))
				if (count == 3) comboPanel <- comboPanel +
					geom_text(aes(x=0, y=0, label=ylab, angle=90, size=axisSizeY, vjust=-1))

				top <- 0.5
				right <- 0.5
				bottom <- 0
				left <- 1

				if (count %in% c(1)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(top, 0, 0, right), 'in'))
				if (count %in% c(2:3)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(0, 0, 0, right), 'in'))
				if (count %in% c(4, 10, 16)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(top, 0, 0, 0), 'in'))
				if (count %in% c(22)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(top, right, 0, 0), 'in'))
				if (count %in% c(22:27)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(0, right, 0, 0), 'in'))
				if (count %in% c(27)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(0, right, bottom, 0), 'in'))
				if (count %in% c(9, 15, 21, 27)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(0, 0, bottom, 0), 'in'))

				panels[[count]] <- comboPanel
				names(panels)[count] <- paste0('errorless', thisNumErrorless, '_precise', thisNumPrecise)
				
			} # next number of precise
			
		} # next number of errorless

		main <- panels[[1]] + panels[[2]] + panels[[3]] + plot_spacer() + plot_spacer() + plot_spacer() + # errorless 20
			panels[[4]] + panels[[5]] + panels[[6]] + panels[[7]] + panels[[8]] + panels[[9]] + # errorless 40
			panels[[10]] + panels[[11]] + panels[[12]] + panels[[13]] + panels[[14]] + panels[[15]] + # errorless 80
			panels[[16]] + panels[[17]] + panels[[18]] + panels[[19]] + panels[[20]] + panels[[21]] + # errorless 160
			panels[[22]] + panels[[23]] + panels[[24]] + panels[[25]] + panels[[26]] + panels[[27]] + # errorless 320
			plot_layout(byrow=FALSE, nrow=6)

		main <- main +
			plot_annotation(
				caption = str_wrap(caption, width=105),
				theme=theme(
					plot.caption=element_text(
						size=11,
						hjust=0,
						vjust=5
					)
				)
			)

		ggsave(plot=main, filename=paste0('./Analysis/Virtual Species/', plotFileName, '.pdf'), width=8.5, height=11, units='in')
		write.csv(trends, paste0('./Analysis/Virtual Species/', trendsFileName, '.csv'), row.names=FALSE)
		
	### univariate niche breadth: MAT
	#################################

		### generalization

		subject <- 'MAT'
		errorless <- 'errorlessNicheBreadthMat_range_degC'
		precise <- 'preciseNicheBreadthMat_range_degC'
		imprecise <- 'impreciseNicheBreadthMat_range_degC'
		
		# ylab <- expression(paste0(log[10]*'(Ratio of estimated-to-true EOO)'))
		ylab <- 'Ratio of estimated-to-true realized niche breadth'
		
		plotFileName <- '!Univariate Niche Breadth MAT - All Results'
		trendsFileName <- '!Trends - Univariate Niche Breadth MAT'

		caption <- matCaption

		### y-axis limits
		these <- c(
			results[ , imprecise] / results[ , errorless],
			results[ , precise] / results[ , errorless]
		)
		ymin <- 0
		ymax <- quantile(these, outlierQuant)
		
		trends <- data.frame()
		panels <- list()
		count <- 0
		for (thisNumErrorless in numErrorlessSelected) {
		
			# subset number of precise so it's < total errorless
			allowableNumPreciseSelected <- numPreciseSelected[numPreciseSelected < thisNumErrorless]

			# x-axis limits
			these <- results[results$numErrorless == thisNumErrorless, ]
			xmax <- 1.015 * max(these$numAdmin)
			xmin <- 1 - 0.015 * xmax

			for (thisNumPrecise in allowableNumPreciseSelected) {

				say(subject, ' omniscient ', thisNumErrorless, ' precise ', thisNumPrecise)
				count <- count + 1

				# tall imprecise data
				thisResults <- results[results$numErrorless == thisNumErrorless & results$numPrecise == thisNumPrecise, ]
				resp <- thisResults[ , imprecise] / thisResults[ , errorless]
				n <- nrow(thisResults)
				impreciseDf <- data.frame(
					numAdmin = thisResults$numAdmin,
					resp = resp
				)
				
				# calculate quantiles for regression: imprecise
				impreciseDf <- impreciseDf[order(impreciseDf$numAdmin), ]
				impreciseDf <- impreciseDf[!is.infinite(impreciseDf$resp), ]
				k <- if (length(unique(impreciseDf$numAdmin)) <= 6) { 1 } else { -1 }
				middleQuant <- qgam(log(resp) ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=0.5)
				lowerQuant <- qgam(log(resp) ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=lower)
				upperQuant <- qgam(log(resp) ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=upper)
				
				middleQuant <- exp(predict(middleQuant, impreciseDf, type='response'))
				lowerQuant <- exp(predict(lowerQuant, impreciseDf, type='response'))
				upperQuant <- exp(predict(upperQuant, impreciseDf, type='response'))

				middleImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					middle = middleQuant
				)
				
				topBoundaryImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					boundary = upperQuant
				)
				
				bottomBoundaryImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					boundary = lowerQuant
				)

				# frames for violin plot of precise
				preciseDf <- data.frame(
					numAdmin = 0,
					resp = thisResults[ , precise] / thisResults[ , errorless]
				)
				preciseUpper <- quantile(preciseDf$resp, upper, na.rm=TRUE)
				preciseMiddle <- quantile(preciseDf$resp, 0.5, na.rm=TRUE)
				preciseLower <- quantile(preciseDf$resp, lower, na.rm=TRUE)

				# trends
				trends <- rbind(
					trends,
					data.frame(
						numErrorless = thisNumErrorless,
						numPrecise = thisNumPrecise,
						preciseLower = as.numeric(preciseLower),
						preciseMedian = as.numeric(preciseMiddle),
						preciseUpper = as.numeric(preciseUpper),
						propImpreciseGt1 = sum(resp > 1, na.rm=TRUE) / length(na.omit(resp)),
						lowerImpreciseQuantAtMaxNumberOfCounties = as.numeric(lowerQuant[which.max(impreciseDf$numAdmin)]),
						medianImpreciseAtMaxNumberOfCounties = as.numeric(middleQuant[which.max(impreciseDf$numAdmin)]),
						upperImpreciseQuantAtMaxNumberOfCounties = as.numeric(upperQuant[which.max(impreciseDf$numAdmin)])
					)
				)

				# labels and titles
				letter <- if (count <= 26) { letters[count] } else { paste0(letters[count - 26], '\'') }
				main <- paste0(letter, ') ', thisNumErrorless, ' omniscient\n& ', thisNumPrecise, ' precise')

				boxPanel <- ggplot(impreciseDf, aes(y=resp)) +
					geom_point(
						data=preciseDf,
						mapping=aes(x=0, y=resp),
						position = position_jitter(w = 0.4, h = 0),
						color = precisePointColAlpha,
						size = pointSize
					) +
					geom_hline(yintercept = 1, size=0.4, linetype='solid', col='gray40') +
					geom_violin(
						data = preciseDf[preciseDf$resp >= quantile(preciseDf$resp, lower, na.rm=TRUE) & preciseDf$resp <= quantile(preciseDf$resp, upper, na.rm=TRUE), ],
						mapping = aes(x=0, y = resp),
						draw_quantiles = 0.5,
						fill = NA
					) +
					scale_x_continuous(breaks=0, labels=c('Pre-\ncise')) +
					scale_y_continuous(limits=c(ymin, ymax)) +
					# scale_y_log10(
						# limits=c(ymin, ymax),
						# breaks=trans_breaks('log10', function(x) 10^x),
						# labels=trans_format('log10', math_format(10^.x))
					# ) +
					ggtitle(main) +
					theme(
						plot.title = element_text(hjust = 0, vjust=-1.5, size=panelSize),
						plot.background = element_blank(),
						panel.grid.major.x = element_blank(),
						panel.grid.minor.x = element_blank(),
						axis.title.x = element_blank(),
						axis.text.x = element_text(angle = 0, hjust=0.5, vjust=0),
						axis.title.y = element_blank(),
						axis.text.y = element_text(size=axisNumSize),
					)

				xbreaks <- pretty(c(1, xmax), 3)
				xbreaks[1] <- 1
					
				imprecisePanel <- ggplot(impreciseDf, aes(x=numAdmin, y=resp)) +
					geom_point(data=impreciseDf, color=pointCol, size=pointSize) +					
					geom_line(data=topBoundaryImprecise, mapping=aes(x=numAdmin, y=boundary), size=boundaryLineThickness) +
					geom_line(data=bottomBoundaryImprecise, mapping=aes(x=numAdmin, y=boundary), size=boundaryLineThickness) +
					geom_line(data=middleImprecise, mapping=aes(x=numAdmin, y=middle), col=impreciseLineCol, size=medianSize) +
					geom_hline(yintercept = 1, size=0.4, linetype='solid', col='gray40') +
					scale_x_continuous(breaks = xbreaks, limits=c(xmin, xmax)) +
					scale_y_continuous(limits=c(ymin, ymax)) +
					# scale_y_log10(
						# limits=c(ymin, ymax),
						# breaks=trans_breaks('log10', function(x) 10^x),
						# labels=trans_format('log10', math_format(10^.x))
					# ) +
					xlab('County records') +
					theme(
						plot.background = element_blank(),
						plot.title = element_blank(),
						axis.title.x = element_text(size=axisSizeX, vjust=8),
						axis.text.x = element_text(size=axisNumSize),
						axis.title.y = element_blank(),
						axis.text.y = element_blank(),
						axis.ticks.y = element_blank()
					)

				boxPanel <- boxPanel + theme(plot.margin = grid::unit(c(0, 0, 0, 0.1), 'cm'))
				imprecisePanel <- imprecisePanel + theme(plot.margin = grid::unit(c(0, 0.2, 0, 0), 'cm'))

				comboPanel <- plot_grid(boxPanel, imprecisePanel, ncol=2, rel_widths=c(0.55, 1), align='h')
				if (count %in% 1:3) comboPanel <- comboPanel + theme(plot.margin=grid::unit(c(0, 0, 0, 1), 'cm'))
				if (count == 3) comboPanel <- comboPanel +
					geom_text(aes(x=0, y=0, label=ylab, angle=90, size=axisSizeY, vjust=-1))

				top <- 0.5
				right <- 0.5
				bottom <- 0
				left <- 1

				if (count %in% c(1)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(top, 0, 0, right), 'in'))
				if (count %in% c(2:3)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(0, 0, 0, right), 'in'))
				if (count %in% c(4, 10, 16)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(top, 0, 0, 0), 'in'))
				if (count %in% c(22)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(top, right, 0, 0), 'in'))
				if (count %in% c(22:27)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(0, right, 0, 0), 'in'))
				if (count %in% c(27)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(0, right, bottom, 0), 'in'))
				if (count %in% c(9, 15, 21, 27)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(0, 0, bottom, 0), 'in'))

				panels[[count]] <- comboPanel
				names(panels)[count] <- paste0('errorless', thisNumErrorless, '_precise', thisNumPrecise)
				
			} # next number of precise
			
		} # next number of errorless

		main <- panels[[1]] + panels[[2]] + panels[[3]] + plot_spacer() + plot_spacer() + plot_spacer() + # errorless 20
			panels[[4]] + panels[[5]] + panels[[6]] + panels[[7]] + panels[[8]] + panels[[9]] + # errorless 40
			panels[[10]] + panels[[11]] + panels[[12]] + panels[[13]] + panels[[14]] + panels[[15]] + # errorless 80
			panels[[16]] + panels[[17]] + panels[[18]] + panels[[19]] + panels[[20]] + panels[[21]] + # errorless 160
			panels[[22]] + panels[[23]] + panels[[24]] + panels[[25]] + panels[[26]] + panels[[27]] + # errorless 320
			plot_layout(byrow=FALSE, nrow=6)

		main <- main +
			plot_annotation(
				caption = str_wrap(caption, width=105),
				theme=theme(
					plot.caption=element_text(
						size=11,
						hjust=0,
						vjust=5
					)
				)
			)

		ggsave(plot=main, filename=paste0('./Analysis/Virtual Species/', plotFileName, '.pdf'), width=8.5, height=11, units='in')
		write.csv(trends, paste0('./Analysis/Virtual Species/', trendsFileName, '.csv'), row.names=FALSE)
		
	### univariate niche breadth: TAP
	#################################

		### generalization

		subject <- 'TAP'
		errorless <- 'errorlessNicheBreadthTap_range_mm'
		precise <- 'preciseNicheBreadthTap_range_mm'
		imprecise <- 'impreciseNicheBreadthTap_range_mm'
		
		# ylab <- expression(paste0(log[10]*'(Ratio of estimated-to-true EOO)'))
		ylab <- 'Ratio of estimated-to-true realized niche breadth'
		
		plotFileName <- '!Univariate Niche Breadth TAP - All Results'
		trendsFileName <- '!Trends - Univariate Niche Breadth TAP'

		caption <- tapCaption

		### y-axis limits
		these <- c(
			(results[ , imprecise]) / (results[ , errorless]),
			(results[ , precise]) / (results[ , errorless])
		)
		ymin <- 0
		ymax <- max(these)
		
		trends <- data.frame()
		panels <- list()
		count <- 0
		for (thisNumErrorless in numErrorlessSelected) {
		
			# subset number of precise so it's < total errorless
			allowableNumPreciseSelected <- numPreciseSelected[numPreciseSelected < thisNumErrorless]

			# x-axis limits
			these <- results[results$numErrorless == thisNumErrorless, ]
			xmax <- 1.015 * max(these$numAdmin)
			xmin <- 1 - 0.015 * xmax

			for (thisNumPrecise in allowableNumPreciseSelected) {

				say(subject, ' omniscient ', thisNumErrorless, ' precise ', thisNumPrecise)
				count <- count + 1

				# tall imprecise data
				thisResults <- results[results$numErrorless == thisNumErrorless & results$numPrecise == thisNumPrecise, ]
				resp <- thisResults[ , imprecise] / thisResults[ , errorless]
				n <- nrow(thisResults)
				impreciseDf <- data.frame(
					numAdmin = thisResults$numAdmin,
					resp = resp
				)
				
				# calculate quantiles for regression: imprecise
				impreciseDf <- impreciseDf[order(impreciseDf$numAdmin), ]
				impreciseDf <- impreciseDf[!is.infinite(impreciseDf$resp), ]
				k <- if (length(unique(impreciseDf$numAdmin)) <= 6) { 1 } else { -1 }
				middleQuant <- qgam(log(resp) ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=0.5)
				lowerQuant <- qgam(log(resp) ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=lower)
				upperQuant <- qgam(log(resp) ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=upper)

				middleQuant <- exp(predict(middleQuant, impreciseDf, type='response'))
				lowerQuant <- exp(predict(lowerQuant, impreciseDf, type='response'))
				upperQuant <- exp(predict(upperQuant, impreciseDf, type='response'))

				middleImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					middle = middleQuant
				)
				
				topBoundaryImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					boundary = upperQuant
				)
				
				bottomBoundaryImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					boundary = lowerQuant
				)

				# frames for violin plot of precise
				preciseDf <- data.frame(
					numAdmin = 0,
					resp = thisResults[ , precise] / thisResults[ , errorless]
				)
				preciseUpper <- quantile(preciseDf$resp, upper, na.rm=TRUE)
				preciseMiddle <- quantile(preciseDf$resp, 0.5, na.rm=TRUE)
				preciseLower <- quantile(preciseDf$resp, lower, na.rm=TRUE)

				# trends
				trends <- rbind(
					trends,
					data.frame(
						numErrorless = thisNumErrorless,
						numPrecise = thisNumPrecise,
						preciseLower = as.numeric(preciseLower),
						preciseMedian = as.numeric(preciseMiddle),
						preciseUpper = as.numeric(preciseUpper),
						propImpreciseGt1 = sum(resp > 1, na.rm=TRUE) / length(na.omit(resp)),
						lowerImpreciseQuantAtMaxNumberOfCounties = as.numeric(lowerQuant[which.max(impreciseDf$numAdmin)]),
						medianImpreciseAtMaxNumberOfCounties = as.numeric(middleQuant[which.max(impreciseDf$numAdmin)]),
						upperImpreciseQuantAtMaxNumberOfCounties = as.numeric(upperQuant[which.max(impreciseDf$numAdmin)])
					)
				)

				# labels and titles
				letter <- if (count <= 26) { letters[count] } else { paste0(letters[count - 26], '\'') }
				main <- paste0(letter, ') ', thisNumErrorless, ' omniscient\n& ', thisNumPrecise, ' precise')

				boxPanel <- ggplot(impreciseDf, aes(y=resp)) +
					geom_point(
						data=preciseDf,
						mapping=aes(x=0, y=resp),
						position = position_jitter(w = 0.4, h = 0),
						color = precisePointColAlpha,
						size = pointSize
					) +
					geom_hline(yintercept = 1, size=0.4, linetype='solid', col='gray40') +
					geom_violin(
						data = preciseDf[preciseDf$resp >= quantile(preciseDf$resp, lower, na.rm=TRUE) & preciseDf$resp <= quantile(preciseDf$resp, upper, na.rm=TRUE), ],
						mapping = aes(x=0, y = resp),
						draw_quantiles = 0.5,
						fill = NA
					) +
					scale_x_continuous(breaks=0, labels=c('Pre-\ncise')) +
					scale_y_continuous(limits=c(ymin, ymax)) +
					# scale_y_log10(
						# limits=c(ymin, ymax),
						# breaks=trans_breaks('log10', function(x) 10^x),
						# labels=trans_format('log10', math_format(10^.x))
					# ) +
					ggtitle(main) +
					theme(
						plot.title = element_text(hjust = 0, vjust=-1.5, size=panelSize),
						plot.background = element_blank(),
						panel.grid.major.x = element_blank(),
						panel.grid.minor.x = element_blank(),
						axis.title.x = element_blank(),
						axis.text.x = element_text(angle = 0, hjust=0.5, vjust=0),
						axis.title.y = element_blank(),
						axis.text.y = element_text(size=axisNumSize),
					)

				xbreaks <- pretty(c(1, xmax), 3)
				xbreaks[1] <- 1
					
				imprecisePanel <- ggplot(impreciseDf, aes(x=numAdmin, y=resp)) +
					geom_point(data=impreciseDf, color=pointCol, size=pointSize) +					
					geom_line(data=topBoundaryImprecise, mapping=aes(x=numAdmin, y=boundary), size=boundaryLineThickness) +
					geom_line(data=bottomBoundaryImprecise, mapping=aes(x=numAdmin, y=boundary), size=boundaryLineThickness) +
					geom_line(data=middleImprecise, mapping=aes(x=numAdmin, y=middle), col=impreciseLineCol, size=medianSize) +
					geom_hline(yintercept = 1, size=0.4, linetype='solid', col='gray40') +
					scale_x_continuous(breaks = xbreaks, limits=c(xmin, xmax)) +
					scale_y_continuous(limits=c(ymin, ymax)) +
					# scale_y_log10(
						# limits=c(ymin, ymax),
						# breaks=trans_breaks('log10', function(x) 10^x),
						# labels=trans_format('log10', math_format(10^.x))
					# ) +
					xlab('County records') +
					theme(
						plot.background = element_blank(),
						plot.title = element_blank(),
						axis.title.x = element_text(size=axisSizeX, vjust=8),
						axis.text.x = element_text(size=axisNumSize),
						axis.title.y = element_blank(),
						axis.text.y = element_blank(),
						axis.ticks.y = element_blank()
					)

				boxPanel <- boxPanel + theme(plot.margin = grid::unit(c(0, 0, 0, 0.1), 'cm'))
				imprecisePanel <- imprecisePanel + theme(plot.margin = grid::unit(c(0, 0.2, 0, 0), 'cm'))

				comboPanel <- plot_grid(boxPanel, imprecisePanel, ncol=2, rel_widths=c(0.55, 1), align='h')
				if (count %in% 1:3) comboPanel <- comboPanel + theme(plot.margin=grid::unit(c(0, 0, 0, 1), 'cm'))
				if (count == 3) comboPanel <- comboPanel +
					geom_text(aes(x=0, y=0, label=ylab, angle=90, size=axisSizeY, vjust=-1))

				top <- 0.5
				right <- 0.5
				bottom <- 0
				left <- 1

				if (count %in% c(1)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(top, 0, 0, right), 'in'))
				if (count %in% c(2:3)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(0, 0, 0, right), 'in'))
				if (count %in% c(4, 10, 16)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(top, 0, 0, 0), 'in'))
				if (count %in% c(22)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(top, right, 0, 0), 'in'))
				if (count %in% c(22:27)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(0, right, 0, 0), 'in'))
				if (count %in% c(27)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(0, right, bottom, 0), 'in'))
				if (count %in% c(9, 15, 21, 27)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(0, 0, bottom, 0), 'in'))

				panels[[count]] <- comboPanel
				names(panels)[count] <- paste0('errorless', thisNumErrorless, '_precise', thisNumPrecise)
				
			} # next number of precise
			
		} # next number of errorless

		main <- panels[[1]] + panels[[2]] + panels[[3]] + plot_spacer() + plot_spacer() + plot_spacer() + # errorless 20
			panels[[4]] + panels[[5]] + panels[[6]] + panels[[7]] + panels[[8]] + panels[[9]] + # errorless 40
			panels[[10]] + panels[[11]] + panels[[12]] + panels[[13]] + panels[[14]] + panels[[15]] + # errorless 80
			panels[[16]] + panels[[17]] + panels[[18]] + panels[[19]] + panels[[20]] + panels[[21]] + # errorless 160
			panels[[22]] + panels[[23]] + panels[[24]] + panels[[25]] + panels[[26]] + panels[[27]] + # errorless 320
			plot_layout(byrow=FALSE, nrow=6)

		main <- main +
			plot_annotation(
				caption = str_wrap(caption, width=105),
				theme=theme(
					plot.caption=element_text(
						size=11,
						hjust=0,
						vjust=5
					)
				)
			)

		ggsave(plot=main, filename=paste0('./Analysis/Virtual Species/', plotFileName, '.pdf'), width=8.5, height=11, units='in')
		write.csv(trends, paste0('./Analysis/Virtual Species/', trendsFileName, '.csv'), row.names=FALSE)
		
	### multivariate niche breadth: volume
	######################################

		### generalization

		subject <- 'VOLUME'
		errorless <- 'errorlessConvHullVol'
		precise <- 'preciseConvHullVol'
		imprecise <- 'impreciseConvHullVol'
		
		# ylab <- expression(paste0(log[10]*'(Ratio of estimated-to-true EOO)'))
		ylab <- 'Ratio of estimated-to-true realized niche volume'
		
		plotFileName <- '!Multivariate Niche Breadth Volume - All Results'
		trendsFileName <- '!Trends - Multivariate Niche Breadth Volume'

		caption <- volCaption

		### y-axis limits
		these <- c(
			(results[ , imprecise]) / (results[ , errorless]),
			(results[ , precise]) / (results[ , errorless])
		)
		ymin <- 0
		ymax <- quantile(these, outlierQuant, na.rm=TRUE)
		
		trends <- data.frame()
		panels <- list()
		count <- 0
		for (thisNumErrorless in numErrorlessSelected) {
		
			# subset number of precise so it's < total errorless
			allowableNumPreciseSelected <- numPreciseSelected[numPreciseSelected < thisNumErrorless]

			# x-axis limits
			these <- results[results$numErrorless == thisNumErrorless, ]
			xmax <- 1.015 * max(these$numAdmin)
			xmin <- 1 - 0.015 * xmax

			for (thisNumPrecise in allowableNumPreciseSelected) {

				say(subject, ' omniscient ', thisNumErrorless, ' precise ', thisNumPrecise)
				count <- count + 1

				# tall imprecise data
				thisResults <- results[results$numErrorless == thisNumErrorless & results$numPrecise == thisNumPrecise, ]
				resp <- ((thisResults[ , imprecise]) / (thisResults[ , errorless]))
				n <- nrow(thisResults)
				impreciseDf <- data.frame(
					numAdmin = thisResults$numAdmin,
					resp = resp
				)
				
				# calculate quantiles for regression: imprecise
				impreciseDf <- impreciseDf[order(impreciseDf$numAdmin), ]
				impreciseDf <- impreciseDf[!is.infinite(impreciseDf$resp), ]
				k <- if (length(unique(impreciseDf$numAdmin)) <= 6) { 1 } else { -1 }
				middleQuant <- qgam(resp ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=0.5)
				lowerQuant <- qgam(resp ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=lower)
				upperQuant <- qgam(resp ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=upper)

				middleQuant <- (predict(middleQuant, impreciseDf, type='response'))
				lowerQuant <- (predict(lowerQuant, impreciseDf, type='response'))
				upperQuant <- (predict(upperQuant, impreciseDf, type='response'))

				middleImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					middle = middleQuant
				)
				
				topBoundaryImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					boundary = upperQuant
				)
				
				bottomBoundaryImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					boundary = lowerQuant
				)

				# frames for violin plot of precise
				preciseDf <- data.frame(
					numAdmin = 0,
					resp = ((thisResults[ , precise]) / (thisResults[ , errorless]))
				)
				preciseUpper <- quantile(preciseDf$resp, upper, na.rm=TRUE)
				preciseMiddle <- quantile(preciseDf$resp, 0.5, na.rm=TRUE)
				preciseLower <- quantile(preciseDf$resp, lower, na.rm=TRUE)

				# trends
				trends <- rbind(
					trends,
					data.frame(
						numErrorless = thisNumErrorless,
						numPrecise = thisNumPrecise,
						preciseLower = as.numeric(preciseLower),
						preciseMedian = as.numeric(preciseMiddle),
						preciseUpper = as.numeric(preciseUpper),
						propImpreciseGt1 = sum(resp > 1, na.rm=TRUE) / length(na.omit(resp)),
						lowerImpreciseQuantAtMaxNumberOfCounties = as.numeric(lowerQuant[which.max(impreciseDf$numAdmin)]),
						medianImpreciseAtMaxNumberOfCounties = as.numeric(middleQuant[which.max(impreciseDf$numAdmin)]),
						upperImpreciseQuantAtMaxNumberOfCounties = as.numeric(upperQuant[which.max(impreciseDf$numAdmin)])
					)
				)

				# labels and titles
				letter <- if (count <= 26) { letters[count] } else { paste0(letters[count - 26], '\'') }
				main <- paste0(letter, ') ', thisNumErrorless, ' omniscient\n& ', thisNumPrecise, ' precise')

				boxPanel <- ggplot(impreciseDf, aes(y=resp)) +
					geom_point(
						data=preciseDf,
						mapping=aes(x=0, y=resp),
						position = position_jitter(w = 0.4, h = 0),
						color = precisePointColAlpha,
						size = pointSize
					) +
					geom_hline(yintercept = 1, size=0.4, linetype='solid', col='gray40') +
					geom_violin(
						data = preciseDf[preciseDf$resp >= quantile(preciseDf$resp, lower, na.rm=TRUE) & preciseDf$resp <= quantile(preciseDf$resp, upper, na.rm=TRUE), ],
						mapping = aes(x=0, y = resp),
						draw_quantiles = 0.5,
						fill = NA
					) +
					scale_x_continuous(breaks=0, labels=c('Pre-\ncise')) +
					scale_y_continuous(limits=c(ymin, ymax)) +
					# scale_y_log10(
						# limits=c(ymin, ymax),
						# breaks=trans_breaks('log10', function(x) 10^x),
						# labels=trans_format('log10', math_format(10^.x))
					# ) +
					ggtitle(main) +
					theme(
						plot.title = element_text(hjust = 0, vjust=-1.5, size=panelSize),
						plot.background = element_blank(),
						panel.grid.major.x = element_blank(),
						panel.grid.minor.x = element_blank(),
						axis.title.x = element_blank(),
						axis.text.x = element_text(angle = 0, hjust=0.5, vjust=0),
						axis.title.y = element_blank(),
						axis.text.y = element_text(size=axisNumSize),
					)

				xbreaks <- pretty(c(1, xmax), 3)
				xbreaks[1] <- 1
					
				imprecisePanel <- ggplot(impreciseDf, aes(x=numAdmin, y=resp)) +
					geom_point(data=impreciseDf, color=pointCol, size=pointSize) +					
					geom_line(data=topBoundaryImprecise, mapping=aes(x=numAdmin, y=boundary), size=boundaryLineThickness) +
					geom_line(data=bottomBoundaryImprecise, mapping=aes(x=numAdmin, y=boundary), size=boundaryLineThickness) +
					geom_line(data=middleImprecise, mapping=aes(x=numAdmin, y=middle), col=impreciseLineCol, size=medianSize) +
					geom_hline(yintercept = 1, size=0.4, linetype='solid', col='gray40') +
					scale_x_continuous(breaks = xbreaks, limits=c(xmin, xmax)) +
					scale_y_continuous(limits=c(ymin, ymax)) +
					# scale_y_log10(
						# limits=c(ymin, ymax),
						# breaks=trans_breaks('log10', function(x) 10^x),
						# labels=trans_format('log10', math_format(10^.x))
					# ) +
					xlab('County records') +
					theme(
						plot.background = element_blank(),
						plot.title = element_blank(),
						axis.title.x = element_text(size=axisSizeX, vjust=8),
						axis.text.x = element_text(size=axisNumSize),
						axis.title.y = element_blank(),
						axis.text.y = element_blank(),
						axis.ticks.y = element_blank()
					)

				boxPanel <- boxPanel + theme(plot.margin = grid::unit(c(0, 0, 0, 0.1), 'cm'))
				imprecisePanel <- imprecisePanel + theme(plot.margin = grid::unit(c(0, 0.2, 0, 0), 'cm'))

				comboPanel <- plot_grid(boxPanel, imprecisePanel, ncol=2, rel_widths=c(0.55, 1), align='h')
				if (count %in% 1:3) comboPanel <- comboPanel + theme(plot.margin=grid::unit(c(0, 0, 0, 1), 'cm'))
				if (count == 3) comboPanel <- comboPanel +
					geom_text(aes(x=0, y=0, label=ylab, angle=90, size=axisSizeY, vjust=-1))

				top <- 0.5
				right <- 0.5
				bottom <- 0
				left <- 1

				if (count %in% c(1)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(top, 0, 0, right), 'in'))
				if (count %in% c(2:3)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(0, 0, 0, right), 'in'))
				if (count %in% c(4, 10, 16)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(top, 0, 0, 0), 'in'))
				if (count %in% c(22)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(top, right, 0, 0), 'in'))
				if (count %in% c(22:27)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(0, right, 0, 0), 'in'))
				if (count %in% c(27)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(0, right, bottom, 0), 'in'))
				if (count %in% c(9, 15, 21, 27)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(0, 0, bottom, 0), 'in'))

				panels[[count]] <- comboPanel
				names(panels)[count] <- paste0('errorless', thisNumErrorless, '_precise', thisNumPrecise)
				
			} # next number of precise
			
		} # next number of errorless

		main <- panels[[1]] + panels[[2]] + panels[[3]] + plot_spacer() + plot_spacer() + plot_spacer() + # errorless 20
			panels[[4]] + panels[[5]] + panels[[6]] + panels[[7]] + panels[[8]] + panels[[9]] + # errorless 40
			panels[[10]] + panels[[11]] + panels[[12]] + panels[[13]] + panels[[14]] + panels[[15]] + # errorless 80
			panels[[16]] + panels[[17]] + panels[[18]] + panels[[19]] + panels[[20]] + panels[[21]] + # errorless 160
			panels[[22]] + panels[[23]] + panels[[24]] + panels[[25]] + panels[[26]] + panels[[27]] + # errorless 320
			plot_layout(byrow=FALSE, nrow=6)

		main <- main +
			plot_annotation(
				caption = str_wrap(caption, width=105),
				theme=theme(
					plot.caption=element_text(
						size=11,
						hjust=0,
						vjust=5
					)
				)
			)

		ggsave(plot=main, filename=paste0('./Analysis/Virtual Species/', plotFileName, '.pdf'), width=8.5, height=11, units='in')
		write.csv(trends, paste0('./Analysis/Virtual Species/', trendsFileName, '.csv'), row.names=FALSE)
		
	### multivariate niche breadth: area
	####################################

		### generalization

		subject <- 'AREA'
		errorless <- 'errorlessConvHullArea'
		precise <- 'preciseConvHullArea'
		imprecise <- 'impreciseConvHullArea'
		
		ylab <- 'Ratio of estimated-to-true realized niche surface area'
		
		plotFileName <- '!Multivariate Niche Breadth Area - All Results'
		trendsFileName <- '!Trends - Multivariate Niche Breadth Area'

		caption <- areaCaption

		### y-axis limits
		these <- c(
			(results[ , imprecise]) / (results[ , errorless]),
			(results[ , precise]) / (results[ , errorless])
		)
		ymin <- 0
		ymax <- quantile(these, outlierQuant, na.rm=TRUE)
		
		trends <- data.frame()
		panels <- list()
		count <- 0
		for (thisNumErrorless in numErrorlessSelected) {
		
			# subset number of precise so it's < total errorless
			allowableNumPreciseSelected <- numPreciseSelected[numPreciseSelected < thisNumErrorless]

			# x-axis limits
			these <- results[results$numErrorless == thisNumErrorless, ]
			xmax <- 1.015 * max(these$numAdmin)
			xmin <- 1 - 0.015 * xmax

			for (thisNumPrecise in allowableNumPreciseSelected) {

				say(subject, ' omniscient ', thisNumErrorless, ' precise ', thisNumPrecise)
				count <- count + 1

				# tall imprecise data
				thisResults <- results[results$numErrorless == thisNumErrorless & results$numPrecise == thisNumPrecise, ]
				resp <- ((thisResults[ , imprecise]) / (thisResults[ , errorless]))
				n <- nrow(thisResults)
				impreciseDf <- data.frame(
					numAdmin = thisResults$numAdmin,
					resp = resp
				)
				
				# calculate quantiles for regression: imprecise
				impreciseDf <- impreciseDf[order(impreciseDf$numAdmin), ]
				impreciseDf <- impreciseDf[!is.infinite(impreciseDf$resp), ]
				k <- if (length(unique(impreciseDf$numAdmin)) <= 6) { 1 } else { -1 }
				middleQuant <- qgam(resp ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=0.5)
				lowerQuant <- qgam(resp ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=lower)
				upperQuant <- qgam(resp ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=upper)

				middleQuant <- predict(middleQuant, impreciseDf, type='response')
				lowerQuant <- predict(lowerQuant, impreciseDf, type='response')
				upperQuant <- predict(upperQuant, impreciseDf, type='response')

				middleImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					middle = middleQuant
				)
				
				topBoundaryImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					boundary = upperQuant
				)
				
				bottomBoundaryImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					boundary = lowerQuant
				)

				# frames for violin plot of precise
				preciseDf <- data.frame(
					numAdmin = 0,
					resp = thisResults[ , precise] / thisResults[ , errorless]
				)
				preciseUpper <- quantile(preciseDf$resp, upper, na.rm=TRUE)
				preciseMiddle <- quantile(preciseDf$resp, 0.5, na.rm=TRUE)
				preciseLower <- quantile(preciseDf$resp, lower, na.rm=TRUE)

				# trends
				trends <- rbind(
					trends,
					data.frame(
						numErrorless = thisNumErrorless,
						numPrecise = thisNumPrecise,
						preciseLower = as.numeric(preciseLower),
						preciseMedian = as.numeric(preciseMiddle),
						preciseUpper = as.numeric(preciseUpper),
						propImpreciseGt1 = sum(resp > 1, na.rm=TRUE) / length(na.omit(resp)),
						lowerImpreciseQuantAtMaxNumberOfCounties = as.numeric(lowerQuant[which.max(impreciseDf$numAdmin)]),
						medianImpreciseAtMaxNumberOfCounties = as.numeric(middleQuant[which.max(impreciseDf$numAdmin)]),
						upperImpreciseQuantAtMaxNumberOfCounties = as.numeric(upperQuant[which.max(impreciseDf$numAdmin)])
					)
				)

				# labels and titles
				letter <- if (count <= 26) { letters[count] } else { paste0(letters[count - 26], '\'') }
				main <- paste0(letter, ') ', thisNumErrorless, ' omniscient\n& ', thisNumPrecise, ' precise')

				boxPanel <- ggplot(impreciseDf, aes(y=resp)) +
					geom_point(
						data=preciseDf,
						mapping=aes(x=0, y=resp),
						position = position_jitter(w = 0.4, h = 0),
						color = precisePointColAlpha,
						size = pointSize
					) +
					geom_hline(yintercept = 1, size=0.4, linetype='solid', col='gray40') +
					geom_violin(
						data = preciseDf[preciseDf$resp >= quantile(preciseDf$resp, lower, na.rm=TRUE) & preciseDf$resp <= quantile(preciseDf$resp, upper, na.rm=TRUE), ],
						mapping = aes(x=0, y = resp),
						draw_quantiles = 0.5,
						fill = NA
					) +
					scale_x_continuous(breaks=0, labels=c('Pre-\ncise')) +
					scale_y_continuous(limits=c(ymin, ymax)) +
					# scale_y_log10(
						# limits=c(ymin, ymax),
						# breaks=trans_breaks('log10', function(x) 10^x),
						# labels=trans_format('log10', math_format(10^.x))
					# ) +
					ggtitle(main) +
					theme(
						plot.title = element_text(hjust = 0, vjust=-1.5, size=panelSize),
						plot.background = element_blank(),
						panel.grid.major.x = element_blank(),
						panel.grid.minor.x = element_blank(),
						axis.title.x = element_blank(),
						axis.text.x = element_text(angle = 0, hjust=0.5, vjust=0),
						axis.title.y = element_blank(),
						axis.text.y = element_text(size=axisNumSize),
					)

				xbreaks <- pretty(c(1, xmax), 3)
				xbreaks[1] <- 1
					
				imprecisePanel <- ggplot(impreciseDf, aes(x=numAdmin, y=resp)) +
					geom_point(data=impreciseDf, color=pointCol, size=pointSize) +					
					geom_line(data=topBoundaryImprecise, mapping=aes(x=numAdmin, y=boundary), size=boundaryLineThickness) +
					geom_line(data=bottomBoundaryImprecise, mapping=aes(x=numAdmin, y=boundary), size=boundaryLineThickness) +
					geom_line(data=middleImprecise, mapping=aes(x=numAdmin, y=middle), col=impreciseLineCol, size=medianSize) +
					geom_hline(yintercept = 1, size=0.4, linetype='solid', col='gray40') +
					scale_x_continuous(breaks = xbreaks, limits=c(xmin, xmax)) +
					scale_y_continuous(limits=c(ymin, ymax)) +
					# scale_y_log10(
						# limits=c(ymin, ymax),
						# breaks=trans_breaks('log10', function(x) 10^x),
						# labels=trans_format('log10', math_format(10^.x))
					# ) +
					xlab('County records') +
					theme(
						plot.background = element_blank(),
						plot.title = element_blank(),
						axis.title.x = element_text(size=axisSizeX, vjust=8),
						axis.text.x = element_text(size=axisNumSize),
						axis.title.y = element_blank(),
						axis.text.y = element_blank(),
						axis.ticks.y = element_blank()
					)

				boxPanel <- boxPanel + theme(plot.margin = grid::unit(c(0, 0, 0, 0.1), 'cm'))
				imprecisePanel <- imprecisePanel + theme(plot.margin = grid::unit(c(0, 0.2, 0, 0), 'cm'))

				comboPanel <- plot_grid(boxPanel, imprecisePanel, ncol=2, rel_widths=c(0.55, 1), align='h')
				if (count %in% 1:3) comboPanel <- comboPanel + theme(plot.margin=grid::unit(c(0, 0, 0, 1), 'cm'))
				if (count == 3) comboPanel <- comboPanel +
					geom_text(aes(x=0, y=0, label=ylab, angle=90, size=axisSizeY, vjust=-1))

				top <- 0.5
				right <- 0.5
				bottom <- 0
				left <- 1

				if (count %in% c(1)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(top, 0, 0, right), 'in'))
				if (count %in% c(2:3)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(0, 0, 0, right), 'in'))
				if (count %in% c(4, 10, 16)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(top, 0, 0, 0), 'in'))
				if (count %in% c(22)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(top, right, 0, 0), 'in'))
				if (count %in% c(22:27)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(0, right, 0, 0), 'in'))
				if (count %in% c(27)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(0, right, bottom, 0), 'in'))
				if (count %in% c(9, 15, 21, 27)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(0, 0, bottom, 0), 'in'))

				panels[[count]] <- comboPanel
				names(panels)[count] <- paste0('errorless', thisNumErrorless, '_precise', thisNumPrecise)
				
			} # next number of precise
			
		} # next number of errorless

		main <- panels[[1]] + panels[[2]] + panels[[3]] + plot_spacer() + plot_spacer() + plot_spacer() + # errorless 20
			panels[[4]] + panels[[5]] + panels[[6]] + panels[[7]] + panels[[8]] + panels[[9]] + # errorless 40
			panels[[10]] + panels[[11]] + panels[[12]] + panels[[13]] + panels[[14]] + panels[[15]] + # errorless 80
			panels[[16]] + panels[[17]] + panels[[18]] + panels[[19]] + panels[[20]] + panels[[21]] + # errorless 160
			panels[[22]] + panels[[23]] + panels[[24]] + panels[[25]] + panels[[26]] + panels[[27]] + # errorless 320
			plot_layout(byrow=FALSE, nrow=6)

		main <- main +
			plot_annotation(
				caption = str_wrap(caption, width=105),
				theme=theme(
					plot.caption=element_text(
						size=11,
						hjust=0,
						vjust=5
					)
				)
			)

		ggsave(plot=main, filename=paste0('./Analysis/Virtual Species/', plotFileName, '.pdf'), width=8.5, height=11, units='in')
		write.csv(trends, paste0('./Analysis/Virtual Species/', trendsFileName, '.csv'), row.names=FALSE)

say('#####################################')
say('### make plot of model complexity ###')
say('#####################################')

	# panels, one per scenario, with each column number of errorless, rows number of precise

	### generalization
	##################

		# text size
		panelSize <- 8 # panel title
		axisSizeX <- 8 # axes with labels
		axisSizeY <- 16 # axes with labels
		axisNumSize <- 8 # numbers on axes
		
		# lines
		medianSize <- 1
		boundaryLineThickness <- 0.4 # thickness of inner quantile boundary's lines
		
		# point size
		pointSize <- 0.25
		
		# colors
		pointCol <- '#d95f02'
		
		impreciseFill <- NA
		impreciseLineCol <- 'black'

		preciseFill <- '#1b9e77'
		precisePointColAlpha <- alpha('#1b9e77', 0.5)

		errorlessPointColAlpha <- alpha('gray50', 0.5)

		# quantiles to plot
		lower <- 0.05
		upper <- 0.95
	
	### preliminary
	###############

		load('./Analysis/Virtual Species/!!Collated Results.rda')
		numErrorlessSelected <- sort(unique(results$numErrorless))
		numPreciseSelected <- sort(unique(results$numPrecise))

	### ENM calibration: present
	############################

		### generalization

		subject <- 'ENM COMPLEXITY'
		errorless <- 'modelTermsErrorless'
		precise <- 'modelTermsPrecise'
		imprecise <- 'modelTermsImprecise'
		
		ylab <- 'Number of non-zero coefficients'
		
		plotFileName <- '!ENM Complexity - All Results'
		trendsFileName <- '!Trends - ENM Complexity'

		caption <- complexityCaption

		### y-axis limits
		these <- c(
			results[ , 'modelTermsErrorless'], results[ , 'modelTermsPrecise'], results[ , 'modelTermsImprecise']
		)
		ymin <- 0
		ymax <- max(these)
		ybreaks <- pretty(c(ymin, ymax))
		
		trends <- data.frame()
		panels <- list()
		count <- 0
		for (thisNumErrorless in numErrorlessSelected) {
		
			# subset number of precise so it's < total errorless
			allowableNumPreciseSelected <- numPreciseSelected[numPreciseSelected < thisNumErrorless]

			# x-axis limits
			these <- results[results$numErrorless == thisNumErrorless, ]
			xmax <- 1.015 * max(these$numAdmin)
			xmin <- 1 - 0.015 * xmax

			for (thisNumPrecise in allowableNumPreciseSelected) {

				say(subject, ' omniscient ', thisNumErrorless, ' precise ', thisNumPrecise)
				count <- count + 1

				# tall imprecise data
				thisResults <- results[results$numErrorless == thisNumErrorless & results$numPrecise == thisNumPrecise, ]
				resp <- thisResults[ , imprecise]
				n <- nrow(thisResults)
				impreciseDf <- data.frame(
					numAdmin = thisResults$numAdmin,
					resp = resp
				)
				
				# calculate quantiles for regression: imprecise
				impreciseDf <- impreciseDf[order(impreciseDf$numAdmin), ]
				k <- if (length(unique(impreciseDf$numAdmin)) <= 6) { 1 } else { -1 }
				middleQuant <- qgam(resp ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=0.5)
				lowerQuant <- qgam(resp ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=lower)
				upperQuant <- qgam(resp ~ s(numAdmin, bs='cr', k=k), data=impreciseDf, qu=upper)

				middleQuant <- predict(middleQuant, impreciseDf, type='response')
				lowerQuant <- predict(lowerQuant, impreciseDf, type='response')
				upperQuant <- predict(upperQuant, impreciseDf, type='response')

				middleImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					middle = middleQuant
				)
				
				topBoundaryImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					boundary = upperQuant
				)
				
				bottomBoundaryImprecise <- data.frame(
					numAdmin = impreciseDf$numAdmin,
					boundary = lowerQuant
				)

				# frames for violin plot of precise
				n <- nrow(thisResults)
				errorlessDf <- data.frame(
					x = rep(-0.5, n),
					resp = thisResults[ , errorless]
				)
				preciseDf <- data.frame(
					x = rep(0.5, n),
					resp = thisResults[ , precise]
				)
				errorlessPreciseDf <- rbind(errorlessDf, preciseDf)
				
				errorlessUpper <- quantile(errorlessDf$resp, upper, na.rm=TRUE)
				errorlessMiddle <- quantile(errorlessDf$resp, 0.5, na.rm=TRUE)
				errorlessLower <- quantile(errorlessDf$resp, lower, na.rm=TRUE)

				preciseUpper <- quantile(preciseDf$resp, upper, na.rm=TRUE)
				preciseMiddle <- quantile(preciseDf$resp, 0.5, na.rm=TRUE)
				preciseLower <- quantile(preciseDf$resp, lower, na.rm=TRUE)

				# trends
				trends <- rbind(
					trends,
					data.frame(
						numErrorless = thisNumErrorless,
						numPrecise = thisNumPrecise,
						errorlessLower = as.numeric(errorlessLower),
						errorlessMedian = as.numeric(errorlessMiddle),
						errorlessUpper = as.numeric(errorlessUpper),
						preciseLower = as.numeric(preciseLower),
						preciseMedian = as.numeric(preciseMiddle),
						preciseUpper = as.numeric(preciseUpper),
						lowerImpreciseQuantAtMaxNumberOfCounties = as.numeric(lowerQuant[which.max(impreciseDf$numAdmin)]),
						medianImpreciseAtMaxNumberOfCounties = as.numeric(middleQuant[which.max(impreciseDf$numAdmin)]),
						upperImpreciseQuantAtMaxNumberOfCounties = as.numeric(upperQuant[which.max(impreciseDf$numAdmin)])
					)
				)

				# labels and titles
				letter <- if (count <= 26) { letters[count] } else { paste0(letters[count - 26], '\'') }
				main <- paste0(letter, ') ', thisNumErrorless, ' omniscient\n& ', thisNumPrecise, ' precise')

				boxPanel <- ggplot(errorlessPreciseDf, aes(y=resp)) +
					geom_point(
						data=errorlessDf,
						mapping=aes(x=x, y=resp),
						position = position_jitter(w = 0.4, h = 0),
						color = errorlessPointColAlpha,
						size = pointSize
					) +
					geom_point(
						data=preciseDf,
						mapping=aes(x=x, y=resp),
						position = position_jitter(w = 0.4, h = 0),
						color = precisePointColAlpha,
						size = pointSize
					) +
					geom_violin(
						data = preciseDf[preciseDf$resp >= preciseLower & preciseDf$resp <= preciseUpper, ],
						mapping = aes(x=x, y = resp),
						draw_quantiles = 0.5,
						fill = NA
					) +
					geom_violin(
						data = errorlessDf[errorlessDf$resp >= preciseLower & errorlessDf$resp <= preciseUpper, ],
						mapping = aes(x=x, y = resp),
						draw_quantiles = 0.5,
						fill = NA
					) +
					scale_x_continuous(breaks=c(-0.5, 0.5), labels=c('Omni', 'Prec')) +
					scale_y_continuous(breaks=ybreaks) +
					coord_cartesian(xlim=c(-1, 1), ylim=c(ymin, ymax), expand=FALSE) +
					ggtitle(main) +
					theme(
						plot.title = element_text(hjust = 0, size=panelSize),
						plot.background = element_blank(),
						panel.grid.major.x = element_blank(),
						panel.grid.minor.x = element_blank(),
						axis.title.x = element_blank(),
						axis.text.x = element_text(angle = 90, hjust=1, vjust=0, size=axisNumSize),
						axis.title.y = element_blank(),
						axis.text.y = element_text(size=axisNumSize),
						legend.position='none'
					)

				xbreaks <- pretty(c(1, xmax), 3)
				xbreaks[1] <- 1
					
				imprecisePanel <- ggplot(impreciseDf, aes(x=numAdmin, y=resp)) +
					geom_point(data=impreciseDf, color=pointCol, size=pointSize) +					
					geom_line(data=topBoundaryImprecise, mapping=aes(x=numAdmin, y=boundary), size=boundaryLineThickness) +
					geom_line(data=bottomBoundaryImprecise, mapping=aes(x=numAdmin, y=boundary), size=boundaryLineThickness) +
					geom_line(data=middleImprecise, mapping=aes(x=numAdmin, y=middle), col=impreciseLineCol, size=medianSize) +
					coord_cartesian(xlim=c(xmin, xmax), ylim=c(ymin, ymax), expand=FALSE) +
					xlab('County records') +
					scale_x_continuous(breaks = xbreaks) +
					theme(
						plot.background = element_blank(),
						plot.title = element_blank(),
						axis.title.x = element_text(size=axisSizeX, vjust=7),
						axis.text.x = element_text(size=axisNumSize),
						axis.title.y = element_blank(),
						axis.text.y = element_blank(),
						axis.ticks.y = element_blank()
					)

				boxPanel <- boxPanel + theme(plot.margin = grid::unit(c(0, 0, 0, 0.1), 'cm'))
				imprecisePanel <- imprecisePanel + theme(plot.margin = grid::unit(c(0, 0.2, 0, 0), 'cm'))

				comboPanel <- plot_grid(boxPanel, imprecisePanel, ncol=2, rel_widths=c(0.65, 1), align='h')
				if (count %in% 1:3) comboPanel <- comboPanel + theme(plot.margin=grid::unit(c(0, 0, 0, 1), 'cm'))
				if (count == 3) comboPanel <- comboPanel +
					geom_text(aes(x=0, y=0, label=ylab, angle=90, size=axisSizeY, vjust=-1))

				top <- 0.5
				right <- 0.5
				bottom <- 0
				left <- 1

				if (count %in% c(1)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(top, 0, 0, right), 'in'))
				if (count %in% c(2:3)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(0, 0, 0, right), 'in'))
				if (count %in% c(4, 10, 16)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(top, 0, 0, 0), 'in'))
				if (count %in% c(22)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(top, right, 0, 0), 'in'))
				if (count %in% c(22:27)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(0, right, 0, 0), 'in'))
				if (count %in% c(27)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(0, right, bottom, 0), 'in'))
				if (count %in% c(9, 15, 21, 27)) comboPanel <- comboPanel + theme(plot.margin = grid::unit(c(0, 0, bottom, 0), 'in'))

				panels[[count]] <- comboPanel
				names(panels)[count] <- paste0('errorless', thisNumErrorless, '_precise', thisNumPrecise)
				
			} # next number of precise
			
		} # next number of errorless

		main <- panels[[1]] + panels[[2]] + panels[[3]] + plot_spacer() + plot_spacer() + plot_spacer() + # errorless 20
			panels[[4]] + panels[[5]] + panels[[6]] + panels[[7]] + panels[[8]] + panels[[9]] + # errorless 40
			panels[[10]] + panels[[11]] + panels[[12]] + panels[[13]] + panels[[14]] + panels[[15]] + # errorless 80
			panels[[16]] + panels[[17]] + panels[[18]] + panels[[19]] + panels[[20]] + panels[[21]] + # errorless 160
			panels[[22]] + panels[[23]] + panels[[24]] + panels[[25]] + panels[[26]] + panels[[27]] + # errorless 320
			plot_layout(byrow=FALSE, nrow=6)

		main <- main +
			plot_annotation(
				caption = str_wrap(caption, width=105),
				theme=theme(
					plot.caption=element_text(
						size=11,
						hjust=0,
						vjust=5
					)
				)
			)

		ggsave(plot=main, filename=paste0('./Analysis/Virtual Species/', plotFileName, '.pdf'), width=8.5, height=11, units='in')
		write.csv(trends, paste0('./Analysis/Virtual Species/', trendsFileName, '.csv'), row.names=FALSE)
	
	

say('DONE!', deco='&', level=1)
