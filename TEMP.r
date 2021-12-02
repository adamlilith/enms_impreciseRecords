# source('C:/Ecology/Drive/Research Active/Vaguely Georeferenced Specimen Records/Code/TEMP.r')

say('##################################################################################################')
say('### make composite plot with SUBSET of EOO and niche breadth results - TWO errorless scenarios ###')
say('##################################################################################################')
	
	# sets of panels, one per metric (EOO, MAT niche breadth, etc.), four panels per set crossing number of errorless and number of precise

	### generalization
	##################

		# text size
		setSize <- 12 # panel set title
		panelSize <- 9 # panel title
		axisSizeX <- 9 # axes with labels
		axisSizeY <- 10 # axes with labels
		axisNumSize <- 8 # numbers on axes
		
		# lines
		medianSize <- 1
		hlineSize <- 0.6 # thickness of horizontal line at 1
		
		# point size
		pointSize <- 0.25
		
		# colors
		pointCol <- '#d95f02'
		
		impreciseFill <- alpha('black', 0.3)
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
		
		numPreciseSelected <- c(5, 30)

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
		
		ylab <- 'Estimated / errorless EOO'
		
		### y-axis limits
		these <- c(
			results[ , imprecise] / results[ , errorless],
			results[ , precise] / results[ , errorless]
		)
		ymin <- min(0, these)
		ymax <- max(these)
		ybreaks <- pretty(c(ymin, ymax), 3)
		
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

				say(subject, ' errorless ', thisNumErrorless, ' precise ', thisNumPrecise)
				count <- count + 1

				# tall imprecise data
				thisResults <- results[results$numErrorless == thisNumErrorless & results$numPrecise == thisNumPrecise, ]
				resp <- thisResults[ , imprecise] / thisResults[ , errorless]
				n <- nrow(thisResults)
				tallImprecise <- data.frame(
					numAdmin = thisResults$numAdmin,
					resp = resp
				)
				
				# calculate quantiles for regression: imprecise
				tallImprecise <- tallImprecise[order(tallImprecise$numAdmin), ]
				k <- if (length(unique(tallImprecise$numAdmin)) <= 6) { 1 } else { -1 }
				middleQuant <- qgam(log(resp) ~ s(numAdmin, bs='cr', k=k), data=tallImprecise, qu=0.5)
				lowerQuant <- qgam(log(resp) ~ s(numAdmin, bs='cr', k=k), data=tallImprecise, qu=lower)
				upperQuant <- qgam(log(resp) ~ s(numAdmin, bs='cr', k=k), data=tallImprecise, qu=upper)
				
				middleQuant <- exp(predict(middleQuant, tallImprecise, type='response'))
				lowerQuant <- exp(predict(lowerQuant, tallImprecise, type='response'))
				upperQuant <- exp(predict(upperQuant, tallImprecise, type='response'))

				middleImprecise <- data.frame(
					numAdmin = tallImprecise$numAdmin,
					middle = middleQuant
				)
				
				boundaryImprecise <- data.frame(
					numAdmin = c(tallImprecise$numAdmin, rev(tallImprecise$numAdmin)),
					boundary = c(lowerQuant, rev(upperQuant))
				)
				
				middleImprecise <- middleImprecise[!duplicated(middleImprecise), ]
				boundaryImprecise <- boundaryImprecise[!duplicated(boundaryImprecise), ]

				# frames for boxplot of precise
				preciseDf <- data.frame(
					numAdmin = 0,
					resp = thisResults[ , precise] / thisResults[ , errorless]
				)
				preciseUpper <- quantile(preciseDf$resp, upper, na.rm=TRUE)
				preciseMiddle <- quantile(preciseDf$resp, 0.5, na.rm=TRUE)
				preciseLower <- quantile(preciseDf$resp, lower, na.rm=TRUE)

				# labels and titles
				main <- paste0(thisNumErrorless, ' errorless ', thisNumPrecise, ' precise')

				boxPanel <- ggplot(tallImprecise, aes(y=resp)) +
					geom_point(
						data=preciseDf,
						mapping=aes(x=0, y=resp),
						position = position_jitter(w = 0.4, h = 0),
						color = precisePointColAlpha,
						size = pointSize
					) +
					geom_boxplot(
						data = preciseDf,
						mapping = aes(y = resp),
						upper = preciseUpper, middle = preciseMiddle, lower = preciseLower,
						coef = 0, outlier.shape = NA, 
						position = position_nudge(x = 0),
						fill = NA
					) +
					geom_hline(yintercept = 1, size=0.4, linetype='dashed') +
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
					
				imprecisePanel <- ggplot(tallImprecise, aes(x=numAdmin, y=resp)) +
					geom_point(data=tallImprecise, color=pointCol, size=pointSize) +					
					geom_polygon(data=boundaryImprecise, mapping=aes(x=numAdmin, y=boundary), fill=impreciseFill) +
					geom_line(data=middleImprecise, mapping=aes(x=numAdmin, y=middle), col=impreciseLineCol, size=medianSize) +
					geom_hline(yintercept = 1, size=0.4, linetype='dashed') +
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
				if (length(panels) == 2) comboPanel <- comboPanel +
					geom_text(aes(x=0, y=1, label='Estimated / errorless EOO', angle=90, vjust=-1, size=axisSizeY))

				panels[[length(panels) + 1]] <- comboPanel
				names(panels)[length(panels)] <- paste0('errorless', thisNumErrorless, '_precise', thisNumPrecise)
				
			} # next number of precise
			
		} # next number of errorless

		eoo <- (panels[[1]] + panels[[2]]) / (panels[[3]] + panels[[4]]) +
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
		errorless <- 'errorlessNicheBreadthMat_090_degC'
		precise <- 'preciseNicheBreadthMat_090_degC'
		imprecise <- 'impreciseNicheBreadthMat_090_degC'
		
		ylab <- 'Estimated / errorless MAT niche breadth'
		
		### y-axis limits
		these <- c(
			results[ , imprecise] / results[ , errorless],
			results[ , precise] / results[ , errorless]
		)
		ymin <- min(0, these)
		ymax <- quantile(these, outlierQuant)
		ybreaks <- pretty(c(ymin, ymax), 3)
		
		trends <- data.frame()
		panels <- list()
		for (thisNumErrorless in numErrorlessSelected) {
		
			# subset number of precise so it's < total errorless
			allowableNumPreciseSelected <- numPreciseSelected[numPreciseSelected < thisNumErrorless]

			# x-axis limits
			these <- results[results$numErrorless == thisNumErrorless, ]
			xmax <- 1.015 * max(these$numAdmin)
			xmin <- 1 - 0.015 * xmax

			for (thisNumPrecise in allowableNumPreciseSelected) {

				say(subject, ' errorless ', thisNumErrorless, ' precise ', thisNumPrecise)
				count <- count + 1

				# tall imprecise data
				thisResults <- results[results$numErrorless == thisNumErrorless & results$numPrecise == thisNumPrecise, ]
				resp <- thisResults[ , imprecise] / thisResults[ , errorless]
				n <- nrow(thisResults)
				tallImprecise <- data.frame(
					numAdmin = thisResults$numAdmin,
					resp = resp
				)
				
				# calculate quantiles for regression: imprecise
				tallImprecise <- tallImprecise[order(tallImprecise$numAdmin), ]
				k <- if (length(unique(tallImprecise$numAdmin)) <= 6) { 1 } else { -1 }
				middleQuant <- qgam(log(resp) ~ s(numAdmin, bs='cr', k=k), data=tallImprecise, qu=0.5)
				lowerQuant <- qgam(log(resp) ~ s(numAdmin, bs='cr', k=k), data=tallImprecise, qu=lower)
				upperQuant <- qgam(log(resp) ~ s(numAdmin, bs='cr', k=k), data=tallImprecise, qu=upper)
				
				middleQuant <- exp(predict(middleQuant, tallImprecise, type='response'))
				lowerQuant <- exp(predict(lowerQuant, tallImprecise, type='response'))
				upperQuant <- exp(predict(upperQuant, tallImprecise, type='response'))

				middleImprecise <- data.frame(
					numAdmin = tallImprecise$numAdmin,
					middle = middleQuant
				)
				
				boundaryImprecise <- data.frame(
					numAdmin = c(tallImprecise$numAdmin, rev(tallImprecise$numAdmin)),
					boundary = c(lowerQuant, rev(upperQuant))
				)
				
				middleImprecise <- middleImprecise[!duplicated(middleImprecise), ]
				boundaryImprecise <- boundaryImprecise[!duplicated(boundaryImprecise), ]

				# frames for boxplot of precise
				preciseDf <- data.frame(
					numAdmin = 0,
					resp = thisResults[ , precise] / thisResults[ , errorless]
				)
				preciseUpper <- quantile(preciseDf$resp, upper, na.rm=TRUE)
				preciseMiddle <- quantile(preciseDf$resp, 0.5, na.rm=TRUE)
				preciseLower <- quantile(preciseDf$resp, lower, na.rm=TRUE)

				# labels and titles
				main <- paste0(thisNumErrorless, ' errorless ', thisNumPrecise, ' precise')

				boxPanel <- ggplot(tallImprecise, aes(y=resp)) +
					geom_point(
						data=preciseDf,
						mapping=aes(x=0, y=resp),
						position = position_jitter(w = 0.4, h = 0),
						color = precisePointColAlpha,
						size = pointSize
					) +
					geom_boxplot(
						data = preciseDf,
						mapping = aes(y = resp),
						upper = preciseUpper, middle = preciseMiddle, lower = preciseLower,
						coef = 0, outlier.shape = NA, 
						position = position_nudge(x = 0),
						fill = NA
					) +
					geom_hline(yintercept = 1, size=0.4, linetype='dashed') +
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
					
				imprecisePanel <- ggplot(tallImprecise, aes(x=numAdmin, y=resp)) +
					geom_point(data=tallImprecise, color=pointCol, size=pointSize) +					
					geom_polygon(data=boundaryImprecise, mapping=aes(x=numAdmin, y=boundary), fill=impreciseFill) +
					geom_line(data=middleImprecise, mapping=aes(x=numAdmin, y=middle), col=impreciseLineCol, size=medianSize) +
					geom_hline(yintercept = 1, size=0.4, linetype='dashed') +
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
				if (length(panels) == 2) comboPanel <- comboPanel +
					geom_text(aes(x=0, y=1, label='Estimated / errorless niche breath', angle=90, vjust=-1, size=axisSizeY))

				panels[[length(panels) + 1]] <- comboPanel
				names(panels)[length(panels)] <- paste0('errorless', thisNumErrorless, '_precise', thisNumPrecise)
				
			} # next number of precise
			
		} # next number of errorless

		mat <- (panels[[1]] + panels[[2]]) / (panels[[3]] + panels[[4]]) +
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
		errorless <- 'errorlessNicheBreadthTap_090_mm'
		precise <- 'preciseNicheBreadthTap_090_mm'
		imprecise <- 'impreciseNicheBreadthTap_090_mm'
		
		ylab <- 'Estimated / errorless TAP niche breadth'
		
		### y-axis limits
		these <- c(
			results[ , imprecise] / results[ , errorless],
			results[ , precise] / results[ , errorless]
		)
		ymin <- min(0, these)
		ymax <- quantile(these, outlierQuant)
		ybreaks <- pretty(c(ymin, ymax), 3)
		
		trends <- data.frame()
		panels <- list()
		for (thisNumErrorless in numErrorlessSelected) {
		
			# subset number of precise so it's < total errorless
			allowableNumPreciseSelected <- numPreciseSelected[numPreciseSelected < thisNumErrorless]

			# x-axis limits
			these <- results[results$numErrorless == thisNumErrorless, ]
			xmax <- 1.015 * max(these$numAdmin)
			xmin <- 1 - 0.015 * xmax

			for (thisNumPrecise in allowableNumPreciseSelected) {

				say(subject, ' errorless ', thisNumErrorless, ' precise ', thisNumPrecise)
				count <- count + 1

				# tall imprecise data
				thisResults <- results[results$numErrorless == thisNumErrorless & results$numPrecise == thisNumPrecise, ]
				resp <- thisResults[ , imprecise] / thisResults[ , errorless]
				n <- nrow(thisResults)
				tallImprecise <- data.frame(
					numAdmin = thisResults$numAdmin,
					resp = resp
				)
				
				# calculate quantiles for regression: imprecise
				tallImprecise <- tallImprecise[order(tallImprecise$numAdmin), ]
				k <- if (length(unique(tallImprecise$numAdmin)) <= 6) { 1 } else { -1 }
				middleQuant <- qgam(log(resp) ~ s(numAdmin, bs='cr', k=k), data=tallImprecise, qu=0.5)
				lowerQuant <- qgam(log(resp) ~ s(numAdmin, bs='cr', k=k), data=tallImprecise, qu=lower)
				upperQuant <- qgam(log(resp) ~ s(numAdmin, bs='cr', k=k), data=tallImprecise, qu=upper)
				
				middleQuant <- exp(predict(middleQuant, tallImprecise, type='response'))
				lowerQuant <- exp(predict(lowerQuant, tallImprecise, type='response'))
				upperQuant <- exp(predict(upperQuant, tallImprecise, type='response'))

				middleImprecise <- data.frame(
					numAdmin = tallImprecise$numAdmin,
					middle = middleQuant
				)
				
				boundaryImprecise <- data.frame(
					numAdmin = c(tallImprecise$numAdmin, rev(tallImprecise$numAdmin)),
					boundary = c(lowerQuant, rev(upperQuant))
				)
				
				middleImprecise <- middleImprecise[!duplicated(middleImprecise), ]
				boundaryImprecise <- boundaryImprecise[!duplicated(boundaryImprecise), ]

				# frames for boxplot of precise
				preciseDf <- data.frame(
					numAdmin = 0,
					resp = thisResults[ , precise] / thisResults[ , errorless]
				)
				preciseUpper <- quantile(preciseDf$resp, upper, na.rm=TRUE)
				preciseMiddle <- quantile(preciseDf$resp, 0.5, na.rm=TRUE)
				preciseLower <- quantile(preciseDf$resp, lower, na.rm=TRUE)

				# labels and titles
				main <- paste0(thisNumErrorless, ' errorless ', thisNumPrecise, ' precise')

				boxPanel <- ggplot(tallImprecise, aes(y=resp)) +
					geom_point(
						data=preciseDf,
						mapping=aes(x=0, y=resp),
						position = position_jitter(w = 0.4, h = 0),
						color = precisePointColAlpha,
						size = pointSize
					) +
					geom_boxplot(
						data = preciseDf,
						mapping = aes(y = resp),
						upper = preciseUpper, middle = preciseMiddle, lower = preciseLower,
						coef = 0, outlier.shape = NA, 
						position = position_nudge(x = 0),
						fill = NA
					) +
					geom_hline(yintercept = 1, size=0.4, linetype='dashed') +
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
					
				imprecisePanel <- ggplot(tallImprecise, aes(x=numAdmin, y=resp)) +
					geom_point(data=tallImprecise, color=pointCol, size=pointSize) +					
					geom_polygon(data=boundaryImprecise, mapping=aes(x=numAdmin, y=boundary), fill=impreciseFill) +
					geom_line(data=middleImprecise, mapping=aes(x=numAdmin, y=middle), col=impreciseLineCol, size=medianSize) +
					geom_hline(yintercept = 1, size=0.4, linetype='dashed') +
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
				if (length(panels) == 2) comboPanel <- comboPanel +
					geom_text(aes(x=0, y=1, label='Estimated / errorless niche breath', angle=90, vjust=-1, size=axisSizeY))

				panels[[length(panels) + 1]] <- comboPanel
				names(panels)[length(panels)] <- paste0('errorless', thisNumErrorless, '_precise', thisNumPrecise)
				
			} # next number of precise
			
		} # next number of errorless

		tap <- (panels[[1]] + panels[[2]]) / (panels[[3]] + panels[[4]]) +
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
		
		ylab <- 'Estimated / errorless niche volume'
		
		### y-axis limits
		these <- c(
			results[ , imprecise] / results[ , errorless],
			results[ , precise] / results[ , errorless]
		)
		ymin <- min(0, these)
		ymax <- quantile(these, outlierQuant)
		ybreaks <- pretty(c(ymin, ymax), 3)
		
		trends <- data.frame()
		panels <- list()
		for (thisNumErrorless in numErrorlessSelected) {
		
			# subset number of precise so it's < total errorless
			allowableNumPreciseSelected <- numPreciseSelected[numPreciseSelected < thisNumErrorless]

			# x-axis limits
			these <- results[results$numErrorless == thisNumErrorless, ]
			xmax <- 1.015 * max(these$numAdmin)
			xmin <- 1 - 0.015 * xmax

			for (thisNumPrecise in allowableNumPreciseSelected) {

				say(subject, ' errorless ', thisNumErrorless, ' precise ', thisNumPrecise)
				count <- count + 1

				# tall imprecise data
				thisResults <- results[results$numErrorless == thisNumErrorless & results$numPrecise == thisNumPrecise, ]
				resp <- thisResults[ , imprecise] / thisResults[ , errorless]
				n <- nrow(thisResults)
				tallImprecise <- data.frame(
					numAdmin = thisResults$numAdmin,
					resp = resp
				)
				
				# calculate quantiles for regression: imprecise
				tallImprecise <- tallImprecise[order(tallImprecise$numAdmin), ]
				k <- if (length(unique(tallImprecise$numAdmin)) <= 6) { 1 } else { -1 }
				middleQuant <- qgam(log(resp) ~ s(numAdmin, bs='cr', k=k), data=tallImprecise, qu=0.5)
				lowerQuant <- qgam(log(resp) ~ s(numAdmin, bs='cr', k=k), data=tallImprecise, qu=lower)
				upperQuant <- qgam(log(resp) ~ s(numAdmin, bs='cr', k=k), data=tallImprecise, qu=upper)
				
				middleQuant <- exp(predict(middleQuant, tallImprecise, type='response'))
				lowerQuant <- exp(predict(lowerQuant, tallImprecise, type='response'))
				upperQuant <- exp(predict(upperQuant, tallImprecise, type='response'))

				middleImprecise <- data.frame(
					numAdmin = tallImprecise$numAdmin,
					middle = middleQuant
				)
				
				boundaryImprecise <- data.frame(
					numAdmin = c(tallImprecise$numAdmin, rev(tallImprecise$numAdmin)),
					boundary = c(lowerQuant, rev(upperQuant))
				)
				
				middleImprecise <- middleImprecise[!duplicated(middleImprecise), ]
				boundaryImprecise <- boundaryImprecise[!duplicated(boundaryImprecise), ]

				# frames for boxplot of precise
				preciseDf <- data.frame(
					numAdmin = 0,
					resp = thisResults[ , precise] / thisResults[ , errorless]
				)
				preciseUpper <- quantile(preciseDf$resp, upper, na.rm=TRUE)
				preciseMiddle <- quantile(preciseDf$resp, 0.5, na.rm=TRUE)
				preciseLower <- quantile(preciseDf$resp, lower, na.rm=TRUE)

				# labels and titles
				main <- paste0(thisNumErrorless, ' errorless ', thisNumPrecise, ' precise')

				boxPanel <- ggplot(tallImprecise, aes(y=resp)) +
					geom_point(
						data=preciseDf,
						mapping=aes(x=0, y=resp),
						position = position_jitter(w = 0.4, h = 0),
						color = precisePointColAlpha,
						size = pointSize
					) +
					geom_boxplot(
						data = preciseDf,
						mapping = aes(y = resp),
						upper = preciseUpper, middle = preciseMiddle, lower = preciseLower,
						coef = 0, outlier.shape = NA, 
						position = position_nudge(x = 0),
						fill = NA
					) +
					geom_hline(yintercept = 1, size=0.4, linetype='dashed') +
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
					
				imprecisePanel <- ggplot(tallImprecise, aes(x=numAdmin, y=resp)) +
					geom_point(data=tallImprecise, color=pointCol, size=pointSize) +					
					geom_polygon(data=boundaryImprecise, mapping=aes(x=numAdmin, y=boundary), fill=impreciseFill) +
					geom_line(data=middleImprecise, mapping=aes(x=numAdmin, y=middle), col=impreciseLineCol, size=medianSize) +
					geom_hline(yintercept = 1, size=0.4, linetype='dashed') +
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
				if (length(panels) == 2) comboPanel <- comboPanel +
					geom_text(aes(x=0, y=1, label='Estimated / errorless niche volume', angle=90, vjust=-1, size=axisSizeY))

				panels[[length(panels) + 1]] <- comboPanel
				names(panels)[length(panels)] <- paste0('errorless', thisNumErrorless, '_precise', thisNumPrecise)
				
			} # next number of precise
			
		} # next number of errorless

		vol <- (panels[[1]] + panels[[2]]) / (panels[[3]] + panels[[4]]) +
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
		
		ylab <- 'Estimated / errorless niche area'
		
		### y-axis limits
		these <- c(
			results[ , imprecise] / results[ , errorless],
			results[ , precise] / results[ , errorless]
		)
		ymin <- min(0, these)
		ymax <- quantile(these, outlierQuant)
		ybreaks <- pretty(c(ymin, ymax), 3)
		
		trends <- data.frame()
		panels <- list()
		for (thisNumErrorless in numErrorlessSelected) {
		
			# subset number of precise so it's < total errorless
			allowableNumPreciseSelected <- numPreciseSelected[numPreciseSelected < thisNumErrorless]

			# x-axis limits
			these <- results[results$numErrorless == thisNumErrorless, ]
			xmax <- 1.015 * max(these$numAdmin)
			xmin <- 1 - 0.015 * xmax

			for (thisNumPrecise in allowableNumPreciseSelected) {

				say(subject, ' errorless ', thisNumErrorless, ' precise ', thisNumPrecise)
				count <- count + 1

				# tall imprecise data
				thisResults <- results[results$numErrorless == thisNumErrorless & results$numPrecise == thisNumPrecise, ]
				resp <- thisResults[ , imprecise] / thisResults[ , errorless]
				n <- nrow(thisResults)
				tallImprecise <- data.frame(
					numAdmin = thisResults$numAdmin,
					resp = resp
				)
				
				# calculate quantiles for regression: imprecise
				tallImprecise <- tallImprecise[order(tallImprecise$numAdmin), ]
				k <- if (length(unique(tallImprecise$numAdmin)) <= 6) { 1 } else { -1 }
				middleQuant <- qgam(log(resp) ~ s(numAdmin, bs='cr', k=k), data=tallImprecise, qu=0.5)
				lowerQuant <- qgam(log(resp) ~ s(numAdmin, bs='cr', k=k), data=tallImprecise, qu=lower)
				upperQuant <- qgam(log(resp) ~ s(numAdmin, bs='cr', k=k), data=tallImprecise, qu=upper)
				
				middleQuant <- exp(predict(middleQuant, tallImprecise, type='response'))
				lowerQuant <- exp(predict(lowerQuant, tallImprecise, type='response'))
				upperQuant <- exp(predict(upperQuant, tallImprecise, type='response'))

				middleImprecise <- data.frame(
					numAdmin = tallImprecise$numAdmin,
					middle = middleQuant
				)
				
				boundaryImprecise <- data.frame(
					numAdmin = c(tallImprecise$numAdmin, rev(tallImprecise$numAdmin)),
					boundary = c(lowerQuant, rev(upperQuant))
				)
				
				middleImprecise <- middleImprecise[!duplicated(middleImprecise), ]
				boundaryImprecise <- boundaryImprecise[!duplicated(boundaryImprecise), ]

				# frames for boxplot of precise
				preciseDf <- data.frame(
					numAdmin = 0,
					resp = thisResults[ , precise] / thisResults[ , errorless]
				)
				preciseUpper <- quantile(preciseDf$resp, upper, na.rm=TRUE)
				preciseMiddle <- quantile(preciseDf$resp, 0.5, na.rm=TRUE)
				preciseLower <- quantile(preciseDf$resp, lower, na.rm=TRUE)

				# labels and titles
				main <- paste0(thisNumErrorless, ' errorless ', thisNumPrecise, ' precise')

				boxPanel <- ggplot(tallImprecise, aes(y=resp)) +
					geom_point(
						data=preciseDf,
						mapping=aes(x=0, y=resp),
						position = position_jitter(w = 0.4, h = 0),
						color = precisePointColAlpha,
						size = pointSize
					) +
					geom_boxplot(
						data = preciseDf,
						mapping = aes(y = resp),
						upper = preciseUpper, middle = preciseMiddle, lower = preciseLower,
						coef = 0, outlier.shape = NA, 
						position = position_nudge(x = 0),
						fill = NA
					) +
					geom_hline(yintercept = 1, size=0.4, linetype='dashed') +
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
					
				imprecisePanel <- ggplot(tallImprecise, aes(x=numAdmin, y=resp)) +
					geom_point(data=tallImprecise, color=pointCol, size=pointSize) +					
					geom_polygon(data=boundaryImprecise, mapping=aes(x=numAdmin, y=boundary), fill=impreciseFill) +
					geom_line(data=middleImprecise, mapping=aes(x=numAdmin, y=middle), col=impreciseLineCol, size=medianSize) +
					geom_hline(yintercept = 1, size=0.4, linetype='dashed') +
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
				if (length(panels) == 2) comboPanel <- comboPanel +
					geom_text(aes(x=0, y=1, label='Estimated / errorless niche area', angle=90, vjust=-1, size=axisSizeY))

				panels[[length(panels) + 1]] <- comboPanel
				names(panels)[length(panels)] <- paste0('errorless', thisNumErrorless, '_precise', thisNumPrecise)
				
			} # next number of precise
			
		} # next number of errorless

		area <- (panels[[1]] + panels[[2]]) / (panels[[3]] + panels[[4]]) +
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
		
		ggsave(plot=main, filename=paste0('./Analysis/Virtual Species/!Selected EOO & Niche Breadth ', paste(numErrorlessSelected, collapse=' '), ' Errorless & ', paste(numPreciseSelected, collapse=' '), ' Precise.pdf'), width=8.5, height=11, units='in')
		
		say('Displaying only lower ', outlierQuant, 'quantile of points.', level=2, deco='!')
		
