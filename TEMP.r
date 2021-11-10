# source('E:/Ecology/Drive/Research/Vaguely Georeferenced Specimen Records/Code/TEMP.r')

		# calculate quantiles for regression
		this <- tall[tall$data == 'precise', ]
		middleQuantPrecise <- qgam(log(ratio) ~ numAdmin, data=this, qu=0.5)
		lowerQuantPrecise <- qgam(log(ratio) ~ numAdmin, data=this, qu=lower)
		upperQuantPrecise <- qgam(log(ratio) ~ numAdmin, data=this, qu=upper)
		
		middleQuantPrecise <- exp(predict(middleQuantPrecise, this, type='response'))
		lowerQuantPrecise <- exp(predict(lowerQuantPrecise, this, type='response'))
		upperQuantPrecise <- exp(predict(upperQuantPrecise, this, type='response'))

		this <- tall[tall$data == 'precise + imprecise', ]
		middleQuantVague <- qgam(log(ratio) ~ numAdmin, data=this, qu=0.5)
		lowerQuantVague <- qgam(log(ratio) ~ numAdmin, data=this, qu=lower)
		upperQuantVague <- qgam(log(ratio) ~ numAdmin, data=this, qu=upper)
		
		middleQuantVague <- exp(predict(middleQuantVague, this, type='response'))
		lowerQuantVague <- exp(predict(lowerQuantVague, this, type='response'))
		upperQuantVague <- exp(predict(upperQuantVague, this, type='response'))

		middlePrecise <- data.frame(
			numAdmin = tall$numAdmin,
			data = rep('precise', length(lowerQuantPrecise)),
			middle = middleQuantPrecise
		)
		
		middleVague <- data.frame(
			numAdmin = tall$numAdmin,
			data = rep('precise', length(lowerQuantVague)),
			middle = middleQuantVague
		)
		
		middlePrecise <- middlePrecise[!duplicated(middlePrecise), ]
		middleVague <- middleVague[!duplicated(middleVague), ]

		order <- order(tall$numAdmin[tall$data=='precise'])
		numAdmin <- thisResults$numAdmin[order]
		quants <- data.frame(
			numAdmin = c(numAdmin, rev(numAdmin), numAdmin, rev(numAdmin)),
			ratio = c(lowerQuantPrecise[order], rev(upperQuantPrecise[order]), lowerQuantVague[order], rev(upperQuantVague[order])),
			data = c(rep('precise', 2 * length(lowerQuantPrecise)), rep('precise + imprecise', 2 * length(lowerQuantVague)))
		)
		
		quants <- quants[!duplicated(quants), ]

		### plot!
		p[[countPrecise]] <- ggplot() +
			geom_polygon(data=quants, mapping=aes(x=numAdmin, y=ratio, fill=data)) +
			geom_point(data=tall, size=2, aes(x=numAdmin, y=ratio, shape=data, color=data)) +
			scale_fill_manual(values=c('precise'=preciseFill, 'precise + imprecise'=vagueFill)) +
			scale_color_manual(values=c('precise'=preciseCol, 'precise + imprecise'=vagueCol)) +
			scale_shape_manual(values=c('precise'=2, 'precise + imprecise'=1)) +
			geom_line(data=middlePrecise, aes(x=numAdmin, y=middle), color='#01665e', size=2) +
			geom_line(data=middleVague, aes(x=numAdmin, y=middle), color='#d73027', size=2) +
			ggtitle(paste0(letter, ') ', numPrecise, ' precise occurrences')) + 
			xlab('Additional imprecise occurrences') +
			ylab('Ratio of estimated-to-errorless\nextent of occurrence') +
			ylim(ylim[1], ylim[2]) +
			theme(
				text = element_text(size=12),
				axis.text.x = element_text(angle = 60, vjust = 0.9, hjust=1),
				legend.position='bottom'
			)
		