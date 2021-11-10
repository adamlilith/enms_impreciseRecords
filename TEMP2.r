
		dups <- duplicated(quantsPrecise)
		quantsPrecise <- quantsPrecise[!dups, ]

		### plot!
		p <- ggplot(data=tall, aes(x=numAdmin, y=ratio, aes(shape=data, color=data))) +
			geom_point(size=2) +
			# scale_color_manual(values=c('precise'=preciseFill, 'precise + imprecise'=vagueFill)) +
			# scale_shape_manual(values=c('precise'=2, 'precise + imprecise'=1)) +
			# scale_x_log10() +
			# geom_line(data=quants, aes(x=numAdmin, y=middle, color=data), size=2) +
			# geom_line(data=quants, aes(x=numAdmin, y=lower, color=data)) +
			# geom_line(data=quants, aes(x=numAdmin, y=upper, color=data)) +
			geom_polygon(data=quantsPrecise, x=quantsPrecise$numAdmin, y=quantsPrecise$ratio, color='blue', fill='green')
			# ggtitle(paste0(letter, ') ', numPrecise, ' precise occurrences')) + 
			# xlab('Additional imprecise occurrences') +
			# ylab('Ratio of estimated-to-errorless\nextent of occurrence') +
			# theme(
				# text = element_text(size=12),
				# axis.text.x = element_text(angle = 60, vjust = 0.9, hjust=1),
				# legend.position='bottom'
			# )
			
		print(p)
		