# source('E:/Ecology/Drive/Research Active/Vaguely Georeferenced Specimen Records/Code/TEMP.r')

		cols <- colorRampPalette(c('gray40', 'white'))
		cols <- cols(10)

		# MCPs
		errorlessMcpSp <- sp::spTransform(errorlessMcpSpEa, getCRS('wgs84', TRUE))
		preciseMcpSp <- sp::spTransform(preciseMcpSpEa, getCRS('wgs84', TRUE))
		impreciseMcpSp <- sp::spTransform(impreciseMcpSpEa, getCRS('wgs84', TRUE))

		plot(errorlessBuffSp)
		surround <- createPlotPoly(errorlessBuffX2Sp)

		png('./Analysis/Example - Virtual Species with Present-Day Suitability.png', res=600, width=1600, height=1200)

			par(mfrow=c(1, 1), oma=c(1, 1, 1, 1), mar=c(0, 0, 0, 0))

			# present
			plot(sqNicheRast, ext=surround, col=cols, legend=FALSE, axes=FALSE)
			plot(adminSp, border='orange', lwd=0.6, add=TRUE)
			points(errorlessRecsSp, pch=1, lwd=0.4, cex=0.3)
			points(preciseRecsSp, bg='#1b9e77', pch=21, lwd=0.4, cex=0.3)
			points(impreciseErrorlessRecs, bg='orange', lwd=0.4, pch=21, cex=0.3)

			plot(errorlessMcpSp, lty='dotted', lwd=0.6, add=TRUE)
			plot(preciseMcpSp, border='#1b9e77', lwd=0.6, lty='dotted', add=TRUE)
			plot(impreciseMcpSp, border='orange', lwd=0.6, lty='dotted', add=TRUE)
			plot(errorlessBuffSp, lwd=0.6, add=TRUE)
			
			legend(
				'right',
				inset=-0.6,
				xpd=NA,
				bty='n',
				cex=0.36,
				pt.lwd=0.4,
				legend=c(
					'Omniscient occurrence',
					'Omniscient MCP',
					'Precise occurrence',
					'Precise MCP',
					'Imprecise occurrence',
					'County occurrence',
					'Precise/county MCP',
					'Focal region'
				),
				pch=c(
					1,
					NA,
					21,
					NA,
					21,
					NA,
					NA,
					NA
				),
				pt.bg=c(
					NA,
					NA,
					'#1b9e77',
					NA,
					'orange',
					NA,
					NA,
					NA
				),
				col=c(
					'black',
					'black',
					'black',
					'#1b9e77',
					'black',
					'orange',
					'orange',
					'black'
				),
				lty=c(
					NA,
					'dotted',
					NA,
					'dotted',
					NA,
					'solid',
					'dotted',
					'solid'
				),
				lwd=c(
					NA,
					0.6,
					NA,
					0.6,
					NA,
					0.6,
					0.6,
					0.7
				)
				
			)
			
		dev.off()
			
		png('./Analysis/Example - Virtual Species with Future Suitability.png', res=600, width=1600, height=1200)

			par(mfrow=c(1, 1), oma=c(1, 1, 1, 1), mar=c(0, 0, 0, 0))

			# present
			plot(futNicheRast, ext=surround, col=cols, legend=FALSE, axes=FALSE)
			plot(errorlessBuffSp, lwd=0.6, add=TRUE)

		dev.off()
			
		
		png('./Analysis/Example - Virtual Species with Present Suitability from Errorless Model.png', res=600, width=1600, height=1200)

			par(mfrow=c(1, 1), oma=c(1, 1, 1, 1), mar=c(0, 0, 0, 0))

			# present
			plot(futNicheRast, ext=surround, col='gray75', legend=FALSE, axes=FALSE)
			plot(sqPredRast_errorless, col=c('gray65', 'forestgreen'), legend=FALSE, add=TRUE)
			points(errorlessRecsSp, pch=1, lwd=0.4, cex=0.3)
			plot(errorlessBuffSp, lwd=0.6, add=TRUE)
			
		dev.off()
			
		png('./Analysis/Example - Virtual Species with FUTURE Suitability from Errorless Model.png', res=600, width=1600, height=1200)

			par(mfrow=c(1, 1), oma=c(1, 1, 1, 1), mar=c(0, 0, 0, 0))

			# present
			plot(futNicheRast, ext=surround, col='gray75', legend=FALSE, axes=FALSE)
			plot(futPredRast_errorless, col=c('gray65', 'forestgreen'), legend=FALSE, add=TRUE)
			plot(errorlessBuffSp, lwd=0.6, add=TRUE)
			
		dev.off()
			
		png('./Analysis/Example - Virtual Species with PRESENT Suitability from Precise Model.png', res=600, width=1600, height=1200)

			par(mfrow=c(1, 1), oma=c(1, 1, 1, 1), mar=c(0, 0, 0, 0))

			# present
			plot(futNicheRast, ext=surround, col='gray75', legend=FALSE, axes=FALSE)
			plot(sqPredRast_precise, col=c('gray65', 'forestgreen'), legend=FALSE, add=TRUE)
			points(preciseRecsSp, bg='#1b9e77', pch=21, lwd=0.4, cex=0.3)
			plot(errorlessBuffSp, lwd=0.6, add=TRUE)
			
		dev.off()
			
		png('./Analysis/Example - Virtual Species with FUTURE Suitability from Precise Model.png', res=600, width=1600, height=1200)

			par(mfrow=c(1, 1), oma=c(1, 1, 1, 1), mar=c(0, 0, 0, 0))

			# present
			plot(futNicheRast, ext=surround, col='gray75', legend=FALSE, axes=FALSE)
			plot(futPredRast_precise, col=c('gray65', 'forestgreen'), legend=FALSE, add=TRUE)
			plot(errorlessBuffSp, lwd=0.6, add=TRUE)
			
		dev.off()
			
		png('./Analysis/Example - Virtual Species with PRESENT Suitability from Imprecise Model.png', res=600, width=1600, height=1200)

			par(mfrow=c(1, 1), oma=c(1, 1, 1, 1), mar=c(0, 0, 0, 0))

			# present
			plot(futNicheRast, ext=surround, col='gray75', legend=FALSE, axes=FALSE)
			plot(sqPredRast_imprecise, col=c('gray65', 'forestgreen'), legend=FALSE, add=TRUE)
			plot(adminSp, border='orange', lwd=0.6, add=TRUE)
			points(preciseRecsSp, bg='#1b9e77', pch=21, lwd=0.4, cex=0.3)
			plot(errorlessBuffSp, lwd=0.6, add=TRUE)
			
		dev.off()
			
		png('./Analysis/Example - Virtual Species with FUTURE Suitability from Imprecise Model.png', res=600, width=1600, height=1200)

			par(mfrow=c(1, 1), oma=c(1, 1, 1, 1), mar=c(0, 0, 0, 0))

			# present
			plot(futNicheRast, ext=surround, col='gray75', legend=FALSE, axes=FALSE)
			plot(futPredRast_imprecise, col=c('gray65', 'forestgreen'), legend=FALSE, add=TRUE)
			plot(errorlessBuffSp, lwd=0.6, add=TRUE)
			
		dev.off()
