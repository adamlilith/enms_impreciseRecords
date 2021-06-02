# source('E:/Ecology/Drive/Research/Vaguely Georeferenced Specimen Records/Code/TEMP.r')

			file <- paste0('./Analysis/ENMs/2021-02-04 Feb 4 results from Stephen/Maps of Climate Change Exposure for RCP', rcp, '.png')
			png(file, width=1920, height=1080, res=120)

			par(mfrow=c(2, 5), oma=c(1, 1, 3, 1))
				
			for (background in backgrounds) {
						
				bgNice <- if (background == 'convexHull') {
					'buffered MCP models'
				} else if (background == 'buffers') {
					'buffered points models'
				}
				
				plots <- list()
			
				for (assignMethod in assignMethods) {
				
					say(species, ' ', region, ' ', rcp, ' ', background, ' ', assignMethod)
					
					### ENM output from model using precise + imprecise records
					modelCount <- if (background == 'convexHull') {
						if (assignMethod == 'accurate') {
							1
						} else if (assignMethod == 'centroid') {
							2
						} else if (assignMethod == 'mean') {
							3
						} else if (assignMethod == 'closest') {
							4
						} else if (assignMethod == 'farthest') {
							5
						}
					} else if (background == 'buffers') {
						if (assignMethod == 'accurate') {
							6
						} else if (assignMethod == 'centroid') {
							7
						} else if (assignMethod == 'mean') {
							8
						} else if (assignMethod == 'closest') {
							9
						} else if (assignMethod == 'farthest') {
							10
						}
					}
					
					### raster and points
					current <- currentEnms[[modelCount]][[countSpecies]]
					fut <- futEnms[[modelCount]][[countSpecies]]
					
					current <- rast(current)						
					fut <- rast(fut)						
					
					mask <- terra::rasterize(buffMcpAccsInaccsAdminVectUnproj, current)
					mask <- crop(mask, buffMcpAccsInaccsAdminVectUnproj)
					
					current <- crop(current, mask)
					fut <- crop(fut, mask)
					
					current <- current * mask
					fut <- fut * mask
					
					currentNA <- app(current, function(x) ifelse(x==0, NA, x))
					futNA <- app(fut, function(x) ifelse(x==0, NA, x))
					
					delta <- fut - current
					sumNA <- futNA + currentNA
					
					stable <- sumNA * 0 + 1
					gain <- app(delta, function(x) ifelse(x == 1, 1, NA))
					loss <- app(delta, function(x) ifelse(x == -1, 1, NA))
					
					admins <- crop(nam1VectSp, mask)
					
					### plot
					main <- paste(assignMethod, '|', bgNice)
					plot(stable, col=NA, legend=FALSE, main=main)
					plot(inaccsAdminVectUnproj, border='orange', col=alpha('orange', 0.25), lwd=1.4, add=TRUE)
					plot(stable, col='gray45', legend=FALSE, add=TRUE)
					plot(gain, col='forestgreen', legend=FALSE, add=TRUE)
					plot(loss, col='firebrick3', legend=FALSE, add=TRUE)
					
					plot(admins, border='gray', add=TRUE)

					points(accsVectUnproj, pch=16, col='orange')
					plot(inaccsAdminVectUnproj, border='orange', lwd=1.4, add=TRUE)

					plot(mcpAccsVectUnproj, lwd=1.8, border='blue', add=TRUE)
					plot(buffMcpAccsVectUnproj, lwd=1.8, border='blue', add=TRUE)
					plot(mcpAccsInaccsAdminVectUnproj, lwd=1.8, border='blue', add=TRUE)
					plot(buffMcpAccsInaccsAdminVectUnproj, lwd=1.8, border='blue', add=TRUE)

					legend('bottomleft', legend=c('stable', 'gain', 'loss', 'occurrence', 'region'), fill=c('gray45', 'forestgreen', 'firebrick3', alpha('orange', 0.25), NA), col=c(NA, NA, NA, 'orange', 'blue'), border=c(NA, NA, NA, 'orange', NA), lwd=c(NA, NA, NA, NA, 1.8), pch=c(NA, NA, NA, 16, NA), bty='n', cex=1.2)
				
				} # assign methods
				
			} # backgrounds
			
			main=paste0(species, ' | RCP ', rcp / 10)
			title(main, outer=TRUE, line=1, cex.main=1.8)
			
			dev.off()
		
