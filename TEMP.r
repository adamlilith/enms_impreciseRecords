# source('E:/Ecology/Drive/Research/Vaguely Georeferenced Specimen Records/Code/TEMP.r')

	p <- ggplot(df, aes(x=log10(preciseEoo_km2), fill=species)) +
		geom_histogram(aes(y = 0.25 * ..density..), binwidth=0.25, color='black', position='identity') +
		scale_fill_manual(values=c(alpha('firebrick3', 0.4), alpha('darkblue', 0.4))) +
		ylab('Density') + xlab('EOO of Precise Occurrences (km2)')
	
	print(p)

	q <- ggplot(df, aes(x=log10(vagueEoo_km2), fill=species)) +
		geom_histogram(aes(y = 0.25 * ..density..), binwidth=0.25, color='black', position='identity') +
		scale_fill_manual(values=c(alpha('firebrick3', 0.4), alpha('darkblue', 0.4))) +
		ylab('Density') + xlab('EOO of Precise + Imprecise Occurrences (km2)')
	
	print(q)
	