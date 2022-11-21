# source('E:/Ecology/Drive/Research/Vaguely Georeferenced Specimen Records/enms_impreciseRecords/TEMP.r')

x$name <- factor(x$name, levels = x$name)

blue <- c(131, 170, 170)
peach <- c(246, 205, 158)
red <- c(215, 113, 98)

rgbToHex <- function(x) rgb(x[1], x[2], x[3], maxColorValue=255)

graph <- ggplot(data=x, aes(x=name, y=Percent, fill=name)) +
	geom_bar(stat='identity') +
	# scale_fill_brewer(palette='Dark2')
	scale_fill_manual(
		values = c(
			rgbToHex(blue),
			rgbToHex(blue),
			rgbToHex(red),
			rgbToHex(blue),
			rgbToHex(blue),
			rgbToHex(blue)
		)
	) +
	scale_y_continuous(labels=scales::percent) +
	ylab('Percentage of publications') +
	theme(
		legend.position = 'none',
		axis.title.x = element_blank(),
		axis.text.x = element_text(size=12),
		axis.title.y = element_text(size=16),
		axis.text.y = element_text(size=12)
	)
	
ggsave(graph, filename='E:/Ecology/Drive/Research/Vaguely Georeferenced Specimen Records/Analysis/Literature Survey/Studies Reporting Cleaning Methods.pdf')
	
