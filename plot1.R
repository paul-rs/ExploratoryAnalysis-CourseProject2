library(ggplot2)

#Have total emissions from PM2.5 decreased in the United States from 1999 to 2008 ? 
#Using the base plotting system, make a plot showing the total PM2.5 emission
#from all sources for each of the years 1999, 2002, 2005, and 2008.
plot1 <- function() {
	loadData()

	png(filename = "plot1.png", width = 480, height = 480)
	data <- exdata_summary %>%
  		group_by(year) %>%
 		summarise_each(funs(sum), Emissions) %>%
 		mutate(Emissions = Emissions / 1000)
	with(data,
 		plot(year, Emissions, ylab = "Emissions (kilotons)", main = "Total PM2.5 Emissions in United States.",
			 pch = 19, type = "o", col = "blue", lty = "dashed"))
	dev.off()
}
