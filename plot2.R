library(ggplot2)

#Have total emissions from PM2.5 decreased in the Baltimore City, Maryland(fips == "24510") from 1999 to 2008 ? 
#Use the base plotting system to make a plot answering this question.
plot2 <- function() {
   	loadData()

   	png(filename = "plot2.png", width = 480, height = 480)
   	data <- exdata_summary %>%
		group_by(year) %>%
		filter(fips == "24510") %>%
  		summarise_each(funs(sum), Emissions) %>%
  		mutate(Emissions = Emissions / 1000)
   	with(data,
	  plot(year, Emissions, ylab = "Emissions (kilotons)", main = "Total PM2.5 Emissions in Baltimore City",
		   pch = 19, type = "o", col = "blue", lty = "dashed"))
   	dev.off()
}
