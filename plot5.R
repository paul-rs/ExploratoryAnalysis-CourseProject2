library(ggplot2)

#How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
plot5 <- function() {
   	loadData()

   	data <- exdata_summary %>%
  		filter(fips == "24510") %>%
		merge(classification_data, by.x = "SCC", by.y = "SCC") %>%
		mutate(year = as.factor(year)) %>%
		filter(grepl("Vehicle", Source, ignore.case = T))

	ggplot(data, aes(x = year, y = Emissions, fill = year)) +
	geom_bar(stat = "identity") +
	theme(legend.position = "none") +
	labs(x = "Year", y = "Emissions (tons)") +
	ggtitle("Motor Vehicle Emissions in Baltimore City")
   	ggsave("plot5.png", width = 9.6, height = 4.8, dpi = 100)
}


