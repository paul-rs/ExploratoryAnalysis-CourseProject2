

#Compare emissions from motor vehicle sources in Baltimore City 
#with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). 
#Which city has seen greater changes over time in motor vehicle emissions?
plot6 <- function() {
   	loadData()

   	data <- exdata_summary %>%
	filter(fips == "24510" | fips == "06037") %>%
  		merge(classification_data, by.x = "SCC", by.y = "SCC") %>%
  		mutate(year = as.factor(year), location = ifelse(fips == "24510", "Baltimore", "Los Angeles")) %>%
  		filter(grepl("Motor Vehicle", Source, ignore.case = T))

	ggplot(data, aes(x = year, y = Emissions, group = location, color = location)) +
	geom_point() + geom_line() +
	stat_smooth(method = lm, se = F, color = "blue", lty = "dashed", lwd = 1) +
   	labs(x = "Year", y = "Emissions (tons)") +
   	ggtitle("Motor Vehicle Emissions in Los Angeles and Baltimore City")
	ggsave("plot6.png", width = 9.6, height = 4.8, dpi = 100)
}
