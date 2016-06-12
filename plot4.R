#Across the United States, how have emissions from coal combustion - related sources changed from 1999 –2008?
plot4 <- function() {
   	loadData()

	coalSCC <- filter(classification_data, grepl("Coal", Source) & grepl("Combustor Types", Source))$SCC
	data <- exdata_summary %>%
		filter(SCC %in% coalSCC) %>%
		merge(classification_data, by.x = "SCC", by.y = "SCC") %>%
  		mutate(year = as.factor(year)) 

	ggplot(data, aes(x = year, y = Emissions)) +
	geom_boxplot() +
	coord_cartesian(ylim = c(-1, 5)) +
	labs(x = "Year", y = "Emissions (tons)") +
	theme(legend.position = "none") +
	ggtitle("Coal Combustion Emissions in the United States")
   	ggsave("plot4.png", width = 9.6, height = 4.8, dpi = 100) 
}
