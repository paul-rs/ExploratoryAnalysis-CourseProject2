library(ggplot2)

#Of the four types of sources indicated by the type(point, nonpoint, onroad, nonroad) variable, 
#which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? 
#Which have seen increases in emissions from 1999–2008? 
#Use the ggplot2 plotting system to make a plot answer this question.
plot3 <- function() {
   	loadData()

	data <- exdata_summary %>% 
		filter(fips == "24510") %>%
		mutate(year = as.factor(year)) %>%
		merge(classification_data, by.x = "SCC", by.y = "SCC") %>%
		filter(Data.Category != "Event") %>%
		group_by(year, Data.Category) %>%
		summarise_each(funs(sum), Emissions)

	ggplot(data, aes(x = year, y = Emissions, color = Data.Category, group = Data.Category)) +
	geom_point() + geom_line() +
	guides(fill = F) +
	facet_grid(. ~ Data.Category) +
	stat_smooth(method = lm, se = F, color = "blue", lty = "dashed", lwd = .5) +
	theme(legend.position = "none") +
	labs(x = "Year", y = "Emissions (tons)") +
	ggtitle("Total PM2.5 Emissions in Baltimore City")
	ggsave("plot3.png", width = 9.6, height = 4.8, dpi = 100)
}
