library(R.utils)
library(data.table)
library(dplyr)
library(ggplot2)

downloadData <- function(destination) {
   	source <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
   	if (!file.exists(destination)) {
   		cat(">>> downloading data from ", source, "...\n", sep = "")
   		download.file(source, destfile = destination, method = "curl")
   	}
}

loadData <- function(directory = ".", unload = FALSE) {
   	destination <- filePath(directory, "exdata.zip")
   	summary_file <- filePath(directory, "summarySCC_PM25.rds")
   	classification_file <- filePath(directory, "Source_Classification_Code.rds")

   	if (unload)
	  		unloadData()

   	if (!file.exists(directory))
	  		dir.create(directory)

   	if (!file.exists(summary_file) || !file.exists(classification_file)) {
   		downloadData(destination)
   		cat(">>> extracting ", destination, " to ", directory, "...\n", sep = "")
   		unzip(destination, exdir = directory)
   	}

   	if (!exists("classification_data") || is.null(classification_data)) {
   		cat(">>> loading classification code data into environment...\n")
   		classification_data <<- readRDS(classification_file) %>%
 		  select(SCC, Data.Category, Short.Name) %>%
 		  rename(Source = Short.Name)
   	}

   	if (!exists("exdata_summary") || is.null(exdata_summary)) {
   		cat(">>> loading summary data into environment...\n")
   		exdata_summary <<- readRDS(summary_file)
   	}
}

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
