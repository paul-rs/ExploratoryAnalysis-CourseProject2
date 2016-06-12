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
