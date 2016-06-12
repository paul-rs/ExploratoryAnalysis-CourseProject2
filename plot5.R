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


