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
