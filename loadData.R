library(R.utils)
library(data.table)
library(dplyr)

downloadData <- function(destination) {
	source <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
	if (!file.exists(destination)) {
	   	cat(">>> downloading data from ", source, "...\n", sep = "")
	   	download.file(source, destfile = destination, method = "curl")
	}
}

unloadData <- function() {
	if (exists("exdata_summary"))
		rm(exdata_summary, envir = as.environment(".GlobalEnv"))
	if (exists("classification_data"))
		rm(classification_data, envir = as.environment(".GlobalEnv"))
}

loadData <- function(directory = ".", unload = FALSE) {
	destination			<- filePath(directory, "exdata.zip")
	summary_file		<- filePath(directory, "summarySCC_PM25.rds")
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
