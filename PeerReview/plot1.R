datasetup <- function(dataset) {
    if (!file.exists("data/summarySCC_PM25.rds") || !file.exists("data/Source_Classification_Code.rds")) {
        # Create a temp file to hold the download
        zipTemp <- tempfile()

        # Download the required zip file into the temp file
        download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip", destfile = zipTemp)

        if (!dir.exists("data")) {
            dir.create("data")
        }

        unzip(zipfile = zipTemp, exdir = "data")

        # Clean up the temp file
        unlink(zipTemp)
        rm(zipTemp)
    }

    if (dataset == "NEI") {
        return(readRDS("data/summarySCC_PM25.rds"))
    }

    if (dataset == "SCC") {
        return(readRDS("data/Source_Classification_Code.rds"))
    }
}

# Environment setup
if (!exists("NEI")) {
    NEI <- datasetup("NEI")
}

# Yearly totals
yearly_totals <- with(NEI, tapply(Emissions, year, sum, na.rm = TRUE))
yearly_model <- data.frame(year = names(yearly_totals), total = yearly_totals)

hist(yearly_model$year, yearly_model$total)