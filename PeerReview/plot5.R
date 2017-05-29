library(dplyr)
library(ggplot2)

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

if (!exists("SCC")) {
    SCC <- datasetup("SCC")
}

mv_scc <- SCC %>%
    mutate_if(is.factor, as.character) %>%
    filter(Data.Category == "Onroad") %>%
    select(SCC, Short.Name)

mv_data <- NEI %>%
    filter(fips == "24510") %>%
    inner_join(mv_scc) %>%
    select(year, Short.Name, Emissions)

mv_plot <- ggplot(mv_data, aes(year, Emissions)) +
    geom_smooth(method = "lm", se = FALSE) +
    theme_linedraw(base_size = 14) +
    xlab("Years") +
    ylab("PM2.5 Emissions") +
    ggtitle("PM2.5 Emmissions for Motor Vehicles in the city of Baltimore, Maryland") +
    theme(plot.title = element_text(hjust = 0.5))

print(mv_plot)

ggsave("plot5.png", device = png, height = 480, width = 640, limitsize = FALSE)