# Totals for the city of Baltimore

baltimore_totals <- with(NEI[NEI$fips == "24510",], tapply(Emissions, year, sum, na.rm = TRUE))
baltimore_model <- data.frame(year = names(baltimore_totals), total = baltimore_totals)

plot(baltimore_model$year, baltimore_model$total)