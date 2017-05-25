library(jsonlite)
library(rworldmap)
library(RColorBrewer)
library(RCurl)

API_KEY = "ffe481035f0d5033c16498796ae37d19d62261486c93b9b67d84638fe8d85b28"
source_url = paste("http://data.phishtank.com/data", API_KEY, "online-valid.json.bz2", sep = "/")

download.file("http://data.phishtank.com/data/online-valid.json.bz2",destfile="data.json",method="libcurl")

file = "data.json"
data <- fromJSON(file)$details
countries = unlist(sapply(data, function(data) data$country))
countries_tables <- as.data.frame(table(countries))
spdf <- joinCountryData2Map(countries_tables, joinCode="ISO2", nameJoinColumn="countries", verbose = T)
colorPalette <- RColorBrewer::brewer.pal(n = 7, name = "YlOrRd")
mapCountryData(spdf, mapTitle = "PhishMap", nameColumnToPlot="Freq", catMethod = "logFixedWidth", colourPalette = colorPalette, addLegend = F, lwd = 1.5)
