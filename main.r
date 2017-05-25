library(jsonlite)
library(rworldmap)
library(RColorBrewer)
library(RCurl)

#Download data file
API_KEY = "ffe481035f0d5033c16498796ae37d19d62261486c93b9b67d84638fe8d85b28"
source_url = paste("http://data.phishtank.com/data", API_KEY, "online-valid.json.bz2", sep = "/")
download.file("http://data.phishtank.com/data/online-valid.json.bz2",destfile="data.json",method="libcurl")
file = "data.json"
data <- fromJSON(file)$details

#Create Countries data frame
countries = unlist( sapply(data, function(data) data$country[data$country != ''] ) )
countries_tables <- as.data.frame(table(countries))
countries_tables <- countries_tables[ order(-countries_tables[,2], countries_tables[,1]), ]

#Generate MapData
map_data <- joinCountryData2Map(countries_tables, joinCode="ISO2", 
                            nameJoinColumn="countries", verbose = T)
#Change MapColorPalette
map_color_palette <- RColorBrewer::brewer.pal(n = 7, name = "YlOrRd")
map_country_data <- mapCountryData(map_data, mapTitle = "PhishMap", nameColumnToPlot="Freq", 
               catMethod = "logFixedWidth", colourPalette = map_color_palette, 
               addLegend = F, lwd = 1, borderCol = 'black')
