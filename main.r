library(jsonlite)
library(rworldmap)
library(RColorBrewer)
library(RCurl)
library(ggplot2)

#Download data file
download.file("http://data.phishtank.com/data/online-valid.json.bz2",destfile="data.json",method="libcurl")
file = "data.json"
data <- fromJSON(file)$details

#Create Countries data frame
countries = unlist( sapply(data, function(data) data$country[data$country != ''] ) )
countries_tables <- as.data.frame(table(countries))
countries_tables <- countries_tables[ order(-countries_tables[,2], countries_tables[,1]), ]

#Create IP data frame
IP = unlist( sapply(data, function(data) data$ip_address[data$ip_address != ''] ) )
ip_tables <- as.data.frame(table(IP))
#Find mot used IP addresses
ip_tables <- ip_tables[ order(-ip_tables[,2], ip_tables[,1]), ]

#Generate MapData
map_data <- joinCountryData2Map(countries_tables, joinCode="ISO2", 
                            nameJoinColumn="countries", verbose = T)
#Change MapColorPalette
map_color_palette <- RColorBrewer::brewer.pal(n = 7, name = "YlOrRd")
map_country_data <- mapCountryData(map_data, mapTitle = "PhishMap", nameColumnToPlot="Freq", 
               catMethod = "logFixedWidth", colourPalette = map_color_palette, 
               addLegend = F, lwd = 1, borderCol = 'black')
#Top 10 table
top_ten_countries <- head(countries_tables, 10)

#Top 10 bars graphic
top_ten_countries_bar_plot <- ggplot(
                                data=top_ten_countries, 
                                aes(x=countries, y=Freq, fill=countries)
                              ) +
                              guides(fill=FALSE) +
                              xlab("Countries") + ylab("Phishing sites") +
                              geom_bar(stat="identity") + theme_grey()
                              
top_ten_countries_bar_plot + scale_fill_brewer(palette="RdBu", 
                                               name = "Country code \n(ISO2) ",
                                               type="seq")
