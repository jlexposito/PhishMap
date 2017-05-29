# Package dependencies:

# install.packages("jsonlite")
# install.packages("rworldmap")
# install.packages("RColorBrewer")
# install.packages("RCurl")
# install.packages("ggplot2")
# install.packages("dplyr")

library(jsonlite)
library(rworldmap)
library(RColorBrewer)
library(RCurl)
library(ggplot2)
library(dplyr)
library(stringr)

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
ip_tables <- as.data.frame(table(IP), stringsAsFactors = F)
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
                              geom_bar(stat="identity", position = 'dodge') + theme_grey() +
                              geom_text(aes(label=Freq), position=position_dodge(width=1), vjust=-0.6)
                              
top_ten_countries_bar_plot + scale_fill_brewer(palette="RdBu", 
                                               name = "Country code \n(ISO2) ",
                                               type="seq")

#### Custom functions

ip2long <- function(ip) {
  # transforma a vector de characters
  ips <- unlist(strsplit(ip, '.', fixed = TRUE))
  # set up a function to bit-shift, then "OR" the octets
  octet <- function(x,y) bitops::bitOr(bitops::bitShiftL(x, 8), y)
  # Reduce applys a function cumulatively left to right
  return(Reduce(octet, as.integer(ips)))
}

find.country <- function(ip) {
  return(dplyr::filter(ip2country, ip >= block_start_long & ip <= block_end_long)$country)
}

#### IP blocks by country, (https://db-ip.com/db/download/country)
ip2countries.url <-  "http://download.db-ip.com/free/dbip-country-2017-05.csv.gz"
download.file(url = ip2countries.url, destfile = "./countries.csv")
ip2country <- read.csv("./countries.csv", header = F, stringsAsFactors = F)
names(ip2country) <- c("block_start", "block_end", "country")
separate_ip4 <- str_match(ip2country$block_start, "(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)(?:[.]|$)){4}")

#### extract ip's & compute numeric equivalent
ip_tables$ip_long <- sapply(X = ip_tables$IP, ip2long)
ip_tables.filtered <- dplyr::filter(ip_tables, !is.na(ip_long))

#### compute numeric equivalent for ip blocks
ip2country$block_start_long <- sapply(X = ip2country$block_start, FUN = ip2long)
ip2country$block_end_long   <- sapply(X = ip2country$block_end, FUN = ip2long)

ip2country <- dplyr::filter(ip2country, !is.na(block_start_long))
ip2country <- dplyr::filter(ip2country, !is.na(block_end_long))


# Compute Aggregates -----------------------------------------------------------

ssl.blacklist$country <- sapply(X = ssl.blacklist$ip_long, FUN = find.country)

unique(ssl.blacklist$country)