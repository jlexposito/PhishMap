# Package dependencies:

# install.packages("jsonlite")
# install.packages("rworldmap")
# install.packages("RColorBrewer")
# install.packages("RCurl")
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("parallel")

library(jsonlite)
library(rworldmap)
library(RColorBrewer)
library(RCurl)
library(ggplot2)
library(dplyr)
library(stringr)
library(parallel)
library(maps)

#Download data file
download.file("http://data.phishtank.com/data/online-valid.json.bz2",destfile="data.json",method="libcurl")
file = "data.json"
data.raw <- fromJSON(file)$details
data.dataFrame <- do.call(rbind.data.frame, data.raw)
keep <- c("ip_address", "cidr_block", "country")
data.dataFrame <- data.dataFrame[keep]

#Create Countries data frame
data.dataFrame <- dplyr::filter(data.dataFrame, country != '')
countries <- unlist(data.dataFrame$country)
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

#Create IP data frame
data.dataFrame <- dplyr::filter(data.dataFrame, ip_address != '' )
usa_data <- dplyr::filter(data.dataFrame, country == 'US')

ip_tables <- as.data.frame(usa_data$ip, stringsAsFactors = F)
colnames(ip_tables) <- c("ip")

ip_tables <- as.data.frame(str_match(ip_tables$ip, "(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)(?:[.]|$)){4}"), stringsAsFactors = F)
colnames(ip_tables) <- c("ip")

ip_tables <- dplyr::filter(ip_tables, !is.na(ip))

#Find mot used IP addresses
ip_tables <- count(ip_tables,ip, sort = T)

#### Custom functions
ip2long <- function(ip) {
  # transforma a vector de characters
  ips <- unlist(strsplit(ip, '.', fixed = TRUE))
  # set up a function to bit-shift, then "OR" the octets
  octet <- function(x,y) bitops::bitOr(bitops::bitShiftL(x, 8), y)
  # Reduce applys a function cumulatively left to right
  return(Reduce(octet, as.integer(ips)))
}

find.cityState <- function(ip) {
  t <- dplyr::filter(ip2country, ip >= block_start_long & ip <= block_end_long)
  return ( dplyr::select_(t, .dots = c("city", "state") ) )
}

#### IP blocks by country, (http://download.db-ip.com/free/dbip-city-2017-05.csv.gz)
ip2countries.url <-  "http://download.db-ip.com/free/dbip-city-2017-05.csv.gz"
download.file(url = ip2countries.url, destfile = "./cities.csv")
ip2country <- read.csv("./cities.csv", header = F, stringsAsFactors = F)

names(ip2country) <- c("block_start", "block_end", "country", "state", "city")
ip2country <- dplyr::filter(ip2country, country == "US")

#### extract ip's & compute numeric equivalent
ip_tables$ip_long <- sapply(X = ip_tables$ip, ip2long)

#### compute numeric equivalent for ip blocks
# Calculate the number of cores
no_cores <- detectCores() -1

# Initiate cluster
cl <- makeCluster(no_cores)

#<cache> true
ip2country$block_start_long <- sapply(X = ip2country$block_start, FUN = ip2long)
ip2country <- dplyr::filter(ip2country, !is.na(block_start_long))

# Parallelized: ~3m 30s
ip2country$block_end_long <-  parSapply(cl, ip2country$block_end, FUN = ip2long)
ip2country <- dplyr::filter(ip2country, !is.na(block_end_long))

#</cache> 


# Compute Aggregates -----------------------------------------------------------
clusterExport(cl, "ip2country")

iptables100 <- head(ip_tables, 10)
test <- parSapply(cl, iptables100$ip_long, FUN = find.cityState)
test <- t(test)
colnames(test) <- c("city", "state")
test.df <- as.data.frame(test)

iptables100$city <- test.df$city
iptables100$state <- test.df$state

#ip_tables['city'] <- as.factor(ip_tables$city)
ip_tables <- dplyr::filter(ip_tables, !is.na(city))
iptables100 <- dplyr::filter(iptables100, !is.na(city))

stopCluster(cl)

unique(ip_tables$city)
unique(iptables100$state)
unique(iptables100$city)

# Compute Aggregates -----------------------------------------------------------
ip_tables.aggregate <- ip_tables %>% dplyr::count(ip_tables$city, sort = T)
iptables100.aggregateState <- as.data.frame(table(iptables100$state))


all_states <- map_data("state")
all_states$state <- iptables100.aggregateState$Var1
all_states <- map_data("state")

p <- ggplot()
p <- p + geom_polygon(data=iptables100.aggregateState, aes(x=long, y=lat, group = group, fill=iptables100.aggregateState$Freq),colour="white") + scale_fill_continuous(low = "thistle2", high = "darkred", guide="colorbar")
P1 <- p + theme_bw()  + labs(fill = "Black to White Incarceration Rates \n Weighted by Relative Population" 
                             ,title = "State Incarceration Rates by Race, 2010", x="", y="")
P1 + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())


mapParams <- mapCountryData(sPDF,
                            nameColumnToPlot = "n",
                            catMethod = "categorical",
                            mapTitle = title,
                            colourPalette = colorPalette,
                            addLegend = F,
                            mapRegion = 'Eurasia')
do.call(addMapLegend, c(mapParams))
