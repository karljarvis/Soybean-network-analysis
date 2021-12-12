## August 31, 2016

## NASA MSU Telecoupling
## Soybean trade - Temporal network analysis

## Danica Schaffer-Smith
## djs50@duke.edu

## Temporal Network Analysis using networkDynamic package in R. 
## https://cran.r-project.org/web/packages/networkDynamic/vignettes/networkDynamic.pdf
## http://stackoverflow.com/questions/19672592/dynamic-network-in-r

###############################################################################################
## Prepare and load the data
## Country id crosswalk table
## from: http://www.fao.org/countryprofiles/iso3list/en/
#soypath = "C:/Users/djs50/Documents/telecoupling"
#soypath = "~/GoogleDriveSUU/telecoupling/Soybean_network_analysis/"
soypath = "I:/telecoupling"

## Source script loads and formats the data
source(file.path(soypath, "soy_load_data.R"))

#########################################################################
## Prepare network data
#############################################################
## Create the node list
library(countrycode)
nodes1 <- soy.cleaned$Reporter.Countries
nodes2 <- soy.cleaned$Partner.Countries
nodes <- c(nodes1, nodes2)
nodes <- unique(nodes)
str(nodes) #221 unique countries

## Convert the country names to the ISO2C standard codes so that other attributes
## may be linked to the network in future. 
#countrycode(unique(soy$Reporter.Countries), "country.name","iso2c")
iso2 <- countrycode(nodes, "country.name", "iso2c", warn = T) # we lost the Pacific Islands Trust Territory!

## bind this to the nodes df
nodes <- data.frame(nodes, iso2, stringsAsFactors = F)

## Add coordinate attributes for each node (country centroids)
test <- merge(nodes, coords, by.x = "iso2", by.y = "ISO3136")
str(test) # 217 countries. Some lost due to mismatched names.
nodes <- test
nodes$id <- nodes$iso2
nodes$latitude <- nodes$LAT
nodes$longitude <- nodes$LONG

## Later we can also add other attributes from the countries of the world dataset (data by year)
#test2 <- merge(test, cow, by.x = "iso2", by.y = "ISO.3166.1.A2")
#str(test2)# 130 obs!

## Create links dataset based on Export Value in 1000 US$ (filter the data)
soy.cleaned.dollars <- soy.cleaned[soy.cleaned$Element=="Export Value",]
links <- data.frame(from = soy.cleaned.dollars$Reporter.Countries, to = soy.cleaned.dollars$Partner.Countries, weights = soy.cleaned.dollars$sum, year = soy.cleaned.dollars$Year)
head(links)

## Remove links where the sender = receiver 
library(dplyr)
str(links)
links$from <- as.character(links$from)
links$to <- as.character(links$to)
linkcheck <- links %>% filter(from != to)

## load the links
#links <- data.frame(from = soy.cleaned.dollars$Reporter.Countries, to = soy.cleaned.dollars$Partner.Countries, weights = soy.cleaned.dollars$sum, year = soy.cleaned.dollars$Year)
links <- linkcheck
head(links)
names(links) <- c("sender", "receiver", "weights", "time")
links$time2 <- links$time+1
head(links)
