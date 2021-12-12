## August 31, 2016

## NASA MSU Telecoupling
## Soybean trade - Temporal network analysis

## Danica Schaffer-Smith
## djs50@duke.edu

## Temporal Network Analysis using networkDynamic package in R. 
## https://cran.r-project.org/web/packages/networkDynamic/vignettes/networkDynamic.pdf
## http://stackoverflow.com/questions/19672592/dynamic-network-in-r

###############################################################################################
## Prepare the data
## Country id crosswalk table
## from: http://www.fao.org/countryprofiles/iso3list/en/
#soypath = file.path("~/GoogleDriveNAU/telecoupling/Soybean_network_analysis")
soypath = "C:/Users/djs50/Documents/telecoupling"
soypath = "/Users/karl_jarvis/GoogleDriveSUU/telecoupling/Soybean_network_analysis/"
countries <- read.csv(file.path(soypath,"data/FAO_country_ids_crosswalk.csv"), header = T, stringsAsFactors = F)

## World Countries data
## data are from: http://ec2-54-201-183-195.us-west-2.compute.amazonaws.com/cow/
cow <- read.csv(file.path(soypath,"data/cow.csv"), header = T, stringsAsFactors = F)
head(cow)

## World Countries xy coordinates
## data area from: https://developers.google.com/public-data/docs/canonical/countries_csv
#coords <- read.csv("C:/Users/djs50/Desktop/Soybeans/data/cow_centroids.csv", header = T, stringsAsFactors = F)
coords <- read.csv(file.path(soypath,"data/country_centroids_all.csv"), header = T, stringsAsFactors = F)
head(coords)
#coords <- merge(coords, countries, by.x = "ISO3136", by.y = "ISO2")
#head(coords)

## Read in the soybean trade data
soy1 <- read.csv(file.path(soypath,"data/FAOSTAT_matrix_soybeans.csv"), header = T, stringsAsFactors = F)

## Filter columns
soy = soy1[, c("Reporter.Countries","Partner.Countries","Element","Year","Unit","Value")]

## Fix multiple mismatches for the country names ---------------------------
## Combine the Chinas
soy$Reporter.Countries[soy$Reporter.Countries=="China, mainland"] <- "China"
soy$Reporter.Countries[soy$Reporter.Countries=="China, Taiwan Province of"] <- "China"
soy$Reporter.Countries[soy$Reporter.Countries=="China, Hong Kong SAR"] <- "China"
soy$Reporter.Countries[soy$Reporter.Countries=="China, Macao SAR"] <- "China"
soy$Partner.Countries[soy$Partner.Countries=="China, mainland"] <- "China"
soy$Partner.Countries[soy$Partner.Countries=="China, Taiwan Province of"] <- "China"
soy$Partner.Countries[soy$Partner.Countries=="China, Hong Kong SAR"] <- "China"
soy$Partner.Countries[soy$Partner.Countries=="China, Macao SAR"] <- "China"

## Separate Belgium and Belgium-Luxembourg
soy$Reporter.Countries[soy$Reporter.Countries=="Belgium-Luxembourg"] <- "Luxembourg"
soy$Partner.Countries[soy$Partner.Countries=="Belgium-Luxembourg"] <- "Luxembourg"

## Make Virgin Islands match
soy$Reporter.Countries[soy$Reporter.Countries=="British Virgin Islands"] <- "Virgin Islands, British" 
soy$Partner.Countries[soy$Partner.Countries=="British Virgin Islands"] <- "Virgin Islands, British" 

## Make Cote d'Ivoire match
soy$Reporter.Countries[soy$Reporter.Countries=="C?te d'Ivoire"] <- "Cote d'Ivoire" 
soy$Partner.Countries[soy$Partner.Countries=="C?te d'Ivoire"] <- "Cote d'Ivoire" 
soy$Reporter.Countries[soy$Reporter.Countries=="C\xf4te d'Ivoire"] <- "Cote d'Ivoire" 
soy$Partner.Countries[soy$Partner.Countries=="C\xf4te d'Ivoire"] <- "Cote d'Ivoire" 

## Make Congo match
soy$Reporter.Countries[soy$Reporter.Countries=="Democratic Republic of the Congo"] <- "Congo, the Democratic Republic of the" 
soy$Partner.Countries[soy$Partner.Countries=="Democratic Republic of the Congo"] <- "Congo, the Democratic Republic of the"

## Make Ethiopea PDR match
soy$Reporter.Countries[soy$Reporter.Countries=="Ethiopea PDR"] <- "Ethiopea" 
soy$Partner.Countries[soy$Partner.Countries=="Ethiopea PDR"] <- "Ethiopea"

## Make Iran match
soy$Reporter.Countries[soy$Reporter.Countries=="Iran (Islamic Republic of)"] <- "Iran, Islamic Republic of"
soy$Partner.Countries[soy$Partner.Countries=="Iran (Islamic Republic of)"] <- "Iran, Islamic Republic of"

## Make Micronesia match
soy$Reporter.Countries[soy$Reporter.Countries=="Micronesia (Federated States of)"] <- "Micronesia, Federated States of"
soy$Partner.Countries[soy$Partner.Countries=="Micronesia (Federated States of)"] <- "Micronesia, Federated States of"

## Make Korea match
soy$Reporter.Countries[soy$Reporter.Countries=="Republic of Korea"] <- "Korea"
soy$Partner.Countries[soy$Partner.Countries=="Republic of Korea"] <- "Korea"

## Make Republic of Moldova Match
soy$Reporter.Countries[soy$Reporter.Countries=="Republic of Moldova"] <- "Moldova, Republic of"
soy$Partner.Countries[soy$Partner.Countries=="Republic of Moldova"] <- "Moldova, Republic of"

## Make R?union Match
soy$Reporter.Countries[soy$Reporter.Countries=="R?union"] <- "Reunion"
soy$Partner.Countries[soy$Partner.Countries=="R?union"] <- "Reunion"

## Make R\xe9union Match
soy$Reporter.Countries[soy$Reporter.Countries=="R\xe9union"] <- "Reunion"
soy$Partner.Countries[soy$Partner.Countries=="R\xe9union"] <- "Reunion"

## Make United States of America Match
soy$Reporter.Countries[soy$Reporter.Countries=="United States of America"] <- "United States"
soy$Partner.Countries[soy$Partner.Countries == "United States of America"] <- "United States"

## Serbia and Montenegro dissolved in 2006... Split the amounts equally?
sm.report <- soy[which(soy$Reporter.Countries=="Serbia and Montenegro"),]
sm.partner <- soy[which(soy$Partner.Countries=="Serbia and Montenegro"),]
sm <- rbind(sm.report, sm.partner)
## Copy to two new dfs
serbia <- sm
montenegro <- sm
## Give each country 1/2 the trade value
serbia$Value <- 0.5*(sm$Value)
montenegro$Value <- 0.5*(sm$Value)
## Overwrite fields with updated country names
soy$Reporter.Countries[soy$Reporter.Countries=="Serbia and Montenegro"] <- "Serbia"
soy$Partner.Countries[soy$Partner.Countries=="Serbia and Montenegro"] <- "Serbia"
soy$Reporter.Countries[soy$Reporter.Countries=="Serbia and Montenegro"] <- "Montenegro"
soy$Partner.Countries[soy$Partner.Countries=="Serbia and Montenegro"] <- "Montenegro"
## Bind these modified records back to the main df
soy <- rbind(soy, serbia, montenegro)
## remove the oudated records
soy <- soy[!soy$Reporter.Countries=="Serbia and Montenegro",]
soy <- soy[!soy$Partner.Countries=="Serbia and Montenegro",]

## Make Sudan Match
soy$Reporter.Countries[soy$Reporter.Countries=="Sudan(former)"] <- "Sudan"
soy$Partner.Countries[soy$Partner.Countries=="Sudan(former)"] <- "Sudan"

## Make Tanzania Match
soy$Reporter.Countries[soy$Reporter.Countries=="United Republic of Tanzania"] <- "Tanzania, United Republic of"
soy$Partner.Countries[soy$Partner.Countries=="United Republic of Tanzania"] <- "Tanzania, United Republic of"

## Make The former Yugoslav Republic of Macedonia Match
soy$Reporter.Countries[soy$Reporter.Countries=="The former Yugoslav Republic of Macedonia"] <- "Macedonia, the former Yugoslav Republic of"
soy$Partner.Countries[soy$Partner.Countries=="The former Yugoslav Republic of Macedonia"] <- "Macedonia, the former Yugoslav Republic of"

## Make the USSR match (outdated name)
soy$Reporter.Countries[soy$Reporter.Countries=="USSR"] <- "Russian Federation"
soy$Partner.Countries[soy$Partner.Countries=="USSR"] <- "Russian Federation"

## Make Yugoslav SFR Match (outdated name)
soy$Reporter.Countries[soy$Reporter.Countries=="Yugoslav SFR"] <- "Yugoslavia"
soy$Partner.Countries[soy$Partner.Countries=="Yugoslav SFR"] <- "Yugoslavia"

## Shorten Bolivia (not a fan of the "Plurinational State" tag)
soy$Reporter.Countries[grep("Bolivia", soy$Reporter.Countries)] <- "Bolivia"
soy$Partner.Countries[grep("Bolivia", soy$Partner.Countries)] <- "Bolivia"

# Do a bit more data cleanup ----------------------------------------------

## Group data by country names, Export units and Year
library(reshape2)
library(dplyr)
grouped <- group_by(soy, Reporter.Countries, Partner.Countries, Element, Year)
soy.cleaned <- summarize(grouped, sum = sum(Value))

length(unique(soy.cleaned$Reporter.Countries)) # 126 reporting countries (exporting)
length(unique(soy.cleaned$Partner.Countries)) # 221 partners (importers)


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

## load the links
links <- data.frame(from = soy.cleaned.dollars$Reporter.Countries, to = soy.cleaned.dollars$Partner.Countries, weights = soy.cleaned.dollars$sum, year = soy.cleaned.dollars$Year)
head(links)
names(links) <- c("sender", "receiver", "weights", "time")
links$time2 <- links$time+1
head(links)

## add the start and end for each timestep 
links2 <- cbind(links$time, links$time2, links$sender, links$receiver, links$weights)

#############################################################################################
## Create the network
library(networkDynamic)
#nd <- networkDynamic(vertex.spells = links[, c()] edge.spells = links2[,c(1,2,3)], create.TEAs = TRUE, edge.TEA.names = c('weight', 'year') ## We should be able to explicitly add vertices and edge attributes (weights) here
nd2 <- networkDynamic(edge.spells = links2, create.TEAs = TRUE, edge.TEA.names = c('weight'))


## Add geographic coordinates attributes to vertices
set.vertex.attribute(nd2, "lat", nodes$LAT)
set.vertex.attribute(nd2, "lat", nodes$LAT)
set.vertex.attribute(nd2, "lon", nodes$LONG)
## Add identifiers for vertices
set.vertex.attribute(nd2, "id", nodes$nodes)
nd2%v%'lat' ## Access vertex attributes

## Try to add weight attribute to edges
set.edge.attribute(nd2, "weight", links2[,5])

plot(nd2, edge.lwd = 'weight', vertex.col = 'gray') # plot with edge weight (doesn't work well...)

# lapply(get.networks(nd), degree) # degree of each node in the network at each timestep
lapply(get.networks(nd2), network.density) # density of the whole network

## get the static network for every year
soyNets <- get.networks(nd2) # get each static network

## Various network and node level measures over time!
soyDensity <- sapply(soyNets, network.density)
plot(soyDensity, type = 'l', xlab = 'time slice', ylab = 'density') # dramatic increase in network density over time!
soyNodeDegree <- sapply(soyNets, degree)
hist(soyNodeDegree) # most nodes in the network are not very connected!
soyNodeBetweenness <- sapply(soyNets, betweenness)
soyConnectednness <- sapply(soyNets, connectedness)
soyEdgeDuration <- edgeDuration(soyNets) # years each edge was active
hist(soyEdgeDuration) # most of the edge connections are of short duration -- network unstable?
which(edgeDuration(nd2,mode='counts')>1)# 2872 edges occured more than one time
which(edgeDuration(nd2,mode='counts')>8) # only one edge (210) occurs in more than 8 timesteps total!
hist(vertexDuration(nd2))
hist(tiedDuration(nd2, mode='counts')) # tied duration is the total # times each vertex has ties
tiedDuration(nd2, mode='counts',neighborhood = 'in') # Who are countries trading with?

## ratio of export events to import events?
plot(tiedDuration(nd2, mode='counts',neighborhood = 'out'),
     tiedDuration(nd2, mode='counts',neighborhood = 'in'),
     xlab='# export events',ylab='#  import events',main='Exporting vs. Importing' )

## Number of times each vertex is either the sender or the receiver
plot(sort(tiedDuration(nd2)),type='l',
     main='sorted tiedDuration for concurrency scenarios',
     xlab='sorted vertices',ylab='duration that each vertex is connected', col ='#55000033',lwd=4)
mean(tiedDuration(nd2)) ## average number of time a vertex is trading

## Is there a relationship between the number of ties and the length of the ties? 
plot(degree(as.network(nd2)),tiedDuration(nd2),,main='Aggregate Degree against Connected Duration')

## What is the temporal density of the network object? 
## This is a measure of the total regional activity in the network--total time vertices are tied
## by active edges / amount of time they could possibly be tied.
tEdgeDensity(nd2) # On average nodes are active 0.2109406 of the time series.
## we could get this due to high activity of a few nodes biasing the result...

## For all the possible dyads in the network, what fraction of time are they actually tied?
tEdgeDensity(nd2,agg.unit = 'dyad') # 0.01246033 is the probability a random pair was connected

## How many trade events are there per unit time? 
tEdgeDensity(nd2,mode='event')
#[1] 0.00102279 Not many partners are actively trading regularly in our network. 

## Edge duration density - fraction of time observed edges were active
## THIS CODE WOULD ONLY WORK WITH A LIST OF NETWORK DYNAMIC OBJECTS
#edd<-sapply(x,tEdgeDensity)
#plot(edd,main='edge duration density',xaxt='n',xlab='networks')
#text(edd,label=names(edd),pos=4)

## # trade events per unit time? 
eed<-sapply(nets,tEdgeDensity,mode='event')
plot(eed,main='edge event density',xaxt='n',xlab='networks')
text(eed,label=names(eed),pos=4)



#############################################################################################3
## Temporal network tools in statnet: networkDynamic, ndtv, tsna
#http://statnet.csde.washington.edu/workshops/SUNBELT/current/ndtv/ndtv_workshop.html
## Some great viz code here, but might be best suited to a smaller subset of the graph

library(tsna)

## How many edges form at each timestep?
tEdgeFormation(nd2)
plot( tEdgeFormation(nd2) )
