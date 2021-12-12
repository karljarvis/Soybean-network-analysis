## September 2, 2016

## NASA MSU Telecoupling
## Spatial plots 
## Soybean trade network analysis

## Danica Schaffer-Smith
## djs50@duke.edu

## Based on example code from:
## http://skyeome.net/wordpress/?p=866

## There are a 3 steps to a really basic geographic network plot:
  
## 1) Get relational data with appropriate lat and long coordinates for vertices 
## (the hard step!)

## 2) Tell one of the R mapping libraries to plot a map

## 3) Tell the network library to plot the network with the lat & long coordinates, 
## without first erasing the map.

## Load the libraries
library(network)  # basic 'statnet' network library
library(maps)     # basic R map plotting library

#################################################################################################################3
## Apply to the soy data

# Data: Import, format, and filter -------------------------------------------------------

## Global trade matrix data from FAOSTAT
#trade <- read.csv("C:/Users/djs50/Desktop/Soybeans/data/FAOSTAT_data/Trade_DetailedTradeMatrix_E_All_Data.csv", header = T, stringsAsFactors = F)
#names(trade)
#The pairwise countries of interest are "Reporter.Countries" and "Partner.Countries". 
#The weights for each timestep are contained in the "YXXXX" columns. 

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

## drop first column so that names (ids) are the first field
nodes <- nodes[, -1]

## Later we can also add other attributes from the countries of the world dataset (data by year)
#test2 <- merge(test, cow, by.x = "iso2", by.y = "ISO.3166.1.A2")
#str(test2)# 130 obs!

## Create links dataset based on Export Value in 1000 US$ (filter the data)
soy.cleaned.dollars <- soy.cleaned[soy.cleaned$Element=="Export Value",]
links <- data.frame(from = soy.cleaned.dollars$Reporter.Countries, to = soy.cleaned.dollars$Partner.Countries, weights = soy.cleaned.dollars$sum, year = soy.cleaned.dollars$Year)
head(links) ## ADD from.lat, from.lon, to.lat, to.lon
nrow(links) # 16963 links are in this network. 

## Add lat/long information for the from and to nodes of each edge
#links$lat.from <- as.numeric(nodes[match(links$from, nodes$nodes),"latitude"])
#links$lon.from <- as.numeric(nodes[match(links$from, nodes$nodes),"longitude"])
#links$lat.to <- as.numeric(nodes[match(links$to, nodes$nodes),"latitude"])
#links$lon.to <- as.numeric(nodes[match(links$to, nodes$nodes),"longitude"])

## reduce the edges by removing weights == 0 (no trade)
reducedLinks<-links[links$weights>0,] ## only keep edges with a weight greater than 0
head(reducedLinks) 
nrow(reducedLinks) #15325 nodes -- this is still a lot of edges!

## Determine the total export value (USD) of imports and exports from each node by year
links.export <- summarize(group_by(links, from, year), totweight = sum(weights))
links.import <- summarize(group_by(links, to, year), totweight = sum(weights))


## ------------------------------------------------
## Load the libraries
library(network)  # basic 'statnet' network library
library(maps)     # basic R map plotting library

## Create the network!
soyNet<-network(reducedLinks[,1:3], # remove the year field from the network input
                 matrix.type='edgelist',
                 directed=TRUE,  # this will be a directed network
                 ignore.eval=FALSE,  # confusingly, this tells it to include edge weights
                 names.eval='weights'  # names for the edge weights
)

## Because we reduced the set of relationships, we also reduced the set of vertices 
## from the full list of countries. So we now need to match up the coordinates in the 
## rawnodes file with the right vertices. For convince, we attach the lat and lon to 
## the network as vertex attributes.

## attach the appropriate lat and long coordinates
## need to subset to the vertices actually in the network
soyNet%v%'lon'<-sapply(network.vertex.names(soyNet),function(name){
  nodes[nodes$nodes==name,]$lon
})
soyNet%v%'lat'<-sapply(network.vertex.names(soyNet),function(name){
  nodes[nodes$nodes==name,]$lat
})

set.edge.attribute(soyNet, 'weight', value = reducedLinks$weights)

## Now we can actually do the plotting of the world map with the map() command, 
## followed by the plot.network() command. Note the the new=FALSE argument that 
## tells it not to erase the map before drawing the network. The coordinates are 
## passed in via the coord= argument as a two-column matrix.

# plot the map for the background
map('world',fill=TRUE,col='#f2f2f2',lwd=0.08)

# plot the network using the geo coordinates
plot.network(soyNet,  # pass in the network
             # don't erase the map before drawing the network
             new=FALSE, 
             # get coordiantes from vertices and pass in as 2-col matrix
             coord=cbind(soyNet%v%'lon',soyNet%v%'lat'), 
             xlim = c(-175, 178),
             ylim = c(-51.75, 72),
             # ---- all the rest of these are optional to make it look nice ------
             # set a semi-transparent edge color
             edge.col='#AA555555',
             # specifiy an edge width scaled as fraction of total co-occurence
             edge.lwd=soyNet%e%'weights'/sd, # KJ: this gives me an error
             edge.curve = 0.1,
             # set the vertex size
             vertex.cex=0.5,
             # set a semi transparent vertex color
             vertex.col='#AA555555',
             vertex.border='white',
             # please don't jitter the points around
             jitter=FALSE)
## This looks a bit messy because we have so many edges...


## ---------------------------------------------------------
## Map trade relationships in a single year

## Reduce the edges to only 2013
links.2013 <-reducedLinks[reducedLinks$year==2013, ] ## only keep edges with year == 2013
links.2013 <-links.2013[links.2013$weights > 59854, ] ## only keep stronger connections (mean for 2013 = 59854)
head(links.2013) 
nrow(links.2013) #80 nodes -- this is more manageable
summary(links.2013) # verify that there are no edges with weight = 0 

## Determine the total export value (USD) of imports and exports from each node by year
links.export.2013 <- summarize(group_by(links.2013, from, year), exportUSD = sum(weights))
links.import.2013 <- summarize(group_by(links.2013, to, year), importUSD = sum(weights))
links.trade.2013 <- merge(links.export.2013, links.import.2013, by.x = 'from', by.y = 'to', all = TRUE)
#links.trade.2013[is.na(links.trade.2013)] <- 0

## Create the network!
soyNet2013<-network(links.2013[,1:3], # remove the year field from the network input
                matrix.type='edgelist',
                directed=TRUE,  # this will be a directed network
                ignore.eval=FALSE,  # confusingly, this tells it to include edge weights
                names.eval='weights'  # names for the edge weights
)

## Because we reduced the set of relationships, we also reduced the set of vertices 
## from the full list of countries. So we now need to match up the coordinates in the 
## rawnodes file with the right vertices. For convince, we attach the lat and lon to 
## the network as vertex attributes.

## attach the appropriate lat and long coordinates
## need to subset to the vertices actually in the network
soyNet2013%v%'lon'<-sapply(network.vertex.names(soyNet2013),function(name){
  nodes[nodes$nodes==name,]$lon
})
soyNet2013%v%'lat'<-sapply(network.vertex.names(soyNet2013),function(name){
  nodes[nodes$nodes==name,]$lat
})

## Attach vertex attributes for import and exports (USD) in 2013
soyNet2013%v%'exportUSD'<-sapply(network.vertex.names(soyNet2013),function(name){
  links.trade.2013[links.trade.2013$from == name, ]$exportUSD
})
soyNet2013%v%'importUSD'<-sapply(network.vertex.names(soyNet2013),function(name){
  links.trade.2013[links.trade.2013$from == name, ]$importUSD
})

# compute indegree and outdegree centrality
library(sna)
soyNet2013 %v% "indegree" <- degree(soyNet2013, cmode = "indegree")
soyNet2013 %v% "outdegree" <- degree(soyNet2013, cmode = "outdegree")
soyNet2013 %v% "degree" <- degree(soyNet2013, cmode = "freeman")

## verify that we have the needed attributes
soyNet2013 # vertex: lat, lon; edge: weights, exportUSD, importUSD, degree

## Nice plotting options with ggnetworkmap
## Plotting the network
# create a world map
library(ggplot2)
library(maps)
library(network)
#library(sna)
library(GGally)
#library(ggmap)

world <- fortify(map("world", plot = FALSE, fill = TRUE))
world <- ggplot(world, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group), color = "grey65",
               fill = "#f9f9f9", size = 0.2) +
  coord_fixed()

## Plot the exporter size by export value
ggnetworkmap(world, soyNet2013, size = 10,
             weight = 'exportUSD',
             node.color = 'dodgerblue4',
             node.alpha = 0.5,
             segment.color = NA)  # No segments

## NOT RUN --------------------------------------
## One other method w/ igraph and popgraph
#test <- asIgraph(soyNet2013)
#test <- as.popgraph(test)

## add "latitude" and "longitude" attributes to the vertices
#V(test)$longitude <- V(test)$lon
#V(test)$latitude <- V(test)$lat

#world + geom_nodeset(aes(x = lon, y = lat), test, size = 4)
#world + geom_nodeset(aes(x = Longitude, y = Latitude, size = exportUSD), test) +
#                       labs(title = "Total Soy Exports 2013", size="Export Value\n (1000 USD)")

#world + geom_edgeset(aes(x = Longitude, y = Latitude), test, color = "black", lwd = E(test)$weights*0.0000002, alpha = 0.6, arrow = arrow(length=unit(0.25,"cm")))
## --------------------------------------------------


## Plot the importer size by export value imported
ggnetworkmap(world, soyNet2013, size = 15,
             weight = 'importUSD',
             node.color = 'dark red',
             node.alpha = 0.5,
             segment.color = NA) # No segments

## Now we can actually do the plotting of the world map with the map() command, 
## followed by the plot.network() command. Note the the new=FALSE argument that 
## tells it not to erase the map before drawing the network. The coordinates are 
## passed in via the coord= argument as a two-column matrix.

# plot the map for the background
map('world',fill=TRUE,col='#f2f2f2',lwd=0.08)

# plot the network using the geo coordinates
plot.network(soyNet2013,  # pass in the network
             # don't erase the map before drawing the network
             new=FALSE, 
             # get coordinates from vertices and pass in as 2-col matrix
             coord=cbind(soyNet2013%v%'lon',soyNet2013%v%'lat'), 
             xlim = c(-175, 178),
             ylim = c(-51.75, 72),
             #xlim = c(-100, 100),
             #ylim = c(-45, 45),
             # ---- all the rest of these are optional to make it look nice ------
             # set a semi-transparent edge color
             #edge.col='#AA555555',
             edge.col='#AA666666',
             # specifiy an edge width scaled as fraction of total co-occurence
             edge.lwd=((soyNet2013%e%'weights'/17678271)*2),
             #edge.lwd=0.08,
             edge.curve = 0.2,
             # set the vertex size
             vertex.cex=0.5,
             # set a semi transparent vertex color
             vertex.col='#AA555555',
             vertex.border='black',
             # please don't jitter the points around
             jitter=FALSE)

 
## Plot the network showing the degree of each node
ggnetworkmap(world, soyNet2013, size = 10,
             weight = degree,
             segment.color = NA)

## Plot connections 
ggnetworkmap(world, soyNet2013, 
             node.group = outdegree, 
             ring.group = indegree, size = 4) + # color symbology on node outlines!
  scale_fill_continuous("Outdegree", high = "red", low = "yellow") + 
  scale_color_continuous(low = "lightgreen", high = "darkgreen")+
  labs(color = "Indegree")

# view global structure
## Note that the great.circles argument does not play nice with customized line width. 
#ggnetworkmap(world, soyNet2013, great.circles = TRUE,
ggnetworkmap(world, soyNet2013,
             segment.size = (soyNet2013%e%"weights"/17678271)*4,
             #segment.size = soyNet2013%e%'exportUSD',
             segment.color = "black",
             segment.alpha = 0.4,
             arrow.size = 0.5)



## -------------------------------------------------------------
## Plot connections with Brazil in 2013

## Reduce the edges to only Brazil connections in 2013
#links.2013.Brazil <-links.2013[links.2013$from == "Brazil", ] ## Brazil exports only
links.2013.Brazil <- links.2013[ which( links.2013$from == "Brazil" | links.2013$to == "Brazil") , ] ## Brazil exports and imports of soybean
nrow(links.2013.Brazil) #19 links (1 import) 
summary(links.2013.Brazil) # verify that there are no edges with weight = 0 

## Create the network!
soyNet2013.B <- network(links.2013.Brazil[,1:3], # remove the year field from the network input
                    matrix.type='edgelist',
                    directed=TRUE,  # this will be a directed network
                    ignore.eval=FALSE,  # confusingly, this tells it to include edge weights
                    names.eval='weights'  # names for the edge weights
)

## Because we reduced the set of relationships, we also reduced the set of vertices 
## from the full list of countries. So we now need to match up the coordinates in the 
## rawnodes file with the right vertices. For convince, we attach the lat and lon to 
## the network as vertex attributes.

## attach the appropriate lat and long coordinates
## need to subset to the vertices actually in the network
soyNet2013.B%v%'lon'<-sapply(network.vertex.names(soyNet2013.B),function(name){
  nodes[nodes$nodes==name,]$lon
})
soyNet2013.B%v%'lat'<-sapply(network.vertex.names(soyNet2013.B),function(name){
  nodes[nodes$nodes==name,]$lat
})

#set.edge.attribute(soyNet2013, 'weight', value = links.2013$weights)

## verify that we have the needed attributes lat, lon verte
soyNet2013.B # vertex: lat, lon; edge: weights

## Now we can actually do the plotting of the world map with the map() command, 
## followed by the plot.network() command. Note the the new=FALSE argument that 
## tells it not to erase the map before drawing the network. The coordinates are 
## passed in via the coord= argument as a two-column matrix.

# plot the map for the background
map('world',fill=TRUE,col='#f2f2f2',lwd=0.08)

# plot the network using the geo coordinates
plot.network(soyNet2013.B,  # pass in the network
             # don't erase the map before drawing the network
             new=FALSE, 
             # get coordiantes from vertices and pass in as 2-col matrix
             coord=cbind(soyNet2013.B%v%'lon',soyNet2013.B%v%'lat'), 
             #xlim = c(-175, 178),
             #ylim = c(-51.75, 72),
             xlim = c(-100, 100),
             ylim = c(-45, 45),
             # ---- all the rest of these are optional to make it look nice ------
             # set a semi-transparent edge color
             #edge.col='#AA555555',
             edge.col='#AA666666',
             # specifiy an edge width scaled as fraction of total co-occurence
             edge.lwd=((soyNet2013.B%e%'weights'/17678271)*2),
             #edge.lwd=0.08,
             edge.curve = 0.1,
             # set the vertex size
             vertex.cex=0.5,
             # set a semi transparent vertex color
             vertex.col='#AA555555',
             vertex.border='black',
             # please don't jitter the points around
             jitter=FALSE)

## Plot Brazil connections using ggnetwork
ggnetworkmap(world, soyNet2013.B, 
             segment.size = (soyNet2013.B%e%"weights"/17678271)*4,
             segment.color = "black",
             segment.alpha = 0.4,
             arrow.size = 0.5)

## -------------------------------------------------------------
## Plot connections with China

## Reduce the edges to only China connections in 2013
links.2013.China <- links.2013[ which(links.2013$from == "China" | links.2013$to == "China") , ] ## keep all connections with China
#links.2013.China <-links.2013[links.2013$to == "China", ] ## China imports only
nrow(links.2013.China) #6 links (1 export to the US) 
summary(links.2013.China) # verify that there are no edges with weight = 0 

## Create the network!
soyNet2013.C <- network(links.2013.China[,1:3], # remove the year field from the network input
                        matrix.type='edgelist',
                        directed=TRUE,  # this will be a directed network
                        ignore.eval=FALSE,  # confusingly, this tells it to include edge weights
                        names.eval='weights'  # names for the edge weights
)

## Because we reduced the set of relationships, we also reduced the set of vertices 
## from the full list of countries. So we now need to match up the coordinates in the 
## rawnodes file with the right vertices. For convince, we attach the lat and lon to 
## the network as vertex attributes.

## attach the appropriate lat and long coordinates
## need to subset to the vertices actually in the network
soyNet2013.C%v%'lon'<-sapply(network.vertex.names(soyNet2013.C),function(name){
  nodes[nodes$nodes==name,]$lon
})
soyNet2013.C%v%'lat'<-sapply(network.vertex.names(soyNet2013.C),function(name){
  nodes[nodes$nodes==name,]$lat
})

#set.edge.attribute(soyNet2013, 'weight', value = links.2013$weights)

## verify that we have the needed attributes lat, lon verte
soyNet2013.C # vertex: lat, lon; edge: weights

## Now we can actually do the plotting of the world map with the map() command, 
## followed by the plot.network() command. Note the the new=FALSE argument that 
## tells it not to erase the map before drawing the network. The coordinates are 
## passed in via the coord= argument as a two-column matrix.

# plot the map for the background
map('world',fill=TRUE,col='#f2f2f2',lwd=0.08)

# plot the network using the geo coordinates
plot.network(soyNet2013.C,  # pass in the network
             # don't erase the map before drawing the network
             new=FALSE, 
             # get coordiantes from vertices and pass in as 2-col matrix
             coord=cbind(soyNet2013.C%v%'lon',soyNet2013.C%v%'lat'), 
             #xlim = c(-175, 178),
             #ylim = c(-51.75, 72),
             xlim = c(-100, 100),
             ylim = c(-45, 45),
             # ---- all the rest of these are optional to make it look nice ------
             # set a semi-transparent edge color
             #edge.col='#AA555555',
             edge.col='#AA666666',
             # specifiy an edge width scaled as fraction of total co-occurence
             edge.lwd=((soyNet2013.C%e%'weights'/17678271)*2),
             #edge.lwd=0.08,
             edge.curve = 0.1,
             # set the vertex size
             vertex.cex=0.5,
             # set a semi transparent vertex color
             vertex.col='#AA555555',
             vertex.border='black',
             # please don't jitter the points around
             jitter=FALSE)

## Plot China connections using ggnetwork
ggnetworkmap(world, soyNet2013.C, 
             segment.size = (soyNet2013.C%e%"weights"/17678271)*4,
             segment.color = "black",
             segment.alpha = 0.4,
             arrow.size = 0.5)

#################################################################
#################################################################
## OLD CODE - TUTORIAL DATA
#################################################################

## Load the libraries
library(network)  # basic 'statnet' network library
library(maps)     # basic R map plotting library

#######################################################
## DATA

## Usually the hardest part of working with geographic data is doing the "geocoding" 
## to assign latitude and longitude to data points (vertices). For this example, 
## we can grab a dataset, courtesy of Katherine Ognyanova, that has already been 
## geocoded from http://kateto.net/countries  This is a dataset of co-occurrences 
## of country names in statements by U.S. Congressmembers.

## The first file contains node-level data about all all of the countries, including 
## the latitude and longitude data we will use.

##  download and parse CSV files
rawnodes<-read.csv('http://www.kateto.net/wordpress/wp-content/uploads/2015/06/Country_terms_FREQ.csv')
names(rawnodes) # print the column names

## The second file contains the pairs of country-country names and information on 
## the number of term co-occurrences. We will use this to create the edgelist.
rawedges<-read.csv('http://www.kateto.net/wordpress/wp-content/uploads/2015/06/Country_terms_COOC.csv')
names(rawedges) # print the column names

nrow(rawedges) # how many edges are there?

## If we plot this whole network with all edges, it will be unitelligible!
## Filter on the Tot_cooc variable to only include relationships between
## terms that have a total co-occurence of > 500. 

# subset to form a smaller matrix with only strong ties
reducedEdgelist<-rawedges[rawedges$Tot_cooc>500,c('Source','Target',"Tot_cooc")]
head(reducedEdgelist) 

nrow(reducedEdgelist) # 80 edges --- much more manageable. 

## 80 relationships seems a lot more workable, so lets convert the edgelist into 
## a network, bringing along the total co-occurrence variable as an edge value attribute.

coocNet<-network(reducedEdgelist,
                 matrix.type='edgelist',
                 directed=FALSE,  # this will be an undirected network
                 ignore.eval=FALSE,  # confusingly, this tells it to include edge weights
                 names.eval='Tot_cooc'  # names for the edge weights
)

## Because we reduced the set of relationships, we also reduced the set of vertices 
## from the full list of countries. So we now need to match up the coordinates in the 
## rawnodes file with the right vertices. For convince, we attach the lat and lon to 
## the network as vertex attributes.

## attach the appropriate lat and long coordinates
## need to subset to the vertices actually in the network
coocNet%v%'lon'<-sapply(network.vertex.names(coocNet),function(name){
  rawnodes[rawnodes$ID==name,]$lon
})

coocNet%v%'lat'<-sapply(network.vertex.names(coocNet),function(name){
  rawnodes[rawnodes$ID==name,]$lat
})

## Now we can actually do the plotting of the world map with the map() command, 
## followed by the plot.network() command. Note the the new=FALSE argument that 
## tells it not to erase the map before drawing the network. The coordinates are 
## passed in via the coord= argument as a two-column matrix.

# plot the map for the background
map('world',fill=TRUE,col='#f2f2f2',lwd=0.08)

# plot the network using the geo coordinates
plot.network(coocNet,  # pass in the network
             # don't erase the map before drawing the network
             new=FALSE, 
             # get coordiantes from vertices and pass in as 2-col matrix
             coord=cbind(coocNet%v%'lon',coocNet%v%'lat'),  
             # ---- all the rest of these are optional to make it look nice ------
             # set a semi-transparent edge color
             edge.col='#AA555555',
             # specifiy an edge width scaled as fraction of total co-occurence
             edge.lwd=coocNet%e%'Tot_cooc'/500,
             # set the vertex size
             vertex.cex=0.5,
             # set a semi transparent vertex color
             vertex.col='#AA555555',
             vertex.border='white',
             # please don't jitter the points around
             jitter=FALSE)

## For this network we see that a lot of the ties involved the US. Not surprising, 
## but may not be the most interesting feature of the data. So might be worthwhile 
## to remove the USA vertex and lower the filtering threshold . but this post is 
## supposed to be just about the mechanics of overlaying a network on a map.

## However, it is often the case that geographic positioning of networks doesn't do 
## a great job of revealing the network structure. The map is essentially providing 
## geographic context and implicit labeling, but if the underlying data is not 
## driven by geographic proximity important details can be obscured. For this example, 
## I think it would be more intepretable as traditional network diagram where vertex 
## positions are determined by network proximity.

# but it is actually far more useful as a regular netowrk
plot.network(coocNet,displaylabels=TRUE,boxed.labels=TRUE,
             vertex.cex=0,
             label.pos=5,
             label.cex=0.6,
             edge.lwd=coocNet%e%'Tot_cooc'/500,
             edge.col='#AA555555',
             mode='kamadakawai')



#######################################################
## Another example with the airport data
## http://stackoverflow.com/questions/33122456/r-creating-a-world-network-map
## http://www.gis-blog.com/flight-connection-map-with-r/

## Here is an example showing flight conncetions from the JKF airport (sample data 
## from the nycflights13 package filtered with dplyr):

library(maps)
library(geosphere)
library(dplyr)
library(nycflights13)

## Filter to only the united states
## Remove JFK and make this a separate df
usairports <- filter(airports, lat < 48.5)
usairports <- filter(usairports, lon > -130)
usairports <- filter(usairports, faa!="JFK")
jfk <- filter(airports, faa=="JFK")

#create basemap
map("world", regions=c("usa"), fill=T, col="grey8", bg="grey15", ylim=c(21.0,50.0), xlim=c(-130.0,-65.0))
#overlay airports
points(usairports$lon,usairports$lat, pch=3, cex=0.1, col="chocolate1")

## The next and final step is to add the flight connectins from NYC to all other airports using the gesophere 
## package and the gcIntermediate() function.

for (i in (1:dim(usairports)[1])) { 
  inter <- gcIntermediate(c(jfk$lon[1], jfk$lat[1]), c(usairports$lon[i], usairports$lat[i]), n=200)
  lines(inter, lwd=0.1, col="turquoise2")    
}
for (i in (1:dim(usairports)[1])) { 
  
  inter <- gcIntermediate(c(jfk$lon[1], jfk$lat[1]), c(usairports$lon[i], usairports$lat[i]), n=200)
  
  lines(inter, lwd=0.1, col="turquoise2")    
}


points(usairports$lon,usairports$lat, pch=3, cex=0.1, col="chocolate1")

