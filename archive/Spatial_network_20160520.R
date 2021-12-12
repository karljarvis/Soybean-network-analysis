## May 20, 2016

## NASA MSU Telecoupling
## Soybean trade spatial network analysis

## Danica Schaffer-Smith
## djs50@duke.edu

library(geomnet)
#library(GGally)
library(ggnetwork)
library(network)
library(sna)
library("igraph")
library(ggplot2)
#library(FAOSTAT)
#library(WDI)
library(countrycode)

## 1) DATA

## Global trade matrix data from FAOSTAT
#trade <- read.csv("C:/Users/djs50/Desktop/Soybeans/data/FAOSTAT_data/Trade_DetailedTradeMatrix_E_All_Data.csv", header = T, stringsAsFactors = F)
#names(trade)
#The pairwise countries of interest are "Reporter.Countries" and "Partner.Countries". 
#The weights for each timestep are contained in the "YXXXX" columns. 

## Country id crosswalk table
## from: http://www.fao.org/countryprofiles/iso3list/en/
countries <- read.csv("C:/Users/djs50/Desktop/Soybeans/data/FAO_country_ids_crosswalk.csv", header = T, stringsAsFactors = F)

## World Countries data
## data are from: http://ec2-54-201-183-195.us-west-2.compute.amazonaws.com/cow/
cow <- read.csv("C:/Users/djs50/Desktop/Soybeans/data/cow.csv", header = T, stringsAsFactors = F)
head(cow)

## World Countries xy coordinates
## data area from: https://developers.google.com/public-data/docs/canonical/countries_csv
#coords <- read.csv("C:/Users/djs50/Desktop/Soybeans/data/cow_centroids.csv", header = T, stringsAsFactors = F)
coords <- read.csv("C:/Users/djs50/Desktop/Soybeans/data/country_centroids_all.csv", header = T, stringsAsFactors = F)
head(coords)
#coords <- merge(coords, countries, by.x = "ISO3136", by.y = "ISO2")
#head(coords)

library(dplyr)

# We need to filter the data to look just at soybeans
# Look at data in terms of Export Value in US$
#soy <- trade[which(trade$Item =="Soybeans"),]
#soy <- soy[which(soy$Unit=="1000 US$"),]
#soy <- soy[which(soy$Element=="Export Value"),]
soy <- read.csv("C:/Users/djs50/Desktop/Soybeans/data/FAOSTAT_matrix_soybeans.csv", header = T, stringsAsFactors = F)


##################################################################################
## We need to fix multiple mismatches for the country names
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

## Make Côte d'Ivoire match
soy$Reporter.Countries[soy$Reporter.Countries=="Côte d'Ivoire"] <- "Cote d'Ivoire" 
soy$Partner.Countries[soy$Partner.Countries=="Côte d'Ivoire"] <- "Cote d'Ivoire" 

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

## Make Réunion Match
soy$Reporter.Countries[soy$Reporter.Countries=="Réunion"] <- "Reunion"
soy$Partner.Countries[soy$Partner.Countries=="Réunion"] <- "Reunion"

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

###############################
## Do a bit more data cleanup
## Group data by country names, Export units and Year
library(reshape2)
grouped <- group_by(soy, Reporter.Countries, Partner.Countries, Element, Year)
soy.cleaned <- summarize(grouped, sum = sum(Value))

length(unique(soy.cleaned$Reporter.Countries)) # 126 reporting countries (exporting)
length(unique(soy.cleaned$Partner.Countries)) # 221 partners (importers)

###########################
## Create the node list
nodes1 <- soy.cleaned$Reporter.Countries
nodes2 <- soy.cleaned$Partner.Countries
nodes <- c(nodes1, nodes2)
nodes <- unique(nodes)
str(nodes) #221 unique countries
#nodes <- as.data.frame(nodes)
#names(nodes) <- "names"

##Convert the country names to the ISO2C standard codes so that we can link other attributes
library(countrycode)
iso2 <- countrycode(nodes, "country.name", "iso2c", warn = T) # we lost the Pacific Islands Trust Territory!
## bind this to the nodes df
nodes <- data.frame(nodes, iso2, stringsAsFactors = F)

## Add coordinate attributes 
test <- merge(nodes, coords, by.x = "iso2", by.y = "ISO3136")
str(test) # 217 countries. Some lost due to mismatched names. 
nodes <- test
#nodes$id <- nodes$iso2

## Later we can also add other attributes from the countries of the world dataset (data by year)
#test2 <- merge(test, cow, by.x = "iso2", by.y = "ISO.3166.1.A2")
#str(test2)# 130 obs!


#######################################################
## Create links dataset for Exports in 1000 US$ (filter the data)
soy.cleaned.dollars <- soy.cleaned[soy.cleaned$Element=="Export Value",]
## Let's look just at 1986!
soy.cleaned.1986 <- soy.cleaned.dollars[soy.cleaned.dollars$Year == "1986",]
links.1986 <- data.frame(from = soy.cleaned.1986$Reporter.Countries, to = soy.cleaned.1986$Partner.Countries, weights = soy.cleaned.1986$sum)
rownames(links.1986) <- NULL

# Collapse multiple links of the same type between the same two nodes
# by summing their weights, using aggregate() by "from", "to", & "type":
grouped.soy <- group_by(links.1986, from, to)
links.1986 <- summarize(grouped.soy, weight = sum(weights))
links.1986 <- links.1986[order(links.1986$from, links.1986$to),]
rownames(links.1986) <- NULL

####################################################
## 2) Converting the data to an igraph object:
net <- graph.data.frame(links.1986[c("from", "to")], directed=TRUE)
net <- set.edge.attribute(net, "weight", value = links.1986$weight)

# Examine the resulting object:
class(net)
net 

# It's easy to access nodes, edges, and their attributes:
E(net)
V(net)

# You can also manipulate the network matrix:
net[1,]
net[5,7]

# First attempt to plot the graph:
plot(net) # not pretty!

# Removing loops from the graph:
net <- simplify(net, remove.multiple = F, remove.loops = T) 

# Let's and reduce the arrow size and remove the labels:
plot(net, edge.arrow.size=.4,vertex.label=NA)

list.graph.attributes(net)

#############################################################
## facet by year
ggplot(data = soy.cleaned.dollars, aes(from_id = Reporter.Countries, to_id = Partner.Countries)) +
  geom_net(aes(colour= "grey"), linewidth = 0.5, fiteach=TRUE)+
  scale_colour_brewer(palette="Set2") +
  facet_wrap(~Year, nrow=5) + theme(legend.position="bottom")

#############################################################
# Run the Louvain clustering community detection algorithm
# This is only for an undirected graph, so we need to create a new undirected version
net.undir <- graph.data.frame(links.1986[c("from", "to")], directed=FALSE)
net.undir <- set.edge.attribute(net.undir, "weight", value = links.1986$weight)
louvain.result <- cluster_louvain(net.undir) # this will automatically use our supplied weights

# Create data frame with the ids of individuals and their assigned communities
memberships <- membership(louvain.result)
network.result <- as.data.frame(cbind(names(memberships), membership(louvain.result)))
names(network.result) <- c("name", "group_assignment")
network.result # 7 groups detected!
#network.result$name <- as.character(network.result$name)



####################################################
## 3) Make it spatial
## Impose a layout using actual coordinates of the nodes
nodes.coord <- data.frame(x = nodes$DMS_LONG, y = nodes$DMS_LAT)
plot(net, layout = as.matrix(nodes.coord[,c("x", "y")]), edge.arrow.size=.4,vertex.label=NA)


## Plot the memberships spatially

## once individuals are assigned to communities, making your network analysis spatial is 
## straightforward. Do an attribute join of memberships you created with a spatial* object 
## containing information on the locations of the network nodes. Communities will then be an 
## attribute you can plot with spplot. 
library(sp)

## Spatial graph
spatial.data <- nodes
#names(spatial.data) <- c("x", "y", "name")
coordinates(spatial.data) <- c("LONG", "LAT")
#spatial.data$nodes.names <- as.character(spatial.data$nodes.names)
plot(spatial.data)

## This part isn't working yet... 
# Merge in memberships
spatial.data <- merge(spatial.data, network.result, by.x ="nodes", by.y = "name", type = "left")
#spatial.data@data = data.frame(spatial.data@data, network.result[match(spatial.data[,"nodes.names"], network.result[,"names"]),])
my.palette <- c("red", "blue", "yellow", "purple", "orange", "pink", "green")

spplot(spatial.data, "group_assignment", col.regions = my.palette, cex = 3)



