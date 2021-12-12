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
library(plyr)
library(dplyr)

## 1) DATA

## Global trade matrix data from FAOSTAT
#trade <- read.csv("C:/Users/djs50/Desktop/Soybeans/data/FAOSTAT_data/Trade_DetailedTradeMatrix_E_All_Data.csv", header = T, stringsAsFactors = F)
#names(trade)
#The pairwise countries of interest are "Reporter.Countries" and "Partner.Countries". 
#The weights for each timestep are contained in the "YXXXX" columns. 

## Country id crosswalk table
## from: http://www.fao.org/countryprofiles/iso3list/en/
soypath = file.path("~/GoogleDriveNAU/telecoupling/Soybean_network_analysis")
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

# We need to filter the data to look just at soybeans
# Look at data in terms of Export Value in US$
#soy <- trade[which(trade$Item =="Soybeans"),]
#soy <- soy[which(soy$Unit=="1000 US$"),]
#soy <- soy[which(soy$Element=="Export Value"),]
soy1 <- read.csv(file.path(soypath,"data/FAOSTAT_matrix_soybeans.csv"), header = T, stringsAsFactors = F)

# filter columns
soy = soy1[, c(
  # "Reporter.Country.Code",
  "Reporter.Countries",
  # "Partner.Country.Code",
  "Partner.Countries",
  "Element",
  "Year",
  "Unit",
  "Value"
)]

# # Match country codes
# # Doesn't work, because the country code files don't necessarily contain the
# # same countries as the data itself
# codes <- read.table(file.path(soypath,"data/FAO_country_ids_crosswalk.csv"), header = T, stringsAsFactors = F, sep=",", quote="")[c("Short.name","FAOSTAT","ISO3")]
# ex.codes = im.codes = codes
# names(ex.codes) = c("ex.name","ex.code","ex.ISO3")
# names(im.codes) = c("im.name","im.code","im.ISO3")
# 
# names(soy2)[c(1,2)] = c("ex.code","im.code")
# 
# soy3 = merge(im.codes, soy2)
# soy = merge(ex.codes, soy3)
# 
# grep("China",soy$ex.name)
# unique(soy$ex.name)


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

## Make C?te d'Ivoire match
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
countrycode(unique(soy$Reporter.Countries), "country.name","iso2c")
##Convert the country names to the ISO2C standard codes so that we can link other attributes
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
## Filter data: 
## 2013, top 10 exporters, top 5 importers for each of the top 10 exporters
soy.cleaned.dollars <- soy.cleaned[soy.cleaned$Element=="Export Value",]

## 2013
soy.cleaned.2013 <- soy.cleaned.dollars[soy.cleaned.dollars$Year == "2013",]

# # top 10 exporters
# soy.sum = ddply(soy.cleaned.dollars, .(Reporter.Countries), summarize, sum=sum(sum))
# exporters.top10 = soy.sum[order(-soy.sum$sum), "Reporter.Countries"][1:10]
# soy.extop10 = soy.cleaned.2013[soy.cleaned.2013$Reporter.Countries %in% exporters.top10,]
# 
# # top 5 importers for each
# soy.iall = soy.extop10[order(soy.extop10$Reporter.Countries, -soy.extop10$sum),]
# soy.i5 = soy.iall[0,]
# for(i in 1:10)
# {
#   importers = soy.iall[soy.iall$Reporter.Countries %in% exporters.top10[i],]
#   soy.i5 = rbind(soy.i5, importers[1:5,])
# }

# # links: top 5 importers (i5)
# links.2013 <- data.frame(from = soy.i5$Reporter.Countries, to = soy.i5$Partner.Countries, weights = soy.i5$sum)

# links: no filtering
links.2013 <- data.frame(from = soy.cleaned.2013$Reporter.Countries, to = soy.cleaned.2013$Partner.Countries, weights = soy.cleaned.2013$sum)
rownames(links.2013) <- NULL

# Collapse multiple links of the same type between the same two nodes
# by summing their weights, using aggregate() by "from", "to", & "type":
grouped.soy <- group_by(links.2013, from, to)
links.2013 <- dplyr::summarize(grouped.soy, weight = sum(weights))
links.2013 <- links.2013[order(links.2013$from, links.2013$to),]
rownames(links.2013) <- NULL


####################################################
## 2) Converting the data to an igraph object:
net <- graph.data.frame(links.2013[c("from", "to")], directed=TRUE)
net <- set.edge.attribute(net, "weight", value = links.2013$weight)

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

plot(net,				#the graph to be plotted
     layout=layout.fruchterman.reingold,	# the layout method. see the igraph documentation for details
     main='Soy',	#specifies the title
     vertex.label.dist=0.5,			#puts the name labels slightly off the dots
     vertex.frame.color='gray', 		#the color of the border of the dots 
     vertex.label.color='black',		#the color of the name labels
     vertex.label.font=2,			#the font of the name labels
     vertex.label=V(net)$name,		#specifies the lables of the vertices. in this case the 'name' attribute is used
     vertex.label.cex=1,			#specifies the size of the font of the labels. can also be made to vary
     edge.arrow.size=0.4,
     edge.width=log(E(net)$weight)/10
)

#############################################################
## facet by year
# ggplot(data = soy.cleaned.dollars, aes(from_id = Reporter.Countries, to_id = Partner.Countries)) +
#   geom_net(aes(colour= "grey"), linewidth = 0.5, fiteach=TRUE)+
#   scale_colour_brewer(palette="Set2") +
#   facet_wrap(~Year, nrow=5) + theme(legend.position="bottom")


# Run the Louvain clustering community detection algorithm ----------------

# This is only for an undirected graph, so we need to create a new undirected version
net.undir <- graph.data.frame(links.2013[c("from", "to")], directed=FALSE)
net.undir <- set.edge.attribute(net.undir, "weight", value = links.2013$weight)
louvain.result <- cluster_louvain(net.undir) # this will automatically use our supplied weights

# Create data frame with the ids of individuals and their assigned communities
memberships <- membership(louvain.result)
network.result <- as.data.frame(cbind(names(memberships), membership(louvain.result)))
names(network.result) <- c("name", "group_assignment")
network.result # 7 groups detected!
# KJ 6/24: 15 groups with full dataset
write.csv(network.result, file.path(soypath, ))
#network.result$name <- as.character(network.result$name)

# may be interesting to figure out how to force it to cluster in different numbers
# most important is to make sure it's defensible at least, but no need
# to make it the "ideal" analysis.

####################################################
## 3) Make it spatial

plot(net, layout = as.matrix(nodes.coord[,c("x", "y")]), edge.arrow.size=.4,vertex.label=NA)
plot(net, layout = as.matrix(nodes.coord[,c("x", "y")]), edge.arrow.size=.4)
## Impose a layout using actual coordinates of the nodes
nodes.coord <- data.frame(x = nodes$DMS_LONG, y = nodes$DMS_LAT)
# this is not actually using coordinates

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

# Merge in memberships
spatial.data <- merge(spatial.data, network.result, by.x ="nodes", by.y = "name")
#spatial.data@data = data.frame(spatial.data@data, network.result[match(spatial.data[,"nodes.names"], network.result[,"names"]),])

# Plot countries by group membership
# Same but overlaid on world map
library(rworldmap)
plot(getMap())
plot(spatial.data, pch=16, col=spatial.data$group_assignment, add=T)

# rworldmap:
spatial.df = as.data.frame(spatial.data)
spatial.na = na.omit(spatial.data)
spatial.join = joinCountryData2Map(dF = spatial.df, joinCode = "ISO2", nameJoinColumn = "iso2")
mapCountryData(spatial.join, nameColumnToPlot = "group_assignment", catMethod="categorical", colourPalette="diverging")


# Add flows to spatial plots ----------------------------------------------

# add flows to this plot


# Pairwise relationships over time ----------------------------------------

# how to represent directionality of flow? 
#   could just focus on single pairwise relationship, 
#   and plot change over time

# Brazil exports: top 10 importers 
br.df = soy.cleaned.dollars[soy.cleaned.dollars$Reporter.Countries=="Brazil",]
br.sums.df = ddply(br.df, .(Partner.Countries), summarize, total=sum(sum))
br.top10 = as.character(br.sums.df[order(-br.sums.df$total),][1:10,1])
br.top10.df = br.df[br.df$Partner.Countries %in% br.top10,]

ggplot(data=br.top10.df, aes(x=Year, y=sum, group=Partner.Countries, color=Partner.Countries)) +
  geom_line() + ggtitle("Brazil soy exports \n top 10 importers") +
  scale_color_brewer(palette="Spectral")

# China imports: top 10 exporters 
ch.df = soy.cleaned.dollars[soy.cleaned.dollars$Partner.Countries=="China",]
ch.sums.df = ddply(ch.df, .(Reporter.Countries), summarize, total=sum(sum))
ch.top10 = as.character(ch.sums.df[order(-ch.sums.df$total),][1:10,1])
ch.top10.df = ch.df[ch.df$Reporter.Countries %in% ch.top10,]

ggplot(data=ch.top10.df, aes(x=Year, y=sum, group=Reporter.Countries, color=Reporter.Countries)) +
  geom_line() + ggtitle("China soy imports \n top 10 exporters") +
  scale_color_brewer(palette="Spectral")



# Add other datasets ------------------------------------------------------

# could add on other country-level dataset with GDP, population, etc.
#   could add things like deforestation. 
#   meat production and consumption links to soy, 
#   because most soy is used for feed
#   goal: find global drivers of deforestation

# 2000-2013 estimates of forest loss by country
# Latin america - soy is main driver of deforestation
# Strongest production and consumption partners over time
#   Are countries that are tightly linked - 
#   are they more linked than we'd expect
#   based on geographic distance or other factors?
# Ways of quantifying how countries are affected, focusing on big ones.

# plot time series:
#   panels with networks for each year of data
#   scatter plot of one pair of countries relationship vs time
#   or a scatter plot with a few key relationships in different colors
# -Dominant directionality of flow
# -Which nodes have the highest centrality (most important nodes) in each time
# step?
# -Graph diameter for each timestep (largest distance between a pair of nodes,
# could be geographic or in terms of trade data)
# -Are there connected subgroups or cliques in this trade network?  Are there
# any other country characteristics that would explain the groupings (i.e.,
# GDP)? How strong are groups over time?
# -Are relationships stronger than would be expected based on geographic
# distances between nodes?

