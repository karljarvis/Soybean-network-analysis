## May 20, 2016

## NASA MSU Telecoupling
## Soybean trade spatial network analysis

## Danica Schaffer-Smith
## djs50@duke.edu

## Additional filtering and plots by Karl Jarvis

#############################################################
## Research Questions and potential approaches:
# 1. where are the centers of soy production and consumption? 
# -plotting using geographic coordinates (hallelujah ! See section 7 in the more
# detailed pdf from the workshop)
# - heatmap representation of the network (could be a cool way to look at the
# trends over time (how many times each node was connected to all other nodes)?
# - degree distribution (how many nodes each node is connected to)

# 2. which are the most important nodes in the trade network?
# - highlight paths between nodes of interest
# - highlight all edges going in and out of a node of interest
# - highlight all neighbors for a node of interest

# 3. how has the strength of trade relationships changed over time? 
# - plotting different types of connections on separate graphs,
# but maintaining node position in the plot (could be useful for looking at soy
# vs. meat if we decide to do that)
# - plot animations and interactive plots ! (could be a great way to convey time
# series as supplemental online content)
  
### Other potential tasks:
# - histogram of all weights (trade values) in the network 
# Filter by removing countries with import or export below a threshold


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

# note: if using Rstudio, click on the triangle next to the "1) Data" line to collapse 
# that section of the code. Click on Code > Show Document Outline to jump to other parts
# of the code without collapsing code sections.

# Data: Import, format, and filter -------------------------------------------------------

## Global trade matrix data from FAOSTAT
#trade <- read.csv("C:/Users/djs50/Desktop/Soybeans/data/FAOSTAT_data/Trade_DetailedTradeMatrix_E_All_Data.csv", header = T, stringsAsFactors = F)
#names(trade)
#The pairwise countries of interest are "Reporter.Countries" and "Partner.Countries". 
#The weights for each timestep are contained in the "YXXXX" columns. 

## Country id crosswalk table
## from: http://www.fao.org/countryprofiles/iso3list/en/
soypath = file.path("~/GoogleDriveSUU/telecoupling/Soybean_network_analysis")

source(file.path(soypath, "soy_load_data.R"))

# Do a bit more data cleanup ----------------------------------------------

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
nodes$id <- nodes$iso2

## Later we can also add other attributes from the countries of the world dataset (data by year)
#test2 <- merge(test, cow, by.x = "iso2", by.y = "ISO.3166.1.A2")
#str(test2)# 130 obs!

# Create links dataset for Exports in 1000 US$ (filter the data) ----------


## Filter data: 
## 2013, top 10 exporters, top 5 importers for each of the top 10 exporters
soy.cleaned.dollars <- soy.cleaned[soy.cleaned$Element=="Export Value",]

## 2013
soy.cleaned.2013 <- soy.cleaned.dollars[soy.cleaned.dollars$Year == "2013",]

# links: no filtering
links.2013 <- data.frame(from = soy.cleaned.2013$Reporter.Countries, to = soy.cleaned.2013$Partner.Countries, weights = soy.cleaned.2013$sum)
rownames(links.2013) <- NULL

# Collapse multiple links of the same type between the same two nodes
# by summing their weights, using aggregate() by "from", "to", & "type":
grouped.soy <- group_by(links.2013, from, to)
links.2013 <- dplyr::summarize(grouped.soy, weight = sum(weights))
links.2013 <- links.2013[order(links.2013$from, links.2013$to),]
rownames(links.2013) <- NULL

# remove links with 0 values
links.2013 = links.2013[links.2013$weight != 0,]

# Convert to igraph and plot ----------------------------------------------

## Converting the data to an igraph object:

net <- graph.data.frame(links.2013[c("from", "to")], directed=TRUE)
net <- igraph::set.edge.attribute(net, "weight", value = links.2013$weight)

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

# Cluster countries and plot ----------------------------------------------

#############################################################
### Run the Louvain clustering community detection algorithm

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
# KJ 6/30: 7 groups detected... not sure why the different result...
# KJ 7/6: 15 groups detected. Very different groups than in past times.
# Very inconsistent results. Not sure how useful it is...
write.csv(network.result, file.path(soypath, "network_groups.csv"))
#network.result$name <- as.character(network.result$name)

# may be interesting to figure out how to force it to cluster in different numbers
# most important is to make sure it's defensible at least, but no need
# to make it the "ideal" analysis.


## Plot the membership in groups spatially

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

pdf(file.path(soypath, "figures", "groups_all.pdf"))
mapCountryData(spatial.join, nameColumnToPlot = "group_assignment", catMethod="categorical", colourPalette="diverging")
dev.off()

# Relationships of 2 countries at a time ----------------------------------

# Brazil exports: top 10 importers 
br.df = soy.cleaned.dollars[soy.cleaned.dollars$Reporter.Countries=="Brazil",]
br.sums.df = ddply(br.df, .(Partner.Countries), summarize, total=sum(sum))
br.top10 = as.character(br.sums.df[order(-br.sums.df$total),][1:10,1])
br.top10.df = br.df[br.df$Partner.Countries %in% br.top10,]

br.plot = ggplot(data=br.top10.df, aes(x=Year, y=sum, group=Partner.Countries, color=Partner.Countries)) +
  geom_line() + ggtitle("Brazil soy exports \n top 10 importers") +
  scale_color_brewer(palette="Spectral")
ggsave(file.path(soypath, "Brazil_top10_importers.pdf"), br.plot)

# China imports: top 10 exporters 
ch.df = soy.cleaned.dollars[soy.cleaned.dollars$Partner.Countries=="China",]
ch.sums.df = ddply(ch.df, .(Reporter.Countries), summarize, total=sum(sum))
ch.top10 = as.character(ch.sums.df[order(-ch.sums.df$total),][1:10,1])
ch.top10.df = ch.df[ch.df$Reporter.Countries %in% ch.top10,]

ch.plot = ggplot(data=ch.top10.df, aes(x=Year, y=sum, group=Reporter.Countries, color=Reporter.Countries)) +
  geom_line() + ggtitle("China soy imports \n top 10 exporters") +
  scale_color_brewer(palette="Spectral")
ggsave(file.path(soypath, "China_top10_exporters.pdf"), ch.plot)

# 1. where are the centers of soy production and consumption?  ------------
# -plotting using geographic coordinates (hallelujah ! See section 7 in the more
# detailed pdf from the workshop)
# - heatmap representation of the network (could be a cool way to look at the
# trends over time (how many times each node was connected to all other nodes)?
# - degree distribution (how many nodes each node is connected to)

library("maps")
library("geosphere")

# Plot a map of the world:
map("world", col="grey50", fill=TRUE, bg="black", lwd=0.1)

# Add a point on the map for center of each country:
points(x=spatial.df$LONG, y=spatial.df$LAT, pch=19, 
       cex=0.1, col="orange")

# Generate edge colors: lighter color means greater soy trade
col.1 <- adjustcolor("orange red", alpha=0.4)
col.2 <- adjustcolor("orange", alpha=0.4)
edge.pal <- colorRampPalette(c(col.1, col.2), alpha = TRUE)
edge.col <- edge.pal(100)

# For each connection, we will generate the coordinates of an arc that connects
# its begin and end point, using gcIntermediate() from package 'geosphere'.
# Then we will plot that arc over the map using lines().

# make sure levels are the same for all relevant factors
fromlevs = spatial.df$nodes[!spatial.df$nodes %in% links.2013$from]
tolevs = spatial.df$nodes[!spatial.df$nodes %in% links.2013$to]
levels(links.2013$from) = c(levels(links.2013$from), fromlevs)
levels(links.2013$to) = c(levels(links.2013$to), tolevs)


for(i in 1:nrow(links.2013))  {
  node1 <- spatial.df[spatial.df$nodes == links.2013[i,]$from,]
  node2 <- spatial.df[spatial.df$nodes == links.2013[i,]$to,]

  arc <- gcIntermediate( c(node1[1,]$LONG, node1[1,]$LAT), 
                         c(node2[1,]$LONG, node2[1,]$LAT), 
                         n=1000, addStartEnd=TRUE )
  edge.ind <- round(100*links.2013[i,]$weight / max(links.2013$weight))
  
  lines(arc, col=edge.col[edge.ind], lwd=edge.ind/30)
  
  # node1 <- airports[airports$ID == flights[i,]$Source,]
  # node2 <- airports[airports$ID == flights[i,]$Target,]
  #
  # arc <- gcIntermediate( c(node1[1,]$longitude, node1[1,]$latitude), 
  #                        c(node2[1,]$longitude, node2[1,]$latitude), 
  #                        n=1000, addStartEnd=TRUE )
  # edge.ind <- round(100*flights[i,]$Freq / max(flights$Freq))
  # 
  # lines(arc, col=edge.col[edge.ind], lwd=edge.ind/30)
}







# 2. which are the most important nodes in the trade network? -------------



# 3. how has the strength of trade relationships changed over time? -------



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

