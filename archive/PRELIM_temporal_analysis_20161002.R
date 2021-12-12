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
soypath = "C:/Users/djs50/Documents/telecoupling"
#soypath = "~/GoogleDriveSUU/telecoupling/Soybean_network_analysis/"

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

## load the links
links <- data.frame(from = soy.cleaned.dollars$Reporter.Countries, to = soy.cleaned.dollars$Partner.Countries, weights = soy.cleaned.dollars$sum, year = soy.cleaned.dollars$Year)
head(links)
names(links) <- c("sender", "receiver", "weights", "time")
links$time2 <- links$time+1
head(links)

## add the start and end for each timestep 
links2 <- cbind(links$time, links$time2, links$sender, links$receiver, links$weights)
links.df = data.frame(links2, links$sender, links$receiver)
# 40 = China
# 27 = Brazil
# 208 = United States
china.links = links.df[links.df$links.receiver == "China"|links.df$links.sender == "China",]
brazil.links = links.df[links.df$links.receiver == "Brazil"|links.df$links.sender == "Brazil",]
us.links = links.df[links.df$links.receiver == "United States"|links.df$links.sender == "United States",]

# Find top 5 for each

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




melt(china.links, )
dcast(china.links, links.sender ~ 0, median)

china.links[china.links$links.sender == 'Brazil',]
china.links[china.links$links.sender == 'United States',]

#############################################################################################
## Create the network
library(networkDynamic)
library(network)
library(sna)
library(tsna)
#nd <- networkDynamic(vertex.spells = links[, c()] edge.spells = links2[,c(1,2,3)], create.TEAs = TRUE, edge.TEA.names = c('weight', 'year') ## We should be able to explicitly add vertices and edge attributes (weights) here
nd2 <- networkDynamic(edge.spells = links2, create.TEAs = TRUE, edge.TEA.names = c('weight'))
# won't accept character names for nodes

## Add geographic coordinates attributes to vertices
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
years <- seq(from = 1986, to = 2013, by = 1)

## Various network and node level measures over time!
soyDensity <- sapply(soyNets, network.density)
plot(soyDensity~years, type = 'l', xlab = 'Year', ylab = 'Network Density') # dramatic increase in network density over time!
soyNodeDegree <- sapply(soyNets, degree)
hist(soyNodeDegree) # most nodes in the network are not very connected!
soyNodeBetweenness <- sapply(soyNets, betweenness)
hist(soyNodeBetweenness)
soyConnectedness <- sapply(soyNets, connectedness)
plot(soyConnectedness~years, type = 'l', xlab = 'Year', ylab = 'Network Connectedness') # dramatic increase in network connectedness over time!)
soyEdgeDuration <- edgeDuration(nd2) # years each edge was active
hist(soyEdgeDuration, xlab = 'Duration (years)', ylab = 'Number of Edges', main = 'Edge Duration') # most of the edge connections are of short duration -- network unstable?
which(edgeDuration(nd2,mode='counts')>1)# 2872 edges occured more than one time
which(edgeDuration(nd2,mode='counts')>8) # only one edge (210) occurs in more than 8 timesteps total!
hist(vertexDuration(nd2)) # not very interesting.
soyTiedDuration <- sapply(soyNets, tiedDuration)
hist(tiedDuration(nd2, mode='counts', neighborhood = 'in'), xlab = 'Duration (years)', ylab = 'Number of Ties', main = "Duration of Import Ties") # Who are countries trading with (incoming ties only)?
hist(tiedDuration(nd2, mode='counts',neighborhood = 'out'), xlab = 'Duration (years)', ylab = 'Number of Ties', main = "Duration of Export Ties") # Who are countries trading with (outgoing ties only)?
hist(tiedDuration(nd2, mode='counts')) # tied duration is the total # times each vertex has ties

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
plot(degree(as.network(nd2)),tiedDuration(nd2),main='Aggregate Degree against Connected Duration', xlab = 'Aggregate Degree', ylab = 'Connected Duration')
## There does appear to be a relationship! Nodes that are connected to more trading partners over time also have longer-term connections

## What is the temporal density of the network object? 
## This is a measure of the total regional activity in the network--total time vertices are tied
## by active edges / amount of time they could possibly be tied.
tEdgeDensity(nd2) # On average nodes are active 0.2109406 of the time series.
tEdgeDensity(china.nd)
tEdgeDensity(brazil.nd)
tEdgeDensity(us.nd)
## we could get this due to high activity of a few nodes biasing the result...

## For all the possible dyads in the network, what fraction of time are they actually tied?
tEdgeDensity(nd2,agg.unit = 'dyad') # 0.01246033 is the probability a random pair was connected
tEdgeDensity(china.nd,agg.unit = 'dyad')
tEdgeDensity(brazil.nd,agg.unit = 'dyad')
tEdgeDensity(us.nd,agg.unit = 'dyad')

## How many trade events are there per unit time? 
tEdgeDensity(nd2,mode='event')
tEdgeDensity(china.nd,mode='event')
tEdgeDensity(brazil.nd,mode='event')
tEdgeDensity(us.nd,mode='event')
#[1] 0.00102279 Not many partners are actively trading regularly in our network. 

## Edge duration density - fraction of time observed edges were active
## THIS CODE WOULD ONLY WORK WITH A LIST OF NETWORK DYNAMIC OBJECTS
# edd<-sapply(x,tEdgeDensity)
# plot(edd,main='edge duration density',xaxt='n',xlab='networks')
# text(edd,label=names(edd),pos=4)

## # trade events per unit time? 
# eed<-sapply(nets,tEdgeDensity,mode='event')
# plot(eed,main='edge event density',xaxt='n',xlab='networks')
# text(eed,label=names(eed),pos=4)



#############################################################################################3
## Temporal network tools in statnet: networkDynamic, ndtv, tsna
#http://statnet.csde.washington.edu/workshops/SUNBELT/current/ndtv/ndtv_workshop.html
## Some great viz code here, but might be best suited to a smaller subset of the graph

library(tsna)

## How many edges form at each timestep?
tEdgeFormation(nd2)
plot( tEdgeFormation(nd2) )


## mean degree of China, Brazil, and US
library(ergm)
pdf(file.path(soypath, 'figures', 'mean_degree_us_ch_br.pdf'))
plot(tErgmStats(us.nd,'meandeg'), main='Mean Degree', col='blue')
lines(tErgmStats(brazil.nd,'meandeg'), col='green')
lines(tErgmStats(china.nd,'meandeg'), col='red')
legend('bottomleft', legend=c('USA','Brazil','China'), col=c('blue','green','red'), lty=1, bty='n')
dev.off()


# Pairwise links
br.ch.links = links.df[links.df$links.receiver == "China"&links.df$links.sender == "Brazil",]
br.us.links = links.df[links.df$links.receiver == "Brazil"&links.df$links.sender == "United States",]
us.ch.links = links.df[links.df$links.receiver == "United States"&links.df$links.sender == "China",]

pdf(file.path(soypath, 'figures', 'edge_weights_us_ch_br.pdf'))
plot(br.ch.links$X1, br.ch.links$X5, type='l')
lines(br.us.links$X1, br.us.links$X5, col='red')
lines(us.ch.links$X1, us.ch.links$X5, col='blue')
dev.off()



head(links.df)
ch.br.df

tiedDuration(china.nd)
plot(tEdgeFormation(china.nd))

tEdgeFormation(brazil.nd)
plot(tEdgeFormation(brazil.nd))

tEdgeFormation(us.nd)
plot(tEdgeFormation(us.nd))

tEdgeDensity(china.nd)
