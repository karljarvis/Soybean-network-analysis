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
# soypath = "C:/Users/djs50/Documents/telecoupling"
soypath = "~/GoogleDriveSUU/telecoupling/Soybean_network_analysis/"

## Source script loads and formats the data
source(file.path(soypath, "soy_load_data.R"))

library(plyr)
library(ggplot2)


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
names(links.df) = c('t1','t2','code1','code2','qty','sender','receiver')
links.df$qtyM = links.df$qty/1000
# 40 = China
# 27 = Brazil
# 208 = United States
ch.links = links.df[links.df$receiver == "China"|links.df$sender == "China",]
br.links = links.df[links.df$receiver == "Brazil"|links.df$sender == "Brazil",]
us.links = links.df[links.df$receiver == "United States"|links.df$sender == "United States",]



# Find top 5 edge weights for each

# Brazil exports: top 5 partners 
br.sums.df = ddply(br.links, .(receiver), summarize, total=sum(qty))
br.top5 = as.character(br.sums.df[order(-br.sums.df$total),][1:5,1])
br.top5.df = br.links[br.links$receiver %in% br.top5,]

# US exports: top 5 partners 
us.sums.df = ddply(us.links, .(receiver), summarize, total=sum(qty))
us.top5 = as.character(us.sums.df[order(-us.sums.df$total),][1:5,1])
us.top5.df = us.links[us.links$receiver %in% us.top5,]

# China imports: top 5 partners 
ch.sums.df = ddply(ch.links, .(sender), summarize, total=sum(qty))
ch.top5 = as.character(ch.sums.df[order(-ch.sums.df$total),][1:6,1])[-4]
ch.top5.df = ch.links[ch.links$sender %in% ch.top5,]

# USA exports 83x more soy than it imports
length(unique(us.links[us.links$receiver == "United States","sender"]))
nrow(us.sums.df)
sum(us.links[us.links$sender == "United States","qty"])/sum(us.links[us.links$receiver == "United States",'qty'])

############################
# Temporal graphs: FIGURE 6

br.top5.df$receiver = factor(br.top5.df$receiver, levels=c("China","Netherlands","Spain","Germany","Thailand"))
br.plot = ggplot(data=br.top5.df, aes(x=t1, y=qtyM, group=receiver, color=receiver)) +
  geom_line() + # ggtitle("Top 5 receivers of soybean sent by Brazil") +
  labs(x="Year", y="1,000,000 USD", color='Receiver') +
  scale_color_manual(values=c("#d95f02", "#66a61e", "#e6ab02", "#a6761d", "#666666")) +
  theme_bw() + ylim(0,18000) + ggtitle("A")

ch.top5.df$sender = factor(ch.top5.df$sender, levels=c("United States","Brazil","Argentina","Canada","Uruguay"))
ch.plot = ggplot(data=ch.top5.df, aes(x=t1, y=qtyM, group=sender, color=sender)) +
  geom_line() + # ggtitle("Top 5 senders of soybean received by China") +
  labs(x="Year", y="1,000,000 USD", color='Sender') +
  scale_color_manual(values=c("#1b9e77", "#7570b3", "#e6ab02", "#a6761d", "#666666")) +
  theme_bw() + ylim(0,18000) + ggtitle("B")

us.top5.df$receiver = factor(us.top5.df$receiver, levels=c("China","Japan","Mexico","Netherlands","Indonesia"))
us.plot = ggplot(data=us.top5.df, aes(x=t1, y=qtyM, group=receiver, color=receiver)) +
  geom_line() + # ggtitle("Top 5 receivers of soybean sent by the US") +
  labs(x="Year", y="1,000,000 USD", color='Receiver') +
  scale_color_manual(values=c("#d95f02", "#66a61e", "#e6ab02", "#a6761d", "#666666")) +
  theme_bw() + ylim(0,18000) + ggtitle("C")



library(gridExtra)
png(file.path(soypath, 'figures', 'FIG6_temporal_brazil_us_china.png'), width = 480, height=600)
grid.arrange(br.plot, ch.plot, us.plot)
dev.off()

###########################################
# proportion of total edge weight per year
br.sum.year = ddply(br.links, .(t1), summarize, total=sum(qty))
br.top5.df = merge(br.top5.df, br.sum.year)
br.top5.df$proportion = 1-(br.top5.df$total - br.top5.df$qty)/br.top5.df$total

ch.sum.year = ddply(ch.links, .(t1), summarize, total=sum(qty))
ch.top5.df = merge(ch.top5.df, ch.sum.year)
ch.top5.df$proportion = 1-(ch.top5.df$total - ch.top5.df$qty)/ch.top5.df$total

us.sum.year = ddply(us.links, .(t1), summarize, total=sum(qty))
us.top5.df = merge(us.top5.df, us.sum.year)
us.top5.df$proportion = 1-(us.top5.df$total - us.top5.df$qty)/us.top5.df$total

###########################################
# Temporal graphs by proportion : FIGURE 7
br.top5.df$receiver = factor(br.top5.df$receiver, levels=c("China","Netherlands","Spain","Germany","Thailand"))
br.prop.plot = ggplot(data=br.top5.df, aes(x=t1, y=proportion, group=receiver, color=receiver)) +
  geom_line() + #ggtitle("Top 5 receivers of soy sent by Brazil") +
  labs(y="Proportion of Brazil soybean\nsending events per year", x="Year", color='Receiver') +
  scale_color_manual(values=c("#d95f02", "#66a61e", "#e6ab02", "#a6761d", "#666666")) +
  theme_bw() + ylim(0,1) + ggtitle("A")

ch.top5.df$sender = factor(ch.top5.df$sender, levels=c("United States","Brazil","Argentina","Canada","Uruguay"))
ch.prop.plot = ggplot(data=ch.top5.df, aes(x=t1, y=proportion, group=sender, color=sender)) +
  geom_line() + #ggtitle("Top 5 senders of soy received by China") +
  labs(y="Proportion of China soybean\nreceiving events per year", x="Year", color='Sender') +
  scale_color_manual(values=c("#1b9e77", "#7570b3", "#e6ab02", "#a6761d", "#666666")) +
  theme_bw() + ylim(0,1) + ggtitle("B")

us.top5.df$receiver = factor(us.top5.df$receiver, levels=c("China","Japan","Mexico","Netherlands","Indonesia"))
us.prop.plot = ggplot(data=us.top5.df, aes(x=t1, y=proportion, group=receiver, color=receiver)) +
  geom_line() + #ggtitle("Top 5 receivers of soy sent by the US") +
  labs(y="Proportion of US soybean\nsending events per year", x="Year", color='Receiver') +
  scale_color_manual(values=c("#d95f02", "#66a61e", "#e6ab02", "#a6761d", "#666666")) +
  theme_bw() + ylim(0,1) + ggtitle("C")


library(gridExtra)
png(file.path(soypath, 'figures', 'FIG7_prop_brazil_us_china.png'), width=480, height=600)
grid.arrange(br.prop.plot, ch.prop.plot, us.prop.plot)
dev.off()  
  
  

#############################################################################################
## Create the network
library(networkDynamic)
library(network)
library(sna)
library(tsna)
#nd <- networkDynamic(vertex.spells = links[, c()] edge.spells = links2[,c(1,2,3)], create.TEAs = TRUE, edge.TEA.names = c('weight', 'year') ## We should be able to explicitly add vertices and edge attributes (weights) here
nd2 <- networkDynamic(edge.spells = links2, create.TEAs = TRUE, edge.TEA.names = c('weight'))
ch.nd = networkDynamic(edge.spells = ch.links[,1:5], create.TEAs = TRUE, edge.TEA.names = c('weight'))
br.nd = networkDynamic(edge.spells = br.links[,1:5], create.TEAs = TRUE, edge.TEA.names = c('weight'))
us.nd = networkDynamic(edge.spells = us.links[,1:5], create.TEAs = TRUE, edge.TEA.names = c('weight'))

# won't accept character names for nodes

## Add geographic coordinates attributes to vertices
network::set.vertex.attribute(nd2, "lat", nodes$LAT)
network::set.vertex.attribute(nd2, "lon", nodes$LONG)
## Add identifiers for vertices
network::set.vertex.attribute(nd2, "id", nodes$nodes)
nd2%v%'lat' ## Access vertex attributes

## Try to add weight attribute to edges
network::set.edge.attribute(nd2, "weight", links2[,5])
plot(nd2, edge.lwd = 'weight', vertex.col = 'gray') # plot with edge weight (doesn't work well...)

# lapply(get.networks(nd), degree) # degree of each node in the network at each timestep
lapply(get.networks(nd2), network.density) # density of the whole network

## get the static network for every year
soyNets <- get.networks(nd2) # get each static network


## Various network and node level measures over time!
soyNodeDegree <- sapply(soyNets, sna::degree)
hist(soyNodeDegree) # most nodes in the network are not very connected!
soyNodeBetweenness <- sapply(soyNets, sna::betweenness)
soyEdgeDuration <- edgeDuration(nd2, mode="duration") # years each edge was active

##################
# FIGURE 4b
soyDensity <- sapply(soyNets, network.density)
density.df = data.frame(year=1986:2013, density=soyDensity)
density.plot = ggplot(density.df, aes(x=year, y=density)) + 
  geom_line() + theme_bw() + ggtitle("B")
  labs(y='Network Density', x='Year')

##################
# FIGURE 4a
soyConnectedness <- sapply(soyNets, sna::connectedness)
connectedness.df = data.frame(year=1986:2013, connectedness=soyConnectedness)
connectedness.plot = ggplot(connectedness.df, aes(x=year, y=connectedness)) + 
  geom_line() + theme_bw() + ggtitle("A")
  labs(y='Network Connectedness', x='Year')

################
# Plot FIGURE 4
png(file.path(soypath, 'figures', 'FIG4_network_stats_time.png'), width=480, height=480)
grid.arrange(connectedness.plot, density.plot)
dev.off()  



################
# FIGURE 5a
edgedur.df = data.frame(duration=soyEdgeDuration)
edgedur.plot = ggplot(edgedur.df, aes(duration)) + 
  geom_histogram(bins=15) + theme_bw() + ggtitle("A")
  labs(x='Duration (Years)', y="Number of Edges") 


hist(soyEdgeDuration) # most of the edge connections are of short duration -- network unstable?
which(edgeDuration(nd2,mode='counts')>1)# 2872 edges occured more than one time
which(edgeDuration(nd2,mode='counts')>8) # only one edge (210) occurs in more than 8 timesteps total!
hist(vertexDuration(nd2))
hist(tiedDuration(nd2, mode='counts')) # tied duration is the total # times each vertex has ties
tiedDuration(nd2, mode='counts',neighborhood = 'in') # Who are countries trading with?
td = tiedDuration(nd2, mode='duration', neighborhood='combined')


################
# FIGURE 5c
## ratio of export events to import events?
durationdf = data.frame(send=tiedDuration(nd2, mode='counts',neighborhood = 'out'), receive=tiedDuration(nd2, mode='counts',neighborhood = 'in'))
duration.plot = ggplot(durationdf, aes(x=send, y=receive)) + geom_point() + theme_bw() +
  labs(x='Number of Sending Events', y='Number of Receiving Events') +
  coord_fixed() + ggtitle("C")

mean(tiedDuration(br.nd)) ## average number of time a vertex is trading

##############
# FIGURE 5b
## Is there a relationship between the number of ties and the length of the ties? 
Nlength.ties.df = data.frame(number=sna::degree(as.network(nd2)), length=tiedDuration(nd2))
Nlength.ties.plot = ggplot(Nlength.ties.df, aes(x=number, y=length)) + geom_point() + 
  theme_bw() + labs(x='Degree Centrality', y='Connected Duration') + ggtitle("B")
# + ggtitle('Degree Centrality against Connected Duration')

## What is the temporal density of the network object? 
## This is a measure of the total regional activity in the network--total time vertices are tied
## by active edges / amount of time they could possibly be tied.
tEdgeDensity(nd2) # On average nodes are active 0.2109406 of the time series.
tEdgeDensity(ch.nd)
tEdgeDensity(br.nd)
tEdgeDensity(us.nd)
## we could get this due to high activity of a few nodes biasing the result...

## For all the possible dyads in the network, what fraction of time are they actually tied?
tEdgeDensity(nd2,agg.unit = 'dyad') # 0.01246033 is the probability a random pair was connected

## How many trade events are there per unit time? 
tEdgeDensity(nd2,mode='event')
#[1] 0.00102279 Not many partners are actively trading regularly in our network. 


################
# Plot FIGURE 5

png(file.path(soypath, 'figures', 'FIG5_network_stats.png'), width=480, height=600)
grid.arrange(edgedur.plot, Nlength.ties.plot, duration.plot)
dev.off()  




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
plot(tErgmStats(us.nd,'meandeg')[-29], main='Mean Degree', col='blue', ylim=c(0,0.8), type="l")
lines(tErgmStats(br.nd,'meandeg')[-29], col='green')
lines(tErgmStats(ch.nd,'meandeg')[-29], col='red')
lines(tErgmStats(nd2,'meandeg')[-29],col='purple')
legend('bottomleft', legend=c('USA','Brazil','China'), col=c('blue','green','red'), lty=1, bty='n')
dev.off()


# Pairwise links
br.ch.links = links.df[links.df$receiver == "China" & links.df$sender == "Brazil",]
br.us.links = links.df[links.df$sender == "Brazil" & links.df$receiver == "United States",]
us.ch.links = links.df[links.df$sender == "United States" & links.df$receiver == "China",]

pdf(file.path(soypath, 'figures', 'edge_weights_us_ch_br.pdf'))
plot(br.ch.links$t1, br.ch.links$qty, type='l')
lines(br.us.links$t1, br.us.links$qty, col='red')
lines(us.ch.links$t1, us.ch.links$qty, col='blue')
dev.off()



head(links.df)
ch.br.df

tiedDuration(ch.nd)
plot(tEdgeFormation(ch.nd))

tEdgeFormation(br.nd)
plot(tEdgeFormation(br.nd))

tEdgeFormation(us.nd)
plot(tEdgeFormation(us.nd))

tEdgeDensity(ch.nd)



# Discover positive feedbacks using regression ----------------------------

### Create LM for relationship between number of senders/receivers at beginning and end of dataset
links$sender = gsub(" ", "_", links$sender)
links$receiver = gsub(" ", "_", links$receiver)

# Senders
send86all = links[links$time == 1986,] # 35 senders
send13all = links[links$time == 2013,] # 87 senders
send13 = send13all[send13all$sender %in% send86all$sender,] # 2013 senders that were there in 1986 (32)
send86 = send86all[send86all$sender %in% send13$sender,] # 1986 senders that were still there in 2013 (32)

sendN86 = plyr::count(send86, .(sender))
sendN13 = plyr::count(send13, .(sender))
sendN = merge(sendN86, sendN13, "sender")
names(sendN)[2:3] = c("1986","2013")

sendlm = lm(sendN$`2013` ~ sendN$`1986`)
summary(sendlm)
plot(sendN$`1986`, sendN$`2013`)
abline(sendlm)

# Receivers
receive86all = links[links$time == 1986,] # 85 receivers
receive13all = links[links$time == 2013,] # 168 receivers

# 2013 receivers that were there in 1986 (77)
receive13 = receive13all[receive13all$receiver %in% receive86all$receiver,] 

# 1986 senders that were still there in 2013 (77)
receive86 = receive86all[receive86all$receiver %in% receive13$receiver,] 

receiveN86 = plyr::count(receive86, .(receiver))
receiveN13 = plyr::count(receive13, .(receiver))
receiveN = merge(receiveN86, receiveN13, "receiver")
names(receiveN)[2:3] = c("1986","2013")

receivelm = lm(receiveN$`2013` ~ receiveN$`1986`)
summary(receivelm)
plot(receiveN$`1986`, receiveN$`2013`)
abline(receivelm)

# OK, so we got LMs pointing up and r2 values in the 30s... but is this what we want?
# All trade increased between 1986 and 2013, so of course there is more overall.

### Total $ for sending and receiving
sendUSD86 = ddply(send86, .(sender), summarize, `1986` = sum(weights))
sendUSD13 = ddply(send13, .(sender), summarize, `2013` = sum(weights))
sendUSD = merge(sendUSD86, sendUSD13, "sender")

USDlm = lm(sendUSD$`2013` ~ sendUSD$`1986`)
summary(USDlm)
plot(sendUSD$`1986`, sendUSD$`2013`)
abline(USDlm)
