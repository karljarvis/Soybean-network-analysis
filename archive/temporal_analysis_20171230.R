## August 31, 2016

## NASA MSU Telecoupling
## Soybean trade - Temporal network analysis

## Danica Schaffer-Smith
## djs50@duke.edu

## Temporal Network Analysis using networkDynamic package in R. 
## https://cran.r-project.org/web/packages/networkDynamic/vignettes/networkDynamic.pdf
## http://stackoverflow.com/questions/19672592/dynamic-network-in-r

### Prepare and load the data ----
## Country id crosswalk table
## from: http://www.fao.org/countryprofiles/iso3list/en/
# soypath = "C:/Users/djs50/Documents/telecoupling"
soypath = "~/Google Drive/telecoupling/Soybean_network_analysis/"

## Source script loads and formats the data
source(file.path(soypath, "soy_load_data.R"))

library(plyr)
library(ggplot2)
library(countrycode)
library(gridExtra)
library(networkDynamic)
library(network)
library(sna)
library(tsna)
library(ergm)


### Prepare network data -----
## Create the node list
nodes1 <- soy.cleaned$Reporter.Countries
nodes2 <- soy.cleaned$Partner.Countries
nodes <- unique(c(nodes1, nodes2))
str(nodes) #221 unique countries

## Convert the country names to the ISO2C standard codes so that other attributes
## may be linked to the network in future. 
#countrycode(unique(soy$Reporter.Countries), "country.name","iso2c")
iso2 <- countrycode(nodes, "country.name", "iso2c", warn = T) # we lost the Pacific Islands Trust Territory!

## bind this to the nodes df
nodes <- data.frame(nodes=nodes, iso2=iso2, stringsAsFactors = F)

## Add coordinate attributes for each node (country centroids)
nodexy <- merge(nodes, coords, by.x = "iso2", by.y = "ISO3136")
str(nodexy) # 217 countries. Some lost due to mismatched names.
nodexy$id <- nodexy$iso2
nodexy$latitude <- nodexy$LAT
nodexy$longitude <- nodexy$LONG

## Create links dataset based on Export Value in 1000 US$ (filter the data)
soy.cleaned.dollars <- soy.cleaned[soy.cleaned$Element == "Export Value", ]
links.df <- data.frame(
  t1 = soy.cleaned.dollars$Year,
  t2 = soy.cleaned.dollars$Year + 1,
  receiver = soy.cleaned.dollars$Partner.Countries,
  sender = soy.cleaned.dollars$Reporter.Countries,
  weight = soy.cleaned.dollars$sum / 1000) # $1000 units, convert to $1M

## change links dataset to have numbers instead of countries
links.df$r.no = countrycode(links.df$receiver, "country.name","fao") # lost a number of small countries
links.df$s.no = countrycode(links.df$sender, "country.name","fao") # lost a number of small countries

# subset by country -------------------------------------------------------
ch.links = links.df[links.df$receiver == "China"|links.df$sender == "China",]
br.links = links.df[links.df$receiver == "Brazil"|links.df$sender == "Brazil",]
us.links = links.df[links.df$receiver == "United States"|links.df$sender == "United States",]

## Find top 5 edge weights for each
# Brazil exports: top 5 partners 
br.sums.df = ddply(br.links, .(receiver), summarize, total=sum(weight))
br.top5 = as.character(br.sums.df[order(-br.sums.df$total),][1:5,1])
br.top5.df = br.links[br.links$receiver %in% br.top5,]

# US exports: top 5 partners 
us.sums.df = ddply(us.links, .(receiver), summarize, total=sum(weight))
us.top5 = as.character(us.sums.df[order(-us.sums.df$total),][1:5,1])
us.top5.df = us.links[us.links$receiver %in% us.top5,]

# China imports: top 5 partners 
ch.sums.df = ddply(ch.links, .(sender), summarize, total=sum(weight))
ch.top5 = as.character(ch.sums.df[order(-ch.sums.df$total),][1:6,1])[-4]
ch.top5.df = ch.links[ch.links$sender %in% ch.top5,]

# USA exports 83x more soy than it imports
length(unique(us.links[us.links$receiver == "United States","sender"]))
nrow(us.sums.df)
sum(us.links[us.links$sender == "United States","qty"])/sum(us.links[us.links$receiver == "United States",'qty'])

### Temporal graphs: FIGURE 6 ----

br.top5.df$receiver = factor(br.top5.df$receiver, levels=c("China","Netherlands","Spain","Germany","Thailand"))
br.plot = ggplot(data=br.top5.df, aes(x=t1, y=weight, group=receiver, color=receiver)) +
  geom_line() + # ggtitle("Top 5 receivers of soybean sent by Brazil") +
  labs(x="Year", y="1,000,000 USD", color='Receiver') +
  scale_color_manual(values=c("#d95f02", "#66a61e", "#e6ab02", "#a6761d", "#666666")) +
  theme_bw() + ylim(0,18000) + ggtitle("A")

ch.top5.df$sender = factor(ch.top5.df$sender, levels=c("United States","Brazil","Argentina","Canada","Uruguay"))
ch.plot = ggplot(data=ch.top5.df, aes(x=t1, y=weight, group=sender, color=sender)) +
  geom_line() + # ggtitle("Top 5 senders of soybean received by China") +
  labs(x="Year", y="1,000,000 USD", color='Sender') +
  scale_color_manual(values=c("#1b9e77", "#7570b3", "#e6ab02", "#a6761d", "#666666")) +
  theme_bw() + ylim(0,18000) + ggtitle("B")

us.top5.df$receiver = factor(us.top5.df$receiver, levels=c("China","Japan","Mexico","Netherlands","Indonesia"))
us.plot = ggplot(data=us.top5.df, aes(x=t1, y=weight, group=receiver, color=receiver)) +
  geom_line() + # ggtitle("Top 5 receivers of soybean sent by the US") +
  labs(x="Year", y="1,000,000 USD", color='Receiver') +
  scale_color_manual(values=c("#d95f02", "#66a61e", "#e6ab02", "#a6761d", "#666666")) +
  theme_bw() + ylim(0,18000) + ggtitle("C")

png(file.path(soypath, 'figures', 'FIG6_temporal_brazil_us_china.png'), width = 480, height=600)
grid.arrange(br.plot, ch.plot, us.plot)
dev.off()

# ### proportion of total edge weight per year ----
# br.sum.year = ddply(br.links, .(t1), summarize, total=sum(weight))
# br.top5.df = merge(br.top5.df, br.sum.year)
# br.top5.df$proportion = 1-(br.top5.df$total - br.top5.df$qty)/br.top5.df$total
# 
# ch.sum.year = ddply(ch.links, .(t1), summarize, total=sum(weight))
# ch.top5.df = merge(ch.top5.df, ch.sum.year)
# ch.top5.df$proportion = 1-(ch.top5.df$total - ch.top5.df$qty)/ch.top5.df$total
# 
# us.sum.year = ddply(us.links, .(t1), summarize, total=sum(weight))
# us.top5.df = merge(us.top5.df, us.sum.year)
# us.top5.df$proportion = 1-(us.top5.df$total - us.top5.df$qty)/us.top5.df$total

### Temporal graphs by proportion 
# br.top5.df$receiver = factor(br.top5.df$receiver, levels=c("China","Netherlands","Spain","Germany","Thailand"))
# br.prop.plot = ggplot(data=br.top5.df, aes(x=t1, y=proportion, group=receiver, color=receiver)) +
#   geom_line() + #ggtitle("Top 5 receivers of soy sent by Brazil") +
#   labs(y="Proportion of Brazil soybean\nsending events per year", x="Year", color='Receiver') +
#   scale_color_manual(values=c("#d95f02", "#66a61e", "#e6ab02", "#a6761d", "#666666")) +
#   theme_bw() + ylim(0,1) + ggtitle("A")
# 
# ch.top5.df$sender = factor(ch.top5.df$sender, levels=c("United States","Brazil","Argentina","Canada","Uruguay"))
# ch.prop.plot = ggplot(data=ch.top5.df, aes(x=t1, y=proportion, group=sender, color=sender)) +
#   geom_line() + #ggtitle("Top 5 senders of soy received by China") +
#   labs(y="Proportion of China soybean\nreceiving events per year", x="Year", color='Sender') +
#   scale_color_manual(values=c("#1b9e77", "#7570b3", "#e6ab02", "#a6761d", "#666666")) +
#   theme_bw() + ylim(0,1) + ggtitle("B")
# 
# us.top5.df$receiver = factor(us.top5.df$receiver, levels=c("China","Japan","Mexico","Netherlands","Indonesia"))
# us.prop.plot = ggplot(data=us.top5.df, aes(x=t1, y=proportion, group=receiver, color=receiver)) +
#   geom_line() + #ggtitle("Top 5 receivers of soy sent by the US") +
#   labs(y="Proportion of US soybean\nsending events per year", x="Year", color='Receiver') +
#   scale_color_manual(values=c("#d95f02", "#66a61e", "#e6ab02", "#a6761d", "#666666")) +
#   theme_bw() + ylim(0,1) + ggtitle("C")
# 
# png(file.path(soypath, 'figures', 'prop_brazil_us_china.png'), width=480, height=600)
# grid.arrange(br.prop.plot, ch.prop.plot, us.prop.plot)
# dev.off()  
#   

### Create the network ----
# remove NAs from links.mat
links.mat = as.matrix(links.df[c("t1","t2","r.no","s.no","weight")])
links.mat = links.mat[complete.cases(links.mat),]
nd2 <- networkDynamic(edge.spells = links.mat, 
                      create.TEAs = TRUE, 
                      edge.TEA.names = c('weight'))
network.dynamic.check(nd2)

## Add geographic coordinates attributes to vertices
#### Need to fix this! mismatch of number of nodes in nd2 and nodes objects.
#### the nd2 object is repeating node IDs when it runs out of them. Therefore
#### they can't be matched up properly. This would also mean that lat and long 
#### are incorrect!
# network::set.vertex.attribute(nd2, "lat", nodes$LAT)
# network::set.vertex.attribute(nd2, "lon", nodes$LONG)
## Add identifiers for vertices
get.vertex.attribute(nd2,"id") # no countries

#### need to add country names in here, but I can't see how.
#### there are 351 missing names for the vertices in nd2.
#### but there are only 347 unique sender and receiver names in links.df.
#### and once those are converted to numbers, with the ones that get left out it's only 316.
network::set.vertex.attribute(nd2, "id", country)
nd2%v%'id' ## Access vertex attributes

## Try to add weight attribute to edges
network::set.edge.attribute(nd2, "weight", links.m[,5])
plot(nd2, edge.lwd = 'weight', vertex.col = 'gray') # plot with edge weight (doesn't work well...)

# lapply(get.networks(nd), degree) # degree of each node in the network at each timestep
lapply(get.networks(nd2), network.density) # density of the whole network

## get the static network for every year
soyNets <- get.networks(nd2) # get each static network

## Various network and node level measures over time!
soyNodeDegree <- sapply(soyNets, sna::degree)
rownames(soyNodeDegree) = nd2%v%'id'
colnames(soyNodeDegree) = 1986:2013
hist(soyNodeDegree) # most nodes in the network are not very connected!
soyNodeBetweenness <- sapply(soyNets, sna::betweenness)
rownames(soyNodeBetweenness) = nd2%v%'id'
colnames(soyNodeBetweenness) = 1986:2013
soyEdgeDuration <- edgeDuration(nd2, mode="duration") # years each edge was active

### FIGURE 3b ----
soyDensity <- sapply(soyNets, network.density)
density.df = data.frame(year=1986:2013, density=soyDensity)
density.plot = ggplot(density.df, aes(x=year, y=density)) + 
  geom_line() + theme_bw() + ggtitle("B") +
  labs(y='Network Density', x='Year')

### FIGURE 3a ----
soyConnectedness <- sapply(soyNets, sna::connectedness)
connectedness.df = data.frame(year=1986:2013, connectedness=soyConnectedness)
connectedness.plot = ggplot(connectedness.df, aes(x=year, y=connectedness)) + 
  geom_line() + theme_bw() + ggtitle("A") +
  labs(y='Network Connectedness', x='Year')

## Plot FIGURE 3
png(file.path(soypath, 'figures', 'FIG03_network_stats_time.png'), width=480, height=480)
grid.arrange(connectedness.plot, density.plot)
dev.off()  



### FIGURE 4a ----
edgedur.df = data.frame(duration=soyEdgeDuration)
edgedur.plot = ggplot(edgedur.df, aes(duration)) + 
  geom_histogram(bins=15) + theme_bw() + ggtitle("A") +
  labs(x='Duration (Years)', y="Number of Edges") 


hist(soyEdgeDuration) # most of the edge connections are of short duration -- network unstable?
which(edgeDuration(nd2,mode='counts')>1)# 2872 edges occured more than one time
which(edgeDuration(nd2,mode='counts')>8) # only one edge (210) occurs in more than 8 timesteps total!
hist(vertexDuration(nd2))
hist(tiedDuration(nd2, mode='counts')) # tied duration is the total # times each vertex has ties
tiedDuration(nd2, mode='counts',neighborhood = 'in') # Who are countries trading with?
td = tiedDuration(nd2, mode='duration', neighborhood='combined')

### FIGURE 4c ----
## ratio of export events to import events?
durationdf = data.frame(send=tiedDuration(nd2, mode='counts',neighborhood = 'out'), receive=tiedDuration(nd2, mode='counts',neighborhood = 'in'))
duration.plot = ggplot(durationdf, aes(x=send, y=receive)) + geom_point() + theme_bw() +
  labs(x='Number of Sending Events', y='Number of Receiving Events') +
  coord_fixed() + ggtitle("C")

mean(tiedDuration(br.nd)) ## average number of time a vertex is trading


### FIGURE 4b ---------------------------------------------------------------

## Is there a relationship between the number of ties and the length of the ties? 
Nlength.ties.df = data.frame(number=sna::degree(as.network(nd2)), length=tiedDuration(nd2))
Nlength.ties.plot = ggplot(Nlength.ties.df, aes(x=number, y=length)) + geom_point() + 
  theme_bw() + labs(x='Degree Centrality by Country', y='Connected Duration by Country') + ggtitle("B")
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


### Plot FIGURE 4
  png(file.path(soypath, 'figures', 'FIG04_network_stats.png'), width=480, height=600)
  grid.arrange(edgedur.plot, Nlength.ties.plot, duration.plot)
  dev.off()  




#############################################################################################3
## Temporal network tools in statnet: networkDynamic, ndtv, tsna
#http://statnet.csde.washington.edu/workshops/SUNBELT/current/ndtv/ndtv_workshop.html
## Some great viz code here, but might be best suited to a smaller subset of the graph

## How many edges form at each timestep?
tEdgeFormation(nd2)
plot( tEdgeFormation(nd2) )


## mean degree of China, Brazil, and US
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
links = links.df[,c("t1","receiver","sender","weight")]
names(links)[1] = "time"
links$sender = gsub(" ", "_", links$sender)
links$receiver = gsub(" ", "_", links$receiver)

  # # Total $ for sending and receiving 
  # sendUSD86 = ddply(send86, .(sender), summarize, `1986` = sum(weights))
  # sendUSD13 = ddply(send13, .(sender), summarize, `2013` = sum(weights))
  # sendUSD = merge(sendUSD86, sendUSD13, "sender")
  # 
  # USDlm = lm(sendUSD$`2013` ~ sendUSD$`1986`)
  # summary(USDlm)
  # plot(sendUSD$`1986`, sendUSD$`2013`)
  # abline(USDlm)
  # 
  # receiveUSD86 = ddply(receive86, .(receiver), summarize, `1986` = sum(weights))
  # receiveUSD13 = ddply(receive13, .(receiver), summarize, `2013` = sum(weights))
  # receiveUSD = merge(receiveUSD86, receiveUSD13, "receiver")
  # 
  # USDlm = lm(receiveUSD$`2013` ~ receiveUSD$`1986`)
  # summary(USDlm)
  # plot(receiveUSD$`1986`, receiveUSD$`2013`)
  # abline(USDlm)


# trade vs time -----------------------------------------------------------

# Pull 2 datasets out of the links object: send and receive
# make a count of trade partners for each one
# Approaches to analysis
# 1) calculate % change in trade from T1 to T2, repeat across all time steps
# Looks at how # trade partners depends on previous time step
# Most complicated, but it summarizes all in one graph
# 2) calculate % of total trade partners in the world in that time step
# good because it deals with the share of world trade. Do those with higher trade levels 
# in early time steps keep or even increase their share of total world trade? This is
# probably what we care about most. 
# 3) raw counts, change over time
# this deals only with whether countries tend to increase their # of trade partners or not.
# this is not much different than what I've already done. It's pretty clear that 
# trade is increasing. But positive feedback means that greater early values
# will lead to even greater later values. 
# 4) Alternative: pool top 10%/20%/etc countries with highest initial share of trade
# partners. How does that share of total world trade change over time? 

# We need #3 as a step toward the others. Get this first.
# Then do #2 because it has most potential to show feedbacks.
# Then work on #4 depending on how long #2 takes.

# *what to do about countries that are missing data for some years? will that 
# be an issue? I may need to create entries with counts of 0 for those.

# remove "Unspecified" receivers
links = links[links$receiver != "Unspecified",]

# Use plyr to summarize by count for each sender in each year
s.count = dcast(links, time ~ sender, length)
r.count = dcast(links, time ~ receiver, length)
  
### Approach #3 ----
# Senders: make lm for each
s.list = vector(mode = "list",length=(ncol(s.count)-1))
for(i in 1:(ncol(s.count)-1)) 
  {
  s.list[[i]] = lm(s.count[,i] ~ time, data=s.count)
  names(s.list)[i-1] = colnames(s.count)[i]
  }

# Receivers: make lm for each
r.list = vector(mode = "list",length=(ncol(r.count)-1))
for(i in 1:(ncol(r.count)-1)) 
  {
  r.list[[i]] = lm(r.count[,i] ~ time, data=r.count)
  names(r.list)[i-1] = colnames(r.count)[i]
  }

# plot counts
s.melt = melt(s.count, id='time')
names(s.melt) = c('Year','Country','Counts')
s3.melt = s.melt[s.melt$Country %in% c("Brazil","China","United States"),]
s.lm.plot = ggplot() +
  # geom_path(data=s.melt, aes(Year,Counts, group=Country)) +
  geom_smooth(data=s.melt, aes(Year, Counts, group=Country), method="lm", se=FALSE, col="grey80") +
  ggtitle('Number of Global Trade Partners by Sender') +
  geom_path(data=s3.melt, aes(Year,Counts, group=Country, color=Country)) +
  geom_smooth(data=s3.melt, aes(Year, Counts, group=Country, color=Country), method="lm") +
  scale_color_manual(values=c("#7570b3", "#d95f02", "#1b9e77")) 

r.melt = melt(r.count, id='time')
names(r.melt) = c('Year','Country','Counts')
r3.melt = r.melt[r.melt$Country %in% c("Brazil","China","United States"),]
r.lm.plot = ggplot() +
  # geom_path(data=r.melt, aes(Year,Counts, group=Country)) +
  geom_smooth(data=r.melt, aes(Year, Counts, group=Country), method="lm", se=FALSE, col="grey80") +
  ggtitle('Number of Global Trade Partners by Receiver') +
  geom_path(data=r3.melt, aes(Year,Counts, group=Country, color=Country)) +
  geom_smooth(data=r3.melt, aes(Year, Counts, group=Country, color=Country), method="lm") +
  scale_color_manual(values=c("#7570b3", "#d95f02", "#1b9e77")) 

pdf(file.path(soypath, "figures", "Feedback_Counts.pdf"))
grid.arrange(s.lm.plot, r.lm.plot)
dev.off()

### Approach #2: % of total trade ----

# Senders
s.total=rowSums(s.count[,-1])
s.prop = data.frame(time=s.count$time, s.count[,-1]/s.total)

# make lm for each
sp.list = vector(mode = "list",length=(ncol(s.prop)-1))
for(i in 2:ncol(s.prop))
{
  sp.list[[i-1]] = lm(s.prop[,i] ~ time, data=s.prop)
  names(sp.list)[i-1] = colnames(s.prop)[i]
}

# Receivers
r.total=rowSums(r.count[,-1])
r.prop = data.frame(time=r.count$time, r.count[,-1]/r.total)
rp.list = vector(mode = "list",length=(ncol(r.prop)-1))
for(i in 2:ncol(r.count))
{
  rp.list[[i-1]] = lm(r.prop[,i] ~ time, data=r.prop)
  names(rp.list)[i-1] = colnames(r.prop)[i]
}

# plot
  # Senders
  sp.melt = melt(s.prop, id='time')
  sp.melt$variable = gsub("[.]", " ", sp.melt$variable)
  names(sp.melt) = c('Year','Country','Proportion')
  sp3.melt = sp.melt[sp.melt$Country %in% c("Brazil","China","United States"),]
  sp3.melt$Country = factor(as.character(sp3.melt$Country))
  sp.lm.plot = ggplot() +
    # geom_path(data=sp.melt, aes(Year,Proportion, group=Country)) +
    geom_smooth(data=sp.melt, aes(Year, Proportion, group=Country), method="lm", se=FALSE, col="grey80") +
    ggtitle('Proportion of Global Trade Partners by Sender') +
    geom_path(data=sp3.melt, aes(Year,Proportion, group=Country, color=Country)) +
    geom_smooth(data=sp3.melt, aes(Year, Proportion, group=Country, color=Country), method="lm") +
    scale_color_manual(values=c("#7570b3", "#d95f02", "#1b9e77")) 
  # China: "#d95f02" Brazil: "#7570b3" United States: "#1b9e77"
    
  # Receivers
  rp.melt = melt(r.prop, id='time')
  rp.melt$variable = gsub("[.]", " ", rp.melt$variable)
  names(rp.melt) = c('Year','Country','Proportion')
  rp3.melt = rp.melt[rp.melt$Country %in% c("Brazil","China","United States"),]
  rp3.melt$Country = factor(as.character(rp3.melt$Country))
  
  rp.lm.plot = ggplot() +
    # geom_path(data=rp.melt, aes(Year,Proportion, group=Country)) +
    geom_smooth(data=rp.melt, aes(Year, Proportion, group=Country), method="lm", se=FALSE, col="grey80") +
    ggtitle('Proportion of Global Trade Partners by Receiver') +
    geom_path(data=rp3.melt, aes(Year,Proportion, group=Country, color=Country)) +
    geom_smooth(data=rp3.melt, aes(Year, Proportion, group=Country, color=Country), method="lm") +
    scale_color_manual(values=c("#7570b3", "#d95f02", "#1b9e77")) 

  # all 4 in one data frame
  rs.melt = rbind(
    data.frame(Year=s.melt$Year, Country=s.melt$Country, trade="N receivers each sender has", metric="Counts of trade partners", value=s.melt$Counts),
    data.frame(Year=r.melt$Year, Country=r.melt$Country, trade="N senders each receiver has", metric="Counts of trade partners", value=r.melt$Counts),
    data.frame(Year=sp.melt$Year, Country=sp.melt$Country, trade="N receivers each sender has", metric="Proportion of all trade partners", value=sp.melt$Proportion),
    data.frame(Year=rp.melt$Year, Country=rp.melt$Country, trade="N senders each receiver has", metric="Proportion of all trade partners", value=rp.melt$Proportion)
  )
  
  # all 4 figures of the 3 focal countries
  rs3.melt = rbind(
    data.frame(Year=s3.melt$Year, Country=s3.melt$Country, trade="N receivers each sender has", metric="Counts of trade partners", value=s3.melt$Counts),
    data.frame(Year=r3.melt$Year, Country=r3.melt$Country, trade="N senders each receiver has", metric="Counts of trade partners", value=r3.melt$Counts),
    data.frame(Year=sp3.melt$Year, Country=sp3.melt$Country, trade="N receivers each sender has", metric="Proportion of all trade partners", value=sp3.melt$Proportion),
    data.frame(Year=rp3.melt$Year, Country=rp3.melt$Country, trade="N senders each receiver has", metric="Proportion of all trade partners", value=rp3.melt$Proportion)
  )
  
  # plot all 4 in one shot
  rs.plot = ggplot() +
    geom_smooth(data=rs.melt, aes(Year, value, group=Country), method="lm", se=FALSE, col="grey80") +
    facet_wrap(~ metric + trade, scales = "free") + theme_bw() +
    geom_path(data=rs3.melt, aes(Year,value, group=Country, color=Country)) +
    geom_smooth(data=rs3.melt, aes(Year, value, group=Country, color=Country), method="lm") +
    scale_color_manual(values=c("#7570b3", "#d95f02", "#1b9e77")) 
  
  # problem: y axis label is only one on the side. Also, the labels for each figure in the panel
  # are not great. 
  # But there's one legend, which is good. Overall I think doing it as 2 data frames
  # and combinding those two panels of 2 into one figure of 4 is better.
  
  # 2 data frames
  rs.melt = rbind(
    # Number of receivers each sender has
    data.frame(Year=s.melt$Year, Country=s.melt$Country, trade="A", Counts=s.melt$Counts),
    # Number of senders each receiver has
    data.frame(Year=r.melt$Year, Country=r.melt$Country, trade="B", Counts=r.melt$Counts)
  )
  rs3.melt = rs.melt[rs.melt$Country %in% c("Brazil","China","United States"),]
  rs3.melt$Country = factor(as.character(rs3.melt$Country))
  
  rsp.melt = rbind(
    # Proportion of total receivers each sender has
    data.frame(Year=sp.melt$Year, Country=sp.melt$Country, trade="C", Proportion=sp.melt$Proportion),
    # Proportion of total senders each receiver has
    data.frame(Year=rp.melt$Year, Country=rp.melt$Country, trade="D", Proportion=rp.melt$Proportion)
  )
  rsp3.melt = rsp.melt[rsp.melt$Country %in% c("Brazil","China","United States"),]
  rsp3.melt$Country = factor(as.character(rsp3.melt$Country))
  
  rs.plot = ggplot() +
    geom_smooth(data=rs.melt, aes(Year, Counts, group=Country), method="lm", se=FALSE, col="grey80") +
    # ggtitle('Linear models of counts of trade partners over time') +
    facet_wrap(~ trade, scales = "free") +
    geom_path(data=rs3.melt, aes(Year,Counts, group=Country, color=Country)) +
    geom_smooth(data=rs3.melt, aes(Year, Counts, group=Country, color=Country), method="lm") +
    scale_color_manual(values=c("#7570b3", "#d95f02", "#1b9e77")) +
    theme_bw() + theme(strip.background = element_rect(colour = "black", fill = "white"))
  
  rsp.plot = ggplot() +
    geom_smooth(data=rsp.melt, aes(Year, Proportion, group=Country), method="lm", se=FALSE, col="grey80") +
    # ggtitle('Linear models of proportion of total trade partners over time') +
    facet_wrap(~ trade, scales = "free") +
    geom_path(data=rsp3.melt, aes(Year,Proportion, group=Country, color=Country)) +
    geom_smooth(data=rsp3.melt, aes(Year, Proportion, group=Country, color=Country), method="lm") +
    scale_color_manual(values=c("#7570b3", "#d95f02", "#1b9e77")) +
    theme_bw() + theme(strip.background = element_rect(colour = "black", fill = "white"))
  
png(file.path(soypath, "figures", "FIG6_Feedback_Combined.png"), units = "cm", width = 16, height = 16, res=300)
grid.arrange(rs.plot, rsp.plot)
dev.off()


# Create table summarizing feedback results

  # regression coefficients, p-values and number of observations for each
  # senders - proportions
  sp.coef = ldply(sp.list, function(x) summary(x)$coefficients[2,c(1,4)])
  sp.r2 = ldply(sp.list, function(x) summary(x)$adj.r.squared)
  sp.nobs = ldply(s.prop[,-1], function(x) sum(x > 0))
  sp1.df = merge(sp.coef, sp.r2, by=".id")
  sp.df = merge(sp1.df, sp.nobs, by=".id")
  names(sp.df) = c("country", "sender_coef","sender_pval","sender_adjr2","sender_nobs")
  sp.df$sender_coef = round(sp.df$sender_coef, 8)
  sp.df$sender_pval = round(sp.df$sender_pval, 4)
  sp.df$sender_adjr2 = round(sp.df$sender_adjr2, 4)
  sp.propmeans = colMeans(s.prop[,-1])
  sp.propdf = data.frame(country=names(sp.propmeans), mean_prop=round(sp.propmeans,3))
  sp.p = merge(sp.df, sp.propdf, by="country", all=T)
  sp.p = sp.p[order(-sp.p$mean_prop),]
  sum(sp.p$sender_coef > 0, na.rm = T)/nrow(sp.p)
  sp.p05 = sp.p[sp.p$sender_pval < 0.05,]
  sp.p4 = sp.p05[sp.p05$sender_nobs > 3,]
  sum(sp.p4$sender_coef > 0, na.rm = T)/nrow(sp.p4)
  sp.sum = rbind(t(ldply(lapply(sp.p4[,-1], summary))),
                 sd = lapply(sp.p4[,-1], function(x) sd(x, na.rm = T)))
  write.csv(sp.sum, file.path(soypath, "figures", "senders_prop_summary.csv"))
  
  # receivers - proportions
  rp.coef = ldply(rp.list, function(x) summary(x)$coefficients[2,c(1,4)])
  rp.r2 = ldply(rp.list, function(x) summary(x)$adj.r.squared)
  rp.nobs = ldply(r.prop[,-1], function(x) sum(x > 0))
  rp.1df = merge(rp.coef, rp.r2, by=".id")
  rp.df = merge(rp.1df, rp.nobs, by=".id")
  names(rp.df) = c("country", "receiver_coef","receiver_pval","receiver_adjr2","receiver_nobs")
  rp.df$receiver_coef = round(rp.df$receiver_coef, 8)
  rp.df$receiver_pval = round(rp.df$receiver_pval, 4)
  rp.df$receiver_adjr2 = round(rp.df$receiver_adjr2, 4)
  rp.propmeans = colMeans(r.prop[,-1])
  rp.propdf = data.frame(country=names(rp.propmeans), mean_prop=round(rp.propmeans,3))
  rp.p = merge(rp.df, rp.propdf, by="country", all=T)
  rp.p = rp.p[order(-rp.p$mean_prop),]
  sum(rp.p$receiver_coef > 0, na.rm = T)/nrow(rp.p)
  rp.p05 = rp.p[rp.p$receiver_pval < 0.05,]
  rp.p4 = rp.p05[rp.p05$receiver_nobs > 3,]
  sum(rp.p4$receiver_coef > 0, na.rm = T)/nrow(rp.p4)
  rp.sum = rbind(t(ldply(lapply(rp.p4[,-1], summary))),
                 sd = lapply(rp.p4[,-1], function(x) sd(x, na.rm = T)))
  write.csv(rp.sum, file.path(soypath, "figures", "receivers_prop_summary.csv"))
  
  # Senders - raw counts
  s.coef = ldply(s.list, function(x) summary(x)$coefficients[2,c(1,4)])
  s.r2 = ldply(s.list, function(x) summary(x)$adj.r.squared)
  s.nobs = ldply(s.count[,-1], function(x) sum(x > 0))
  s1.df = merge(s.coef, s.r2, by=".id")
  s.df = merge(s1.df, s.nobs, by=".id")
  names(s.df) = c("country", "sender_coef","sender_pval","sender_adjr2","sender_nobs")
  s.df$sender_coef = round(s.df$sender_coef, 8)
  s.df$sender_pval = round(s.df$sender_pval, 4)
  s.df$sender_adjr2 = round(s.df$sender_adjr2, 4)
  s.propmeans = colMeans(s.prop[,-1])
  s.propdf = data.frame(country=names(s.propmeans), mean_prop=round(s.propmeans,3))
  s.p = merge(s.df, s.propdf, by="country", all=T)
  s.p = s.p[order(-s.p$mean_prop),]
  sum(s.p$sender_coef > 0, na.rm = T)/nrow(s.p)
  s.p05 = s.p[s.p$sender_pval < 0.05,]
  s.p4 = s.p05[s.p05$sender_nobs > 3,]
  sum(s.p4$sender_coef > 0, na.rm = T)/nrow(s.p4)
  s.sum = rbind(t(ldply(lapply(s.p4[,-1], summary))),
                sd = lapply(s.p4[,-1], function(x) sd(x, na.rm = T)))
  write.csv(s.sum, file.path(soypath, "figures", "senders_counts_summary.csv"))
  
  # Receivers - raw counts
  r.coef = ldply(r.list, function(x) summary(x)$coefficients[2,c(1,4)])
  r.r2 = ldply(r.list, function(x) summary(x)$adj.r.squared)
  r.nobs = ldply(r.count[,-1], function(x) sum(x > 0))
  r1.df = merge(r.coef, r.r2, by=".id")
  r.df = merge(r1.df, r.nobs, by=".id")
  names(r.df) = c("country", "receiver_coef","receiver_pval","receiver_adjr2","receiver_nobs")
  r.df$receiver_coef = round(r.df$receiver_coef, 8)
  r.df$receiver_pval = round(r.df$receiver_pval, 4)
  r.df$receiver_adjr2 = round(r.df$receiver_adjr2, 4)
  r.propmeans = colMeans(r.prop[,-1])
  r.propdf = data.frame(country=names(r.propmeans), mean_prop=round(r.propmeans,3))
  r.p = merge(r.df, r.propdf, by="country", all=T)
  r.p = r.p[order(-r.p$mean_prop),]
  sum(r.p$receiver_coef > 0, na.rm = T)/nrow(r.p)
  r.p05 = r.p[r.p$receiver_pval < 0.05,]
  r.p4 = r.p05[r.p05$receiver_nobs > 3,]
  sum(r.p4$receiver_coef > 0, na.rm = T)/nrow(r.p4)
  r.sum = rbind(t(ldply(lapply(r.p4[,-1], summary))),
                sd = lapply(r.p4[,-1], function(x) sd(x, na.rm = T)))
  write.csv(r.sum, file.path(soypath, "figures", "receivers_counts_summary.csv"))  
  # subset the table by focusing on top ones, summary stats for rest.
  r.propmeans = r.propmeans[order(-r.propmeans)]
  
  
  # s.25 = merge(s.df, s.prop25df, by="country")
  # s.25$mean_prop = round(s.prop25,3)
  # s.25 = s.25[order(-s.25$mean_prop),]
  # s.proprest = s.propmeans[-1:-25]
  # s.rest = s.df[!s.df$country %in% names(s.prop25),]
  # sum(s.prop25) #78.6% of receivers are linked with these 25 senders
  # nrow(s.rest) #80 countries left after the top 25.
  # summary(s.rest[,-1])
  # 
  # s.top10df = data.frame(country = names(s.propmeans[1:10]),
  #                         mean_prop = round(s.propmeans[1:10], 3))
  # s.top10 = merge(s.df, s.top10df, by="country")
  # s.top10 = s.top10[order(-s.top10$mean_prop),]
  # 
  # s.lo10df = data.frame(country = names(tail(s.propmeans,10)),
  #                        mean_prop = round(tail(s.propmeans, 10), 3))
  # s.lo10 = merge(s.df, s.lo10df, by="country")
  # s.lo10 = s.lo10[order(-s.lo10$mean_prop),]
  # 
  # 
  # # subset the table by focusing on top ones, summary stats for rest.
  # r.propmeans = colMeans(r.prop[,-1])
  # r.propmeans = r.propmeans[order(-r.propmeans)]
  # r.prop25 = r.propmeans[1:25]
  # r.25 = r.df[r.df$country %in% names(r.prop25),]
  # r.25$mean_prop = round(r.prop25,3)
  # r.proprest = r.propmeans[-1:-25]
  # r.rest = r.df[!r.df$country %in% names(r.prop25),]
  # nrow(r.rest) #171 countries left
  # sum(r.prop25) #48.0% of senders are linked with these 25 senders  
  
  
  options(scipen=10)
  write.csv(s.p4, file.path(soypath, "figures", "Table_sender_feedback_count.csv"))
  write.csv(r.p4, file.path(soypath, "figures", "Table_receiver_feedback_count.csv"))          
  write.csv(sp.p4, file.path(soypath, "figures", "Table_sender_feedback_prop.csv"))
  write.csv(rp.p4, file.path(soypath, "figures", "Table_receiver_feedback_prop.csv"))          
  
  

# Explore spillover effects -----------------------------------------------

# # show all senders to China
#   ch.df = ch.links[!ch.links$sender == "China",c("t1","weight","sender")]
#   ch.sum.year = ddply(ch.df, .(t1), summarize, total=sum(weight))
#   ch.df = merge(ch.df, ch.sum.year)
#   ch.df$proportion = 1-(ch.df$total - ch.df$weight)/ch.df$total
#   ch.df = ch.df[order(ch.df$t1, ch.df$sender),c("t1","sender","proportion")]
#   head(ch.df)
#   ch.cast = dcast(ch.df, t1 ~ sender, value.var = "proportion")
#   names(ch.cast) = gsub(" ", "_", names(ch.cast))
#   names(ch.cast) = gsub("'", "_", names(ch.cast))
#   names(ch.cast) = gsub(",", "", names(ch.cast))
#   write.csv(ch.cast, file.path(soypath, "data", "China_Senders.csv"), row.names=F)
  ch.cast = read.csv(file.path(soypath, "data", "China_Senders.csv"))
  
  # # Exclude senders that only sent one time
  # colSums(!is.na(ch.cast))
  # ch1.cast = ch.cast[,colSums(!is.na(ch.cast)) > 1]
  # write.csv(ch1.cast, file.path(soypath, "data", "China_Senders1.csv"), row.names=F)
  ch1.cast = read.csv(file.path(soypath, "data", "China_Senders1.csv"))
  
# rank by total soy exported to China
  ch.sums.df = ddply(ch.links, .(sender), summarize, total=sum(weight))
  ch.rank.df = ch.sums.df[order(-ch.sums.df$total),]
  ch.rank.df$rank = 1:nrow(ch.rank.df)
  
# create models of spillovers 
  # Null model
  no.lm = lm(Brazil ~ 1, ch.cast)
  # US only
  usa.lm = lm(Brazil ~ United_States, ch.cast)
  plot(ch.cast$United_States, ch.cast$Brazil)
  # Argentina only
  arg.lm = lm(Brazil ~ Argentina, ch.cast)
  plot(ch.cast$Argentina, ch.cast$Brazil)
  # US + Argentina
  usa.arg.lm = lm(Brazil ~ United_States + Argentina, ch.cast)
  # African countries (All but South Africa have only sent to China in only 1 year total. Cannot make models of that, nor should we.)
  rsa.lm = lm(Brazil ~ South_Africa, ch.cast)
  plot(ch.cast$South_Africa, ch.cast$Brazil)
  # South Africa + USA + Argentina
  rsa_usa_arg.lm = lm(Brazil ~ South_Africa + United_States + Argentina, ch.cast)
  plot(ch.cast$South_Africa, ch.cast$Brazil)
  # Uruguay is #5 sender
  ury.lm = lm(Brazil ~ Uruguay, ch.cast)
  plot(ch.cast$Uruguay, ch.cast$Brazil)
  # S American countries (Peru excluded bc only 1 observation, models don't work with Paraguay and Bolivia included)
  sam.lm = lm(Brazil ~ Argentina + Uruguay, ch.cast)
  summary(sam.lm)
  
  # model selection
  library(AICcmodavg)
  mods = list(no.lm, usa.lm, arg.lm, usa.arg.lm, rsa.lm, rsa_usa_arg.lm, ury.lm, sam.lm)
  countries = c("None", "USA","ARG","USA_ARG","RSA","RSA_USA_ARG", "URY", "SAM")
  (aic.df = aictab(cand.set = mods, modnames = countries))
  write.csv(aic.df, file.path(soypath, "figures", "Spillovers.csv"))
  
  # model averaging
  usa.par = modavg(cand.set = mods, modnames = countries, parm = "United_States")
  write.csv(usa.par$Mod.avg.table, file.path(soypath, "figures", "Spillovers_USA.csv"))
  sink(file.path(soypath, "figures", "Spillovers_USA.txt")); usa.par; sink()  

  arg.par = modavg(cand.set = mods, modnames = countries, parm = "Argentina")
  write.csv(arg.par$Mod.avg.table, file.path(soypath, "figures", "Spillovers_ARG.csv"))
  sink(file.path(soypath, "figures", "Spillovers_ARG.txt")); arg.par; sink()  

  # includes zero in confidence interval
  sink(file.path(soypath, "figures", "Spillovers_RSA.txt"))
  modavg(cand.set = mods, modnames = countries, parm = "South_Africa")
  sink()  

  # includes zero in confidence interval
  sink(file.path(soypath, "figures", "Spillovers_URY.txt"))
  modavg(cand.set = mods, modnames = countries, parm = "Uruguay")
  sink()  

  
# Use network metrics as data for model selection approach ----------------

  soyNodeDegree
  deg.df = as.data.frame(t(soyNodeDegree))
  
  soyNodeBetweenness
  btw.df = as.data.frame(t(soyNodeBetweenness))
  
  
#   10/30 notes:
#   clean up, get rid of alternative China subunits ## Haven't done.
#   look at countries individually
#   include canada, asian countries
#   could look at all countries that are above the mean individually
  ch.cast[is.na(ch.cast)] = 0
  meanChinaExports = colMeans(ch.cast[,-1])
  meanChinaExports[meanChinaExports > mean(meanChinaExports)]
  # hmm, this is only Argentina, Brazil, and US.

  meanChinaExports[meanChinaExports > median(meanChinaExports)]
  # hmm, this is 22 countries long, down from 46.
     
#   US Canada
#   US predicted by emerging markets
#   Canada predicted by emerging markets
#   
#   Asian regional partners
#   
#   Use some metric of centrality instead of particular trade relationships as data for models
#   degree centrality or betweenness centrality
#   will make use of network analysis instead of just being linear models of proportion.
#   can use China export approach as a comparison to centrality metrics.
#   
#   US centrality as a function of Brazil-China trade
  USAcent_BRA_CHN.lm = lm(btw.df$`United States` ~ ch.cast$Brazil)
  summary(USAcent_BRA_CHN.lm)

  CANcent_BRA_CHN.lm = lm(btw.df$Canada ~ ch.cast$Brazil)
  summary(CANcent_BRA_CHN.lm)
  
#   US degree as a function of Brazil degree and Argentina degree
  USAdeg_BRAdeg.lm = lm(deg.df$`United States` ~ deg.df$Brazil)
  summary(USAdeg_BRAdeg.lm)
  
  USAdeg_ARGdeg.lm = lm(deg.df$`United States` ~ deg.df$Argentina)
  summary(USAdeg_ARGdeg.lm)
  
#   plots
  pdf(file.path(soypath, "spillovers.pdf"))
  par(mfrow=c(2,2))
  plot(ch.cast$Brazil, btw.df$`United States`, xlab="Brazil-China Exports", ylab="US Betweenness")
  plot(ch.cast$Brazil, btw.df$Canada, xlab="Brazil-China Exports", ylab="Canada Betweenness")
  plot(deg.df$Brazil, deg.df$`United States`, xlab="Brazil Degree", ylab="US Degree")
  plot(deg.df$Argentina, deg.df$`United States`, xlab="Argentina Degree", ylab="US Degree")
  dev.off()
  
  
#   What centrality metric takes magnitude into account? How to weight centrality by edges? 
#   If they are weighted by strength of edges, then we are set.
#   ### According to Wikipedia, betweeness is a weighted metric.

  