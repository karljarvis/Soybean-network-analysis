## May 24, 2016

## ggnet2 examples
## https://briatte.github.io/ggnet/

library(GGally)

##The package dependencies of ggnet2 are, on the one hand, the network and sna packages 
##for network manipulation, and the ggplot2 package for plot construction.

library(network)
library(sna)
library(ggplot2)

##Additionally, ggnet2 suggests the following packages:
  
##If the RColorBrewer package is installed, ggnet2 will be able to use ColorBrewer palettes 
##to color network nodes.
##If the intergraph package is installed, ggnet2 will be able to process one-mode networks 
## objects created with the igraph package.


#################################################
## Example (1): Random graph
#################################################
## Let's start with an undirected Bernoulli random graph, with 10 nodes named "a, b, ., i, j", and a rather high likelihood of an edge to exist between them:
  
## random graph
net = rgraph(10, mode = "graph", tprob = 0.5)
net = network(net, directed = FALSE)

## vertex names
network.vertex.names(net) = letters[1:10]

## This graph can be visualized with ggnet2 without any further work:
ggnet2(net)

## "net"  can be a network object or any object that can be coerced to that class through its edgeset.constructors functions, such as adjacency matrixes, 
## incidence matrixes and edge lists. If the intergraph package is installed, net can also be an igraph one-mode network object, which is the only type of 
## network that the package can convert from the igraph to the network class.


################################
## Changing node color and size
ggnet2(net, node.size = 6, node.color = "black", edge.size = 1, edge.color = "grey")

## The vertex-related arguments of ggnet2 start with node, and its edge-related arguments 
## start with edge. The node.color and node.size arguments can be abbreviated:
ggnet2(net, size = 6, color = "black", edge.size = 1, edge.color = "grey")

## It also possible to pass a vector of node colors directly to ggnet2, as long as it has the same number of elements as the network has nodes:
ggnet2(net, size = 6, color = rep(c("tomato", "steelblue"), 5)) # Color 1/2 nodes with tomato and 1/2 with steelblue


#################
## Node Placement
## By default, ggnet2 places nodes with the Fruchterman-Reingold force-directed algorithm. Just like the plot.network function of the network package, it supports all node placement algorithms provided by the sna package, such as these:
  
ggnet2(net, mode = "circle")
ggnet2(net, mode = "kamadakawai")

## See the documentation of the gplot.layout function for the list of placement algorithms. 
## ggnet2 also supports passing options to the algorithm through the layout.par argument:
  
ggnet2(net, mode = "fruchtermanreingold", layout.par = list(cell.jitter = 0.75))
ggnet2(net, mode = "target", layout.par = list(niter = 100))


################
## Node colors
## Let's now assign a vertex attribute called phono, which indicates whether the name of the 
## vertex is a vowel or a consonant:
  
net %v% "phono" = ifelse(letters[1:10] %in% c("a", "e", "i"), "vowel", "consonant")

## This attribute can be passed to ggnet2 to indicate that the nodes belong to a group. 
## All the user has to do is to pass the name of the vertex attribute to the color argument, 
## which will find it in the list of vertex attributes and use it to map the colors of the 
## nodes:
  
ggnet2(net, color = "phono")

## By default, ggnet2 assigns a grayscale color to each group. To modify this behavior, 
## let's review three different options. The first one consists in "hard-coding" the colors 
## into the graph by assigning them to a vertex attribute, and then in passing this attribute 
## to ggnet2:

net %v% "color" = ifelse(net %v% "phono" == "vowel", "steelblue", "tomato")
ggnet2(net, color = "color")


## Other options are to pass the color legend as a named vector through the palette argument, 
## just like ggplot2 allows through the values argument of the scale_color_manual controller, 
## or to generate the color vector "on the fly", directly in the function call:
  
ggnet2(net, color = "phono", palette = c("vowel" = "steelblue", "consonant" = "tomato"))
ggnet2(net, color = ifelse(net %v% "phono" == "vowel", "steelblue", "tomato"))

## A final option is to use pre-defined color palettes. If the RColorBrewer package is installed 
## and palette refers to the name of any ColorBrewer palette, ggnet2 will try to use it to color 
## the nodes, or will return an error if there are not enough colors in the palette:
  
ggnet2(net, color = "phono", palette = "Set2")


#############
## Node sizes
## It is common to size the nodes of a network by their centrality or by some other indicator of 
## interest. Just like its color argument, the size argument of ggnet2 can take a single numeric 
## value, a vector of values, or a vertex attribute:
  
ggnet2(net, size = "phono")
## In similar fashion to how the color argument works, the actual size of the nodes can be styled 
## by a 'palette' argument, called size.palette. This allows to create nodes of highly unequal 
## sizes that will be more visually distinguishable:
  
ggnet2(net, size = "phono", size.palette = c("vowel" = 10, "consonant" = 1))

## When the size attribute is not a single numeric value, the maximum size of the nodes is 
## determined by the max_size argument, just like in the scale_size_area controller of ggplot2, 
## which ggnet2 emulates to compute the relative size of the nodes:
  
ggnet2(net, size = sample(0:2, 10, replace = TRUE), max_size = 9)

## ggnet2 can also size nodes by calculating their in-degree, out-degree, or total (Freeman) 
## degree, using the degree function of the sna package. All the user has to do is to pass the 
## indegree, outdegree, or freeman option to the weight argument (degree is also understood, 
## and is equivalent to freeman):
  
ggnet2(net, size = "degree")

## ggnet2 gives the user further control over the node size by providing a quick way to cut the 
## node sizes into quantiles, using the size.cut argument. If set to TRUE, it defaults to quartiles, 
## but any numeric value above 1 is acceptable:
  
ggnet2(net, size = "degree", size.cut = 3)

## In the example above, ggnet2 calculated the total degree of the nodes, and then cut them into 
## tertiles. If there are not enough distinct values to create the number of quantiles passed to 
## size.cut, ggnet2 will use the closest possible number.

## When size contains numeric values, ggnet2 can subset the graph based on these, which is useful 
## when plotting large networks. The arguments size.min and size.max achieve this functionality, 
## and let the user know how many nodes they removed:

# remove any isolated nodes
x = ggnet2(net, size = "degree", size.min = 1)
## size.min removed 0 nodes out of 10

# remove all nodes
x = ggnet2(net, size = "degree", size.max = 1)
## size.max removed 10 nodes out of 10
## Warning in ggnet2(net, size = "degree", size.max = 1): size.max removed all
## nodes; nothing left to plot

## Last, the size.zero argument controls whether ggnet2 should accept to plot zero-sized nodes. 
## The argument is FALSE by default, which ensures that every node gets plotted as a visible shape. 
## Set it to TRUE if you want zero-sized nodes in the plot:
  
ggnet2(net, size = sample(0:2, 10, replace = TRUE), size.zero = TRUE)


###############
## Node legends
## The alpha, color, shape and size arguments of ggnet2 produce ggplot2 legends that 
## are named after the vertex attributes that they carry. These names can be changed 
## with the alpha.legend, color.legend, shape.legend and size.legend arguments:
  
ggnet2(net, alpha = "phono", alpha.legend = "Phonetics")
ggnet2(net, shape = "phono", shape.legend = "Phonetics")
ggnet2(net, color = "phono", color.legend = "Phonetics")
ggnet2(net, size = "degree", size.legend = "Centrality")

## Another option is to remove these legends completely, as ggplot2 allows to do:
  
ggnet2(net, color = "phono", size = "degree") +
guides(color = FALSE, size = FALSE)

## A final option is to replace these legends with any compatible ggplot2 scale. 
## Due to how ggnet2 works internally, additional legends have to be discrete_scale 
## controllers, even when the scale applies to the size of the nodes:
  
# control the colors of the nodes
ggnet2(net, color = "phono") +
scale_color_brewer("", palette = "Set1",
                     labels = c("consonant" = "C", "vowel" = "V"),
                     guide = guide_legend(override.aes = list(size = 6)))

# control the size of the nodes
ggnet2(net, size = "degree") +
  scale_size_discrete("", range = c(5, 10), breaks = seq(10, 2, -2))

## The legends can be futher styled by modifying the theme of the plot, or by using 
## the shorthands built into ggnet2. The legend.text argument controls the size of the 
## legends symbols, text labels and title, and the legend.position argument controls 
## its placement:
ggnet2(net, color = "phono", legend.size = 12, legend.position = "bottom") +
  theme(panel.background = element_rect(color = "grey"))


#############
## Node labels
## Through the label argument, ggnet2 can label the nodes of a network by using their 
## vertex names, another vertex attribute, or any other vector of labels:
  
ggnet2(net, label = TRUE)
ggnet2(net, label = "phono")
ggnet2(net, label = 1:10)

## If label is a vector of values that does not contain exactly as many elements as the 
## number of nodes in the graph, ggnet2 will label the nodes that match one of these 
## values:
  
ggnet2(net, label = c("a", "e", "i"), color = "phono", label.color = "black")

## The size of the labels, which is automatically set to half of the node size, is 
## controlled by the label.size argument, their color by the label.color argument, and 
## their level of transparency by the label.alpha argument:
  
ggnet2(net, size = 12, label = TRUE, label.size = 5)
ggnet2(net, size = 12, label = TRUE, color = "black", label.color = "white")
ggnet2(net, label = TRUE, label.alpha = 0.75)

## Just like many of the other arguments in ggnet2, the label.alpha, label.color and 
## label.size arguments also accept vectors of values, or the name of a vertex attribute. 
## The example below also shows how to use a dark background with ggnet2:
  
ggnet2(net, color = "grey15", size = 12, label = TRUE, label.color = "color") +
  theme(panel.background = element_rect(fill = "grey15"))

## Node shapes and transparency
## The shapes and transparency of the nodes can be set exactly like the color and size of 
## the nodes, either through a single value, a vector of (numeric) values, or a vertex 
## attribute. This allows to create nodes that can be distinguished even in the plot 
## loses its colors:

ggnet2(net, color = "phono", shape = 15)
ggnet2(net, color = "phono", shape = "phono")

## Note: the second example above will return a warning about a duplicated plotting 
## parameter. This is an innocuous warning that is produced by mapping two 
## characteristics of the nodes to the same vertex attribute. It cannot be avoided 
## without modifying ggplot2.

## Again, just like color and size, the alpha and shape arguments can take manual 
## 'palettes' of values through the alpha.palette and shape.palette arguments, which 
## will bypass the default values assigned to these by ggplot2:
  
ggnet2(net, alpha = "phono", alpha.palette = c("vowel" = 0.2, "consonant" = 1))
ggnet2(net, shape = "phono", shape.palette = c("vowel" = 19, "consonant" = 15))

## Although ggnet2 is flexible about node shapes and transparency, node shapes are 
## difficult to distinguish when there are more than six different shapes in the plot, 
## and setting the transparency of the nodes to anything too low will also create 
## difficulties for the reader:
  
ggnet2(net, shape = sample(1:10))
ggnet2(net, alpha = "phono")


###################################################################################
## EXAMPLE 2: Bipartite network
#####################################################################################
## Note: the functionalities described in this section were inspired by a discussion 
## over bipartite graphs with Pedro Jordano, who has written more advanced code to 
## handle bipartite graphs with ggplot2: 
## see https://pedroj.github.io/bipartite_plots/

## ggnet2 automatically detects two-mode graphs from their bipartite network attribute. 
## To simplify the plotting of each mode, it understands arguments of the form 
## [alpha, color, shape, size] = "mode", 
## which will mark the primary mode as "actor" and the secondary mode as "event".

## Let's illustrate this functionality through the same example as shown in the 
## documentation of the network.bipartite function:

# weighted adjacency matrix
bip = data.frame(event1 = c(1, 2, 1, 0),
                 event2 = c(0, 0, 3, 0),
                 event3 = c(1, 1, 0, 4),
                 row.names = letters[1:4])

# weighted bipartite network
bip = network(bip,
              matrix.type = "bipartite",
              ignore.eval = FALSE,
              names.eval = "weights")

## By default, ggnet2 will not do anything particular to the network, treating it as 
## if it were a one-mode network object:
ggnet2(bip, label = TRUE)

## To use the mode of the nodes as the basis for their colors, all the user has to do is 
## to pass the color = "mode" argument, and then to style the "actor" and "event" values:

# set colors for each mode
col = c("actor" = "grey", "event" = "gold")

# detect and color the mode
ggnet2(bip, color = "mode", palette = col, label = TRUE)

## Let's use this network to show what ggnet2 can do to style edges in addition to nodes.


################
## Edge labels
## The edge.label argument accepts a character vector or an edge attribute, which will 
## be plotted at midpoint between the nodes that are connected to each other. Let's use 
## this argument to show the edge weights included in the bipartite network example 
## constructed above:
  
ggnet2(bip, color = "mode", palette = col, label = TRUE, edge.label = "weights")

## The color and size of the labels can be controlled with edge.label.color and 
## edge.label.size. The former argument defaults to label.color, the color used for 
## the node labels, and the latter argument defaults to label.size, the size of the 
## node labels.

ggnet2(bip, shape = "mode", edge.label = "weights", edge.label.color = "darkred")
ggnet2(bip, shape = "mode", edge.label = "weights", edge.label.size = 6)

## Both edge.label.color and edge.label.size also accept edge attributes. The example 
## below maps the color of the edge labels to the weight of the edges that they are 
## attached to:
  
set.edge.attribute(bip, "color", ifelse(bip %e% "weights" > 1, "black", "grey75"))
ggnet2(bip, shape = "mode", edge.label = "weights", edge.label.color = "color")

## By default, the edge.label argument will add a white background underneath the label, 
## in order to avoid overplotting edges and edge labels. The color of that background, 
## which is draw as a circle with geom_point, can be styled with edge.label.fill, or 
## removed completely by setting edge.label.fill to NA.

ggnet2(bip, shape = "mode", edge.label = "weights", edge.label.fill = NA)


######################
## Edge size and color
## At the very beginning of this vignette, we showed how to size the edges of the network 
## using a single value. However, in the context of a weighted network, the edge weight 
## information might also be used to size the edges proportionally by passing an edge 
## attribute to edge.size:
  
ggnet2(bip, color = "mode", palette = col, edge.size = "weights")

## The edge.size argument will also accept a vector of edge weights, as long as it 
## contains as many values as there are edges in the network.

## Similarly, the edge.color argument accepts either a single color value, or a vector 
## of them, as long as it contains as many values as there are edges in the network. 
## Because it also accepts an edge attribute, we can again map the color of the edge to 
## one of its other properties:
  
set.edge.attribute(bip, "color", ifelse(bip %e% "weights" > 1, "black", "grey75"))
ggnet2(bip, color = "mode", palette = col, edge.size = "weights", edge.color = "color")


#######################
## Edge linetype
## Like edge.color and edge.size, edge.lty accepts a single linetype value, or an edge 
## attribute, or a vector of edge linetypes, as long as it contains as many values as 
## there are edges in the network:
set.edge.attribute(bip, "lty", ifelse(bip %e% "weights" > 1, 1, 2))
ggnet2(bip, color = "mode", palette = col, edge.size = "weights", edge.lty = "lty")


####################################################################################
## ADDITIONAL OPTIONS
####################################################################################

## Edge arrows

## ggnet2 supports directed graphs, but has only minimal support for adding arrows for 
## edges with the arrow.size, arrow.gap and arrow.type arguments.

## The issue with edge arrows is that they will often get plotted below the nodes, as 
## in this example, which tries to set 12-point edge arrows:
  
ggnet2(network(rgraph(10, tprob = 0.25), directed = TRUE), arrow.size = 12)

## As a workaround, ggnet2 lets the user draw shorter edges, so that the arrows get 
## plotted before the nodes, as in this example:
  
ggnet2(network(rgraph(10, tprob = 0.25), directed = TRUE),
         arrow.size = 12, arrow.gap = 0.025)

## The arrow.gap argument more or less represents the fraction of the edge that will 
## be removed. Setting it to a value close (but not equal) to 0, such as in the example 
## above, should help to 'unmask' the edge arrows if they have been covered by the nodes.

## The arrow.type argument determines the shape of the arrow. It should be set to either 
## "closed" (the default), or "open".


#######################################
## Coloring edges from node attributes
## Some network plotting software, such as the Sigma.js library, allow the user to color 
## the edges of a graph in function of the nodes that they connect. This functionality 
## is useful to detect edges between nodes that belong to a same group, as in commonly 
## in graphs with strong group homophily.

## ggnet2 supports this functionality by allowing the edge.color argument to take the 
## c("color", "grey") value. The first value will tell ggnet2 to color edges between 
## nodes of the same group with the color of that group. The second value is the color 
## to use for edges that connect nodes belonging to different groups.

## Using the same random graph as we used previously, let's see which edges connect two 
## vowels, and which connect two consonants:

ggnet2(net, color = "phono", palette = "Set1", edge.color = c("color", "grey50"))


###########################################
## Removing nodes based on missing values
## If a vertex attribute name is passed to the na.rm argument of ggnet2, all nodes for 
## which this vertex attribute is missing (NA) will be discarded from the network before 
## plotting. This argument can come in handy in some circumstances, one of which is 
## illustrated below.

#####################################
## Using fixed placement coordinates
## In the special case of temporal networks, it is often useful to plot the nodes at 
## different points in time using the same placement coordinates, while showing some 
## variation on another parameter. Let's show how to do this with ggnet2, using a 
## simple example of node activation over t=3t=3 time intervals.

## First, let's define the placement coordinates of the full graph as two vertex 
## attributes:
  
x = gplot.layout.fruchtermanreingold(net, NULL)
net %v% "x" = x[, 1]
net %v% "y" = x[, 2]

## Next, let's define three binary vertex attributes indicating node activation through 
## time:
  
net %v% "t1" = c(0, 0, 0, 0, 0, 0, 1, 1, 1, 1)
net %v% "t2" = c(0, 0, 0, 0, 1, 1, 1, 1, 1, 1)
net %v% "t3" = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

## Finally, let's modify these attributes to indicate NA if the node is not yet 
## activated:
  
net %v% "t1" = ifelse(net %v% "t1", 1, NA)
net %v% "t2" = ifelse(net %v% "t2", 1, NA)
net %v% "t3" = ifelse(net %v% "t3", 1, NA)

## We can now create four ggnet2 plots, using each attribute in turn, setting na.rm 
## to TRUE in order to remove nodes that are not yet activated from the graphs.

t1 = ggnet2(net, mode = c("x", "y"), size = 3, color = "black", na.rm = "t1")
## na.rm removed 6 nodes out of 10
t2 = ggnet2(net, mode = c("x", "y"), size = 3, color = "black", na.rm = "t2")
## na.rm removed 4 nodes out of 10
t3 = ggnet2(net, mode = c("x", "y"), size = 3, color = "black", na.rm = "t3")
## na.rm removed 0 nodes out of 10

## When the mode argument is given two vertex attributes, it understands that these 
## attributes contain the placement coordinates to use for plotting. As a result, 
## all three plots will use the same coordinates for the nodes. We can then use the 
## gridExtra package to show all plots next to each other:
  
# common plotting parameters
b = theme(panel.background = element_rect(color = "grey50"))
z = guides(color = FALSE)
y = scale_y_continuous(limits = range(x[, 2] * 1.1), breaks = NULL)
x = scale_x_continuous(limits = range(x[, 1] * 1.1), breaks = NULL)

# show each temporal network
gridExtra::grid.arrange(t1 + x + y + z + ggtitle("t = 1") + b,
                        t2 + x + y + z + ggtitle("t = 2") + b,
                        t3 + x + y + z + ggtitle("t = 3") + b,
                        nrow = 1)


################################
## Expanding the horizontal axis
## When ggnet2 is used to plot a network with node labels, the labels close to the 
## margins of the plot panel might get clipped. This issue can be handled by expanding 
## the horizontal axis of the plot, using the layout.exp argument, as in this example:
  
# no horizontal expansion
ggnet2(net, label = rep("abcdefghijklmnopqrstuvwxyz", 10))

# 50% horizontal expansion
ggnet2(net, label = rep("abcdefghijklmnopqrstuvwxyz", 10), layout.exp = 0.5)


################################
## Hacking into internal values
## ggnet2 returns a ggplot object, so the underlying data can be accessed by requesting 
## the data component of the plot. The structure of that component always contains the 
## following columns, which match the names of ggplot2 arguments:
  
ggnet2(net, color = "phono", size = 1:10)$data

## This means that you can append any ggplot2 component to the graph by passing 
## additional aesthetics to it, which allows for a fair amount of "plot hacking". In 
## this example, we use ggnet2 to get the basic data structure in place, while sizing 
## the nodes to 0. The nodes are then plotted manually, by overlaying several geom 
## objects:

ggnet2(net, color = "phono", palette = "Set1", size = 0) +
  geom_point(aes(color = color), size = 12, color = "white") +
  geom_point(aes(color = color), size = 12, alpha = 0.5) +
  geom_point(aes(color = color), size = 9) +
  geom_text(aes(label = toupper(substr(color, 1, 1))), color = "white", fontface = "bold") +
  guides(color = FALSE)



## MORE EXAMPLES

################################################################################
## EXAMPLE 3: Icelandic legal code
################################################################################
## The network loaded by the shortlink below, which comes from this Gist, connects 
## articles of the Icelandic legal code by their cross-references. The data reflect 
## the state of the Icelandic legal code as of August 2015.
source("https://goo.gl/q1JFih")

## There are 845 nodes and over 1,500 edges in the network. Let's add a four-level 
## interval variable indicating the period at which each article was introduced, and 
## then assign some colors based on it:

x = cut_number(as.integer(net %v% "year"), 4)
col = c("#E1AF00", "#EBCC2A", "#78B7C5", "#3B9AB2")
names(col) = levels(x)

## The network is shown below with nodes sized by their out-degree and colored by their 
## period of introduction into Icelandic law, using the variable that we just defined:

ggnet2(net, color = x, color.legend = "period", palette = col,
         edge.alpha = 1/4, edge.size = "weight",
         size = "outdegree", max_size = 4, size.cut = 3,
         legend.size = 12, legend.position = "bottom") +
coord_equal()


################################################################################
## EXAMPLE 4: French MPs on Twitter
################################################################################
## The development repository of ggnet2 contains a dataset of 339 French Members 
## of Parliament (MPs), and the ties that they formed by following each other on 
## Twitter. The data are from May 2013 and come in two files that index the edges and the nodes separately:
  
# root URL
r = "https://raw.githubusercontent.com/briatte/ggnet/master/"

# read nodes
v = read.csv(paste0(r, "inst/extdata/nodes.tsv"), sep = "\t")
names(v)

# read edges
e = read.csv(paste0(r, "inst/extdata/network.tsv"), sep = "\t")
names(e)

## The network is constructed by converting the edge list into a network object. 
## The party affiliations of the MPs are then used to construct a manual color palette:
  
# network object
net = network(e, directed = TRUE)

# party affiliation
x = data.frame(Twitter = network.vertex.names(net))
x = merge(x, v, by = "Twitter", sort = FALSE)$Groupe
net %v% "party" = as.character(x)

# color palette
y = RColorBrewer::brewer.pal(9, "Set1")[ c(3, 1, 9, 6, 8, 5, 2) ]
names(y) = levels(x)

# network plot
ggnet2(net, color = "party", palette = y, alpha = 0.75, size = 4, edge.alpha = 0.5)

## Let's further visualize the amount of party homophily by coloring edges between MPs 
## who share the same partisan affiliation, and single out two MPs:
  
ggnet2(net, color = "party", palette = y, alpha = 0.75, size = 4, edge.alpha = 0.5,
         edge.color = c("color", "grey50"), label = c("BrunoLeRoux", "nk_m"), label.size = 4)


########################################################################################
## Known limitations
########################################################################################

## ggnet2 does not support all of the graph plotting options provided by the igraph and 
## network packages, and there are a few things that it does not do at all. Here is a 
## non-exhaustive list of things that ggnet2 does not handle:
  
## Curved edges: ggnet2 does not yet handle curved edges, although the next version of 
## ggplot2 might make it possible to implement these at some point.

## Self-loops: ggnet2 does not know how to handle self-loops, and will warn the user 
## about it. Self-loops will not show up in the plots created by ggnet2.

## Complex graphs: ggnet2 does not know how to handle hypergraphs or multiplex graphs, 
## and will return an error if asked to plot network objects with these properties.
## If you find other limitations to ggnet2, please submit an issue about them, thanks!
  
  