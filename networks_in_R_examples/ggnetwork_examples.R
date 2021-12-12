## ggnetwork vignette
## from: https://briatte.github.io/ggnetwork/#node-faceting

library(ggnetwork)
library(ggplot2)
library(igraph)
library(network)
library(sna)

## minimal example
n <- network(rgraph(10, tprob = 0.2), directed = FALSE)

## add categorical and continuous attributes for both edges and vertices. 
## Add a categorical vertex attribute "family", set to "a", "b", or "c". 
## Add a continuous vertex attribute, "importance" set to 1, 2, or 3. 

n %v% "family" <- sample(letters[1:3], 10, replace = TRUE)
n %v% "importance" <- sample(1:3, 10, replace = TRUE)

##  Add a categorical edge attribute called "type", which is set to either 
## "x", "y" or "z", and a continuous vertex attribute called "day", which 
## is set to either 1, 2 or 3.

e <- network.edgecount(n)
set.edge.attribute(n, "type", sample(letters[24:26], e, replace = TRUE))
set.edge.attribute(n, "day", sample(1:3, e, replace = TRUE))


ggnetwork(n, layout = "fruchtermanreingold", cell.jitter = 0.75) #doesn't work!
ggnetwork(n, layout = "target", niter = 100)

head(ggnetwork(n))
tail(ggnetwork(n))

## The data frame returned by ggnetwork has (N + E) rows, where N is the number of 
## nodes of the network, and E its number of edges. This data format is very likely 
## to include duplicate information about the nodes, which is unavoidable.

## Note that ggnetwork does not include any safety mechanism against duplicate column 
## names. As a consequence, if there is both a vertex attribute called "na" and an edge 
## attribute called "na", as in the example above, then the vertex attribute will be 
## renamed "na.x" and the edge attribute will be renamed "na.y".

ggplot(n) ## ggplot recognizes igraph and network graph objects
## if n is a matrix or edge list to be coerced to a network, use ggnetwork to pass the object to ggplot2
ggplot(ggnetwork(n)) 


## draw the network edges using geom_edges. We map the type attribute to the linetype of the edges. 
ggplot(n, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(aes(linetype = type), color = "grey50") +
  theme_blank()

## The other aesthetics that we mapped are the basic coordinates of the network plot. 
## These might also be set as part of the call to geom_segment, but setting them at 
## the root of the plot avoids having to repeat them in additional geoms.

## Note that geom_edges can also produce curved edges by setting its curvature argument 
## to any value above 0 (the default):
  
ggplot(n, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(aes(linetype = type), color = "grey50", curvature = 0.1) +
  theme_blank()

## Let's now draw the nodes using geom_nodes, which is just a lightly hacked version 
## of geom_point. In the example below, we map the family vertex attribute to the color 
## of the nodes, and make the size of these nodes proportional to the importance vertex 
## attribute:
ggplot(n, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(aes(linetype = type), color = "grey50") +
  geom_nodes(aes(color = family, size = importance)) +
  theme_blank()

## Because ggplot2 follows Wilkinson's grammar of graphics, it accepts only one color 
## scale. In the example above, that scale is mapped to a vertex attribute, but it 
## could have also been mapped to an edge attribute. Mapping a color to both a vertex 
## attribute and an edge attribute will create a single color scale that incorrectly 
## merges both attributes into one:

ggplot(n, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(aes(color = type)) +
  geom_nodes(aes(color = family)) +
  theme_blank()

## This is a limitation of ggnetwork that would require violating some fundamental 
## aspects of the grammar of graphics to be circumvented.

## Let's now add node labels. These are simply plotted over the nodes by the nodetext 
## geom, which works exactly like geom_text. In the example below, we map the 
## vertex.names attribute (which contains numbers 1 to 10) to uppercase letters:

ggplot(n, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(color = "black") +
  geom_nodes(color = "black", size = 8) +
  geom_nodetext(aes(color = family, label = LETTERS[ vertex.names ]),
                fontface = "bold") +
  theme_blank()

## If you prefer to use the geom_label geom recently introduced in ggplot2, ggnetwork 
## also supports these through the nodelabel geom:
  
ggplot(n, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(color = "black") +
  geom_nodelabel(aes(color = family, label = LETTERS[ vertex.names ]),
                 fontface = "bold") +
  theme_blank()

## ggnetwork supports the repulsive label functions introduced by the ggrepel package, 
## which allows to label nodes with non-overlapping annotations. Simply add _repel to 
## either geom_nodetext or geom_nodelabel to use that functionality:
  
ggplot(n, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(color = "black") +
  geom_nodelabel_repel(aes(color = family, label = LETTERS[ vertex.names ]),
                       fontface = "bold", box.padding = unit(1, "lines")) +
  geom_nodes(color = "black", size = 8) +
  theme_blank()

## Let's now add edge labels. These are plotted at mid-distance of the nodes that the 
## edges connect by the edgetext geom, which works exactly like geom_label, except that 
## its default arguments do not draw a border around the labels. Here's an example where 
## we map the day edge attribute to edge labels:
ggplot(n, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(aes(linetype = type), color = "grey75") +
  geom_nodes(color = "gold", size = 8) +
  geom_nodetext(aes(label = LETTERS[ vertex.names ])) +
  geom_edgetext(aes(label = day), color = "white", fill = "grey25") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_rect(fill = "grey25"),
        panel.grid = element_blank())

## The edgelabel geom is just an alias of the edgetext geom. Note that these geoms are 
## unlikely to produce adequate results if the edges produced by geom_edges are curved.

## As you would do with nodes, simply add _repel to either geom_edgetext or 
## geom_edgelabel to draw repulsive edge labels:
  
ggplot(n, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(aes(linetype = type), color = "grey75") +
  geom_nodes(color = "gold", size = 8) +
  geom_nodetext(aes(label = LETTERS[ vertex.names ])) +
  geom_edgetext_repel(aes(label = day), color = "white", fill = "grey25",
                      box.padding = unit(1, "lines")) +
theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_rect(fill = "grey25"),
        panel.grid = element_blank())

## Edge arrows
## ggnetwork uses code by Heike Hoffmann to better show arrows in directed graphs. 
## To illustrate this, we will need a directed graph example, so let's use the first 
## of the seven emon networks bundled in the network package:
data(emon)
emon[[1]]
## If this network is passed to ggnetwork without any further plotting parameter, the 
## result will feature "shortened" edges that do not reach their receiver nodes:

## This is because directed networks are expected to be plotted with edge arrows 
## indicating the directedness of each edge. Adding edge arrows with geom_edges works 
## through the same call to the arrow function that is supported by geom_segment:
  
ggplot(emon[[1]], aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(arrow = arrow(length = unit(6, "pt"), type = "closed")) +
  geom_nodes(color = "tomato", size = 4) +
  theme_blank()

## Edge weights
## ggnetwork can use an edge attribute as edge weights when computing the network layout. 
## The name of that edge attribute should be passed to the weights argument for that to 
## happen, as in this example, which will produce different layouts than if weights had 
## been left set to NULL (the default):
  
ggnetwork(emon[[1]], weights = "Frequency")

## The Kamada-Kawai is one example of a network layout that supports edge weights. The 
## user should refer to the documentation of each network layout to understand which of 
## these can make use of edge weights.

## If ggnetwork finds duplicated edges in a network, it will return a warning, as these 
## edges should probably have been converted to single weighted edges for adequate 
## plotting.


## Node faceting
## In order for ggnetwork to operate correctly with faceted plots, the by argument, 
## which is NULL by default, can be set to the name of an edge attribute. The result 
## will be a longer data frame that can be plotted with either facet_wrap or facet_grid, 
## as in the example below, where the faceting variable, the Frequency edge attribute, 
## has to be specified twice (once to ggnetwork, once to facet_wrap):
  
ggplot(ggnetwork(emon[[1]], arrow.gap = 0.04, by = "Frequency"),
         aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(arrow = arrow(length = unit(6, "pt"), type = "closed"),
             aes(color = Sponsorship)) +
  geom_nodes(aes(color = Sponsorship), size = 4) +
  facet_wrap(~ Frequency) +
  theme_facet()

## The by argument is basically an attempt to bring minimal support for temporal 
## networks in ggnetwork. It will systematically show all nodes in all plot facets, 
## using the same coordinates in every facet. For more advanced plots of dynamic 
## networks, the user should turn to the ndtv and tsna packages.

## The example above also shows how to use a vertex attribute as part of the aesthetics 
## of the edges. Given how ggnetwork operates, these vertex attributes will always be 
## those of the sender node.

## Last, the example also shows that ggnetwork comes with a theme called theme_facet. 
## This theme is a variation of the previously mentioned theme_blank that preserves its 
## facet boxes:
  
  theme_facet
## function(base_size = 12, base_family = "", ...) {
##   theme_blank(base_size = base_size, base_family = base_family) +
##     ggplot2::theme(
##       panel.border = ggplot2::element_rect(fill = NA, color = "grey50"),
##       ...
##     )
## }
## <environment: namespace:ggnetwork>

  
###########################################################################
## Additional methods
## Since ggnetwork works entirely through ggplot2, all ggplot2 methods apply:
    
ggplot(n, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_edges(aes(linetype = type), color = "grey50") +
    geom_nodes(aes(x, y, color = family, size = 1.5 * importance)) +
    geom_nodetext(aes(label = LETTERS[ vertex.names ], size = 0.5 * importance)) +
    geom_edgetext(aes(label = day), color = "grey25") +
    scale_color_brewer(palette = "Set2") +
    scale_size_area("importance", breaks = 1:3, max_size = 9) +
    theme_blank()

## Similarly, it is possible to use any of the geometries more than once per plot:
  
ggplot(n, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(color = "grey50", alpha = 0.5) +
  geom_nodes(aes(x, y, color = family, size = 5.5 * importance), alpha = 0.5) +
  geom_nodes(aes(x, y, color = family, size = 1.5 * importance)) +
  scale_color_brewer(palette = "Set1") +
  guides(size = FALSE) +
  theme_blank()

## Last, all geoms provided by ggnetwork can be subsetted through the data argument, 
## just as any ggplot2 geom, and as in the example below, which draws only a subset 
## of all node labels:
  
ggplot(n, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(color = "grey50", alpha = 0.5) +
  geom_nodes(aes(x, y, color = family), size = 3) +
  geom_nodelabel_repel(aes(label = vertex.names),
                       box.padding = unit(1, "lines"),
                       data = function(x) { x[ x$family == "a", ]}) +
  scale_color_brewer(palette = "Set1") +
  theme_blank()

