# riv_net functions ####
# Descriptions ####
# This script is a collection of helper functions for the analysis
# and processing of stream networks relying mainly on the
# sfnetworks package.

## riv_net_stat ####
# This function returns and plots information about the network:
# - total number of edges
# - number of sources
# - number of unconnected and sink edges
# - number of diverging edges
# - number of crashflow edges (confluences with no outflow)

## riv_net_shreve ####
# This function calculates the Shreve stream magnitude, that is
# the number of sources upstream of a given point. The function
# is a simpler derivate of the upstream_summarize function.

## Code ####

riv_net_stat <- function(net, plot = F){
  # required packages
  require(c("dplyr", "sfnetworks", "tmap"))

  # disable traceback
  options(error = NULL)

  # check for right class
  if(!("sfnetwork"  %in% class(net))){
    stop("input must be a sf_network object!")
  }

  # check for right plot input
  if(!is.bool(plot)){
    stop("plot must be TRUE or FALSE")
  }

  # get edge info
  edges <- st_as_sf(net, "edges")
  if(!("geom" %in% colnames(edges))){
    edges <- rename(edges, "geom" = attr(edges, "sf_column"))}
  edges %>% distinct(geom, .keep_all = T)
  unc_edges <- edges %>% filter(!(to  %in%  from))
  crash_edges <- edges %>% group_by(to) %>% filter((n() > 1) & !(to  %in% edges$from)) %>% ungroup()
  nodes <- st_as_sf(net, "nodes") %>% mutate("ID" = rownames(.))

  # print info
  cat(
    "total edges:", nrow(edges), "\n",
    "number of sources:", filter(edges, !(from %in% to)) %>% nrow(), "\n",
    "unconnected & sink edges:",unc_edges %>% nrow(), "\n",
    "number of diverging edges:", group_by(edges, from) %>% filter(n() > 1) %>% nrow(), "\n",
    "number of crashflow edges:", group_by(edges, to) %>% filter((n() > 1) & !(to  %in% edges$from)) %>% nrow()
  )

  # plot
  if(plot == T) {
    tm_shape(filter(edges, !(to %in% from)), name = "unconnected edges")+tm_lines(lwd = 3, id = "to", col = "orange")+
      tm_shape(filter(edges, !(geom %in% unc_edges$geom)), name = "connected edges")+tm_lines(lwd = 3, id = "to", col = "darkgreen")+
      tm_shape(filter(nodes, ID  %in% unc_edges$to), name = "unconnected nodes")+tm_dots(col = "darkred")+
      tm_shape(filter(nodes, ID  %in% crash_edges$to), name = "crashflow nodes")+tm_dots(col = "red")
  }
}

riv_net_shreve <- function(net, start) {
  # required packages
  require(c("dplyr", "sfnetworks"))

  # disable traceback
  options(error = NULL)

  # Check for valid class of net input
  if (!is.sfnetwork(net)) {
    stop("net must be an sfnetwork object!", call. = F)  # check for valid network input
  }

  # Check for presence of start node in net
  if (any(!(start %in% rownames(st_as_sf(net, "nodes"))))) {
    stop("start node not present in net!", call. = F)
  }


  # Perform filtering steps
  nodes_us <- suppressWarnings(igraph::shortest_paths(net, from = start, to = igraph::V(net), mode = "in")) %>%
    unlist(.$vpath) %>%
    unique()

  sub_nodes <- st_as_sf(net, "nodes") %>% mutate(igraph_ID = seq.int(nrow(.))) %>% .[nodes_us,]

  # Calculate Shreve
  tab_shreve <- tibble(
    Shreve = st_as_sf(net, "edges") %>% filter(from %in% sub_nodes$igraph_ID & !(from %in% to)) %>% nrow()
  )

  # Return the summarized table with sums
  return(tab_shreve)
}
