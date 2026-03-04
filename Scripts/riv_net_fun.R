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

riv_net_stat <- function(net, sink_nodes = NULL, plot = F){

  # disable traceback
  options(error = NULL)

  # check for right net input
  if(!("sfnetwork"  %in% class(net))){
    stop("net must be a sfnetwork object!", .call = F)
  }

  # check for right plot input
  if(!is.logical(plot)){
    stop("plot must be TRUE or FALSE!", .call = F)
  }

    # get edge info
    edges <- st_as_sf(net, "edges")
    if(!("geom" %in% colnames(edges))){
      edges <- rename(edges, "geom" = attr(edges, "sf_column"))}
    edges <- edges %>% distinct(geom, .keep_all = T)
    unc_edges <- edges %>% filter(!(to  %in%  from))
    src_edges <- edges %>% filter(!(from  %in%  to))
    conv_edges <- edges %>% filter(to %in% from) %>% group_by(to) %>% filter(n() > 1)
    div_edges <- edges %>% filter(from %in% to) %>% group_by(from) %>% filter(n() > 1)
    crash_edges <- edges %>% filter(!(to  %in% from)) %>% group_by(to) %>% filter(n() > 1)
    mltsrc_edges <- edges %>% filter(!(from %in% to)) %>% group_by(from) %>% filter(n() > 1)

    nodes <- st_as_sf(net, "nodes") %>% mutate("ID" = rownames(.))

    if(is.null(sink_nodes)){

    # print info
    cat(
      "total edges:", nrow(edges), "\n",
      "sources:", src_edges %>% nrow(), "\n",
      "unconnected & sink edges:",unc_edges %>% nrow(), "\n",
      "convergences:", nodes %>% filter(ID  %in% conv_edges$to) %>% nrow(), "\n",
      "divergences:", nodes %>% filter(ID  %in% div_edges$from) %>% nrow(), "\n",
      "crashflows:", crash_edges %>% nrow()/2, "\n",
      "multi-edge sources:", mltsrc_edges %>% nrow()/2
      )

    } else {

      # check for sink_nodes presence in net
      if("sf" %in% class(sink_nodes)){
        if(any(st_geometry_type(sink_nodes) != "POINT")){
          stop("sink_nodes must have POINT geometry!")
        }
        else{
          if(!any(sink_nodes$geom %in% st_as_sf(net, "nodes")$geom)){
            stop("sink_nodes geom not present in network!")
          }
        }

      }
      else{
        if(!any(sink_nodes %in% rownames(st_as_sf(net, "nodes")))){
          stop("sink_nodes IDs not present in network!")
          }
        }

      # get edge info
      if("sf" %in% class(sink_nodes)){
        sink_edges <- edges %>% st_filter(sink_nodes, .predicate = st_touches)
        unc_edges <- edges %>% filter(!(to  %in%  from | geom  %in% sink_edges$geom))
        sink_nodes <- st_geometry_type(sink_nodes)
      }

      else{
      unc_edges <- edges %>% filter(!(to  %in%  from | to  %in% sink_nodes))
      }

      # print info
      cat(
        "total edges:", nrow(edges), "\n",
        "sources:", src_edges %>% nrow(), "\n",
        "sinks:", sink_nodes %>% length(), "\n",
        "convergences:", nodes %>% filter(ID  %in% conv_edges$to) %>% nrow(), "\n",
        "divergences:", nodes %>% filter(ID  %in% div_edges$from) %>% nrow(), "\n",
        "crashflows:", crash_edges %>% nrow()/2, "\n",
        "multi-edge sources:", mltsrc_edges %>% nrow()/2
      )
      }

  # plot
  if(plot == T) {
    tm_shape(filter(edges, !(to %in% from)), name = "unconnected edges")+tm_lines(lwd = 3, id = "to", col = "orange")+
      tm_shape(filter(edges, !(geom %in% unc_edges$geom)), name = "connected edges")+tm_lines(lwd = 3, id = "to", col = "darkgreen")+
      tm_shape(filter(nodes, ID  %in% unc_edges$to), name = "unconnected nodes")+tm_dots(col = "darkred")+
      tm_shape(filter(nodes, ID  %in% div_edges$from), name = "divergence nodes")+tm_dots(col = "white")+
      tm_shape(filter(nodes, ID  %in% crash_edges$to), name = "crashflow nodes")+tm_dots(col = "red")+
      tm_shape(filter(nodes, ID  %in% mltsrc_edges$from), name = "multi-edge sources")+tm_dots(col = "violet")
  }
}

riv_net_connect <- function(net, sink_nodes = NULL, round_coords = NULL, blend_tol = Inf){

  # disable traceback
  options(error = NULL)

  # check for right class
  if(!("sfnetwork"  %in% class(net))){
    stop("net must be a sfnetwork object!", .call = F)
  }

  # check for right precision input
  if(!is.null(round_coords)){
    if(!is.numeric(round_coords)){
      stop("precision must be a numeric value!", .call = F)
      }
    }

  #check right input for blend_tol
  if(!is.numeric(blend_tol)){
    stop("precision must be a numeric value!", .call = F)
  }

  # check for right length unit
  if(st_crs(net, parameters = T)$units_gdal != "metre"){
    stop("net CRS must have metre as length unit!")
  }

  # check right sink_nodes input
  if(is.null(sink_nodes)){
    stop("must provide sink_nodes to connect network correctly!")
  }

  # get edges and nodes
  edges <- st_as_sf(net, "edges")
  if(!("geom" %in% colnames(edges))){
    edges <- rename(edges, "geom" = attr(edges, "sf_column"))}
  edges <- edges %>% distinct(geom, .keep_all = T)

  nodes <-  st_as_sf(net, "nodes") %>% mutate("ID" = rownames(.))
  if(!("geom" %in% colnames(nodes))){
    edges <- rename(nodes, "geom" = attr(nodes, "sf_column"))}
  nodes <- nodes %>% distinct(geom, .keep_all = T)

  # check for sink nodes presence
  if("sf" %in% class(sink_nodes)){
    if(any(st_geometry_type(sink_nodes) != "POINT")){
      stop("sink_nodes must have POINT geometry!")
    }
    else{
      if(!any(sink_nodes$geom %in% st_as_sf(net, "nodes")$geom)){
        stop("sink_nodes geom not present in network!")
      }
    }

  }
  else{
    if(!any(sink_nodes %in% rownames(st_as_sf(net, "nodes")))){
      stop("sink_nodes IDs not present in network!")
    }
  }

  # sink edges
  if("sf" %in% class(sink_nodes)){
    sink_edges <- edges %>% st_filter(sink_nodes, .predicate = st_touches)
  }
  else{
    sink_edges <- edges %>% filter(to  %in%  sink_nodes)
  }

  # identify unconnected edges
  unc_edges <- edges %>% filter(!(to %in% from | geom  %in% sink_edges$geom))
  unc_edges_prev <- 0

  # loop untill number of unconnected edges stays constant
  while(!(nrow(unc_edges) == unc_edges_prev)){

    # override value
    unc_edges_prev <- nrow(unc_edges)

    # override nodes from new net
    nodes <- net %>% st_as_sf("nodes") %>% mutate("ID" = rownames(.))
    unc_nodes <- nodes %>% filter(ID  %in% unc_edges$to)

    # blend in unconnected nodes
    net_2 <- edges %>% filter(!(geom  %in% unc_edges$geom)) %>% as_sfnetwork() %>%
      st_network_blend(unc_nodes, tolerance = set_units(blend_tol, "m"))

    # get sf of edges
    edges_2 <- st_as_sf(net_2, "edges")

    # round coordinates
    if(!is.null(round_coords)){
      if(st_is_longlat(net)){
        st_geometry(edges_2) <- st_geometry(edges_2) %>%
          lapply(function(x) round(x, round_coords)) %>%
          st_sfc(crs = st_crs(edges_2))

        st_geometry(sink_edges) <- st_geometry(sink_edges) %>%
          lapply(function(x) round(x, round_coords)) %>%
          st_sfc(crs = st_crs(sink_edges))

        st_geometry(unc_edges) <- st_geometry(unc_edges) %>%
          lapply(function(x) round(x, round_coords)) %>%
          st_sfc(crs = st_crs(unc_edges))
        }

      else{
        st_geometry(edges_2) <- st_geometry(edges_2) %>%
          lapply(function(x) round(x, -round_coords)) %>%
          st_sfc(crs = st_crs(edges_2))

        st_geometry(sink_edges) <- st_geometry(sink_edges) %>%
          lapply(function(x) round(x, -round_coords)) %>%
          st_sfc(crs = st_crs(sink_edges))

        st_geometry(unc_edges) <- st_geometry(unc_edges) %>%
          lapply(function(x) round(x, -round_coords)) %>%
          st_sfc(crs = st_crs(unc_edges))
        }
      }

    # combine with unconnected edges
    net <- bind_rows(edges_2, unc_edges) %>% as_sfnetwork() %>% convert(to_spatial_smooth)

    # override edges from new net
    edges <- net %>% st_as_sf("edges")
    unc_edges <- edges %>% filter(!(to %in% from | geom  %in% sink_edges$geom))
  }

  return(net)
}

riv_net_reverse <- function(net, edge_index){

  # check for right net input
  if(!("sfnetwork"  %in% class(net))){
    stop("net must be a sfnetwork object!", .call = F)
  }

  # check for right edge_index input
  if(!any(edge_index %in% unlist(st_as_sf(net, "edges"), ".tidygraph_edge_index"))){
    stop("edge_index not found in net")
  }

  net %>% st_as_sf("edges") %>%
    mutate(geom = case_when(.tidygraph_edge_index  %in% edge_index ~ st_reverse(geom), .default = geom)) %>%
    as_sfnetwork() %>%
  return(.)

}

riv_net_shreve <- function(net, start) {

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
