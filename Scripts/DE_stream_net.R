# Setup ####

library(tidyverse)
library(units)
library(sf)
library(sfnetworks)
library(tidygraph)
library(whitebox)
library(tmap)
library(doParallel)
library(parallelMap)

setwd("C:/Users/Daniel Enns/Documents/Promotion/Nathan stuff")

source("C:/Users/Daniel Enns/Documents/Promotion/MZB/Enns-et-al.-3/R/Shreve.R")

tmap_mode("view")

# set up stream network ####

# load network
streams <- st_read("./UETK_2024-05-02.gdb", layer = "upes_l")

# convert to  sf_network
str_net <- streams %>% st_cast("LINESTRING") %>% as_sfnetwork(., precision = 10) %>% convert(to_spatial_smooth)

# extract edges and nodes
edges <- st_as_sf(str_net, "edges")
nodes <- st_as_sf(str_net, "nodes") %>% mutate("ID" = rownames(.))

# visualize
tm_shape(edges)+tm_lines(lwd = 2, id = "to")+tm_shape(filter(nodes, ID == "1755"))+tm_dots(col="red", id = "ID")

# reverse edges
rev_edges <- edges %>% filter(
  from == "756" & to == "757"|
  from == "5761" & to == "5760"|
  from == "2177" & to == "7965"|
  from == "4864" & to == "4863"|
  from == "464" & to == "465"|
  from == "6850" & to == "6851"|
  from == "5004" & to == "5005"|
  from == "6602" & to == "6603"|
  from == "10163" & to == "10162"|
  from == "5708" & to == "5709"|
  from == "9778" & to == "9777"|
  from == "6843" & to == "6842"|
  from == "9322" & to == "9320"|
  from == "10147" & to == "10148"|
  from == "8757" & to == "8755"|
  from == "7646" & to == "7647"|
  from == "7229" & to == "7230"|
  from == "10147" & to == "10148"|
  from == "8441" & to == "8442"|
  from == "6115" & to == "6113"|
  from == "358" & to == "7982"|
  from == "5109" & to == "5110"
)

non_rev_edges <- edges %>% filter(!(SHAPE %in% rev_edges$SHAPE))

# build corrected network
net_1 <- bind_rows(non_rev_edges, st_reverse(rev_edges)) %>% as_sfnetwork(., precision = 10) %>% convert(to_spatial_smooth)

# get edges and nodes
edges_1 <- st_as_sf(net_1, "edges")
nodes_1 <-  st_as_sf(net_1, "nodes") %>% mutate("ID" = rownames(.))

# identify unconnected edges and nodes
unc_edges <- edges_1 %>% filter(!(to %in% from))
unc_nodes <- nodes_1 %>% filter(ID  %in% unc_edges$to)

# copy fully connected and left over edges
con_edges <- edges_1 %>% filter(!(from  %in% unc_edges$from))
lo_edges <- edges_1 %>% filter(!(SHAPE %in% c(unc_edges$SHAPE, con_edges$SHAPE)))

# visualize
tm_shape(unc_edges)+tm_lines(lwd = 2, id = "to", col = "yellow")+
  tm_shape(con_edges)+tm_lines(lwd = 2, id = "to", col = "green")+
  tm_shape(lo_edges)+tm_lines(lwd = 2, id = "to", col = "orange")+
  tm_shape(filter(nodes_1, ID  %in% c(unc_edges$from, unc_edges$to)))+ tm_dots(col = "red", id = "ID")

# blend in unconnected nodes
net_2 <- bind_rows(con_edges, lo_edges) %>% as_sfnetwork() %>% st_network_blend(unc_nodes, tolerance = set_units(10, "m"))

# get sf of edges
edges_2 <- st_as_sf(net_2, "edges")

# combine with unconnected edges
net_comp <- bind_rows(edges_2, unc_edges) %>% as_sfnetwork(., precision = 10) %>% convert(to_spatial_smooth)

# get edges and nodes of complete network
edges_comp <- st_as_sf(net_comp, "edges")
nodes_comp <- st_as_sf(net_comp, "nodes") %>% mutate("ID" = rownames(.))

# calculate Shreve stream magnitude ####

# set up cluster
num_cores <- (detectCores()-1)
num_cores %>% makeCluster() %>% registerDoParallel()

# split data into chunks
data_chunks <- nodes_comp %>% split(., ceiling(seq_along(row_number(.)) / (length(row_number(.)) %/% num_cores)))

# run function parallel over data chunks
nodes_comp <- foreach(chunk = data_chunks, .combine = rbind, .packages = c("dplyr", "dtplyr","sf","sfnetworks","tidygraph")) %dopar% {
  chunk %>% rowwise() %>%
    mutate(
      Shreve(net_comp, .$ID)
    )
}

# visualize
tm_shape(edges_comp)+tm_lines(lwd = 2)+tm_shape(nodes_comp)+tm_dots(col = "Shreve", id = "ID")
