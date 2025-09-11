# Setup ####

library(tidyverse)
library(sf)
library(sfnetworks)
library(whitebox)

setwd("C:/Users/Daniel Enns/Documents/Promotion/Nathan stuff")

# set up stream network ####

# load network
streams <- st_as_sf("./str_net_lit/")
