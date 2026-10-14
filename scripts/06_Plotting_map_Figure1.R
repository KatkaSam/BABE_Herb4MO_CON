#----------------------------------------------------------#
#
#
#               BABE 4MO controls herbivory
#
#                 Plotting Map Figure 1
#   
#                  Katerina Sam   2025
#
#----------------------------------------------------------#

if(!("maptools" %in% installed.packages())) install.packages("maptools")
if (!("maptools" %in% (.packages()))) library(maptools)

if (!require(gpclib)) install.packages("gpclib", type="source")
gpclibPermit()

install.packages('rgeos', type='source')
install.packages('rgdal', type='source')

#----------------------------------------------------------#
# 1. Prepare the map in unified colour  -----
#----------------------------------------------------------#
setwd("C:/Users/ksam/Documents/GitHub/BABE_Herb4MO_CON/data/input")
library(sf)

biomes <- st_read("tnc_terr_ecoreg_clip.shp", quiet = TRUE)

tropical <- biomes[
  biomes$WWF_MHTNUM %in% c(1, 2, 3),
]

temperate <- biomes[
  biomes$WWF_MHTNUM %in% c(4, 5),
]

par(xpd = FALSE, mai = c(0, 0, 0, 0))

# Grey background, restricted to Africa–Australia
plot(
  st_geometry(biomes),
  col = "grey87",
  border = NA,
  xlim = c(18, 180),
  ylim = c(-60, 85),
  reset = FALSE
)

# Tropical forests
plot(
  st_geometry(tropical),
  col = "#009E73",
  border = NA,
  add = TRUE
)

# Temperate forests
plot(
  st_geometry(temperate),
  col = "#B3EE3A",
  border = NA,
  add = TRUE
)

box(lwd = 2)

legend(
  "bottomleft",
  legend = c("Tropical and subtropical forests", "Temperate forests", "Other biomes"),
  fill = c("#009E73", "#B3EE3A", "gray87"),
  border = NA,
  bty = "n"
)


#----------------------------------------------------------#
# 2. Enter our points to the map  -----
#----------------------------------------------------------#
bg <- "black"
point_lwd <- 1.5

foodwebs <- read.csv("Plot_sites_BABE.csv")

# Check which columns contain longitude and latitude
names(foodwebs)

coord <- foodwebs[, 2:1]

points(
  coord,
  pch = 16,
  cex = 1,
  bg = bg,
  col = "black",
  lwd = point_lwd
)


