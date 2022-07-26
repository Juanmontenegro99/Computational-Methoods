# Introduction to spatial data in R ----
library(sf)
library(tmap)
library(dplyr)
# Examining an sf object ----
data(World)
# package tmap has a syntax similar to ggplot. The functions start all with tm_
tm_shape(World) +
  tm_borders()

head(World)
names(World)
class(World)
dplyr::glimpse(World)

plot(World[1])
plot(World[,1])
plot(World[1,])
plot(World["pop_est"])

# The geometry “column” and geometries as objects ----
head(World[, 1:4])
class(World)
class(World$geometry)

head(sf::st_coordinates(World))
no_geom <- sf::st_drop_geometry(World)
class(no_geom)

#bounding boxes
st_bbox(World)

# Manipulating sf objects ----
names(World)
unique(World$continent)

World %>%
  filter(continent == "South America") %>%
  tm_shape() +
  tm_borders()

color = "lightgreen"
# if_else si la respuesta logica es si, hace la primera coma si no hace la segunda
World %>%
  mutate(our_countries = if_else(iso_a3 %in% c("COL","BRA", "MEX", "ARG", "USA", "ESP", "ITA"), color, "gray")) %>%
  tm_shape() +
  tm_borders() +
  tm_fill(col = "our_countries") +
  tm_add_legend("fill",
                "Countries",
                col = color)

# Loading, ploting, and saving a shapefile from the disk ----
# install.packages("rnaturalearth")
# install.packages("remotes")
# remotes::install_github("ropensci/rnaturalearthhires")
library(rnaturalearth)
library(rnaturalearthhires)
bra <- ne_states(country = "brazil", returnclass = "sf")
plot(bra)

col <- ne_states(country = "colombia", returnclass = "sf")
plot(col)

# recursive es para crear varias carpetas (o un path entero) en un solo comando,
# si no se pone y no existiera la carpeta data, saldría error.
dir.create("data/shapefiles", recursive = TRUE)
st_write(obj = bra, dsn = "data/shapefiles/bra.shp", delete_layer = TRUE)
st_write(obj = col, dsn = "data/shapefiles/col.shp", delete_layer = TRUE)


bra2 <- read_sf("data/shapefiles/bra.shp")
col2 <- read_sf("data/shapefiles/col.shp")
class(bra)
class(bra2)

plot(bra)
plot(bra2)
plot(col)
plot(col2)

# Loading, ploting, and saving a raster from the disk ----
library(raster)
dir.create(path = "data/raster/", recursive = TRUE)
tmax_data <- getData(name = "worldclim", var = "tmax", res = 10, path = "data/raster/")
plot(tmax_data)

is(tmax_data) #the data are a raster stack, several rasters piled
dim(tmax_data)
extent(tmax_data)
res(tmax_data)

