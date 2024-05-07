

setwd(paste0(Sys.getenv('CS_HOME'),'/SuburbanDensification/Models/Matching/Validation/MatchingValidationSampling/'))

#library(devtools)
#devtools::install_github("gearslaboratory/gdalUtils")
# -> fail, requires rgdal which has been discontinued
source('gdalUtils.R') # code version: https://github.com/gearslaboratory/gdalUtils/tree/784b61c43727cbc9d27b275aa78d3e707ef04225

library(sf)
library(r5r) # install rJava with apt : https://github.com/s-u/rJava/issues/255 (fails jni)
library(osmdata)
library(osmextract)
library(rjson)
#library(gdalUtils)
#library(usethis)

# cities and their countries (needed for OSM data download)
cities = list('Strasbourg'=c('France','Germany'),
              'Toulouse'=c('France'),
              'Dortmund'=c('Germany'),
              'Frankfurt'=c('Germany'),
              'Liverpool'=c('England', 'Wales'),
              'Bristol'=c('England', 'Wales')
              )
# radius in which networks are constructed to build isochrones
max_bbox_radius = 400 #km 


dir.create('data/osm',recursive = T, showWarnings = F)

# 1) Get OSM data

# setup download directory -> OSMEXT_DOWNLOAD_DIRECTORY environment variable
#usethis::edit_r_environ()
#readRenviron(".Renviron")
Sys.setenv(OSMEXT_DOWNLOAD_DIRECTORY = './data/osm')

#test <- oe_get("Strasbourg",quiet = FALSE,
#               query = "SELECT * FROM 'lines' WHERE highway IS NOT NULL" # highway column: 78312 NAs out of 371577 with no filter
#)

# specifying regions above could save some data, or using city name matching in oe_get function, but this does not tackle the issue of trans-boundary regions
options(timeout = 3600)
for(country in unique(unlist(cities))){oe_get(country, download_only=T, skip_vectortranslate = T, max_file_size = 5e10)}
write(rjson::toJSON(list(provider='geofabrik',date=as.character(Sys.Date()), path='./data/osm/')),'osm.json')


# 2) Construct multimodal isochrone

# https://www.crs-geo.eu/crs-pan-european.htm
etrs89lcc = "+proj=lcc +lat_0=52 +lon_0=10 +lat_1=35 +lat_2=65 +x_0=4000000 +y_0=2800000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs"
wgs84 = "+proj=longlat +datum=WGS84 +no_defs +type=crs"

# extract OSM networks
for(city in names(cities)){
  city_centroid = st_transform(st_centroid(st_sfc(st_multipoint(osmdata::getbb(city))) %>% st_set_crs(wgs84)), etrs89lcc)
  bbox_data = c(st_coordinates(city_centroid)[,1] - max_bbox_radius*1000, st_coordinates(city_centroid)[,2] - max_bbox_radius*1000, 
                st_coordinates(city_centroid)[,1] - max_bbox_radius*1000, st_coordinates(city_centroid)[,2] + max_bbox_radius*1000,
                st_coordinates(city_centroid)[,1] + max_bbox_radius*1000, st_coordinates(city_centroid)[,2] - max_bbox_radius*1000,
                st_coordinates(city_centroid)[,1] + max_bbox_radius*1000, st_coordinates(city_centroid)[,2] + max_bbox_radius*1000
                )
  broad_bbox = st_bbox(st_transform(st_sfc(st_multipoint(matrix(data = bbox_data, ncol = 2, byrow = T)), crs = etrs89lcc), wgs84))
  network =  st_sf(st_sfc())
  for(country in cities[city]){
    network = rbind(network,
      oe_get(place=country, query = "SELECT * FROM lines WHERE highway IS NOT NULL",
             boundary = broad_bbox, boundary_type = "spat")
    )
  }
  
  st_write(network, dsn = 'data/networks.gpkg', layer = city)
  
}


# for java R5
options(java.parameters = '-Xmx16G')




network <- setup_r5(data_path = 'data')




# 2) Get buildings within the area




# export data : zenodo, nakala? (flat), (~dataverse~)
