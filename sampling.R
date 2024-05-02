
library(devtools)
devtools::install_github("gearslaboratory/gdalUtils")

setwd(paste0(Sys.getenv('CS_HOME'),'/SuburbanDensification/Models/Matching/Validation/MatchingValidationSampling/'))

library(sf)
library(r5r) # install rJava with apt : https://github.com/s-u/rJava/issues/255 (fails jni)
library(osmdata)
library(osmextract)
library(rjson)
library(gdalUtils)
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

# extract OSM networks
for(city in names(cities)){
  city_centroid = st_transform(st_centroid(osmdata::getbb(city)),'ETRS89-LCC') # https://www.crs-geo.eu/crs-pan-european.htm
  broad_bbox = st_transform(st_bbox(st_multipoint(matrix(data = c(city_centroid$x - max_bbox_radius*1000, city_centroid$y - max_bbox_radius*1000, 
                                                     city_centroid$x - max_bbox_radius*1000, city_centroid$y + max_bbox_radius*1000,
                                                     city_centroid$x + max_bbox_radius*1000, city_centroid$y - max_bbox_radius*1000,
                                                     city_centroid$x + max_bbox_radius*1000, city_centroid$y + max_bbox_radius*1000
                                                     ), ncol = 2, byrow = T))), 4326)
  network =  st_sf(st_sfc())
  for(country in cities[city]){
    network = rbind(network,
      oe_get(place=country, query = "SELECT * FROM 'lines' WHERE highway IS NOT NULL",
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
