

setwd(paste0(Sys.getenv('CS_HOME'),'/SuburbanDensification/Models/Matching/Validation/MatchingValidationSampling/'))

#library(devtools)
#devtools::install_github("gearslaboratory/gdalUtils")
# -> fail, requires rgdal which has been discontinued
#source('gdalUtils.R') # code version: https://github.com/gearslaboratory/gdalUtils/tree/784b61c43727cbc9d27b275aa78d3e707ef04225

library(sf)
library(osmdata)
library(osmextract)
library(rjson)
library(dplyr)
library(readr)
library(httr)
#library(gdalUtils)
#library(usethis)

# cities and their countries (needed for OSM data download)
cities = list(
#  'Strasbourg'=c('France','Germany'),
              'Toulouse'=c('France')#,
#              'Dortmund'=c('Germany'),
#              'Frankfurt'=c('Germany'),
#              'Liverpool'=c('England', 'Wales'),
#              'Bristol'=c('England', 'Wales')
              )

#cities=list('Luxembourg'=c('Luxembourg')) # test
# radius in which networks are constructed to build isochrones
max_bbox_radius = 100 #km 
#max_bbox_radius = 1

osm_data_dir = './data/osm/'
data_dir = './data/'

dir.create(osm_data_dir,recursive = T, showWarnings = F)

# 1) Get OSM data

# setup download directory -> OSMEXT_DOWNLOAD_DIRECTORY environment variable
#usethis::edit_r_environ()
#readRenviron(".Renviron")
Sys.setenv(OSMEXT_DOWNLOAD_DIRECTORY = osm_data_dir)

#test <- oe_get("Strasbourg",quiet = FALSE,
#               query = "SELECT * FROM 'lines' WHERE highway IS NOT NULL" # highway column: 78312 NAs out of 371577 with no filter
#)

# specifying regions above could save some data, or using city name matching in oe_get function, but this does not tackle the issue of trans-boundary regions
options(timeout = 3600)
for(country in unique(unlist(cities))){
  #oe_get(country, download_only=T, skip_vectortranslate = T, max_file_size = 5e10)
  oe_get(country, max_file_size = 5e10)
}

# uncomment for prod
#write(rjson::toJSON(list(provider='geofabrik',date=as.character(Sys.Date()), path='./data/osm/')),'osm.json')


# 2) Construct multimodal isochrone

# https://www.crs-geo.eu/crs-pan-european.htm
etrs89lcc = "+proj=lcc +lat_0=52 +lon_0=10 +lat_1=35 +lat_2=65 +x_0=4000000 +y_0=2800000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs"
wgs84 = "+proj=longlat +datum=WGS84 +no_defs +type=crs"

# extract OSM networks
for(city in names(cities)){
  city_centroid = st_transform(st_centroid(st_sfc(st_multipoint(t(osmdata::getbb(city)))) %>% st_set_crs(wgs84)), etrs89lcc)
  bbox_data = c(st_coordinates(city_centroid)[,1] - max_bbox_radius*1000, st_coordinates(city_centroid)[,2] - max_bbox_radius*1000, 
                st_coordinates(city_centroid)[,1] - max_bbox_radius*1000, st_coordinates(city_centroid)[,2] + max_bbox_radius*1000,
                st_coordinates(city_centroid)[,1] + max_bbox_radius*1000, st_coordinates(city_centroid)[,2] - max_bbox_radius*1000,
                st_coordinates(city_centroid)[,1] + max_bbox_radius*1000, st_coordinates(city_centroid)[,2] + max_bbox_radius*1000
                )
  broad_bbox = st_bbox(st_transform(st_sfc(st_multipoint(matrix(data = bbox_data, ncol = 2, byrow = T)), crs = etrs89lcc), wgs84)) %>% st_as_sfc()
  
  #network =  st_sf(st_sfc(),crs = wgs84)
  #for(country in cities[city]){
  #  datafile = paste0('geofabrik_',strsplit(tail(strsplit(oe_match(country)$url,'/',fixed=T)[[1]], n=1),'.',fixed=T)[[1]][1], '.gpkg')
  #  allnw = st_read(paste0(osm_data_dir, datafile), 'lines') %>% dplyr::filter(!is.na(highway)) %>% st_transform(wgs84)
  #  network = rbind(network,
  #    #oe_get(place=country, query = "SELECT * FROM lines WHERE highway IS NOT NULL",
  #    #       boundary = broad_bbox, boundary_type = "spat")
  #    st_intersection(allnw,broad_bbox)
  #  )
  #}
  
  #if(!cached_city_data){dir}
  dir.create(paste0(data_dir,city),showWarnings = F)
  
  for(country in cities[city]){
    datafile = paste0('geofabrik_',strsplit(tail(strsplit(oe_match(country)$url,'/',fixed=T)[[1]], n=1),'.',fixed=T)[[1]][1])
    
    # osmosis --read-pbf geofabrik_luxembourg-latest.osm.pbf --bounding-box top=49.61773 left=6.121288 bottom=49.59816 right=6.151396 --tf accept-ways highway=* --used-node --write-pbf test.osm.pbf
    system(paste0('osmosis --read-pbf ', osm_data_dir, datafile, '.osm.pbf --bounding-box top=', st_bbox(broad_bbox)$ymax, ' left=',st_bbox(broad_bbox)$xmin, ' bottom=',st_bbox(broad_bbox)$ymin, ' right=',st_bbox(broad_bbox)$xmax,' --tf accept-ways highway=* --used-node --write-pbf ',osm_data_dir, datafile,'_',city,'_highways.osm.pbf'))
    
    # not needed
    # ogr2ogr test.gpkg test.osm.pbf 
    #system(paste0('ogr2ogr ',osm_data_dir, datafile,'_',city,'_highways.gpkg ',osm_data_dir, datafile,'_',city,'_highways.osm.pbf'))
    
    system(paste0('cp ',osm_data_dir, datafile,'_',city,'_highways.osm.pbf ',data_dir,city,'/highways.osm.pbf'))
    
  }
  
  
  # TODO merge if more than one country
  # cp data/osm/geofabrik_luxembourg-latest_Luxembourg_highways.osm.pbf 
   
  #st_write(network, dsn = paste0(data_dir,city,'/network.gpkg'), layer = city)
  #sf::gdal_utils(util = "vectortranslate", source = paste0(data_dir,city,'/network.gpkg'), destination = paste0(data_dir,city,'/network.osm.pbf'),options = c("-f", "OSM"))
  
}



# getting GTFS data from the Mobility Database https://mobilitydatabase.org/

#curl --location 'https://api.mobilitydatabase.org/v1/tokens'\
#--header 'Content-Type: application/json'\
#--data '{ "refresh_token": "[Your Refresh Token]" }'
#curl_fetch_memory('https://api.mobilitydatabase.org/v1/tokens')
refreshToken = gsub('\n','',read_file(paste0(data_dir,'.mobilitydatabase')), fixed=T)

req = POST('https://api.mobilitydatabase.org/v1/tokens', add_headers('Content-Type' = 'application/json'), body=paste0('{ "refresh_token": "',refreshToken,'" }'))
token = content(req)$access_token


#content(GET(paste0('https://api.mobilitydatabase.org/v1/search?search_query="mdb"'), add_headers('Accept'= 'application/json', 'Authorization'=paste0('Bearer ',token))))

#allfeeds = content(GET(paste0('https://api.mobilitydatabase.org/v1/feeds'), add_headers('Accept'= 'application/json', 'Authorization'=paste0('Bearer ',token))))
#sapply(allfeeds,function(x){x$feed_name})
#allfeeds[[714]]

#feed_data = content(GET('https://api.mobilitydatabase.org/v1/feeds/mdb-1640', add_headers('Accept'= 'application/json', 'Authorization'=paste0('Bearer ',token))))
#feed_data = content(GET('https://api.mobilitydatabase.org/v1/gtfs_feeds/mdb-1640/datasets', add_headers('Accept'= 'application/json', 'Authorization'=paste0('Bearer ',token))))

#all_gtfs_feeds = content(GET(paste0('https://api.mobilitydatabase.org/v1/gtfs_feeds'), add_headers('Accept'= 'application/json', 'Authorization'=paste0('Bearer ',token))))
#sapply(all_gtfs_feeds,function(x){x$feed_name})
#all_gtfs_feeds[[544]]
#feed_data = content(GET('https://api.mobilitydatabase.org/v1/gtfs_feeds/mdb-1924/datasets', add_headers('Accept'= 'application/json', 'Authorization'=paste0('Bearer ',token))))
#feed_data = content(GET('https://api.mobilitydatabase.org/v1/datasets/gtfs/mdb-1210-202402121801', add_headers('Accept'= 'application/json', 'Authorization'=paste0('Bearer ',token))))
# $latest_dataset$hosted_url
# https://files.mobilitydatabase.org/mdb-1924/mdb-1924-202405130021/mdb-1924-202405130021.zip

# locations
#gtfs = st_sfc(st_multipoint(
#matrix(unlist(sapply(all_gtfs_feeds,function(x){
#  bb = x$latest_dataset$bounding_box
#  if(!is.null(bb)){
#    c(bb$minimum_longitude,bb$minimum_latitude)
#    }
#})),ncol=2,byrow = T)), crs=wgs84)

# -> search by bbox? - list explicitely feeds

#gtfs_fr = content(GET(paste0('https://api.mobilitydatabase.org/v1/gtfs_feeds?country_code=FR'), add_headers('Accept'= 'application/json', 'Authorization'=paste0('Bearer ',token))))
#unlist(sapply(gtfs_fr,function(x){x$locations[[1]]$municipality}))
#gtfs_fr[[2]]

catalog = read_csv('https://storage.googleapis.com/storage/v1/b/mdb-csv/o/sources.csv?alt=media')


# mdb ids in the catalog
gtfs_feeds = list(
  'Luxembourg'=c(1108),
  'Toulouse'=c(1024)
)

# TODO handle multiple GTFS
for(city in names(cities)){
  zipfile=paste0(data_dir,city,'/',city,'.zip')
  download.file(catalog$urls.latest[gtfs_feeds[[city]][1]],destfile = zipfile)
  unzip(zipfile,exdir = paste0(data_dir,city))
}


Sys.setenv(JAVA_HOME = '/usr/lib/jvm/java-1.21.0-openjdk-amd64')


library(r5r) # install rJava with apt : https://github.com/s-u/rJava/issues/255 (fails jni)


# for java R5
options(java.parameters = '-Xmx32G')

#Sys.setenv(JAVA_HOME = '/usr/lib/jvm/java-1.21.0-openjdk-amd64')
# sudo R CMD javareconf JAVA_HOME=/usr/lib/jvm/java-1.21.0-openjdk-amd64
# see https://stackoverflow.com/questions/28133360/rjava-is-not-picking-up-the-correct-java-version

network <- setup_r5(data_path = paste0(data_dir,city))

street_net <- street_network_to_sf(network)

#orig = data.frame(id='1',st_coordinates(st_centroid(broad_bbox)))
#names(orig)<-c('id','lon','lat')
#dest = data.frame(id=as.character(1:4),st_coordinates(broad_bbox)[1:4,c(1,2)])
#names(dest)<-c('id','lon','lat')

#set.seed(42)
#orig = data.frame(id='1',st_coordinates(street_net$vertices[sample.int(nrow(street_net$vertices), size=1),]));names(orig)<-c('id','lon','lat')
#dest = data.frame(id='1',st_coordinates(street_net$vertices[sample.int(nrow(street_net$vertices), size=1),]));names(dest)<-c('id','lon','lat')
#gc()

#'
#' constructs a point grid around the origin point (wgs84), with a given step (meters), of width/height 2*size+1
point_grid <- function(orig, step, size){
  p0 = st_coordinates(st_transform(orig,etrs89lcc))
  x0 = p0[1,1]; y0 = p0[1,2]
  xcoords = seq(from = x0 - size*step, to = x0 + size*step, by = step)
  ycoords = seq(from = y0 - size*step, to = y0 + size*step, by = step)
  allx = matrix(data=rep(xcoords,length(ycoords)),nrow = length(ycoords), byrow = T)
  ally = matrix(data=rep(ycoords,length(xcoords)),ncol = length(xcoords), byrow = F)
  return(st_transform(st_sfc(st_multipoint(as.matrix(data.frame(X = c(allx), Y=c(ally)))), crs = etrs89lcc), wgs84))
}

orig = data.frame(id='1',st_coordinates(st_centroid(broad_bbox))); names(orig)<-c('id','lon','lat')
points_dest = point_grid(st_centroid(broad_bbox), 1000, 10)
dest = st_coordinates(points_dest)
dest = data.frame(id = as.character(1:nrow(dest)), lon=dest[,1], lat=dest[,2])

# pete en mem avec 100km
#isochrone = r5r::isochrone(network,origins = orig ,mode=c( "TRANSIT", "CAR"))
#plot(isochrone[2,])
travel_time = travel_time_matrix(network, orig, dest,mode=c("TRANSIT","CAR"),
                                 mode_egress = "WALK",
                                 max_trip_duration = 240,verbose = T, progress = T)
dest$time = rep(NA, nrow(dest))
dest$time[as.numeric(travel_time$to_id)] = travel_time$travel_time_p50
ggplot(dest)+geom_point(aes(x=lon,y=lat,fill=time),size=2, shape=1)

det = detailed_itineraries(network, orig, dest,mode=c("TRANSIT","CAR"),
                           max_trip_duration = 240,verbose = T, progress = T)

library(ggplot2)
ggplot() +geom_sf(data = street_net$edges, color='gray85') +
  geom_sf(data = det, aes(color=mode)) +
  facet_wrap(.~option) + 
  theme_void()

# TODO heuristic to calculate approximate isochrones : PT+WALK+BICYCLE, CAR, PT+CAR


# 2) Get buildings within the area




# export data : zenodo, nakala? (flat), (~dataverse~)
