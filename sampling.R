
setwd(paste0(Sys.getenv('CS_HOME'),'/SuburbanDensification/Models/Matching/Validation/MatchingValidationSampling/'))


library(sf)
library(osmdata)
library(osmextract)
library(rjson)
library(dplyr)
library(readr)
library(httr)
library(ggplot2)
library(r5r) # install rJava with apt : https://github.com/s-u/rJava/issues/255 (fails jni otherwise)

source('functions.R')

Sys.setenv(JAVA_HOME = '/usr/lib/jvm/java-1.21.0-openjdk-amd64') # r5r requires java 21
options(java.parameters = '-Xmx32G')


# cities and their countries (needed for OSM data download)
#cities = list(
#  'Strasbourg'=c('France','Germany'),
#              'Toulouse'=c('France')#,
#              'Dortmund'=c('Germany'),
#              'Frankfurt'=c('Germany'),
#              'Liverpool'=c('England', 'Wales'),
#              'Bristol'=c('England', 'Wales')
#              )
cities=list('Luxembourg'=c('Luxembourg', 'France', 'Germany', 'Belgium')) # test

# radius in which networks are constructed to build isochrones
#max_bbox_radius = 100 #km 
max_bbox_radius = 30

processing_steps = c(download_osm=T, road_network=T, download_gtfs=T,
                     contruct_network=T, compute_isochrones=T)

osm_data_dir = './data/osm/'
data_dir = './data/'

# https://www.crs-geo.eu/crs-pan-european.htm
etrs89lcc = "+proj=lcc +lat_0=52 +lon_0=10 +lat_1=35 +lat_2=65 +x_0=4000000 +y_0=2800000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs"
wgs84 = "+proj=longlat +datum=WGS84 +no_defs +type=crs"


########
# 1) Get OSM data
######

if(processing_steps['download_osm']){

  dir.create(osm_data_dir,recursive = T, showWarnings = F)
  
  Sys.setenv(OSMEXT_DOWNLOAD_DIRECTORY = osm_data_dir)

  options(timeout = 3600)
  for(country in unique(unlist(cities))){
    oe_get(country, max_file_size = 5e10, download_only = T, skip_vectortranslate = T)
  }

  # TODO proper metadata export
  #write(rjson::toJSON(list(provider='geofabrik',date=as.character(Sys.Date()), path='./data/osm/')),'osm.json')

}
  

########
# 2) extract OSM road network
########

if(processing_steps['road_network']){

  for(city in names(cities)){
    city_centroid = st_transform(st_centroid(st_sfc(st_multipoint(t(osmdata::getbb(city)))) %>% st_set_crs(wgs84)), etrs89lcc)
    bbox_data = c(st_coordinates(city_centroid)[,1] - max_bbox_radius*1000, st_coordinates(city_centroid)[,2] - max_bbox_radius*1000,
                  st_coordinates(city_centroid)[,1] - max_bbox_radius*1000, st_coordinates(city_centroid)[,2] + max_bbox_radius*1000,
                  st_coordinates(city_centroid)[,1] + max_bbox_radius*1000, st_coordinates(city_centroid)[,2] - max_bbox_radius*1000,
                  st_coordinates(city_centroid)[,1] + max_bbox_radius*1000, st_coordinates(city_centroid)[,2] + max_bbox_radius*1000
                 )
    broad_bbox = st_bbox(st_transform(st_sfc(st_multipoint(matrix(data = bbox_data, ncol = 2, byrow = T)), crs = etrs89lcc), wgs84)) %>% st_as_sfc()
  
    dir.create(paste0(data_dir,city),showWarnings = F)
  
    datafiles=c()
    for(country in cities[[city]]){
      datafile_root = paste0('geofabrik_',strsplit(tail(strsplit(oe_match(country)$url,'/',fixed=T)[[1]], n=1),'.',fixed=T)[[1]][1])
      datafile = paste0(datafile_root,'.osm.pbf')
      highwaysfile = paste0(osm_data_dir,datafile_root,'_',city,'_highways','.osm.pbf')
      datafiles = append(datafiles,highwaysfile)
      
      # osmosis command to extract from bounding box and filter highways
      system(paste0('osmosis --read-pbf ', osm_data_dir, datafile,
                  ' --bounding-box top=', st_bbox(broad_bbox)$ymax,
                  ' left=',st_bbox(broad_bbox)$xmin, ' bottom=',st_bbox(broad_bbox)$ymin,
                  ' right=',st_bbox(broad_bbox)$xmax,' --tf accept-ways highway=* --used-node --write-pbf ',
                  highwaysfile)
           )
    }
  
    # osmosis command to merge pbf files (command `--merge` has to appear N_files - 1)
    system(paste0('osmosis --rb ',paste0(datafiles,collapse = ' --rb '),' ',paste0(rep('--merge',length(cities[[city]])-1)),' --wb ',osm_data_dir,city,"_highways.osm.pbf"))
    
    # copy final file to data dir
    system(paste0('cp ',osm_data_dir,city,'_highways.osm.pbf ',data_dir,city,'/highways.osm.pbf'))
    
  }
  
}


######
# 3) Get GTFS data from the Mobility Database https://mobilitydatabase.org/
######

if(processing_steps['download_gtfs']){

  # get open catalog, does not need an API token
  catalog = read_csv('https://storage.googleapis.com/storage/v1/b/mdb-csv/o/sources.csv?alt=media')

  # mdb ids in the catalog (to be determined by hand)
  gtfs_feeds = list(
    'Luxembourg'=c(1108,1091),# Chemins Fer Luxembourgeois, Aggregated Luxembourg
    'Toulouse'=c(1024)
  )


  for(city in names(cities)){
    feedids = gtfs_feeds[[city]]
    # r5r handles multiple GTFS in zip format
    for(feedid in feedids){
      zipfile=paste0(data_dir,city,'/',city,'_',feedid,'.zip')
      download.file(catalog$urls.latest[feedid],destfile = zipfile)
    }
  }

}


#######
# 4) Construct the networks using r5r
#######


if(processing_steps['contruct_network']){

  for(city in names(cities)){
    network <- setup_r5(data_path = paste0(data_dir,city))
    street_net <- street_network_to_sf(network)
  }
  
}
  

#######
# 5) Construct multimodal isochrones with r5r
#######


if(processing_steps['compute_isochrones']){

  orig = data.frame(id='1',st_coordinates(st_centroid(broad_bbox))); names(orig)<-c('id','lon','lat')
  points_dest = point_grid(st_centroid(broad_bbox), 1000, 30)
  dest = st_coordinates(points_dest)
  dest = data.frame(id = as.character(1:nrow(dest)), lon=dest[,1], lat=dest[,2])

  travel_time = travel_time_matrix(network, orig, dest,mode=c("TRANSIT","CAR"),
                                 mode_egress = "WALK",
                                 max_trip_duration = 240,verbose = T, progress = T)
  dest$time = rep(NA, nrow(dest))
  dest$time[as.numeric(travel_time$to_id)] = travel_time$travel_time_p50
  
  #ggplot(dest[!is.na(dest$time),])+geom_point(aes(x=lon,y=lat,col=time),size=2)

  #det = detailed_itineraries(network, orig, dest,mode=c("TRANSIT","CAR"),
  #                         max_trip_duration = 360,verbose = T, progress = T)

  #library(ggplot2)
  #ggplot() +geom_sf(data = street_net$edges, color='gray85') +
  #geom_sf(data = det, aes(color=mode)) +
  #facet_wrap(.~option) + 
  #theme_void()

  # TODO heuristic to calculate approximate isochrones : PT+WALK+BICYCLE, CAR, PT+CAR

}




