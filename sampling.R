
setwd(paste0(Sys.getenv('CS_HOME'),'/SuburbanDensification/Models/Matching/Validation/MatchingValidationSampling/'))


library(sf)
library(osmdata)
library(osmextract)
library(rjson)
library(dplyr)
library(readr)
library(httr)
library(ggplot2)
library(viridis)
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
max_bbox_radius = 100 #km 
#max_bbox_radius = 30

# resolution of the grid of destination points to compute travel time, in meters
travel_time_destinations_resolution = 1000

processing_steps = c(download_osm=T, road_network=T, download_gtfs=T,
                     contruct_network=T, compute_isochrones=T)

# save isochrone map in a png
export_isochrone_map = T

osm_data_dir = './data/osm/'
data_dir = './data/'
res_dir = './res/'

# https://www.crs-geo.eu/crs-pan-european.htm
etrs89lcc = "+proj=lcc +lat_0=52 +lon_0=10 +lat_1=35 +lat_2=65 +x_0=4000000 +y_0=2800000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs"
wgs84 = "+proj=longlat +datum=WGS84 +no_defs +type=crs"

dir.create(osm_data_dir,recursive = T, showWarnings = F)
for(city in names(cities)){
  dir.create(paste0(data_dir,city),showWarnings = F)
}
dir.create(res_dir, showWarnings = F)

########
# 1) Get OSM data
######

if(processing_steps['download_osm']){
  
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
    system(
      paste0('osmosis --rb ',paste0(datafiles,collapse = ' --rb '),' ',paste0(rep('--merge',length(cities[[city]])-1),collapse=' '),' --wb ',osm_data_dir,city,"_highways.osm.pbf")
          )
    
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
    'Luxembourg'=c(1091),#c(1108,1091),# Chemins Fer Luxembourgeois, Aggregated Luxembourg -> only Aggreg (uncompatible dates)
    'Toulouse'=c(1024, 1205) # Tisséo, TER France
  )
  gtfs_dates = list(
    'Luxembourg'='29-10-2024'
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
    network <- setup_r5(data_path = paste0(data_dir,city),overwrite = T)
  }
  
}
  

#######
# 5) Construct multimodal isochrones with r5r
#######


if(processing_steps['compute_isochrones']){

  for(city in names(cities)){
    
    # reload network from cached data (will construct it if not cached)
    network <- setup_r5(data_path = paste0(data_dir,city))
    
    city_centroid = st_centroid(st_sfc(st_multipoint(t(osmdata::getbb(city)))) %>% st_set_crs(wgs84))
  
    orig = data.frame(id='1',st_coordinates(city_centroid)); names(orig)<-c('id','lon','lat')
    points_dest = point_grid(st_centroid(broad_bbox), travel_time_destinations_resolution, max_bbox_radius)
    dest = st_coordinates(points_dest)
    dest = data.frame(id = as.character(1:nrow(dest)), lon=dest[,1], lat=dest[,2])

    departure_datetime <- as.POSIXct(
      paste0(gtfs_dates[city]," 08:00:00"),
      format = "%d-%m-%Y %H:%M:%S"
    )
    
    # different mode combinations : PT+BICYCLE (goes broader than walking and we need an upper bound of the area),
    #       CAR, PT+BICYCLE+CAR EGRESS (car left in parking at destination)
    travel_time_car =  travel_time_matrix(network, orig, dest,
                      mode=c("CAR"),
                      departure_datetime = departure_datetime,
                      max_trip_duration = 240,
                      verbose = F,progress = T
                    )
    
    travel_time_pt =  travel_time_matrix(network, orig, dest,
      mode=c("BICYCLE", "TRANSIT"),
      mode_egress = c("BICYCLE"),
      departure_datetime = departure_datetime,
      max_trip_duration = 240,
      verbose = F,progress = T
    )
    
    travel_time_pt_car =  travel_time_matrix(network, orig, dest,
                                         mode=c("BICYCLE", "TRANSIT"),
                                         mode_egress = c("CAR"),
                                         departure_datetime = departure_datetime,
                                         max_trip_duration = 240,
                                         verbose = F,progress = T
    )
    
  
    dest$time_car = rep(NA, nrow(dest))
    dest$time_car[as.numeric(travel_time_car$to_id)] = travel_time_car$travel_time_p50
    dest$time_pt = rep(NA, nrow(dest))
    dest$time_pt[as.numeric(travel_time_pt$to_id)] = travel_time_pt$travel_time_p50
    dest$time_ptcar = rep(NA, nrow(dest))
    dest$time_ptcar[as.numeric(travel_time_pt_car$to_id)] = travel_time_pt_car$travel_time_p50
    
    dest$time = pmin(dest$time_car, dest$time_pt, dest$time_ptcar, na.rm = T)
    
    if(export_isochrone_map){
      ggsave(
        ggplot(dest[!is.na(dest$time),])+
          #geom_point(aes(x=lon,y=lat,col=ifelse(time_pt>120,'white','grey')))+
          geom_point(aes(x=lon,y=lat,col=time),size=1.3,shape=15)+
          geom_point(data=orig,aes(x=lon,y=lat),color='red')+
          #scale_colour_gradient2(low='blue',high='red',mid = 'white',midpoint = median(dest$time_pt,na.rm = T))+
          scale_colour_viridis(name='Travel time'),
        filename = paste0(res_dir,'travel_time_map_allmodes_',city,'_',max_bbox_radius,'km.png'),
        width = 30, height=30, units = 'cm'
      )
    }
 
  }
  
  
  

}




######
# 6) Extract buildings for the full area
######


