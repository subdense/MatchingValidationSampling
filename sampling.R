
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
library(tidytransit)

source('functions.R')

Sys.setenv(JAVA_HOME = '/usr/lib/jvm/java-1.21.0-openjdk-amd64') # r5r requires java 21
options(java.parameters = '-Xmx32G')


# cities and their countries (needed for OSM data download)
cities = list(
              #'Strasbourg'=c('France','Germany'),
              'Strasbourg'=c('France'), # no data for the German part of Strasbourg for now
              'Toulouse'=c('France'),
              'Dortmund'=c('Germany'),
              'Frankfurt'=c('Germany')#,
#              'Liverpool'=c('England', 'Wales'),
#              'Bristol'=c('England', 'Wales')
              )
#cities=list('Luxembourg'=c('Luxembourg', 'France', 'Germany', 'Belgium')) # test

# radius in which networks are constructed to build isochrones
max_bbox_radius = 100 #km 
#max_bbox_radius = 30

# resolution of the grid of destination points to compute travel time, in meters
travel_time_destinations_resolution = 1000

# travel time defining the functional area (https://www.sciencedirect.com/science/article/pii/S0094119020300139 : no empirical value for max commuting time?)
max_travel_time = 90

# buffer size in meters to compute the built area
built_area_buffer_size = 500

# number of sampling points
sampling_points = 100

# random seed
seed = 42

# GTFS data ad hoc processing - to be checked and modified accordingly
source('gtfs.R')



processing_steps = c(download_osm=F, road_network=F, download_gtfs=F,
                     contruct_network=F, compute_isochrones=T,
                     extract_buildings=T, compute_sampling_area=T,
                     sample_points=T)

# save maps in a png
export_maps = T

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

set.seed(seed)

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
    
    show(paste0('Extracting road network for ',city))
    
    city_centroid = get_city_centroid(city,wgs84,etrs89lcc)
    broad_bbox = get_bbox(city_centroid,max_bbox_radius,wgs84,etrs89lcc)
  
    filter_osmpbf_file(city,cities,osm_data_dir,data_dir,broad_bbox,"highway")
  }
  
}


######
# 3) Get GTFS data from the Mobility Database https://mobilitydatabase.org/
######

if(processing_steps['download_gtfs']){

  # get open catalog, does not need an API token
  catalog = read_csv('https://storage.googleapis.com/storage/v1/b/mdb-csv/o/sources.csv?alt=media')

  # GTFS feeds ids and GTFS dates : global variables at the beginning of the script

  for(city in names(cities)){
    
    show(paste0('Collecting GTFS data for ',city))
    
    feedids = gtfs_feeds[[city]]
    # r5r handles multiple GTFS in zip format
    for(feedid in feedids){
      url=catalog$urls.latest[feedid]
      zipfile=paste0(data_dir,city,'/',city,'_',feedid,'.zip')
      show(paste0('	Saving ',url,' to ',zipfile))
      gtfs = gtfs_preprocessing[[city]](read_gtfs(url),feedid)
      write_gtfs(gtfs,zipfile)
    }
    
  }

}


#######
# 4) Construct the networks using r5r
#######


if(processing_steps['contruct_network']){

  for(city in names(cities)){
    
    show(paste0('Constructing network for ',city))
    
    network <- setup_r5(data_path = paste0(data_dir,city),overwrite = T)
  }
  
}
  

#######
# 5) Construct multimodal isochrones with r5r
#######


if(processing_steps['compute_isochrones']){

  for(city in names(cities)){
    
    show(paste0('Computing travel time for ',city))
    
    # reload network from cached data (will construct it if not cached)
    network <- setup_r5(data_path = paste0(data_dir,city))
    
    city_centroid = st_transform(get_city_centroid(city,wgs84,etrs89lcc),wgs84)
    broad_bbox = get_bbox(city_centroid,max_bbox_radius,wgs84,etrs89lcc)

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
    travel_time_car = travel_time_matrix(network, orig, dest,
                      mode=c("CAR"),
                      departure_datetime = departure_datetime,
                      max_trip_duration = 240,
                      verbose = T,progress = T
                    )
    
    travel_time_pt = travel_time_matrix(network, orig, dest,
      mode=c("BICYCLE", "TRANSIT"),
      mode_egress = c("BICYCLE"),
      departure_datetime = departure_datetime,
      max_trip_duration = 240,
      verbose = T,progress = T
    )
    
    travel_time_pt_car = travel_time_matrix(network, orig, dest,
                                         mode=c("BICYCLE", "TRANSIT"),
                                         mode_egress = c("CAR"),
                                         departure_datetime = departure_datetime,
                                         max_trip_duration = 240,
                                         verbose = T,progress = T
    )
    
  
    dest$time_car = rep(NA, nrow(dest))
    dest$time_car[as.numeric(travel_time_car$to_id)] = travel_time_car$travel_time_p50
    dest$time_pt = rep(NA, nrow(dest))
    dest$time_pt[as.numeric(travel_time_pt$to_id)] = travel_time_pt$travel_time_p50
    dest$time_ptcar = rep(NA, nrow(dest))
    dest$time_ptcar[as.numeric(travel_time_pt_car$to_id)] = travel_time_pt_car$travel_time_p50
    
    dest$time = pmin(dest$time_car, dest$time_pt, dest$time_ptcar, na.rm = T)
    
    for(j in 5:7){dest[is.na(dest[,j]),j]=Inf};dest[is.na(dest[,4]),4]=-Inf
    dest$mode = apply(dest,1,function(row){if(as.numeric(row[4])==as.numeric(row[5])){return('car')}else{if(as.numeric(row[4])==as.numeric(row[6])){return('PT')}else{if(as.numeric(row[4])==as.numeric(row[7])){return('PT-car')}else{return(NA)}}}})
    
    if(export_maps){
      ggsave(
        ggplot(dest[!is.na(dest$time),])+
          geom_point(aes(x=lon,y=lat,col=time),size=1.4,shape=15)+
          geom_point(data=orig,aes(x=lon,y=lat),color='red')+
          scale_colour_viridis(name='Travel time\n(all modes)')+ggtitle(city),
        filename = paste0(res_dir,'travel_time_map_allmodes_',city,'_',max_bbox_radius,'km.png'),
        width = 30, height=30, units = 'cm'
      )
      ggsave(
        ggplot(dest[!is.na(dest$time_car),])+
          geom_point(aes(x=lon,y=lat,col=time_car),size=1.4,shape=15)+
          geom_point(data=orig,aes(x=lon,y=lat),color='red')+
          scale_colour_viridis(name='Travel time\n(car)')+ggtitle(city),
        filename = paste0(res_dir,'travel_time_map_car_',city,'_',max_bbox_radius,'km.png'),
        width = 30, height=30, units = 'cm'
      )
      ggsave(
        ggplot(dest[!is.na(dest$time_pt),])+
          geom_point(aes(x=lon,y=lat,col=time_pt),size=1.4,shape=15)+
          geom_point(data=orig,aes(x=lon,y=lat),color='red')+
          scale_colour_viridis(name='Travel time\n(public transport)')+ggtitle(city),
        filename = paste0(res_dir,'travel_time_map_pt_',city,'_',max_bbox_radius,'km.png'),
        width = 30, height=30, units = 'cm'
      )
      ggsave(
        ggplot(dest[!is.na(dest$time_ptcar),])+
          geom_point(aes(x=lon,y=lat,col=time_ptcar),size=1.4,shape=15)+
          geom_point(data=orig,aes(x=lon,y=lat),color='red')+
          scale_colour_viridis(name='Travel time\n(PT and car)')+ggtitle(city),
        filename = paste0(res_dir,'travel_time_map_ptcar_',city,'_',max_bbox_radius,'km.png'),
        width = 30, height=30, units = 'cm'
      )
      ggsave(
        ggplot(dest[!is.na(dest$mode),])+
          geom_point(aes(x=lon,y=lat,col=mode),size=1.4,shape=15)+
          geom_point(data=orig,aes(x=lon,y=lat),color='red')+
          scale_colour_discrete(name='Travel mode')+ggtitle(city),
        filename = paste0(res_dir,'travel_mode_map_',city,'_',max_bbox_radius,'km.png'),
        width = 30, height=30, units = 'cm'
      )
      
    }
    
    # export the isochrone polygon
    fuapoints = st_cast(st_transform(st_sfc(st_multipoint(as.matrix(dest[dest$time<max_travel_time,c('lon','lat')])),crs = wgs84),crs = etrs89lcc),to="POINT")
    fuabuffer = st_cast(st_union(st_buffer(fuapoints,travel_time_destinations_resolution)),to="POLYGON")
    st_write(fuabuffer,dsn=paste0(res_dir,'isochrone_',max_travel_time,'min_',city,'.gpkg'),layer = paste0('isochrone_',max_travel_time,'min'))
    
  }
  
}




######
# 6) Extract buildings for the full area
######


if(processing_steps['extract_buildings']){
  
  for(city in names(cities)){
    
    show(paste0('Extracting buildings for ',city))
    
    city_centroid = get_city_centroid(city,wgs84,etrs89lcc)
    broad_bbox = get_bbox(city_centroid,max_bbox_radius,wgs84,etrs89lcc)
    
    filter_osmpbf_file(city,cities,osm_data_dir,data_dir,broad_bbox,"building")
    
    filename_root = paste0(data_dir,city,'/building')
    
    system(
      paste0('ogr2ogr -f GPKG ',filename_root,'.gpkg ',filename_root,'.osm.pbf')
    )
  }
  
}


#####
# 7) Construct sampling area
#####


if(processing_steps['compute_sampling_area']){
  
  for(city in names(cities)){
    
    show(paste0('Computing sampling area for ',city))
    
    #st_layers(paste0(data_dir,city,'/building.gpkg'))
    buildings = st_read(paste0(data_dir,city,'/building.gpkg'),layer = 'multipolygons')
    isochrone = st_read(paste0(res_dir,'isochrone_',max_travel_time,'min_',city,'.gpkg'))
    
    # for memory purposes, run buffer sequentially on subsets
    batchsize = 100000
    builtup = st_as_sf(st_cast(st_union(st_buffer(st_transform(st_make_valid(buildings[1:batchsize,]),crs = etrs89lcc), built_area_buffer_size)),to="POLYGON"))
    ind = batchsize+1
    while(ind<=nrow(buildings)){
      show(ind)
      newbuiltup = st_as_sf(st_cast(st_union(st_buffer(st_transform(st_make_valid(buildings[ind:min((ind+batchsize-1),nrow(buildings)),]),crs = etrs89lcc), built_area_buffer_size)),to="POLYGON"))
      tmpbuiltup = rbind(builtup,newbuiltup)
      builtup = st_as_sf(st_cast(st_union(tmpbuiltup),to="POLYGON"))
      show(paste0("  polygons in builtup = ",nrow(builtup)))
      ind = ind+batchsize
    }
    # merge to one single multipolygon and split into polygons
    #builtup_merged = st_cast(st_union(builtup),to="POLYGON")
    
    if(export_maps){
      ggsave(
        ggplot(builtup)+geom_sf(fill='black')+geom_sf(data=isochrone,color='red',fill=alpha('grey',0.1))+ggtitle(city),
        filename = paste0(res_dir,'builtup-isochrone',max_travel_time,'min_',city,'_',max_bbox_radius,'km.png'),
        width = 10, height=10, units = 'cm'
      )
    }
      
    # sampling area is intersection of builtup and isochrone
    sampling_area = st_intersection(isochrone,builtup)
    
    st_write(sampling_area,dsn=paste0(data_dir,'samplingarea_',max_travel_time,'min_',city,'_',max_bbox_radius,'km.gpkg'),layer = paste0('samplingarea_',max_travel_time,'min'),append=F)
    
  }
  
}



#####
# 8) Sample points
#####


if(processing_steps['sample_points']){
  
  for(city in names(cities)){
    
    show(paste0('Sampling points for ',city))
    
    sampling_area = st_transform(st_read(dsn = paste0(data_dir,'samplingarea_',max_travel_time,'min_',city,'_',max_bbox_radius,'km.gpkg'),layer = paste0('samplingarea_',max_travel_time,'min')),wgs84)
    
    city_centroid = get_city_centroid(city,wgs84,etrs89lcc)
    broad_bbox = st_bbox(get_bbox(city_centroid,max_bbox_radius,wgs84,etrs89lcc))
    
    # small number of points to be sampled -> rejection sampling is fine
    points=data.frame()
    while(nrow(points)<sampling_points){
      lon = runif(1,min=broad_bbox$xmin,max=broad_bbox$xmax)
      lat = runif(1,min=broad_bbox$ymin,max=broad_bbox$ymax)
      if(length(st_within(st_point(c(lon,lat)),sampling_area)[[1]])>0){
        points=rbind(points,c(lon=lon,lat=lat))
      }
    }
    names(points)<-c('lon','lat')
    
    if(export_maps){
      ggsave(
        ggplot(sampling_area)+geom_sf(fill=alpha('grey',0.3))+geom_point(data=points,aes(x=lon,y=lat),col='red')+ggtitle(city),
        filename = paste0(res_dir,'sampling_',max_travel_time,'min_',city,'_',max_bbox_radius,'km.png'),
        width = 10, height=10, units = 'cm'
      )
    }
   
    final_points = st_cast(st_sfc(st_multipoint(as.matrix(points)), crs = wgs84),to="POINT")
    
    st_write(final_points,dsn=paste0(res_dir,'sampling.gpkg'),layer = paste0('sampling_',max_travel_time,'min_',city,'_',max_bbox_radius,'km_seed',seed),append=T)
    
  }
  
}





