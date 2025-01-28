

## Old code / testing

#library(gdalUtils)
#library(usethis)

#library(devtools)
#devtools::install_github("gearslaboratory/gdalUtils")
# -> fail, requires rgdal which has been discontinued
#source('gdalUtils.R') # code version: https://github.com/gearslaboratory/gdalUtils/tree/784b61c43727cbc9d27b275aa78d3e707ef04225



# setup download directory -> OSMEXT_DOWNLOAD_DIRECTORY environment variable
#usethis::edit_r_environ()
#readRenviron(".Renviron")

#test <- oe_get("Strasbourg",quiet = FALSE,
#               query = "SELECT * FROM 'lines' WHERE highway IS NOT NULL" # highway column: 78312 NAs out of 371577 with no filter
#)
# specifying regions above could save some data, or using city name matching in oe_get function, but this does not tackle the issue of trans-boundary regions




#oe_get(country, download_only=T, skip_vectortranslate = T, max_file_size = 5e10)


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

# ! fail -> ogr2ogr read osm, not pbf -> use directly osmosis
#system(paste0('ogr2ogr ',osm_data_dir,highwaysfile_gpkg,' ',osm_data_dir, highwaysfile))
# merge if more than one countries
#if(length(cities[city])>1){
#  network = rbind(network,st_read())
#}


#st_write(network, dsn = paste0(data_dir,city,'/network.gpkg'), layer = city)
#sf::gdal_utils(util = "vectortranslate", source = paste0(data_dir,city,'/network.gpkg'), destination = paste0(data_dir,city,'/network.osm.pbf'),options = c("-f", "OSM"))






#######
## GTFS : API real time feeds?

#  Register an account on the database and put your refresh token in .mobilitydatabase
#refreshToken = gsub('\n','',read_file(paste0(data_dir,'.mobilitydatabase')), fixed=T)
#req = POST('https://api.mobilitydatabase.org/v1/tokens', add_headers('Content-Type' = 'application/json'), body=paste0('{ "refresh_token": "',refreshToken,'" }'))
#token = content(req)$access_token


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



#### isochrones
# pete en mem avec 100km
#isochrone = r5r::isochrone(network,origins = orig ,mode=c( "TRANSIT", "CAR"))
#plot(isochrone[2,])




# Get buildings within the area

# export data : zenodo, nakala? (flat), (~dataverse~)
