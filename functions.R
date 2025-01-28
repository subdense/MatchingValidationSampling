

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


get_city_centroid<-function(city,wgs84,etrs89lcc){
  return(st_transform(st_centroid(st_sfc(st_multipoint(t(osmdata::getbb(city)))) %>% st_set_crs(wgs84)), etrs89lcc))
}


get_bbox<-function(city_centroid,max_bbox_radius,wgs84,etrs89lcc){
  bbox_data = c(st_coordinates(city_centroid)[,1] - max_bbox_radius*1000, st_coordinates(city_centroid)[,2] - max_bbox_radius*1000,
                st_coordinates(city_centroid)[,1] - max_bbox_radius*1000, st_coordinates(city_centroid)[,2] + max_bbox_radius*1000,
                st_coordinates(city_centroid)[,1] + max_bbox_radius*1000, st_coordinates(city_centroid)[,2] - max_bbox_radius*1000,
                st_coordinates(city_centroid)[,1] + max_bbox_radius*1000, st_coordinates(city_centroid)[,2] + max_bbox_radius*1000
  )
  
  return(st_bbox(st_transform(st_sfc(st_multipoint(matrix(data = bbox_data, ncol = 2, byrow = T)), crs = etrs89lcc), wgs84)) %>% st_as_sfc())
}


filter_osmpbf_file<-function(city,cities,osm_data_dir,data_dir,broad_bbox,objects_type){
  
  datafiles=c()
  for(country in cities[[city]]){
    datafile_root = paste0('geofabrik_',strsplit(tail(strsplit(oe_match(country)$url,'/',fixed=T)[[1]], n=1),'.',fixed=T)[[1]][1])
    datafile = paste0(datafile_root,'.osm.pbf')
    outputfile = paste0(osm_data_dir,datafile_root,'_',city,'_',objects_type,'.osm.pbf')
    datafiles = append(datafiles,outputfile)
    
    # osmosis command to extract from bounding box and filter highways
    system(paste0('osmosis --read-pbf ', osm_data_dir, datafile,
                  ' --bounding-box top=', st_bbox(broad_bbox)$ymax,
                  ' left=',st_bbox(broad_bbox)$xmin, ' bottom=',st_bbox(broad_bbox)$ymin,
                  ' right=',st_bbox(broad_bbox)$xmax,' --tf accept-ways ',objects_type,'=* --used-node --write-pbf ',
                  outputfile)
    )
  }
  
  # osmosis command to merge pbf files (command `--merge` has to appear N_files - 1)
  system(
    paste0('osmosis --rb ',paste0(datafiles,collapse = ' --rb '),' ',paste0(rep('--merge',length(cities[[city]])-1),collapse=' '),' --wb ',osm_data_dir,city,"_",objects_type,".osm.pbf")
  )
  
  # copy final file to data dir
  system(paste0('cp ',osm_data_dir,city,'_',objects_type,'.osm.pbf ',data_dir,city,'/',objects_type,'.osm.pbf'))
  
}

