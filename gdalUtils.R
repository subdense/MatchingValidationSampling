






#' gdal_cmd_builder
#' 
#' Helper function for building GDAL commands.
#' 
#' @param executable Character. The GDAL command to use (e.g. "gdal_translate")
#' @param parameter_variables List. A list of parameter names, organized by type.
#' @param parameter_values List. A list of the parameters names/values.
#' @param parameter_order Character. The order of the parameters for the GDAL command.
#' @param parameter_noflags Character. Parameters which do not have a flag.
#' @param parameter_doubledash Character. Parameters which should have a double dash "--".
#' @param parameter_noquotes Character. Parameters which should not be wrapped in quotes (vector parameters only, at present).
#' @param gdal_installation_id Numeric. The ID of the GDAL installation to use.  Defaults to 1.
#' @param python_util Logical. Is the utility a python utility?  Default = FALSE.
#' @param verbose Logical. Enable verbose execution? Default is FALSE.  
#' @return Formatted GDAL command for use with system() calls. 
#' @author Jonathan A. Greenberg (\email{gdalUtils@@estarcion.net})
#' 
#' @details This function takes the executable name (e.g. "gdal_translate"),
#' a list of parameter names organized by logical, vector,
#' scalar, character, repeatable, a list of values of these parameters, 
#' the order they should be used in the GDAL command, and a list of
#' parameters that should not have a flag, and returns a properly
#' formatted GDAL command (with the full path-to-executable) that
#' should work with a system() call.
#' 
#' Sometimes, a user may not want to use the most recent GDAL install
#' (gdal_installation_id=1), so the gdal_installation_id can be used
#' to set a different install.  This is often used with gdal_chooseInstallation
#' if, for instance, the particular GDAL installation required needs
#' a specific driver that may not be available in all installations.
#' 
#' In general, an end user shouldn't need to use this function -- it
#' is used by many of the GDAL wrappers within gdalUtils.
#'
#' @references \url{http://www.gdal.org/gdal_translate.html}
#' @examples \dontrun{ 
#' # This builds a gdal_translate command.
#' executable <- "gdal_translate"
#' 
#' parameter_variables <- list(
#' 			logical = list(
#' 					varnames <- c("strict","unscale","epo",
#' 					"eco","q","sds","stats")),
#' 			vector = list(
#' 					varnames <- c("outsize","scale","srcwin",
#' 					"projwin","a_ullr","gcp")),
#' 			scalar = list(
#' 					varnames <- c("a_nodata")),
#' 			character = list(
#' 					varnames <- c("ot","of","mask","expand","a_srs",
#' 					"src_dataset","dst_dataset")),
#' 			repeatable = list(
#' 					varnames <- c("b","mo","co")))
#' 
#' parameter_order <- c(
#' 			"strict","unscale","epo","eco","q","sds","stats",
#' 			"outsize","scale","srcwin","projwin","a_ullr","gcp",
#' 			"a_nodata",
#' 			"ot","of","mask","expand","a_srs",
#' 			"b","mo","co",
#' 			"src_dataset","dst_dataset")
#' 
#' parameter_noflags <- c("src_dataset","dst_dataset")
#' 
#' # Now assign some parameters:
#' parameter_values = list(
#' 	src_dataset = "input.tif",
#' 	dst_dataset = "output.envi",
#' 	of = "ENVI",
#' 	strict = TRUE
#' )
#' 
#' cmd <- gdal_cmd_builder(
#' 			executable=executable,
#' 			parameter_variables=parameter_variables,
#' 			parameter_values=parameter_values,
#' 			parameter_order=parameter_order,
#' 			parameter_noflags=parameter_noflags)
#' 
#' cmd
#' system(cmd,intern=TRUE) 
#' }
#' @export

#TODO: additional commands
#TODO: work without parameters (executable only)
#TODO: command aliases (e.g. for commands with a hyphen, 
#	since R doesn't allow that in variable naming).

gdal_cmd_builder <- function(executable,parameter_variables=c(),
                             parameter_values=c(),parameter_order=c(),parameter_noflags=c(),
                             parameter_doubledash=c(),
                             parameter_noquotes=c(),
                             gdal_installation_id=1,
                             python_util=FALSE,
                             verbose=FALSE)
{
  if(verbose) message("Checking installation...")
  # path to executable check in here?
  gdal_setInstallation()
  if(is.null(getOption("gdalUtils_gdalPath"))) return()
  
  executable <- normalizePath(list.files(
    getOption("gdalUtils_gdalPath")[[gdal_installation_id]]$path,
    executable,full.names=TRUE))
  
  if(!file.exists(executable) && !file.exists(paste0(executable,".exe")))
  {
    stop(paste0(executable," does not exist on your system.  Please check your installation."))
  }
  
  parameter_variables_types <- names(parameter_variables)
  
  # print(sapply(parameter_values,function(X) class(X)))
  
  defined_variables <- names(parameter_values)[sapply(parameter_values,function(X) class(X)[1] != "name")]
  
  if(verbose) message("Setting up logical variables...")
  
  if(any("logical" %in% parameter_variables_types))
  {
    parameter_variables_logical <- parameter_variables$logical[[1]]
    parameter_variables_logical_defined <- defined_variables[defined_variables %in% parameter_variables_logical]
    # Only set the flag if TRUE
    # browser()
    if(length(parameter_variables_logical_defined)>0)
    {
      
      #			browser()
      parameter_variables_logical_defined_true <- sapply(parameter_variables_logical_defined,
                                                         function(X,parameter_values)
                                                         {
                                                           return(parameter_values[[which(names(parameter_values)==X)]])
                                                         },parameter_values=parameter_values)
      
      # parameter_variables_logical_defined_true <- parameter_variables_logical_defined_true[parameter_variables_logical_defined_true==T]
      
      #			browser()
      
      parameter_variables_logical_strings <- sapply(parameter_variables_logical_defined,
                                                    function(X,parameter_doubledash)
                                                    {
                                                      if(X %in% parameter_noflags)
                                                      {
                                                        flag=NULL
                                                      } else
                                                      {
                                                        if(X %in% parameter_doubledash)
                                                        {
                                                          flag=paste("--",X," ",sep="")	
                                                        } else
                                                        {
                                                          flag=paste("-",X," ",sep="")
                                                        }
                                                      }
                                                      return(flag)
                                                    },parameter_doubledash=parameter_doubledash)	
      names(parameter_variables_logical_strings) <- names(parameter_variables_logical_defined_true)
      # Get rid of ones you don't need:
      parameter_variables_logical_strings <- parameter_variables_logical_strings[parameter_variables_logical_defined_true==T]
      
    } else
    {
      parameter_variables_logical_strings <- NULL
    }
    
  }
  
  if(verbose) message("Setting up vector variables...")
  
  if(any("vector" %in% parameter_variables_types))
  {
    parameter_variables_vector <- parameter_variables$vector[[1]]
    parameter_variables_vector_defined <- defined_variables[defined_variables %in% parameter_variables_vector]
    if(length(parameter_variables_vector_defined)>0)
    {
      parameter_variables_vector_strings <- sapply(parameter_variables_vector_defined,
                                                   function(X,parameter_values,parameter_doubledash)
                                                   {
                                                     if(X %in% parameter_noflags)
                                                     {
                                                       flag=NULL
                                                     } else
                                                     {
                                                       if(X %in% parameter_doubledash)
                                                       {
                                                         flag=paste("--",X," ",sep="")	
                                                       } else
                                                       {
                                                         flag=paste("-",X," ",sep="")
                                                       }
                                                     }
                                                     
                                                     if(X %in% parameter_noquotes)
                                                     {
                                                       parameter_variables_vector_string <- paste(flag,
                                                                                                  paste(parameter_values[[which(names(parameter_values)==X)]],collapse=" "),
                                                                                                  sep="")
                                                     } else
                                                     {						
                                                       parameter_variables_vector_string <- paste(flag,
                                                                                                  qm(paste(parameter_values[[which(names(parameter_values)==X)]],collapse=" ")),
                                                                                                  sep="")
                                                     }
                                                     return(parameter_variables_vector_string)
                                                   },parameter_values=parameter_values,parameter_doubledash=parameter_doubledash)			
    } else
    {
      parameter_variables_vector_strings <- NULL
    }
  } else
  {
    parameter_variables_vector_strings <- NULL
  }
  
  if(verbose) message("Setting up scalar variables...")
  
  if(any("scalar" %in% parameter_variables_types))
  {
    parameter_variables_scalar <- parameter_variables$scalar[[1]]
    parameter_variables_scalar_defined <- defined_variables[defined_variables %in% parameter_variables_scalar]
    if(length(parameter_variables_scalar_defined)>0)
    {
      parameter_variables_scalar_strings <- sapply(parameter_variables_scalar_defined,
                                                   function(X,parameter_values,parameter_doubledash)
                                                   {
                                                     if(X %in% parameter_noflags)
                                                     {
                                                       flag=NULL
                                                     } else
                                                     {
                                                       if(X %in% parameter_doubledash)
                                                       {
                                                         flag=paste("--",X," ",sep="")	
                                                       } else
                                                       {
                                                         flag=paste("-",X," ",sep="")
                                                       }
                                                     }
                                                     parameter_variables_scalar_string <- paste(flag,
                                                                                                qm(parameter_values[[which(names(parameter_values)==X)]]),
                                                                                                sep="")
                                                     return(parameter_variables_scalar_string)
                                                   },parameter_values=parameter_values,parameter_doubledash=parameter_doubledash)			
    } else
    {
      parameter_variables_scalar_strings <- NULL
    }
  } else
  {
    parameter_variables_scalar_strings <- NULL
  }
  
  if(verbose) message("Setting up character variables...")
  
  if(any("character" %in% parameter_variables_types))
  {
    # Do we need to embed quotes in the command?
    parameter_variables_character <- parameter_variables$character[[1]]
    parameter_variables_character_defined <- defined_variables[defined_variables %in% parameter_variables_character]
    if(length(parameter_variables_character_defined)>0)
    {
      parameter_variables_character_strings <- sapply(parameter_variables_character_defined,
                                                      function(X,parameter_values,parameter_noflags,parameter_doubledash)
                                                      {
                                                        if(X %in% parameter_noflags)
                                                        {
                                                          flag=NULL
                                                        } else
                                                        {
                                                          if(X %in% parameter_doubledash)
                                                          {
                                                            flag=paste("--",X," ",sep="")	
                                                          } else
                                                          {
                                                            flag=paste("-",X," ",sep="")
                                                          }
                                                        }
                                                        parameter_variables_character_string <- paste(flag,
                                                                                                      qm(parameter_values[[which(names(parameter_values)==X)]]),
                                                                                                      sep="")
                                                        return(parameter_variables_character_string)
                                                      },parameter_values=parameter_values,parameter_noflags=parameter_noflags,parameter_doubledash=parameter_doubledash)			
    } else
    {
      parameter_variables_character_strings <- NULL
    }
  } else
  {
    parameter_variables_character_strings <- NULL
  }
  
  if(verbose) message("Setting up repeatable variables...")
  
  
  if(any("repeatable" %in% parameter_variables_types))
  {
    parameter_variables_repeatable <- parameter_variables$repeatable[[1]]
    parameter_variables_repeatable_defined <- defined_variables[defined_variables %in% parameter_variables_repeatable]
    if(length(parameter_variables_repeatable_defined)>0)
    {
      parameter_variables_repeatable_strings <- sapply(parameter_variables_repeatable_defined,
                                                       function(X,parameter_values,parameter_doubledash)
                                                       {
                                                         #						if(X == "gcp") browser()
                                                         
                                                         if(X %in% parameter_noflags)
                                                         {
                                                           flag=NULL
                                                         } else
                                                         {
                                                           if(X %in% parameter_doubledash)
                                                           {
                                                             flag=paste("--",X," ",sep="")	
                                                           } else
                                                           {
                                                             flag=paste("-",X," ",sep="")
                                                           }
                                                         }
                                                         
                                                         if(X %in% parameter_noquotes)
                                                         {
                                                           parameter_variables_repeatable_string <- paste(
                                                             paste(flag,
                                                                   (parameter_values[[which(names(parameter_values)==X)]]),
                                                                   sep=""),
                                                             collapse=" ")			
                                                         } else
                                                         {	
                                                           parameter_variables_repeatable_string <- paste(
                                                             paste(flag,
                                                                   qm(parameter_values[[which(names(parameter_values)==X)]]),
                                                                   sep=""),
                                                             collapse=" ")
                                                         }
                                                         return(parameter_variables_repeatable_string)
                                                       },parameter_values=parameter_values,parameter_doubledash=parameter_doubledash)			
    } else
    {
      parameter_variables_repeatable_strings <- NULL
    }
  } else
  {
    parameter_variables_repeatable_strings <- NULL
  }
  
  if(verbose) message("Setting up noflag variables...")
  
  
  if(!is.null(parameter_noflags))
  {
    #		parameter_variables_noflag <- parameter_variables$noflag[[1]]
    #		parameter_variables_noflag_defined <- defined_variables[defined_variables %in% parameter_variables_noflag]
    #		if(length(parameter_variables_noflag_defined)>0)
    #		{
    parameter_variables_noflag_strings <- sapply(parameter_noflags,
                                                 function(X,parameter_values)
                                                 {
                                                   parameter_variables_noflag_string <- paste(
                                                     parameter_values[[which(names(parameter_values)==X)]],
                                                     sep="")
                                                   return(parameter_variables_noflag_string)
                                                 },parameter_values=parameter_values)			
    #		} else
    #		{
    #			parameter_variables_noflag_strings <- NULL
    #		}
  } else
  {
    parameter_variables_noflag_strings <- NULL	
  }
  
  if(verbose) message("Putting them all together...")
  
  
  parameter_vector <- c(
    parameter_variables_logical_strings,
    parameter_variables_vector_strings,
    parameter_variables_scalar_strings,
    parameter_variables_character_strings,
    parameter_variables_repeatable_strings,
    parameter_variables_noflag_strings
  )
  
  # Reorder the parameters if neccessary
  if(!missing(parameter_order))
  {
    parameter_order_defined <- parameter_order[which(parameter_order %in% names(parameter_vector))]
    parameter_vector <- parameter_vector[parameter_order_defined]
  }
  
  # Collapse multiple parameter entries:
  parameter_vector <- sapply(parameter_vector,function(x) paste(x,collapse=" "))
  
  cmd <- paste(c(qm(executable),parameter_vector),collapse=" ")
  
  #	if(python_util)
  #	{
  #		py_check <- py_available(initialize=T)
  #		if(!py_check) stop("Python not available, please fix.")
  #		cmd <- paste(py_config()$python,cmd)
  #	}
  
  return(cmd)
  
}












#' ogr2ogr
#' 
#' R wrapper for ogr2ogr: converts simple features data between file formats
#' 
#' @param src_datasource_name Character. Input vector file or input directory.
#' @param dst_datasource_name Character. Output vector file or input directory.
#' @param layer Character. Layer to use.
#' @param f Character. output file format name (default is ESRI Shapefile), some possible values are: "ESRI Shapefile", "TIGER", "MapInfo File", "GML", "PostgreSQL"
#' @param append Logical. Append to existing layer instead of creating new
#' @param overwrite Logical. Delete the output layer and recreate it empty.
#' @param update Logical. Open existing output datasource in update mode rather than trying to create a new one
#' @param select Character. Comma-delimited list of fields from input layer to copy to the new layer. A field is skipped if mentioned previously in the list even if the input layer has duplicate field names. (Defaults to all; any field is skipped if a subsequent field with same name is found.) Starting with OGR 2.0, geometry fields can also be specified in the list.
#' @param progress Logical. (starting with GDAL 1.7.0) Display progress on terminal. Only works if input layers have the "fast feature count" capability.
#' @param sql Character. SQL statement to execute. The resulting table/layer will be saved to the output.
#' @param dialect Character. SQL dialect. In some cases can be used to use (unoptimized) OGR SQL instead of the native SQL of an RDBMS by passing OGRSQL. Starting with GDAL 1.10, the "SQLITE" dialect can also be used with any datasource.
#' @param where Character. Attribute query (like SQL WHERE).
#' @param skipfailures Logical. Continue after a failure, skipping the failed feature.
#' @param spat Numeric. c(xmin,ymin,xmax,ymax) spatial query extents. Only features whose geometry intersects the extents will be selected. The geometries will not be clipped unless -clipsrc is specified
#' @param spat_srs Character. srs_def. (OGR >= 2.0) Override spatial filter SRS.
#' @param geomfield Character. (OGR >= 1.11) Name of the geometry field on which the spatial filter operates on.
#' @param dsco Character. Dataset creation option (format specific).
#' @param lco Character. Layer creation option (format specific).
#' @param nln Character. Assign an alternate name to the new layer.
#' @param nlt Character. Define the geometry type for the created layer. One of NONE, GEOMETRY, POINT, LINESTRING, POLYGON, GEOMETRYCOLLECTION, MULTIPOINT, MULTIPOLYGON or MULTILINESTRING. Add "25D" to the name to get 2.5D versions. Starting with GDAL 1.10, PROMOTE_TO_MULTI can be used to automatically promote layers that mix polygon or multipolygons to multipolygons, and layers that mix linestrings or multilinestrings to multilinestrings. Can be usefull when converting shapefiles to PostGIS (and other target drivers) that implements strict checks for geometry type.
#' @param dim Numeric. (starting with GDAL 1.10) Force the coordinate dimension to val (valid values are 2 or 3). This affects both the layer geometry type, and feature geometries. Starting with GDAL 2.0, the value can be set to "layer_dim" to instruct feature geometries to be promoted to the coordinate dimension declared by the layer.
#' @param a_srs Character. Assign an output SRS.
#' @param t_srs Character. Reproject/transform to this SRS on output.
#' @param s_srs Character. Override source SRS.
#' @param preserve_fid Logical. Use the FID of the source features instead of letting the output driver to automatically assign a new one.
#' @param fid Character. If provided, only the feature with this feature id will be reported. Operates exclusive of the spatial or attribute queries. Note: if you want to select several features based on their feature id, you can also use the fact the 'fid' is a special field recognized by OGR SQL. So, '-where "fid in (1,3,5)"' would select features 1, 3 and 5.

#' @param oo Character. "NAME=VALUE". (starting with GDAL 2.0) Input dataset open option (format specific).
#' @param doo Character. "NAME=VALUE". (starting with GDAL 2.0) Destination dataset open option (format specific), only valid in -update mode.
#' @param gt Numeric. group n features per transaction (default 200). Increase the value for better performance when writing into DBMS drivers that have transaction support.
#' @param ds_transaction Logical. (starting with GDAL 2.0) Force the use of a dataset level transaction (for drivers that support such mechanism), especially for drivers such as FileGDB that only support dataset level transaction in emulation mode.
#' @param clipsrc Character.  [xmin ymin xmax ymax]|WKT|datasource|spat_extent: (starting with GDAL 1.7.0) clip geometries to the specified bounding box (expressed in source SRS), WKT geometry (POLYGON or MULTIPOLYGON), from a datasource or to the spatial extent of the -spat option if you use the spat_extent keyword. When specifying a datasource, you will generally want to use it in combination of the -clipsrclayer, -clipsrcwhere or -clipsrcsql options
#' @param clipsrcsql Character. Select desired geometries using an SQL query instead.
#' @param clipsrclayer Character. Select the named layer from the source clip datasource.
#' @param clipsrcwhere Character. Restrict desired geometries based on attribute query.
#' @param clipdst Character. (starting with GDAL 1.7.0) clip geometries after reprojection to the specified bounding box (expressed in dest SRS), WKT geometry (POLYGON or MULTIPOLYGON) or from a datasource. When specifying a datasource, you will generally want to use it in combination of the -clipdstlayer, -clipdstwhere or -clipdstsql options
#' @param clipdstsql Character. Select desired geometries using an SQL query instead.
#' @param clipdstlayer Character. Select the named layer from the destination clip datasource.
#' @param clipdstwhere Character. Restrict desired geometries based on attribute query.
#' @param wrapdateline Logical. (starting with GDAL 1.7.0) split geometries crossing the dateline meridian (long. = +/- 180deg).
#' @param datelineoffset Logical. (starting with GDAL 1.10) offset from dateline in degrees (default long. = +/- 10deg, geometries within 170deg to -170deg will be splited)
#' @param simplify Numeric. (starting with GDAL 1.9.0) distance tolerance for simplification. Note: the algorithm used preserves topology per feature, in particular for polygon geometries, but not for a whole layer.
#' @param segmentize Numeric. (starting with GDAL 1.6.0) maximum distance between 2 nodes. Used to create intermediate points
#' @param fieldTypeToString Character. (starting with GDAL 1.7.0) converts any field of the specified type to a field of type string in the destination layer. Valid types are : Integer, Real, String, Date, Time, DateTime, Binary, IntegerList, RealList, StringList. Special value All can be used to convert all fields to strings. This is an alternate way to using the CAST operator of OGR SQL, that may avoid typing a long SQL query.
#' @param mapFieldType Character. srctype|All=dsttype,... (starting with GDAL 2.0) converts any field of the specified type to another type. Valid types are : Integer, Integer64, Real, String, Date, Time, DateTime, Binary, IntegerList, Integer64List, RealList, StringList. Types can also include subtype between parenthesis, such as Integer(Boolean), Real(Float32), ... Special value All can be used to convert all fields to another type. This is an alternate way to using the CAST operator of OGR SQL, that may avoid typing a long SQL query. This is a generalization of -fieldTypeToString. Note that this does not influence the field types used by the source driver, and is only an afterwards conversion.
#' @param unsetFieldWidth Logical. (starting with GDAL 2.0) set field width and precision to 0.		
#' @param splitlistfields Logical. (starting with GDAL 1.8.0) split fields of type StringList, RealList or IntegerList into as many fields of type String, Real or Integer as necessary.
#' @param maxsubfields Numeric. To be combined with -splitlistfields to limit the number of subfields created for each split field.
#' @param explodecollections Logical. (starting with GDAL 1.8.0) produce one feature for each geometry in any kind of geometry collection in the source file.
#' @param zfield Character. (starting with GDAL 1.8.0) Uses the specified field to fill the Z coordinate of geometries.
#' @param gcp Numeric. c(ungeoref_x,ungeoref_y,georef_x georef_y,elevation) (starting with GDAL 1.10.0) Add the indicated ground control point. This option may be provided multiple times to provide a set of GCPs.
#' @param order Numeric. (starting with GDAL 1.10.0) order of polynomial used for warping (1 to 3). The default is to select a polynomial order based on the number of GCPs.
#' @param tps Logical. (starting with GDAL 1.10.0) Force use of thin plate spline transformer based on available GCPs.
#' @param fieldmap Character. (starting with GDAL 1.10.0) Specifies the list of field indexes to be copied from the source to the destination. The (n)th value specified in the list is the index of the field in the target layer definition in which the n(th) field of the source layer must be copied. Index count starts at zero. There must be exactly as many values in the list as the count of the fields in the source layer. We can use the 'identity' setting to specify that the fields should be transferred by using the same order. This setting should be used along with the -append setting.
#' @param addfields Logical. (starting with GDAL 2.0) This is a specialized version of -append. Contrary to -append, -addfields has the effect of adding, to existing target layers, the new fields found in source layers. This option is usefull when merging files that have non-strictly identical structures. This might not work for output formats that don't support adding fields to existing non-empty layers.
#' @param relaxedFieldNameMatch Logical. (starting with GDAL 1.11) Do field name matching between source and existing target layer in a more relaxed way if the target driver has an implementation for it. [-relaxedFieldNameMatch] [-forceNullable]
#' @param forceNullable Logical. (starting with GDAL 2.0) Do not propagate not-nullable constraints to target layer if they exist in source layer.
#' @param unsetDefault Logical. (starting with GDAL 2.0) Do not propagate default field values to target layer if they exist in source layer.
#' @param unsetFid Logical. (starting with GDAL 2.0) Can be specify to prevent the new default behaviour that consists in, if the output driver has a FID layer creation option and we are not in append mode, to preserve the name of the source FID column and source feature IDs.
#' @param nomd Logical. (starting with GDAL 2.0) To disable copying of metadata from source dataset and layers into target dataset and layers, when supported by output driver.
#' @param mo Character. "META-TAG=VALUE". (starting with GDAL 2.0) Passes a metadata key and value to set on the output dataset, when supported by output driver.
## @param additional_commands Character. Additional commands to pass directly to ogrinfo.
#' @param ignore.full_scan Logical. If FALSE, perform a brute-force scan if other installs are not found.  Default is TRUE.
#' @param verbose Logical. Enable verbose execution? Default is FALSE.  
#'  
#' @return character
#' @author Jonathan A. Greenberg (\email{gdalUtils@@estarcion.net}) (wrapper) and Frank Warmerdam (GDAL lead developer).
#' @details This is an R wrapper for the 'ogr2ogr' function that is part of the 
#' Geospatial Data Abstraction Library (GDAL).  It follows the parameter naming
#' conventions of the original function, with some modifications to allow for more R-like
#' parameters.  For all parameters, the user can use a single character string following,
#' precisely, the gdalinfo format (\url{http://gdal.org/ogrinfo.html}), or,
#' in some cases, can use R vectors to achieve the same end.  
#' 
#' PERFORMANCE HINTS
#' 
#' When writing into transactional DBMS (SQLite/PostgreSQL,MySQL, etc...), it might be 
#' beneficial to increase the number of INSERT statements executed between BEGIN TRANSACTION 
#' and COMMIT TRANSACTION statements. This number is specified with the -gt option. For 
#' example, for SQLite, explicitly defining -gt 65536 ensures optimal performance while 
#' populating some table containing many hundredth thousand or million rows. However, note 
#' that if there are failed insertions, the scope of -skipfailures is a whole transaction.
#' 
#' For PostgreSQL, the PG_USE_COPY config option can be set to YES for significantly insertion
#' performance boot. See the PG driver documentation page.
#' 
#' More generally, consult the documentation page of the input and output drivers for performance hints.
#' 
#' NOTE FOR SQL USERS: When using SQL statements via the sql="some sql statement", be aware the src_datasource_name and
#' dst_datasource_name should still be set.  src_datasource_name is treated as the path to the 
#' tables/vectors called in the SQL statement, and dst_datasource_name will be the folder the outputs will be stored in.
#' 
#' This function assumes the user has a working GDAL on their system.  If the 
#' "gdalUtils_gdalPath" option has been set (usually by gdal_setInstallation),
#' the GDAL found in that path will be used.  If nothing is found, gdal_setInstallation
#' will be executed to attempt to find a working GDAL.
#'
#' @references \url{http://www.gdal.org/ogr2ogr.html}
#' 
#' @examples 
#' # We'll pre-check to make sure there is a valid GDAL install.
#' # Note this isn't strictly neccessary, as executing the function will
#' # force a search for a valid GDAL install.
#' gdal_setInstallation()
#' valid_install <- !is.null(getOption("gdalUtils_gdalPath"))
#' if(valid_install)
#' {
#' src_datasource_name <- system.file("external/tahoe_highrez_training.shp", package="gdalUtils")
#' dst_datasource_name <- paste(tempfile(),".shp",sep="")
#' ogrinfo(src_datasource_name,"tahoe_highrez_training")
#' # reproject the input to mercator
#' ogr2ogr(src_datasource_name,dst_datasource_name,t_srs="EPSG:3395",verbose=TRUE)
#' ogrinfo(dirname(dst_datasource_name),layer=remove_file_extension(basename(dst_datasource_name)))
#' }
#' @export

ogr2ogr <- function(src_datasource_name,dst_datasource_name,
                    layer,
                    f,append,overwrite,update,select,progress,sql,dialect,
                    where,skipfailures,spat,spat_srs,geomfield,dsco,lco,
                    nln,nlt,dim,a_srs,t_srs,s_srs,preserve_fid,fid,
                    oo,doo,gt,ds_transaction,
                    clipsrc,clipsrcsql,clipsrclayer,
                    clipsrcwhere,clipdst,clipdstsql,clipdstlayer,
                    clipdstwhere,wrapdateline,datelineoffset,
                    simplify,segmentize,
                    fieldTypeToString,mapFieldType,unsetFieldWidth,
                    splitlistfields,maxsubfields,
                    explodecollections,zfield,gcp,order,
                    tps,fieldmap,addfields,relaxedFieldNameMatch,
                    forceNullable,unsetDefault,unsetFid,
                    nomd,mo,
                    #		additional_commands,
                    ignore.full_scan=TRUE,
                    verbose=FALSE)
{
  
  parameter_values <- as.list(environment())
  
  if(verbose) message("Checking gdal_installation...")
  gdal_setInstallation(ignore.full_scan=ignore.full_scan,verbose=verbose)
  if(is.null(getOption("gdalUtils_gdalPath"))) return()
  
  # Start gdalinfo setup
  parameter_variables <- list(
    logical = list(
      varnames <- c("append","overwrite","update",
                    "progress","skipfailures",
                    "preserve_fid",
                    "ds_transaction",
                    "wrapdateline",
                    "datelineoffset","unsetFieldWidth",
                    "splitlistfields","explodecollections",
                    "tps","addfields","relaxedFieldNameMatch",
                    "forceNullable","unsetDefault","unsetFid",
                    "nomd","mo")),
    vector = list(
      varnames <- c("spat","clipdst")),
    scalar = list(
      varnames <- c("dim","gt","simplify","segmentize",
                    "maxsubfields","order")),
    character = list(
      varnames <- c("f","select","sql","dialect",
                    "where","spat_srs","geomfield","dsco","lco","nln","nlt",
                    "a_srs","t_srs","s_srs",
                    "fid","oo","doo",
                    #						"clipsrc",
                    "clipsrcsql","clipsrclayer",
                    "clipsrcwhere","clipdst","clipdstsql",
                    "clipdstlayer","clipdstwhere","fieldTypeToString",
                    "mapFieldType",
                    "zfield","fieldmap",
                    "dst_datasource_name","src_datasource_name",
                    "layer")),
    repeatable = list(
      varnames <- c("gcp"))
  )
  
  # Fix for clipsrc bug reported by Alex Zvoleff, 5/11/2015
  if(!missing(clipsrc))
  {
    if(is.numeric(clipsrc))
    {
      parameter_variables$vector[[1]] <- c(parameter_variables$vector[[1]],"clipsrc")
    } else
    {
      parameter_variables$character[[1]] <- c(parameter_variables$character[[1]],"clipsrc")
    }
  }
  
  #	browser()
  
  parameter_order <- c(
    "append","overwrite","update",
    "progress","skipfailures",
    "preserve_fid",
    "ds_transaction",
    "wrapdateline",
    "datelineoffset","unsetFieldWidth",
    "splitlistfields","explodecollections",
    "tps","addfields","relaxedFieldNameMatch",
    "forceNullable","unsetDefault","unsetFid",
    "nomd","mo",
    "spat","clipdst",
    "dim","gt","simplify","segmentize",
    "maxsubfields","order",
    "f","select","sql","dialect",
    "where","spat_srs","geomfield","dsco","lco","nln","nlt",
    "a_srs","t_srs","s_srs",
    "fid","oo","doo","clipsrc","clipsrcsql","clipsrclayer",
    "clipsrcwhere","clipdst","clipdstsql",
    "clipdstlayer","clipdstwhere","fieldTypeToString",
    "mapFieldType",
    "zfield","fieldmap",
    "gcp",			
    "dst_datasource_name","src_datasource_name",
    "layer"
  )
  
  parameter_noflags <- c("dst_datasource_name","src_datasource_name","layer")
  
  parameter_doubledash <- NULL
  
  parameter_noquotes <- c("spat","clipsrc")
  
  executable <- "ogr2ogr"
  # End ogr2ogr setup
  
  cmd <- gdal_cmd_builder(
    executable=executable,
    parameter_variables=parameter_variables,
    parameter_values=parameter_values,
    parameter_order=parameter_order,
    parameter_noflags=parameter_noflags,
    parameter_doubledash=parameter_doubledash,
    parameter_noquotes=parameter_noquotes)
  
  if(verbose) message(paste("GDAL command being used:",cmd))
  
  cmd_output <- system(cmd,intern=TRUE) 
  
  return(cmd_output)
}