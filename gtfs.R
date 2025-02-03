
# GTFS data ids and ad hoc preprocessing


# MobilityDatabase ids for GTFS feeds (to be adjusted by hand)
gtfs_feeds = list(
  'Luxembourg'=c(1091),#c(1108,1091),# Chemins Fer Luxembourgeois, Aggregated Luxembourg -> only Aggreg (uncompatible dates)
  'Toulouse'=c(1024, 1205),# Tisséo, TER France
  'Strasbourg'=c(856, 1205), # Compagnie des Transports Strasbourgeois (CTS), TER France ; no German trains needed for now
  'Dortmund'=c(),
  'Frankfurt'=c()
)
# GTFS dates on which travel time is computed (need a weekday with a date in all used feeds)
#  -> to be checked by hand (can use tidytransit package : tidytransit::read_gtfs()$calendar_dates$date)
#  for example Toulouse Tisseo and TER are updated regularly and only recent dates (+-1month ~) are available
gtfs_dates = list(
  'Luxembourg'='29-10-2024',
  'Toulouse'='31-01-2025',
  'Strasbourg'='31-01-2025'
)


# adhoc preprocessing functions, such as replacing calendar_dates to have common dates between two feeds

gtfs_preprocessing = list(
  'Luxembourg'=identity,
  'Toulouse'=identity,
  'Strasbourg'= function(gtfs,feedid){
    if(feedid==856){
      show('Correcting GTFS calendar date for Strasbourg : CTS : 2024-08-15 -> 2025-01-31')
      gtfs$calendar_dates$date[gtfs$calendar_dates$date=="2024-08-15"]="2025-01-31"
    }
    return(gtfs)
  },
  'Dortmund'=identity,
  'Frankfurt'=identity
)





