#Load election results
election_boundaries = readOGR(dsn = './referendumData/Geotheory-UK-2016-EU-Referendum/', layer = 'Geotheory-UK-2016-EU-Referendum')

#Projection to match precipitation data
new_proj = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
election_boundaries_proj = spTransform(election_boundaries, CRS(new_proj))

#Load precipitation data
processed_files = paste0('./getImages/processed/', list.files('./getImages/processed/'))
precip = stack(processed_files)

#Create data for each half hour step
#From 12 hours pre-polling to 12 hours post-polling
#2016-06-22 19:00:00 to 2016-06-24 10:00:00
times = 337:414

constituency_rainfall_list = vector(mode = 'list', length = length(times))

#Hold constituency level data
constituency_id_data = election_boundaries_proj@data[, 1:5]
constituency_geo_ids = as.numeric(row.names(election_boundaries_proj@data))

#Loop over half-hour intervals
for (i in 1:length(times)) {
  print(i / length(times))
  time = times[i]
  d = unlist(strsplit(names(precip[[time]]), '\\.'))
  start_time = strptime(paste(d[1],d[2]), "X%Y%m%d S%H%M%S", tz = 'UTC')
  end_time = strptime(paste(d[1],d[3]), "X%Y%m%d E%H%M%S", tz = 'UTC')
  
  #Make temp.table
  temp.table = as.data.table(constituency_id_data)
  temp.table[, geo_id := constituency_geo_ids]
  temp.table[, start := as.character(start_time)]
  temp.table[, end := as.character(end_time)]
  
  #Extract mean precipitation
  precip.time = crop(precip[[time]], extent(election_boundaries_proj))
  temp.rainfall = extract(precip.time, election_boundaries_proj, weights = F, small = T, fun = mean, na.rm = T)

  temp.table[, precip_mm := temp.rainfall]
  
  constituency_rainfall_list[[i]] = temp.table
}

#Combine all timeperiods
constituency_rainfall_data = do.call('rbind' , constituency_rainfall_list)

#Get time period start and end points
constituency_rainfall_data[, start_time := as.numeric(strptime(start, format = "%Y-%m-%d %H:%M:%S", tz = 'UTC'))]
constituency_rainfall_data[, end_time := as.numeric(strptime(end, format = "%Y-%m-%d %H:%M:%S", tz = 'UTC'))]

#Define types of rain
constituency_rainfall_data[, heavy_rain := 0]
constituency_rainfall_data[precip_mm > 10 & precip_mm <= 50 , heavy_rain := 1]

constituency_rainfall_data[, medium_rain := 0]
constituency_rainfall_data[precip_mm > 2 & precip_mm <= 10 , medium_rain := 1]


constituency_rainfall_data[, light_rain := 0]
constituency_rainfall_data[precip_mm > 0 & precip_mm <= 2 , light_rain := 1]

#Define polling times
polls_open = as.numeric(strptime("2016-06-23 07:00:00", format = "%Y-%m-%d %H:%M:%S", tz = 'UTC'))
polls_close = as.numeric(strptime("2016-06-23 22:00:00", format = "%Y-%m-%d %H:%M:%S", tz = 'UTC'))

#Define rush hour times
rush_1a = as.numeric(strptime("2016-06-23 07:00:00", format = "%Y-%m-%d %H:%M:%S", tz = 'UTC'))
rush_1b = as.numeric(strptime("2016-06-23 10:00:00", format = "%Y-%m-%d %H:%M:%S", tz = 'UTC'))
rush_2a = as.numeric(strptime("2016-06-23 16:00:00", format = "%Y-%m-%d %H:%M:%S", tz = 'UTC'))
rush_2b = as.numeric(strptime("2016-06-23 20:00:00", format = "%Y-%m-%d %H:%M:%S", tz = 'UTC'))

rush_hour = c(rush_1a:(rush_1b-1), rush_2a:(rush_2b-1))


#Make data for distinct time periods
rainfall_polling = constituency_rainfall_data[start_time >= polls_open & end_time < polls_close , 
                           list(
                             total_precip_polling = sum(precip_mm),
                             min_precip_polling = min(precip_mm),
                             max_precip_polling = max(precip_mm),
                             heavy_rain_polling  = mean(heavy_rain), 
                             medium_rain_polling  = mean(medium_rain), 
                             light_rain_polling  = mean(light_rain),
                             any_rain_polling = mean(precip_mm > 0)
                           ), by = id]

rainfall_prepolling = constituency_rainfall_data[start_time < polls_open , 
                                              list(
                                                total_precip_prepolling = sum(precip_mm),
                                                min_precip_prepolling = min(precip_mm),
                                                max_precip_prepolling = max(precip_mm),
                                                heavy_rain_prepolling = mean(heavy_rain), 
                                                medium_rain_prepolling = mean(medium_rain), 
                                                light_rain_prepolling = mean(light_rain),
                                                any_rain_prepolling = mean(precip_mm > 0)
                                              ), by = id]

rainfall_rushhour = constituency_rainfall_data[start_time %in% rush_hour, 
                                               list(
                                                 total_precip_rushhour = sum(precip_mm),
                                                 min_precip_rushhour = min(precip_mm),
                                                 max_precip_rushhour = max(precip_mm),
                                                 heavy_rain_rushhour = mean(heavy_rain), 
                                                 medium_rain_rushhour = mean(medium_rain), 
                                                 light_rain_rushhour = mean(light_rain),
                                                 any_rain_rushhour = mean(precip_mm > 0)
                                               ), by = id]


#Combine all precipitation measures
setkey(rainfall_rushhour, id)
setkey(rainfall_prepolling, id)
setkey(rainfall_polling, id)

constituency_rainfall_agg = rainfall_polling[rainfall_rushhour][rainfall_prepolling]

#Create data for analysis
constituency_vote = as.data.table(election_boundaries_proj@data)
constituency_vote[, area := unlist(lapply(election_boundaries_proj@polygons, function(x) x@area))]
constituency_vote[, pop_density := Electrt / area]
setkey(constituency_rainfall_agg, id)
setkey(constituency_vote, id)

#Combine constituency rainfall and referendum results
use = constituency_rainfall_agg[constituency_vote]

write.csv(use, 'analysisData.csv', row.names = F)

