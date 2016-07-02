#List of raw files
raw_files = list.files('./getImages/raw')
processed_files = list.files('./getImages/processed')

#Files to process
r_dt = "([0-9]{8}[-]S[0-9]{6}[-]E[0-9]{6})"
idx = which(as.vector(str_extract_all(raw_files, r_dt, simplify = T)) %in% as.vector(str_extract_all(processed_files, r_dt, simplify = T)))
if (length(idx) == 0) {use_files = raw_files} else {use_files = raw_files[-idx]}

#Process
for (file in use_files){
  print(file)
  image_crs = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
  precipImage = (h5read(paste0('./getImages/raw/', file), 'Grid/precipitationCal'))
  H5close()
  precipImage[precipImage < 0] = NA
  rasExt <- extent(-180,180,-90,90)
  res = 0.1
  precipImageR <- flip(raster(precipImage, crs = image_crs), 2)
  extent(precipImageR) <- rasExt
  #Write to file
  r_dt = "([0-9]{8}[-]S[0-9]{6}[-]E[0-9]{6})"
  datetime = as.vector(str_extract_all(file, r_dt, simplify = T))
  filepath = paste0('./getImages/processed/', datetime, '.tif')
  
  writeRaster(precipImageR,
              file=filepath,
              format="GTiff",
              overwrite=TRUE)
}
