#Time interval function
#Create half hour intervals
half_hour_intervals = function(date) {
  dt = strptime( paste(date, "000000"), format = "%Y-%m-%d %H%M%S", tz = "UTC")
  seconds = seq(0, 24*60*60, 30*60)
  cutpoints = dt + seconds
  intervals = paste0(format(cutpoints[-length(cutpoints)], "%Y%m%d-S%H%M%S"), format(cutpoints[-1] - 1, "-E%H%M%S") )
  return(intervals)
}

#Date range
start_date = as.Date("2016-06-16")
end_date = as.Date("2016-06-29")
date_range = seq.Date(start_date, end_date, by = 'day')

#Make files_table
files_table = data.table(date = date_range)
files_table[, intervals := list(list(half_hour_intervals(date))), by = 1:nrow(files_table)]
files_table = files_table[, list(interval = unlist(intervals)), by = date]
files_table[, download := 0]
files_table[, month := format(date, "%Y%m")]

raster_files = list.files('./getImages/raw')
intervals = files_table$interval

#Indicate whether half hour interval already downloaded.
for (i in intervals){
  if (any(str_detect(raster_files, fixed(i)))) {
    files_table[interval %in% i, download := 1]
  }
}

#Download files
months = unique(files_table[download %in% 0, month])

for (month in months) {
  pw = "'mdweaver@uchicago.edu:mdweaver@uchicago.edu'"
  ftp = paste0('ftp://jsimpson.pps.eosdis.nasa.gov/data/imerg/late/',month,'/')
  curl.call = paste("curl -u", pw, ftp)
  ftp_ls = system(curl.call, intern = T)
  imerg = "(3B[-]HHR[-]L[.]MS[.]MRG[.]3IMERG[.][0-9]{8}[-]S[0-9]{6}[-]E[0-9]{6}[.][0-9]{4}[.]V[0-9]{2}[A-Z][.]RT[-]H5)"
  files = as.vector(str_extract_all(ftp_ls, imerg, simplify = T))

  to.download = files_table[download %in% 0, interval]
  file.download = vector(mode = 'numeric', length = length(to.download))
  for (i in 1:length(to.download)){
    idx = which(str_detect(files, fixed(to.download[i])))
    file.download[i] = files[idx]
  }
  
  con = getCurlHandle(ftp.use.epsv = FALSE, userpwd=pw)
  
  for (file in file.download){
    print (file)
    file.url = paste0(ftp, file)
    file.out = paste0("./getImages/raw/", file)
    writeBin(getBinaryURL(file.url, curl = con, dirlistonly = FALSE), file.out)
  }
}
