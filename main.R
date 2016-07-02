###################################################
#Replication files for:############################
#"Polling-Day Rainfall Decreased 'Remain' Turnout"#
#Gareth Nellis and Michael Weaver##################
###################################################
setwd("/path/to/brexitReplication/")

###############
#Load packages#
###############
require(data.table)
require(stringr)
require(rgdal)
require(sp)
require(lfe)
require(httr)
require(RCurl)
require(stargazer)
if ('rhdf5' %in% rownames(installed.packages())) {require(rhdf5)} else {
  source("https://bioconductor.org/biocLite.R")
  biocLite("rhdf5")
  require(rhdf5)
}

#Download Referendum results and shapefile#
###########################################
if (!dir.exists('./referendumData')){
  dir.create('./referendumData')
}

GET('https://t.co/GYqX2PCqJ0', write_disk("./referendumData/Geotheory-UK-2016-EU-Referendum.zip", overwrite=TRUE))
unzip("./referendumData/Geotheory-UK-2016-EU-Referendum.zip", exdir = './referendumData', overwrite = T)

#Download precipitation images#
###############################
if (!dir.exists('./getImages')){
  dir.create('./getImages')
  dir.create('./getImages/raw')
  dir.create('./getImages/processed')
}

#Note, this will take some time and take up about 2GB
source('./scripts/downloadImages.R')

#Georeference precipitation images#
###################################

#Note this will take some time.
source('./scripts/processImages.R')

#Make data for analysis#
########################
source('./scripts/makeData.R')

############
############
##Analysis##
############
############

use = fread('./analysisData.csv')
use[, Electrt10 := Electrt/ 10000]

#Turnout analysis
#################
turnout_estimates = list(
  naive_iid = (felm(Pct_Trn ~ any_rain_polling | 0 | 0 | 0, data = use)),
  naive_cl = (felm(Pct_Trn ~ any_rain_polling | 0 | 0 | Region, data = use)),
  fe_iid = (felm(Pct_Trn ~ any_rain_polling | Region | 0 | 0, data = use)),
  fe_cl = (felm(Pct_Trn ~ any_rain_polling | Region | 0 | Region, data = use)),
  fe_iid_cov = (felm(Pct_Trn ~ any_rain_polling + Electrt10 + area | Region | 0 | 0, data = use)),
  fe_cl_cov = (felm(Pct_Trn ~ any_rain_polling + Electrt10 + area | Region | 0 | Region, data = use))
)


#Remain analysis
################
remain_estimates = list(
  naive_iid = (felm(Pct_Rmn ~ any_rain_polling | 0 | 0 | 0, data = use)),
  naive_cl = (felm(Pct_Rmn ~ any_rain_polling | 0 | 0 | Region, data = use)),
  fe_iid = (felm(Pct_Rmn ~ any_rain_polling | Region | 0 | 0, data = use)),
  fe_cl = (felm(Pct_Rmn ~ any_rain_polling | Region | 0 | Region, data = use)),
  fe_iid_cov = (felm(Pct_Rmn ~ any_rain_polling + Electrt10 + area | Region | 0 | 0, data = use)),
  fe_cl_cov = (felm(Pct_Rmn ~ any_rain_polling + Electrt10 + area | Region | 0 | Region, data = use))
)


#Make tables
############
if (!dir.exists('./tables')){
  dir.create('./tables')
}


stargazer(turnout_estimates, 
          style = 'apsr', 
          out = './tables/turnout_table.tex', 
          dep.var.labels = "Turnout", 
          covariate.labels = c('Rainy polling hours (\\%)', 'Electorate (10000s)', 'Area', 'Constant'),
          add.lines = list(c('Region FEs', 'no', 'no', 'yes', 'yes', 'yes', 'yes'), c('Region-Clustered SEs', 'no', 'yes', 'no', 'yes', 'no', 'yes')),
          label = 'table:turnout', 
          omit.stat = 'ser', 
          star.cutoffs = c(0.05, 0.01, 0.001),
          font.size = 'small',
          float.env = 'sidewaystable')

stargazer(remain_estimates, 
          style = 'apsr', 
          out = './tables/remain_table.tex', 
          dep.var.labels = "Remain Voteshare", 
          covariate.labels = c('Rainy polling hours (\\%)', 'Electorate (10000s)', 'Area', 'Constant'),
          add.lines = list(c('Region FEs', 'no', 'no', 'yes', 'yes', 'yes', 'yes'), c('Region-Clustered SEs', 'no', 'yes', 'no', 'yes', 'no', 'yes')),
          label = 'table:remain', 
          omit.stat = 'ser', 
          star.cutoffs = c(0.05, 0.01, 0.001),
          font.size = 'small', 
          float.env = 'sidewaystable')