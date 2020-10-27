library(tidyverse)
library(janitor)
library(ff)
library(ffbase)

#location data (note that well type is not the same as on the GAMA map interface - 10/28/2020)
loc_link <- 'https://gamagroundwater.waterboards.ca.gov/gama/data_download/gama_location_construction_gis.zip'
temp <- tempfile()
download.file(loc_link, temp)
loc <- readr::read_tsv(unz(temp, 'gama_location_construction_gis.txt'), guess_max = 100000)
loc <- clean_names(loc)

#usgs data ####
usgs_link <- 'https://geotracker.waterboards.ca.gov/gama/data_download/gama_usgs_statewide.zip'
temp <- tempfile()
download.file(usgs_link, temp)
USGS <- readr::read_tsv(unz(temp, 'gama_usgs_statewide.txt'), guess_max = 1000)
unlink(temp)
USGS <- clean_names(USGS)

#usgs nwis data ####
usgsnwis_link <- 'https://gamagroundwater.waterboards.ca.gov/gama/data_download/gama_usgsnwis_statewide.zip'
temp <- tempfile()
download.file(usgsnwis_link, temp)
NWIS <- readr::read_tsv(unz(temp, 'gama_usgsnew_statewide.txt'), guess_max = 1000)
unlink(temp)
NWIS <- clean_names(NWIS)

#gama data ####
gama_link <- 'https://geotracker.waterboards.ca.gov/gama/data_download/gama_gama_statewide.zip'
temp <- tempfile()
download.file(gama_link, temp)
GAMA <- readr::read_tsv(unz(temp, 'gama_gama_statewide.txt'), guess_max = 1000)
unlink(temp)
GAMA <- clean_names(GAMA)

#dwr data ####
dwr_link <- 'https://gamagroundwater.waterboards.ca.gov/gama/data_download/gama_dwr_statewide.zip'
temp <- tempfile()
download.file(dwr_link, temp)
DWR <- readr::read_tsv(unz(temp, 'gama_dwr_statewide.txt'), guess_max = 1000)
unlink(temp)
DWR <- clean_names(DWR)

#ddw/dhs data ####
ddw_link <- 'https://geotracker.waterboards.ca.gov/gama/data_download/gama_ddw_statewide.zip'
temp <- tempfile()
download.file(ddw_link, temp)
DDW <- readr::read_tsv(unz(temp, 'gama_ddw_statewide.txt'), guess_max = 1000)
unlink(temp)
DDW <- clean_names(DDW)

#edf data (geotracker) - this file is very large, see multiple ways of downloading ####
#edf data 1 (all data) ####
edf_link <- 'https://gamagroundwater.waterboards.ca.gov/gama/data_download/gama_edf_statewide.zip'
temp <- tempfile()
download.file(edf_link, temp)

headset <- read.table(file = unz(temp, 'gama_edf_statewide.txt'), header = T, sep = "\t",
                      nrows = 20, skip = 0, skipNul = T, comment.char = "", quote = "")
headset[1:5,]
#get class types for each column
headclasses <- sapply(headset, class)

#adjust column types as needed
headclasses["WELL.DEPTH..FT."] <- "numeric"
headclasses["TOP.OF.SCREEN..FT."] <- "numeric"
headclasses["SCREEN.LENGTH..FT."] <- "numeric"
headclasses

#create ff object
edf_ff <- read.table.ffdf(file = unz(temp, 'gama_edf_statewide.txt'), fill = T, sep = "\t",
                          header = T, skipNul = T, first.rows = 25000, comment.char = "", 
                          encoding = "UTF-8", quote = "", check.names = T, colClasses = headclasses)
edf_ff[1,]

#select chemicals/wells/dates of interest
edf_ff_subset <- subset(edf_ff, CHEMICAL == "BRO3")
#convert subset to regular dataframe to combine with others, etc.
edf_df_subset <- as.daata.frame(edf_ff_subset)

#edf data 2 (subset of data) ####
#this works best if you already know what you want, and is slightly faster than way #1 above
#select location data to determine unique counties with GT data (all)
cl <- loc %>% select(county) %>% distinct()
cl$county <- tolower(cl$county)                       #make lower case and remove NAs
cl <- cl %>% filter(!is.na(county), !county == "no county found")
cl <- cl[order(cl$county),]

#create empty dataframe to host downloaded GT/EDF data from gama site
edf <- data.frame()
col_sty <- cols(
  `WELL ID` = col_character(),
  RESULTS = col_double(),
  CHEMICAL = col_character(),
  DATE = col_character(),
  UNITS = col_character(),
  QUALIFER = col_character(),
  RL = col_character(),
  LATITUDE = col_double(),
  LONGITUDE = col_double(),
  `WELL TYPE` = col_character(),
  `WELL DEPTH (FT)` = col_character(),
  `TOP OF SCREEN (FT)` = col_character(),
  `SCREEN LENGTH (FT)` = col_character(),
  SOURCE = col_character(),
  `SOURCE NAME` = col_character(),
  `OTHER NAMES` = col_character()
)
for (x in 1:nrow(cl)) {
  baselink <- "https://gamagroundwater.waterboards.ca.gov/gama/data_download/gama_edf_"
  county <- gsub(" ", "", cl[x,1], fixed = TRUE)
  countyedflink <- paste0(baselink, county, ".zip")
  countyfilename <- paste0('gama_edf_', county, ".txt")
  temp <- tempfile()
  download.file(countyedflink, temp)
  newdat <- readr::read_tsv(unz(temp, countyfilename), quote = '', col_types = col_sty, skip_empty_rows = T)
  #download.file(url = countyedflink, destfile = temp, method = 'curl')
  #newdat <- readr::read_tsv(file = unzip(zipfile = temp), quote = '', col_types = col_sty, skip_empty_rows = T)
  unlink(temp)
  names(newdat) <- clean_names(newdat)
  newdat <- newdat %>% dplyr::filter(chemical == "NO3N") #filter by well id, chemical, date, etc.
  edf <- rbind(edf, newdat)
  print(cl[x,1])     #to keep track of progress!
  rm(newdat)
}
head(edf)
