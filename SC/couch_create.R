################################################################################
## re-create whole couch db from scratch

Sys.setenv(SC_ROOT="~/Dropbox/sites/shiftchart.com")
library(plyr)

source("couch.R")
source("nhl_scrape.R")
source("parse.R")

raw_html_dir <- "/home/erik/Dropbox/sites/shiftchart.com/data/scrape/raw_html"
raw_json_dir <- "/home/erik/Dropbox/sites/shiftchart.com/data/scrape/raw_json/"

## comment out for fast eval when testing 
season2012 <- read_nhl_season("20122013", raw_html_dir)
system.time(gen_season_json(season2012, raw_json_dir))

season2013 <- read_nhl_season("20132014", raw_html_dir)

system.time(gen_season_json(season2013, raw_json_dir))

json_files <- list.files(pattern = "20122013-",
                         raw_json_dir, full.names = TRUE)
lapply(json_files, file_to_couch)


json_files <- list.files(pattern = "20132014-",
                         raw_json_dir, full.names = TRUE)

lapply(json_files, file_to_couch)

##
################################################################################

