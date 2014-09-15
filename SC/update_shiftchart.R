#! /usr/bin/Rscript --vanilla

## above is a comment for R, but required for cron job

## process command line args
args <- commandArgs(TRUE)

## force_download
if( length(args) > 0 && "force_download" %in% args) {
    force_download = TRUE
    message("force_download ENABLED via command line")
} else {
    force_download <- FALSE
}

## reprocess - don't download again but do redo the parsing of the html file
if( length(args) > 0 && "reprocess" %in% args) {
    reprocess = TRUE
    message("reprocess ENABLED via command line.")
} else {
    reprocess <- FALSE
}

## specific game was requested
if(length(args) > 0 && grepl("[[:digit:]]", args)) {
    specific_game <- args[grepl("[[:digit:]]", args)]
    force_download <- TRUE
    ## this should force reprocess below
    message("specific_game ", specific_game, " will be downloaded, and force_download has been set.")
} else {
    specific_game <- NA
}

## all playoff games requested
if( length(args) > 0 && "all_playoffs" %in% args) {
    specific_game <- paste0(0, sort(apply(expand.grid(1:4, 1:8, 1:7), 1,
                                          paste, collapse = ""))[-c(85:112, 127:168, 176:224)])
    force_download <- TRUE
    message("All playoff games will be downloaded, and force_download has been set.")
}

## If between midnight and noon local time, get previous day's games
if(Sys.time() >= as.POSIXct(paste(Sys.Date(), "00:00:00")) &&
   Sys.time() <= as.POSIXct(paste(Sys.Date(), "11:00:00"))) {
    back <- -1
} else {
    back <- 0
}

## shortcuts on command line
if(length(args) > 0 && "yesterday" %in% args) {
    back <- -1
    force_download <- TRUE
    ## this should force reprocess below
    message("All of YESTERDAY'S games will be downloaded, and force_download has been set.")
} 

## directory on server, only use if SC_ROOT environment variable is unset
Sys.setenv( SC_ROOT= "/Users/saipuck/projects/shiftchart.com" )


root_dir <- Sys.getenv("SC_ROOT")
if(root_dir == "") {
    message("SC_ROOT unset, assuming we're on the live server!")
    root_dir <- "/home/erik/sites/shiftchart.com" #on the server
}


# root_dir <- "/Users/saipuck/projects/shiftchart.com"

setwd(paste0(root_dir, "/data/parse/R"))
raw_html_dir <- paste0(root_dir, "/data/scrape/raw_html")
raw_json_dir <- paste0(root_dir, "/data/scrape/raw_json")

## until we compile a package, have to source individual files I
## believe the ordering matters here for reasons involving the JSON
## package that this version of httr uses
source("couch.R")
source("nhl_scrape.R")
source("parse.R")

## nhl_scrape.R / get_todays_data()
current_game_list <- get_todays_data(back, force_download = force_download,  ## this is data.frame(games = games, downloaded  = ret) 
                                     games = specific_game)

## only proceed on those games where we need to update (downloaded = TRUE)
## But, what if we have to re-gen some of the games because we're
## adding a field to the database, for example?

if(nrow(current_game_list) == 0) {
    stop("No games are happening today. Goodbye.")
}

proceed <- if(reprocess) {
    current_game_list
}  else {
    subset(current_game_list, downloaded == TRUE)
}

if(nrow(proceed) == 0) {
    stop("All games for the evening are over. Goodnight.")
}

## This will have to be fixed for the playoffs

## parse.R / read_nhl_game()
current_game_html <-
    lapply(proceed$games,
           function(x)
           read_nhl_game("20132014", "playoffs", x, raw_html_dir))

## This will get rid of games where we downloaded a 401 page because
## it probably hasn't started yet. Not elegant.
current_game_html <- current_game_html[!sapply(current_game_html,is.null)]

## generate json files for the current list
in_database <-
    !is.na(sapply(current_game_html, function(x)
                  get_current_doc_rev(
                      paste0(x$game_info$season, "-",
                             x$game_info$session, "-",
                             x$game_info$game_id))))

mapply(gen_game_json, game = current_game_html,
       include_couch_rev = in_database,
       MoreArgs = list(dir = raw_json_dir, smartfile = TRUE))

## construct a regex

game_regex <- paste0("(", paste0(proceed$games, "|", collapse = ""))
game_regex <- paste0(substr(game_regex, 1, nchar(game_regex) - 1), ")")

current_json_files <- list.files(pattern = paste0("20132014-playoffs-",  ### HARD CODED
                                     game_regex),
                                     raw_json_dir,
                                full.names = TRUE)

## update couch with current games
lapply(current_json_files, file_to_couch)

## END

