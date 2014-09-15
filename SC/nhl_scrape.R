## scrape data from NHL.com

library(httr)
library(nhlscrapr)

root_dir <- Sys.getenv("SC_ROOT")
if(root_dir == "") {
    message("SC_ROOT unset, assuming we're on the live server!")
    root_dir <- "/home/erik/sites/shiftchart.com" #on the server
}


# get_nhl_data ------------------------------------------------------------
# Get .HTM file from NHL.com URL and save to file.
# No checks for if file already exists.  this should be done in calling function write_nhl_data().

get_nhl_data <- function(season, session, type, game, file = "") {
    type_id <-
        if(type == "roster") "RO"
        else if(type == "toi_home") "TH"
        else if(type == "toi_away") "TV"
        else if(type == "pbp") "PL"
        else if(type == "game_summary") "GS"
        else if(type == "event_summary") "ES"
        else if(type == "faceoff") "FC"
        else if(type == "shot") "SS"
        else stop("Please request a valid Official Game Report type.")

    session_id <- if(session == "regular") "02"
    else if(session == "playoffs") "03"
    else stop("Argument session should be either 'regular' or 'playoffs'.")
       
    url <- paste0("http://www.nhl.com/scores/htmlreports/",
                  season, "/", type_id, session_id, game, ".HTM")
  
    cat(content(GET(url), "text"), file = file)
}

# write_nhl_data ----------------------------------------------------------

write_nhl_data <- function(season, session, type, games, sleep = NULL,
                           force_download = FALSE) {

    get_if_not_final_or_present <- function(game) {
        ## Determine right here if a game is still going on, and only run
        ## the following code to get game file *IF* so. Relies on couch functions!
    
        doc <- paste0(season, "-", session, "-", game)
        doc_content <- fromJSON(content(GET(paste0(default_couch_server, doc)))) ## couch server

        ## if the id is not even in the database, this condition is
        ## satisfied and we return FALSE. Otherwise check if it's "Final".
        ## The problem is, even "Final" games might be a few shifts short,
        ## so we'll need to pull a couple more times.

        if(!identical(names(doc_content), c("error", "reason"))) {
            final <-
                if("game_state" %in% names(doc_content$game)) {

                    doc_content$game$game_state == "Final"

                    ## but also check game end time, and if within 60
                    ## minutes, continue assuming it's not final,
                    ## since there may be updates to the file.
                    
                } else {
                    ## game state is not even there, so assume FALSE
                    FALSE
                }
        } else final <- FALSE
    
        ## if(length(final) != 0 && final) {
        ##     message(game, "is Final. No download or write.")
        ##     return(FALSE)
        ## }
        
        file_name <- paste0(root_dir, "/data/scrape/raw_html/",
                            season, "-", session,
                            "-", game, "-", type, ".HTM" )

        if( !file.exists(file_name) || !final || force_download) {
            get_nhl_data(season, session, type, game, file = file_name)

            if(!is.null(sleep))
                Sys.sleep(sleep)

            return(TRUE)

        } else {
            message(game,
                    " is already present, final, and force_download was not indicated. No download or write occurred.")
            return(FALSE)
        }
            
    }
    sapply(games, get_if_not_final_or_present)  ## games can be a vector
}


# get_daily_data ----------------------------------------------------------

## new games
get_daily_data <- function(game_date, games, sleep = NULL, ...) {
  df <- do.call(rbind, lapply(content(
    GET(url = "http://www.nicetimeonice.com/api/seasons/20132014/games")),
                              as.data.frame,
                              stringsAsFactors = FALSE))

  if(missing(games) || is.na(games)) {
      game_ids <- subset(df, date == game_date)$gameID  # returns format like 2013021157
      games <- substr(game_ids, 7, 10)  ## 4 digit code only
  }

  # nhl_scrape.R / write_nhl_data()
  write_nhl_data(season = "20132014", session = "playoffs",
                 type = "game_summary", games = games,
                 sleep = sleep, ...)
  write_nhl_data(season = "20132014", session = "playoffs",
                 type = "roster", games = games,
                 sleep = sleep, ...)
  write_nhl_data(season = "20132014", session = "playoffs",
                 type = "toi_home", games = games,
                 sleep = sleep, ...)
  ret <- write_nhl_data(season = "20132014", session = "playoffs",
                 type = "toi_away", games = games,
                 sleep = sleep, ...)
  ## could add pbp here!!

  message(paste(c("wrote", games), collapse = " "))
  
  ## return:
  data.frame(games = games, downloaded  = ret)
}


# get_todays_data ---------------------------------------------------------

get_todays_data <- function(back = 0, ...) {
  ## use back = -1 for yesterday, for example
  date <- format(Sys.Date() + back, "%a %b %d, %Y")
  get_daily_data(date, ...)
}
