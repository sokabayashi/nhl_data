## roster from NHL live

library( rvest )
library( jsonlite )
library( plyr )

# season  = "20132014"
# session = "regular"
# gameID4 = "1157"

# game_roster -------------------------------------------------------------

game_roster <- function( season="20132014", session="regular", gameID4="1157" ) {
  gc_id <- paste0( substr( season, 1,4), "0", ifelse(session=="regular",2,3), gameID4 )
  # url <- "http://www.nhl.com/gamecenter/en/icetracker?id=2013021157&navid=sb:icetracker"
  url <- paste0("http://www.nhl.com/gamecenter/en/icetracker?id=", gc_id, "&navid=sb:icetracker")
  
  raw_roster_json <- url %>% html() %>% 
                  html_node( "#pageBody") %>% 
                  html_node(xpath = "script" ) %>% 
                  html_text() %>% 
                  gsub( "\n\tvar gcPlayerMap = ", "", . ) %>% gsub( ";\n", "", . )
  
  raw_roster_list <- fromJSON( raw_roster_json )
  
  # roster_from_list --------------------------------------------------------
  roster_from_list <- function( homeaway, raw_list ) {
    numbers <- names( raw_list[[ homeaway ]] ) %>% gsub( "sw", "", .) %>% as.integer()
    roster  <- ldply( raw_list[[ homeaway ]], .fun = function(x) c( id=x$id, 
                                                                    last_name=toupper(x$last), 
                                                                    first_name=toupper(x$first), 
                                                                    position=x$pos) )
    roster$number <- numbers
    roster$ha <- ifelse( homeaway=="home", "H", "A" )    
    return( roster )
  }
  
  ha <- c( "home", "away")
  rosters <- lapply( ha, roster_from_list, raw_roster_list ) 
  roster <- rbind( rosters[[1]], rosters[[2]] )
  roster[ ".id" ] <- NULL

  return( roster )
}

season  = "20132014"
session = "regular"
gameID4 = "1157"

game_roster( season=season, session=session, gameID4=gameID4 )