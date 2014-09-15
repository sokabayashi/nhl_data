
source("sharedTOI.R")

library(XML)    # R DOM object out of HTM file
library(zoo)    # time series   na.locf()  last observation carried forward
library(RJSONIO)  # generates RJSONIO
library(data.table) 
library(stringr) # str_locate
library(httr)    # used here?  GET()?
library(nhlscrapr)

root_dir <- Sys.getenv("SC_ROOT")
if(root_dir == "") {
    message("SC_ROOT unset, assuming we're on the live server!")
    root_dir <- "/home/erik/sites/shiftchart.com" #on the server
}

## but watch out for the 3-letter abbreviations
## I don't think the following work as in the goals:  L.A, N.J, T.B, S.J
nhl_teams <- read.csv(paste0(root_dir, "/data/teamInfo.csv"),
                      stringsAsFactors = FALSE)

## add in 3-letter code for goals
nhl_teams$teamName3CharGoals <- nhl_teams$teamName3Char
nhl_teams$teamName3CharGoals[4] <- "L.A"
nhl_teams$teamName3CharGoals[25] <- "N.J"
nhl_teams$teamName3CharGoals[21] <- "T.B"
nhl_teams$teamName3CharGoals[6] <- "S.J"

## capitalize first letter of ....?
simple_cap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
      sep="", collapse=" ")
}

## http://stackoverflow.com/questions/5186972/how-to-convert-time-mmss-to-decimal-form-in-r
time_convert <- function(x, y) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  x + (y/60)
}

time_convert_back <- function(x) {
  int <- floor(x)
  frac <- x %% 1
  paste0(int, ":", sprintf("%02d", round(frac * 60, 0)))
}

get_game_date <- function(x) {
  x <- as.character(x$GameInfo[3,1])
  as.Date(substr(x, str_locate(x, ",")[,"start"] + 2, nchar(x)),
          format = "%B %d, %Y")
}

get_team <- function(x) {
  x <- as.character(x[3,1])
  team <- substr(x, 1, str_locate(x, "Game")[,"start"] - 1)

  if(substr(team, nchar(team) - 5, nchar(team)) == "Match/")
    capped_name <- substr(team, 1, nchar(team) - 6)
  else
    capped_name <- team

  simple_cap(tolower(capped_name))
}

parse_toi_data <- function(file, style = "shiftchart") {

  if(style == "knoll") {
    game_id <- substr(strsplit(file, "/")[[1]][12], 5, 8)
    season <- substr(strsplit(file, "/")[[1]][9], 1, 4)
    session <- "implement later" 
  } else {
    file_name <- tail(unlist(strsplit(file, "/")), n = 1)

    season <- unlist(strsplit(file_name, "-"))[1]
    game_id <- unlist(strsplit(file_name, "-"))[3]
    session <- unlist(strsplit(file_name, "-"))[2]
  }
  
  doc <- htmlTreeParse(file, useInternalNodes = TRUE)  # from XML package

  ## extract from doc.  Erik found this via stackoverflow
  players <- xpathSApply(
    doc, "//td[contains(concat(' ',normalize-space(@class),' '),' playerHeading ')]",
    xmlValue)

  team <- xpathSApply(
    doc, "//td[contains(concat(' ',normalize-space(@class),' '),' teamHeading ')]",
    xmlValue)

  row_data <- xpathSApply(
    doc, "//tr[contains(concat(' ',normalize-space(@class),' '),' oddColor ')] |
        //tr[contains(concat(' ',normalize-space(@class),' '),' evenColor ')]",
    xmlValue)

  ## this is our first indication that we wrote a 401 page to
  ## disk. This usually means a game hasn't started yet. There are
  ## better ways to detect this, but for now, can return a special
  ## value.
  if(length(row_data) == 0) {
      return("401")
  }
  
  all_rows <- strsplit(row_data, "\r\n")
  toi_rows <- all_rows[sapply(all_rows, length) == 6]  # drop icetime by period,strength.  COuld bring this back!!!

  toi <- as.data.frame(do.call(rbind, toi_rows))

  names(toi) <- c("shift_number", "period", "shift_time_on", "shift_time_off",
                  "shift_duration", "event")

  ## labelled as 'OT' in regular season games 
  toi$period <- ifelse(toi$period == "OT", "4", as.character(toi$period))

  toi$shift_number <- as.numeric(as.character(toi$shift_number))

  toi[toi$shift_number == 1, "player"] <- players
  toi$player <- na.locf(toi$player)

  number_loc <- str_locate(toi$player, " ")[, "start"]
  last_loc <- str_locate(toi$player, ",")[, "start"]

  toi$number <- substr(toi$player, 1, number_loc - 1)
  toi$last_name <- substr(toi$player, number_loc + 1, last_loc - 1) 
  toi$first_name <- substr(toi$player, last_loc + 2, nchar(toi$player)) 

  toi$event <- factor(ifelse(toi$event == "\u00A0", "", as.character(toi$event)))

  toi$start <- sapply(strsplit(as.character(toi$shift_time_on), "/"), "[[", 1)
  toi$start_min <- sapply(strsplit(toi$start, ":"), "[[", 1)
  toi$start_sec <- sapply(strsplit(toi$start, ":"), "[[", 2)
  toi$start <- time_convert(toi$start_min, toi$start_sec)

  toi$end <- sapply(strsplit(as.character(toi$shift_time_off), "/"), "[[", 1)
  toi$end_min <- sapply(strsplit(toi$end, ":"), "[[", 1)
  toi$end_sec <- sapply(strsplit(toi$end, ":"), "[[", 2)
  toi$end <- time_convert(toi$end_min, toi$end_sec)

  toi$duration <- toi$end - toi$start
  toi$season <- season
  toi$game_id <- game_id
  toi$session <- session

  toi$team <- simple_cap(tolower(team))

  toi$team <- merge(toi, nhl_teams[c("teamName3Char", "teamNameFull")],
                    by.x = "team", by.y = "teamNameFull")$teamName3Char

  toi[order(toi$player, toi$shift_number),]
}

gen_toi_json_struct <- function(agg_df) {

  shifts <- data.frame(start = agg_df$start, duration = agg_df$duration,
                       event = agg_df$event,
                       period = as.numeric(as.character(agg_df$period)))
  shifts <- lapply(split(shifts, as.numeric(rownames(shifts))), as.vector)
  names(shifts) <- NULL

  list(number = agg_df$number[1],
       last_name = agg_df$last_name[1],
       first_name = agg_df$first_name[1],
       position = agg_df$position[1],
       team = agg_df$team[1],
       shifts = shifts)
}

list_game_ids <- function(season, session, dir) {
  files <- list.files(dir, pattern = paste0(season, "-", session, ".*HTM"), 
                      recursive = TRUE, full.names = FALSE)

  ## damn, it's not a unique part of the substring
  ## too tired to figure out the regexp, so let's do this
  games <- if(session == "playoffs")
    substr(files, 19, 22)
  else substr(files, 18, 21)

  unique(games)

}

parse_roster_data <- function(file) {

  file_name <- tail(unlist(strsplit(file, "/")), n = 1)

  season <- unlist(strsplit(file_name, "-"))[1]
  game_id <- unlist(strsplit(file_name, "-"))[3]
  session <- unlist(strsplit(file_name, "-"))[2]

  tabs <- readHTMLTable(file)  ## from XML package

  away_players <- tabs[[11]]
  away_players$team_ha <- "A"
  
  home_players <- tabs[[12]]
  home_players$team_ha <- "H"
  
  all_players <- rbind(home_players, away_players)
  names(all_players) <- c("number", "position", "name", "team_ha")

  # C, A roles for players
  all_players$role <-
    ifelse(grepl("(C)" , fixed = TRUE, all_players$name),
           "C", ifelse(grepl("(A)", fixed = TRUE, all_players$name),
                       "A", "R"))

  all_players$number <- as.character(all_players$number)
  all_players$position <- as.character(all_players$position)

  all_players$name <- gsub("  \\(C\\)", "", all_players$name)  # gsub in base R
  all_players$name <- gsub("  \\(A\\)", "", all_players$name)

  first_loc <- str_locate(all_players$name, " ")[,"start"]
  all_players$first_name <- substr(all_players$name, 1, first_loc - 1)
  all_players$last_name <- substr(all_players$name, first_loc + 1,
                                  nchar(all_players$name))
  
  all_players$game_id <- game_id
  all_players$season <- season
  all_players$session <- session
  
  all_players
}

gen_season_json <- function(season, dir, include_couch_rev = TRUE) {
  lapply(season, gen_game_json, smartfile = TRUE, dir = dir,
         include_couch_rev = include_couch_rev)
}

parse_point <- function(info) {
  info <- as.character(info)
  number <- str_locate(info, " ")[,"start"]
  name   <- str_locate(info, "\\.")[,"start"]
  count  <- str_locate(info, "\\(")[,"start"]

  ## because we subtract 1 below
  count <- ifelse(is.na(count), nchar(info) + 1, count)

  list(substr(info, 1, number - 1),
       substr(info, number + 1, name - 1),
       substr(info, name + 1, count - 1))
}

parse_game_summary_data <- function(file) {

  ## game summary information
  file_name <- tail(unlist(strsplit(file, "/")), n = 1)
  season <- unlist(strsplit(file_name, "-"))[1]
  game_id <- unlist(strsplit(file_name, "-"))[3]
  session <- unlist(strsplit(file_name, "-"))[2]
  game_info <- readHTMLTable(file)

  ## Determine if game is currently being played, or completed.
  game_state <- game_info$GameInfo[7,]
  game_date <- as.character(get_game_date(game_info))

  ## teams information
  home_team <- get_team(game_info$Home)
  away_team <- get_team(game_info$Visitor)

  ## for goals info
  home_team_short <- nhl_teams[nhl_teams$teamNameFull == home_team,
                               "teamName3Char"]

  away_team_short <- nhl_teams[nhl_teams$teamNameFull == away_team,
                               "teamName3Char"]

  ## due to silly re-naming for the 2-word teams (e.g., L.A)
  home_team_short_goals <- nhl_teams[nhl_teams$teamNameFull == home_team,
                               "teamName3CharGoals"]

  away_team_short_goals <- nhl_teams[nhl_teams$teamNameFull == away_team,
                               "teamName3CharGoals"]
  
  home_team_location <- nhl_teams[nhl_teams$teamNameFull == home_team,
                               "teamLocation"]

  away_team_location <- nhl_teams[nhl_teams$teamNameFull == away_team,
                               "teamLocation"]

  home_team_common <- nhl_teams[nhl_teams$teamNameFull == home_team,
                               "teamNameCommon"]

  away_team_common <- nhl_teams[nhl_teams$teamNameFull == away_team,
                               "teamNameCommon"]

  home_team_dash <- tolower(gsub(" ", "-", home_team_common))
  away_team_dash <- tolower(gsub(" ", "-", away_team_common))
  sc_id <- paste(game_date, away_team_dash, home_team_dash, sep = "-")

  ## White for now
  away_team_color_fill <- "#FFFFFF"
                          
  away_team_color_border <- nhl_teams[nhl_teams$teamNameFull == away_team,
                                     "teamColor"]

  home_team_color_fill <-  nhl_teams[nhl_teams$teamNameFull == home_team,
                                     "teamColor"]
                          
  home_team_color_border <- nhl_teams[nhl_teams$teamNameFull == home_team,
                                      "teamColor2"]

  ## if missing, just use fill color if border color missing
  if(home_team_color_border == "") {
    home_team_color_border <- home_team_color_fill
  }
  
  ## goal information 
  goals <- game_info[[10]]

  if(length(goals) == 0) {
      goals <- as.data.frame(setNames(replicate(10,numeric(0), simplify = F),
                                      letters[1:10]))
  }

  ## if 9 columns, assume only goal in game is penalty shot
  if(ncol(goals) == 9) {
      goals <- data.frame(goals[1:7], V8 = "", goals[8:9])
  }

  ## blame Canada!

  names(goals) <- c("sequence", "period", "time", "strength", "team",
                    "goal_scorer", "first_assist", "second_assist",
                    "away_on_ice", "home_on_ice")

  goals$period <- ifelse(goals$period == "OT", "4",
                         ifelse(goals$period == "SO", "0",
                                as.character(goals$period)))

  ## here is a good spot to do a hardcode for the game that started 1-0 (020971)

  ## first, convert to character because we'll be assigning some levels that are not present
  goals$time <- as.character(goals$time)
  goals$strength <- as.character(goals$strength)
  
  # Make up game for Dallas Peverly canceled game
  if(nrow(goals) > 0 && goals$period[1] == "") {
      goals[1,"period"] <- 1
      goals[1,"time"] <- "00:00"
      goals[1,"strength"] <- "EV"
  }

  goals$period <- as.numeric(as.character(goals$period))

  goals$team_ha <- ifelse(goals$team == home_team_short_goals, "H", "A")
  
  goals[c("goal_player_number",
          "goal_first_init",
          "goal_last_name")] <- parse_point(goals$goal_scorer)
  goals[c("assist_1_player_number",
          "assist_1_first_init",
          "assist_1_last_name")] <- parse_point(goals$first_assist)
  goals[c("assist_2_player_number",
          "assist_2_first_init",
          "assist_2_last_name")] <- parse_point(goals$second_assist)

  ## handle shoot-outs
  goals$time <- ifelse(goals$period == 0, "0:00", as.character(goals$time))
  goals$min <- sapply(strsplit(as.character(goals$time), ":"), "[[", 1)
  goals$sec <- sapply(strsplit(as.character(goals$time), ":"), "[[", 2)
  goals$display_time <- time_convert(goals$min, goals$sec)

  ## these variables should go in a separate list, not be repeated for
  ## each goal

  game_information <- list(sc_id = sc_id,
                           season = season, session = session,
                           game_id = game_id, game_date = game_date,
                           game_state = game_state,
                           home_team = home_team, away_team = away_team,
                           home_team_short = home_team_short,
                           away_team_short = away_team_short,
                           home_team_location = home_team_location,
                           away_team_location = away_team_location,
                           home_team_common = home_team_common,
                           away_team_common = away_team_common,
                           home_team_color_fill = home_team_color_fill,
                           home_team_color_border = home_team_color_border,
                           away_team_color_fill = away_team_color_fill,
                           away_team_color_border = away_team_color_border)

  ## remove unsuccessful penalty shots
  goal_information <- subset(goals, sequence != "-")

  ## FIX: final score calculation, probably have to adapt for penalty shots 
  score <- table(factor(goal_information$team_ha, levels = c("H", "A")))

  game_information <- c(game_information, home_team_score = as.vector(score["H"]),
                        away_team_score = as.vector(score["A"]))

  doc <- htmlTreeParse(file, useInternalNodes = TRUE)  # need to do this way after all for penalty
  ## TODO
  xpathSApply(
    doc, "//td[contains(concat(' ',normalize-space(@class),' '),' PenaltySummary ')]",
    xmlValue)
  test <- xpathSApply(doc, "//*[@id = 'PenaltySummary']", xmlValue)
  blah <- unlist(strsplit(test, "\r\n"))
  non <- blah[!blah %in% c("\u00A0", "")]
  yay <- non[2:(length(non)-8)]         #what is this line up to?
  add_number <- which(yay == "Time" | yay == "TempsTime")  ## French!!!!!
  huh <- append(yay, "number", add_number[1])
  huh <- append(huh, "number", add_number[2] + 1)
  team_penalties <- grepl("^(TEAM)|[.]$", huh)
  if(any(team_penalties)) {
    team_pen_spots <- rev(which(team_penalties)) #biggest first for insert
    for(i in seq_along(team_pen_spots)) {
      huh <- append(huh, "", team_pen_spots[i] - 1)
    }
  }
  pen_matrix <- matrix(huh, ncol = 7, byrow = TRUE)
  team_rows <- which(pen_matrix[,1] == "#")
  pen_df <- as.data.frame(pen_matrix, stringsAsFactors = FALSE)
  colnames(pen_df) <- as.character(pen_df[1,])
  away_team <- away_team
  home_team <- home_team
  pen_df$team <- c(rep(away_team, times = diff(team_rows)),
                   rep(home_team, times = nrow(pen_df) -
                       team_rows[2] + 1))
  pen_df <- pen_df[-c(1,team_rows[2]), !names(pen_df) %in% "#"]
  names(pen_df) <- c("period", "time", "number", "player", "pim", "penalty", "team")

  pen_df$start_min <- sapply(strsplit(pen_df$time, ":"), "[[", 1)
  pen_df$start_sec <- sapply(strsplit(pen_df$time, ":"), "[[", 2)
  pen_df$start <- time_convert(pen_df$start_min, pen_df$start_sec)

  pen_df$team_ha <- ifelse(pen_df$team == home_team, "H", "A")

  pen_df$team <- merge(pen_df, nhl_teams[c("teamName3Char", "teamNameFull")],
                       by.x = "team", by.y = "teamNameFull")$teamName3Char

  
  penalty_information <- pen_df

  list(game_info = game_information,
       goal_info = goal_information,
       penalty_info = penalty_information)
}

gen_goal_json_struct <- function(goal_info) {
  useful <- as.data.frame(goal_info[c("goal_last_name",
                                      "goal_player_number", "display_time",
                                      "period", "team", "team_ha", 
                                      "assist_1_last_name",
                                      "assist_1_player_number",
                                      "assist_2_last_name",
                                      "assist_2_player_number")])

  ret <- lapply(split(useful, as.numeric(rownames(useful))), as.vector)
  names(ret) <- NULL
  ret
}


gen_penalty_json_struct <- function(penalty_info) {
  ret <- lapply(split(penalty_info, as.numeric(rownames(penalty_info))), as.vector)
  names(ret) <- NULL
  ret
}

read_nhl_game <- function(season, session, game_id, dir) {
  if(missing(dir))
    stop("Which directory should I look in? Specify dir argument.")

  
  
  toi <- do.call(rbind,
                 lapply(list.files(dir,
                                   pattern = paste(season, session, game_id,
                                     ".*toi", sep = "-"), full.names = TRUE),
                        parse_toi_data))

  ## not started or not present ("" if no playoff games will be the regex)
  if(toi[1,1] == "401" || game_id == "") {
      return(NULL)
  }

  game_summary <- parse_game_summary_data(
    list.files(dir, pattern = paste(season, session, game_id, ".*game_summary",
                      sep = "-"), full.names = TRUE))

  rosters <- parse_roster_data(
    list.files(dir, pattern = paste(season, session, game_id, ".*roster",
                      sep = "-"), full.names = TRUE))



  ret <- list(game_info = game_summary$game_info, 
              rosters = rosters,
              shifts = toi,
              goals = game_summary$goal_info,
              penalties = game_summary$penalty_info)

  shared_toi <- toi_struct(ret)
  ret <- c(ret, shared_toi = list(shared_toi))

  class(ret) <- "hockey"
  ret
}

read_nhl_season <- function(season, dir) {

  regular <- lapply(list_game_ids(season, "regular", dir),
                    read_nhl_game, season = season, session = "regular",
                    dir = dir)
  playoffs <- lapply(list_game_ids(season, "playoffs", dir),
                     read_nhl_game, season = season, session = "playoffs",
                     dir = dir)
  c(regular, playoffs)
}

gen_game_json <- function(game, file = "", smartfile = FALSE, dir,
                          include_couch_rev = TRUE) {

  rosters <- data.table(game$rosters)
  setkeyv(rosters, c("session", "game_id", "first_name", "last_name", "number"))

  shifts <- data.table(game$shifts)
  setkeyv(shifts, c("session", "game_id", "first_name", "last_name", "number"))

  shifts_positions <- rosters[shifts]
  
  toi_struct <- lapply(split(shifts_positions, shifts_positions$player),
                       gen_toi_json_struct)
  names(toi_struct) <- NULL             #for array in JSON


  ## this really can't be correct, splitting on last name!
  goal_struct <- sapply(split(game[["goals"]], rownames(game[["goals"]])),
                        gen_goal_json_struct)
  names(goal_struct) <- NULL            #for array in JSON


  ## penalty structure 
  penalty_struct <- sapply(split(game[["penalties"]], rownames(game[["penalties"]])),
                           gen_penalty_json_struct)
  names(penalty_struct) <- NULL            #for array in JSON

  ## shared toi structure
  shared_toi_struct <- game$shared_toi

  session <- ifelse(game$game_info$session == "regular", 2, 3)

  ## shot structure (from nhlscrapr currently)
  pbp <- process.single.game(season = game$game_info$season[1],
                             gcode= paste0(session, unique(rosters$game_id)),
                             override.download = TRUE,
                             save.to.file = FALSE)$playbyplay
  shots <- subset(pbp, etype %in% c("SHOT", "GOAL", "BLOCK", "MISS"),
                  select = c("period", "seconds", "ev.team", "etype", "ev.player.1"))
  shots$sog <- ifelse(shots$etype %in% c("SHOT", "GOAL"), 1, 0)
  shots$display_time <- shots$seconds / 60
  names(shots)[names(shots) == 'ev.team'] <- 'team'
  names(shots)[names(shots) == 'ev.player.1'] <- 'player_number'
  shots$player_number <- sapply( shots$player_number, function(name) { unlist( strsplit( name, " " ) )[1] } )
  
  ## hardcode team name fix
  shots$team <- as.character(shots$team)
  shots$team <- ifelse(shots$team == "L.A", "LAK", shots$team)
  shots$team <- ifelse(shots$team == "N.J", "NJD", shots$team)
  shots$team <- ifelse(shots$team == "T.B", "TBL", shots$team)
  shots$team <- ifelse(shots$team == "S.J", "SJS", shots$team)
  
  shots <- lapply(split(shots, as.numeric(rownames(shots))), as.vector)
  names(shots) <- NULL
  
  ## faceoffs, also from nhlscrapr play-by-play
  fac <- subset(pbp, etype %in% c("FAC"),
                  select = c("period", "seconds", "ev.team","ev.player.1", "ev.player.2", "homezone"))
  fac$display_time <- fac$seconds / 60
  names(fac)[names(fac) == 'ev.team'] <- 'team'
  names(fac)[names(fac) == 'ev.player.1'] <- 'player_number_a'
  names(fac)[names(fac) == 'ev.player.2'] <- 'player_number_h'
  fac$player_number_a <- sapply( fac$player_number_a, function(name) { unlist( strsplit( name, " " ) )[1] } )
  fac$player_number_h <- sapply( fac$player_number_h, function(name) { unlist( strsplit( name, " " ) )[1] } )
  ## hardcode team name fix
  fac$team <- as.character(fac$team)
  fac$team <- ifelse(fac$team == "L.A", "LAK", fac$team)
  fac$team <- ifelse(fac$team == "N.J", "NJD", fac$team)
  fac$team <- ifelse(fac$team == "T.B", "TBL", fac$team)
  fac$team <- ifelse(fac$team == "S.J", "SJS", fac$team)
  
  fac <- lapply(split(fac, as.numeric(rownames(fac))), as.vector)
  names(fac) <- NULL
  
  ## game_struct has overall game information 
  game_struct <- game$game_info
  
  if(smartfile)
    file <- paste0(dir, "/", game_struct$season, "-", game_struct$session, "-",
                   game_struct$game_id,
                   ".json")

  couch_rev <- get_current_doc_rev(
      paste0(game_struct$season, "-", game_struct$session, "-",
             game_struct$game_id))

  if(include_couch_rev && !is.null(couch_rev) && !is.na(couch_rev)) {
    
    cat(RJSONIO::toJSON(list(`_rev` = couch_rev,
                             game = game_struct,
                             toi = toi_struct,
                             goals = goal_struct,
                             penalties = penalty_struct,
                             shared_toi = shared_toi_struct,
                             shots = shots,
                             fac = fac
                             ),
               .withNames = TRUE), file = file)
  } else {
      cat(RJSONIO::toJSON(list(game = game_struct,
                               toi = toi_struct,
                               goals = goal_struct,
                               penalties = penalty_struct,
                               shared_toi = shared_toi_struct,
                               shots = shots,
                               fac = fac),
                          .withNames = TRUE), file = file)
  }
}

## end of package functions 
################################################################################




