################################################################################
## testing

library(plyr)
raw_html_dir <- "/home/erik/Dropbox/sites/shiftchart.com/data/scrape/raw_html"
raw_json_dir <- "/home/erik/Dropbox/sites/shiftchart.com/data/scrape/raw_json/"

## comment out for fast eval when testing 
season2012 <- read_nhl_season("20122013", raw_html_dir)
system.time(gen_season_json(season2012, raw_json_dir))

season2013 <- read_nhl_season("20132014", raw_html_dir)
system.time(gen_season_json(season2013, raw_json_dir))

json_files <- list.files(pattern = "20122013-regular",
                         raw_json_dir, full.names = TRUE)
lapply(json_files, file_to_couch)


json_files <- list.files(pattern = "20132014-regular",
                         raw_json_dir, full.names = TRUE)
lapply(json_files, file_to_couch)


##
################################################################################


################################################################################
## add the sc_id field to each game in the current season

games_regular_2013 <- sprintf("%04.0f", 1:1230) 
## 2013-2014 regular season rosters

write_nhl_data(season = "20132014", session = "regular",
               type = "roster", games = games_regular_2013,
               sleep = 1, force_download = FALSE)

## 2013-2014 regular season TOI
write_nhl_data(season = "20132014", session = "regular",
               type = "toi_home", games = games_regular_2013,
               sleep = .2)

write_nhl_data(season = "20132014", session = "regular",
               type = "toi_away", games = games_regular_2013,
               sleep = .2)

## 2013-2014 regular_season/playofs game summaries
write_nhl_data(season = "20132014", session = "regular",
               type = "game_summary", games = games_regular_2013,
               sleep = .2)

## now they are on disk, read them into R
current_season <- read_nhl_season("20132014", raw_html_dir)
## might be 1 NULL element in here before playoffs start? 

## generate the JSON
system.time(gen_season_json(current_season, raw_json_dir,
                            include_couch_rev = TRUE))

json_files <- list.files(pattern = "20132014-regular",
                         raw_json_dir, full.names = TRUE)

lapply(json_files, file_to_couch)

################################################################################


## R data.frame
nhl <- read_nhl_season("20132014", raw_html_dir)
save(nhl, file = "~/Dropbox/sites/shiftchart-data/nhl_current.RData")

## setwd() to path of dropbox share
load("nhl_current.RData")

## What do we got per game?
names(nhl[[1]])

## construct overall TOI data.frame, wish this had position, can merge with roster
toi <- do.call(rbind, lapply(nhl, "[[", "shifts"))

## leaders, lots of goalies obviously 
rev(sort(tapply(toi$duration, paste(toi$team, toi$player), sum)))[1:30]

goals <- do.call(rbind, lapply(nhl, "[[", "goals"))

## goal leaders
rev(sort(table(paste(goals$goal_player_number, goals$goal_last_name, goals$team))))[1:5]



## ## scrape, put here to get it out of the nhl_scrape.R file
games_regular_2012 <- sprintf("%04.0f", 1:720)
## games_regular_2011 <- sprintf("%04.0f", 1:1230)
## games_regular_2010 <- sprintf("%04.0f", 1:1230)
## games_regular_2009 <- sprintf("%04.0f", 1:1230)

possible_playoff_games <-
  paste0(0, sort(apply(expand.grid(1:4, 1:8, 1:7), 1,
             paste, collapse = ""))[-c(85:112, 127:168, 176:224)])

## ## 2010-2011 regular season rosters
## write_nhl_data(season = "20102011", session = "regular",
##                type = "roster", games = games_regular_2010,
##                sleep = 2)
## write_nhl_data(season = "20102011", session = "playoffs",
##                type = "roster", games = possible_playoff_games,
##                sleep = 1)


## ## 2011-2012 regular season rosters
## write_nhl_data(season = "20112012", session = "regular",
##                type = "roster", games = games_regular_2011,
##                sleep = 0.5)

## ## 2011-2012 playoffs rosters
## write_nhl_data(season = "20112012", session = "playoffs",
##                type = "roster", games = possible_playoff_games,
##                sleep = 1)


## 2012-2013 regular season rosters
write_nhl_data(season = "20122013", session = "regular",
               type = "roster", games = games_regular_2012,
               sleep = 0.5)

## 2012-2013 playoffs rosters
write_nhl_data(season = "20122013", session = "playoffs",
               type = "roster", games = possible_playoff_games,
               sleep = 1)

## 2012-2013 regular season TOI
write_nhl_data(season = "20122013", session = "regular",
               type = "toi_home", games = games_regular_2012,
               sleep = 0.5)
write_nhl_data(season = "20122013", session = "regular",
               type = "toi_away", games = games_regular_2012,
               sleep = 0.5)

## 2012-2013 playoffs TOI
write_nhl_data(season = "20122013", session = "playoffs",
               type = "toi_home", games = possible_playoff_games,
               sleep = 1)
write_nhl_data(season = "20122013", session = "playoffs",
               type = "toi_away", games = possible_playoff_games,
               sleep = 1)

## 2012-2013 regular_season/playofs play-by-play
write_nhl_data(season = "20122013", session = "regular",
               type = "pbp", games = games_regular_2012,
               sleep = .5)

write_nhl_data(season = "20122013", session = "playoffs",
               type = "pbp", games = possible_playoff_games,
               sleep = 1)


## 2012-2013 regular_season/playofs game summaries
write_nhl_data(season = "20122013", session = "regular",
               type = "game_summary", games = games_regular_2012,
               sleep = .5)

write_nhl_data(season = "20122013", session = "playoffs",
               type = "game_summary", games = possible_playoff_games,
               sleep = .5)

## remove 404 files 

file_df <- file.info(
  list.files("/home/erik/Dropbox/sites/shiftchart.com/data/scrape/raw_html",
             full.names = TRUE))

row.names(subset(file_df, size %in% c(237, 238)))
file.remove(row.names(subset(file_df, size %in% c(237, 238))))

## last nights' TOI leaders
toi <- do.call(rbind, lapply(todays_games, "[[", "shifts"))
toi$teamName3Char <- toi$team
toi <- merge(toi, nhl_teams, by = "teamName3Char")
summary(toi$duration) * 60              #should remove goalies for this calc
subset(toi, duration * 60 > 500)
sort(tapply(toi$duration * 60, paste(toi$team, toi$player), sum))


gen_daily_tweet <- function(todays_games, n = 3) {
  toi <- do.call(rbind, lapply(todays_games, "[[", "shifts"))
  toi$teamName3Char <- toi$team
  toi <- merge(toi, nhl_teams, by = "teamName3Char")
  top_toi <- ddply(toi, .(team, player), function(x)
                 data.frame(total_toi = sum(x$duration),
                            hashtag = unique(x$hashtag)))
  
  top_toi <- top_toi[order(top_toi$total_toi, decreasing = TRUE),]
  top_toi$display_time <- time_convert_back(top_toi$total_toi)

  paste(format(Sys.Date() - 1, "%m-%d-%y"),
        "#NHL TOI leaders:",
        apply(top_top[1:n,], 1,
              function(x) paste(x[),
        "shiftchart.com")))
}

gen_daily_tweet(todays_games)




























        

        
