## try uploading a (JSON) document to couchdb instance

library(httr)
library(tools)
library(RJSONIO)

## set as option when package created 
default_couch_server <- "http://localhost:5984/shiftchart/"

file_to_couch <- function(file,
                          couch_server = default_couch_server) {
  game_id <- file_path_sans_ext(basename(file))
  system(paste0("curl -X PUT -d @", file," ", couch_server, game_id))
}

get_current_doc_rev <- function(doc, couch_server = default_couch_server) {
  doc_content <- fromJSON(content(GET(paste0(couch_server, doc))))

  if(identical(names(doc_content), c("error", "reason")))
    NA
  else
    doc_content[["_rev"]]
}
