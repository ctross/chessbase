
rm(list=ls())

print(Sys.time())

library(devtools)
install_github('babeheim/chessparse')

library(jsonlite)
library(chessparse)
library(RCurl)

# setwd("C:\\Users\\cody_ross\\Dropbox\\Open Papers\\Chess Database\\DataInBrief")

download_file <- function(url, dest, overwrite=FALSE, mode="w"){
    if(!file.exists(dest) | overwrite){
        if(url.exists(url)){
            download.file(url, dest, mode=mode)
        } else {
            print("url is not responding")
        }
    } else {
        print("file already exists")
    }
}
 
dir_init <- function(path, verbose=FALSE){
    if(substr(path, 1, 2)!='./') stop('path argument must be formatted
        with "./" at beginning')
    contents <- dir(path, recursive=TRUE)
    if(verbose){
        if(length(contents)==0) print(paste('folder ', path, ' created.', sep=""))
        if(length(contents)>0) print(paste('folder ', path, ' wiped of ', length(contents), ' files/folders.', sep=""))
    }
    if(dir.exists(path)) unlink(path, recursive=TRUE)
    dir.create(path)
}

dir_init("./Data")

download_file("http://www.top-5000.nl/dl/millionbase%202.2.exe?forcedownload", 
  "./Data/millionbase.7z")

dir_init('./Data/pgn')

system("7z x ./Data/millionbase.7z -o./Data/pgn") # only works on max/linux with p7zip installed!

# oh oh add n_moves!

dir_init('./Data/json')
dir_init('./Data/csv')

pgn_file <- "./Data/pgn/millionbase-2.22.pgn"

d_all <- read_pgn(pgn_file)

save(d_all, file="./Data/millionbase-2.22_raw.RData")
load("./Data/millionbase-2.22_raw.RData")

chunk_size <- 30000

n_chunks <- ceiling(length(d_all)/chunk_size)

N_moves <- 10

for(i in 1:n_chunks){

  target_entries <- 1:chunk_size + chunk_size*(i-1)
  target_entries <- target_entries[target_entries %in% 1:length(d_all)]

  d_chunk <- d_all[target_entries]

  dj <- toJSON(d_chunk)
  chunk_json <- paste("./Data/json/ChessData (", i, ").json", sep="")
  writeLines(dj, chunk_json)

  d <- read_json(chunk_json, simplifyVector=TRUE)
  move_tab <- matrix(NA, nrow=length(d$moves), ncol=N_moves)
  for(i in 1:length(d$moves)) if(!is.null(d$moves[[i]][1:N_moves])) move_tab[i,] <- d$moves[[i]][1:N_moves]
  colnames(move_tab) <- c("m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10")
  d <- d[-which(names(d)=="moves")] # how to fix?
  d <- df_list_to_dataframe(d)
  d <- cbind(d, move_tab)
  chunk_csv <- gsub("json", "csv", chunk_json)
  write.csv(d, chunk_csv, row.names=FALSE)

  print(chunk_csv)

}


print(Sys.time())
