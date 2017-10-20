################################################################################
# Clean MillionBase2.2
#
# This script reads a series of .csv text files taken from MillionBase 2.2,
# thins and cleans the data, and outputs an R environment.
#

################################################################################
# Load Data: All files are included in the ParsedData folder

rbind_plus <- function( df1, df2 ){
    df1 <- do.call(data.frame, df1)
    df2 <- do.call(data.frame, df2)
    n1 <- colnames(df1)
    n2 <- colnames(df2)

    if( any(!n1 %in% n2) ){
        new_cols <- n1[!n1 %in% n2]
        df2 <- cbind(df2, matrix("", nrow=nrow(df2), ncol=length(new_cols)))
        colnames(df2) <- c(n2, new_cols)
        }

    if( any(!n2 %in% n1) ){
        new_cols <- n2[!n2 %in% n1]
        df1 <- cbind(df1, matrix("", nrow=nrow(df1), ncol=length(new_cols)))
        colnames(df1) <- c(n1, new_cols)
        }

    output <- rbind(df1, df2)
    output
}

my_files <- list.files("./Data/csv", pattern="*.csv", full.names=TRUE)

R <- read.csv(my_files[1], stringsAsFactors=FALSE)

for(i in 2:length(my_files)){
  R <- rbind_plus(R, read.csv(my_files[i], stringsAsFactors=FALSE))
}

################################################################################
# Deal with Names
# Rename names to remove extra (wh) and (bl)
R$White <- gsub("\\(wh\\)", "", R$White)
R$Black <- gsub("\\(bl\\)", "", R$Black)

R$White <- gsub(" ", "", R$White)
R$Black <- gsub(" ", "", R$Black)

R$White <- gsub("\\.", "", R$White)
R$Black <- gsub("\\.", "", R$Black)

R$White <- gsub("\\,", "", R$White)
R$Black <- gsub("\\,", "", R$Black)

# Drop games played against self
Drop1 <- which(R$White==R$Black)
length(Drop1) # Drops 132 games
R<-R[-Drop1,]

################################################################################
# Deal with Elo
# Drop games where either player has probably erroneously Elo
Drop2 <- which(R$BlackElo<1000 | R$WhiteElo<1000)
length(Drop2) # Drops 28
R<-R[-Drop2,]

################################################################################
# Deal with Moves
# Drop broken records or impossible moves for first two moves

legal_m2s <- c(
"a6","b6","c6","d6","e6","f6","g6","h6",
"Na6",    "Nc6",         "Nf6",   "Nh6",
"a5","b5","c5","d5","e5","f5","g5","h5"
)

legal_m1s <- c(
"a4","b4","c4","d4","e4","f4","g4","h4",
"Na3",    "Nc3",         "Nf3",   "Nh3",
"a3","b3","c3","d3","e3","f3","g3","h3"
)

Drop3 <- which(!R$m1 %in% legal_m1s)
length(Drop3) # Drops 48
R<-R[-Drop3,]

Drop4 <- which(!R$m2 %in% legal_m2s)
length(Drop4)  # Drops 24
R<-R[-Drop4,]


################################################################################
# Deal with Years
R$Year <-  substr(as.character(R$Date), 1, 4)
R$Month <- substr(as.character(R$Date), 6, 7)
R$Day <- substr(as.character(R$Date), 9, 10)

R$Year <-  ifelse(R$Year!="??",R$Year,NA)
R$Month <- ifelse(R$Month!="??",R$Month,NA)
R$Day <- ifelse(R$Day!="??",R$Day,NA)

R$Date <- as.Date(R$Date, "%Y.%m.%d")

################################################################################
R$Outcome <- ifelse(R$Result=="1-0",1,
               ifelse(R$Result=="1/2-1/2",2,
                 ifelse(R$Result=="0-1",3,4)
               )
             )
Drop5 <- which(R$Outcome>3)
length(Drop5)  # Drops 217
R <- R[-Drop5,]

save(R, file="./Data/ChessOpeningDatabase.RData")
# 2196739 games in cleaned database



