################################################################################
# Thin MillionBase2.2
#
# Thin to higher quality subset of data

load("./Data/ChessOpeningDatabase.RData")

################################################################################
R <- data.frame(GameID=R$hash_id,
                Year=R$Year,
                Month=R$Month,
                White=factor(R$White),
                Black=factor(R$Black),
                Result=R$Outcome,
                WhiteElo=R$WhiteElo,
                BlackElo=R$BlackElo,
                M1=factor(R$m1),
                M2=factor(R$m2),
                M3=factor(R$m3),
                M4=factor(R$m4),
                M5=factor(R$m5),
                M6=factor(R$m6),
                M7=factor(R$m7),
                M8=factor(R$m8),
                M9=factor(R$m9),
                M10=factor(R$m10)
                )

######################################################### Remove games involving unrated players
Players <- c(as.character(R$White),as.character(R$Black))
Elos <- c(R$WhiteElo,R$BlackElo)

EloFreq <- tapply(Elos,Players,function(z) mean(!is.na(z)))

unratedPlayers <- names(EloFreq)[which(EloFreq==0)]

Drop1a <- which(R$White %in% unratedPlayers)
Drop1b <- which(R$Black %in% unratedPlayers)

Drop1 <- unique(c(Drop1a, Drop1b))
length(Drop1)         # Drops 809456 games
length(Drop1)/nrow(R) # thats 0.37 of games

R<-R[-Drop1,]

################################################################### Thin on year
R$Win  <- as.numeric(R$Result=="1")
R$Draw <- as.numeric(R$Result=="2")
R$Lose <- as.numeric(R$Result=="3")

n.games.yr <- table(R$Year)
year.list <- sort(unique(R$Year))
player.data <- c(as.character(R$White), as.character(R$Black))
player.data.date <- c(R$Year, R$Year)
n.games <- nrow(R)
n.players.yr <- tapply(player.data, player.data.date, function(z) length(unique(z)))

n.games.white.wins <- tapply(R$Win, R$Year, sum)
n.games.black.wins <- tapply(R$Lose, R$Year, sum)
n.games.drawn <- tapply(R$Draw, R$Year, sum)


plot(as.numeric(as.character(year.list)), na.omit(as.numeric(n.games.white.wins/n.games.yr)), type="n", lwd=3,col="gray", ylim=c(0,1),ylab="Frequency",xlab="Year")
abline(h=0.2,lty=3,col="grey80")
abline(h=0.4,lty=3,col="grey80")
abline(h=0.6,lty=3,col="grey80")
abline(h=0.3,lty=3,col="grey80")
abline(h=0.5,lty=3,col="grey80")
points(as.numeric(as.character(year.list)), na.omit(as.numeric(n.games.white.wins/n.games.yr)), type="l",col="gray",lwd=3)
points(as.numeric(as.character(year.list)), na.omit(as.numeric(n.games.black.wins/n.games.yr)), type="l",lwd=3)
points(as.numeric(as.character(year.list)), na.omit(as.numeric(n.games.drawn/n.games.yr)), type="l",lwd=3, col="indianred")

# Frequency of wins, draws, and losses seems to stabilize around 1970
R$Year <- as.numeric(as.character(R$Year))
R$Month <- as.numeric(as.character(R$Month))
Drop2 <- which(R$Year<1971)
length(Drop2)     # Drops 24553 games
length(Drop2)/nrow(R) # thats a little under 0.02 of games
R<-R[-Drop2,]
dim(R)
# thinned database now has 1362730 games


# Finally, drop records of games by white in which the name appears less than 5 times, since these might often be due to misspellings of name
n_games_minimum <- 5
n_games_as_white <- table(R$White)
PeepsWithEnough <- names(which(n_games_as_white>=n_games_minimum))
PeepsWithoutEnough <- names(which(n_games_as_white<n_games_minimum))

PeepsWithoutEnough <- sample(PeepsWithoutEnough, size=floor(length(PeepsWithoutEnough)*0.01))

n_players <- length(PeepsWithEnough)

similarity <- function(x, y) 1-adist(x, y)/pmax(nchar(x), nchar(y))

how_similar <- rep(NA, length(PeepsWithoutEnough))
for(i in 1:length(PeepsWithoutEnough)){
  
  how_similar[i] <- max(similarity(PeepsWithEnough, PeepsWithoutEnough[i]))
  
  if(i %% 100 == 0) print(round((i/n_players)*100))

}


Keep <- which(R$White %in% PeepsWithEnough)
length(R$White)-length(Keep)     # Drops 51273 games

R <- data.frame(GameID=R$GameID[Keep],
                Year=R$Year[Keep],
                Month=R$Month[Keep],
                White=factor(R$White[Keep]),
                Black=factor(R$Black[Keep]),
                Result=R$Result[Keep],
                WhiteElo=R$WhiteElo[Keep],
                BlackElo=R$BlackElo[Keep],
                M1=factor(R$M1[Keep]),
                M2=factor(R$M2[Keep]),
                M3=factor(R$M3[Keep]),
                M4=factor(R$M4[Keep]),
                M5=factor(R$M5[Keep]),
                M6=factor(R$M6[Keep]),
                M7=factor(R$M7[Keep]),
                M8=factor(R$M8[Keep]),
                M9=factor(R$M9[Keep]),
                M10=factor(R$M10[Keep])
                )

################################################################################
# collision check: career lengths

player_list <- sort(unique(as.character(R$White)))

#player_list <- sample(player_list, size=floor(length(player_list)*0.01))

n_players <- length(player_list)

career_length <- rep(NA, length(player_list))
career_games <- rep(NA, length(player_list))
career_pace_stddev <- rep(NA, length(player_list))

for(i in 1:length(player_list)){

  my_years <- R$Year[which(R$White==player_list[i])]
  career_length[i] <- max(my_years) - min(my_years)
  career_pace_stddev[i] <- sd(my_years)
  career_games[i] <- length(my_years)

  if(i %% 100 == 0) print(round(i/n_players*100))

}

plot(career_pace_stddev~career_games,log="x")
player_list[which(career_pace_stddev>14)]

set.seed(1)
samps<-sample(player_list[which(career_pace_stddev>14)],34)   # Sample 20 percent

Rcheck <- R[which(R$White %in% samps),1:9]
Rcheck <- Rcheck[order(Rcheck$White, Rcheck$Year),]


Keep <- !R$White %in% player_list[which(career_pace_stddev>14 & career_games<50 )]
length(R$White )-sum(Keep)     # Drops 1037 games

R <- data.frame(GameID=R$GameID[Keep],
                Year=R$Year[Keep],
                Month=R$Month[Keep],
                White=factor(R$White[Keep]),
                Black=factor(R$Black[Keep]),
                Result=R$Result[Keep],
                WhiteElo=R$WhiteElo[Keep],
                BlackElo=R$BlackElo[Keep],
                M1=factor(R$M1[Keep]),
                M2=factor(R$M2[Keep]),
                M3=factor(R$M3[Keep]),
                M4=factor(R$M4[Keep]),
                M5=factor(R$M5[Keep]),
                M6=factor(R$M6[Keep]),
                M7=factor(R$M7[Keep]),
                M8=factor(R$M8[Keep]),
                M9=factor(R$M9[Keep]),
                M10=factor(R$M10[Keep])
                )

save(R, file="./Data/ChessOpeningDatabase_Clean_Thin.RData")








