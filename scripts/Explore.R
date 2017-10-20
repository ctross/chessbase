################################################################################
# Explore MillionBase2.2
#
# This script visulaizes the descriptive properties of the data-base
#
library(rethinking)
################################################################################
# First load the database
load("./Data/ChessOpeningDatabase_Clean_Thin.RData")

################################################################################
# Games and players as white over time
year.list <- sort(unique(R$Year))
player.data <- c(R$White)
player.data.date <- c(R$Year)
n.games <- nrow(R)
n.games.yr <- table(R$Year)
n.players.yr <- tapply(player.data, player.data.date, function(z) length(unique(z)))

pdf("ChessSampleSize.pdf",height=4,width=8)
par(mar = c(5,5,2,5))
plot(year.list, log10(as.numeric(n.games.yr)), type="h", lwd=12, ylab=expression(paste("Sample size of games, ", log[10], " units.")),xlab="Year",ylim=c(1,5.5),col="grey",lend="square")
par(new = T)
plot(year.list, log10(n.players.yr), type="h", axes=F, xlab=NA, ylab=NA, lwd=4,lend="square",ylim=c(1,5.5))
axis(side = 4)
mtext(side = 4, line = 3, expression(paste("Sample size of individuals, ", log[10], " units.")))
 dev.off()

################################################################################
# Elo over time
player.data <- c(R$White, R$Black)
player.elo <- c(R$WhiteElo, R$BlackElo)
player.data.date <- c(R$Year, R$Year)
mean.Elo.yr <- tapply(player.elo, player.data.date, function(z) mean(z, na.rm=TRUE))
pci.Elo.yr <- tapply(player.elo, player.data.date, function(z) PCI(na.omit(z)))
pcl.Elo.year <- unlist(pci.Elo.yr)[seq(1,85,by=2)]
pch.Elo.year <- unlist(pci.Elo.yr)[seq(2,86,by=2)]

pdf("ChessElo.pdf",height=4,width=8)
plot(year.list, mean.Elo.yr, type="l", ylim=c(1600, 2900),lwd=3,ylab="Elo",xlab="Year")
abline(h=1800,lty=3,col="grey80")
abline(h=2000,lty=3,col="grey80")
abline(h=2200,lty=3,col="grey80")
abline(h=2400,lty=3,col="grey80")
abline(h=2600,lty=3,col="grey80")
abline(h=2800,lty=3,col="grey80")
lines(year.list, mean.Elo.yr,lwd=3)
lines(year.list, pcl.Elo.year,col="grey",lwd=3)
lines(year.list, pch.Elo.year,col="grey",lwd=3)
dev.off()

################################################################################
# Wins, lossess, and draws over time
R$Win  <- as.numeric(R$Result=="1")
R$Draw <- as.numeric(R$Result=="2")
R$Lose <- as.numeric(R$Result=="3")

n.games.yr <- table(R$Year)

n.games.white.wins <- tapply(R$Win, R$Year, sum)
n.games.black.wins <- tapply(R$Lose, R$Year, sum)
n.games.drawn <- tapply(R$Draw, R$Year, sum)

pdf("ChessWinLossDraw.pdf",height=4,width=8)
plot(year.list, as.numeric(n.games.white.wins/n.games.yr), type="l", lwd=3,col="gray", ylim=c(0.2,0.5),ylab="Frequency",xlab="Year")
abline(h=0.35,lty=3,col="grey80")
abline(h=0.4,lty=3,col="grey80")
abline(h=0.45,lty=3,col="grey80")
abline(h=0.3,lty=3,col="grey80")
abline(h=0.5,lty=3,col="grey80")
abline(h=0.25,lty=3,col="grey80")
abline(h=0.2,lty=3,col="grey80")
points(year.list, n.games.white.wins/n.games.yr, type="l",col="gray",lwd=3)
points(year.list, n.games.black.wins/n.games.yr, type="l",lwd=3)
points(year.list, n.games.drawn/n.games.yr, type="l",lwd=3, col="indianred")
dev.off()

################################################################### Table 1
library(xtable)

Tal<-R[which(R$White=="TalM" & R$Year==1982),][1:9,c(1:11,18)]
Tal$Year<-factor(Tal$Year)
Tal$Month<-factor(Tal$Month)
Tal$Result<-factor(Tal$Result)
Tal$M3 <- rep("$\\ldots$",9)
colnames(Tal)[11]<-"$\\ldots$"
print(xtable(Tal), include.rownames = FALSE, sanitize.text.function = identity,sanitize.colnames.function = identity)









