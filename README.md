# chessbase
<img align="right" src="https://github.com/ctross/chessbase/blob/master/logo.png" alt="logo" width="200">
An R database of 1.12 million chess opening sequences: A 43-year time-series of game-play from 42,644 Chess players linked in annually-resolved game-playing networks.
----

A central focus in the study of cultural evolution is the relative importance of individual and social information in driving the diffusion of behavioral and symbolic variants. In observational social learning research, however, empirical tests of quantitative models are limited by a dearth of long-term, individual-level data. Here we present an archive of longitudinal records from the game of Chess, which will allow researchers to study the yearby-year evolution of opening strategies among a large multi-national population of players. These records will allow for analyses of within-individual skill development, diffusion models of strategies over time, network-structured time-series modeling of social learning, and measurement of effects motivated by other theoretical models of social learning including payoff- and frequency-dependent heuristics. These records provide a unique window into a long-term record of human behavior, often in high stakes competitions.

# Install with
library(devtools)

install_github("ctross/chessbase")

# Explore
library(chessbase)

head(chessdata)
