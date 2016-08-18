#####
# some documentation should go here
#####

# get oracle from yawgatog

# thanks to r2Evans on StackOverflow
# http://stackoverflow.com/questions/38958597/r-split-text-on-empty-line


oracleRaw <- readLines("All Sets.txt")

breaks <- which(!nzchar(oracleRaw))
nbreaks <- length(breaks)
if (nbreaks > 0L) {
  oracle <- mapply(function(a,b) paste(oracleRaw[a:b], collapse = " "),
                   c(1L, breaks[-nbreaks]),
                   c(breaks[-1L], length(oracleRaw)))
}


oracleRaw <- readLines("All Sets.txt")

breaks <- which(!nzchar(oracleRaw))
nbreaks <- length(breaks)
if (nbreaks > 0L) {
  oracle <- mapply(function(a,b) paste(oracleRaw[a:b], collapse = " "),
                   c(1L, breaks[1:(nbreaks-1)]),
                   breaks)
}

oracle[1]

breaks[1]
oracleRaw[breaks[1]]


#################

# how to strip out a creatures basic attributes line to convert to a vector


unlist(strsplit(oracleRaw[breaks[72]+5], split=", "))

################

# find differences in breaks

diff <- vector()

for(i in 2:length(breaks)){
  diff <- c(diff, breaks[i] - breaks[i-1])
}

max(diff)

min(diff)

plot(sort(diff))


# function to get a card based on number

getCard <- function(breakNum, oracleRaw, breaks){
  return(oracleRaw[(breaks[breakNum] + 1):(breaks[breakNum + 1] - 1)])
}


getCard(100, oracleRaw, breaks)
getCard(200, oracleRaw, breaks)
getCard(300, oracleRaw, breaks)
getCard(400, oracleRaw, breaks)   # no casting cost, language for Transform,
getCard(500, oracleRaw, breaks)
getCard(600, oracleRaw, breaks)
getCard(700, oracleRaw, breaks)
getCard(800, oracleRaw, breaks)
getCard(900, oracleRaw, breaks)
getCard(1000, oracleRaw, breaks)

getCard(length(breaks) - 1, oracleRaw, breaks) # this is the last index that will work



# fill a ragged Momir database

Momir <- list()

creature <- "Creature"
unhinged <- "UNH-"
unglued <- "UG-" # might be problematic if a new set comes out "_UG-"
                 # UG- filter only removed 8 creatures... seems... low...

MomirCount <- 0

for(i in 1:(length(breaks) - 1)){
  card <- getCard(i, oracleRaw, breaks)
  if(grepl(creature,card[3]) & !grepl(unhinged, card[length(card)]) & !grepl(unglued, card[length(card)])){
    MomirCount <- MomirCount + 1
    Momir[[MomirCount]] <- card
  }
}

# next make CMC variable


