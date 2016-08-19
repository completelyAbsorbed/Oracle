#####
# some documentation should go here
#####

# Credits and Licensing : 
# 
# this code is written by Ben Young @BitBotTestBot on Twitter
# 
# please feel free to use it for any media that is available freely ala MTGGoldfish or ChannelFireball
# as of 08/19/2106. Please credit me by name and twitter handle!
# 
# you may not use this code for any media or software that requires a paywall
# this includes SCG articles, and pay-to-use software.
#
# any questions? tweet me @BitBotTestBot


# get oracle from yawgatog

# helpful links that informed some of the code below :
# http://stackoverflow.com/questions/38958597/r-split-text-on-empty-line
# http://stackoverflow.com/questions/7878992/finding-the-indexes-of-multiple-overlapping-matching-substrings

setwd("D:/GITHUB/Oracle")

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

creature <- "Creature --"
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


#get 0-cost creatures

Momir0 <- list()

MomirCount0 <- 0

for(i in 1:(length(breaks) - 1)){
  card <- getCard(i, oracleRaw, breaks)
  if(grepl(creature,card[3]) & !grepl(unhinged, card[length(card)]) & !grepl(unglued, card[length(card)]) & 
     card[2] == 0){
    MomirCount0 <- MomirCount0 + 1
    Momir0[[MomirCount0]] <- card
  }
}

Momir0

# look at unique mana costs among creatures

UniqueMC <- vector()
MC <- vector()

for(i in 1:length(Momir)){
  MC <- c(MC, Momir[[i]][2])
}

UniqueMC <- sort(unique(MC))

UniqueMC 

print(paste("The length of UniqueMC is : ", length(UniqueMC), sep = ""))




# next make CMC variable

CMC <- vector()

for(i in 1:length(Momir)){
 
}


getCMC <- function(manaCost){  # if new mana paradigms emerge, this will need to be updated and tested!
  runningTotal <- 0
  Xexpr <- Xdex <- TWOexpr <- TWOdex <- SPLITexpr <- SPLITdex <- STDexpr <- STDdex <- vector()
  # remove all "X" [0 per]
    # do a perl-based regex, then reduce to an index in Xdex
    Xexpr <- gregexpr("(?=X)", manaCost, perl = TRUE)[[1]]
    Xdex <- Xexpr[1:length(Xexpr)]
    # augment manaCost
    if(length(Xdex > 0) & !(length(Xdex) == 1 & min(Xdex) == -1)){
      if(min(Xdex) > 1){
        manaCost <- paste(substr(manaCost, 1, min(Xdex) - 1), 
                          substr(manaCost, max(Xdex) + 1, nchar(manaCost)), sep = "")
      }
      else{
        manaCost <- substr(manaCost, max(Xdex) + 1, nchar(manaCost))
      }
    }
  # if contains "2/" [2 per]
    # do a perl-based regex, then reduce to an index in Xdex
    TWOexpr <- gregexpr("(?=2/)", manaCost, perl = TRUE)[[1]]
    TWOdex <- TWOexpr[1:length(TWOexpr)]
    # augment manaCost, 
    if(length(TWOdex > 0) & !(length(TWOdex) == 1 & min(TWOdex) == -1)){
      if(min(TWOdex) > 1){
        manaCost <- paste(substr(manaCost, 1, min(TWOdex) - 2), 
                          substr(manaCost, max(TWOdex) + 4, nchar(manaCost)), sep = "")
      }
      else{
        manaCost <- substr(manaCost, max(TWOdex) + 4, nchar(manaCost))
      }
      runningTotal <- runningTotal + 2 * length(TWOdex)
    }
  # if contains "(w/" | "(u/" | "(r/" | "(b/" | "(g/" [1 per] #check that phyrexian mana CMC = 1
    SPLITexpr <- gregexpr("(?=w/)", manaCost, perl = TRUE)[[1]]
    if(!(min(SPLITexpr) == -1)){
      SPLITdex <- c(SPLITdex, SPLITexpr[1:length(SPLITexpr)])
    }
    SPLITexpr <- gregexpr("(?=u/)", manaCost, perl = TRUE)[[1]]
    if(!(min(SPLITexpr) == -1)){
      SPLITdex <- c(SPLITdex, SPLITexpr[1:length(SPLITexpr)])
    }
    SPLITexpr <- gregexpr("(?=b/)", manaCost, perl = TRUE)[[1]]
    if(!(min(SPLITexpr) == -1)){
      SPLITdex <- c(SPLITdex, SPLITexpr[1:length(SPLITexpr)])
    }
    SPLITexpr <- gregexpr("(?=r/)", manaCost, perl = TRUE)[[1]]
    if(!(min(SPLITexpr) == -1)){
      SPLITdex <- c(SPLITdex, SPLITexpr[1:length(SPLITexpr)])
    }
    SPLITexpr <- gregexpr("(?=g/)", manaCost, perl = TRUE)[[1]]
    if(!(min(SPLITexpr) == -1)){
      SPLITdex <- c(SPLITdex, SPLITexpr[1:length(SPLITexpr)])
    }
    runningTotal <- runningTotal + length(SPLITdex)  # need if for empty case ######### !!! !!! !!!
    
    # augment manaCost, 
    if(length(SPLITdex > 0)){
      if(min(SPLITdex) > 1){
        manaCost <- paste(substr(manaCost, 1, min(SPLITdex) - 2), 
                          substr(manaCost, max(SPLITdex) + 4, nchar(manaCost)), sep = "")
      }
      else{
        manaCost <- substr(manaCost, max(SPLITdex) + 4, nchar(manaCost))
      }
    }                                
  # if contains "W" | "U" | "R" | "B" | "G" | "C" [1 per]
    STDexpr <- gregexpr("(?=W)", manaCost, perl = TRUE)[[1]]
    if(!(min(STDexpr) == -1)){
      STDdex <- c(STDdex, STDexpr[1:length(STDexpr)])
    }
    STDexpr <- gregexpr("(?=U)", manaCost, perl = TRUE)[[1]]
    if(!(min(STDexpr) == -1)){
      STDdex <- c(STDdex, STDexpr[1:length(STDexpr)])
    }
    STDexpr <- gregexpr("(?=B)", manaCost, perl = TRUE)[[1]]
    if(!(min(STDexpr) == -1)){
      STDdex <- c(STDdex, STDexpr[1:length(STDexpr)])
    }
    STDexpr <- gregexpr("(?=R)", manaCost, perl = TRUE)[[1]]
    if(!(min(STDexpr) == -1)){
      STDdex <- c(STDdex, STDexpr[1:length(STDexpr)])
    }
    STDexpr <- gregexpr("(?=G)", manaCost, perl = TRUE)[[1]]
    if(!(min(STDexpr) == -1)){
      STDdex <- c(STDdex, STDexpr[1:length(STDexpr)])
    }
    STDexpr <- gregexpr("(?=C)", manaCost, perl = TRUE)[[1]]
    if(!(min(STDexpr) == -1)){
      STDdex <- c(STDdex, STDexpr[1:length(STDexpr)])
    }
    runningTotal <- runningTotal + length(STDdex)  
    
    # augment manaCost, 
    if(length(STDdex > 0)){
      if(min(STDdex) > 1){
        manaCost <- paste(substr(manaCost, 1, min(STDdex) - 1), 
                          substr(manaCost, max(STDdex) + 1, nchar(manaCost)), sep = "")
      }
      else{
        manaCost <- substr(manaCost, max(STDdex) + 1, nchar(manaCost))
      }
    }                                
    
    # remove listings
  
  # should be nothing or a number remaining [# or 0]
  if(nchar(manaCost) == 0){
    # if nchar = 0, do nothing, we're done!
    # otherwise, extract the remaining value and add
  }
  else{
    runningTotal <- runningTotal + as.numeric(manaCost)
  }
    
  return(runningTotal)
}

# display CMC for each unique MC

for(mc in UniqueMC){
  print(paste("Mana Cost : ", mc, "      CMC : ", getCMC(mc), sep = ""))
}


# Append CMC to the last entry for each card in Momir

for(i in 1:length(Momir)){
  Momir[[i]][length(Momir[[i]]) + 1] <- getCMC(Momir[[i]][2])
}

head(Momir)