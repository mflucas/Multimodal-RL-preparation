#setwd("C:/Users/mlucas/polybox/Masterarbeit/Multimodal_Estimation/Data_preparation_ptandwalk")
#setwd("/Users/lucasmeyer/polybox/Masterarbeit/Multimodal_Estimation/Data_preparation_ptandwalk")
getwd()

library(plyr)
library(dplyr)
library(geosphere)
library(stringr)
library(data.table)
library(igraph)
library(rgdal)
library(raster)
library(sp)
library(maptools)


rm(list = ls())
#_______________________________________________________________________________________________________________________________________________________________#
#This commented out part is for the R5 network
# walkingNetwork <- read.table("Walk_edges_large.txt", sep=",", header=TRUE)
# 
# #Scale the coordinates to real lat lon values
# walkingNetwork$fromLat <- walkingNetwork$fromLat/10000000
# walkingNetwork$fromLon <- walkingNetwork$fromLon/10000000
# walkingNetwork$toLat <- walkingNetwork$toLat/10000000
# walkingNetwork$toLon <- walkingNetwork$toLon/10000000
# 
# walkingNetwork$OSMid <- NULL
# 
# walkingNetwork <- subset(walkingNetwork, fromLon<8.6541 & fromLon>8.3019 & fromLat<47.4940& fromLat>47.2960)
# walkingNetwork <- subset(walkingNetwork, toLon<8.6541 & toLon>8.3019 & toLat<47.4940 & toLat>47.2960)

#Read in the Graphhopper MATSim network_Car
hopper_Network_car <- read.table("Edgelist_hopper.txt", sep=",", header=TRUE)

#Read in the Graphhopper MATSim network_slow
hopper_Network_slow <- read.table("edgeList_MATSIM_slow.txt", sep=",", header=TRUE)

#Read in the transport network
transitNetwork <- read.table("network_pt_zurich_coords.txt", sep=",", header=TRUE)

transitNetwork$isTransfer
ptNet <- subset(transitNetwork, isTransfer==0)
transPT <- subset(transitNetwork, isTransfer==1)


#PLOT TESTS
#_______________________________________________________________________________________________________________________________________________________________#

#Quick plot of the network
#First convert latlon to UTM to maintain true distances in plot
LongLatToUTM<-function(x,y,zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
  return(as.data.frame(res))
}

fromUTM_hopper <- LongLatToUTM(hopper_Network_car$FromX, hopper_Network_car$FromY, "32T")
toUTM_hopper <- LongLatToUTM(hopper_Network_car$ToX, hopper_Network_car$ToY, "32T")

hopper_Network_car$FromX <- fromUTM_hopper$X
hopper_Network_car$FromY <- fromUTM_hopper$Y
hopper_Network_car$ToX <- toUTM_hopper$X
hopper_Network_car$ToY <- toUTM_hopper$Y

#Plot the map. Add background in illustrator.
plot(hopper_Network_car$FromX, hopper_Network_car$FromY, cex=0.1)
segments(hopper_Network_car$FromX, hopper_Network_car$FromY, hopper_Network_car$ToX, hopper_Network_car$ToY, lwd=4, col="gray12")


fromUTM_r5 <- LongLatToUTM(ptNet$FromX, ptNet$FromY, "32T")
toUTM_r5 <- LongLatToUTM(ptNet$ToX, ptNet$ToY, "32T")

ptNet$FromX <- fromUTM_r5$X
ptNet$FromY <- fromUTM_r5$Y
ptNet$ToX <- toUTM_r5$X
ptNet$ToY <- toUTM_r5$Y

#Plot the map. Add background in illustrator.
points(ptNet$FromX, ptNet$FromY, cex=0.1, col="red")
segments(ptNet$FromX, ptNet$FromY, ptNet$ToX, ptNet$ToY, lwd=1.6, col="red")
# 
fromUTM_trans<- LongLatToUTM(transPT$FromX, transPT$FromY, "32T")
toUTM_trans <- LongLatToUTM(transPT$ToX, transPT$ToY, "32T")

transPT$FromX <- fromUTM_trans$X
transPT$FromY <- fromUTM_trans$Y
transPT$ToX <- toUTM_trans$X
transPT$ToY <- toUTM_trans$Y

points(transPT$FromX, transPT$FromY, cex=0.1)
segments(transPT$FromX, transPT$FromY, transPT$ToX, transPT$ToY, lwd=1, col="blue")

# 
# 
# segments(edgeListTransfers$FromX, edgeListTransfers$FromY, edgeListTransfers$ToX, edgeListTransfers$ToY, col="red")
#_______________________________________________________________________________________________________________________________________________________________#


#Create stop lists for each network
#1 Transport Network
stops1 <- transitNetwork[,c("FromStop","FromX", "FromY")]
names(stops1) <- c("StopID","X","Y")
stops2 <- transitNetwork[,c("ToStop","ToX", "ToY")]
names(stops2) <- c("StopID","X","Y")
allEntries <- rbind(stops1, stops2)
stops <- allEntries[!(duplicated(allEntries$StopID)),]
rm(allEntries)
rm(stops1)
rm(stops2)
stopsR5 <- stops
rm(stops)

#2 Walking Network
stops1 <- hopper_Network_car[,c("FromNode","FromX", "FromY")]
names(stops1) <- c("StopID","X","Y")
stops2 <- hopper_Network_car[,c("ToNode","FromX", "FromY")]
names(stops2) <- c("StopID","X","Y")
allEntries <- rbind(stops1, stops2)
stops <- allEntries[!(duplicated(allEntries$StopID)),]
rm(allEntries)
rm(stops1)
rm(stops2)
stopsHopper <- stops
rm(stops)

#Make sure that the stop ID's are different for both sets: 
stopsHopper$StopID <- paste0("H", as.character(stopsHopper$StopID),sep="")
stopsR5$StopID <- paste0("R", as.character(stopsR5$StopID),sep="")

transitNetwork$FromStop <-   paste0("R", as.character(transitNetwork$FromStop),sep="")
transitNetwork$ToStop <-   paste0("R", as.character(transitNetwork$ToStop),sep="")
hopper_Network_car$FromNode <- paste0("H", as.character(hopper_Network_car$FromNode),sep="")
hopper_Network_car$ToNode <- paste0("H", as.character(hopper_Network_car$ToNode),sep="")

#Create stops from the slow network to match them to the R5 network
stops1 <- hopper_Network_slow[,c("FromNode","FromX", "FromY")]
names(stops1) <- c("StopID","X","Y")
stops2 <- hopper_Network_slow[,c("ToNode","FromX", "FromY")]
names(stops2) <- c("StopID","X","Y")
allEntries <- rbind(stops1, stops2)
stops_slow <- allEntries[!(duplicated(allEntries$StopID)),]
rm(stops1)
rm(stops2)

#_______________________________________________________________________________________________________________________________________________________________#
# #Create virtual transfer links to each node in the walk network within 50m from each stop in the transport 
# 
# Links_pttowalk <- transitNetwork[,c("FromStop", "FromX", "FromY", "ToStop", "ToX", "ToY")]
# Links_pttowalk[] <- 0
# Links_pttowalk[nrow(Links_pttowalk)+800000,] <- 0
# Links_pttowalk$ToStop <- as.character(Links_pttowalk$ToStop)
# 
# 
# a=1
# c=1
# for(a in 1:nrow(stopsR5)){
# 
#   aX=stopsR5[a,"X"]
#   aY=stopsR5[a,"Y"]
#   stopsA=c(aX,aY)
#   test <- unlist(lapply(seq_len(nrow(stops_slow)), function(i) distHaversine(c(stops_slow[i,"X"],stops_slow[i,"Y"]),stopsA)))
#   test <- as.data.frame(test)
# 
#   b=1
#   for(b in 1:nrow(test)){
#       if(test[b,1]<400){
#         Links_pttowalk[c, "FromStop"] <- as.character(stopsR5[a,"StopID"])
#         Links_pttowalk[c, "ToStop"] <- as.character(stops_slow[b,"StopID"])
#         Links_pttowalk[c, "FromX"] <- aX
#         Links_pttowalk[c, "FromY"] <- aY
#         Links_pttowalk[c, "ToX"] <- stops_slow[b,"X"]
#         Links_pttowalk[c, "ToY"] <- stops_slow[b,"Y"]
#         c=c+1
#       }
#     }
#   }
# 
# Links_pttowalk <- subset(Links_pttowalk, FromX>0)

# write.table(Links_pttowalk, "Links_pttowalk.txt", sep=",",row.names = FALSE, quote=FALSE,col.names = TRUE)
#IMPORTANT NOTE: THIS LINKS-PTTOWALK TABLE IS UNIDIRECTIONAL. IT IS NECESSARY TO MAKE IT BIDIRECTIONAL AFTERWARDS!!!!


#________________________________________________________________________________________________________________________________________________________________



# 
# dist <- function(x, y){
#   dt <- data.table(distHaversine(stopsR5[,2:3],c(x,y)))
#   return(stopsR5[which.min(dt$V1),"StopID"])}
# 
# closest2 <- as.data.frame(unlist(lapply(seq_len(nrow(stops_slow)), function(i) dist(stops_slow[i,"X"],stops_slow[i,"Y"]))))
# 
# names(closest2)[1] <- "StopID"
# closest <- left_join(closest2, stopsR5, "StopID")
# names(closest) <- c("To","ToX","ToY")
# names(stops_slow) <- c("From","FromX","FromY")
# 
# dir1 <- cbind(stops_slow, closest)
# #Links_pttowalk <- subset(dir1, FromY!=ToY | FromX!=ToX)
# 
# dir2 <- cbind(closest, stops_slow)
# names(dir2) <- names(dir1)
# 
# Links_pttowalk <- rbind(dir1,dir2)
Links_pttowalk <- read.table("Links_pttowalk.txt", sep=",", header=TRUE)

#IMPORTANT: MAKE IT BIDRECTIONAL!!!!!!!!!!!!!!
Links_pttowalk2 <- Links_pttowalk[,c("ToStop","ToX","ToY","FromStop","FromX","FromY")]
names(Links_pttowalk2) <- c("FromStop","FromX","FromY","ToStop","ToX","ToY")

Links_pttowalk <- rbind(Links_pttowalk,Links_pttowalk2)
rm(Links_pttowalk2)


#Now remove the ones that are too far away
Links_pttowalk$dist <- distHaversine(Links_pttowalk[,2:3],Links_pttowalk[,5:6])
Links_pttowalk <- subset(Links_pttowalk, dist<150)
Links_pttowalk$dist <- NULL

#_______________________________________________________________________________________________________________________________________________________________#
#Now standarize the stop number sequence for both neworks
# k <- c(1:nrow(stopsPT))
# stopsPT$NewID <-  k+10000000
# 
# l <- c(1:nrow(stopsHopper))
# stopsHopper$NewID <-  l
# stopsHopper$NewID <-  stopsHopper$NewID+max(stopsPT$NewID)
# 
# transitNetwork$FromStop <- stopsPT$NewID[match(unlist(transitNetwork$FromStop), stopsPT$FromNode)]
# transitNetwork$ToStop <- stopsPT$NewID[match(unlist(transitNetwork$ToStop), stopsPT$FromNode)]
# 
# hopper_Network_car$FromNode <- stopsHopper$NewID[match(unlist(hopper_Network_car$FromNode), stopsHopper$StopID)]
# hopper_Network_car$ToNode <- stopsHopper$NewID[match(unlist(hopper_Network_car$ToNode), stopsHopper$StopID)]
# 
# hopper_Network_slow$FromNode <- stopsHopper$NewID[match(unlist(hopper_Network_slow$FromNode), stopsHopper$StopID)]
# hopper_Network_slow$ToNode <- stopsHopper$NewID[match(unlist(hopper_Network_slow$ToNode), stopsHopper$StopID)]

#patterns <- read.table("patterns.txt", sep=";", header=TRUE)
#_______________________________________________________________________________________________________________________________________________________________#
#DONE WITH THE NETWORK PREPARATION NOW READ IN THE OBSERVATIONS BELOW::
#_______________________________________________________________________________________________________________________________________________________________#



rm(dir1)
rm(dir2)
rm(closest)
rm(closest2)



#Read in PT observations
#Create Edge sequence as observations for each transit leg
#_______________________________________________________________________________________________________________________________________________________________#
# 
# obs_pt <- read.table("observations_pt_zurich_transit.txt", sep=";", header=TRUE)
# 
# 
# obs_pt <- obs_pt[!(obs_pt$ID==-99),]
# 
# 
# 
# obs_pt$Links <- as.character(0)
# i=1
# for(i in 1:nrow(obs_pt)){
#   tripPattern = subset(patterns, TripID==as.character(obs_pt[i,"TripId"]))
#   
#   j=(obs_pt[i,"FromStop"])
#   for(j in (obs_pt[i,"FromStop"]):((obs_pt[i,"ToStop"])-1)){
#     
#     if(j==(obs_pt[i,"FromStop"])){
#       obs_pt[i,"Links"] <- paste0(tripPattern[j,"EdgeID"],sep="")
#     } else{
#       obs_pt[i,"Links"] <- paste(obs_pt[i,"Links"] ,tripPattern[j,"EdgeID"],sep=",")
#     }
#   }
# }
# 
# 
# transfers <- read.table("transferLinks.txt", sep=";", header=TRUE)
# transfers$FromStop <-   paste("R", as.character(transfers$FromStop*1000),sep="")
# 

#_______________________________________________________________________________________________________________________________________________________________#

#Now add the transferLinks and convert observations to a single row 
#_______________________________________________________________________________________________________________________________________________________________#

# obs_pt_test <- obs_pt
# o=2
# for(o in 2:nrow(obs_pt_test)){
#   if(obs_pt_test[o,"ID"]==obs_pt_test[(o-1), "ID"]){
#     first <- str_split(obs_pt_test[(o-1),"Links"], ",")
#     second <- str_split(obs_pt_test[(o),"Links"], ",")
#     a <- as.numeric(first[[1]][length(first[[1]])])
#     b <- as.numeric(second[[1]][1])
#     from <- transitNetwork$ToStop[transitNetwork$LinkID==a]
#     to <- transitNetwork$FromStop[transitNetwork$LinkID==b]
# 
#     c <- (transitNetwork$LinkID[transitNetwork$FromStop==from & transitNetwork$ToStop==to & transitNetwork$isTransfer==1])
#     obs_pt_test[o,"Links"] <- paste(c,obs_pt_test[o,"Links"],collapse=",", sep=",")
#   }
# }
# 
# 
# #Convert all observations of a single ID to a single row and write them to file
# final_observations_pt <- aggregate(Links ~ ID, data = obs_pt_test, paste, collapse = ",")
# final_observations_pt <- final_observations_pt[-(grep("NA", final_observations_pt$Links)),]
# final_observations_pt <- final_observations_pt[-(grep(",,", final_observations_pt$Links)),]
# final_observations_pt <- left_join(final_observations_pt, obs_pt[,c("ID","TransitLeg")],by="ID")
# final_observations_pt <- final_observations_pt[!duplicated(final_observations_pt$ID),]
# 
# #Now add the id's of destination stop at the beggining and end of the observations
# x=1
# final_observations_pt$lastStop <- 0
# for(x in 1:nrow(final_observations_pt)){
#   last <- str_split(final_observations_pt[x,"Links"], ",")
#   final_observations_pt[x,"lastStop"]  <-   transitNetwork$ToStop[transitNetwork$LinkID==as.numeric(last[[1]][length(last[[1]])])]
# }
# 
# final_observations_pt$lastStop2 <- final_observations_pt$lastStop
# final_observations_pt <- final_observations_pt[,c("ID", "TransitLeg","lastStop","Links","lastStop2")]
# final_observations_pt$Links <- gsub(",,", ",", final_observations_pt$Links)
# 
# write.table(final_observations_pt, "final_observations_pt_zurich.txt", sep=",",row.names = FALSE, quote=FALSE,col.names = FALSE)
  #_______________________________________________________________________________________________________________________________________________________________#


#________________________________________________________________________________________________________________________________________________________________________________
#Now standarize the stop number sequence for both neworks as well as create a bike network, walk network and car network
k <- c(1:nrow(stopsR5))
stopsR5$NewID <-  k+100000000

l <- c(1:nrow(stopsHopper))
stopsHopper$NewID <-  l
stopsHopper$NewID <-  stopsHopper$NewID+max(stopsR5$NewID)

m <- c(1:nrow(stops_slow))
stops_slow$NewID <-  m
stops_slow$NewID <-  stops_slow$NewID+max(stopsHopper$NewID)


stopsH <- stopsHopper[,c("NewID", "StopID")]
names(stopsH) <- c("NewID", "ID")
stopsH_slow <- stops_slow[,c("NewID", "StopID")]
names(stopsH_slow) <- c("NewID", "ID")
stopsR <- stopsR5[,c("NewID", "StopID")]
names(stopsR) <- c("NewID", "ID")

#Now the same for the links
#Transit network is already in place
links_transit <- as.data.frame(transitNetwork[,c("LinkID")])
links_transit$ID <- links_transit[,1]
names(links_transit) <- c("NewID", "ID")

#Create separate walk, car and bike networks from the hopper network:

links_bike <- as.data.frame(hopper_Network_slow[,c("LinkId")])
names(links_bike)[1] <- "ID"
links_bike$NewID <- c(1:nrow(links_bike))+max(links_transit$NewID)
links_bike <- links_bike[,c("NewID","ID")]

links_walk <- as.data.frame(hopper_Network_slow[,c("LinkId")])
names(links_walk)[1] <- "ID"
links_walk$NewID <- c(1:nrow(links_walk))+max(links_bike$NewID)
links_walk <- links_walk[,c("NewID","ID")]

Links_pttowalk$NewID <- c(1:nrow(Links_pttowalk))+max(links_walk$NewID)
lookup_pttowalk <- rbind(stopsH_slow,stopsR)
Links_pttowalk$FromStop <- lookup_pttowalk$NewID[match(unlist(Links_pttowalk$FromStop), lookup_pttowalk$ID)]
Links_pttowalk$ToStop <- lookup_pttowalk$NewID[match(unlist(Links_pttowalk$ToStop), lookup_pttowalk$ID)]


links_car <- as.data.frame(hopper_Network_car[,c("LinkId")])
names(links_car)[1] <- "ID"
links_car$NewID <- c(1:nrow(links_car))+max(Links_pttowalk$NewID)
links_car <- links_car[,c("NewID","ID")]


links_walk$ID <- as.character(links_walk$ID)
links_bike$ID <- as.character(links_bike$ID)

lookup_car <- rbind(stopsH,links_car)
lookup_walk <- rbind(stopsH_slow,links_walk)
lookup_bike <- rbind(stopsH_slow,links_bike)
lookup_pt <- rbind(stopsR,links_transit)


#NOW CREATE THE FINAL NETWORK

#Format for the links
#linkID, FromNode, ToNode, FromX, FromY, ToX, ToY, Length, TravelTime, isCar, isPT, isBike, isWalk, isTransferPT, isTransferMultimodal
car_network <- hopper_Network_car
car_network$Length <- ifelse(car_network$Length==0,2,car_network$Length) #in meters
car_network$TravelTime <- (car_network$Length/car_network$Freespeed)/60 #in seconds
car_network$isCar <- 1
car_network$isPT <- 0
car_network$isBike <- 0
car_network$isWalk <- 0
car_network$isTransferPT <- 0
car_network$isTransferMultimodal <- 0
car_network$isLongDistTrain <- 0
car_network$isInterRegTrain <- 0
car_network$isRegTrain <- 0
car_network$isSBahn <- 0
car_network$isTram <- 0
car_network$isBoat <- 0
car_network$isTelecabin <- 0
car_network$isFunicular <- 0
car_network$isCommunalTaxi <- 0
car_network$isBus <- 0
car_network$Headway <- 0

#Replace old ID's with new ones
lookup_car <- lookup_car %>% mutate_all(as.character)
car_network[1:3] <- car_network[1:3] %>% mutate_all(as.character)
car_network[,1:3] <- lookup_car$NewID[match(unlist(car_network[,1:3]), lookup_car$ID)]
net_car <- car_network[,c("LinkId","FromNode","ToNode","FromX","FromY","ToX","ToY","Length","TravelTime","Headway","isCar","isPT","isBike","isWalk","isTransferPT","isTransferMultimodal","isLongDistTrain", "isInterRegTrain", "isRegTrain", "isSBahn", "isBus", "isTram", "isBoat", "isTelecabin","isFunicular", "isCommunalTaxi")]

#Now Bike
bike_network <- hopper_Network_slow
bike_network$Length <- ifelse(bike_network$Length==0,2,bike_network$Length) #in meters
bike_network$TravelTime <- (bike_network$Length/5)/60 #in minutes #UPDATE WITH VALUES FROM MIKROZENSUS SPEEDS
bike_network$isCar <- 0
bike_network$isPT <- 0
bike_network$isBike <- 1
bike_network$isWalk <- 0
bike_network$isTransferPT <- 0
bike_network$isTransferMultimodal <- 0

bike_network$isLongDistTrain <- 0
bike_network$isInterRegTrain <- 0
bike_network$isRegTrain <- 0
bike_network$isSBahn <- 0
bike_network$isTram <- 0
bike_network$isBoat <- 0
bike_network$isTelecabin <- 0
bike_network$isFunicular <- 0
bike_network$isCommunalTaxi <- 0
bike_network$isBus <- 0
bike_network$Headway <- 0
summary(bike_network$TravelTime)


#Replace old ID's with new ones
lookup_bike <- lookup_bike %>% mutate_all(as.character)
bike_network[1:3] <- bike_network[1:3] %>% mutate_all(as.character)
bike_network[,1:3] <- lookup_bike$NewID[match(unlist(bike_network[,1:3]), lookup_bike$ID)]

net_bike <- bike_network[,c("LinkId","FromNode","ToNode","FromX","FromY","ToX","ToY","Length","TravelTime","Headway","isCar","isPT","isBike","isWalk","isTransferPT","isTransferMultimodal","isLongDistTrain", "isInterRegTrain", "isRegTrain", "isSBahn", "isBus", "isTram", "isBoat", "isTelecabin","isFunicular", "isCommunalTaxi")]

#Now Walk
walk_network <- hopper_Network_slow
walk_network$Length <- ifelse(walk_network$Length==0,2,walk_network$Length) #in meters
walk_network$TravelTime <- (walk_network$Length/1.3)/60 #in minutes#UPDATE WITH VALUES FROM MIKROZENSUS SPEEDS
walk_network$isCar <- 0
walk_network$isPT <- 0
walk_network$isBike <- 0
walk_network$isWalk <- 1
walk_network$isTransferPT <- 0
walk_network$isTransferMultimodal <- 0

walk_network$isLongDistTrain <- 0
walk_network$isInterRegTrain <- 0
walk_network$isRegTrain <- 0
walk_network$isSBahn <- 0
walk_network$isTram <- 0
walk_network$isBoat <- 0
walk_network$isTelecabin <- 0
walk_network$isFunicular <- 0
walk_network$isCommunalTaxi <- 0
walk_network$isBus <- 0
walk_network$Headway <- 0
#Replace old ID's with new ones
lookup_walk <- lookup_walk %>% mutate_all(as.character)
walk_network[1:3] <- walk_network[1:3] %>% mutate_all(as.character)
walk_network[,1:3] <- lookup_walk$NewID[match(unlist(walk_network[,1:3]), lookup_walk$ID)]

net_walk <- walk_network[,c("LinkId","FromNode","ToNode","FromX","FromY","ToX","ToY","Length","TravelTime","Headway","isCar","isPT","isBike","isWalk","isTransferPT","isTransferMultimodal","isLongDistTrain", "isInterRegTrain", "isRegTrain", "isSBahn", "isBus", "isTram", "isBoat", "isTelecabin","isFunicular", "isCommunalTaxi")]



#Now PT
transitNetwork$isCar <- 0
transitNetwork$isPT <- ifelse(transitNetwork$isTransfer==0, 1,0)
transitNetwork$isBike <- 0
transitNetwork$isWalk <- 0
transitNetwork$isTransferMultimodal <- 0
transitNetwork$Distance <- transitNetwork$Distance*1000 #to get distances in m as in other networks

#Replace old ID's with new ones
lookup_pt <- lookup_pt %>% mutate_all(as.character)
transitNetwork[1:3] <- transitNetwork[1:3] %>% mutate_all(as.character)
transitNetwork[,1:3] <- lookup_pt$NewID[match(unlist(transitNetwork[,1:3]), lookup_pt$ID)]

net_pt <- transitNetwork[,c("LinkID","FromStop","ToStop","FromX","FromY","ToX","ToY","Distance","TravelTime","Headway","isCar","isPT","isBike","isWalk","isTransfer","isTransferMultimodal","isLongDistTrain", "isInterRegTrain", "isRegTrain", "isSBahn", "isBus", "isTram", "isBoat", "isTelecabin","isFunicular", "isCommunalTaxi")]
names(net_pt) <- c("LinkId","FromNode","ToNode","FromX","FromY","ToX","ToY","Length","TravelTime","Headway","isCar","isPT","isBike","isWalk","isTransferPT","isTransferMultimodal","isLongDistTrain", "isInterRegTrain", "isRegTrain", "isSBahn", "isBus", "isTram", "isBoat", "isTelecabin","isFunicular", "isCommunalTaxi")



#Now Transfers between networks
Links_pttowalk$dist <- distHaversine(Links_pttowalk[,2:3], Links_pttowalk[,5:6])
Links_pttowalk$isCar <- 0
Links_pttowalk$isPT <- 0
Links_pttowalk$isBike <- 0
Links_pttowalk$isWalk <- 0
Links_pttowalk$isTransferMultimodal <- 1
Links_pttowalk$isTransferPT <- 0
Links_pttowalk$TravelTime <- 0
Links_pttowalk$isLongDistTrain <- 0
Links_pttowalk$isInterRegTrain <- 0
Links_pttowalk$isRegTrain <- 0
Links_pttowalk$isSBahn <- 0
Links_pttowalk$isTram <- 0
Links_pttowalk$isBoat <- 0
Links_pttowalk$isTelecabin <- 0
Links_pttowalk$isFunicular <- 0
Links_pttowalk$isCommunalTaxi <- 0
Links_pttowalk$isBus <- 0
Links_pttowalk$Headway <- 0

#Replace old ID's with new ones
#No need here its already done above

net_transfer <- Links_pttowalk[,c("NewID","FromStop","ToStop","FromX","FromY","ToX","ToY","dist","TravelTime","Headway","isCar","isPT","isBike","isWalk","isTransferPT","isTransferMultimodal","isLongDistTrain", "isInterRegTrain", "isRegTrain", "isSBahn", "isBus", "isTram", "isBoat", "isTelecabin","isFunicular", "isCommunalTaxi")]
names(net_transfer) <- c("LinkId","FromNode","ToNode","FromX","FromY","ToX","ToY","Length","TravelTime","Headway","isCar","isPT","isBike","isWalk","isTransferPT","isTransferMultimodal","isLongDistTrain", "isInterRegTrain", "isRegTrain", "isSBahn", "isBus", "isTram", "isBoat", "isTelecabin","isFunicular", "isCommunalTaxi")

FINAL_NETWORK <- rbind(net_pt, net_bike, net_walk, net_transfer,net_car)


#NETWORK CREATED
#________________________________________________________________________________________________________________________________________

#Read in observations
#_______________________________________________________________________________________________________________________________________________________________#

data <- "final_observations_pt_zurich.txt"
obs_pt_final <- read.table(data, header = FALSE, sep = ",", col.names = paste0("V",seq_len(max(count.fields(data, sep = ',')))), fill = TRUE)


data <- "observations_pt_zurich_foot.txt"
obs_foot <- read.table(data, header = FALSE, sep = ",", 
                       col.names = paste0("V",seq_len(max(count.fields(data, sep = ',')))), fill = TRUE)

data <- "observations_pt_zurich_bike.txt"
obs_bike <- read.table(data, header = FALSE, sep = ",", 
                       col.names = paste0("V",seq_len(max(count.fields(data, sep = ',')))), fill = TRUE)

data <- "observations_pt_zurich_car.txt"
obs_car <- read.table(data, header = FALSE, sep = ",", 
                      col.names = paste0("V",seq_len(max(count.fields(data, sep = ',')))), fill = TRUE)


obs_foot <- obs_foot[!(obs_foot$V2==-99),]
obs_car <- obs_car[!(obs_car$V2==-99),]
obs_bike <- obs_bike[!(obs_bike$V2==-99),]


#Add empty columns to all other observations than car so that we can rbind them. 
n <- ncol(obs_car)
append = data.frame(matrix(NA, ncol=((n-ncol(obs_foot)):n), nrow=nrow(obs_foot)))
obs_foot <- cbind(obs_foot, append)

append = data.frame(matrix(NA, ncol=((n-ncol(obs_bike)):n), nrow=nrow(obs_bike)))
obs_bike <- cbind(obs_bike, append)

append = data.frame(matrix(NA, ncol=((n-ncol(obs_pt_final)):n), nrow=nrow(obs_pt_final)))
obs_pt_final <- cbind(obs_pt_final, append)

rm(append)

#Randomly remove 50% of rows from car, bike and walk datasets
obs_car <- obs_car[sample(nrow(obs_car), nrow(obs_car)/2), ]
obs_bike <- obs_bike[sample(nrow(obs_bike), nrow(obs_bike)/2), ]



colnames <- c(1:ncol(obs_car))
colnames <- paste0("V", colnames, sep="")
colnames(obs_car) <- colnames
colnames(obs_foot) <- colnames
colnames(obs_bike) <- colnames
colnames(obs_pt_final) <- colnames


#Now substitute all the old ID's by new ID's
lookup_car <- lookup_car %>% mutate_all(as.character)
obs_car <- obs_car %>% mutate_all(as.character)
obs_car[,c(3:ncol(obs_car))] <- lookup_car$NewID[match(unlist(obs_car[,c(3:ncol(obs_car))]), lookup_car$ID)]

lookup_walk <- lookup_walk %>% mutate_all(as.character)
obs_foot <- obs_foot %>% mutate_all(as.character)
obs_foot[,c(3:ncol(obs_foot))] <- lookup_walk$NewID[match(unlist(obs_foot[,c(3:ncol(obs_foot))]), lookup_walk$ID)]

lookup_bike <- lookup_bike %>% mutate_all(as.character)
obs_bike <- obs_bike %>% mutate_all(as.character)
obs_bike[,c(3:ncol(obs_bike))] <- lookup_bike$NewID[match(unlist(obs_bike[,c(3:ncol(obs_bike))]), lookup_bike$ID)]

lookup_pt <- lookup_pt %>% mutate_all(as.character)
obs_pt_final <- obs_pt_final %>% mutate_all(as.character)
obs_pt_final[,c(3:ncol(obs_pt_final))] <- lookup_pt$NewID[match(unlist(obs_pt_final[,c(3:ncol(obs_pt_final))]), lookup_pt$ID)]



#Now prepare and join all the observations
obs_car$mode <- "car"
obs_foot$mode <- "foot"
obs_bike$mode <- "bike"
obs_pt_final$mode <- "pt"

col_idx <- grep("mode", names(obs_car))
obs_car <- obs_car[, c(col_idx, (1:ncol(obs_car))[-col_idx])]
obs_foot <- obs_foot[, c(col_idx, (1:ncol(obs_foot))[-col_idx])]
obs_bike <- obs_bike[, c(col_idx, (1:ncol(obs_bike))[-col_idx])]
obs_pt_final <- obs_pt_final[, c(col_idx, (1:ncol(obs_pt_final))[-col_idx])]

#Join all
observations <- rbind(obs_car, obs_foot, obs_bike, obs_pt_final)
observations[,2:ncol(observations)] <- observations[,2:ncol(observations)] %>% mutate_all(as.numeric)
observations <- observations[order(observations$V1,observations$V2),]

#Complement the ones without the last node 
b=1
for(b in 1:nrow(observations)){
  if(is.na(observations[b,"V3"])){
    
    x <- as.numeric(FINAL_NETWORK$ToNode[FINAL_NETWORK$LinkId==tail(observations[b,][!is.na(observations[b,])], 1)])
    observations[b,"V3"] <- x
    observations[b,min(which(is.na(observations[b,])))] <- x
  }
}

names(observations)[2] <- "ID"

# #Check if car and bike are actually available for decision makers. 
# load("zielpersonen.rda")
# zielpersonen <- newfile
# rm(newfile)
# wege <- read.table("wege_final.txt", sep=" ", header=TRUE)
# 
# wege$HHNR <- as.numeric(wege$HHNR)
# zielpersonen$HHNR <- as.numeric(zielpersonen$HHNR)
# 
# wege <- left_join(wege, zielpersonen[,c("HHNR", "car_available")],"HHNR")
# observations <- left_join(observations, wege[,c("ID","car_available")],"ID")
# 
# col_idx <- grep("car_available", names(observations))
# observations <- observations[, c(col_idx, (1:ncol(observations))[-col_idx])]
# 
# 
# observations$car_available <- ifelse(observations$car_available!=0 | observations$car_available!=1, observations$car_available, 1)
# 
# count(observations, is.na(car_available))
# observations$car_available <- ifelse(is.na(observations$car_available), 1, observations$car_available)
# 
# count(observations, car_available)
# 


#__________________________________________________________________________________________________________________________________________________________________
#Remove all legs of paths which were partly unable to be routed. 
# 
# # #Make sure the right router_input file is read in!
# router_input <-read.table("input_MULTIMODAL_ZH_FINAL.txt", sep=";",header=TRUE)
# router_input$Mode
# router_car <- subset(router_input, Mode==7)
# 
# router_car$count <- str_count(router_car$V_X, ",")+1
# mean(router_car$count)
# 
# count(router_input, Mode>7)
# 
# 
# router_input$legcount <- ave(c(1:nrow(router_input)),router_input$ID, FUN=seq_along)
# router_input <- data.table(router_input)
# router_input[,Freq1 := length(unique(legcount)), by = ID]
# router_input <- data.frame(router_input)
# 
# 
# observations$legcount2 <- ave(c(1:nrow(observations)),observations$V1, FUN=seq_along)
# observations <- data.table(observations)
# observations[,Freq2 := length(unique(legcount2)), by = V1]
# observations <- data.frame(observations)
# 
# router_input <- router_input[!duplicated(router_input$ID),]
# 
# names(observations)[2] <- "ID"
# observations <- dplyr::left_join(observations, router_input[,c("ID", "Freq1")],by="ID")
# 
# #Delete observations with different amount of etappen
# observations <- observations[!(observations$Freq1 != observations$Freq2),]
# 
# test <- subset(observations, mode=="pt")
# #Now we have 6009 legs out of a total of 11058 which were used as an input to the file.
# 
# observations$legcount2 <- NULL
# observations$Freq1 <- NULL
# observations$Freq2 <- NULL
#__________________________________________________________________________________________________________________________________________________________________

observations$Links <- NULL
#Now format the observations file so that different legs of the same ID are merged into one observation line which represents one path
paster <- function(x)  { 
  paste0(head(x[5:length(x)][!is.na(x[5:(length(x))])],-1),collapse=",")
}

pastedobs <- apply(observations, 1, paster)

observations$Links <- pastedobs

col_idx <- grep("Links", names(observations))
observations <- observations[, c(col_idx, (1:ncol(observations))[-col_idx])]


o=2
for(o in 2:nrow(observations)){
  if(observations[o,"ID"]==observations[(o-1), "ID"]){
    first <- str_split(observations[(o-1),"Links"], ",")
    second <- str_split(observations[(o),"Links"], ",")
    a <- as.numeric(first[[1]][length(first[[1]])])
    b <- as.numeric(second[[1]][1])
    from <- FINAL_NETWORK$ToNode[FINAL_NETWORK$LinkId==a ]
    to <- FINAL_NETWORK$FromNode[FINAL_NETWORK$LinkId==b]
    c <- as.numeric(FINAL_NETWORK$LinkId[FINAL_NETWORK$FromNode==from & FINAL_NETWORK$ToNode==to & FINAL_NETWORK$isTransferMultimodal==1])
    
    observations[o,"Links"] <- base::paste(c,observations[o,"Links"],sep=",")
    
  }
}



obs_estimation <- aggregate(Links ~ ID, data = observations, paste, collapse = ",")
obs_estimation <- obs_estimation[-(grep(",,", obs_estimation$Links)),]

x=1
obs_estimation$lastStop <- 0
for(x in 1:nrow(obs_estimation)){
  last <- str_split(obs_estimation[x,"Links"], ",")
  obs_estimation[x,"lastStop"]  <-   FINAL_NETWORK$ToNode[FINAL_NETWORK$LinkId==as.numeric(last[[1]][length(last[[1]])])]
}

obs_estimation$Links <- gsub(",,", ",", obs_estimation$Links)

 



#Now that we have everything in the right format, create destination links for the recursive logit estimation 
length(unique(obs_estimation$lastStop))

destinations <- as.data.frame(unique(obs_estimation$lastStop))
destinations$LinkId <- c(1:nrow(destinations))+max(as.numeric(FINAL_NETWORK$LinkId))
names(destinations)[1] <- "StopID"

obs_estimation$lastStop <- destinations$LinkId[match(unlist(obs_estimation$lastStop), destinations$StopID)]

obs_estimation$lastStop2 <- obs_estimation$lastStop
obs_estimation <- obs_estimation[,c("ID","lastStop","Links","lastStop2")]

#Save file with ID's to select paths for mode choice model 
ids <- as.data.frame(obs_estimation$ID)
names(ids) <- "ID"
write.table(ids, "ids_RL_Multimodal", sep=",",row.names = FALSE, quote=FALSE)



#Observations ready, now just write them to file and then add the destinations to the network file:
obs_estimation$ID <- NULL

destinations[,c("FromNode","ToNode","FromX","FromY","ToX","ToY","Length","TravelTime","Headway","isCar","isPT","isBike","isWalk","isTransferPT","isTransferMultimodal","isLongDistTrain", "isInterRegTrain", "isRegTrain", "isSBahn", "isBus", "isTram", "isBoat", "isTelecabin","isFunicular", "isCommunalTaxi",
                "isTrain", "isSteep", "PT_TT", "Bike_TT", "Walk_TT", "TransferPT_TT", "TransferMultimodal_TT", "Train_TT", "SBahn_TT", "Bus_TT", "Boat_TT", "Train_H", "SBahn_H","Bus_H", "Boat_H","Car_TT")] <- 0
destinations$FromNode <- destinations$StopID
FINAL_NETWORK$FromNode <- as.numeric(FINAL_NETWORK$FromNode)
FINAL_NETWORK$ToNode <- as.numeric(FINAL_NETWORK$ToNode)

destinations$ToNode <- c(1:nrow(destinations))+ max(FINAL_NETWORK$FromNode)
destinations$StopID <- NULL

#Format values in network:
FINAL_NETWORK$isTrain <- ifelse(FINAL_NETWORK$isLongDistTrain==1 | FINAL_NETWORK$isInterRegTrain==1 | FINAL_NETWORK$isRegTrain==1,1,0)
FINAL_NETWORK$isSteep <- ifelse(FINAL_NETWORK$isTelecabin==1 | FINAL_NETWORK$isFunicular==1,1,0)
FINAL_NETWORK$PT_TT <- FINAL_NETWORK$isPT*FINAL_NETWORK$TravelTime
FINAL_NETWORK$Bike_TT <- FINAL_NETWORK$isBike*FINAL_NETWORK$TravelTime
FINAL_NETWORK$Walk_TT <- FINAL_NETWORK$isWalk*FINAL_NETWORK$TravelTime
FINAL_NETWORK$TransferPT_TT <- FINAL_NETWORK$isTransferPT*FINAL_NETWORK$TravelTime
FINAL_NETWORK$TransferMultimodal_TT <- FINAL_NETWORK$isTransferMultimodal*FINAL_NETWORK$TravelTime
FINAL_NETWORK$Train_TT <- FINAL_NETWORK$isTrain*FINAL_NETWORK$TravelTime
FINAL_NETWORK$SBahn_TT <- FINAL_NETWORK$isSBahn*FINAL_NETWORK$TravelTime
FINAL_NETWORK$Bus_TT <- FINAL_NETWORK$isBus*FINAL_NETWORK$TravelTime
FINAL_NETWORK$Boat_TT <- FINAL_NETWORK$isBoat*FINAL_NETWORK$TravelTime
FINAL_NETWORK$Train_H <- FINAL_NETWORK$isTrain*FINAL_NETWORK$Headway
FINAL_NETWORK$SBahn_H <- FINAL_NETWORK$isSBahn*FINAL_NETWORK$Headway
FINAL_NETWORK$Bus_H <- FINAL_NETWORK$isBus*FINAL_NETWORK$Headway
FINAL_NETWORK$Boat_H <- FINAL_NETWORK$isBoat*FINAL_NETWORK$Headway
FINAL_NETWORK$Car_TT <- FINAL_NETWORK$isCar*FINAL_NETWORK$TravelTime

FINAL_NETWORK_DEST <- rbind(FINAL_NETWORK,destinations)
dest <- as.data.frame(nrow(destinations))


#Put cost in for car and pt
FINAL_NETWORK$Cost_car <- FINAL_NETWORK$isCar*FINAL_NETWORK$Length*0.13/1000 #the 0.13 is per km, therefore we divide length by 1000
FINAL_NETWORK$Cost_pt <- FINAL_NETWORK$isPT*4.4
FINAL_NETWORK$COST <- FINAL_NETWORK$Cost_car+FINAL_NETWORK$Cost_pt

FINAL_NETWORK$Tram_TT <- FINAL_NETWORK$isTram*FINAL_NETWORK$TravelTime
FINAL_NETWORK$Tram_H <- FINAL_NETWORK$isTram*FINAL_NETWORK$Headway


write.table(obs_estimation, "obs_multimodal_estimation_zh_reduced.txt", sep=",",row.names = FALSE, quote=FALSE,col.names = FALSE)
write.table(FINAL_NETWORK_DEST, "LinkAttributes_multimodal_complete_reduced.txt", sep=",", quote=FALSE, row.names = FALSE)
write.table(FINAL_NETWORK, "LinkAttributes_multimodal_estimation_reduced.txt", sep=",", quote=FALSE,row.names = FALSE)
write.table(dest, "destinations_multimodal_reduced.txt", sep=",", quote=FALSE, row.names = FALSE, col.names=FALSE)

#____________________________________________________________________________________________________________________________________
#THE END OF THE CODE
#____________________________________________________________________________________________________________________________________

wege1 <- subset(wege, ID %in% observations$ID)
count(wege1, w_verkehrsmittel_agg==3)

wege2 <- subset(wege, ID %in% obs_estimation$ID)
count(wege2, w_verkehrsmittel_agg==3)














# #Check if car and bike are actually available for decision makers. 
load("zielpersonen.rda")
zielpersonen <- newfile
rm(newfile)
wege <- read.table("wege_final.txt", sep=" ", header=TRUE)


wege$HHNR <- as.numeric(wege$HHNR)
zielpersonen$HHNR <- as.numeric(zielpersonen$HHNR)

sum(walk$w_rdist)

wege <- subset(wege, ID %in% obs_estimation$ID)

count(wege, w_verkehrsmittel)

count(wege, w_verkehrsmittel_agg)
load("etappen.rda")
etappen <- newfile
etappen <- subset(etappen, HHNR %in% wege$HHNR)
wege$ID2 <- paste0(wege$HHNR,wege$WEGNR, sep="")
etappen$ID2 <- paste0(etappen$HHNR,etappen$WEGNR, sep="")

wege$ID2 <- as.numeric(wege$ID2)
etappen$ID2 <- as.numeric(etappen$ID2)
etappen$ID2

etappen <- subset(etappen, ID2 %in% wege$ID2 )

etappen_car <- subset(etappen, ID2 %in% car$ID2)
etappen_pt <- subset(etappen, ID2 %in% pt$ID2)
etappen_bike <- subset(etappen, ID2 %in% bike$ID2)
etappen_walk <- subset(etappen, ID2 %in% walk$ID2)

nrow(etappen_car)/nrow(car)
nrow(etappen_pt)/nrow(pt)
nrow(etappen_bike)/nrow(bike)
nrow(etappen_walk)/nrow(walk)
nrow(etappen)/nrow(wege)


summary(etappen_pt$benutztes_verkehrsmittel)

count(etappen_pt, benutztes_verkehrsmittel)

rm(newfile)

wege$speed <- wege$w_rdist*1000/(wege$dauer1*60)
summary(wege$dauer2)
count(wege, dauer2==0)
wege$da

count(wege, w_zweck_weg_1)

summary(wege$car_available)

etappen$benutztes_verkehrsmittel
sum(walk_speed$dauer2)

wege_speed <- subset(wege, dauer1>0)
car_speed <- subset(wege_speed, w_verkehrsmittel_agg==1)
pt_speed <- subset(wege_speed, w_verkehrsmittel_agg==3)
walk_speed <- subset(wege_speed, w_verkehrsmittel==15)
bike_speed <- subset(wege_speed, w_verkehrsmittel==14)

car <- subset(wege, w_verkehrsmittel_agg==1)
pt <- subset(wege, w_verkehrsmittel_agg==3)
walk <- subset(wege, w_verkehrsmittel==15)
bike <- subset(wege, w_verkehrsmittel==14)

summary(car$car_available)
summary(bike$car_available)
summary(pt$car_available)

mean(walk_speed$speed)

car

nrow(bike)


count(etappen_pt, benutztes_verkehrsmittel)




count(walk, w_zweck_weg_3)
count(pt, w_zweck_weg_3)
count(car, w_zweck_weg_3)
count(bike, w_zweck_weg_1)


mean(pt$dauer2)

mean(wege$dauer2)

count(wege, w_verkehrsmittel_agg)
count(wege, w_verkehrsmittel)
#Velo 14, Walk 15
#Do some summary statistics and plots
ggplot(subset(wege, wmittela==1)) + aes(x=distance_bike/w_rdist)   + 
  geom_histogram(aes(y=..count../sum(..count..)), binwidth = 0.05) + theme_bw() + coord_cartesian(xlim=c(0,3))+
  theme(legend.title=element_blank(),legend.position="bottom")  +
  xlab("Routed path distance (Google) / Actual path distance (Mikrozensus)") + ylab("Density") + scale_y_continuous(labels=percent)

