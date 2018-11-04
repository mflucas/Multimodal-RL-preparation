#setwd("C:/Users/mlucas/polybox/Masterarbeit/Multimodal_Estimation/Data_preparation")
setwd("/Users/lucasmeyer/polybox/Masterarbeit/Multimodal_Estimation/Data_preparation")

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
library(ggplot2)
library(scales)


rm(list = ls())

#rawData <- read.table("Pt_edges_large.txt", sep=";", header=TRUE)
rawData2 <- read.table("Pt_edges_large2.txt", sep=";", header=TRUE)

#Reduce GTFS data to study area

#The delimited area for the transit Network has to be larger than the area for the observations
# rawData <- subset(rawData2, FromX<8.8866 & FromX>8.1876 & FromY<47.5265& FromY>47.1533)
# rawData <- subset(rawData, ToX<8.8866 & ToX>8.1876 & ToY<47.5265 & ToY>47.1533)
rawData <- subset(rawData2, FromX<8.6541 & FromX>8.3019 & FromY<47.4940& FromY>47.2960)
rawData <- subset(rawData, ToX<8.6541 & ToX>8.3019 & ToY<47.4940 & ToY>47.2960)
#Delete Duplicated edges

#uniqueRoutes <- rawData[!(duplicated(rawData[c("RouteID","DirectionID","FromStop","ToStop")])),]
#TODO:: route transit pairs in zurich to find transit time in links

#Create the simplified network
#_______________________________________________________________________________________________________________________________________________________________#


averageTT <- ddply(rawData, .(RouteID, DirectionID, FromStop, ToStop, Type),summarize,TravelTime=min(TravelTime),FromX=mean(FromX), FromY=mean(FromY), ToX=mean(ToX), ToY=mean(ToY), Edge=mean(Edge), Headway=mean(Headway))
#Transform the dynamic transit schedule to a static one, that is allow for only one link for each pair of Stops, by direction and RouteID. 
summary(averageTT$TravelTime)

length(unique(averageTT$RouteID))
#Add traveltimes for null values of travel time. 
averageTT <- as.data.table(averageTT)
averageTT[ , Distance := distGeo(matrix(c(FromX, FromY), ncol = 2), 
                         matrix(c(ToX, ToY), ncol = 2))]
averageTT <- as.data.frame(averageTT)
 
averageSpeed <- mean(averageTT$Distance[averageTT$TravelTime>0]/ averageTT$TravelTime[averageTT$TravelTime>0])
 
averageTT = within(averageTT, {TravelTime = ifelse(TravelTime==0, 1.2*Distance/averageSpeed, TravelTime)})

#Write out the network file
#_______________________________________________________________________________________________________________________________________________________________#


#Now give these patterns to the rawData, because the observations from R5 only have a TripID, note RouteID, so that 
#this can be used to look up which links were exactly taken. 
#_______________________________________________________________________________________________________________________________________________________________#
k <- c(1:nrow(averageTT))
averageTT$EdgeID <- k

patterns <- left_join(rawData, averageTT[,c("FromStop","ToStop","RouteID","DirectionID","EdgeID")], by=c("FromStop","ToStop","RouteID","DirectionID"))
#_______________________________________________________________________________________________________________________________________________________________#

#Now create stop ID's, which will be used for the creation of the transfer links
#ATTENTION: The code below takes a long time to run (30min for around 1600 stops). This is why this code is usually saved to file
#_______________________________________________________________________________________________________________________________________________________________#
stops1 <- averageTT[,c("FromStop","FromX", "FromY")]
names(stops1) <- c("StopID","X","Y")
stops2 <- averageTT[,c("ToStop","ToX", "ToY")]
names(stops2) <- c("StopID","X","Y")
allEntries <- rbind(stops1, stops2)

stops <- allEntries[!(duplicated(allEntries$StopID)),]
allEntries <- NULL
stops1 <- NULL
stops2 <- NULL
# 
# transfers <- averageTT
# transfers$TransferDistance <- 0
# 
# #Expand the table several times
# transfers <- rbind(transfers,transfers)
# transfers <- rbind(transfers,transfers)
# transfers <- rbind(transfers,transfers)
# transfers <- rbind(transfers,transfers)
# transfers <- rbind(transfers,transfers)
# transfers <- rbind(transfers,transfers)
# transfers <- rbind(transfers,transfers)
# transfers <- rbind(transfers,transfers)
# transfers <- rbind(transfers,transfers)
# 
# 
# #Create transferLinks
# 
# c=1
# a=1
# for(a in 1:nrow(stops)){
#   
#   aX=stops[a,"X"]
#   aY=stops[a,"Y"]
#   stopsA=c(aX,aY)
# test <- unlist(lapply(seq_len(nrow(stops)), function(i) distGeo(c(stops[i,"X"],stops[i,"Y"]),stopsA)))
# test <- as.data.frame(test)
# 
# b=1
# for(b in 1:nrow(test)){
#     if(test[b,1]<400){
#       transfers[c, "RouteID"] <- as.character("Transfer")
#       transfers[c, "DirectionID"] <- as.character("Transfer")
#       transfers[c, "FromStop"] <- stops[a,"StopID"]
#       transfers[c, "ToStop"] <- stops[b,"StopID"]
#       transfers[c, "TravelTime"] <- 1.2*test[b,1]/1.4+120
#       transfers[c, "FromX"] <- aX
#       transfers[c, "FromY"] <- aY
#       transfers[c, "ToX"] <- stops[b,"X"]
#       transfers[c, "ToY"] <- stops[b,"Y"]
#       transfers[c, "TransferDistance"] <- test[b,1]
#       c=c+1
#     }
#   }
# }
# 
# transfers <- subset(transfers, is.na(RouteID))
# transfers <- subset(transfers, FromStop!=ToStop)
# 
# 
# 
# #Create full network(transit links + transfer links)
# transfers$Edge <- NULL
# averageTT$Edge <- NULL
# 
# 
# transfers$isTransfer <- as.numeric(1)
# averageTT$isTransfer <- 0
# transfers$RouteID <- "Transfer"
# 


# 
# 
# write.table(transfers, "transferLinks.txt", row.names = FALSE, sep=";", quote=FALSE)
transfers <- read.table("transferLinks.txt", sep=";", header=TRUE)

transfers_same <- transfers
transfers_same <- transfers_same[!(duplicated(transfers_same$FromStop)),]
transfers_same$ToStop <- transfers_same$FromStop
transfers_same$TravelTime <- 2
transfers_same$ToX <- transfers_same$FromX
transfers_same$ToY <- transfers_same$FromY
transfers_same$TransferDistance <- 0
transfers_same$isTransfer <- 1

transfers <- rbind(transfers, transfers_same)
rm(transfers_same)


#Now add the transferLinks to the Pt Links to create the final transit network
averageTT$isTransfer <- 0
transfers$Headway <- 0

a <- c(1:nrow(transfers))
transfers$EdgeID <- a
transfers$EdgeID <- transfers$EdgeID + max(averageTT$EdgeID)
transfers$Type <- 1

pt_network <- averageTT[,c("EdgeID","FromStop", "ToStop", "FromX","FromY","ToX","ToY","TravelTime","Distance","Headway", "Type")]
transfer_network <- transfers[,c("EdgeID","FromStop", "ToStop", "FromX","FromY","ToX","ToY","TravelTime","TransferDistance","Headway", "Type")]
colnames(transfer_network)[9] <- "Distance"

edgeList <- averageTT[,c("EdgeID","FromX","FromY","ToX","ToY")]
edgeListTransfers <- transfers[,c("EdgeID","FromX","FromY","ToX","ToY")]

#Quick plot of the network
#First convert latlon to UTM to maintain true distances in plot
LongLatToUTM<-function(x,y,zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
  return(as.data.frame(res))
}
fromUTM <- LongLatToUTM(edgeList$FromX, edgeList$FromY, "32T")
toUTM <- LongLatToUTM(edgeList$ToX, edgeList$ToY, "32T")
edgeList$FromX <- fromUTM$X
edgeList$FromY <- fromUTM$Y
edgeList$ToX <- toUTM$X
edgeList$ToY <- toUTM$Y

#Plot the map. Add background in illustrator.
plot(edgeList$FromX, edgeList$FromY, cex=0.3)
segments(edgeList$FromX, edgeList$FromY, edgeList$ToX, edgeList$ToY, lwd=0.7)
segments(edgeListTransfers$FromX, edgeListTransfers$FromY, edgeListTransfers$ToX, edgeListTransfers$ToY, col="red")


plot <- subset(network, Type>1)
plot = within(plot, {mode = ifelse(Type==1, "Transfer",
                                ifelse(Type==102, "LongDistTrain",
                                       ifelse(Type==103, "InterRegTrain",
                                              ifelse(Type==106,"RegTrain",
                                                     ifelse(Type==400 | Type==100, "CommuterTrain",
                                                            ifelse(Type==700, "Bus",
                                                                   ifelse(Type==900, "Tram",
                                                                          ifelse(Type==1000,"Boat",
                                                                                 ifelse(Type==1300,"Telecabin",
                                                                                       ifelse(Type==1400,"Funicular", "CommunalTaxi"
                                                                   ))))))))))})

ggplot(plot)+ aes(x=mode, group=mode)  + 
  stat_count(aes(y=..count..), binwidth = 1) + theme_bw() + #coord_cartesian(xlim=c(0,20))+
  theme(legend.title=element_blank(),legend.position="bottom")  +
  xlab("Mode") + ylab("Edges") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
rm(plot)



#Now save all non-transfer links to a file to route on R5 and get the real headways
transitNetwork <- averageTT[,c("EdgeID", "FromX", "FromY", "ToX", "ToY","TravelTime")]
write.table(transitNetwork, "network_input_R5.txt", sep=";",row.names = FALSE, quote=FALSE)



#___________________________________________________________________________________________________________________________
#Route the file above in the headwayWriter class in Java. First for morning peak:
# profileRequest.fromTime=27000;7:30am
# profileRequest.toTime = 30600;


#___________________________________________________________________________________________________________________________

#Check headways
headways <- read.table("headways.txt", header= TRUE, sep=";")
headways_midday <- read.table("headways_midday.txt", header= TRUE, sep=";")
test <- subset(headways,TravelTime>0)

ggplot(test)+ aes(x=increase)  + 
  geom_histogram(aes(y=..count../sum(..count..)), binwidth = 0.1) + theme_bw() + coord_cartesian(xlim=c(0,2.2))+
  theme(legend.title=element_blank(),legend.position="bottom")  +
  xlab("% difference between off-peak to peak headways") + ylab("Frequency") + scale_y_continuous(labels=percent)


#Now add headways and updated travel times to the network
pt_network$TravelTime <- NULL
pt_network$Headway <- NULL
pt_network <- left_join(pt_network, headways[,c("EdgeID", "TravelTime", "Headway")],"EdgeID")

pt_network = within(pt_network, {TravelTime = ifelse(TravelTime==0, 60,TravelTime)})#Force travel time to 1min if it is 0

#___________________________________________________________________________________________________________________________


#Create transport network
network <- rbind(pt_network,transfer_network)

#Now scale the variables

network$TravelTime <- network$TravelTime/60 #to get travel time in minutes
network$Headway <- network$Headway/60 #headways also in minutes
network$Distance <- network$Distance/1000 #distance in km


#Now add the type of the transit route
network$isTransfer <- ifelse(network$Type==1,1,0)
network$isLongDistTrain <- ifelse(network$Type==102,1,0)
network$isInterRegTrain <- ifelse(network$Type==103,1,0)
network$isRegTrain <- ifelse(network$Type==106,1,0)
network$isSBahn <- ifelse(network$Type==400 | network$Type==100,1,0) #Almost all 100 types are SBahn, specially in the smaller area within the city
network$isBus <- ifelse(network$Type==700,1,0)
network$isTram <- ifelse(network$Type==900,1,0)
network$isBoat <- ifelse(network$Type==1000,1,0)
network$isTelecabin <- ifelse(network$Type==1300,1,0)
network$isFunicular <- ifelse(network$Type==1400,1,0)
network$isCommunalTaxi <- ifelse(network$Type==1501,1,0)


#Make sure stop ids are unique

network$FromStop <- network$FromStop*1000
network$ToStop <- network$ToStop*1000
colnames(network)[1] <- "LinkID"

write.table(network, "network_pt_zurich_coords.txt", sep=",",row.names = FALSE, quote=FALSE)

#_______________________________________________________________________________________________________________________________________________________________#

#Read in observations
#Create Edge sequence as observations for each transit leg
#_______________________________________________________________________________________________________________________________________________________________#

observations <- read.table("observations_pt_zurich.txt", sep=";", header=TRUE)

observations <- observations[!(observations$ID==-99),]

#Make stop ID from observations match the ID from the network (Java always starts indexes with 0)
observations$FromStop <- observations$FromStop
observations$ToStop <- observations$ToStop
observations$Links <- as.character(0)


i=1
for(i in 1:nrow(observations)){
  tripPattern = subset(patterns, TripID==as.character(observations[i,"TripId"]))
  
  j=(observations[i,"FromStop"])
  for(j in (observations[i,"FromStop"]):(observations[i,"ToStop"])-1){
    
    if(j==(observations[i,"FromStop"])){
      observations[i,"Links"] <- paste0(tripPattern[j,"EdgeID"],sep="")
    } else{
    observations[i,"Links"] <- paste(observations[i,"Links"] ,tripPattern[j,"EdgeID"],sep=",")
  }
  }
}


#Now add the transferLinks and convert observations to a single row 
#_______________________________________________________________________________________________________________________________________________________________#
o=2
for(o in 2:nrow(observations)){
  if(observations[o,"ID"]==observations[(o-1), "ID"]){
    first <- str_split(observations[(o-1),"Links"], ",")
    second <- str_split(observations[(o),"Links"], ",")
    a <- as.numeric(first[[1]][length(first[[1]])])
    b <- as.numeric(second[[1]][1])
    from <- averageTT$ToStop[averageTT$EdgeID==a]
    to <- averageTT$FromStop[averageTT$EdgeID==b]
    c <- as.numeric(transfers$EdgeID[transfers$FromStop==from & transfers$ToStop==to])
    
    observations[o,"Links"] <- paste(c,observations[o,"Links"],collapse=",", sep=",")

  }
}


#Convert all observations of a single ID to a single row and write them to file
final_observations <- aggregate(Links ~ ID, data = observations, paste, collapse = ",")
final_observations <- final_observations[-(grep("NA", final_observations$Links)),]

#Now add the id's of destination stop at the beggining and end of the observations
x=1
final_observations$lastStop <- 0
for(x in 1:nrow(final_observations)){
  last <- str_split(final_observations[x,"Links"], ",")
  final_observations[x,"lastStop"]  <-   network$ToStop[network$LinkID==as.numeric(last[[1]][length(last[[1]])])]
}

final_observations$lastStop2 <- final_observations$lastStop
final_observations <- final_observations[,c("ID", "lastStop","Links","lastStop2")]
final_observations$Links <- gsub(",,", ",", final_observations$Links)


write.table(final_observations, "final_observations_pt_zurich.txt", sep=",",row.names = FALSE, quote=FALSE,col.names = FALSE)
#_______________________________________________________________________________________________________________________________________________________________#



#_______________________________________________________________________________________________________________________________________________________________#

#Now complete data formatting to match the one required by the RL estimator by adding destination links
#First clean workspace
rm(list = ls())
network <- read.table("network_pt_zurich_coords.txt", sep=",", header=TRUE)

data <- "final_observations_pt_zurich.txt"
observations <- read.table(data, header = FALSE, sep = ",", col.names = paste0("V",seq_len(max(count.fields(data, sep = ',')))), fill = TRUE)


#ENSURE CONSISTENCY
#This is to make sure that all of the links in the observations are indeed in the link attributes file and deletes observations which are not. 
# modifier <- observations
# modifier[cbind(1:nrow(modifier), max.col(!is.na(modifier), 'last'))] <- NA
# #modifier$V300 <- NA
# 
# alll <- function(x)   ifelse(all(x[3:length(x)][!is.na(x[3:length(x)])] %in% network$LinkId)==FALSE,x[1],0)
# problems <- apply((modifier), 1, alll)
# problems <- as.data.frame(problems)
# problems <- subset(problems, problems>0)
# observations <- observations[!(observations$V1 %in% problems$problems),]
# 


#Create Destination Links
lastValues <- function(x)   tail(x[!is.na(x)], 2)
destinations <- apply(observations, 1, lastValues)
destinations <- as.data.frame(destinations)
destinations <- transpose(destinations)



colnames(destinations) <- c("LinkID","ToStop")
destinations$FromStop <- destinations$ToStop
destinations <- destinations[!(duplicated(destinations$ToStop)),]

destinations$temp <- 1:nrow(destinations)
destinations$ToStop <- destinations$temp+max(network[,c("FromStop","ToStop")])
destinations$temp <- NULL

destinations$LinkID <- 1:nrow(destinations)
destinations$LinkID <- destinations$LinkID+max(network$LinkID)


destinations[,c("Distance", "Headway", "TravelTime", "Type", "isTransfer","isLongDistTrain", "isRegTrain", "isSBahn", "isBus", "isTram", "isBoat", "isTelecabin", "isFunicular", "isCommunalTaxi")] <- 0



replace <- destinations[,c("LinkID", "FromStop")]
repNet <- network["LinkID"]
repNet$FromStop <- repNet$LinkID

replace <- rbind(replace, repNet)

observations[] <- replace$LinkID[match(unlist(observations), replace$FromStop)]

observations <- observations[,2:ncol(observations)]
observations[is.na(observations)] <- 0

network <- network[,c("LinkID","FromStop", "ToStop","TravelTime","Distance","Headway", "Type", "isTransfer","isLongDistTrain", "isRegTrain", "isSBahn", "isBus", "isTram", "isBoat", "isTelecabin", "isFunicular", "isCommunalTaxi")]
destinations <- destinations[,c("LinkID","FromStop", "ToStop","TravelTime","Distance","Headway", "Type", "isTransfer","isLongDistTrain", "isRegTrain", "isSBahn", "isBus", "isTram", "isBoat", "isTelecabin", "isFunicular", "isCommunalTaxi")]

network_final <- rbind(network, destinations)
dest <- as.data.frame(nrow(destinations))



write.table(observations, "observations_estimation_pt.txt", sep=",", quote=FALSE, row.names = FALSE, col.names=FALSE)
write.table(network_final, "LinkAttributes_complete.txt", sep=",", quote=FALSE, row.names = FALSE)
write.table(network, "LinkAttributes_estimation.txt", sep=",", quote=FALSE,row.names = FALSE)
write.table(dest, "destinations.txt", sep=",", quote=FALSE, row.names = FALSE, col.names=FALSE)
#_______________________________________________________________________________________________________________________________________________________________#

