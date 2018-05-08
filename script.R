# Key Issues (Ranked):
# - Running Asymmetric TSP, since routes will be visually plotted. Need to do sanity check on route
# - Error handling: i) geocode(), if there is no internet connection
# - Several methods are not responding to explicit start point: arbitrary_insertion, repetitive_nn, two_opt. Not major issue since it is a loop; just need to tweak plotting
# - In route(), structure should be "route" or "leg"?

# Broader Issues:
# - What if user inputs Gardens by the Bay and Cloud Forest, or Zoo and Night Safari (i.e. when they are just right next to each other/ referring to the same thing)
# - Have option to suggest hotel based on location

# Test Issues:
# - Limit the number of attractions in a cluster(day) to force higher clustering where needed
# - Code change: allow easy switching from automated cluster to manual cluster
# - Output stats on the clustering technique, the mean, SD, full list, graph
# - Check why Singapore City Gallery lat/lon is NA - is there repeat call in the API?

library(ggmap)
library(googleway)
library(Rcpp)
library(sp)
library(cluster)
library(fpc)
library(combinat)
library(Matrix)
library(TSP)
library(RCurl)
library(jsonlite)
library(dplyr)
library(leaflet)
options(scipen=999)




# #Upcoming feature: If no accommodation, find centoid, and pass on to hotel/airbnb search
# test <- as.numeric(geocode("1.306928, 103.898943"))
# test2 <- revgeocode(test)

city <- "Singapore" #User needs to input city, and only needs to enter the POI name 
transportMode <- "walking" #walking or driving
clusterOption <- 'a' #a or m, for auto or manual identification of clusters
manual.clusterNum.input <- 4 #If manual cluster selected, how many clusters does user want
poi.limit <- 6 #Number of places one can visit in a day

# csv.data <- read.csv("home_poi.csv", header = FALSE, stringsAsFactors = FALSE)[[1]] #Just update csv file directly for testing; first row taken as home
# home <- csv.data[1]
# combined <- csv.data
# ALTERNATIVELY
home <- c("161 Haig Road")
poi <- c("Katong V", "Parkway Parade", "Marine Parade CC")
#poi <- c("Katong V", "Parkway Parade", "Marine Parade CC", "City Hall", "Esplanade", "Marina Bay Sands", "Vivocity", "Sentosa")
# poi <- c("Parkway Parade", "Katong V", "Marine Parade CC")
combined <- c(home, poi)


all <- unlist(lapply(combined, FUN=function(x){paste(x, ", ", city, sep = "")}))


api_key <- readline(prompt="Paste API key: ")
register_google(key = api_key)
                
# ---- RUN PHASE 1: Cluster ----

all.df <- as.data.frame(all)
colnames(all.df) <- "Name"
geocodes <- geocode(all)
geocodes2 <- cbind(all.df, geocodes)


#Initial plot using Leaflet (optional)
p <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=geocodes2$lon, lat=geocodes2$lat, popup=geocodes2$Name)
p  # P

#Initial plot using ggmap (optional)
# plot(geocodes2$lon, geocodes2$lat, xlab="lon",ylab="lat")
# sbbox <- make_bbox(lon = geocodes2$lon, lat = geocodes2$lat, f=3) 
# sbbox
# sq_map <- get_map(location = sbbox, maptype = "roadmap", source = "google")
# ggmap(sq_map) + geom_point(data = geocodes2, mapping = aes(x = lon, y = lat), color = "red", size=2)

# #Old code: Use geocode() to obtain lat long
# master <- read.csv("master_coordinates.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
# master.c <- cbind(master, geocode(master$Name))
# par(pty="s")
# plot(master.c$lon, master.c$lat, xlab="lon",ylab="lat")


# Cluster (exclude home)
geocodes3 <- geocodes2[-1,] #Remove home (assumes first row is always home)
rownames(geocodes3) <- geocodes3[,1] #prepare data into right format to feed into kmeans() #converts column into row name
geocodes4 <- geocodes3[,-1]



auto.clusterNum <- function(data){
  mydata <- data
  wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
  for (i in 2:(length(geocodes4[[1]])-1)) wss[i] <- sum(kmeans(mydata, centers=i)$withinss)
  
  wss.changes <- diff(wss, 1)/wss[1] * 100
  change.threshold <- -15
  auto.clusterNum <- NULL
  for(i in seq_along(wss.changes)){
    if(wss.changes[i] > change.threshold){ #once the abs(change) is smaller than threshold, takes that cluster number and breaks out of loop
      auto.clusterNum <- i
      break
    }
  }
  
  #Handles situation where: 1) Only two clusters 2) If the wss never tapers off
  if(is.null(auto.clusterNum)){
    auto.clusterNum <- length(wss)
  }
  auto.clusterNum
  
  # #This is where algo determines optimal cluster
  # #Original code: Plot chart of WSS for Elbow Method, to find optimal cluster number
  # mydata <- geocodes4
  # wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
  # for (i in 2:(length(geocodes4[[1]])-1)) wss[i] <- sum(kmeans(mydata,
  #                                                              centers=i)$withinss)
  # plot(1:length(wss), wss, type="b", xlab="Number of Clusters",
  #      ylab="Within groups sum of squares")
  # 
  # wss.changes <- diff(wss, 1)/wss[1] * 100
  # change.threshold <- -15
  # auto.clusterNum <- NULL
  # for(i in seq_along(wss.changes)){
  #   if(wss.changes[i] > change.threshold){
  #     auto.clusterNum <- i
  #     break
  #   }
  # }
  # 
  # #Handles situation where: 1) Only two clusters 2) If the wss never tapers off
  # if(is.null(auto.clusterNum)){
  #   auto.clusterNum <- length(wss)
  # }
  #
  # auto.clusterNum
  #Function that takes in data, runs Elbow method, and returns optimial cluster number
}


#Function that takes in data, calls auto.clusterNum multiple times to get multiple optimal cluster numbers, and returns the average
avg_auto.clusterNum <- function(data, n){
  all <- vector(mode="numeric", length=n)
  for(i in seq_along(all)){
    all[i] <- auto.clusterNum(data)
  }
  result <- mean(all)
  result
}

avg_auto.clusterNum.result <- round(avg_auto.clusterNum(geocodes4, 20))
avg_auto.clusterNum.result

# #ALTERNATIVELY, allow users to input number of clusters. Code moved up top
if(clusterOption == 'm'){
  selected.clusterNum <- manual.clusterNum.input
} else {
  selected.clusterNum <- avg_auto.clusterNum.result
}

#Ensures that there is a limit to number of POIs that can be visited each day (limited POIs in a cluster)  
minClusterNum <- ceiling((length(all) - 1) / poi.limit) #Minus 1 to exclude the home
if(selected.clusterNum < minClusterNum){
  selected.clusterNum <- minClusterNum
}

# Run k-means clustering based on the specified number  of clusters, and plot


runClustering <- function(oldGeocodes, clusterNum){
  fit <- kmeans(oldGeocodes, clusterNum) #Runs k-means
  #plot(oldGeocodes, col=(fit$cluster +1), pch=20, cex=2)
  clusterList <- fit$cluster
  tempGeocodes <- cbind(oldGeocodes, clusterList)
  colnames(tempGeocodes) <- c("lon","lat","cluster")
  newGeocodes <- tibble::rownames_to_column(tempGeocodes, "Places")
  newGeocodes
  
  #Original code of runClustering function
  # fit <- kmeans(geocodes4, selected.clusterNum) #Runs k-means
  # plot(geocodes4, col=(fit$cluster +1), pch=20, cex=2)
  # clusterList <- fit$cluster
  # geocodes5 <- cbind(geocodes4, clusterList)
  # colnames(geocodes5) <- c("lon","lat","cluster")
  # geocodes6 <- tibble::rownames_to_column(geocodes5, "Places")
}
geocodes6 <- runClustering(geocodes4, selected.clusterNum)



#Taking the clustering result, check that each cluster has at most 6. If yes, proceed. If not, rerun clustering iteratively
ensurePOIcap <- function(oldGeocodes, newGeocodes){
  tempGeocodes <- newGeocodes
  switch <- 0
  while(switch < 1){
    track <- 0
    running.clusterNum <- max(tempGeocodes$cluster)
    for(i in 1:running.clusterNum){
      temp <- filter(tempGeocodes, cluster == i)
      if(length(temp$Places) <= poi.limit){track <- track + 1} #Sometimes not able to hit criteria because a cluster of >6 might persist being one cluster
    }
    if(track == running.clusterNum){
      switch <- 1
    } else {
      tempGeocodes <- runClustering(oldGeocodes, running.clusterNum + 1)
    }
  }
  tempGeocodes
  
  # #Original code for function
  # switch <- 0
  # while(switch < 1){
  #   track <- 0
  #   running.clusterNum <- max(geocodes6$cluster)
  #   for(i in 1:running.clusterNum){
  #     temp <- filter(geocodes6, cluster == i)
  #     if(length(temp$Places) <= poi.limit){track <- track + 1}
  #   }
  #   if(track == running.clusterNum){
  #     switch <- 1
  #   } else {
  #     geocodes6 <- runClustering(geocodes4, running.clusterNum + 1)
  #   }
  # }
}
clusterNumVec <- vector(mode = "numeric", 10)
clusterList <- vector("list", 10)
for(i in 1:10){
  temp <- ensurePOIcap(geocodes4, geocodes6)
  clusterList[[i]] <- temp
  # temp.clusterNum <- max(temp$cluster) #Old code where the cluster # was being recorded for each iteration
  # clusterNumVec[i] <- temp.clusterNum
}

#Of the 10 clusterings that were done, pick out the one that had the minimum number of clusters
geocodes6 <- clusterList[[which.min(sapply(clusterList, FUN= function(x){max(x$cluster)}))]]
#Plot that particular clustering
plot(x=geocodes6$lon, y=geocodes6$lat, col=(geocodes6$cluster +1), pch=20, cex=2)

#Would need user to verify the specific clusters, before running optimization
final.clusterNum <- max(geocodes6$cluster) 
print(paste("Final Cluster Number: ", final.clusterNum))






# ---- RUN PHASE 2: Optimize ----
#Function that takes in data in the format of geocodes6, and the number of clusters to filter for and analyze. Returns list containing: 1) POI sequence 2) total distance 3) lon/lat of POI, in sequence 4) route line (set of lon/lat)
optimizeRoute <- function(data, n){
  geocodes7 <- data[data$cluster == n, ]
  cluster.poi <- geocodes7$Places
  all <- c(home, cluster.poi)
  
  from <- as.vector(sapply(all, FUN = function(x) rep(x,length(all))))
  to <- rep(all, length(all))
  distresult <- mapdist(from=from, to=to, mode=transportMode) #Note: Alternative to Google Maps: http://stackoverflow.com/questions/17361909/determining-the-distance-between-two-zip-codes-alternatives-to-mapdist
  resultmatrix <- matrix(distresult$km,nrow=length(all),ncol=length(all)) #Asymmetric TSP
  rownames(resultmatrix) <- all
  colnames(resultmatrix) <- all
  resultmatrix.sym <- forceSymmetric(resultmatrix) #Force symmetry, keep upper-right triangle
  cleanresultmatrix.sym <- as.matrix(resultmatrix.sym)
  rownames(cleanresultmatrix.sym) <- all
  colnames(cleanresultmatrix.sym) <- all
  matrix.tsp <- TSP(cleanresultmatrix.sym) #Original order: labels(matrix.tsp)
  labels(matrix.tsp)
  
  # result.tsp <- solve_TSP(matrix.tsp, method = "two_opt", start=as.integer(4)) #Note that it loops back to the first selected location. order() returns optimized order. tour_length() returns distance
  # labels(result.tsp)
  
  methods <- c("nearest_insertion", "farthest_insertion", "cheapest_insertion", "nn")
  # methods <- c("nearest_insertion", "farthest_insertion", "cheapest_insertion", "nn", "arbitrary_insertion", "repetitive_nn", "two_opt")
  tours <- sapply(methods, FUN = function(m) solve_TSP(matrix.tsp, method = m, start=as.integer(1)), simplify = FALSE)
  # dotchart(sort(sapply(tours, tour_length)), xlab = "tour length", xlim = c(0, 30)) #Plots dot chart for visualization
  sapply(tours, tour_length)  #Returns the distances for all the methods
  sapply(tours, labels)
  
  
  best <- tours[[which.min(sapply(tours, tour_length))]]
  best.route <- labels(best)
  best.distance <- tour_length(best)
  best.all <- list(best.route, best.distance)
  
  
  #Plot 
  waypoints <- as.data.frame(best.all[1],stringsAsFactors = FALSE, col.names = "Places")
  
  #Data preparation: column for "for" and column for "to"
  rte.from <- waypoints
  colnames(rte.from) <- "From"
  shiftPointDown <- function(df){
    temp_row <- df[1,]
    rte.to.temp <- as.data.frame(df[-1,], stringsAsFactors = FALSE)
    rte.to <- rbind(rte.to.temp, temp_row)
    colnames(rte.to) <- "To"
    rte.to
  }
  rte.to <- shiftPointDown(rte.from)
  rte.all <- as.data.frame(cbind(rte.from, rte.to), stringsAsFactors = FALSE)
  
  #Run route() for each pair of from/to
  route_objects <- mapply(route, rte.all$From, rte.all$To, SIMPLIFY=FALSE, MoreArgs=list(mode=transportMode, structure="route", output="all"))
  
  #Run decodeLine() on each of the returned object, and rbind to get full list of lon/lat for plotting
  route_df <- NULL
  # Need decode function for route to be plotted on-road: http://stackoverflow.com/questions/30270011/ggmap-route-finding-doesnt-stay-on-roads
  decodeLine <- function(encoded){
    require(bitops)
    
    vlen <- nchar(encoded)
    vindex <- 0
    varray <- NULL
    vlat <- 0
    vlng <- 0
    
    while(vindex < vlen){
      vb <- NULL
      vshift <- 0
      vresult <- 0
      repeat{
        if(vindex + 1 <= vlen){
          vindex <- vindex + 1
          vb <- as.integer(charToRaw(substr(encoded, vindex, vindex))) - 63  
        }
        
        vresult <- bitOr(vresult, bitShiftL(bitAnd(vb, 31), vshift))
        vshift <- vshift + 5
        if(vb < 32) break
      }
      
      dlat <- ifelse(
        bitAnd(vresult, 1)
        , -(bitShiftR(vresult, 1)+1)
        , bitShiftR(vresult, 1)
      )
      vlat <- vlat + dlat
      
      vshift <- 0
      vresult <- 0
      repeat{
        if(vindex + 1 <= vlen) {
          vindex <- vindex+1
          vb <- as.integer(charToRaw(substr(encoded, vindex, vindex))) - 63        
        }
        
        vresult <- bitOr(vresult, bitShiftL(bitAnd(vb, 31), vshift))
        vshift <- vshift + 5
        if(vb < 32) break
      }
      
      dlng <- ifelse(
        bitAnd(vresult, 1)
        , -(bitShiftR(vresult, 1)+1)
        , bitShiftR(vresult, 1)
      )
      vlng <- vlng + dlng
      
      varray <- rbind(varray, c(vlat * 1e-5, vlng * 1e-5))
    }
    coords <- data.frame(varray)
    names(coords) <- c("lat", "lon")
    coords
  }
  for(i in 1:length(route_objects)){
    x <- decodeLine(route_objects[[i]]$routes[[1]]$overview_polyline$points)
    route_df <- rbind(route_df,x)
  }
  
  #Plot route
  
  waypoints2 <- geocode(waypoints$Places)
  waypoints3 <- cbind(waypoints, waypoints2)
  
  # m <- leaflet() %>%
  #   addTiles() %>%  # Add default OpenStreetMap map tiles
  #   addAwesomeMarkers(lng=waypoints3$lon, lat=waypoints3$lat, label=waypoints3$Places,  group=n , icon=awesomeIcons(markerColor='red', fontFamily="Arial", text=seq_along(waypoints3$lon))) %>% 
  #   addPolylines(lng=route_df$lon, lat=route_df$lat, group=n)
  # m  # P
  
  # center.coord <- paste(mean(geocodes7$lat), ",", mean(geocodes7$lon), sep=" ")
  # mapOutput <- qmap(center.coord, zoom = 13) +
  #   geom_path(aes(x = lon, y = lat),  colour = "red", size = 1.5, data = route_df, lineend = "round") +
  #   geom_point(data=waypoints3,aes(x=as.numeric(lon),y=as.numeric(lat)), size=6,color="yellow") +
  #   geom_text(data=waypoints3, aes(x=as.numeric(lon),y=as.numeric(lat), label=seq_along(lon)))
  # mapOutput
  # best.route
  # best.distance
  
  route.output.all <- list(best.route, best.distance, waypoints3, route_df)
  route.output.all
  
}




#Calls optimizeRoute() on each cluster, storing returned list(s) in a list
final.results <- vector("list", final.clusterNum)
for(i in 1:final.clusterNum){ 
  final.results[[i]] <- optimizeRoute(geocodes6, i)
}

#Plot map
#Get unique list of POIs + Home (extract from latest geocode() call in the optimizeRoute() function call)
final.poi.df <- data.frame(Places=character(), lon=numeric(), lat=numeric(), stringsAsFactors=FALSE)
for(i in 1:final.clusterNum){
  final.poi.df <- rbind(final.poi.df, final.results[[i]][[3]])
}
final.poi.df <- unique(final.poi.df)

overlay.vector <- vector(mode="character", length=final.clusterNum)
m <- leaflet() 
for(i in 1:final.clusterNum){
  overlay.vector[i] <- paste("Day ", as.character(i))
  m <- addTiles(m, group=paste("Day ", as.character(i)))
  m <- addAwesomeMarkers(map=m, lng=final.poi.df$lon, lat=final.poi.df$lat, label=final.poi.df$Places,  group=paste("Day ", as.character(i)) , icon=awesomeIcons(markerColor='blue', fontFamily="Arial", text=" ")) 
  m <- addAwesomeMarkers(map=m, lng=final.results[[i]][[3]]$lon, lat=final.results[[i]][[3]]$lat, label=final.results[[i]][[3]]$Places,  group=paste("Day ", as.character(i)) , icon=awesomeIcons(markerColor='red', fontFamily="Arial", text=seq_along(final.results[[i]][[3]]$lon))) 
  m <- addPolylines(map=m, lng=final.results[[i]][[4]]$lon, lat=final.results[[i]][[4]]$lat, group=paste("Day ", as.character(i)))
}
m <- addLayersControl(map = m, baseGroups = overlay.vector, options = layersControlOptions(collapsed = FALSE))
m




# ---- END OF CORE PROGRAM ----

##############################################
geocodes7 <- data[data$cluster == n, ]
cluster.poi <- geocodes7$Places
all <- c(home, cluster.poi)

from <- as.vector(sapply(all, FUN = function(x) rep(x,length(all))))
to <- rep(all, length(all))

from <- c("waco, texas")
to <- c("washington dc")
distresult <- mapdist(from=from, to=to, mode='walking') 


from <- c("161 Haig Road", "161 Haig Road", "161 Haig Road", "Katong V, Singapore", "Katong V, Singapore", "Katong V, Singapore", "Parkway Parade, Singapore", "Parkway Parade, Singapore","Parkway Parade, Singapore")
to <- c("161 Haig Road", "Katong V, Singapore", "Parkway Parade, Singapore", "161 Haig Road", "Katong V, Singapore", "Parkway Parade, Singapore", "161 Haig Road", "Katong V, Singapore", "Parkway Parade, Singapore")

from <- c("161 Haig Road", "161 Haig Road", "161 Haig Road")
to <- c("161 Haig Road", "Katong V, Singapore", "Parkway Parade, Singapore")
distresult <- mapdist(from=from, to=to, mode='walking') 


#Using googleway
#Observations: ggmap output seems to be inconsistent (df vs list); at least googleway seems consistent

test0 <- google_distance(origins = list(c("Melbourne Airport, Australia"),
                                        c("MCG, Melbourne, Australia"),
                                        c(-37.81659, 144.9841)),
                         destinations = c("Portsea, Melbourne, Australia"),
                         key = api_key,
                         simplify = TRUE)

test1 <- google_distance(origins = list(c("waco, texas")),
                destinations = c("washington dc"),
                key = api_key,
                simplify = TRUE)

test2 <- google_distance(origins = list(c("161 Haig Road"),c("161 Haig Road"),c("161 Haig Road")),
                        destinations = list(c("161 Haig Road"),c("Katong V, Singapore"),c("Parkway Parade, Singapore")),
                        key = api_key,
                        simplify = TRUE)





#For 1 home + 8 POIs:
#Calls routeQueryCheck() 11 times
#Calls distQueryCheck() 41 times
routeQueryCheck()
distQueryCheck()

# final.results[[1]][[1]] #First route - POI sequence
# final.results[[1]][[2]] #First route - total distance
# final.results[[1]][[3]] #First route - lon/lat of POI, in sequence
# final.results[[1]][[4]] #First route - route line (set of lon/lat)


#Testing use of revgeocode
o <- geocode('Baylor University')
test <- matrix(data=c(103.9030, 1.303620), nrow=1, ncol=2)
test1<- as.data.frame(test, stringsAsFactors=FALSE)
colnames(test1) <- c("lon", "lat")
test2 <- revgeocode(test1)




#Original code behind optimizeRoute() function
# geocodes7 <- geocodes6[geocodes6$cluster == 1, ]
# cluster.poi <- geocodes7$Places
# all <- c(home, cluster.poi)
# 
# from <- as.vector(sapply(all, FUN = function(x) rep(x,length(all))))
# to <- rep(all, length(all))
# distresult <- mapdist(from=from, to=to, mode=transportMode) #Note: Alternative to Google Maps: http://stackoverflow.com/questions/17361909/determining-the-distance-between-two-zip-codes-alternatives-to-mapdist
# resultmatrix <- matrix(distresult$km,nrow=length(all),ncol=length(all)) #Asymmetric TSP
# rownames(resultmatrix) <- all
# colnames(resultmatrix) <- all
# resultmatrix.sym <- forceSymmetric(resultmatrix) #Force symmetry, keep upper-right triangle
# cleanresultmatrix.sym <- as.matrix(resultmatrix.sym)
# rownames(cleanresultmatrix.sym) <- all
# colnames(cleanresultmatrix.sym) <- all
# matrix.tsp <- TSP(cleanresultmatrix.sym) #Original order: labels(matrix.tsp)
# labels(matrix.tsp)
# 
# # result.tsp <- solve_TSP(matrix.tsp, method = "two_opt", start=as.integer(4)) #Note that it loops back to the first selected location. order() returns optimized order. tour_length() returns distance
# # labels(result.tsp)
# 
# methods <- c("nearest_insertion", "farthest_insertion", "cheapest_insertion", "nn")
# # methods <- c("nearest_insertion", "farthest_insertion", "cheapest_insertion", "nn", "arbitrary_insertion", "repetitive_nn", "two_opt")
# tours <- sapply(methods, FUN = function(m) solve_TSP(matrix.tsp, method = m, start=as.integer(1)), simplify = FALSE)
# # dotchart(sort(sapply(tours, tour_length)), xlab = "tour length", xlim = c(0, 30)) #Plots dot chart for visualization
# sapply(tours, tour_length)  #Returns the distances for all the methods
# sapply(tours, labels)
# 
# 
# best <- tours[[which.min(sapply(tours, tour_length))]]
# best.route <- labels(best)
# best.distance <- tour_length(best)
# best.all <- list(best.route, best.distance)
# 
# 
# #Plot 
# waypoints <- as.data.frame(best.all[1],stringsAsFactors = FALSE, col.names = "Places")
# 
# #Data preparation: column for "for" and column for "to"
# rte.from <- waypoints
# colnames(rte.from) <- "From"
# shiftPointDown <- function(df){
#   temp_row <- df[1,]
#   rte.to.temp <- as.data.frame(df[-1,], stringsAsFactors = FALSE)
#   rte.to <- rbind(rte.to.temp, temp_row)
#   colnames(rte.to) <- "To"
#   rte.to
# }
# rte.to <- shiftPointDown(rte.from)
# rte.all <- as.data.frame(cbind(rte.from, rte.to), stringsAsFactors = FALSE)
# 
# #Run route() for each pair of from/to
# route_objects <- mapply(route, rte.all$From, rte.all$To, SIMPLIFY=FALSE, MoreArgs=list(mode=transportMode, structure="route", output="all"))
# 
# #Run decodeLine() on each of the returned object, and rbind to get full list of lon/lat for plotting
# route_df <- NULL
# # Need decode function for route to be plotted on-road: http://stackoverflow.com/questions/30270011/ggmap-route-finding-doesnt-stay-on-roads
# decodeLine <- function(encoded){
#   require(bitops)
#   
#   vlen <- nchar(encoded)
#   vindex <- 0
#   varray <- NULL
#   vlat <- 0
#   vlng <- 0
#   
#   while(vindex < vlen){
#     vb <- NULL
#     vshift <- 0
#     vresult <- 0
#     repeat{
#       if(vindex + 1 <= vlen){
#         vindex <- vindex + 1
#         vb <- as.integer(charToRaw(substr(encoded, vindex, vindex))) - 63  
#       }
#       
#       vresult <- bitOr(vresult, bitShiftL(bitAnd(vb, 31), vshift))
#       vshift <- vshift + 5
#       if(vb < 32) break
#     }
#     
#     dlat <- ifelse(
#       bitAnd(vresult, 1)
#       , -(bitShiftR(vresult, 1)+1)
#       , bitShiftR(vresult, 1)
#     )
#     vlat <- vlat + dlat
#     
#     vshift <- 0
#     vresult <- 0
#     repeat{
#       if(vindex + 1 <= vlen) {
#         vindex <- vindex+1
#         vb <- as.integer(charToRaw(substr(encoded, vindex, vindex))) - 63        
#       }
#       
#       vresult <- bitOr(vresult, bitShiftL(bitAnd(vb, 31), vshift))
#       vshift <- vshift + 5
#       if(vb < 32) break
#     }
#     
#     dlng <- ifelse(
#       bitAnd(vresult, 1)
#       , -(bitShiftR(vresult, 1)+1)
#       , bitShiftR(vresult, 1)
#     )
#     vlng <- vlng + dlng
#     
#     varray <- rbind(varray, c(vlat * 1e-5, vlng * 1e-5))
#   }
#   coords <- data.frame(varray)
#   names(coords) <- c("lat", "lon")
#   coords
# }
# for(i in 1:length(route_objects)){
#   x <- decodeLine(route_objects[[i]]$routes[[1]]$overview_polyline$points)
#   route_df <- rbind(route_df,x)
# }
# 
# #Plot route
# 
# waypoints2 <- geocode(waypoints$Places)
# waypoints3 <- cbind(waypoints, waypoints2)
# 
# # center.coord <- paste(mean(geocodes7$lat), ",", mean(geocodes7$lon), sep=" ")
# # mapOutput <- qmap(center.coord, zoom = 13) +
# #   geom_path(aes(x = lon, y = lat),  colour = "red", size = 1.5, data = route_df, lineend = "round") +
# #   geom_point(data=waypoints3,aes(x=as.numeric(lon),y=as.numeric(lat)), size=6,color="yellow") +
# #   geom_text(data=waypoints3, aes(x=as.numeric(lon),y=as.numeric(lat), label=seq_along(lon)))
# # mapOutput
# 
# m <- leaflet() %>%
#   addTiles() %>%  # Add default OpenStreetMap map tiles
#   addAwesomeMarkers(lng=waypoints3$lon, lat=waypoints3$lat, label=waypoints3$Places,  icon=awesomeIcons(markerColor='red', fontFamily="Arial", text=seq_along(waypoints3$lon))) %>%
#   addPolylines(lng=route_df$lon, lat=route_df$lat)
# m  # P
# best.route
# best.distance
# 
# route.output.all <- list(m, best.route, best.distance)


# For checking route visually from point A to point B
# route_objects <- route("Katong V, Singapore", "Parkway Parade, Singapore", mode=transportMode, structure="route", output="all")
# route_df <- NULL
# route_df <- decodeLine(route_objects$routes[[1]]$overview_polyline$points)
# 
# qmap("438772, singapore", zoom = 14) +
#   geom_path(aes(x = lon, y = lat),  colour = "red", size = 1.5, data = route_df, lineend = "round") 




# #Intro to apply functions in r: 
# https://nsaunders.wordpress.com/2010/08/20/a-brief-introduction-to-apply-in-r/
# http://stackoverflow.com/questions/3505701/r-grouping-functions-sapply-vs-lapply-vs-apply-vs-tapply-vs-by-vs-aggrega

#Use mapply to combined two columns of lon and lat, into lon-lat?
