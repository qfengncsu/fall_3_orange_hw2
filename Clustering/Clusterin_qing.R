
##############################################
#Diferent Way to look at it
###############################################
#nrc_total <- get_sentiments("afinn")

#######################################################
library(tidytext)
library(stringr)
library(dplyr)
library(text2vec)
library(readr)
library(ggmap)

##############################################
#Diferent Way to look at it
###############################################
setwd("/Users/qing/Desktop/Clustering/Project/boston-airbnb-open-data/")
nrc_total <- get_sentiments("afinn")

#Load the dataset and group the 
#reviews by the listing id 
#keep all listings having more than 4 reviews 
reviews <- read_csv("reviews.csv")
#attractions <- read_csv("Attractions.csv")
attractions8 <- read_csv("attractions8.csv")
listings <- read_csv("listings.csv")

rv <- reviews %>% group_by(listing_id) %>%
  count(listing_id, sort = TRUE) %>% filter( n >= 4) %>% select(-"n")

#break up the dataset's listing into individual words
#join this dataset with rv, keeping only those listing IDs that have 4 or more
# reviews remove all of the na values
new_reviews <- reviews %>%
  group_by(listing_id) %>%
  unnest_tokens(word, comments)  %>%
  right_join(rv,by="listing_id") %>% filter(!is.na(word)) %>%
  left_join(nrc_total,by="word") %>% filter(!is.na(score))

#Find the number of words scored
score <- new_reviews %>% group_by(listing_id) %>% mutate(sscore = sum(score)) %>% distinct(listing_id,sscore)
nwords <- new_reviews %>% group_by(listing_id) %>% count(listing_id) 

complete <- nwords %>% left_join(score,"listing_id") %>% mutate(avg = sscore/n)


#we later find three listing_id have 0 beds, thus lead to ppb=infinity
#listing_id=7728508, 7856323, 8882195
#exclude these three listing id from start will make the process easier.
complete <-complete[!(complete$listing_id=="7728508" | complete$listing_id=="7856323" | complete$listing_id=="8882195" ),]

#exclude listing_id with ppb=NA
complete <-complete[!(complete$listing_id=="1426754" | complete$listing_id=="9239190" | complete$listing_id=="12627225" | complete$listing_id=="12857811" ),]

complete$avg <- scale(complete$avg) #standardize the score

#### calculate price/bed ####
listings$price <- as.numeric(gsub('[$,]', '', listings$price))
listings$ppb <- listings$price/listings$beds

#listings have 3585 rows, left join with complete.
#to make the merge easier, extract listing_id,ppb, lat, lon from listings
listing <- cbind(listings$id, listings$ppb, listings$longitude, listings$latitude)
colnames(listing) <- c("listing_id", "ppb", "longitude", "latitude")
listing <- as.data.frame(listing)
combined <- complete %>% left_join(listing, "listing_id")

combined$std.lat <- scale(combined$latitude)
combined$std.lon <- scale(combined$longitude)
#combined$ppb1 <- as.numeric(gsub('[$,]', '',combined$ppb))

combined$std.ppb <- scale(combined$ppb)



# Calculate distance in kilometers between two points
earth.dist <- function (long1, lat1, long2, lat2)
{
  rad <- pi/180
  a1 <- lat1*rad
  a2 <- long1*rad
  b1 <- lat2*rad 
  b2 <- long2*rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}
#attractions <- read_csv("attractions.csv")
#attractions8 <- read_csv("attractions8.csv")
calendar <- read_csv("calendar.csv")
#lsid <- unique(new_reviews$listing_id)
#full <- data.frame(matrix(0,nrow=length(unique(new_reviews$listing_id)),ncol = nrow((attractions))))
full <- data.frame(matrix(0,nrow= length(unique(combined$listing_id)),ncol = nrow((attractions8))))
names(full) <- attractions8$Attractions
rownames(full) <- combined$listing_id

for (i in 1:nrow(combined)){
  for (ii in 1:nrow(attractions8)){
    full[i,ii]=earth.dist(attractions8$Lon[ii], attractions8$Lat[ii],	combined$longitude[i], combined$latitude[i])
  }
}

full$avg_dist <- rowMeans(full)
full$listing_id <- as.numeric(rownames(full))
full = subset(full, select = c(listing_id, avg_dist ))
              

combined1 <- full %>% left_join(combined,"listing_id")
combined1$std.avg_dist <- scale(combined1$avg_dist)

#Count calendar
combined2 <- combined1
Rented <- subset(calendar, available=='f')

count <- Rented %>% group_by(listing_id) %>%
  count(listing_id, sort = TRUE)

combined2$avg <- as.numeric(combined2$avg)
combined2$std.lat <- as.numeric(combined2$std.lat)
combined2$std.lon <- as.numeric(combined2$std.lon)
combined2$std.ppb <- as.numeric(combined2$std.ppb)
combined2$std.avg_dist <- as.numeric(combined2$std.avg_dist)

count$count <- count$n
count <- subset(count, select = -n)

combined3 <- combined2 %>% left_join(count, "listing_id") 

#Make clusters
toC1 <- cbind(combined1$avg,combined1$std.ppb,combined1$std.avg_dist)

clusters.c <- hclust(dist(toC1),method="complete")
#clusters.a <- hclust(dist(toC1),method="average")

plot(clusters.c)

combined3$clus <- cutree(clusters.c,6) #it looks like 6 clusters is reasonable
#combined2 <- combined1 %>% mutate_all(funs( as.numeric(scale(.) )))
#combined1$clus <- cutree(clusters.s,6) #it looks like 6 clusters is reasonable

library(ggmap)
clu1 <- subset(combined3, clus==1)
clu2 <- subset(combined3, clus==2)
clu3 <- subset(combined3, clus==3)
clu4 <- subset(combined3, clus==4)
clu5 <- subset(combined3, clus==5)
clu6 <- subset(combined3, clus==6)

median(clu6$std.avg_dist) #4.537537
median(clu6$avg) #-0.1608696
median(clu6$std.ppb)  ### 

mean(clu6$avg_dist) #3.112559
mean(clu6$avg) #0.6960601
mean(clu6$ppb) # gives NA

#register_google("AIzaSyDsXl1rUspZ1sLY_dnMrb1urJq_O0rVFTY")

#Load Map
load("/Users/qing/Desktop/Clustering/Project/boston.RData")
#map <- get_map(location = "Boston", zoom = 11)

ggmap(map, fullpage = TRUE) +
  geom_point(data = clu1, aes(x = longitude, y = latitude), color = 'pink', size = 2) +
  geom_point(data = clu2, aes(x = longitude, y = latitude), color = 'yellow', size = 2) +
  geom_point(data = clu3, aes(x = longitude, y = latitude), color = 'blue', size = 2) +
  geom_point(data = clu4, aes(x = longitude, y = latitude), color = 'orange', size = 2) +
  geom_point(data = clu5, aes(x = longitude, y = latitude), color = 'green', size = 2) +
  geom_point(data = clu6, aes(x = longitude, y = latitude), color = 'red', size = 2) +
  geom_point(data = attractions8, aes(x = Lon, y = Lat), color = 'black', size = 5)

ggmap(map, fullpage = TRUE) +
  geom_point(data = clu3, aes(x = longitude, y = latitude), color = 'blue', size = 2)+
  geom_point(data = attractions8, aes(x = Lon, y = Lat), color = 'black', size = 5)
#Save map
#save(map,file = "boston_2.RData")
