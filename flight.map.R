
###################################################################
#### This is a R program to map out the GEO location of our guests
###################################################################



library(tidyverse)
library(ggplot2)
library(maps)
library(ggmap)
## dir set-up
dir<-"/Users/haoshu/Desktop"
setwd(dir)
getwd()

guest<-read.csv("My Guest List_0404.csv") %>%
       filter(Wedding.Day...RSVP == "Attending") %>%
       select('First.Name','Last.Name', 
              'City', 'State.Province', 'Zip.Postal.Code',
              'Street.Address.1') %>%
        rename(Zip= 'Zip.Postal.Code') %>%
        mutate(Address= paste0(Street.Address.1,",",City,",", State.Province)) 
        

#mutate_geocode(guest, Address)

url<-"https://gist.githubusercontent.com/erichurst/7882666/raw/5bdc46db47d9515269ab12ed6fb2850377fd869e/US%2520Zip%2520Codes%2520from%25202013%2520Government%2520Data"

geo_cord<-read_csv(url(url)) %>% as.data.frame()%>%
         rename(Zip = ZIP)

guest_geo<-guest %>% 
         inner_join(geo_cord, by = "Zip") %>%
         mutate(dest_LAT = dest$LAT,
                dest_LNG = dest$LNG)
         

## Generate Map
usMap <- borders("state", colour="grey", fill="white")

map<-ggplot() + usMap +
        geom_curve(data=guest_geo,
                   aes(y=LAT, x=LNG, yend=dest_LAT, xend=dest_LNG),
                   col="#00008b",
                   size=.5,
                   curvature=0.2) +
        geom_point(data=guest_geo,
                   aes(y=LAT, x=LNG), 
                   colour="blue",
                   size=1.5) +
        geom_point(data=guest_geo,
                   aes(y=dest_LAT, x=dest_LNG), 
                   colour="red") +
        theme(axis.line=element_blank(),
              axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              axis.ticks=element_blank(),
              plot.title=element_text(hjust=0.5, size=12)) +
        ggtitle("Guest Map: Travel Map of Our Wedding Guests")
