#création fichier csv

data$lattitude<-data$position.latitude
data$longitude<-data$position.longitude

# géocodage

install.packages('tidygeocoder')
library(dplyr, warn.conflicts = FALSE)
library(tidygeocoder)
geo<-reverse_geocode(data,lat = lattitude , long = longitude , method = 'osm',
                  address = address_found, full_results = TRUE) 

#création d'un nouveau data frame pour les position geo et les code postaux

carte<-data.frame(number=data$number,codepostale=geo$postcode)
write.table(carte,"L:/BUT/SD/Promo 2022/lchaurand/Rshiny porjet/carte.csv",row.names=F)
