
require(here)
require(rgdal)
require(raster)
## carregar os shapes corais

coral_dir <- list.dirs(here ("corals"),full.names =F)[-1]

# corais
shapes_corais <- lapply (coral_dir,function (i) {
  
            readit<-readOGR(dsn=here("corals",i), layer="data_0",
                   encoding="UTF-8", use_iconv = T)
            transformit <- spTransform(readit, 
                                       CRS=CRS("+proj=lcc +lat_0=0 +lon_0=-20 +lat_1=10 +lat_2=-33 +x_0=0 +y_0=0  +datum=WGS84 +units=m +no_defs"), 
                                       check=T)
            
           ;
            transformit
            } )

names (shapes_corais)<-lapply (shapes_corais,"[[","BINOMIAL")

## shape fish

fish_dir <- list.dirs(here ("fish"),full.names =F)[-1]

shapes_fish <- lapply (fish_dir,function (i) {
  
  readit <-readOGR(dsn=here("fish",i), layer="data_0",
          encoding="UTF-8", use_iconv = T)
  transformit <- spTransform(readit, 
              CRS=CRS("+proj=lcc +lat_0=0 +lon_0=-20 +lat_1=10 +lat_2=-33 +x_0=0 +y_0=0  +datum=WGS84 +units=m +no_defs"), 
              check=T)
  ;
  transformit
  
}
)

names (shapes_fish)<-lapply (shapes_fish,"[[","BINOMIAL")

## coral relying on one coral 
coral_reliant_mill <- c("Bodianus rufus","Rypticus saponaceus",
                        "Scarus zelindae","Abudefduf saxatilis",
                        "Cryptotomus roseus","Stegastes fuscus")

plot(shapes_corais[[which(names(shapes_corais) == "Millepora alcicornis")]])
lapply (shapes_fish[which(names(shapes_fish) %in% coral_reliant_mill)],
        function (i)
        
              plot(i,add=T,col=rgb(0,0.8,0,alpha=0.3))
              
              )

require(raster)
# 

lapply (coral_reliant_mill, function (i){

  c1 <- shapes_corais[[which(names(shapes_corais) == "Millepora alcicornis")]]
  p1 <- shapes_fish[[which(names(shapes_fish) %in% i)]]
  
  #plot(p1)
  #plot(c1,add=T,col="red")
  ## cropping
  crop_maps <- crop (c1,p1)
  #plot(test,add=T,col="black")
  
  # calculating total area (remember to change projection)
  area_total <- area (p1)
  area_reduzida <- area(crop_maps)
  # ratio
  ratio <- area_reduzida/area_total
  ;
  ratio
})
