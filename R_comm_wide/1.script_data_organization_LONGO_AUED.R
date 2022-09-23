# ------------------------------------------
# Organizing data (video plots and photoquadrats)
# low fish vulnerability to live coral loss in Southwestern Atlantic reefs

# First we need to create a 3D array of sites x sampling occasion (video plot) x species.
# And then we transform the 3D array into a long format data frame, to remove NAs

# load packages and functions
source("R_comm_wide/packages.R")
source("R_comm_wide/functions.R")

# create a folder to receive the output
dir.create("output_comm_wide_R1")

# load trait data (to find adult and juvenile fish)

traits <- read.csv (here ("data", "traits", "Atributos_especies_Atlantico_&_Pacifico_Oriental_2020_04_28.csv"),
                    sep = ";",h=T)


# benthos
bentos_emof <- read.table (here("data","detection_DwC","AAued_spatialData", "DF_eMOF.txt"),sep=",",h=T) # measurements or facts
bentos_event <- read.table (here("data","detection_DwC","AAued_spatialData", "event_core.txt"),sep=",",h=T) # event
bentos <- bentos_event [match (bentos_emof$eventID,bentos_event$eventID),]   # match
bentos <- cbind (bentos_emof,
                 bentos) # cbind

# fish
fish_emof <- read.table (here("data","detection_DwC","GLongo_spatialData", "DF_eMOF.txt"),sep=",",h=T) # measurements or facts
fish_event <- read.table (here("data","detection_DwC","GLongo_spatialData", "event_core.txt"),sep=",",h=T) # event
L.peixes <- fish_event [match (fish_emof$eventID,fish_event$eventID),]   # match
L.peixes <- cbind (fish_emof,
                   L.peixes) # cbind



# match fish and trait data
L.peixes <- cbind (L.peixes,
                   traits [match (L.peixes$scientificName, (traits$Name)),
                           c("Name", 
                             "Body_size",
                             "Aspect_ratio",
                             "Trophic_level",
                             "Size_group",
                             "TempPref_max",
                             "Depth_max")]
)

# size into numeric
L.peixes$Body_size <- as.numeric(gsub (",",".",L.peixes$Body_size))
L.peixes$Aspect_ratio <- as.numeric(gsub (",",".",L.peixes$Aspect_ratio))
L.peixes$Trophic_level <- as.numeric(gsub (",",".",L.peixes$Trophic_level))
L.peixes$Depth_max <- as.numeric(gsub (",",".",L.peixes$Depth_max))
L.peixes$TempPref_max <- as.numeric(gsub (",",".",L.peixes$TempPref_max))


# order of sites
unique(bentos$site)[order(unique(bentos$site))]
unique(L.peixes$site)[order(unique(L.peixes$site))]


# nspp Longo et al. 
unique_spp_longo138 <- unique(L.peixes$scientificName)


# modify eventID by removing 'year'

#bentos$eventID_MOD <- substr(bentos$eventID, 1,nchar(as.character(bentos$eventID))-5) 
# unique(bentos$eventID_MOD)

# create dataset for those species matching the above defined criteria
L.peixes_subset <- L.peixes#[which (L.peixes$ScientificName %in% sp_fundo_sed_fam$Name),]

# matching sites after remover of species
L.peixes_subset <- L.peixes_subset [which(L.peixes_subset$site %in% bentos$site),]# fish
L.bentos_subset <- bentos [which(bentos$site %in% L.peixes_subset$site),]# benthos
unique(L.peixes_subset$site)[order(unique(L.peixes_subset$site))]
unique(L.bentos_subset$site)[order(unique(L.bentos_subset$site))]

# rm fishes not identified, weird names ...
# unique(L.peixes_subset$ScientificName)
list_to_rm <- c("kyphosus",
                "sparisoma",
                "lutjanus",
                "caranx",
                "scarus",
                "acanthurus",
                "labrisomus",
                "malacoctenus",
                "bothus",
                "parablennius",
                "caranx",
                "synodus",
                "scorpaena",
                "halichoeres",
                "not identified",
                "mycteroperca",
                "haemulon")

# maintain spp not in this list
L.peixes_subset<- L.peixes_subset[which(tolower (L.peixes_subset$scientificName) %in% list_to_rm ==F), ]
#L.peixes_subset<- L.peixes_subset[is.na(L.peixes_subset$scientificName) !=T, ]

unique(L.peixes_subset$scientificName)[order(unique(L.peixes_subset$scientificName))]


## export data.frame of fish basic data (for interpretation of size profiles - Supporting information)
# save (L.peixes_subset, file=here("output_comm_wide","L.peixes_subset.RData"))



######################################################################
##                Formatting data to modeling 
##               Data from Longo ET AL. 2019
######################################################################


# categorical depth
L.peixes_subset$depthCategorical <- ifelse (L.peixes_subset$maximumDepthinMeters<= 7, "shallow", "deep")

# check
summary(L.peixes_subset$maximumDepthinMeters [which(L.peixes_subset$depthCategorical == "shallow")])
summary(L.peixes_subset$maximumDepthinMeters [which(L.peixes_subset$depthCategorical == "deep")])



# defining the sites
sites_longo <- paste (L.peixes_subset$site,
                      L.peixes_subset$locality,
                      L.peixes_subset$depthCategorical,
                      sep = ".")



# paste them into the df
L.peixes_subset$sites_analysis <- sites_longo



# replace NAs with 1, because there were bite detection in this places


L.peixes_subset[which(L.peixes_subset$measurementType == "foraging behavior"),"measurementValue"][is.na(L.peixes_subset[which(L.peixes_subset$measurementType == "foraging behavior"),"measurementValue"])] <- 1


# ==========================================================================
# define fish age

fish_size <- L.peixes_subset[which(L.peixes_subset$measurementType == "total length"),]
fish_size$measurementValue <- as.numeric(fish_size$measurementValue)
fish_size$Fish_age<-NA


# Maximum body size 8-16cm, adults >5cm
# adult
fish_size$Fish_age <- ifelse (fish_size$Body_size >= 8 & 
          fish_size$Body_size <= 16 &
          fish_size$measurementValue > 5, 
        "Adult", 
        fish_size$Fish_age )

# juvenile
fish_size$Fish_age <- ifelse (fish_size$Body_size >= 8 & 
                                fish_size$Body_size <= 16 &
                                fish_size$measurementValue <= 5, 
                              "Juvenile", 
                              fish_size$Fish_age )


# Maximum body size >16cm, adults >10cm
# adult
fish_size$Fish_age <- ifelse (fish_size$Body_size >16  &
                                fish_size$measurementValue > 10, 
                              "Adult", 
                              fish_size$Fish_age )

# juvenile
fish_size$Fish_age <- ifelse (fish_size$Body_size >16  &
                                fish_size$measurementValue <= 10, 
                              "Juvenile", 
                              fish_size$Fish_age )

# elacatinus figaro (prettysmall, cleaner fish, only adults observed)
fish_size [which(fish_size$Body_size < 5),"Fish_age"] <- "Adult"

# check
range(fish_size[which(fish_size$Fish_age == "Adult"),c("Body_size", "measurementValue")] [,"measurementValue"])
# 3 cm is elacatinus figaro
range(fish_size[which(fish_size$Fish_age == "Juvenile"),c("Body_size", "measurementValue")][,"measurementValue"])



# ============================================================================== 
# what interest now is the dataset of foraging behavior
foraging_behavior <- L.peixes_subset[which(L.peixes_subset$measurementType == "foraging behavior"),]
foraging_behavior$measurementValue <- as.numeric(foraging_behavior$measurementValue)


# bind age

foraging_behavior<- cbind (foraging_behavior,
                           age = fish_size$Fish_age)


# datasets per age


# fish age for the loop
fish_age <- unique(foraging_behavior$age)[is.na(unique(foraging_behavior$age))!= T]

# find videos per sampling event
splitted_eventid<-strsplit (foraging_behavior$eventID, "_")

# gather videoID
videoid <- lapply (splitted_eventid, function (i)
  i[length(i)]
)
foraging_behavior$videoid<-(unlist(videoid)) # bind


# all species found in Longo et al.
todas_sp_longo <- unique (foraging_behavior$scientificName)


# then create one table site (list), species (rows) and video (cols) 
data_longo <- lapply (fish_age, function (age)
  
  lapply (unique(sites_longo), function (i)
  
      cast(foraging_behavior [which (
                              foraging_behavior$age %in% age &
                              foraging_behavior$sites_analysis == i),],
           formula = scientificName  ~ videoid,
           value= "measurementValue",
           fun.aggregate = sum,
           na.rm=T)
    )
)

# adjust colnames, removing the word 'video' (except the 1st col -> species),
# and order each table
data_longo_adjusted <- lapply (data_longo, function (age)  # age level
  
  
        lapply (age, function (i) { # site level
      
  
            list.L.peixes <- i$scientificName # get sp ID
            cont_preliminar <- data.frame (i[,-1]) # rm the 1st col (species)
            #colnames (cont_preliminar) <- gsub ("video","",colnames (cont_preliminar))# replacing 'video' by ''
            colnames (cont_preliminar) <- seq (1, ncol (cont_preliminar))
            #cont_preliminar<- data.frame (cont_preliminar [,order (as.numeric (colnames (cont_preliminar)))])# cols' ordering
            #colnames (cont_preliminar) <- seq (1,length(colnames(cont_preliminar)))# colnames along a sequence of 1 up to nvideos
            cont_preliminar <- cbind(list.L.peixes, 
                                     cont_preliminar)# bind species ID
            
            ; # return
            cont_preliminar
  
    }
  ) # close site level
) # close age level

# we need to bind cols for videos not deployed in some sites
maximo_videos <-  max(unlist (lapply (data_longo_adjusted[[1]],ncol)))# +1  # find the max number of videos 
#                                           (+1 to avoid errors [after we will remove this additional video])

# average nunmber of plots

mean(unlist (lapply (data_longo_adjusted[[1]],ncol))-1)
sd(unlist (lapply (data_longo_adjusted[[1]],ncol))-1)

        
        
imput_longo_data <- lapply (data_longo_adjusted, function (age)  # age level
  
  
                         lapply (age, function (i) # site level
  
                              (matrix (NA, # fill a matrix with NAs when  some videos are lacking
                                       nrow= nrow (i),
                                       ncol = maximo_videos - ncol (i), # +1 to avoid prob here (i.e. ncol =0 can not be bind)
                                       dimnames = list (i$list.L.peixes ))) # set dimnames (spp)
      ) # close site level
  ) # close age level

# now bind the matrix filled with NAs
imputed_longo_data <- lapply (seq (1,length (data_longo_adjusted)), function (age)  # age level
  
  
  lapply (seq (1, length (data_longo_adjusted[[age]])), function (i) # for each site
  
          cbind (data_longo_adjusted [[age]][[i]], # bind
                 add = imput_longo_data [[age]][[i]])
  ) # close site level
) # close age level

# adjust colnames
imputed_longo_data <- lapply (imputed_longo_data, function (age)  # age level
  
  
                          lapply (age, function (i) {# for each site table
  
                              colnames (i)[-1] <- seq (1,length (colnames (i)[-1])) # set colnames
                              
                              ; # return
                              i 
  
                          }
             
        ) # close site level
) # close age level


# set site names to each list at age level

imputed_longo_data <- lapply (imputed_longo_data, function (age) {
      
    names (age ) <- unique(sites_longo) # set names to the list
    ; # return
    age
})

# Until now, the data format is site (list), spp (row) and video number (col)
# Now we need to transform this into an 3D array
# We can do that using 'do.call' to melt the list and transform into data frame

list_sp_longo_adult <- unique(unlist (lapply (imputed_longo_data[[1]], '[',1)))# list of fish spp, adult
list_sp_longo_juvenile <- unique(unlist (lapply (imputed_longo_data[[2]], '[',1)))# list of fish spp, juveline
table(list_sp_longo_juvenile %in% list_sp_longo_adult) # not all adults are found as juvenile

# list of spp per age
list_sp_longo <- list (list_sp_longo_adult,list_sp_longo_juvenile)


# one list per species
tabela_data_longo <-  lapply (seq (1,length(imputed_longo_data)), function (age) # for each age
  
  
  lapply (list_sp_longo[[age]], function (k) # for each species

  # melt and organize dimensions
  # now species will be in the list, sites in the rows, and n videos in the cols
    do.call (rbind,
      
            lapply (imputed_longo_data[[age]], function (i)
  
                  i [which(i$list.L.peixes == k),])
      )
    )
  )

## cf dimensions
# length(list_sp_longo)# n species
# ncol(tabela_data_longo[[1]][[1]]) ## n videos
# nrow(tabela_data_longo[[1]][[1]]) ## n sites

# now we need to imput sites in the species dataset
# 'model table'
nvideos_site <- unlist (lapply (data_longo_adjusted[[1]],ncol))-1 # the first col is spp

## sites we need to imput- 0 = no observation of one species in sites that actually had video plots
imput_sitios <- lapply (seq(1,length(nvideos_site)), function (i)
  
  matrix(0, 
         ncol=nvideos_site[i],
         nrow=1, # fill matrix of zeros
         dimnames = list(unique(sites_longo)[i],# with dimnames
                         seq(1,
                             nvideos_site[i]))
         )
  )




# imput sites ( now for video that were not deployed into a site)
imput_sitios <- lapply (seq (1,length (imput_sitios)), function (i)

  cbind(imput_sitios[[i]], matrix (NA, 
                                   nrow=1,
                                   ncol = maximo_videos - ncol(imput_sitios [[i]]),
                                    dimnames =list(rownames(imput_sitios [[i]]),
                                                  seq(ncol(imput_sitios [[i]])+1, # add video here as a workaround
                                                        maximo_videos))
                          )
)
)

imput_sitios <- do.call (rbind,imput_sitios) # sites to imput (minus the additional col we set before)
imput_sitios<-imput_sitios[,which(colSums(is.na(imput_sitios)!= T) != 0)]

# it is the 14st column
# rownames(imput_sitios)<- unique(sites_longo) # set names to the table

# bind
tabela_data_longo <- lapply (tabela_data_longo, function (age) # age level
  
  lapply (age, function (i) # site level
  
          # imput sites using R bind
          rbind (i[,-which(colnames(i) == "list.L.peixes")], 
                 # bind the sites of imput
                 imput_sitios [which(rownames(imput_sitios) %in% rownames(i) == F),]
                 ) 
               
        ) # close site level
) # close age level

# equal data structure between ages
# tabela_data_longo [[1]][[1]] == tabela_data_longo[[2]][[1]]

# set names to species in each age

tabela_data_longo <- lapply (seq (1,length(tabela_data_longo)), function (age) { # each age

    names(tabela_data_longo[[age]]) <- list_sp_longo[[age]] # spp names in the list
    tabela_data_longo[[age]]

})


# rows in alphabetic order
tabela_data_longo <- lapply(tabela_data_longo, function (age) # age level
                            
  lapply (age, function (i) { # site level
  
       i [order(rownames(i)),]

    }
  ) # close site level
) # close age

# finally list into 3D array [site,video,species]
# check rownames' matching
# unique (sites_longo) [order(unique (sites_longo))] == rownames(tabela_data_longo [[1]]$`acanthurus bahianus`)

# produce one array per age

arranjo_longo_sitio_video <-   lapply (seq (1,length(tabela_data_longo)), function (age) # for each age
  
  
                          array(unlist(tabela_data_longo[[age]] ), 
                                         dim = c(nrow(tabela_data_longo[[age]] [[1]]),  # sites
                                                 ncol(tabela_data_longo [[age]] [[1]]),  # sampling occasions
                                                 length(tabela_data_longo[[age]])), # spp
                                         dimnames = list (unique (sites_longo) [order(unique (sites_longo))],
                                                          NULL,
                                                          list_sp_longo[[age]]))
) # close age level



# transform into detection (1) and non-detection (0) data

arranjo_longo_sitio_video <- lapply (arranjo_longo_sitio_video, function (age) { # each age
  
  age[age > 1] <- 1;
  age
  
})
#arranjo_longo_sitio_video <- arranjo_longo_sitio_video [,-13,] # remove the last video (no detection)



# -------------------------------------------------------
# -------------------------------------------------------
# observation covariates

# time of recording
time_video <- L.peixes_subset[which(L.peixes_subset$measurementType == "time"),]

# time in seconds
tempo<-lapply (time_video$measurementValue, function (i) {
  minuto <- as.numeric (strsplit (gsub ("''","",i), "'") [[1]] [1])
  segundo <- as.numeric (strsplit (gsub ("''","",i), "'") [[1]] [2])
  tempo <- (minuto*60)+segundo
  ;
  tempo
  }
)

# bind time to data
time_video$tempo <- unlist (tempo)

# bind videoid
time_video$videoid <- foraging_behavior$videoid

# find time per site and video
data_longo_tempo <- lapply (unique(sites_longo), function (i)
  # pivot table per site
  as.data.frame (
    cast(time_video [which (time_video$sites_analysis == i),],
       formula = sites_analysis  ~ videoid,
       value= "tempo",
       fun.aggregate = sum,
       na.rm=T)#[,-1]
))


# find the max number of videos deployed (+1 is a workaround)
maximo_videos_longo <- max(unlist(lapply (data_longo_tempo,ncol)))-1

# to imputing data (as before)
df_tempo <- lapply (data_longo_tempo, function (i) {
  
  # ordering cols (video number)
  #ordem <- i [,order (as.numeric(gsub ("a",".1", (gsub ("video","",colnames (i))))))]
  list_sites <- data.frame (sites_analysis=i$sites_analysis)
  k <- i
  k <- data.frame (k[,-1])
  colnames (k) <- seq(1,ncol(k))
  k<- data.frame(k[,order(as.numeric(as.numeric(colnames(k))))])
  colnames (k) <- seq(1,ncol(k))
  #i <- i [,order (i, as.numeric(colnames(i)))]
  # imput NA for missing occasions
  imput <- matrix (NA, ncol = maximo_videos_longo+1 - ncol(k),
                   nrow = nrow(k),
                   dimnames = list (NULL,
                                    paste ("video", seq (ncol(k)+1,
                                                         maximo_videos_longo+1), # workaround
                                           sep="")
                                    ))
  # imput
  ordem <- cbind(k,imput)
  # adjust colnames
  colnames(ordem) <- seq (1,ncol(ordem))
  ordem <- cbind (list_sites, ordem)
  ; # return
  ordem # ordered data
  
  
})
  
# list to df
df_tempo <- do.call (rbind, df_tempo)#[,-11] # remove col 11 with any info
df_tempo<- df_tempo[order (df_tempo$sites_analysis,decreasing=F),]
df_tempo<- df_tempo[,-14]# rm the last column (previous workaround)

# site names
df_tempo$sites_analysis == rownames(arranjo_longo_sitio_video[[1]]) # adults
df_tempo$sites_analysis == rownames(arranjo_longo_sitio_video[[1]]) # juveniles

# test of matching of number of sampling occasions and videos
# check
rowSums (is.na(df_tempo)) == rowSums (is.na(arranjo_longo_sitio_video[[1]][,,1])) # adults
rowSums (is.na(df_tempo)) == rowSums (is.na(arranjo_longo_sitio_video[[2]][,,1])) # juveniles


# ===========================================================
# find depth

data_longo_prof <- strsplit(df_tempo$sites_analysis,"\\.")
data_longo_prof<- sapply (data_longo_prof, "[[",3)
data_longo_prof <- data.frame (sites_analysis = df_tempo$sites_analysis,
            depth = data_longo_prof)

data_longo_prof$sites_analysis == (df_tempo$sites_analysis)




###################################################################

# Formating coral data to site occ modeling 
# and matching with fish data from Longo et al.

##################################################################
#sitios_bentos <- unique (L.bentos_subset$eventID_MOD)# sites with benthos' sampling
#sitios_bentos <- gsub ("oc_isl.","",gsub ("se_reefs.","",gsub ("ne_reefs.","",sitios_bentos))) # adjust names
#sitios_bentos <- sitios_bentos[match(sitios_longo, sitios_bentos)]# sites in both Aued et al. and Longo et al.
#L.bentos_subset$eventID_MOD <- gsub ("oc_isl.","",gsub ("se_reefs.","",gsub ("ne_reefs.","",L.bentos_subset$eventID_MOD))) # edit ID in the original tab


# categorical depth
L.bentos_subset$depthCategorical <- ifelse (L.bentos_subset$minimumDepthinMeters >= 0 & 
                                              L.bentos_subset$maximumDepthinMeters <=7 , 
                                            "shallow", "deep")


# different depths
L.bentos_subset$maximumDepthinMeters == L.bentos_subset$minimumDepthinMeters
(L.bentos_subset$maximumDepthinMeters [which(L.bentos_subset$depthCategorical == "shallow")])


# check
summary(L.bentos_subset$minimumDepthinMeters [which(L.bentos_subset$depthCategorical == "shallow")])
summary(L.bentos_subset$maximumDepthinMeters [which(L.bentos_subset$depthCategorical == "deep")])


# defining the sites
sites_bentos <- paste (L.bentos_subset$site,
                       L.bentos_subset$locality,
                       L.bentos_subset$depthCategorical,
                      sep = ".")



# paste them into the df
L.bentos_subset$sites_analysis <- sites_bentos



# find videos per sampling event
splitted_eventid_bentos<-strsplit (L.bentos_subset$eventID, "_")

# gather videoID
videoid_bentos <- lapply (splitted_eventid_bentos, function (i)
  i[length(i)]
)



# adjust anequim 2010 (ajsutar depois no dataset)
videoid_bentos <- unlist(videoid_bentos)
videoid_bentos[which(videoid_bentos == "2010")] <- "Video6" # missing video in anequim, set to video6


# bind
L.bentos_subset$videoid<-((videoid_bentos)) # bind
L.bentos_subset$videoid <- gsub ("Filmagem","Video",L.bentos_subset$videoid)


# number of videos per site
summary(rowSums(table(L.bentos_subset$sites_analysis,L.bentos_subset$videoid)>0))
sd(rowSums(table(L.bentos_subset$sites_analysis,L.bentos_subset$videoid)>0))


# Agaricia (hard to be identified)
L.bentos_subset$scientificName[grep("Agaricia", L.bentos_subset$scientificName)] <- "Agaricia spp"
L.bentos_subset$verbatimIdentification[grep("Agaricia", L.bentos_subset$verbatimIdentification)] <- "Agaricia spp"

# one pivot table per site and sp

cob_bentos <- cast(L.bentos_subset,
       formula = sites_analysis  ~ verbatimIdentification,
       value= "measurementValue",
       fun.aggregate = mean)


# list of coral species we're interested in
sp_corais <- c("agaricia spp", # agaricia cannot be correctly identified in the pictures
               "favia gravida",
               "madracis decactis", "meandrina brasiliensis","millepora alcicornis",
               "millepora nitida",
               "montastraea cavernosa", "mussismilia harttii", "mussismilia braziliensis",
               "mussismilia hispida", "mussismilia leptophylla", "porites astreoides",
               "porites branneri", "siderastrea spp") # did not consider "Millepora.sp", "Millepora.incrusting", and  "Porites.sp"

colnames(cob_bentos)<-tolower (gsub ("\\.", " ", colnames(cob_bentos)))

## range of cover of corals
cob_corals<- cob_bentos[, c(1,which(colnames(cob_bentos) %in% sp_corais))]
range(cob_corals[,-1])
mean(as.matrix(cob_corals[,-1]))



# things to report in the paper
summary_stat <- apply (cob_corals[,-1],2,summary)*100
colnames(summary_stat) <- sp_corais



# Fig. S1.1


par (mfrow=c(1,2))
hist(
  
  apply (cob_corals[,-1],1,sum)*100, 
  xlab="Mean coral cover (%)",ylab="Frequency (number of sites)",main=""
)

abline (v=mean (apply (cob_corals[,-1],1,sum)*100),lwd=3,col="black")






## algae


sp_algae <- c(colnames(cob_bentos) [grep("turf",colnames(cob_bentos))],
              colnames(cob_bentos) [grep("algae",colnames(cob_bentos))])[c(1,3,5:7)]

# get the site cover
cob_algae<- cob_bentos[, c(1,which(colnames(cob_bentos) %in% sp_algae))]
cor(cob_algae[,-1])
# statistics
range(cob_algae[,-1])
mean(as.matrix(cob_algae[,-1]))
# most frequent algae
colSums(cob_algae[,-1]>0)


hist(
  
  cob_algae$`calcareous turf`*100, 
  xlab="Mean algae cover (%)",ylab="",main=""
)

abline (v=mean (apply (cob_algae[,-1],1,sum)*100),lwd=3,col="black")





## Obtain geographic coordinates
## NAs are produced because aggregation of factors is not possible
coordenadas <- aggregate(L.bentos_subset, 
                         by= list (L.bentos_subset$sites_analysis), 
                         FUN=mean)[c("Group.1","decimalLongitude","decimalLatitude")]



# -------------------------------------------------
# Finally, we match fish and coral data


# match fish and coral
arranjo_longo_sitio_video <- lapply (arranjo_longo_sitio_video, function (age) 
  
  (age[which(rownames(age) %in% cob_bentos$sites_analysis),,])
)
# check dims
# lapply (arranjo_longo_sitio_video,dim)

# match time and fish
df_tempo <- df_tempo [which(df_tempo$sites_analysis %in% rownames(arranjo_longo_sitio_video[[1]])),]


# match depth and fish
data_longo_prof <- data_longo_prof [which(data_longo_prof$sites_analysis %in% rownames(arranjo_longo_sitio_video[[1]])),]


# match coral and fish
cob_corals <- (cob_corals[which(cob_corals$sites_analysis %in% rownames(arranjo_longo_sitio_video[[1]])),])


# match algae and fish
cob_algae <- (cob_algae[which(cob_algae$sites_analysis %in% rownames(arranjo_longo_sitio_video[[1]])),])

# match all benthos and fish
cob_bentos <- (cob_bentos[which(cob_bentos$sites_analysis %in% rownames(arranjo_longo_sitio_video[[1]])),])


# check matching
cob_corals$sites_analysis == rownames(arranjo_longo_sitio_video[[1]])

# algae
cob_algae$sites_analysis == rownames(arranjo_longo_sitio_video[[1]])


# time
df_tempo$sites_analysis == rownames(arranjo_longo_sitio_video[[1]])

# prof
data_longo_prof$sites_analysis == rownames(arranjo_longo_sitio_video[[1]])

# match coordinates
coordenadas <-  (coordenadas[which(coordenadas$Group.1 %in% rownames(arranjo_longo_sitio_video[[1]])),])
# check
(coordenadas$Group.1 == rownames(arranjo_longo_sitio_video[[1]]))



# -------------------------------------------------

# avoid spatial mismatch between species



site_peixe_detection <- lapply (arranjo_longo_sitio_video, function (age) 
  
                      apply(age,c(1,3),
                              sum,
                              na.rm=T) # sites with fish detection
)


# remove fish that after the matching was not detected in any site
arranjo_longo_sitio_video<- lapply (seq(1,length(arranjo_longo_sitio_video)), function (age)
  
  arranjo_longo_sitio_video [[age]] [,,which(colSums(site_peixe_detection[[age]])>0)]
  
)

site_peixe_detection <- lapply (site_peixe_detection, function (age) 
  
  age[,which(colSums(age)>0)]
)




# the same for corals (all corals are there)
colSums(cob_corals[,-1])>0
sel_corals <-colnames(cob_corals[,-1])[which(colSums(cob_corals[,-1]>0)>=4)] # subset of corals detected in 
cob_corals<-cob_corals[,sel_corals]

# which corals occur in a min of sites (20% of total number of sites)

#min_sites<- 0#round(nrow (cob_corals) * 0.2)



# number of sites with coral detection
site_coral_detection <- ifelse (cob_corals >0,1,0)# sites with coral detection
#sp_coral_cover <- cob_corals [,which(colSums (site_coral_detection)>=min_sites)]


#lapply (seq (1,ncol (site_peixe_detection[[1]])), function (i)
#  
#  length(which (age[,i] >=1) )
#)
#
## which fishes share these "min_sites" with coral observation
#over_peixe_coral <- lapply (seq(1,ncol (site_coral_detection)), function (coral) # for each coral
#  
#  
#  lapply (site_peixe_detection, function (age)
#  
#    unlist( # unlist of fish level
#    
#    
#      lapply (seq (1,ncol (age)), function (sp) # and fish
#      ## check if the fish was detected in the "min sites" with coral detection
#        # of the N sites that a given fish was detected, how many had the detection of a given coral
#      table(which (age[,sp] >=1) %in% which (site_coral_detection[,coral] ==1))[2] > min_sites
#      
#    )# close fish
#    )  # unlist
#    )# close age
#  ) # close corals
#



## finally we select species with spatial match

subset_peixes <- lapply (seq (1,ncol(site_coral_detection)), function (coral) # each coral
  
  
  lapply (seq (1,length(arranjo_longo_sitio_video)), function (age) # and age
  
  
    arranjo_longo_sitio_video[[age]] [,,]#which (is.na(over_peixe_coral[[coral]][[age]])!=T)]
  
  ) # close age
) # close coral



# then transform data into long format data frame
df_fish_data <- lapply (subset_peixes, function (coral) ## for each coral spp
  
  lapply (coral, function (age) ## for each age
  
    lapply (seq(1,dim(age)[3]), function (sp) { ## and fish
    
    ## matrix into vector
    y_long <- as.numeric(age [,,sp])# observation
    tabela_tempo <- as.numeric (as.matrix(df_tempo[,-1])) # recording time
    
    ## sampling occasions
    df_data <- data.frame(obs= seq(1,length(y_long)),
                          y= y_long,
                          time = as.numeric(tabela_tempo),
                          M = rep (seq (1,dim(age)[1]), 
                                   ncol(df_tempo[,-1])),
                          J = unlist(
                            lapply (seq(1,ncol(df_tempo[,-1])), function (i) 
                              rep (i,dim(age)[1]))
                          ),
                          prof = as.numeric(ifelse (data_longo_prof$depth =="deep", 2,1))
    )
    
    ## remove NAs
    df_data <- df_data[which (is.na(df_data$time) != T),]
    
    ; # return
    df_data # long format data
    
  }
  )
  )
)



## finally, a list of long formats (arrays across coral and fish spp)

df_fish_data_per_coral <- lapply (seq (1,length(df_fish_data)), function (coral)  # each coral
  
  lapply (seq (1,length (df_fish_data[[coral]])), function (age)   { # each age
  
  ## create an empty array with dims given by long format data
  empty_array <- array (NA, dim=c(nrow(df_fish_data[[coral]][[age]][[1]]), # no difference if it is [[1]] or e.g., [[5]] -> all dims are equal
                                  ncol (df_fish_data[[coral]][[age]][[1]]),
                                  length(df_fish_data[[coral]][[age]])),
                        dimnames = list(NULL, 
                                        colnames(df_fish_data[[coral]][[age]][[1]]),NULL))
      # fill array
      for (k in 1:dim(empty_array)[3]) {
        
        empty_array [,,k] <- as.matrix(df_fish_data[[coral]][[age]][[k]])
        
      }
  filled_array <- empty_array
  ; # return this filled array
  filled_array
  
  }
  ) # close age
) # close coral


# get final species list 
coral_species <- colnames(site_coral_detection)# corals
fish_species <- lapply (subset_peixes, function (coral) 
  
  lapply (coral, function (age)  { # each
   
  nomes <- dimnames(age)[3]  
  nomes[[1]]
  
  }))






## set dimnames to all data
df_fish_data_per_coral <- lapply (seq (1,length(df_fish_data_per_coral)), function (coral) # across corals
  
  lapply (seq (1,length(df_fish_data_per_coral[[coral]])), function (age) { # and ages
    #set dimnames
    dimnames(df_fish_data_per_coral[[coral]][[age]])[[3]] <- fish_species[[coral]][[age]]
    df_fish_data_per_coral[[coral]][[age]]
}))


# naming lists with coral names
names(df_fish_data_per_coral) <- coral_species

# lists with juvenile names
df_fish_data_per_coral <- lapply (df_fish_data_per_coral, function (coral){
  
  names(coral) <- c("adult", "juvenile") 
  coral
  
})



# anayzed fish

test <- lapply (df_fish_data_per_coral, function (coral)
  
  lapply (coral, function (age)
    
    dimnames(age)[[3]] ))



#
length(unique(unlist(test)))


## basic statistics

mean((df_fish_data_per_coral[[1]][[1]][,"time",1])/60)
sd((df_fish_data_per_coral[[1]][[1]][,"time",1])/60)
range((df_fish_data_per_coral[[1]][[1]][,"time",1])/60)

## av number of det per fish

mean(
unlist(
lapply (df_fish_data_per_coral, function (coral)
  
  lapply (coral, function (age)
    
    lapply (seq (1,dim(age)[3]), function (sp)
    
    sum(age[,"y",sp], na.rm=T)
    
    ))))

)

#SD 

sd(
  unlist(
    lapply (df_fish_data_per_coral, function (coral)
      
      lapply (coral, function (age)
        
        lapply (seq (1,dim(age)[3]), function (sp)
          
          sum(age[,"y",sp], na.rm=T)
          
        ))))
  
)




## find most detected spp

most_det <- (lapply (df_fish_data_per_coral, function (coral)
  
  lapply (coral, function (age)
    
    lapply (seq (1,dim(age)[3]), function (sp)
      
      sum(age[,"y",sp], na.rm=T)
      
    ))))

# detction equal across corals
# adult
most_det_adult <- unlist(most_det$`porites astreoides`$adult)
names(most_det_adult) <- fish_species[[1]][[1]]

most_det_adult[order(most_det_adult)]

# juvenile
most_det_juvenile <- unlist(most_det$`porites astreoides`$juvenile)
names(most_det_juvenile) <- fish_species[[1]][[2]]

(most_det_juvenile[order(most_det_juvenile)])

# put into a df
#most_det <- data.frame (ndet=most_det,
#                        sp = unlist(fish_species))

#most_det <- most_det[order(most_det$ndet,decreasing=T),] # order
#write.table (most_det, file = here("output_comm_wide","most_detVIDEO.csv")) # and save

  
###############################################
# # # # # # # # #  SAVE # # # # # # # # # # # #
###############################################

### save data -> fish site-occupancy modeling

save (fish_size, # all data of size == useful for functional analyses
      subset_peixes, # array of fishes after spatial matching checking
      df_fish_data_per_coral,## fish data for modeling
      fish_species,## id of analyzed fish, per coral spp
      todas_sp_longo,## id of all fishes detected by Longo et al. 2019
      # coral & algae data
      coral_species,
      cob_bentos,
      cob_corals,
      cob_algae,
      # coordinates
      coordenadas,
      
      file = here("output_comm_wide_R1",
                  "Data_fish_detection_LONGO_AUED.RData"))


# end



