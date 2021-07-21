# ------------------------------------------
# Organizing data (video plots and photoquadrats)
# fish vulnerability to live coral cover loss in southwestern atlantic reefs

# First we need to create a 3D array of sites x sampling occasion (video plot) x species.
# And then we transform the 3D array into a long format data frame, to remove NAs

# load packages and functions
source("R/packages.R")
source("R/functions.R")

# create a folder to receive the output
dir.create("output_comm_wide")

# -------------------------------------------------------------------------- #
# listing the ID sites with larger coral cover based on Aued et al. 2018 PLosOne
# -------------------------------------------------------------------------- #
locais_corais <- c("rgnor_parrachos",
                   "rgnor_norte",
                   "rgnor_sul",
                   "costa_corais",
                   "btds_santos",
                   "abrolhos",
                   "ceara",
                   "espirito_santo",
                   "arraial",
                   "ilhabela",
                   "alcatrazes",
                   "manuel_luis",
                   ## ilhas
                   "noronha",
                   "rocas",
                   "trindade")

# ----------------------
# load benthic data set (Aued et al. 2018)
bentos <- read.xlsx(here("data","detection","Updated_compiled_quadrats_allsites.xlsx"),
                    sheet = 1, colNames = TRUE,detectDates=F)
bentos$eventDate <-convertToDate(bentos$eventDate)# adjust data - bug of 'openxlsx'

# ------------------------
# load video plot data from Longo et al. 2019
L.peixes<- read.xlsx(here("data","detection","occ_Longo_et_al","Data_Trophic_Interactions_WAtlantic_GLongo_clean_UPDATED_ALL.xlsx"),
                     sheet = 1, colNames = TRUE,detectDates=F)
L.peixes$date <-convertToDate(L.peixes$date)# adjust data - bug of 'openxlsx'
L.peixes$ScientificName <- tolower (L.peixes$ScientificName)# lower case

# matching sites across datasets
bentos <- bentos [which(bentos$Locality %in% locais_corais),] # which sites are in the list of sites
L.peixes <- L.peixes [which(L.peixes$location %in% locais_corais),]# which sites are in the list of sites

# nspp Longo et al. 
unique_spp_longo138 <- unique(L.peixes$ScientificName)


# modify eventID by removing 'year'

bentos$eventID_MOD <- substr(bentos$eventID, 1,nchar(as.character(bentos$eventID))-5) 
# unique(bentos$eventID_MOD)

# create dataset for those species matching the above defined criteria
L.peixes_subset <- L.peixes#[which (L.peixes$ScientificName %in% sp_fundo_sed_fam$Name),]

# matching sites after remover of species
L.peixes_subset <- L.peixes_subset [which(L.peixes_subset$eventID_MOD %in% bentos$eventID_MOD),]# fish
L.bentos_subset <- bentos [which(bentos$eventID_MOD %in% L.peixes_subset$eventID_MOD),]# benthos

# rm fishes not identified, weird names ...
# unique(L.peixes_subset$ScientificName)
list_to_rm <- c("kyphosus.sp",
                "sparisoma.sp",
                "lutjanus.sp",
                "caranx.sp1",
                "scarus.sp",
                "acanthurus.sp",
                "labrisomus.sp",
                "malacoctenus.sp",
                "bothus.sp",
                "parablennius.sp",
                "caranx.sp2",
                "synodus.sp",
                "scorpaena.sp",
                "halichoeres.sp",
                "not.identified")

# maintain spp not in this list
L.peixes_subset<- L.peixes_subset[which(L.peixes_subset$ScientificName %in% list_to_rm ==F), ]
L.peixes_subset<- L.peixes_subset[is.na(L.peixes_subset$ScientificName) !=T, ]

## export data.frame of fish basic data (for interpretation of size profiles - Supporting information)
save (L.peixes_subset, file=here("output_comm_wide","L.peixes_subset.RData"))

######################################################################
##                Formating data to modeling 
##               Data from Longo ET AL. 2019
######################################################################

sitios_longo <- unique (L.peixes_subset$eventID_MOD)# adjust ID
sitios_longo <- gsub ("oc_isl.","",gsub ("se_reefs.","",gsub ("ne_reefs.","",sitios_longo)))# adjust locality/site - rm region
sitios_longo <- sitios_longo [order(sitios_longo)]# ordering site names
L.peixes_subset$eventID_MOD <- gsub ("oc_isl.","",gsub ("se_reefs.","",gsub ("ne_reefs.","",L.peixes_subset$eventID_MOD))) # edit in original table too

# replace NAs by 1, because there were bite detection in this places
L.peixes_subset$number_of_bites [is.na(L.peixes_subset$number_of_bites)] <- 1

# all species found in Longo et al.
todas_sp_longo <- unique (L.peixes_subset$ScientificName)

# then create one table site (list), species (rows) and video (cols) 
data_longo <- lapply (sitios_longo, function (i)
  
  cast(L.peixes_subset [which (L.peixes_subset$eventID_MOD == i),],
       formula = ScientificName  ~ video,
       value= "number_of_bites",
       fun.aggregate = sum,
       na.rm=T)
)

# adjust colnames, removing the word 'video' (except the 1st col -> species),
# and order each table
data_longo_adjusted <- lapply (data_longo, function (i) {
  
  list.L.peixes <- i$ScientificName # get sp ID
  cont_preliminar <- i[,-1] # rm the 1st col (species)
  colnames (cont_preliminar) <- gsub ("video","",colnames (cont_preliminar))# replacing 'video' by ''
  cont_preliminar<-cont_preliminar [,order (as.numeric (colnames (cont_preliminar)))]# cols' ordering
  colnames (cont_preliminar) <- seq (1,length(colnames(cont_preliminar)))# colnames along a sequence of 1 up to nvideos
  cont_preliminar <- cbind(list.L.peixes, cont_preliminar)# bind species ID
  
  ; # return
  cont_preliminar
  
}
)

# we need to bind cols for videos not deployed in some sites
maximo_videos <-  max(unlist (lapply (data_longo_adjusted,ncol)))+1  # find the max number of videos (+1 to avoid errors [after we will remove this additional video])
imput_longo_data <- lapply (data_longo_adjusted, function (i) # for each site
  
  (matrix (NA, # fill a matrix with NAs when  some videos are lacking
           nrow= nrow (i),
           ncol = maximo_videos - ncol (i), # +1 to avoid prob here (i.e. ncol =0 can not be bind)
           dimnames = list (i$list.L.peixes ))) # set dimnames (spp)
)

# now bind the matrix filled with NAs
imputed_longo_data <- lapply (seq (1, length (data_longo_adjusted)), function (i) # for each site
  
  cbind (data_longo_adjusted [[i]], # bind
         add = imput_longo_data [[i]])
)

# adjust colnames
imputed_longo_data <- lapply (imputed_longo_data, function (i) {# for each site table
  
  colnames (i)[-1] <- seq (1,length (colnames (i)[-1])) # set colnames
  
  ; # return
  i 
  
})
names (imputed_longo_data ) <- sitios_longo # set names to the list

# Until now, the data format is site (list), spp (row) and video number (col)
# Now we need to transform this into an 3D array
# We can do that using 'do.call' to melt the list and transform into data frame

list_sp_longo <- unique(unlist (lapply (imputed_longo_data, '[',1)))# list of fish spp
tabela_data_longo <- lapply (list_sp_longo, function (k) # for each species

  # melt and organize dimensions
  # now species will be in the list, sites in the rows, and n videos in the cols
    do.call (rbind,
      
            lapply (imputed_longo_data, function (i)
  
                  i [which(i$list.L.peixes == k),])
))

## cf dimensions
# length(list_sp_longo)# n species
# ncol(tabela_data_longo[[1]]) ## n videos
# nrow(tabela_data_longo[[1]]) ## n sites

# now we need to imput sites in the species dataset
# 'model table'
nvideos_site <- unlist (lapply (data_longo_adjusted,ncol))

## sites we need to imput- 0 = no observation of one species in sites that actually had video plots
imput_sitios <- lapply (seq(1,length(nvideos_site)), function (i)
  
  matrix(0, ncol=nvideos_site[i],nrow=1, # fill matrix of zeros
         dimnames = list(sitios_longo[i],# with dimnames
                         seq(1,nvideos_site[i]))))

# imput sites ( now for video that were not deployed into a site)
imput_sitios <- lapply (seq (1,length (imput_sitios)), function (i)

  cbind(imput_sitios[[i]], matrix (NA, 
                                   nrow=1,ncol = maximo_videos - ncol(imput_sitios [[i]]),
                                    dimnames =list(rownames(imput_sitios [[i]]),
                                              seq( ncol(imput_sitios [[i]])+1, maximo_videos))
                          )
)
)

imput_sitios <- do.call (rbind,imput_sitios)[,-14] # sites to imput (minus the additional col we set before)
# it is the 14st column
tabela_data_longo <- lapply (tabela_data_longo, function (i)
  # imput sites using R bind
  rbind (i[,-1], imput_sitios [which(rownames(imput_sitios) %in% rownames(i) == F),]) 
       
        )
names(tabela_data_longo) <- list_sp_longo # spp names in the list

# rows in alphabetic order
tabela_data_longo <- lapply(tabela_data_longo, function (i) {
  
  i [order(rownames(i)),]

  })

# finally list into 3D array [site,video,species]

arranjo_longo_sitio_video <-   array(unlist(tabela_data_longo ), 
                                         dim = c(nrow(tabela_data_longo [[1]]), 
                                                 ncol(tabela_data_longo [[1]]), 
                                                 length(tabela_data_longo )),
                                         dimnames = list (sitios_longo [order(sitios_longo)],
                                                          NULL,
                                                          list_sp_longo))

# transform into detection (1) and non-detection (0) data
arranjo_longo_sitio_video [arranjo_longo_sitio_video > 1] <- 1
#arranjo_longo_sitio_video <- arranjo_longo_sitio_video [,-13,] # remove the last video (no detection)

# --------------------------------------------
# observation covariates
# time
tempo<-lapply (L.peixes_subset$total_time, function (i) {
  minuto <- as.numeric (strsplit (gsub ("''","",i), "'") [[1]] [1])
  segundo <- as.numeric (strsplit (gsub ("''","",i), "'") [[1]] [2])
  tempo <- (minuto*60)+segundo
  ;
  tempo
  }
)

# bind time to data
L.peixes_subset$tempo <- unlist (tempo)

# find time per site and video
data_longo_tempo <- lapply (sitios_longo, function (i)
  # pivot table per site
  as.data.frame (cast(L.peixes_subset [which (L.peixes_subset$eventID_MOD == i),],
       formula = eventID_MOD  ~ video,
       value= "tempo",
       fun.aggregate = mean,
       na.rm=T)[,-1]
))

# find the max number of videos deployed
maximo_videos_longo <- max(unlist(lapply (data_longo_tempo,ncol)))
# to imputing data (as before)
df_tempo <- lapply (data_longo_tempo, function (i) {
  
  # ordering cols (video number)
  ordem <- i [,order (as.numeric(gsub ("a",".1", (gsub ("video","",colnames (i))))))]
  # imput NA for missing occasions
  imput <- matrix (NA, ncol = maximo_videos_longo+1 - ncol(i),
                   nrow = nrow(i),
                   dimnames = list (NULL,
                                    paste ("video", seq (ncol(i)+1,maximo_videos_longo+1),sep="")
                                    ))
  # imput
  ordem <- cbind(ordem,imput)
  # adjust colnames
  colnames(ordem) <- seq (1,ncol(ordem))
  ; # return
  ordem # ordered data
  
  
})
  
# list to df
df_tempo <- do.call (rbind, df_tempo)#[,-11] # remove col 11 with any info
# cf
# rowSums (df_tempo>0,na.rm=T) == rowSums (arranjo_longo_sitio_video[,,1]>=0,na.rm=T)

# find depth
data_longo_prof <- unlist(lapply (strsplit (sitios_longo, "\\."), "[[",3))

###################################################################

# Formating coral data to site occ modeling (not used)
# and matching with fish data from Longo et al.

##################################################################

sitios_bentos <- unique (L.bentos_subset$eventID_MOD)# sites with benthos' sampling
sitios_bentos <- gsub ("oc_isl.","",gsub ("se_reefs.","",gsub ("ne_reefs.","",sitios_bentos))) # adjust names
sitios_bentos <- sitios_bentos[match(sitios_longo, sitios_bentos)]# sites in both Aued et al. and Longo et al.
L.bentos_subset$eventID_MOD <- gsub ("oc_isl.","",gsub ("se_reefs.","",gsub ("ne_reefs.","",L.bentos_subset$eventID_MOD))) # edit ID in the original tab

# one pivot table per site, spp, and photoquadrat
cob_bentos <- lapply (sitios_bentos, function (i)
  
  cast(L.bentos_subset [which (L.bentos_subset$eventID_MOD == i),],
       formula = Taxon  ~ Video_number,
       value= "Cover",
       fun.aggregate = max)
)

# list of coral species we're interested in
sp_corais <- c("Agaricia.fragilis", "Agaricia.humilis", "Agaricia.sp","Favia.gravida",
               "Madracis.decactis", "Meandrina.brasiliensis","Millepora.alcicornis",
               "Millepora.nitida",
               "Montastraea.cavernosa", "Mussismilia.harttii", "Mussismilia.braziliensis",
               "Mussismilia.hispida", "Mussismilia.leptophylla", "Porites.astreoides",
               "Porites.branneri", "Siderastrea.spp") # did not consider "Millepora.sp", "Millepora.incrusting", and  "Porites.sp"

## cover
range(L.bentos_subset [which (L.bentos_subset$Taxon %in% sp_corais),"Cover"])

# things to report in the paper
summary_stat <- summary(

  apply ( 
  
  cast (L.bentos_subset[which (L.bentos_subset$Taxon %in% sp_corais),],
      formula = eventID_MOD ~ Taxon,
      value="Cover",
      fun.aggregate = max),1,max # sd
  )
)


# Fig. S1.1


hist(
  
  apply ( 
    
    cast (L.bentos_subset[which (L.bentos_subset$Taxon %in% sp_corais),],
          formula = eventID_MOD ~ Taxon,
          value="Cover",
          fun.aggregate = max),1,max # sd
  )*100, xlab="Coral cover (%)",ylab="Frequency (number of sites)",main=""
)

abline (v=summary_stat[4]*100,lwd=3,col="black")
#abline (v=summary_stat[2]*100,lwd=3,col="gray",lty=2)
#abline (v=summary_stat[5]*100,lwd=3,col="gray",lty=2)

# get coral cover data
cob_bentos <- lapply (cob_bentos, function (i)
  
  i [which (i$Taxon %in% sp_corais),]
  
)

# adjust colnames (rm the word "Video" (except the first col -> spp name))
# also set in order

cob_bentos <- lapply (cob_bentos, function (i) {
  
  corais <- i$Taxon# save spp id
  cob_preliminar <- i[,-1]# rm spp from the table
  colnames (cob_preliminar) <- gsub ("Video","",colnames (cob_preliminar))# replace the word 'video' by ''
  cob_preliminar<-cob_preliminar [,order (as.numeric (colnames (cob_preliminar)))] # ordered cols
  colnames (cob_preliminar) <- seq (1,length(colnames(cob_preliminar))) # set colnames -> 1: nvideos
  cob_preliminar <- cbind(corais, cob_preliminar) # bind spp ID
  
  ;# return
  cob_preliminar
  
}
)

# bind missing videos
maximo_videos <-  max(unlist (lapply (cob_bentos,ncol)))+1 # + 1 to avoid issues
imput_coral_data <- lapply (cob_bentos, function (i)
  
  (matrix (NA,  # fill matrix with NAs -> videos that were not deployed
           nrow= nrow (i),
           ncol = maximo_videos - ncol (i),
           dimnames = list (i$corais)))
)

# imput
imputed_coral_data <- lapply (seq (1, length (cob_bentos)), function (i)
  
  cbind (cob_bentos [[i]],
         add = imput_coral_data [[i]])
)

# adjust colnames
imputed_coral_data <- lapply (imputed_coral_data, function (i) {
  
  colnames (i)[-1] <- seq (1,length (colnames (i)[-1]))
  
  ; i ## return i 
  
})

# as done for fish, we transform the list of sites per species x video into 
# species per site x video
# to then transform into an 3D array
lista_coral_sitio <- lapply (seq (1,length(sp_corais)), function (k) 
  do.call(rbind, lapply (imputed_coral_data, function (i)
    
    i [k,]
  )))

## cf dims
# length(lista_coral_sitio)# n species
# ncol(lista_coral_sitio [[1]]) ## numero videos
# nrow(lista_coral_sitio [[1]]) ## numero sites

names(lista_coral_sitio) <- sp_corais # name list elements

# rm 1st col of each table (spp name)
tab_completa_site_ocasiao_coral <-  lapply (lista_coral_sitio, function (i) 
  
  i[,-1]
  
)
names (tab_completa_site_ocasiao_coral ) <- sp_corais # name list again

# list into array [site,video,species]

arranjo_cob_coral_sitio_video <-   array(unlist(tab_completa_site_ocasiao_coral), 
                                         dim = c(nrow(tab_completa_site_ocasiao_coral[[1]]), 
                                                 ncol(tab_completa_site_ocasiao_coral[[1]]), 
                                                 length(tab_completa_site_ocasiao_coral)),
                                         dimnames = list (sitios_bentos,
                                                          NULL,
                                                          sp_corais))

# summed cover from Agaricia (could not be identified in the field)
agaricia_sp_cover <- apply (arranjo_cob_coral_sitio_video [,,grep("Agaricia", sp_corais)],
                      c(1,2), sum)

## bind in the array of most frequent coral spp
arranjo_cob_coral_sitio_video <- abind(agaricia_sp_cover, arranjo_cob_coral_sitio_video )
dimnames(arranjo_cob_coral_sitio_video)[[3]][1] <- "Agaricia.spp" # adjust names accordingly

# transform cover into binary data (1 if cover >0 of coral k, site i, video j)
arranjo_corais <- arranjo_cob_coral_sitio_video
arranjo_corais <- arranjo_corais [,-16,]# rm col 16 -> no data

# --------------------------------------------------------------------------------------
# coral cover relative to total coral cover per photoquadrat
# Predictor variable in site occupancy models

cover_data <- apply(arranjo_corais,c(1,2),sum,na.rm=T)  # obtain total coral cover in video k, site i

# obtain relative cover, using for =[
shell_array <- array (NA,dim = dim(arranjo_corais)) 

for (i in 1:dim(arranjo_corais)[3]) {
  
  shell_array[,,i] <- arranjo_corais [,,i]/cover_data # cover of each spp divided by total coral cover
  
}

shell_array [is.nan(shell_array)] <- 0 ## Nan into zero, as 0/0 = NaN

# maximum cover in any quadrat
sp_cover_data <- apply (shell_array,c(1,3),mean,na.rm=T) ## NA because some sites lack coral cover
colnames(sp_cover_data) <- dimnames(arranjo_corais )[[3]]
rownames(sp_cover_data)<- dimnames(arranjo_corais )[[1]]

# rm species that occurred in less than 20% of the sites
site_coral_detection <- ifelse (sp_cover_data>0,1,0) # cover to binary data
min_sites <- round(nrow(site_coral_detection)*0.2) # spp with enough data
sp_cover_data <- sp_cover_data[,which(colSums (site_coral_detection)>=min_sites)] # subsetting 
## descriptive statistics
round (apply (sp_cover_data,2,mean,na.rm=T),2)
round (apply (sp_cover_data,2,sd,na.rm=T),2)
round (apply (sp_cover_data,2,range,na.rm=T),2)

## subset in the array too
arranjo_corais <- arranjo_corais [,,which(colSums (site_coral_detection)>=min_sites)]
nvideos <- rowSums(is.na(arranjo_corais[,,1])!=T)## number of videos per site (useful somewhere)

# finally, array into long format 
df_coral_data <- lapply (seq(1,dim(arranjo_corais)[3]), function (sp) {
  
  ## matrix into a vector (per spp)
  y_long <- as.numeric(arranjo_corais [,,sp])
  
  ## each row will be a sampling occasion
  df_data <- data.frame(obs= seq(1,length(y_long)),
                        y= y_long,
                        M = rep (seq (1,dim(arranjo_corais)[1]), 
                                 ncol(arranjo_corais)),
                        J = unlist(
                          lapply (seq(1,ncol(arranjo_corais)), function (i) 
                            rep (i,dim(arranjo_corais)[1]))
                        )
  )
  
  df_data <- df_data[which (is.na(df_data$y) != T),]## remove NAs -> work with observations
  df_data$n.obs <- seq (1,nrow(df_data)) ## new obs - sequence disregarding NAs
  ## remove zeros
  df_data <- df_data [which(df_data$y >0),]
  df_data$n.obs <- seq (1,nrow(df_data)) ## new obs - sequence disregarding zeros
  
  ;# return
  df_data  # data in long format
}
)

## Obtain geographic coordinates
## NAs are produced because aggregation of factors is not possible
coordenadas <- aggregate(L.bentos_subset, 
                         by= list (L.bentos_subset$eventID_MOD), 
                         FUN=mean)[c("Group.1","Lon","Lat")]

coordenadas <- coordenadas [match(sitios_bentos,coordenadas$Group.1),]# order based on sites with benthos sampling

# -------------------------------------------------
# Finally, we match fish and coral data
# avoid spatial mismatch between species

site_peixe_detection <- apply(arranjo_longo_sitio_video,c(1,3),sum,na.rm=T) # sites with fish detection
site_coral_detection <- ifelse (sp_cover_data>0,1,0)# sitse with coral detection
# which corals occur in a min of sites
sp_cover_data <- sp_cover_data [,which(colSums (site_coral_detection)>=min_sites)]

# which fishes share these "min_sites" with coral observation
over_peixe_coral <- lapply (seq(1,ncol (site_coral_detection)), function (k) # for each coral
  
  unlist( # unlist of fish level
    
    lapply (seq (1,ncol (site_peixe_detection)), function (i) # and fish
      ## check if the fish was detected in the "min sites" with coral detection
      table(which (site_peixe_detection[,i] ==1) %in% which (site_coral_detection[,k] ==1))[2] >= min_sites
      
    )))

## finally we select species with spatial match
subset_peixes <- lapply (over_peixe_coral, function (i)
  
  arranjo_longo_sitio_video [,,which (is.na(i)!=T)]
  
)

# then transform data into long format data frame
df_fish_data <- lapply (subset_peixes, function (coral) ## for each coral spp
  lapply (seq(1,dim(coral)[3]), function (sp) { ## and fish
    
    ## matrix into vector
    y_long <- as.numeric(coral [,,sp])# observation
    tabela_tempo <- as.numeric (as.matrix(df_tempo)) # recording time
    
    ## sampling occasions
    df_data <- data.frame(obs= seq(1,length(y_long)),
                          y= y_long,
                          time = as.numeric(tabela_tempo),
                          M = rep (seq (1,dim(coral)[1]), 
                                   ncol(df_tempo)),
                          J = unlist(
                            lapply (seq(1,ncol(df_tempo)), function (i) 
                              rep (i,dim(coral)[1]))
                          ),
                          prof = as.numeric(ifelse (data_longo_prof =="fundo", 2,1))
    )
    
    ## remove NAs
    df_data <- df_data[which (is.na(df_data$time) != T),]
    
    ; # return
    df_data # long format data
    
  }
  ))

## finally, a list of long formats (across coral and fish spp)

df_fish_data_per_coral <- lapply (seq (1,length(df_fish_data)), function (i){
  
  ## create an empty array with dims given by long format data
  empty_array <- array (NA, dim=c(nrow(df_fish_data[[i]][[1]]), # no difference if it is [[1]] or e.g., [[5]] -> all dims are equal
                                  ncol (df_fish_data[[i]][[1]]),
                                  length(df_fish_data[[i]])),
                        dimnames = list(NULL, 
                                        colnames(df_fish_data[[i]][[1]]),NULL))
  # fill array
  for (k in 1:dim(empty_array)[3]) {
    
    empty_array [,,k] <- as.matrix(df_fish_data[[i]][[k]])
    
  }
  filled_array <- empty_array
  ; # return this filled array
  filled_array
  
}
)

# get final species list 
coral_species <- colnames(site_coral_detection)# corals
fish_species <- lapply (subset_peixes, function (i){ # fishes
   
  nomes <- dimnames(i)[3]  
  ;nomes[[1]]
  
  })


## basic statistics

mean((df_fish_data_per_coral[[1]][,"time",1])/60)
sd((df_fish_data_per_coral[[1]][,"time",1])/60)
range((df_fish_data_per_coral[[1]][,"time",1])/60)

## av number of det per fish

mean (
  unlist(
    lapply (seq (1,length(df_fish_data_per_coral)), function (k)
      
      lapply (seq (1,dim(df_fish_data_per_coral[[k]])[3]), function (i)
        
        sum(df_fish_data_per_coral[[k]][,"y",i])
      )
    )
  )
)


sd (
  unlist(
    lapply (seq (1,length(df_fish_data_per_coral)), function (k)
      
      lapply (seq (1,dim(df_fish_data_per_coral[[k]])[3]), function (i)
        
        sum(df_fish_data_per_coral[[k]][,"y",i])
      )
    )
  )
)

## find most detected spp
most_det <- unlist(lapply (seq (1,length(df_fish_data_per_coral)), function (k)
  lapply (seq (1,dim(df_fish_data_per_coral[[k]])[3]), function (i)
    
    sum(df_fish_data_per_coral[[k]][,"y",i])
    
  )))
# put into a df
most_det <- data.frame (ndet=most_det,
                        sp = unlist(fish_species))

most_det <- most_det[order(most_det$ndet,decreasing=T),] # order
write.table (most_det, file = here("output_comm_wide","most_detVIDEO.csv")) # and save

  
###############################################
# # # # # # # # #  SAVE # # # # # # # # # # # #
###############################################

### save data - for coral cover modeling

save (arranjo_corais, ### cover data
      df_coral_data, ## long format with data
      sp_cover_data, ## cover of each coral relative to total coral cover, per site
      nvideos, ## number of videos per site
      coordenadas,# coordiantes
      sitios_bentos,## site names
      coral_species,## id of analyzed corals
      file=here ("output_comm_wide","Data_coral_detection_LONGO_AUED.RData"))

### save data -> fish site-occupancy modeling

save (subset_peixes, # array of fishes after spatial matching checking
      df_fish_data_per_coral,## fish data for modeling
      sitios_longo, # site name
      fish_species,## id of analyzed fish, per coral spp
      todas_sp_longo,## id of all fishes detected by Longo et al. 2019
      file = here("output_comm_wide", "Data_fish_detection_LONGO_AUED.RData"))

