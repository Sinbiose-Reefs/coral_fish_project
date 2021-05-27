## code with routine for result interpretation

# upper part, occupancy
# inner part, detection

# load packages and functions
source ("R/packages.R")
source ("R/functions.R")

# ---------------------# 
# MAP (figure 1)
# ---------------------#

# ---------------------# 
# fish data  -  Morais
# ---------------------# 

# mapa mundi
world <- ne_countries(scale = "medium", returnclass = "sf")

# cortar o mapa para ver a america do Sul e parte da central
wm <- ggplot() + 
  geom_sf (data=world, size = 0.1, 
           fill= "gray90",colour="gray90") +
  coord_sf (xlim = c(-50, -30),  ylim = c(-27, 2), expand = FALSE) +
  theme_bw() + #xlab ("Longitude")  + ylab ("Latitude") +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "lightcyan",#darkslategray1
                                        colour = "lightcyan"),
        axis.text.x = element_text(size=6),
        axis.ticks.x=element_line(size=1),
        axis.text.y = element_text(size=6),
        axis.ticks.y=element_line(size=1),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        title = element_blank()) 

# ---------------------# 
# fish data  -  LONGo
# ---------------------# 

load (here("output","Data_fish_detection_LONGO_AUED.RData"))

## coral detection - with coordinates - LONGO
load (here("output","Data_coral_detection_LONGO_AUED.RData"))

# col for none cover
sp_cover_data_longo <- cbind(sp_cover_data,
                              None=ifelse (rowSums (sp_cover_data) ==0, 1,0))

# jittered coordinates
sp_cover_data_longo <- cbind (sp_cover_data_longo,
                               LonJitter = jitter (coordenadas$Lon,factor=400),
                               LatJitter=jitter (coordenadas$Lat,factor=600))
# transforming into DF
sp_cover_data_longo <- as.data.frame(sp_cover_data_longo)

# discounting a bit of long to longo's data
sp_cover_data_longo$LonJitter <- sp_cover_data_longo$LonJitter# + 6

## advice to jitter : https://stackoverflow.com/questions/52806580/pie-charts-in-geom-scatterpie-overlapping
## pie: http://www.spectdata.com/index.php/2018/10/25/how-to-use-ggplot-to-plot-pie-charts-on-a-map/

# pie chart with sites of Morais
wm_pie_longo <- wm + geom_scatterpie(aes(x=LonJitter, y=LatJitter),
                               data = sp_cover_data_longo,
                               cols = c("Millepora.alcicornis",
                                        "Mussismilia.hispida",
                                        "Porites.astreoides",
                                        "Siderastrea.spp",
                                        "None"),
                               pie_scale = 2,
                               size=0.3,
                               alpha = 0.65,
                               color="black",
                               sorted_by_radius = F,
                               legend_name = "Corals") + 
  
  theme (legend.title = element_text(size=8),
         legend.text = element_text(size=7),
         legend.position = c(.35, .7),
         legend.justification = c("right", "top"),
         legend.box.just = "right",
         legend.margin = margin(6,6,6,6),
         legend.background = element_blank(),
         title=element_text(size=8)) 


wm_pie_longo <- wm_pie_longo + scale_fill_manual(
  
    values= c("Millepora.alcicornis" = "#ca0020",
               "Montastraea.cavernosa" = "#dfc27d",
               "Mussismilia.hispida" = "#0571b0",
               "Porites.astreoides" = "#FF8000",
               "Siderastrea.spp" = "#FCBDBD",
               "None" = "#FFFFFF"))

wm_pie_longo <- wm_pie_longo + #ggtitle("Species cover relative to total coral cover")+
  xlab("Longitude") + ylab("Latitude")

pdf(here ("output", "Vect","MapPoints_longo.pdf"),width = 10,heigh=7)
  wm_pie_longo
dev.off()

# ---------------------------------------- #
# information of fish species
# ---------------------------------------- #

#load (here("output","Data_fish_detection_MORAIS_AUED.RData"))
#sp_morais <- unique(unlist(fish_species))

load (here("output","Data_fish_detection_LONGO_AUED.RData"))
sp_longo <- unique(unlist(fish_species))

# 
tab_spp <- data.frame (sp = sp_longo)#unique(c(sp_morais, sp_longo)))
#tab_spp$video <- ifelse (tab_spp$sp %in% sp_longo, 1,0)
#tab_spp$UVS <- ifelse (tab_spp$sp %in% sp_morais, 1,0)
#write.csv (tab_spp, file =here("output","tab_spp.csv"))
# quantas em comum?
#table(sp_morais %in% sp_longo )

## load trait data
traits_peixes <- read.csv(here("data","traits","Atributos_especies_Atlantico_&_Pacifico_Oriental_2020_04_28.csv"),
                          h=T,sep=";")
traits_peixes$Name <- tolower (gsub (" ",".", traits_peixes$Name))
traits_peixes$Body_size <-  gsub(",",".",traits_peixes$Body_size)
traits_peixes$Body_size <- as.numeric(traits_peixes$Body_size)
traits_peixes$Aspect_ratio <- as.numeric( gsub(",",".",traits_peixes$Aspect_ratio))

## stat
# size
mean (traits_peixes [which(traits_peixes$Name %in% tab_spp$sp),"Body_size"])
sd (traits_peixes [which(traits_peixes$Name %in% tab_spp$sp),"Body_size"])
range_size <- range (traits_peixes [which(traits_peixes$Name %in% tab_spp$sp),"Body_size"])

traits_peixes [which(traits_peixes$Name %in% tab_spp$sp & 
                       traits_peixes$Body_size == range_size[1]),"Name"]
traits_peixes [which(traits_peixes$Name %in% tab_spp$sp & 
                       traits_peixes$Body_size == range_size[2]),"Name"]

# group
table(traits_peixes [which(traits_peixes$Name %in% tab_spp$sp),"Size_group"])
# diet
table(traits_peixes [which(traits_peixes$Name %in% tab_spp$sp),"Diet"])

## table with fish data

traits_peixes_table <- traits_peixes [which(traits_peixes$Name %in% tab_spp$sp),c("Family",
                                                           "Name",
                                                           "Body_size",
                                                           "Diet",
                                                           "Home_range",
                                                           "Size_group",
                                                           "Level_water",
                                                           "Diel_activity",
                                                           "Aspect_ratio",
                                                           "Trophic_level",
                                                           "IUCN_status"
                                                           )]
write.csv (traits_peixes_table,
           file=here("output","Tabs","trait_table.csv"))


# ------------------------------------------------- #  
# ------------------------------------------------- #
# The same  for Longo et al. 
# ------------------------------------------------- #  
# ------------------------------------------------- #

# load packages and functions
source ("R/packages.R")
source ("R/functions.R")

# load basic data for naming the table dims
## fish data
load (here("output","Data_fish_detection_LONGO_AUED.RData"))

## coral detection - with coordinates
load (here("output","Data_coral_detection_LONGO_AUED.RData"))

## LOAD MODEL RESULTS
load(here("output","samples_OCCcoral_PdepthTime_longo.RData")) 

## LOAD MODEL RESULTS
load(here("output","samples_OCCcoral_PdepthTime_longo_RdmP.RData")) 

# ---------------------------------------- #
# FIGURE 2, FISH RELIANCE ON CORAL COVER
# ---------------------------------------- #

## load trait data
traits_peixes <- read.csv(here("data","traits","Atributos_especies_Atlantico_&_Pacifico_Oriental_2020_04_28.csv"),
                          h=T,sep=";")
traits_peixes$Name <- tolower (gsub (" ",".", traits_peixes$Name))
traits_peixes$Name <- gsub("\\."," ", paste0(toupper(substr(traits_peixes$Name, 1, 1)), 
                                             substr(traits_peixes$Name, 2, 100)))

# adjusting trait values
traits_peixes$Body_size <- as.numeric(gsub (",",".",traits_peixes$Body_size))
traits_peixes$Aspect_ratio <- as.numeric(gsub (",",".",traits_peixes$Aspect_ratio))
traits_peixes$Trophic_level <- as.numeric(gsub (",",".",traits_peixes$Trophic_level))

# replacing NAs in aspect ratio by genera average
## finding genera
genera <- unlist(
  lapply (
    strsplit (traits_peixes$Name [which(is.na(traits_peixes$Aspect_ratio))]," "),"[",1)
  )

# calculate the average for species with congenera 
mean_av_ratio <- unlist(lapply (genera, function (i)
  mean (traits_peixes [grep (i , traits_peixes$Name),"Aspect_ratio"],na.rm=T)
))
# imput in the table
traits_peixes[which(is.na(traits_peixes$Aspect_ratio)),"Aspect_ratio"] <- mean_av_ratio

traits_peixes [which(tolower (gsub(" ",".",traits_peixes$Name)) %in% tab_spp$sp),"Aspect_ratio"]

### -------------------- #
###  assessing model fit
### -------------------- #

bpv_video <- lapply (seq (1,length(samples_OCCcoral_PdepthTime_longo)), function (i) 
  do.call (rbind, lapply(seq(1,length (fish_species[[i]])), function (k) {
    
    bpvID<- sum (samples_OCCcoral_PdepthTime_longo[[i]]$sims.list$Chi2repClosed[,k] > samples_OCCcoral_PdepthTime_longo[[i]]$sims.list$Chi2Closed[,k])/length(samples_OCCcoral_PdepthTime_longo[[i]]$sims.list$Chi2Closed[,k])
    bpvRdmP<- sum (samples_OCCcoral_PdepthTime_longo_RdmP[[i]]$sims.list$Chi2repClosed[,k] > samples_OCCcoral_PdepthTime_longo_RdmP[[i]]$sims.list$Chi2Closed[,k])/length(samples_OCCcoral_PdepthTime_longo_RdmP[[i]]$sims.list$Chi2Closed[,k])
    res <- data.frame (sp = gsub ("\\."," ",paste0(toupper(substr(fish_species[[i]][k], 1, 1)), 
                                                   substr(fish_species[[i]][k], 2, 100))),
                       mID= round(bpvID,3),
                       mRdmP=round(bpvRdmP,3));
    res
  })))

## data to use in the plot
extracted_data <- 
  
  lapply (seq(1,length(coral_species)), function (coral)
    
    do.call(rbind,lapply (seq (1, length (fish_species [[coral]])), function (fish)
      
      data.frame (
        
        coral = coral_species[coral],
        
        peixe = gsub("\\."," ", paste0(toupper(substr(fish_species [[coral]][fish], 1, 1)), 
                                       substr(fish_species [[coral]][fish], 2, 100))),
        
        intercept = mean (samples_OCCcoral_PdepthTime_longo_RdmP[[coral]]$sims.list$intercept.psi [,fish]),
        
        low.int = quantile (samples_OCCcoral_PdepthTime_longo_RdmP[[coral]]$sims.list$intercept.psi [,fish], 0.05),
        
        high.int = quantile (samples_OCCcoral_PdepthTime_longo_RdmP[[coral]]$sims.list$intercept.psi [,fish], 0.95),
        
        estimate = mean (samples_OCCcoral_PdepthTime_longo_RdmP[[coral]]$sims.list$beta1 [,fish]),
        
        low = quantile (samples_OCCcoral_PdepthTime_longo_RdmP[[coral]]$sims.list$beta1 [,fish], 0.05),
        
        high = quantile (samples_OCCcoral_PdepthTime_longo_RdmP[[coral]]$sims.list$beta1 [,fish],0.95)
        
        
      )
    )
    )
  )

## binding the bayeasian P- value
extracted_data<- lapply (seq(1,length(extracted_data)), function (i)
  cbind (extracted_data[[i]],bpv_video [[i]])
)

## plots

sp_analyzed <- lapply (seq (1,length(extracted_data)), function (i) {
  
  teste <- extracted_data[[i]]
  
  ## organizar nomes das spp no eixo Y
  ## subset de millepora 
  subset1 <- traits_peixes[which(traits_peixes$Name %in% unique (teste$peixe)),c("Name", "Body_size", "Diet")]
  subset1 <- subset1 [order (subset1$Body_size,decreasing=F),]
  # organizing species according to size
  teste <- teste[order(match(teste$peixe,subset1$Name)),]
  teste$peixe <- gsub("\\."," ", paste0(toupper(substr(teste$peixe, 1, 1)), substr(teste$peixe, 2, 100)))
  teste$peixe <- factor (teste$peixe,
                         levels = unique(teste$peixe))
  
  ## removing species with negative coefficient
  rem_sp_neg <- teste [which(teste$estimate < 0 & teste$high < 0),"peixe"]
  ## removing too imprecise estimates
  rem_sp_imp <- teste [which (abs(teste$low -   teste$high) >= 20),"peixe"]
  ## removing
  rem_sp <-c(as.character(rem_sp_imp),as.character(rem_sp_neg))
  teste <- teste [which(teste$peixe %in% rem_sp != T),]
  
  # plot
  dodge <- c(0.2,0.2)
  pd <- position_dodge(dodge[i])
  
  a <- ggplot (teste, aes  (y=peixe, x=estimate, fill=coral,
                            colour=coral)) + 
    geom_errorbar(aes(xmin=low,xmax=high),width = 0,
                  position=pd) + theme_classic() + 
    geom_point()+ 
    geom_vline(xintercept = 0, linetype="dashed", 
               color = "gray50", size=0.5)+
    scale_color_manual(values=c("gray70", "gray60", "gray50",
                                "gray40","black")) + 
    xlab("Regression coefficient estimate") + 
    ylab ("Reef fish species") + 
    xlim(-5, 17.20) + 
    ggtitle (coral_species[[i]]) + 
    theme (legend.position = "none")
  
  
  # regression shapes
  
  data_shape <- lapply (c (-8,-2,0,2,8), function (i)
    
    data.frame (cc=seq(-2,2,0.1),
                rel=plogis ( 0 + i * seq(-2,2,0.1)))
  )
  
  plot_eff <- lapply (data_shape, function (i)
    
    ggplot (i, aes (x=cc,y=rel)) + 
      geom_line(size=2) + theme_classic() + 
      theme (axis.title = element_blank(),
             axis.text = element_blank(),
             axis.ticks = element_blank()) 
  )
  
  
  pdf (file=here("output", "Vect",paste (i,"longob.pdf",sep="_")),width=4,heigh=3)
  grid.arrange(
    plot_eff [[1]],
    plot_eff [[2]], 
    plot_eff [[3]],
    plot_eff [[4]],
    plot_eff [[5]],
    a,
    ncol=9,nrow=7, 
    layout_matrix = rbind (c(NA,NA,1,2,3,4,5,NA,NA),
                           c(6,6,6,6,6,6,6,6,6),
                           c(6,6,6,6,6,6,6,6,6),
                           c(6,6,6,6,6,6,6,6,6),
                           c(6,6,6,6,6,6,6,6,6),
                           c(6,6,6,6,6,6,6,6,6),
                           c(6,6,6,6,6,6,6,6,6)))
  
  grid.text("% of coral cover", 
            x = unit(0.5, "npc"), 
            y = unit(.86, "npc"),gp = gpar(fontsize=8))
  
  grid.text("Site-occupancy\nprobability",# 
            x = unit(0.21, "npc"), 
            y = unit(.94, "npc"),
            gp = gpar(fontsize=8),
            rot=90)
  
  dev.off() 
  
  list_res <- list (rem_sp = rem_sp,
                    teste = teste)
  
  ;
  
  ## things to report
  list_res
  
})

write.table (do.call(rbind, sapply (sp_analyzed,"[","teste")),
           file=here("output","Tabs","coefs_fit_longo.txt"),sep=";")

## barplot 
bar_plot_data <-lapply (extracted_data, function (i)
  
  data.frame (
      ## positive effect
      Positive = ifelse(length(table (i$low >0 ))==1,
                        0,
                        (table (i$low >0 ))[2]),
                         
      ## no effect
      No= ifelse(length(table (i$low <0 &  i$high >0))==1,
               0,
               (table (i$low <0 &  i$high >0))[2]),
                   
      ## negative effect
      Negative=ifelse(length(table (i$low <0 & i$high <0))==1,
                      0,
                      (table (i$low <0 & i$high <0))[2]),
      
      # total number of spp
      nSP = sum(table (i$low >0))
                       
      )
)

pdf(here ("output","Vect","barplotEffect.pdf"),height=5,width=5)
par (mfrow=c(2,2))
# mi al
barplot(as.numeric(bar_plot_data[[1]])[-4]/as.numeric(bar_plot_data[[1]])[4]*100,
        names.arg = c("Positive", "No", "Negative"),
        xlab = "Coral Effect",ylab="Percentage of species",
        col = c("black","gray80","gray60"),
        ylim=c(0,100),
        cex.axis=0.8)
# mus his
barplot(as.numeric(bar_plot_data[[2]])[-4]/as.numeric(bar_plot_data[[2]])[4]*100,
        names.arg = c("Positive", "No", "Negative"),
        xlab = "Coral Effect",ylab="Percentage of species",
        col = c("black","gray80","gray60"),
        ylim=c(0,100),
        cex.axis=0.8)
# por astr
barplot(as.numeric(bar_plot_data[[3]])[-4]/as.numeric(bar_plot_data[[3]])[4]*100,
        names.arg = c("Positive", "No", "Negative"),
        xlab = "Coral Effect",ylab="Percentage of species",
        col = c("black","gray80","gray60"),
        ylim=c(0,100),
        cex.axis=0.8)
# sid spp
barplot(as.numeric(bar_plot_data[[4]])[-4]/as.numeric(bar_plot_data[[4]])[4]*100,
        names.arg = c("Positive", "No", "Negative"),
        xlab = "Coral Effect",ylab="Percentage of species",
        col = c("black","gray80","gray60"),
        ylim=c(0,100),
        cex.axis=0.8)
dev.off()
# ------------------------------------------------------- #
# functional spaces
# here considering that all fish species with influence 
# of corals would disappear in the future
# ------------------------------------------------------ #

# a complete functional space per species of coral

total <- do.call (rbind,extracted_data)

f.space <- lapply (unique (total$coral), function(k) {
  
  # dfish traits
  subset1 <- traits_peixes [which(traits_peixes$Name %in% 
                                    unique(total$peixe)),
                            c("Name","Body_size", 
                              "Size_group",
                              "Aspect_ratio",
                              "Trophic_level")]
  
  subset1$Size_group <- sapply(subset1$Size_group , function(x) {
    if (x=="sol") {1} 
    else if (x=="pair") {2} 
    else if (x=="smallg") {3} 
    else if (x=="medg") {4} 
    else if (x=="largeg") {5}}
  )
  
  subset1$Size_group <-ordered (subset1$Size_group)
  rownames(subset1) <- subset1$Name; subset1<- subset1[,-1]
  subset1 <- subset1[which(is.na(subset1$Aspect_ratio)==F),]
  
  # first calculate gower distance on traits
  gower_matrix <- daisy (subset1, metric=c("gower")) 
  
  # Building the functional space based on a PCOA 
  pco<-dudi.pco(quasieuclid(gower_matrix), scannf=F, nf=10) # quasieuclid() transformation to make the gower matrix as euclidean. nf= number of axis 
  #barplot(pco$eig) # barplot of eigenvalues for each axis 
  (Inertia2<-(pco$eig[1]+pco$eig[2]) /(sum(pco$eig))) # percentage of inertia explained by the two first axes
  ## only the frst axis
  Inertia.first <- (pco$eig[1]) /(sum(pco$eig))
  ## only the frst axis
  Inertia.scnd <- (pco$eig[2]) /(sum(pco$eig))
  
  ## complete space
  all <- cbind (pco$li[,1:2],ext = F)
  
  a <- all [chull(all[,1:2], y = NULL),]
  #a [order(a$A1,decreasing=T),]
  #a [order(a$A2,decreasing=T),]
  
  ## extracted data of impaired species of each coral spp
  subset_coral <- total[which(total$coral == k),] 
  fuck_sp <- subset_coral [which(subset_coral$low >0),"peixe"]
   
  # reduced space
  setB<-cbind(all, ext1=ifelse(rownames(all) %in% fuck_sp,T,F))
  pk <-setB[which(setB$ext1==F),]
  f <- pk [chull(pk, y = NULL),]
  
  ## quantifying reduction in functional space
  
  # https://chitchatr.wordpress.com/2015/01/23/calculating-the-area-of-a-convex-hull/
  chull.poly.complete <- Polygon(a[,1:2], hole=F)
  chull.area.complete <- chull.poly.complete@area
  
  ## if it is not possible to calculate funct space, then report NA
  if (length(fuck_sp) < 2) {
    red.space <- data.frame (exc=NA, 
                             comp=chull.area.complete, 
                             red=chull.area.complete)
  }   else { 
    chull.poly.exc <- Polygon(f[,1:2], hole=F)
    chull.area.exc <- chull.poly.exc@area
    ## calculate the diff aftering excluding coral-reliant species
    ## how much the complete space is larger than the excluded one
    red.space <- data.frame (exc=chull.area.exc, 
                             comp=chull.area.complete, 
                             red=chull.area.exc/chull.area.complete)
  }
  
  ## plot A
  plotA <- ggplot(a, aes(A1, A2)) + 
    geom_point() + theme_bw()+
    geom_polygon(data=a, aes (A1,A2),alpha=0.5,fill="gray") + 
    geom_polygon(data=f, aes (A1,A2,group=ext1, fill=ext1),alpha=0.5,
                 fill="black",size=3) +
    xlim(min (a$A1)-0.2,max (a$A1)+0.2)
  
  
  ## correlations
  subset1$Size_group <- as.numeric (subset1$Size_group)
  correlations <- cor (pco$li[is.na(subset1$Aspect_ratio) !=T,1:2],
                       subset1[is.na(subset1$Aspect_ratio) !=T,])
  
  ## plotting 
  
  plotA + geom_segment(aes(x = 0, y = 0, 
                           xend = correlations[1,1]*0.2, 
                           yend = correlations[2,1]*0.2),size = 1,
                       arrow = arrow(length = unit(.35, "cm")))  + 
    ## annotate
    annotate(geom="text",x=correlations[1,1]*0.25,
             y=correlations[2,1]*0.25,label="Body size") +
    
    geom_segment(aes(x = 0, y = 0, 
                     xend = correlations[1,2]*0.2, 
                     yend = correlations[2,2]*0.2),size = 1,
                 arrow = arrow(length = unit(.35, "cm"))) + 
    annotate(geom="text",x=correlations[1,2]*0.25,
             y=correlations[2,2]*0.25,label="Size group") +
    
    geom_segment(aes(x = 0, y = 0, 
                     xend = correlations[1,3]*0.2, 
                     yend = correlations[2,3]*0.2),size = 1,
                 arrow = arrow(length = unit(.35, "cm"))) + 
    annotate(geom="text",x=correlations[1,3]*0.25,
             y=correlations[2,3]*0.25,label="Aspect ratio") +
    
    geom_segment(aes(x = 0, y = 0, 
                     xend = correlations[1,4]*0.2, 
                     yend = correlations[2,4]*0.2),size = 1,
                 arrow = arrow(length = unit(.35, "cm"))) + 
    annotate(geom="text",x=correlations[1,4]*0.25,
             y=correlations[2,4]*0.25,label="Trophic level")
  
  ggsave(here ("output","Vect",filename = paste ("Fspace_longo_allcorals",k,".pdf")), 
         width = 4,height=4) 
  
  ## things to report
  res <- list (space = red.space,
               first.axis = Inertia.first,
               scnd.axis = Inertia.scnd,
               fuck_sp = fuck_sp)
  ;
  
  res
  
})

f.space


# global space (binding all fishes and removing all reliant)

# a complete functional space per species of coral

total <- do.call (rbind,extracted_data)

# dfish traits
subset1 <- traits_peixes [which(traits_peixes$Name %in% 
                                  unique(total$peixe)),
                          c("Name","Body_size", 
                            "Size_group",
                            "Aspect_ratio",
                            "Trophic_level")]

subset1$Size_group <- sapply(subset1$Size_group , function(x) {
  if (x=="sol") {1} 
  else if (x=="pair") {2} 
  else if (x=="smallg") {3} 
  else if (x=="medg") {4} 
  else if (x=="largeg") {5}}
)

subset1$Size_group <-ordered (subset1$Size_group)
rownames(subset1) <- subset1$Name; subset1<- subset1[,-1]
subset1 <- subset1[which(is.na(subset1$Aspect_ratio)==F),]

# first calculate gower distance on traits
gower_matrix <- daisy (subset1, metric=c("gower")) 

# Building the functional space based on a PCOA 
pco<-dudi.pco(quasieuclid(gower_matrix), scannf=F, nf=10) # quasieuclid() transformation to make the gower matrix as euclidean. nf= number of axis 
#barplot(pco$eig) # barplot of eigenvalues for each axis 
(Inertia2<-(pco$eig[1]+pco$eig[2]) /(sum(pco$eig))) # percentage of inertia explained by the two first axes
## only the frst axis
Inertia.first <- (pco$eig[1]) /(sum(pco$eig))
## only the frst axis
Inertia.scnd <- (pco$eig[2]) /(sum(pco$eig))

## complete space
all <- cbind (pco$li[,1:2],ext = F)

a <- all [chull(all[,1:2], y = NULL),]
#a [order(a$A1,decreasing=T),]
#a [order(a$A2,decreasing=T),]

## extracted data of impaired species of each coral spp
fuck_sp <- unique(total [which(total$low >0),"peixe"])

# reduced space
setB<-cbind(all, ext1=ifelse(rownames(all) %in% fuck_sp,T,F))
pk <-setB[which(setB$ext1==F),]
f <- pk [chull(pk, y = NULL),]

## quantifying reduction in functional space

# https://chitchatr.wordpress.com/2015/01/23/calculating-the-area-of-a-convex-hull/
chull.poly.complete <- Polygon(a[,1:2], hole=F)
chull.area.complete <- chull.poly.complete@area

## if it is not possible to calculate funct space, then report NA
if (length(fuck_sp) < 2) {
  red.space <- data.frame (exc=NA, 
                           comp=chull.area.complete, 
                           red=chull.area.complete)
}   else { 
  chull.poly.exc <- Polygon(f[,1:2], hole=F)
  chull.area.exc <- chull.poly.exc@area
  ## calculate the diff aftering excluding coral-reliant species
  ## how much the complete space is larger than the excluded one
  red.space <- data.frame (exc=chull.area.exc, 
                           comp=chull.area.complete, 
                           red=chull.area.exc/chull.area.complete)
}

## plot A
plotA <- ggplot(a, aes(A1, A2)) + 
  geom_point() + theme_bw()+
  geom_polygon(data=a, aes (A1,A2),alpha=0.5,fill="gray") + 
  geom_polygon(data=f, aes (A1,A2,group=ext1, fill=ext1),alpha=0.5,
               fill="black",size=3) +
  xlim(min (a$A1)-0.2,max (a$A1)+0.2)


## correlations
subset1$Size_group <- as.numeric (subset1$Size_group)
correlations <- cor (pco$li[is.na(subset1$Aspect_ratio) !=T,1:2],
                     subset1[is.na(subset1$Aspect_ratio) !=T,])

## plotting 

plotA + geom_segment(aes(x = 0, y = 0, 
                         xend = correlations[1,1]*0.2, 
                         yend = correlations[2,1]*0.2),size = 1,
                     arrow = arrow(length = unit(.35, "cm")))  + 
  ## annotate
  annotate(geom="text",x=correlations[1,1]*0.25,
           y=correlations[2,1]*0.25,label="Body size") +
  
  geom_segment(aes(x = 0, y = 0, 
                   xend = correlations[1,2]*0.2, 
                   yend = correlations[2,2]*0.2),size = 1,
               arrow = arrow(length = unit(.35, "cm"))) + 
  annotate(geom="text",x=correlations[1,2]*0.25,
           y=correlations[2,2]*0.25,label="Size group") +
  
  geom_segment(aes(x = 0, y = 0, 
                   xend = correlations[1,3]*0.2, 
                   yend = correlations[2,3]*0.2),size = 1,
               arrow = arrow(length = unit(.35, "cm"))) + 
  annotate(geom="text",x=correlations[1,3]*0.25,
           y=correlations[2,3]*0.25,label="Aspect ratio") +
  
  geom_segment(aes(x = 0, y = 0, 
                   xend = correlations[1,4]*0.2, 
                   yend = correlations[2,4]*0.2),size = 1,
               arrow = arrow(length = unit(.35, "cm"))) + 
  annotate(geom="text",x=correlations[1,4]*0.25,
           y=correlations[2,4]*0.25,label="Trophic level")

ggsave(here ("output","Vect",filename = "Fspace_longo_global.pdf"), 
       width = 4,height=4) 

## things to report
res_global <- list (space = red.space,
             first.axis = Inertia.first,
             scnd.axis = Inertia.scnd,
             fuck_sp = fuck_sp)

#- -------------------------
# random loss
# ---------------------------

# dfish traits
subset1 <- traits_peixes [which(traits_peixes$Name %in% 
                                  unique(total$peixe)),
                          c("Name","Body_size", 
                            "Size_group",
                            "Aspect_ratio",
                            "Trophic_level")]

subset1$Size_group <- sapply(subset1$Size_group , function(x) {
  if (x=="sol") {1} 
  else if (x=="pair") {2} 
  else if (x=="smallg") {3} 
  else if (x=="medg") {4} 
  else if (x=="largeg") {5}}
)

subset1$Size_group <-ordered (subset1$Size_group)
rownames(subset1) <- subset1$Name; subset1<- subset1[,-1]
subset1 <- subset1[which(is.na(subset1$Aspect_ratio)==F),]

# first calculate gower distance on traits
gower_matrix <- daisy (subset1, metric=c("gower")) 

# Building the functional space based on a PCOA 
pco<-dudi.pco(quasieuclid(gower_matrix), scannf=F, nf=10) # quasieuclid() transformation to make the gower matrix as euclidean. nf= number of axis 
#barplot(pco$eig) # barplot of eigenvalues for each axis 
(Inertia2<-(pco$eig[1]+pco$eig[2]) /(sum(pco$eig))) # percentage of inertia explained by the two first axes
## only the frst axis
Inertia.first <- (pco$eig[1]) /(sum(pco$eig))
## only the frst axis
Inertia.scnd <- (pco$eig[2]) /(sum(pco$eig))

## complete space
all <- cbind (pco$li[,1:2],ext = F)

a <- all [chull(all[,1:2], y = NULL),]
#a [order(a$A1,decreasing=T),]
#a [order(a$A2,decreasing=T),]

## extracted data of impaired species of each coral spp
fuck_sp <- replicate (100,sample (total$peixe,
  length(unique(total [which(total$low >0),"peixe"]))))

extinction_random <- lapply(seq (1,ncol(fuck_sp)), function (i) {
  
  # reduced space
  setB<-cbind(all, ext1=ifelse(rownames(all) %in% fuck_sp[,i],T,F))
  pk <-setB[which(setB$ext1==F),]
  f <- pk [chull(pk, y = NULL),]

  ## quantifying reduction in functional space

  # https://chitchatr.wordpress.com/2015/01/23/calculating-the-area-of-a-convex-hull/
  chull.poly.complete <- Polygon(a[,1:2], hole=F)
  chull.area.complete <- chull.poly.complete@area

  ## if it is not possible to calculate funct space, then report NA
  if (length(fuck_sp) < 2) {
    red.space <- data.frame (exc=NA, 
                           comp=chull.area.complete, 
                           red=chull.area.complete)
  }   else { 
    chull.poly.exc <- Polygon(f[,1:2], hole=F)
    chull.area.exc <- chull.poly.exc@area
    ## calculate the diff aftering excluding coral-reliant species
    ## how much the complete space is larger than the excluded one
    red.space <- data.frame (exc=chull.area.exc, 
                           comp=chull.area.complete, 
                           red=chull.area.exc/chull.area.complete)
    ; # return
    res <- list (red.space = red.space,
                 coord_space = f)
  }
})
  
#save (extinction_random,file=here ("output", "extinction_random.RData"))
random_ext_pol <- sapply (extinction_random, "[","coord_space")


## plot A
plotA <- ggplot(a, aes(A1, A2)) + 
    geom_point() + theme_bw()+
    geom_polygon(data=a, aes (A1,A2),alpha=0.7,fill="gray10") + 
    geom_polygon(data=random_ext_pol[[1]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[1]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[2]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[3]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[4]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[5]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[6]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[7]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[8]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[9]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[10]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[11]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[12]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[13]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[14]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[15]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[16]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[17]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[18]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[19]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[20]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[21]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[22]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[23]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[24]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[25]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[26]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[27]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[28]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[29]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[30]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[31]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[32]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[33]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[34]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[35]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[36]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[37]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[38]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[39]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[40]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[41]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[42]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[43]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[44]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[45]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[46]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[46]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[47]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[48]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[49]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[50]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[51]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[52]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[53]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[54]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[55]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[56]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[57]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[58]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[59]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[60]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[61]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[62]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[63]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[64]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[65]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[66]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[66]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[67]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[68]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[69]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[70]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[71]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[72]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[73]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[74]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[75]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[76]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[76]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[77]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[78]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[79]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[80]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[81]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[82]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[83]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[84]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[85]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[86]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[87]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[88]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[89]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[90]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[91]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[92]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[93]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[94]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[95]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[96]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[97]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[98]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[99]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
  geom_polygon(data=random_ext_pol[[100]], aes (A1,A2,group=ext1, fill=ext1),alpha=0.02,
               fill="white",size=3) +
    xlim(min (a$A1)-0.2,max (a$A1)+0.2)

plotA

## correlations
subset1$Size_group <- as.numeric (subset1$Size_group)
correlations <- cor (pco$li[is.na(subset1$Aspect_ratio) !=T,1:2],
                     subset1[is.na(subset1$Aspect_ratio) !=T,])

## plotting 

plotA + geom_segment(aes(x = 0, y = 0, 
                         xend = correlations[1,1]*0.2, 
                         yend = correlations[2,1]*0.2),size = 1,
                     arrow = arrow(length = unit(.35, "cm")))  + 
  ## annotate
  annotate(geom="text",x=correlations[1,1]*0.25,
           y=correlations[2,1]*0.25,label="Body size") +
  
  geom_segment(aes(x = 0, y = 0, 
                   xend = correlations[1,2]*0.2, 
                   yend = correlations[2,2]*0.2),size = 1,
               arrow = arrow(length = unit(.35, "cm"))) + 
  annotate(geom="text",x=correlations[1,2]*0.25,
           y=correlations[2,2]*0.25,label="Size group") +
  
  geom_segment(aes(x = 0, y = 0, 
                   xend = correlations[1,3]*0.2, 
                   yend = correlations[2,3]*0.2),size = 1,
               arrow = arrow(length = unit(.35, "cm"))) + 
  annotate(geom="text",x=correlations[1,3]*0.25,
           y=correlations[2,3]*0.25,label="Aspect ratio") +
  
  geom_segment(aes(x = 0, y = 0, 
                   xend = correlations[1,4]*0.2, 
                   yend = correlations[2,4]*0.2),size = 1,
               arrow = arrow(length = unit(.35, "cm"))) + 
  annotate(geom="text",x=correlations[1,4]*0.25,
           y=correlations[2,4]*0.25,label="Trophic level")

ggsave(here ("output","Vect",filename = "Fspace_longo_random.pdf"), 
       width = 4,height=4) 

## things to report
res_random <- list (space = sapply (extinction_random,"[","red.space"),
             first.axis = Inertia.first,
             scnd.axis = Inertia.scnd,
             fuck_sp = fuck_sp)

apply(do.call(rbind,sapply (extinction_random,"[","red.space")),2,mean)
apply(do.call(rbind,sapply (extinction_random,"[","red.space")),2,sd)


# ------------------------------- #
# mapping reductions

## carregar os shapes corais
## reproject help: https://geocompr.robinlovelace.net/reproj-geo-data.html
coral_dir <- list.dirs(here ("data","coralsIUCN"),full.names =F)[-1]

extent_to_crop <- extent (c(-55, -30, -34, 4.5))

# corais
shapes_corais <- lapply (coral_dir,function (i) {
  
  readit<-readOGR(dsn=here("data","coralsIUCN",i), layer="data_0",
                  encoding="UTF-8", use_iconv = T)
  cropit <- crop (gBuffer (readit,byid=T,width=0),extent_to_crop,snap='near' )
  transformit <- spTransform(cropit, 
                             CRS=CRS("+proj=laea +lat_0=0 +lon_0=-32 +x_0=0 +y_0=0  +datum=WGS84 +units=m +no_defs"), 
                             check=T)
  
  ;
  transformit
} )

## shape fish

fish_dir <- list.dirs(here ("data","fishIUCN"),full.names =F)[-1]

shapes_fish <- lapply (fish_dir,function (i) {
  
  readit <-readOGR(dsn=here("data","fishIUCN",i), layer="data_0",
                   encoding="UTF-8", use_iconv = T)
  cropit <- crop (gBuffer(readit,byid = T, width = 0),extent_to_crop,snap='near' )
  transformit <- spTransform(cropit, 
                             CRS=CRS("+proj=laea +lat_0=0 +lon_0=-32 +x_0=0 +y_0=0  +datum=WGS84 +units=m +no_defs"), 
                             check=T)
  
  ;
  transformit
  
}
)

names_corals <- sapply (shapes_corais,"[[","BINOMIAL")

# estimates
total <- do.call (rbind,extracted_data)
total$coral <- gsub ("\\."," ",total$coral)
coral_spp <- unique(total$coral)
coral_spp<- coral_spp[match(names_corals,coral_spp)]

# reliant (lower CI > 0)
subset_reliant <- lapply (coral_spp, function (i) {
  subset_per_coral <-  total [which(total$coral %in% i),]
  subset_reliant <- subset_per_coral [which(subset_per_coral$low>0),"peixe"]
  ;
  subset_reliant
  })

# testing plot
plot(shapes_corais[[which(names_corals == "Porites astreoides")]])
plot(shapes_corais[[which(names_corals == "Millepora alcicornis")]],add=T,border="red")
plot(shapes_corais[[which(names_corals == "Mussismilia hispida")]],border = "green",add=T)

names_fish <- sapply (shapes_fish,"[[","BINOMIAL")

subset_reliant <- lapply (subset_reliant, function (i) {
  subset_1 <- i [match (names_fish,i)]
  subset_reliant <- subset_1[is.na(subset_1)!= T]
  }
)

require(raster)

# discounting maps 

test_reductions <- lapply (seq (1,3), function (i){
  
  c1 <- shapes_corais[[i]]
  p1 <- shapes_fish[which(names_fish  %in% subset_reliant[[i]])]
  
  #plot(p1)
  #plot(c1,add=T,col="red")
  ## cropping
  crop_maps <- lapply (seq (1,length(p1)), function (i) crop (c1,p1[[i]]))
  
  # plot what happened
  #plot(c1)
  
  # where can occur loss
  #lapply (crop_maps, function (i)
  
  #  plot(i,add=T,col=rgb(0,0.8,0,alpha=0.3))
  
  #)
  
  # calculating total area (remember to change projection)
  area_total <- lapply (p1,function (i) (area (i)/ 1000000))
  area_reduzida <- lapply (crop_maps,function (i) (area (i)/ 1000000))
  # ratio
  ratio <- lapply (seq (1,length(area_reduzida)), function (i)
    
      (area_total[[i]]-area_reduzida[[i]])/(area_total[[i]])
      
      )
  
  # transforming maps back to latlong proj
  # coral
  c1latlong <- spTransform(c1, 
                       CRS=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"), 
                       check=T) 
  # fish
  p1latlong <- lapply (p1, function (i)
    
              spTransform(i, 
                           CRS=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"), 
                           check=T) )
  # crop
  cropMapslatlong <- lapply (crop_maps, function (i)
    
    spTransform(i, 
                CRS=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"), 
                check=T) )
  
  
  # list of res
  res <- list (c1 = c1latlong,
               p1 = p1latlong,
               cropmap = cropMapslatlong,
               area_total = area_total,
               area_reduzida = area_reduzida,
               ratio = ratio) 
  
  ;
  res
})


# mapa mundi
world <- ne_countries(scale = "medium", returnclass = "sf")

# cortar o mapa para ver a america do Sul e parte da central
wm <- ggplot() + 
  geom_sf (data=world, size = 0.1, 
           fill= "gray90",colour="gray90") +
  coord_sf (xlim = c(-55, -29),  ylim = c(-34, 5), expand = FALSE) +
  theme_bw() + #xlab ("Longitude")  + ylab ("Latitude") +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "lightcyan",#darkslategray1
                                        colour = "lightcyan"),
        axis.text.x = element_text(size=6),
        axis.ticks.x=element_line(size=1),
        axis.text.y = element_text(size=6),
        axis.ticks.y=element_line(size=1),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        title = element_text(size=10)) 

# -------------------------- #
# porites
map_porites <- wm +  
  
  # fish polygon
  geom_polygon(data=test_reductions[[1]]$p1[[1]],aes (x = long, y = lat, group = group),
               col = "black",fill="white",alpha=0.1,size=0.5,
               linetype = "dotted") +
  geom_polygon(data=test_reductions[[1]]$p1[[2]],aes (x = long, y = lat, group = group),
               col = "black",fill="white",alpha=0.1,size=0.5,
               linetype = "dashed") +
  geom_polygon(data=test_reductions[[1]]$p1[[3]],aes (x = long, y = lat, group = group),
               col = "black",fill="white",alpha=0.1,size=0.5,
               linetype = "solid") +
  # matching fish x coral
  geom_polygon(data=test_reductions[[1]]$cropmap[[1]],aes (x = long, y = lat, group = group),
               col = "#FF8000",fill="#FF8000",alpha=0.2)+
  geom_polygon(data=test_reductions[[1]]$cropmap[[2]],aes (x = long, y = lat, group = group),
             col = "#FF8000",fill="#FF8000",alpha=0.2)+
  geom_polygon(data=test_reductions[[1]]$cropmap[[3]],aes (x = long, y = lat, group = group),
             col = "#FF8000",fill="#FF8000",alpha=0.2) +
  # coral
  geom_polygon(data=test_reductions[[1]]$c1,aes (x = long, y = lat, group = group),
               col = "#FF8000",fill="white",alpha=0.1,size=1) +
  xlab ("Longitude")+
  ylab ("Latitude")+
  ggtitle ("Porites astreoides")

map_porites

ggsave(here ("output","Vect",filename = "map_porites.pdf"), 
       width = 5,height=6) 

# barplot
pdf (file = here("output","Vect","barplot_porites.pdf"),width=5,height=5)
barplot_porites <- barplot(unlist(test_reductions[[1]]$ratio)*100,
        ylim=c(0,100),
        names.arg = subset_reliant[[1]],
        las=1,
        cex.names=0.5,
        ylab = "% distribution remaining with coral loss")
dev.off()

# ------------------------ #
# MILLEPORA
map_millepora <- wm +   
  # fish polygon
  geom_polygon(data=test_reductions[[2]]$p1[[1]],aes (x = long, y = lat, group = group),
                                   col = "black",fill="white",alpha=0.1,size=0.5,
                                   linetype = "solid") +
  geom_polygon(data=test_reductions[[2]]$p1[[2]],aes (x = long, y = lat, group = group),
               col = "black",fill="white",alpha=0.1,size=0.5,
               linetype = "dashed") +
  geom_polygon(data=test_reductions[[2]]$p1[[3]],aes (x = long, y = lat, group = group),
               col = "black",fill="white",alpha=0.1,size=1,
               linetype = "dotted") +
  geom_polygon(data=test_reductions[[2]]$p1[[4]],aes (x = long, y = lat, group = group),
               col = "black",fill="white",alpha=0.1,size=0.5,
               linetype = "longdash") +
  geom_polygon(data=test_reductions[[2]]$p1[[5]],aes (x = long, y = lat, group = group),
               col = "gray",fill="white",alpha=0.1,size=0.5,
               linetype = "dotdash") +
  geom_polygon(data=test_reductions[[2]]$p1[[6]],aes (x = long, y = lat, group = group),
               col = "gray",fill="white",alpha=0.1,size=0.5,
               linetype = "twodash") +
  
  # matching fish x coral
  geom_polygon(data=test_reductions[[2]]$cropmap[[1]],aes (x = long, y = lat, group = group),
               col = "#ca0020",fill="#ca0020",alpha=0.2)+
  geom_polygon(data=test_reductions[[2]]$cropmap[[2]],aes (x = long, y = lat, group = group),
               col = "#ca0020",fill="#ca0020",alpha=0.2)+
  geom_polygon(data=test_reductions[[2]]$cropmap[[3]],aes (x = long, y = lat, group = group),
               col = "#ca0020",fill="#ca0020",alpha=0.2) +
  geom_polygon(data=test_reductions[[2]]$cropmap[[4]],aes (x = long, y = lat, group = group),
               col = "#ca0020",fill="#ca0020",alpha=0.2)+
  geom_polygon(data=test_reductions[[2]]$cropmap[[5]],aes (x = long, y = lat, group = group),
               col = "#ca0020",fill="#ca0020",alpha=0.2)+
  geom_polygon(data=test_reductions[[2]]$cropmap[[6]],aes (x = long, y = lat, group = group),
               col = "#ca0020",fill="#ca0020",alpha=0.2) +
  # coral
  geom_polygon(data=test_reductions[[2]]$c1,aes (x = long, y = lat, group = group),
               col = "#ca0020",fill="white",alpha=0.1,size=1) +
  xlab ("Longitude")+
  ylab ("Latitude")+
  ggtitle ("Millepora alcicornis")

map_millepora

ggsave(here ("output","Vect",filename = "map_millepora.pdf"), 
       width = 5,height=6) 


# barplot
pdf (file = here("output","Vect","barplot_millepora.pdf"),width=5,height=5)
barplot_millepora <- barplot(unlist(test_reductions[[2]]$ratio)*100,
                           ylim=c(0,100),
                           names.arg = subset_reliant[[2]],
                           las=1,
                           cex.names=0.3,
                           ylab = "% distribution remaining with coral loss")
dev.off()

# --------------------------- #
## Mussismilia
map_mussismilia <- wm +   
  # fish polygon
  geom_polygon(data=test_reductions[[3]]$p1[[1]],aes (x = long, y = lat, group = group),
               col = "black",fill="white",alpha=0.1,size=0.5,
               linetype = "solid") +
  geom_polygon(data=test_reductions[[3]]$p1[[2]],aes (x = long, y = lat, group = group),
               col = "black",fill="white",alpha=0.1,size=0.5,
               linetype = "dashed") +
  geom_polygon(data=test_reductions[[3]]$p1[[3]],aes (x = long, y = lat, group = group),
               col = "black",fill="white",alpha=0.1,size=1,
               linetype = "dotted") +
  geom_polygon(data=test_reductions[[3]]$p1[[4]],aes (x = long, y = lat, group = group),
               col = "black",fill="white",alpha=0.1,size=0.5,
               linetype = "longdash") +
  geom_polygon(data=test_reductions[[3]]$p1[[5]],aes (x = long, y = lat, group = group),
               col = "gray",fill="white",alpha=0.1,size=0.5,
               linetype = "dotdash")+
  
  # matching fish x coral
  geom_polygon(data=test_reductions[[3]]$cropmap[[1]],aes (x = long, y = lat, group = group),
               col = "#0571b0",fill="#0571b0",alpha=0.2)+
  geom_polygon(data=test_reductions[[3]]$cropmap[[2]],aes (x = long, y = lat, group = group),
               col = "#0571b0",fill="#0571b0",alpha=0.2)+
  geom_polygon(data=test_reductions[[3]]$cropmap[[3]],aes (x = long, y = lat, group = group),
               col = "#0571b0",fill="#0571b0",alpha=0.2) +
  geom_polygon(data=test_reductions[[3]]$cropmap[[4]],aes (x = long, y = lat, group = group),
               col = "#0571b0",fill="#0571b0",alpha=0.2)+
  geom_polygon(data=test_reductions[[3]]$cropmap[[5]],aes (x = long, y = lat, group = group),
               col = "#0571b0",fill="#0571b0",alpha=0.2)+
  # coral
  geom_polygon(data=test_reductions[[3]]$c1,aes (x = long, y = lat, group = group),
               col = "#0571b0",fill="white",alpha=0.1,size=1) +
  xlab ("Longitude")+
  ylab ("Latitude")+
  ggtitle ("Mussismilia hispida")

map_mussismilia

ggsave(here ("output","Vect",filename = "map_mussismilia.pdf"), 
       width = 5,height=6) 

# barplot
pdf (file = here("output","Vect","barplot_mussismilia.pdf"),width=5,height=5)
barplot_mussismilia <- barplot(unlist(test_reductions[[3]]$ratio)*100,
                           ylim=c(0,100),
                           names.arg = subset_reliant[[3]],
                           las=1,
                           cex.names=0.3,
                           ylab = "% distribution remaining with coral loss")
dev.off()



# --------------------------------------------------------------------- #
# not run

## IUCN

f.space.IUCN <- lapply (unique (total$coral), function(k) {
  
  # dfish traits
  subset1 <- traits_peixes [which(traits_peixes$Name %in% 
                                    unique(total$peixe)),
                            c("Name","Body_size", 
                              "Size_group",
                              "Aspect_ratio",
                              "Trophic_level")]
  
  subset1$Size_group <- sapply(subset1$Size_group , function(x) {
    if (x=="sol") {1} 
    else if (x=="pair") {2} 
    else if (x=="smallg") {3} 
    else if (x=="medg") {4} 
    else if (x=="largeg") {5}}
  )
  
  subset1$Size_group <-ordered (subset1$Size_group)
  rownames(subset1) <- subset1$Name; subset1<- subset1[,-1]
  subset1 <- subset1[which(is.na(subset1$Aspect_ratio)==F),]
  
  ## when not possible to have functional space
  if (nrow(subset1) <=2) {
    red.space <- data.frame (exc=NA, 
                             comp=NA, 
                             red=NA)
    
    ## things to report
    res <- list (space = red.space)} else {
      
      # first calculate gower distance on traits
      gower_matrix <- daisy (subset1, metric=c("gower")) 
      
      # Building the functional space based on a PCOA 
      pco<-dudi.pco(quasieuclid(gower_matrix), scannf=F, nf=10) # quasieuclid() transformation to make the gower matrix as euclidean. nf= number of axis 
      #barplot(pco$eig) # barplot of eigenvalues for each axis 
      (Inertia2<-(pco$eig[1]+pco$eig[2]) /(sum(pco$eig))) # percentage of inertia explained by the two first axes
      ## only the frst axis
      Inertia.first <- (pco$eig[1]) /(sum(pco$eig))
      ## only the frst axis
      Inertia.scnd <- (pco$eig[2]) /(sum(pco$eig))
      
      ## complete space
      all <- cbind (pco$li[,1:2],ext = F)
      a <- all [chull(all[,1:2], y = NULL),]
      
      ## extracted data of impaired species
      subset_coral <- total[which(total$coral == k),]
      threat_sp <- traits_peixes [which(traits_peixes$IUCN_status %in% c("cr","nt","vu","en")),"Name"]
      fuck_sp <- subset_coral[which(subset_coral$peixe %in% threat_sp == T),"peixe"]
      
      # reduced space
      setB<-cbind(all, ext1=ifelse(rownames(all) %in% fuck_sp,T,F))
      pk <-setB[which(setB$ext1==F),]
      f <- pk [chull(pk, y = NULL),]
      
      ## quantifying reduction in functional space
      
      # https://chitchatr.wordpress.com/2015/01/23/calculating-the-area-of-a-convex-hull/
      chull.poly.complete <- Polygon(a[,1:2], hole=F)
      chull.area.complete <- chull.poly.complete@area
      
      ## if it is not possible to calculate funct space, then report NA
      
      chull.poly.exc <- Polygon(f[,1:2], hole=F)
      chull.area.exc <- chull.poly.exc@area
      
      ## calculate the diff aftering excluding coral-reliant species
      ## how much the complete space is larger than the excluded one
      red.space <- data.frame (exc=chull.area.exc, 
                               comp=chull.area.complete, 
                               red=chull.area.exc/chull.area.complete)
      
      
      
      ## plot A
      plotA <- ggplot(a, aes(A1, A2)) + 
        geom_point() + theme_bw()+
        geom_polygon(data=a, aes (A1,A2),alpha=0.5,fill="gray") + 
        geom_polygon(data=f, aes (A1,A2,group=ext1, fill=ext1),alpha=0.5,
                     fill="black",size=3) +
        xlim(min (a$A1)-0.2,max (a$A1)+0.2)
      
      
      ## correlations
      subset1$Size_group <- as.numeric (subset1$Size_group)
      correlations <- cor (pco$li[is.na(subset1$Aspect_ratio) !=T,1:2],
                           subset1[is.na(subset1$Aspect_ratio) !=T,])
      
      ## plotting 
      
      plotA + geom_segment(aes(x = 0, y = 0, 
                               xend = correlations[1,1]*0.2, 
                               yend = correlations[2,1]*0.2),size = 1,
                           arrow = arrow(length = unit(.35, "cm")))  + 
        ## annotate
        annotate(geom="text",x=correlations[1,1]*0.25,
                 y=correlations[2,1]*0.25,label="Body size") +
        
        geom_segment(aes(x = 0, y = 0, 
                         xend = correlations[1,2]*0.2, 
                         yend = correlations[2,2]*0.2),size = 1,
                     arrow = arrow(length = unit(.35, "cm"))) + 
        annotate(geom="text",x=correlations[1,2]*0.25,
                 y=correlations[2,2]*0.25,label="Size group") +
        
        geom_segment(aes(x = 0, y = 0, 
                         xend = correlations[1,3]*0.2, 
                         yend = correlations[2,3]*0.2),size = 1,
                     arrow = arrow(length = unit(.35, "cm"))) + 
        annotate(geom="text",x=correlations[1,3]*0.25,
                 y=correlations[2,3]*0.25,label="Aspect ratio") +
        
        geom_segment(aes(x = 0, y = 0, 
                         xend = correlations[1,4]*0.2, 
                         yend = correlations[2,4]*0.2),size = 1,
                     arrow = arrow(length = unit(.35, "cm"))) + 
        annotate(geom="text",x=correlations[1,4]*0.25,
                 y=correlations[2,4]*0.25,label="Trophic level")
      
      ggsave(here ("output","Vect",filename = paste ("FspaceIUCN_longo_allcorals",k,".pdf")), 
             width = 4,height=4) 
      
      ## things to report
      res <- list (space = red.space,
                   first.axis = Inertia.first,
                   scnd.axis = Inertia.scnd,
                   fuck_sp=fuck_sp )
      
    }
  
  
  ;
  
  res
  
})

f.space.IUCN
















## 

## IUCN

f.space.IUCN <- lapply (seq (1,length(extracted_data)), function(k) {
  
  # ext coeff
  coef_fish <- extracted_data[[k]]
  
  # rem species
  coef_fish <- coef_fish[which(coef_fish$peixe %in% sapply (sp_analyzed,"[","teste")[[k]]$peixe == T),]
  
  # dfish traits
  subset1 <- traits_peixes [which(traits_peixes$Name %in% 
                                    unique(coef_fish$peixe)),
                            c("Name","Body_size", 
                              "Size_group",
                              "Aspect_ratio",
                              "Trophic_level")]
  
  subset1$Size_group <- sapply(subset1$Size_group , function(x) {
    if (x=="sol") {1} 
    else if (x=="pair") {2} 
    else if (x=="smallg") {3} 
    else if (x=="medg") {4} 
    else if (x=="largeg") {5}}
  )
  
  subset1$Size_group <-ordered (subset1$Size_group)
  rownames(subset1) <- subset1$Name; subset1<- subset1[,-1]
  subset1 <- subset1[which(is.na(subset1$Aspect_ratio)==F),]
  
  ## when not possible to have functional space
  if (nrow(subset1) <=2) {
    red.space <- data.frame (exc=NA, 
                             comp=NA, 
                             red=NA)
    
    ## things to report
    res <- list (space = red.space)} else {
      
      # first calculate gower distance on traits
      gower_matrix <- daisy (subset1, metric=c("gower")) 
      
      # Building the functional space based on a PCOA 
      pco<-dudi.pco(quasieuclid(gower_matrix), scannf=F, nf=10) # quasieuclid() transformation to make the gower matrix as euclidean. nf= number of axis 
      #barplot(pco$eig) # barplot of eigenvalues for each axis 
      (Inertia2<-(pco$eig[1]+pco$eig[2]) /(sum(pco$eig))) # percentage of inertia explained by the two first axes
      ## only the frst axis
      Inertia.first <- (pco$eig[1]) /(sum(pco$eig))
      ## only the frst axis
      Inertia.scnd <- (pco$eig[2]) /(sum(pco$eig))
      
      ## complete space
      all <- cbind (pco$li[,1:2],ext = F)
      a <- all [chull(all[,1:2], y = NULL),]
      
      ## extracted data of impaired species
      threat_sp <- traits_peixes [which(traits_peixes$IUCN_status %in% c("cr","nt","vu","en")),"Name"]
      fuck_sp <- extracted_data[[k]][which(extracted_data[[k]]$peixe %in% threat_sp == T),"peixe"]
      
      # reduced space
      setB<-cbind(all, ext1=ifelse(rownames(all) %in% fuck_sp,T,F))
      pk <-setB[which(setB$ext1==F),]
      f <- pk [chull(pk, y = NULL),]
      
      ## quantifying reduction in functional space
      
      # https://chitchatr.wordpress.com/2015/01/23/calculating-the-area-of-a-convex-hull/
      chull.poly.complete <- Polygon(a[,1:2], hole=F)
      chull.area.complete <- chull.poly.complete@area
      
      ## if it is not possible to calculate funct space, then report NA
      
      chull.poly.exc <- Polygon(f[,1:2], hole=F)
      chull.area.exc <- chull.poly.exc@area
      
      ## calculate the diff aftering excluding coral-reliant species
      ## how much the complete space is larger than the excluded one
      red.space <- data.frame (exc=chull.area.exc, 
                               comp=chull.area.complete, 
                               red=chull.area.complete/chull.area.exc)
      
      
      
      ## plot A
      plotA <- ggplot(a, aes(A1, A2)) + 
        geom_point() + theme_bw()+
        geom_polygon(data=a, aes (A1,A2),alpha=0.5,fill="gray") + 
        geom_polygon(data=f, aes (A1,A2,group=ext1, fill=ext1),alpha=0.5,
                     fill="black",size=3) +
        xlim(min (a$A1)-0.2,max (a$A1)+0.2)
      
      
      ## correlations
      subset1$Size_group <- as.numeric (subset1$Size_group)
      correlations <- cor (pco$li[is.na(subset1$Aspect_ratio) !=T,1:2],
                           subset1[is.na(subset1$Aspect_ratio) !=T,])
      
      ## plotting 
      
      plotA + geom_segment(aes(x = 0, y = 0, 
                               xend = correlations[1,1]*0.2, 
                               yend = correlations[2,1]*0.2),size = 1,
                           arrow = arrow(length = unit(.35, "cm")))  + 
        ## annotate
        annotate(geom="text",x=correlations[1,1]*0.25,
                 y=correlations[2,1]*0.25,label="Body size") +
        
        geom_segment(aes(x = 0, y = 0, 
                         xend = correlations[1,2]*0.2, 
                         yend = correlations[2,2]*0.2),size = 1,
                     arrow = arrow(length = unit(.35, "cm"))) + 
        annotate(geom="text",x=correlations[1,2]*0.25,
                 y=correlations[2,2]*0.25,label="Size group") +
        
        geom_segment(aes(x = 0, y = 0, 
                         xend = correlations[1,3]*0.2, 
                         yend = correlations[2,3]*0.2),size = 1,
                     arrow = arrow(length = unit(.35, "cm"))) + 
        annotate(geom="text",x=correlations[1,3]*0.25,
                 y=correlations[2,3]*0.25,label="Aspect ratio") +
        
        geom_segment(aes(x = 0, y = 0, 
                         xend = correlations[1,4]*0.2, 
                         yend = correlations[2,4]*0.2),size = 1,
                     arrow = arrow(length = unit(.35, "cm"))) + 
        annotate(geom="text",x=correlations[1,4]*0.25,
                 y=correlations[2,4]*0.25,label="Trophic level")
      
      ggsave(here ("output","Vect",filename = paste ("FspaceIUCN_longo",coral_species[k],".pdf")), 
             width = 4,height=4) 
      
      ## things to report
      res <- list (space = red.space,
                   first.axis = Inertia.first,
                   scnd.axis = Inertia.scnd,
                   fuck_sp=fuck_sp )
      
    }
  
  
  ;
  
  res
  
})

f.space.IUCN


# interaction

## IUCN

f.space.IUCN.reliant <- lapply (unique (total$coral), function(k) {
  
  # dfish traits
  subset1 <- traits_peixes [which(traits_peixes$Name %in% 
                                    unique(total$peixe)),
                            c("Name","Body_size", 
                              "Size_group",
                              "Aspect_ratio",
                              "Trophic_level")]
  
  subset1$Size_group <- sapply(subset1$Size_group , function(x) {
    if (x=="sol") {1} 
    else if (x=="pair") {2} 
    else if (x=="smallg") {3} 
    else if (x=="medg") {4} 
    else if (x=="largeg") {5}}
  )
  
  subset1$Size_group <-ordered (subset1$Size_group)
  rownames(subset1) <- subset1$Name; subset1<- subset1[,-1]
  subset1 <- subset1[which(is.na(subset1$Aspect_ratio)==F),]
  
  ## when not possible to have functional space
  if (nrow(subset1) <=2) {
    red.space <- data.frame (exc=NA, 
                             comp=NA, 
                             red=NA)
    
    ## things to report
    res <- list (space = red.space)} else {
      
      # first calculate gower distance on traits
      gower_matrix <- daisy (subset1, metric=c("gower")) 
      
      # Building the functional space based on a PCOA 
      pco<-dudi.pco(quasieuclid(gower_matrix), scannf=F, nf=10) # quasieuclid() transformation to make the gower matrix as euclidean. nf= number of axis 
      #barplot(pco$eig) # barplot of eigenvalues for each axis 
      (Inertia2<-(pco$eig[1]+pco$eig[2]) /(sum(pco$eig))) # percentage of inertia explained by the two first axes
      ## only the frst axis
      Inertia.first <- (pco$eig[1]) /(sum(pco$eig))
      ## only the frst axis
      Inertia.scnd <- (pco$eig[2]) /(sum(pco$eig))
      
      ## complete space
      all <- cbind (pco$li[,1:2],ext = F)
      a <- all [chull(all[,1:2], y = NULL),]
      
      ## extracted data of impaired species
      subset_coral <- total[which(total$coral == k),]
      reliant_sp <- subset_coral [which(subset_coral$low >0),"peixe"]
      threat_sp <- traits_peixes [which(traits_peixes$IUCN_status %in% c("cr","nt","vu","en")),"Name"]
      exclude <- unique(c(reliant_sp,threat_sp))
      fuck_sp <- subset_coral[which(subset_coral$peixe %in% exclude == T),"peixe"]
      
      # reduced space
      setB<-cbind(all, ext1=ifelse(rownames(all) %in% fuck_sp,T,F))
      pk <-setB[which(setB$ext1==F),]
      f <- pk [chull(pk, y = NULL),]
      
      ## quantifying reduction in functional space
      
      # https://chitchatr.wordpress.com/2015/01/23/calculating-the-area-of-a-convex-hull/
      chull.poly.complete <- Polygon(a[,1:2], hole=F)
      chull.area.complete <- chull.poly.complete@area
      
      ## if it is not possible to calculate funct space, then report NA
      
      chull.poly.exc <- Polygon(f[,1:2], hole=F)
      chull.area.exc <- chull.poly.exc@area
      
      ## calculate the diff aftering excluding coral-reliant species
      ## how much the complete space is larger than the excluded one
      red.space <- data.frame (exc=chull.area.exc, 
                               comp=chull.area.complete, 
                               red=chull.area.exc/chull.area.complete)
      
      
      
      ## plot A
      plotA <- ggplot(a, aes(A1, A2)) + 
        geom_point() + theme_bw()+
        geom_polygon(data=a, aes (A1,A2),alpha=0.5,fill="gray") + 
        geom_polygon(data=f, aes (A1,A2,group=ext1, fill=ext1),alpha=0.5,
                     fill="black",size=3) +
        xlim(min (a$A1)-0.2,max (a$A1)+0.2)
      
      
      ## correlations
      subset1$Size_group <- as.numeric (subset1$Size_group)
      correlations <- cor (pco$li[is.na(subset1$Aspect_ratio) !=T,1:2],
                           subset1[is.na(subset1$Aspect_ratio) !=T,])
      
      ## plotting 
      
      plotA + geom_segment(aes(x = 0, y = 0, 
                               xend = correlations[1,1]*0.2, 
                               yend = correlations[2,1]*0.2),size = 1,
                           arrow = arrow(length = unit(.35, "cm")))  + 
        ## annotate
        annotate(geom="text",x=correlations[1,1]*0.25,
                 y=correlations[2,1]*0.25,label="Body size") +
        
        geom_segment(aes(x = 0, y = 0, 
                         xend = correlations[1,2]*0.2, 
                         yend = correlations[2,2]*0.2),size = 1,
                     arrow = arrow(length = unit(.35, "cm"))) + 
        annotate(geom="text",x=correlations[1,2]*0.25,
                 y=correlations[2,2]*0.25,label="Size group") +
        
        geom_segment(aes(x = 0, y = 0, 
                         xend = correlations[1,3]*0.2, 
                         yend = correlations[2,3]*0.2),size = 1,
                     arrow = arrow(length = unit(.35, "cm"))) + 
        annotate(geom="text",x=correlations[1,3]*0.25,
                 y=correlations[2,3]*0.25,label="Aspect ratio") +
        
        geom_segment(aes(x = 0, y = 0, 
                         xend = correlations[1,4]*0.2, 
                         yend = correlations[2,4]*0.2),size = 1,
                     arrow = arrow(length = unit(.35, "cm"))) + 
        annotate(geom="text",x=correlations[1,4]*0.25,
                 y=correlations[2,4]*0.25,label="Trophic level")
      
      ggsave(here ("output","Vect",filename = paste ("FspaceIUCNreliant_longo_allcorals",k,".pdf")), 
             width = 4,height=4) 
      
      ## things to report
      res <- list (space = red.space,
                   first.axis = Inertia.first,
                   scnd.axis = Inertia.scnd,
                   fuck_sp=fuck_sp )
      
    }
  
  
  ;
  
  res
  
})

f.space.IUCN.reliant


## further exploring redundancy

f.space.IUCN.reliant <- lapply (unique (total$coral), function(k) {
  
  # dfish traits
  subset1 <- traits_peixes [which(traits_peixes$Name %in% 
                                    unique(total$peixe)),
                            c("Name","Body_size", 
                              "Size_group",
                              "Aspect_ratio",
                              "Trophic_level")]
  
  subset1$Size_group <- sapply(subset1$Size_group , function(x) {
    if (x=="sol") {1} 
    else if (x=="pair") {2} 
    else if (x=="smallg") {3} 
    else if (x=="medg") {4} 
    else if (x=="largeg") {5}}
  )
  
  subset1$Size_group <-ordered (subset1$Size_group)
  rownames(subset1) <- subset1$Name; subset1<- subset1[,-1]
  subset1 <- subset1[which(is.na(subset1$Aspect_ratio)==F),]
  
  ## when not possible to have functional space
  if (nrow(subset1) <=2) {
    red.space <- data.frame (exc=NA, 
                             comp=NA, 
                             red=NA)
    
    ## things to report
    res <- list (space = red.space)} else {
      
      # first calculate gower distance on traits
      gower_matrix <- daisy (subset1, metric=c("gower")) 
      
      # Building the functional space based on a PCOA 
      pco<-dudi.pco(quasieuclid(gower_matrix), scannf=F, nf=10) # quasieuclid() transformation to make the gower matrix as euclidean. nf= number of axis 
      #barplot(pco$eig) # barplot of eigenvalues for each axis 
      (Inertia2<-(pco$eig[1]+pco$eig[2]) /(sum(pco$eig))) # percentage of inertia explained by the two first axes
      ## only the frst axis
      Inertia.first <- (pco$eig[1]) /(sum(pco$eig))
      ## only the frst axis
      Inertia.scnd <- (pco$eig[2]) /(sum(pco$eig))
      
      ## complete space
      all <- cbind (pco$li[,1:2],ext = F)
      a <- all [chull(all[,1:2], y = NULL),]
      
      ## extracted data of impaired species
      subset_coral <- total[which(total$coral == k),]
      reliant_sp <- subset_coral [which(subset_coral$low >0),"peixe"]
      threat_sp <- traits_peixes [which(traits_peixes$IUCN_status %in% c("cr","nt","vu","en")),"Name"]
      exclude <- unique(c(reliant_sp,threat_sp))
      fuck_sp <- subset_coral[which(subset_coral$peixe %in% exclude == T),"peixe"]
      
      # reduced space
      setB<-cbind(all, ext1=ifelse(rownames(all) %in% fuck_sp,T,F))
      pk <-setB[which(setB$ext1==F),]
      f <- pk [chull(pk, y = NULL),]
      
      ## quantifying reduction in functional space
      
      # https://chitchatr.wordpress.com/2015/01/23/calculating-the-area-of-a-convex-hull/
      chull.poly.complete <- Polygon(a[,1:2], hole=F)
      chull.area.complete <- chull.poly.complete@area
      
      ## if it is not possible to calculate funct space, then report NA
      
      chull.poly.exc <- Polygon(f[,1:2], hole=F)
      chull.area.exc <- chull.poly.exc@area
      
      ## calculate the diff aftering excluding coral-reliant species
      ## how much the complete space is larger than the excluded one
      red.space <- data.frame (exc=chull.area.exc, 
                               comp=chull.area.complete, 
                               red=chull.area.exc/chull.area.complete)
      
      
      
      ## plot A
      plotA <- ggplot(a, aes(A1, A2)) + 
        geom_point() + theme_bw()+
        geom_polygon(data=a, aes (A1,A2),alpha=0.5,fill="gray") + 
        geom_polygon(data=f, aes (A1,A2,group=ext1, fill=ext1),alpha=0.5,
                     fill="black",size=3) +
        xlim(min (a$A1)-0.2,max (a$A1)+0.2)
      
      
      ## correlations
      subset1$Size_group <- as.numeric (subset1$Size_group)
      correlations <- cor (pco$li[is.na(subset1$Aspect_ratio) !=T,1:2],
                           subset1[is.na(subset1$Aspect_ratio) !=T,])
      
      ## plotting 
      
      plotA + geom_segment(aes(x = 0, y = 0, 
                               xend = correlations[1,1]*0.2, 
                               yend = correlations[2,1]*0.2),size = 1,
                           arrow = arrow(length = unit(.35, "cm")))  + 
        ## annotate
        annotate(geom="text",x=correlations[1,1]*0.25,
                 y=correlations[2,1]*0.25,label="Body size") +
        
        geom_segment(aes(x = 0, y = 0, 
                         xend = correlations[1,2]*0.2, 
                         yend = correlations[2,2]*0.2),size = 1,
                     arrow = arrow(length = unit(.35, "cm"))) + 
        annotate(geom="text",x=correlations[1,2]*0.25,
                 y=correlations[2,2]*0.25,label="Size group") +
        
        geom_segment(aes(x = 0, y = 0, 
                         xend = correlations[1,3]*0.2, 
                         yend = correlations[2,3]*0.2),size = 1,
                     arrow = arrow(length = unit(.35, "cm"))) + 
        annotate(geom="text",x=correlations[1,3]*0.25,
                 y=correlations[2,3]*0.25,label="Aspect ratio") +
        
        geom_segment(aes(x = 0, y = 0, 
                         xend = correlations[1,4]*0.2, 
                         yend = correlations[2,4]*0.2),size = 1,
                     arrow = arrow(length = unit(.35, "cm"))) + 
        annotate(geom="text",x=correlations[1,4]*0.25,
                 y=correlations[2,4]*0.25,label="Trophic level")
      
      ggsave(here ("output","Vect",filename = paste ("FspaceIUCNreliant_longo_allcorals",k,".pdf")), 
             width = 4,height=4) 
      
      ## things to report
      res <- list (space = red.space,
                   first.axis = Inertia.first,
                   scnd.axis = Inertia.scnd,
                   fuck_sp=fuck_sp )
      
    }
  
  
  ;
  
  res
  
})




f.space <- lapply (seq (1,length(extracted_data)), function(k) {
  
  # ext coeff
  coef_fish <- extracted_data[[k]]
  
  # rem species
  coef_fish <- coef_fish[which(coef_fish$peixe %in% sapply (sp_analyzed,"[","teste")[[k]]$peixe == T),]
  
  # dfish traits
  subset1 <- traits_peixes [which(traits_peixes$Name %in% 
                                    unique(coef_fish$peixe)),
                            c("Name","Body_size", 
                              "Size_group",
                              "Aspect_ratio",
                              "Trophic_level")]
  
  # format group size
  subset1$Size_group <- sapply(subset1$Size_group , function(x) {
    if (x=="sol") {1} 
    else if (x=="pair") {2} 
    else if (x=="smallg") {3} 
    else if (x=="medg") {4} 
    else if (x=="largeg") {5}}
  )
  
  subset1$Size_group <-ordered (subset1$Size_group)
  rownames(subset1) <- subset1$Name; subset1<- subset1[,-1]
  subset1 <- subset1[which(is.na(subset1$Aspect_ratio)==F),]
  
  # first calculate gower distance on traits
  gower_matrix <- daisy (subset1, metric=c("gower")) 
  
  # Building the functional space based on a PCOA 
  pco<-dudi.pco(quasieuclid(gower_matrix), scannf=F, nf=10) # quasieuclid() transformation to make the gower matrix as euclidean. nf= number of axis 
  #barplot(pco$eig) # barplot of eigenvalues for each axis 
  (Inertia2<-(pco$eig[1]+pco$eig[2]) /(sum(pco$eig))) # percentage of inertia explained by the two first axes
  ## only the frst axis
  Inertia.first <- (pco$eig[1]) /(sum(pco$eig))
  ## only the frst axis
  Inertia.scnd <- (pco$eig[2]) /(sum(pco$eig))
  
  ## complete space
  all <- cbind (pco$li[,1:2],ext = F)
  a <- all [chull(all[,1:2], y = NULL),]
  
  ## extracted data of impaired species
  fuck_sp <- unique(coef_fish[which(coef_fish$low >0),"peixe"])
  
  # reduced space
  setB<-cbind(all, ext1=ifelse(rownames(all) %in% fuck_sp,T,F))
  pk <-setB[which(setB$ext1==F),]
  f <- pk [chull(pk, y = NULL),]
  
  ## quantifying reduction in functional space
  
  # https://chitchatr.wordpress.com/2015/01/23/calculating-the-area-of-a-convex-hull/
  chull.poly.complete <- Polygon(a[,1:2], hole=F)
  chull.area.complete <- chull.poly.complete@area
  
  ## if it is not possible to calculate funct space, then report NA
  if (length(fuck_sp) < 2) {
    red.space <- data.frame (exc=NA, 
                             comp=chull.area.complete, 
                             red=chull.area.complete)
  }   else { 
    chull.poly.exc <- Polygon(f[,1:2], hole=F)
    chull.area.exc <- chull.poly.exc@area
    ## calculate the diff aftering excluding coral-reliant species
    ## how much the complete space is larger than the excluded one
    red.space <- data.frame (exc=chull.area.exc, 
                             comp=chull.area.complete, 
                             red=chull.area.complete/chull.area.exc)
  }
  
  ## plot A
  plotA <- ggplot(a, aes(A1, A2)) + 
    geom_point() + theme_bw()+
    geom_polygon(data=a, aes (A1,A2),alpha=0.5,fill="gray") + 
    geom_polygon(data=f, aes (A1,A2,group=ext1, fill=ext1),alpha=0.5,
                 fill="black",size=3) +
    xlim(min (a$A1)-0.2,max (a$A1)+0.2)
  
  
  ## correlations
  subset1$Size_group <- as.numeric (subset1$Size_group)
  correlations <- cor (pco$li[is.na(subset1$Aspect_ratio) !=T,1:2],
                       subset1[is.na(subset1$Aspect_ratio) !=T,])
  
  ## plotting 
  
  plotA + geom_segment(aes(x = 0, y = 0, 
                           xend = correlations[1,1]*0.2, 
                           yend = correlations[2,1]*0.2),size = 1,
                       arrow = arrow(length = unit(.35, "cm")))  + 
    ## annotate
    annotate(geom="text",x=correlations[1,1]*0.25,
             y=correlations[2,1]*0.25,label="Body size") +
    
    geom_segment(aes(x = 0, y = 0, 
                     xend = correlations[1,2]*0.2, 
                     yend = correlations[2,2]*0.2),size = 1,
                 arrow = arrow(length = unit(.35, "cm"))) + 
    annotate(geom="text",x=correlations[1,2]*0.25,
             y=correlations[2,2]*0.25,label="Size group") +
    
    geom_segment(aes(x = 0, y = 0, 
                     xend = correlations[1,3]*0.2, 
                     yend = correlations[2,3]*0.2),size = 1,
                 arrow = arrow(length = unit(.35, "cm"))) + 
    annotate(geom="text",x=correlations[1,3]*0.25,
             y=correlations[2,3]*0.25,label="Aspect ratio") +
    
    geom_segment(aes(x = 0, y = 0, 
                     xend = correlations[1,4]*0.2, 
                     yend = correlations[2,4]*0.2),size = 1,
                 arrow = arrow(length = unit(.35, "cm"))) + 
    annotate(geom="text",x=correlations[1,4]*0.25,
             y=correlations[2,4]*0.25,label="Trophic level")
  
  ggsave(here ("output","Vect",filename = paste ("Fspace_longo",coral_species[k],".pdf")), 
         width = 4,height=4) 
  
  ## things to report
  res <- list (space = red.space,
               first.axis = Inertia.first,
               scnd.axis = Inertia.scnd,
               fuck_sp = fuck_sp)
  ;
  
  res
  
})

f.space



