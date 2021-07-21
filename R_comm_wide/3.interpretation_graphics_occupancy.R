## code with routine for result interpretation

# upper part, occupancy
# inner part, detection

# load packages and functions
source ("R/packages.R")
source ("R/functions.R")

## function to test space quality
source("R/quality_funct_space_fromdist2.R")

# ---------------------# 
# MAP (figure 1)
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

load (here("output_comm_wide","Data_fish_detection_LONGO_AUED.RData"))

## coral detection - with coordinates - LONGO
load (here("output_comm_wide","Data_coral_detection_LONGO_AUED.RData"))

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

pdf(here ("output_comm_wide", "MapPoints_longo.pdf"),
    width = 10,heigh=7)
  wm_pie_longo
dev.off()

# ---------------------------------------- #
# information of fish species
# ---------------------------------------- #

load (here("output_comm_wide","Data_fish_detection_LONGO_AUED.RData"))
sp_longo <- unique(unlist(fish_species))

# 
tab_spp <- data.frame (sp = sp_longo)#unique spp

# ------------------------------------------------- #  
# ------------------------------------------------- #
# site occupancy estimates
# ------------------------------------------------- #  
# ------------------------------------------------- #
  
# load input data
## fish data
load (here("output_comm_wide","Data_fish_detection_LONGO_AUED.RData"))

## coral detection - with coordinates
load (here("output_comm_wide","Data_coral_detection_LONGO_AUED.RData"))

## LOAD MODEL RESULTS
load(here("output_comm_wide","samples_OCCcoral_PdepthTime_longo.RData")) 

## LOAD MODEL RESULTS
load(here("output_comm_wide","samples_OCCcoral_PdepthTime_longo_RdmP.RData")) 

# ---------------------------------------- #
# FIGURE 2, FISH RELIANCE ON CORAL COVER
# ---------------------------------------- #

## load trait data
traits_peixes <- read.csv(here("data","traits","Atributos_especies_Atlantico_&_Pacifico_Oriental_2020_04_28.csv"),
                          h=T,sep=";")
#adjust names
traits_peixes$Name <- tolower (gsub (" ",".", traits_peixes$Name))
traits_peixes$Name <- gsub("\\."," ", paste0(toupper(substr(traits_peixes$Name, 1, 1)), 
                                             substr(traits_peixes$Name, 2, 100)))

# adjusting trait values
traits_peixes$Body_size <- as.numeric(gsub (",",".",traits_peixes$Body_size))
traits_peixes$Aspect_ratio <- as.numeric(gsub (",",".",traits_peixes$Aspect_ratio))
traits_peixes$Trophic_level <- as.numeric(gsub (",",".",traits_peixes$Trophic_level))

# NAs
traits_peixes [which(tolower (gsub(" ",".",traits_peixes$Name)) %in% tab_spp$sp),c("Family",
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

# imputing a few missing data
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

# subset of the table
traits_peixes_table <- (traits_peixes [which(tolower (gsub(" ",".",traits_peixes$Name)) %in% tab_spp$sp),c("Family",
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
)])

# save to show in supporting info
write.csv (traits_peixes_table,
           file=here("output_comm_wide","trait_table.csv"))

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
extracted_data <-   lapply (seq(1,length(coral_species)), function (coral)
    
    do.call(rbind,lapply (seq (1, length (fish_species [[coral]])), function (fish)
      
      data.frame (
        
        coral = coral_species[coral],
        
        peixe = gsub("\\."," ", paste0(toupper(substr(fish_species [[coral]][fish], 1, 1)), 
                                       substr(fish_species [[coral]][fish], 2, 100))),
        # intercept and credible interval
        intercept = mean (samples_OCCcoral_PdepthTime_longo_RdmP[[coral]]$sims.list$intercept.psi [,fish]),
        low.int = quantile (samples_OCCcoral_PdepthTime_longo_RdmP[[coral]]$sims.list$intercept.psi [,fish], 0.05),
        high.int = quantile (samples_OCCcoral_PdepthTime_longo_RdmP[[coral]]$sims.list$intercept.psi [,fish], 0.95),
        # regression coefficient and credible interval
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

# save this complete result to be shown in the supoporting information
# table with all estimates, Credible intervals, and bayesian P-value
write.table (do.call(rbind, extracted_data),
             file=here("output_comm_wide","coefs_fit_longo.csv"),sep=";")

#############################################################
## plots of regression coefficients (fig 3 of the main text)


sp_analyzed <- lapply (seq (1,length(extracted_data)), function (i) { # for each fish community
  
  teste <- extracted_data[[i]]
  
  ## organize spp names in axis-Y
  ## subset of each coral species
  subset1 <- traits_peixes_table[which(traits_peixes_table$Name %in% unique (teste$peixe)),c("Name", "Body_size", "Diet")]
  subset1 <- subset1 [order (subset1$Body_size,decreasing=F),]
  # ordering species according to body size
  teste <- teste[order(match(teste$peixe,subset1$Name)),]
  teste$peixe <- gsub("\\."," ", paste0(toupper(substr(teste$peixe, 1, 1)), substr(teste$peixe, 2, 100)))
  teste$peixe <- factor (teste$peixe,
                         levels = unique(teste$peixe))
  
  ## listing spp with with negative and significant coefficient
  rem_sp_neg <- teste [which(teste$estimate < 0 & teste$high < 0),"peixe"] # higher and lower CI lower than 0 
  ## listing spp with too imprecise estimates (absolute difference of 20 SD)
  rem_sp_imp <- teste [which (abs(teste$low -   teste$high) >= 20),"peixe"]
  ## removing these spp
  rem_sp <-c(as.character(rem_sp_imp),as.character(rem_sp_neg))
  teste <- teste [which(teste$peixe %in% rem_sp != T),]
  
  # plotting
  dodge <- c(0.2,0.2)
  pd <- position_dodge(dodge[i])
  # coefficients
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
    xlim(-20, 20) + 
    ggtitle (coral_species[[i]]) + 
    theme (legend.position = "none",
           axis.title = element_text(size=5),
           axis.text = element_text(size=5))
  
  # regression shapes
  # it can be useful if you don't know how the shape of the relationship between
  # occupancy and coral cover looks like
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
  
  # save as pdf
  pdf (file=here("output_comm_wide", paste (i,"longob_neg.pdf",sep="_")),width=4,heigh=3)
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

## barplot to inset into the coeff plot
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

# results of percentage of each effect (positive, no/neutral, negative on occupancy probability)
bar_plot_data

# save plots
pdf(here ("output_comm_wide","barplotEffect.pdf"),height=5,width=5)
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
# of corals would disappear in a future coral-less world
# ------------------------------------------------------ #

#---------------------------------------
# 		LOSS PER CORAL
# ------------------------------------

# a complete functional space per species of coral
total <- do.call (rbind,extracted_data) # rbind extracted data (with coefficients and CI)
# building functional space per coral spp
f.space <- lapply (unique (total$coral), function(k) { # for each coral species
  
  # get fish traits of its community
  subset1 <- traits_peixes_table [which(traits_peixes_table$Name %in% 
                                    unique(total$peixe)),
                            c("Name","Body_size", 
                              "Size_group",
                              "Aspect_ratio",
                              "Trophic_level")]

  # adjust traits (group size as an ordered variable)
  subset1$Size_group <- sapply(subset1$Size_group , function(x) {
    if (x=="sol") {1} 
    else if (x=="pair") {2} 
    else if (x=="smallg") {3} 
    else if (x=="medg") {4} 
    else if (x=="largeg") {5}}
  )
  subset1$Size_group <-ordered (subset1$Size_group)
  rownames(subset1) <- subset1$Name; subset1<- subset1[,-1]
  # rm missing data
  subset1 <- subset1[which(is.na(subset1$Aspect_ratio)==F),]
  
  # first calculate gower distance on traits
  gower_matrix <- daisy (subset1, metric=c("gower")) 
  
  # Building the functional space based on a PCOA 
  pco<-dudi.pco(quasieuclid(gower_matrix), scannf=F, nf=10) # quasieuclid() transformation to make the gower matrix as euclidean. nf= number of axis 
  #barplot(pco$eig) # barplot of eigenvalues for each axis 
  (Inertia2<-(pco$eig[1]+pco$eig[2]+pco$eig[3]) /(sum(pco$eig))) # percentage of inertia explained by the two first axes
  
  # estimate quality of f space
  quality<-quality_funct_space_fromdist( gower_matrix,  nbdim=10,   
                                         plot="quality_funct_space_I") # it will produce a plot (hosted in the root folder)
  
  ## only the frst axis
  Inertia.first <- (pco$eig[1]) /(sum(pco$eig))
  ## only the frst axis
  Inertia.scnd <- (pco$eig[2]) /(sum(pco$eig))
  ## only the frst axis
  Inertia.trd <- (pco$eig[3]) /(sum(pco$eig))
  
  ## complete space
  all <- cbind (pco$li[,1:2],ext = F)
  a <- all [chull(all[,1:2], y = NULL),] # its convex hull
  
  ## extracted data of impaired species of each coral spp
  subset_coral <- total[which(total$coral == k),] 
  impaired_sp <- subset_coral [which(subset_coral$low >0),"peixe"]
   
  # reduced space
  setB<-cbind(all, ext1=ifelse(rownames(all) %in% impaired_sp,T,F))
  pk <-setB[which(setB$ext1==F),]
  f <- pk [chull(pk, y = NULL),]
  
  ## quantifying reduction in functional space
  # https://chitchatr.wordpress.com/2015/01/23/calculating-the-area-of-a-convex-hull/
  chull.poly.complete <- Polygon(a[,1:2], hole=F)
  chull.area.complete <- chull.poly.complete@area
  
  ## if it is not possible to calculate reduction in funct space, then report NA
  if (length(impaired_sp) < 2) {
    red.space <- data.frame (exc=NA, 
                             comp=chull.area.complete, 
                             red=chull.area.complete)
  }   else { 
    chull.poly.exc <- Polygon(f[,1:2], hole=F)
    chull.area.exc <- chull.poly.exc@area
    ## calculate the diff after excluding coral-associated fish
    ## how much the complete space is larger than the excluded one
    red.space <- data.frame (exc=chull.area.exc, 
                             comp=chull.area.complete, 
                             red=chull.area.exc/chull.area.complete)
  }
  
  ###########
  ## plot A (complete space)
  plotA <- ggplot(a, aes(A1, A2)) + 
    geom_point() + theme_bw()+
    geom_polygon(data=a, aes (A1,A2),alpha=0.5,fill="gray") + # complete space
    geom_polygon(data=f, aes (A1,A2,group=ext1, fill=ext1),alpha=0.5, # reduced space
                 fill="black",size=3) +
    xlim(min (a$A1)-0.2,max (a$A1)+0.2)
  
  ## correlations to project trait values into the ordination
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
  #save
  ggsave(here ("output_comm_wide",filename = paste ("Fspace_longo_allcorals",k,".pdf")), 
         width = 4,height=4) 
  
  ## results to report
  res <- list (space = red.space,
               quality=quality,
               first.axis = Inertia.first,
               scnd.axis = Inertia.scnd,
               impaired_sp = impaired_sp)
  ;
  
  res
  
})

unique(unlist(sapply (f.space, "[[","impaired_sp"))) [order(unique(unlist(sapply (f.space, "[[","impaired_sp"))))]

# variation in space reduction 
# (exc = space afte exclusion)
# (comp = complete space)
# (red = reduction in functional space; in the text we report 1 - red)

sapply (f.space, "[[","space")

# --------------------------------------------
# 		TOTAL LOSS
# -------------------------------------------

# global space (binding all fishes and removing all associated)
total <- do.call (rbind,extracted_data)

# dfish traits
subset1 <- traits_peixes_table [which(traits_peixes_table$Name %in% 
                                  unique(total$peixe)),
                          c("Name","Body_size", 
                            "Size_group",
                            "Aspect_ratio",
                            "Trophic_level")]

# GROUP SIZE AS ORDERED VARIABLE
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

# complete space
all <- cbind (pco$li[,1:2],ext = F)
a <- all [chull(all[,1:2], y = NULL),]# its convex hull

## extracted data of impaired species of each coral spp
impaired_sp <- unique(total [which(total$low >0),"peixe"])

# reduced space
setB<-cbind(all, ext1=ifelse(rownames(all) %in% impaired_sp,T,F))
pk <-setB[which(setB$ext1==F),]
f <- pk [chull(pk, y = NULL),] # reduced hull area

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

# plotting
## plot A (complete)
plotA <- ggplot(a, aes(A1, A2)) + 
  geom_point() + theme_bw()+
  geom_polygon(data=a, aes (A1,A2),alpha=0.5,fill="gray") + # complete space
  geom_polygon(data=f, aes (A1,A2,group=ext1, fill=ext1),alpha=0.5, # reduced space
               fill="black",size=3) +
  xlim(min (a$A1)-0.2,max (a$A1)+0.2)

## correlations to project trait values into the ordination plot
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

ggsave(here ("output_comm_wide", "Fspace_longo_global.pdf"), 
       width = 4,height=4) 

## things to report
res_global <- list (space = red.space,
             first.axis = Inertia.first,
             scnd.axis = Inertia.scnd,
             fuck_sp = fuck_sp)

res_global$space # functional space (remember RTS = 1-red)

#- -----------------------------------------
# 		RANDOM LOSS SCENARIO
# ----------------------------------------

# dfish traits
subset1 <- traits_peixes_table [which(traits_peixes_table$Name %in% 
                                  unique(total$peixe)),
                          c("Name","Body_size", 
                            "Size_group",
                            "Aspect_ratio",
                            "Trophic_level")]

# group size as an ordered variable
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
a <- all [chull(all[,1:2], y = NULL),] # its hull

## extracted data of impaired species of each coral spp
impaired_sp <- replicate (100,sample (total$peixe,# 100 samples 
  length(unique(total [which(total$low >0),"peixe"]))))# of size equal N associated fish

# run random sampling
extinction_random <- lapply(seq (1,ncol(impaired_sp)), function (i) {
  
  # reduced space
  setB<-cbind(all, ext1=ifelse(rownames(all) %in% impaired_sp[,i],T,F))
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
  
# extract interesting result
random_ext_pol <- sapply (extinction_random, "[","coord_space")

## plot A
plotA <- ggplot(a, aes(A1, A2)) + 
    geom_point() + theme_bw()+
    geom_polygon(data=a, aes (A1,A2),alpha=0.7,fill="gray10") + # complete
	# and then each randomly reduced space
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

## correlations to project trait values into the ordination plot
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

ggsave(here ("output_comm_wide", "Fspace_longo_random.pdf"), 
       width = 4,height=4) 

## things to report
res_random <- list (space = sapply (extinction_random,"[","red.space"),
             first.axis = Inertia.first,
             scnd.axis = Inertia.scnd,
             fuck_sp = fuck_sp)

apply(do.call(rbind,sapply (extinction_random,"[","red.space")),2,mean)
apply(do.call(rbind,sapply (extinction_random,"[","red.space")),2,sd)

# ---------------------------------------------------------------- #
# check the amount of functional space occupied by the 47 spp
# supporting information

# dfish traits
subset1 <- traits_peixes [which(gsub(" ",".",tolower (traits_peixes$Name)) %in% 
                                  todas_sp_longo),
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

# space of analyzed spp
setB<-cbind(all, ext1=ifelse(gsub(" ",".",tolower (rownames(all))) %in% sp_longo,F,T))
pk <-setB[which(setB$ext1==F),]
f <- pk [chull(pk, y = NULL),]

## space of associated / influenced by corals 
fuck_sp <- unique(total [which(total$low >0),"peixe"])
fuck_sp<-tolower(gsub (" ",".",fuck_sp,))

setc<-cbind(all, ext1=ifelse(gsub(" ",".",tolower (rownames(all))) %in% fuck_sp,F,T))
pkB <-setc[which(setc$ext1==F),]
dep <- pkB [chull(pkB, y = NULL),]

## quantifying reduction in functional space

# https://chitchatr.wordpress.com/2015/01/23/calculating-the-area-of-a-convex-hull/
# the complete trait space
chull.poly.complete <- Polygon(a[,1:2], hole=F)
(chull.area.complete <- chull.poly.complete@area)
# space of spp detected in videoplots
chull.poly.complete <- Polygon(f[,1:2], hole=F)
(chull.area.videop <- chull.poly.complete@area)
# space of coral-associated fish
chull.poly.complete <- Polygon(dep[,1:2], hole=F)
(chull.area.videop.reliant <- chull.poly.complete@area)

## plot A
plotA <- ggplot(a, aes(A1, A2)) + 
  geom_point() + theme_bw()+
  geom_polygon(data=a, aes (A1,A2),alpha=0.5,fill="gray") + 
  geom_polygon(data=f, aes (A1,A2,group=ext1, fill=ext1),alpha=0.5,
               fill="black",size=3) +
  geom_polygon(data=dep, aes (A1,A2,group=ext1, fill=ext1),alpha=0.5,
               fill="orange",size=3) +
  xlim(min (a$A1)-0.2,max (a$A1)+0.2) + 
  xlab (paste ("A1 (", round(Inertia.first*100,2),"%)",sep=""))+
  ylab (paste ("A2 (", round(Inertia.scnd*100,2),"%)",sep=""))

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
           y=correlations[2,4]*0.25,label="Trophic level") + 
  
  # ANOTATE FUNCTIONAL SPACE SIZE
  annotate(geom="text",x=-0.12,xmin=-0.12,
           y=-0.15,ymin=-0.15,colour="white",
           label=paste(round(chull.area.videop/chull.area.complete,2)*100,
                       "%")) + 
  # ANOTATE FUNCTIONAL SPACE SIZE
  annotate(geom="text",x=-0.05,xmin=-0.05,
           y=-0.1,ymin=-0.1,colour="white",
           label=paste(round(chull.area.videop.reliant/chull.area.complete,2)*100,
                       "%"))


# save plot
ggsave(here ("output","Vect",filename = paste ("Fspace_longo_all_fish.pdf")), 
       width = 4,height=4) 



# end
