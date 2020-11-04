## code to extract information needed to represent the strength of association between coral cover/site occupancy and fish site-occupancy probability
## I had to do this code because the output of site-occupancy models are too large
## the computer could not deal with such amount of estimates. Thus, for original cover/occupancy, and discounts of
## 25% and 50% of these quantities, I open the model, extracted one of them (original first, 25% discount, 50% discount) and 
## extracted relevant information. The information I need is in a list of tables. Each table refers to the results of one of 6 coral species.
## The fish species are in the table rows, and the interesting quantities are in the cols.

# load packages and functions
source ("R/packages.R")
source ("R/functions.R")

# ---------------------# 
# MAP (figure 1)
# ---------------------#

# ---------------------# 
# fish data  -  Morais
# ---------------------# 

load (here("output","Data_fish_detection_MORAIS_AUED.RData"))

## coral detection - with coordinates - MORAIS
load (here("output","Data_coral_detection_MORAIS_AUED.RData"))

# mapa mundi
world <- ne_countries(scale = "medium", returnclass = "sf")

# cortar o mapa para ver a america do Sul e parte da central
wm <- ggplot() + 
  geom_sf (data=world, size = 0.1, 
           fill= "gray90",colour="gray90") +
  coord_sf (xlim = c(-50, -25),  ylim = c(-27, 2), expand = FALSE) +
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

# col for none cover
sp_cover_data_morais <- cbind(sp_cover_data,
                       None=ifelse (rowSums (sp_cover_data) ==0, 1,0))
# jittered coordinates
sp_cover_data_morais <- cbind (sp_cover_data_morais,
                            LonJitter = jitter (coordenadas$Lon,factor=400),
                            LatJitter=jitter (coordenadas$Lat,factor=600))
# transforming into DF
sp_cover_data_morais <- as.data.frame(sp_cover_data_morais)
rownames(sp_cover_data_morais) <- sitios_bentos

# pie chart with sites of Morais
wm_pie_morais <- wm + geom_scatterpie(aes(x=LonJitter, y=LatJitter),
                               data = sp_cover_data_morais,
                               cols = c("Millepora.alcicornis",
                                        "Montastraea.cavernosa",
                                        "Mussismilia.hispida",
                                        "Siderastrea.spp",
                                        "None"),
                               pie_scale = 1.5,
                               size=0.4,
                               alpha = 0.65,
                               color="gray50",
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
sp_cover_data_longo$LonJitter <- sp_cover_data_longo$LonJitter + 6

## advice to jitter : https://stackoverflow.com/questions/52806580/pie-charts-in-geom-scatterpie-overlapping
## pie: http://www.spectdata.com/index.php/2018/10/25/how-to-use-ggplot-to-plot-pie-charts-on-a-map/

# pie chart with sites of Morais
wm_pie_longo <- wm_pie_morais + geom_scatterpie(aes(x=LonJitter, y=LatJitter),
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

pdf(here ("output", "Vect","MapPoints.pdf"),width = 10,heigh=7)
  wm_pie_longo
dev.off()

# ---------------------------------------- #
# information of fish species
# ---------------------------------------- #

load (here("output","Data_fish_detection_MORAIS_AUED.RData"))
sp_morais <- unique(unlist(fish_species))

load (here("output","Data_fish_detection_LONGO_AUED.RData"))
sp_longo <- unique(unlist(fish_species))

# 
tab_spp <- data.frame (sp = unique(c(sp_morais, sp_longo)))
tab_spp$video <- ifelse (tab_spp$sp %in% sp_longo, 1,0)
tab_spp$UVS <- ifelse (tab_spp$sp %in% sp_morais, 1,0)
write.csv (tab_spp, file =here("output","tab_spp.csv"))
# quantas em comum?
table(sp_morais %in% sp_longo )

## load trait data
traits_peixes <- read.csv(here("data","traits","Atributos_especies_Atlantico_&_Pacifico_Oriental_2020_04_28.csv"),
                          h=T,sep=";")
traits_peixes$Name <- tolower (gsub (" ",".", traits_peixes$Name))
traits_peixes$Body_size <-  gsub(",",".",traits_peixes$Body_size)
traits_peixes$Body_size <- as.numeric(traits_peixes$Body_size)
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


# ---------------------------------------- #
# FIGURE 2, FISH RELIANCE ON CORAL COVER
# ---------------------------------------- #

## fish data
load (here("output","Data_fish_detection_MORAIS_AUED.RData"))

## coral detection - with coordinates
load (here("output","Data_coral_detection_MORAIS_AUED.RData"))

## LOAD MODEL RESULTS
load(here("output","samples_OCCcoral_PdepthObsID.RData")) 

## LOAD MODEL RESULTS
load(here("output","samples_OCCcoral_PdepthObsID_RdmP.RData")) 

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


### -------------------- #
###  assessing model fit
### -------------------- #

bpv_uvs <- lapply (seq (1,length(samples_OCCcoral_PdepthObsID)), function (i) 
  do.call (rbind, lapply(seq(1,length (fish_species[[i]])), function (k) {
    
    bpvID<- sum (samples_OCCcoral_PdepthObsID[[i]]$sims.list$Chi2repClosed[,k] > samples_OCCcoral_PdepthObsID[[i]]$sims.list$Chi2Closed[,k])/length(samples_OCCcoral_PdepthObsID[[i]]$sims.list$Chi2Closed[,k])
    bpvRdmP<- sum (samples_OCCcoral_PdepthObsID_RdmP[[i]]$sims.list$Chi2repClosed[,k] > samples_OCCcoral_PdepthObsID_RdmP[[i]]$sims.list$Chi2Closed[,k])/length(samples_OCCcoral_PdepthObsID_RdmP[[i]]$sims.list$Chi2Closed[,k])
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
          
          intercept = mean (samples_OCCcoral_PdepthObsID_RdmP[[coral]]$sims.list$intercept.psi [,fish]),

          low.int = quantile (samples_OCCcoral_PdepthObsID_RdmP[[coral]]$sims.list$intercept.psi [,fish], 0.05),
          
          high.int = quantile (samples_OCCcoral_PdepthObsID_RdmP[[coral]]$sims.list$intercept.psi [,fish], 0.95),
          
          estimate = mean (samples_OCCcoral_PdepthObsID_RdmP[[coral]]$sims.list$beta1 [,fish]),
  
          low = quantile (samples_OCCcoral_PdepthObsID_RdmP[[coral]]$sims.list$beta1 [,fish], 0.05),

          high = quantile (samples_OCCcoral_PdepthObsID_RdmP[[coral]]$sims.list$beta1 [,fish],0.95)


        )
      )
   )
)

## binding the bayeasian P- value
extracted_data<- lapply (seq(1,length(extracted_data)), function (i)
  cbind (extracted_data[[i]],bpv_uvs [[i]])
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

  
  pdf (file=here("output", "Vect",paste (i,"morais.pdf",sep="_")),width=4,heigh=3)
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

    ;

  ## things to report
  teste

})

write.csv (do.call(rbind, sp_analyzed),
           file=here("output","Tabs","coefs_fit.csv"))

# ------------------------------------------------------- #
# functional spaces
# here considering that all fish species with influence 
# of corals would disappear in the future
# ------------------------------------------------------ #

f.space <- lapply (seq (1,length(extracted_data)), function(k) {
    
      # ext coeff
    coef_fish <- extracted_data[[k]]
    
    # rem species
    coef_fish <- coef_fish[which(coef_fish$peixe %in% sp_analyzed[[k]]$peixe == T),]
    
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
        
        ## scatter(pco)
        ## 
        
        ## complete space
        all <- cbind (pco$li[,1:2],ext = F)
        a <- all [chull(all[,1:2], y = NULL),]
        
        ## extracted data of impaired species
        fuck_sp <- extracted_data[[k]][which(extracted_data[[k]]$low >0),"peixe"]
        
        # reduced space
        setB<-cbind(all, ext1=ifelse(rownames(all) %in% fuck_sp,T,F))
        pk <-setB[which(setB$ext1==T),]
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
    
    ggsave(here ("output","Vect",filename = paste ("Fspace",coral_species[k],".pdf")), 
           width = 4,height=4) 
    
    ## things to report
    res <- list (space = red.space,
                 first.axis = Inertia.first,
                 scnd.axis = Inertia.scnd)
                              
    }
    
    
    ;
    
    res
    
})

f.space

## IUCN
lapply (sp_analyzed, function (i)
  
  traits_peixes[which(traits_peixes$Name %in% i$peixe==T),"IUCN_status"]
)

# ------------------------------ #
# here based on predicted effect
# of coral cover reduction
# ------------------------------ #

# select one sp
species <- 1
coral <- 1

#### trying to predict that (somehow)
beta0 <- extracted_data[[coral]]$intercept [species]
beta1 <- extracted_data[[coral]]$estimate [species]
beta0low <- extracted_data[[coral]]$low.int[species]
beta1low <- extracted_data[[coral]]$low[species]
beta0high <- extracted_data[[coral]]$high.int[species]
beta1high <- extracted_data[[coral]]$high[species]

Data <- (sp_cover_data[,1]-mean(sp_cover_data[,1]))/sd(sp_cover_data[,1])

plot(Data[order(Data)], 
     plogis(beta0+ beta1*Data[order(Data)]),
            type="l")

# simulate new data along the x axis
NewData<-cbind(1,x=seq(0,1,0.2))#cobertura de coral 20%, 60% e 80%

(Y_esperadoOcc<-as.vector(plogis(NewData %*% c(beta0,beta1))))
(Y_esperadoOcc_low<-as.vector(plogis(NewData %*% c(beta0low,beta1low))))
(Y_esperadoOcc_high<-as.vector(plogis(NewData %*% c(beta0high,beta1high))))

# 
plot(NewData[,2][order(plogis(beta0 + beta1 * NewData[,2]))],
     plogis(beta0 + beta1 * NewData[,2])[order(plogis(beta0 + beta1 * NewData[,2]))],
     pch=19,
     col="black",
     type="l",ylim=c(0,1))

lines(NewData[,2][order(plogis(beta0low + beta1low * NewData[,2]))],
      plogis(beta0low + beta1low * NewData[,2])[order(plogis(beta0low + beta1low * NewData[,2]))],
      col = "gray")
lines(NewData[,2][order(plogis(beta0high + beta1high * NewData[,2]))],
      plogis(beta0high + beta0high * NewData[,2])[order(plogis(beta0high + beta1high * NewData[,2]))],
      col = "gray")


to_pred <- c(0.75,0.5,0.25,0.1)
lapply (seq(1,length (to_pred)), function (i)
  lapply (seq (1,length(beta0r)), function (k) {
    
    new_datab <- data.frame(x =coral_cover_data[,1]*to_pred[i])
    
    # 
    lines(new_data[,1][order(plogis(beta0r[k] + beta1r[k] * new_data[,1]))],
          plogis(beta0r[k] + beta1r[k] * new_datab[,1])[order(plogis(beta0r[k] + beta1r[k] * new_datab[,1]))],
          col = "gray")
    
  }))

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
  
  
  pdf (file=here("output", "Vect",paste (i,"longo.pdf",sep="_")),width=4,heigh=3)
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
  
  ;
  
  ## things to report
  teste
  
})

write.csv (do.call(rbind, sp_analyzed),
           file=here("output","Tabs","coefs_fit_longo.csv"))

# ------------------------------------------------------- #
# functional spaces
# here considering that all fish species with influence 
# of corals would disappear in the future
# ------------------------------------------------------ #

f.space <- lapply (seq (1,length(extracted_data)), function(k) {
  
  # ext coeff
  coef_fish <- extracted_data[[k]]
  
  # rem species
  coef_fish <- coef_fish[which(coef_fish$peixe %in% sp_analyzed[[k]]$peixe == T),]
  
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
  fuck_sp <- extracted_data[[k]][which(extracted_data[[k]]$low >0),"peixe"]
  
  # reduced space
  setB<-cbind(all, ext1=ifelse(rownames(all) %in% fuck_sp,T,F))
  pk <-setB[which(setB$ext1==T),]
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
               scnd.axis = Inertia.scnd)
  ;
  
  res
  
  })

f.space


## IUCN
lapply (sp_analyzed, function (i)
  
  traits_peixes[which(traits_peixes$Name %in% i$peixe==T),"IUCN_status"]
)

