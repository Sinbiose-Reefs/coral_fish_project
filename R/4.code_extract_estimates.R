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
                               size=0.5,
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
                               size=0.7,
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

wm_pie_longo <- wm_pie_longo + ggtitle("Species cover relative to total coral cover")+
  xlab("Longitude") + ylab("Latitude")

pdf(here ("output", "MapPoints.pdf"),width = 10,heigh=7)
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
# MODEL FIT
# ---------------------------------------- #

## LOAD MODEL RESULTS
load(here("output","samples_OCCcoral_PdepthObsID.RData")) 

## LOAD MODEL RESULTS
load(here("output","samples_OCCcoral_PdepthObsID_RdmP.RData")) 



# ---------------------------------------- #
# FIGURE 2, FISH RELIANCE ON CORAL COVER
# ---------------------------------------- #

## fish data
load (here("output","Data_fish_detection_MORAIS_AUED.RData"))

## coral detection - with coordinates
load (here("output","Data_coral_detection_MORAIS_AUED.RData"))

## LOAD MODEL RESULTS
load(here("output","samples_OCCcoral_PdepthObsID.RData")) 

## load trait data
traits_peixes <- read.csv(here("data","traits","Atributos_especies_Atlantico_&_Pacifico_Oriental_2020_04_28.csv"),
                          h=T,sep=";")
traits_peixes$Name <- tolower (gsub (" ",".", traits_peixes$Name))

# adjusting trait values
traits_peixes$Body_size <- as.numeric(gsub (",",".",traits_peixes$Body_size))
traits_peixes$Aspect_ratio <- as.numeric(gsub (",",".",traits_peixes$Aspect_ratio))
traits_peixes$Trophic_level <- as.numeric(gsub (",",".",traits_peixes$Trophic_level))

## data to use in the plot
extracted_data <- 
  
  lapply (seq(1,length(coral_species)), function (coral)
            
    do.call(rbind,lapply (seq (1, length (fish_species [[coral]])), function (fish)
    
    data.frame (
  
          coral = coral_species[coral],

          peixe = fish_species [[coral]][fish],
          
          intercept = mean (samples_OCCcoral_PdepthObsID[[coral]]$sims.list$intercept.psi [,fish]),

          low.int = quantile (samples_OCCcoral_PdepthObsID[[coral]]$sims.list$intercept.psi [,fish], 0.05),
          
          high.int = quantile (samples_OCCcoral_PdepthObsID[[coral]]$sims.list$intercept.psi [,fish], 0.95),
          
          estimate = mean (samples_OCCcoral_PdepthObsID[[coral]]$sims.list$beta1 [,fish]),
  
          low = quantile (samples_OCCcoral_PdepthObsID[[coral]]$sims.list$beta1 [,fish], 0.05),

          high = quantile (samples_OCCcoral_PdepthObsID[[coral]]$sims.list$beta1 [,fish],0.95)


        )
      )
   )
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
  rem_sp <- teste [which(teste$estimate < 0 & teste$high < 0),"peixe"]
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
    xlim(-30, 30) + 
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

dev.off() ;

rem_sp

})

# ------------------------------------------------------- #
# functional spaces
# here considering that all fish species with influence 
# of corals would disappear in the future
# ------------------------------------------------------ #

f.space <- lapply (c(1,3,4), function(k) {
    
      # ext coeff
    coef_fish <- extracted_data[[k]]
    # rem species
    coef_fish <- coef_fish[which(coef_fish$peixe %in% sp_analyzed[[k]] != T),]
    
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
    
    chull.poly.exc <- Polygon(f[,1:2], hole=F)
    chull.area.exc <- chull.poly.exc@area
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
    
    ggsave(here ("output","Vect",filename = paste ("Fspace",coral_species[k],".pdf")), 
           width = 4,height=4) 
    ;
    
    red.space

})

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






## The same  for Longo et al. 

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

## load trait data
traits_peixes <- read.csv(here("data","traits","Atributos_especies_Atlantico_&_Pacifico_Oriental_2020_04_28.csv"),
                          h=T,sep=";")
traits_peixes$Name <- tolower (gsub (" ",".", traits_peixes$Name))

# cenarios
coral_cover_data <- lapply (seq(1,ncol(sp_cover_data)), function (i) {
  
  coral_cover <- cbind (original= sp_cover_data[,i], 
                        less20 = sp_cover_data[,i] * 0.80,
                        less40 = sp_cover_data[,i] * 0.60,
                        less60 = sp_cover_data[,i] * 0.40,
                        less80 = sp_cover_data[,i] * 0.20)
})


coral_species <- c(grep("Millepora",coral_species),
                   grep("Mussismilia.hispida",coral_species))

## data to use in the plot
extracted_data <- 
  
  lapply (seq(1,length(coral_species)), function (coral)
    
    lapply (seq (1, length (fish_species [[coral]])), function (fish)
      
      do.call(rbind, lapply (seq (1,length(colnames(coral_cover_data[[coral]]))), function (cenario)
        
        data.frame (
          
          coral = coral_species[coral],
          
          peixe = fish_species [[coral]][fish],
          
          scenario = colnames(coral_cover_data[[coral]])[cenario],
          
          estimate = mean (samples_OCCcoral_PdepthTime_longo[[coral]][[cenario]]$sims.list$beta1 [,fish]),
          
          low = quantile (samples_OCCcoral_PdepthTime_longo[[coral]][[cenario]]$sims.list$beta1 [,fish], 0.05),
          
          high = quantile (samples_OCCcoral_PdepthTime_longo[[coral]][[cenario]]$sims.list$beta1 [,fish],0.95)
          
        )
      )
      )
    ))



lapply (seq (1,length(extracted_data)), function (i) {
  
  teste <- do.call (rbind, extracted_data[[i]])
  
  teste$scenario<-factor(teste$scenario,
                        levels = c("less80","less60","less40","less20","original"))
  teste <- teste[-which(teste$scenario=="less60"),]
  
  ## organizar nomes das spp no eixo Y
  ## subset de millepora 
  subset1 <- traits_peixes[which(traits_peixes$Name %in% unique (teste$peixe)),c("Name", "Body_size", "Diet")]
  subset1$Body_size <- as.numeric (gsub (",","." ,subset1$Body_size))
  subset1 <- subset1 [order (subset1$Body_size,decreasing=F),]
  # organizing species according to size
  teste <- teste[order(match(teste$peixe,subset1$Name)),]
  teste$peixe <- factor (teste$peixe,
                         levels = unique(teste$peixe))
  # plot
  pd <- position_dodge(.5)
  
  a <- ggplot (teste, aes  (y=peixe, x=estimate, fill=scenario,
                            colour=scenario)) + 
    geom_errorbar(aes(xmin=low,xmax=high),width = .3,
                  position=pd) + theme_classic() + 
    geom_point(aes(estimate),size=2,position=pd) + 
    geom_vline(xintercept = 0, linetype="dashed", 
               color = "gray50", size=0.5)+
    scale_color_manual(values=c("gray70", "gray60", "gray50",
                                "gray40","black")) + 
    xlab("Regression coefficient estimate") + 
    ylab ("Reef fish species") + 
    xlim(-18, 18)
  
  
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
  
  
  pdf (file=here("output", paste (i,"longo.pdf",sep="_")),width=7,heigh=7)
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
            x = unit(0.22, "npc"), 
            y = unit(.94, "npc"),
            gp = gpar(fontsize=8),
            rot=90)
  
  dev.off()
})


### fucked species

## the functional space

# adjusting trait values
traits_peixes$Body_size <- as.numeric(gsub (",",".",traits_peixes$Body_size))
traits_peixes$Aspect_ratio <- as.numeric(gsub (",",".",traits_peixes$Aspect_ratio))

# subsetting traits

k = 2 # coral species 

coef_fish <- do.call (rbind, extracted_data[[k]])

coef_fish$scenario<-factor(coef_fish$scenario,
                          levels = c("less80","less60","less40","less20","original"))
coef_fish <- coef_fish[-which(coef_fish$scenario=="less60"),]

subset1 <- traits_peixes [which(traits_peixes$Name %in% unique(coef_fish$peixe)),c("Name","Body_size", 
                                                                                   "Size_group",
                                                                                   "Aspect_ratio")]

subset1$Size_group <- sapply(subset1$Size_group , function(x) {if (x=="sol") {1} 
  else if (x=="pair") {2} 
  else if (x=="smallg") {3} 
  else if (x=="medg") {4} 
  else if (x=="largeg") {5}}
)
subset1$Size_group <-ordered (subset1$Size_group)
rownames(subset1) <- subset1$Name; subset1<- subset1[,-1]

# first calculate gower distance on traits
gower_matrix <- daisy (subset1, metric=c("gower")) 

# Building the functional space based on a PCOA 
pco<-dudi.pco(quasieuclid(gower_matrix), scannf=F, nf=10) # quasieuclid() transformation to make the gower matrix as euclidean. nf= number of axis 
barplot(pco$eig) # barplot of eigenvalues for each axis 
(Inertia2<-(pco$eig[1]+pco$eig[2]) /(sum(pco$eig))) # percentage of inertia explained by the two first axes

##

all <- cbind (pco$li[,1:2],ext = F)
a <- all [chull(all[,1:2], y = NULL),]

## extracted data of impaired species
fuck_sp <- unlist(lapply (seq (1,length(extracted_data[[k]])), function (i)
  ifelse (extracted_data[[k]][[i]]$low[1] >0 &
            extracted_data[[k]][[i]]$low[5] <0,unique(extracted_data[[k]][[i]]$peixe),
          NA)
))

## those that decrease fastly with coral loss
fuck_sp_neg <- unlist(lapply (seq (1,length(extracted_data[[k]])), function (i)
  ifelse (extracted_data[[k]][[i]]$estimate[1] > extracted_data[[k]][[i]]$estimate[5] &
            extracted_data[[k]][[i]]$high[5] < 0,unique(extracted_data[[k]][[i]]$peixe),
          NA)))

fuck_sp_all <- unique(c(fuck_sp, fuck_sp_neg))

# 
setB<-cbind(all, ext1=ifelse(rownames(all) %in% fuck_sp_all,T,F))
pk <-setB[which(setB$ext1==T),]
f <- pk [chull(pk, y = NULL),]

## plot A
plotA <- ggplot(a, aes(A1, A2)) + 
  geom_point() + theme_bw()+
  geom_polygon(data=a, aes (A1,A2),alpha=0.8,fill="black") + 
  geom_polygon(data=f, aes (A1,A2,group=ext1, fill=ext1),alpha=0.8,fill="gray",size=3)

plotA

## correlations
subset1$Size_group <- as.numeric (subset1$Size_group)
cor (pco$li[is.na(subset1$Aspect_ratio) !=T,1:2],
     subset1[is.na(subset1$Aspect_ratio) !=T,])

pdf(file=here("output","Fspace_longo.pdf"))
plotA + geom_segment(aes(x = 0, y = 0, xend = 0.35, yend = 0.35),size = 2,
                     arrow = arrow(length = unit(.5, "cm"))) + 
  annotate(geom="text",x=0.3,y=0.37,label="Body size") + 
  geom_segment(aes(x = 0, y = 0, xend = -0.35, yend = 0.00),size = 2,
               arrow = arrow(length = unit(.5, "cm"))) + 
  annotate(geom="text",x=-.32,y=-0.03,label="Size group") +
  geom_segment(aes(x = 0, y = 0, xend = -0.3, yend = 0.35),size = 2,
               arrow = arrow(length = unit(.5, "cm"))) + 
  annotate(geom="text",x=-.25,y=0.37,label="Aspect ratio")
dev.off()
####

















## table of bayesian P value for models with original coral cover
bvclosed_original <- lapply (lista_modelos_original, function (model)
  
  lapply (model, function (i)
    
    unlist(lapply (i, function (k)
      
      sum (k$sims.list$Chi2repClosed > k$sims.list$Chi2Closed)/
        length(k$sims.list$Chi2repClosed)
    )
    )
  )
)
## table of bayesian P value for models with estimated site occupancy
bvclosed_occupancy <- lapply (lista_modelos_occupancy, function (model)
  
  lapply (model, function (i)
    
    unlist(lapply (i, function (k)
      
      sum (k$sims.list$Chi2repClosed > k$sims.list$Chi2Closed)/
        length(k$sims.list$Chi2repClosed)
    )
    )
  )
)
# nomear a lista de resultados
bvclosed_original <- lapply (bvclosed_original, function (i) {names (i) <- sp_coral; i})
bvclosed_occupancy <- lapply (bvclosed_occupancy, function (i) {names (i) <- sp_coral; i})

# reorganizing the list
tab_bpv_sp_original <- lapply (sp_coral, function (sp)
  lapply (bvclosed_original, function (i) 
    i [ which (names(i) == sp)])
)
tab_bpv_sp_occupancy <- lapply (sp_coral, function (sp)
  lapply (bvclosed_occupancy, function (i) 
    i [ which (names(i) == sp)])
)

## transforming into matrix
tab_bpv_sp_original <- lapply (tab_bpv_sp_original, function (i) 
  data.frame (matrix(unlist(i),ncol= length(bvclosed_original ),byrow = F )))
names(tab_bpv_sp_original)<- sp_coral

tab_bpv_sp_occupancy <- lapply (tab_bpv_sp_occupancy, function (i) 
  data.frame (matrix(unlist(i),ncol= length(bvclosed_occupancy),byrow = F )))
names(tab_bpv_sp_occupancy)<- sp_coral

## selecao do melhor modelo por especie 
## de acordo com o  BPV mais proximo a 0.5 (using the function 'closest')

# model with BPV closest to 0.5 (better fit to data)
# original cover
mod_sel_original <- lapply (seq (1,length(tab_bpv_sp_original)), function (i) # sp coral - lista
  
  do.call(rbind, lapply (seq (1,nrow (tab_bpv_sp_original [[i]])), function (k) { # sp peixe - linhas
    
    
    ## obter o valor de bpv mais proxima a 0.5
    closestbpv <-as.numeric(closest (tab_bpv_sp_original[[i]][k,],0.5)[1])
    
    ## no caso de BPV=0 em todos, pega o primeiro modelo que eh o mais parcimonioso (menos parametros)
    qual.melhor <- ifelse (closestbpv == 0, 1, which(tab_bpv_sp_original[[i]][k,] == closestbpv)) 
    
    
  }
  )
  )
)

names(mod_sel_original)<- sp_coral
# site-occupancy probability
mod_sel_occupancy <- lapply (seq (1,length(tab_bpv_sp_occupancy)), function (i) # sp coral - lista
  
  do.call(rbind, lapply (seq (1,nrow (tab_bpv_sp_occupancy [[i]])), function (k) { # sp peixe - linhas
    
    
    ## obter o valor de bpv mais proxima a 0.5
    closestbpv <-as.numeric(closest (tab_bpv_sp_occupancy[[i]][k,],0.5)[1])
    
    ## no caso de BPV=0 em todos, pega o primeiro modelo que eh o mais parcimonioso (menos parametros)
    qual.melhor <- ifelse (closestbpv == 0, 1, which(tab_bpv_sp_occupancy[[i]][k,] == closestbpv)) 
    
    
  }
  )
  )
)

names(mod_sel_occupancy)<- sp_coral

## selecting models
# original
selected.models_original <- lapply (seq (1,length (lista_modelos_original [[1]])), function (k)
  lapply (seq (1,length (lista_modelos_original [[1]][[1]])), function (i) {
    
    sp.coral <-  k
    sp.peixe <-  i
    qual.sel <- mod_sel_original [[sp.coral]][sp.peixe,]
    
    
    selected.models <- (lista_modelos_original [[qual.sel]] # ## seleciona o modelo mod, 
                        [[sp.coral]] ## da sp de coral k
                        [[sp.peixe]]) ## sp de peixe i
    ; selected.models
    
  }))

names(selected.models_original) <- sp_coral

# occupancy
selected.models_occupancy <- lapply (seq (1,length (lista_modelos_occupancy [[1]])), function (k)
  lapply (seq (1,length (lista_modelos_occupancy [[1]][[1]])), function (i) {
    
    sp.coral <-  k
    sp.peixe <-  i
    qual.sel <- mod_sel_occupancy [[sp.coral]][sp.peixe,]
    
    
    selected.models <- (lista_modelos_occupancy [[qual.sel]] # ## seleciona o modelo mod, 
                        [[sp.coral]] ## da sp de coral k
                        [[sp.peixe]]) ## sp de peixe i
    ; selected.models
    
  }))

names(selected.models_occupancy) <- sp_coral

## de agora em diante, trabalhar somente com os modelos selecionados

## efeitos original
efeitos_original <-  lapply (selected.models_original, function (i) 
  
  do.call(cbind,lapply (i, function (k)
    
    k$summary [grep("beta1",rownames(k$summary)),"overlap0"])) ## pegar o subset com efeito significativo
  ## extrair os betas e se ou nao sobrepoe 0
)

tab_efeitos_sp_original <- lapply (efeitos_original, function (i)
  ifelse (i == 1, "No", "Yes")
)

# efeitos occupancy
efeitos_occupancy <-  lapply (selected.models_occupancy, function (i) 
  
  do.call (cbind, lapply (i, function (k)
    
    k$summary [grep("beta1",rownames(k$summary)),"overlap0"])) ## pegar o subset com efeito significativo
  ## extrair os betas e se ou nao sobrepoe 0
)

tab_efeitos_sp_occupancy <- lapply (efeitos_occupancy, function (i)
  ifelse (i == 1, "No", "Yes")
)

#### coeficientes de regressao
# original cover
coefic_original <-  lapply (selected.models_original, function (i) 
  
  do.call (cbind,lapply (i, function (k)
    
    k$summary [grep("beta1",rownames(k$summary)),"mean"])) ## pegar o subset com efeito significativo
  ## extrair os betas e se ou nao sobrepoe 0
)
# site-occupancy
coefic_occupancy <-  lapply (selected.models_occupancy, function (i) 
  
  do.call (cbind,lapply (i, function (k)
    
    k$summary [grep("beta1",rownames(k$summary)),"mean"])) ## pegar o subset com efeito significativo
  ## extrair os betas e se ou nao sobrepoe 0
)


## finite sample size (n.occ)
#### original cover
nocc_original <- lapply (selected.models_original, function (i) 
  
  unlist (lapply (i, function (k)
    
    k$summary [grep("n.occ",rownames(k$summary)),"mean"])) ## pegar o subset com efeito significativo
  ## extrair os betas e se ou nao sobrepoe 0
)

#### site-occupancy
nocc_occupancy <- lapply (selected.models_occupancy, function (i) 
  
  unlist (lapply (i, function (k)
    
    k$summary [grep("n.occ",rownames(k$summary)),"mean"])) ## pegar o subset com efeito significativo
  ## extrair os betas e se ou nao sobrepoe 0
)

cbind(nocc_original[[1]],nocc_occupancy[[1]])

######## detection probability
det_prob_original <-  lapply (selected.models_original, function (i) 
  
  unlist (lapply (i, function (k)
    
    k$summary [grep("mean.p",rownames(k$summary)),"mean"])) ## pegar o subset com efeito significativo
  ## extrair os betas e se ou nao sobrepoe 0
)

det_prob_occupancy <-  lapply (selected.models_occupancy, function (i) 
  
  unlist (lapply (i, function (k)
    
    k$summary [grep("mean.p",rownames(k$summary)),"mean"])) ## pegar o subset com efeito significativo
  ## extrair os betas e se ou nao sobrepoe 0
)

cbind(det_prob_original[[1]],det_prob_occupancy[[1]])

## bayesian p-value dos selecionados
# original
bpv.selected_original <- lapply (selected.models_original, function (i)
  
  unlist(lapply (i, function (k)
    
    sum (k$sims.list$Chi2repClosed > k$sims.list$Chi2Closed)/
      length(k$sims.list$Chi2repClosed)
  )
  )
)

# occupancy
bpv.selected_occupancy <- lapply (selected.models_occupancy, function (i)
  
  unlist(lapply (i, function (k)
    
    sum (k$sims.list$Chi2repClosed > k$sims.list$Chi2Closed)/
      length(k$sims.list$Chi2repClosed)
  )
  )
)

cbind(bpv.selected_original[[1]], bpv.selected_occupancy[[1]])

## tabela com as info do efeito das covs de coral nas spp
tab_summarized_original <- lapply(seq(1,length(tab_efeitos_sp_original)), function(i)
  data.frame(sp=especie, 
             effectReg1 = tab_efeitos_sp_original[[i]][1,],
             effectReg2 = tab_efeitos_sp_original[[i]][2,],
             coefReg1 = coefic_original[[i]][1,],
             coefReg2 = coefic_original[[i]][2,],
             #int = intercept[[i]],
             nocc = nocc_original[[i]],
             det = det_prob_original [[i]],
             bpv = bpv.selected_original[[i]],
             which_model=mod_sel_original[[i]]))


## nomes das sp de coral
names(tab_summarized_original) <- sp_coral

## occupancy
tab_summarized_occupancy <- lapply(seq(1,length(tab_efeitos_sp_occupancy)), function(i)
  data.frame(sp=especie, 
             effectReg1 = tab_efeitos_sp_occupancy[[i]][1,],
             effectReg2 = tab_efeitos_sp_occupancy[[i]][2,],
             coefReg1 = coefic_occupancy[[i]][1,],
             coefReg2 = coefic_occupancy[[i]][2,],
             #int = intercept[[i]],
             nocc = nocc_occupancy[[i]],
             det = det_prob_occupancy [[i]],
             bpv = bpv.selected_occupancy[[i]],
             which_model=mod_sel_occupancy[[i]])
)

## nomes das sp de coral
names(tab_summarized_occupancy) <- sp_coral
## effect (regression coefficient for each region)
## original cover
efeito_para_bipartite_original_reg1 <- sapply (tab_summarized_original, "[[","coefReg1")
efeito_para_bipartite_original_reg2 <- sapply (tab_summarized_original, "[[","coefReg2")
## setting rownames
rownames(efeito_para_bipartite_original_reg1)<- especie
rownames(efeito_para_bipartite_original_reg2)<- especie

## site occupancy
efeito_para_bipartite_occupancy_reg1 <-sapply (tab_summarized_occupancy, "[[","coefReg1")
efeito_para_bipartite_occupancy_reg2 <-sapply (tab_summarized_occupancy, "[[","coefReg2")
## setting rownames
rownames(efeito_para_bipartite_occupancy_reg1)<- especie
rownames(efeito_para_bipartite_occupancy_reg2)<- especie

## significance of the effect
## original cover
significance_original_reg1 <- sapply (tab_summarized_original, "[[","effectReg1")
significance_original_reg2 <- sapply (tab_summarized_original, "[[","effectReg2")
## transforming No and yes into 1 (significant effect) and 0 (no effect)
significance_original_reg1 <- ifelse (significance_original_reg1 =="Yes",1,0)
significance_original_reg2 <- ifelse (significance_original_reg2 =="Yes",1,0)
## setting rownames
rownames(significance_original_reg1 )<- especie
rownames(significance_original_reg2)<- especie

## site occupancy
significance_occupancy_reg1 <-sapply (tab_summarized_occupancy, "[[","effectReg1")
significance_occupancy_reg2 <-sapply (tab_summarized_occupancy, "[[","effectReg2")
## transforming No and yes into 1 (significant effect) and 0 (no effect)
significance_occupancy_reg1 <- ifelse (significance_occupancy_reg1 =="Yes",1,0)
significance_occupancy_reg2 <- ifelse (significance_occupancy_reg2 =="Yes",1,0)
## setting rownames
rownames(significance_occupancy_reg1)<- especie
rownames(significance_occupancy_reg2)<- especie

save(efeito_para_bipartite_original_reg1 ,
     efeito_para_bipartite_original_reg2 ,
     efeito_para_bipartite_occupancy_reg1 ,
     efeito_para_bipartite_occupancy_reg2 ,
     significance_original_reg1 ,
     significance_original_reg2 ,
     significance_occupancy_reg1 ,
     significance_occupancy_reg2,
     file=here("output","coefficients_ORIGINAL.RData")) 

###### LOSS OF 25% OF COVER OR OCCUPANCY PROBABILITY

## LOAD MODEL RESULTS

load(here("output","samples_OCCcoral_PdepthObsID.RData")) 
res1_orig25 <- samples_OCCcoral_PdepthObsID$cob_desc25
res1_occ25 <- samples_OCCcoral_PdepthObsID$ocupacao_desc25

rm(samples_OCCcoral_PdepthObsID)
gc()
load(here("output","samples_OCCcoral_PdepthObsIDRndm.RData"))
res2_orig25 <- samples_OCCcoral_PdepthObsIDRndm$cob_desc25
res2_occ25 <- samples_OCCcoral_PdepthObsIDRndm$ocupacao_desc25
rm(samples_OCCcoral_PdepthObsIDRndm)
gc()

load(here("output","samples_OCCcoralDepth_PObsIDRndm.RData"))
res3_orig25 <- samples_OCCcoralDepth_PObsIDRndm$cob_desc25
res3_occ25 <- samples_OCCcoralDepth_PObsIDRndm$ocupacao_desc25
rm(samples_OCCcoralDepth_PObsIDRndm)
gc()

load(here("output","StaticModelOccCoral_IDobsRdmP.RData"))
res4_orig25 <- StaticModelOccCoral_IDobsRdmP$cob_desc25
res4_occ25 <- StaticModelOccCoral_IDobsRdmP$ocupacao_desc25
rm(StaticModelOccCoral_IDobsRdmP)
gc()

##

## Creating a list with the results of each model
lista_modelos_original25 <- list(res1_orig25,
                                 res2_orig25,
                                 res3_orig25,
                                 res4_orig25)
lista_modelos_occupancy25 <- list(res1_occ25,
                                  res2_occ25,
                                  res3_occ25,
                                  res4_occ25)

# nomear a lista de resultados
lista_modelos_original25 <- lapply (lista_modelos_original25, function (i) {names (i) <- sp_coral; i})
lista_modelos_occupancy25 <- lapply (lista_modelos_occupancy25, function (i) {names (i) <- sp_coral; i})

## table of bayesian P value for models with original coral cover
bvclosed_original25 <- lapply (lista_modelos_original25, function (model)
  
  lapply (model, function (i)
    
    unlist(lapply (i, function (k)
      
      sum (k$sims.list$Chi2repClosed > k$sims.list$Chi2Closed)/
        length(k$sims.list$Chi2repClosed)
    )
    )
  )
)
## table of bayesian P value for models with estimated site occupancy
bvclosed_occupancy25 <- lapply (lista_modelos_occupancy25, function (model)
  
  lapply (model, function (i)
    
    unlist(lapply (i, function (k)
      
      sum (k$sims.list$Chi2repClosed > k$sims.list$Chi2Closed)/
        length(k$sims.list$Chi2repClosed)
    )
    )
  )
)
# nomear a lista de resultados
bvclosed_original25 <- lapply (bvclosed_original25, function (i) {names (i) <- sp_coral; i})
bvclosed_occupancy25 <- lapply (bvclosed_occupancy25, function (i) {names (i) <- sp_coral; i})

# reorganizing the list
tab_bpv_sp_original25 <- lapply (sp_coral, function (sp)
  lapply (bvclosed_original25, function (i) 
    i [ which (names(i) == sp)])
)
tab_bpv_sp_occupancy25 <- lapply (sp_coral, function (sp)
  lapply (bvclosed_occupancy25, function (i) 
    i [ which (names(i) == sp)])
)

## transforming into matrix
tab_bpv_sp_original25 <- lapply (tab_bpv_sp_original25, function (i) 
  data.frame (matrix(unlist(i),ncol= length(bvclosed_original25),byrow = F )))
names(tab_bpv_sp_original25)<- sp_coral

tab_bpv_sp_occupancy25 <- lapply (tab_bpv_sp_occupancy25, function (i) 
  data.frame (matrix(unlist(i),ncol= length(bvclosed_occupancy25),byrow = F )))
names(tab_bpv_sp_occupancy25)<- sp_coral

## selecao do melhor modelo por especie 
## de acordo com o  BPV mais proximo a 0.5 (using the function 'closest')

# model with BPV closest to 0.5 (better fit to data)
# original cover
mod_sel_original25 <- lapply (seq (1,length(tab_bpv_sp_original25)), function (i) # sp coral - lista
  
  do.call(rbind, lapply (seq (1,nrow (tab_bpv_sp_original25 [[i]])), function (k) { # sp peixe - linhas
    
    
    ## obter o valor de bpv mais proxima a 0.5
    closestbpv <-as.numeric(closest (tab_bpv_sp_original25[[i]][k,],0.5)[1])
    
    ## no caso de BPV=0 em todos, pega o primeiro modelo que eh o mais parcimonioso (menos parametros)
    qual.melhor <- ifelse (closestbpv == 0, 1, which(tab_bpv_sp_original25[[i]][k,] == closestbpv)) 
    
    
  }
  )
  )
)

names(mod_sel_original25)<- sp_coral
# site-occupancy probability
mod_sel_occupancy25 <- lapply (seq (1,length(tab_bpv_sp_occupancy25)), function (i) # sp coral - lista
  
  do.call(rbind, lapply (seq (1,nrow (tab_bpv_sp_occupancy25 [[i]])), function (k) { # sp peixe - linhas
    
    
    ## obter o valor de bpv mais proxima a 0.5
    closestbpv <-as.numeric(closest (tab_bpv_sp_occupancy25[[i]][k,],0.5)[1])
    
    ## no caso de BPV=0 em todos, pega o primeiro modelo que eh o mais parcimonioso (menos parametros)
    qual.melhor <- ifelse (closestbpv == 0, 1, which(tab_bpv_sp_occupancy25[[i]][k,] == closestbpv)) 
    
    
  }
  )
  )
)

names(mod_sel_occupancy25)<- sp_coral

## selecting models
# original
selected.models_original25 <- lapply (seq (1,length (lista_modelos_original25 [[1]])), function (k)
  lapply (seq (1,length (lista_modelos_original25 [[1]][[1]])), function (i) {
    
    sp.coral <-  k
    sp.peixe <-  i
    qual.sel <- mod_sel_original25 [[sp.coral]][sp.peixe,]
    
    
    selected.models <- (lista_modelos_original25 [[qual.sel]] # ## seleciona o modelo mod, 
                        [[sp.coral]] ## da sp de coral k
                        [[sp.peixe]]) ## sp de peixe i
    ; selected.models
    
  }))

names(selected.models_original25) <- sp_coral

# occupancy
selected.models_occupancy25 <- lapply (seq (1,length (lista_modelos_occupancy25 [[1]])), function (k)
  lapply (seq (1,length (lista_modelos_occupancy25 [[1]][[1]])), function (i) {
    
    sp.coral <-  k
    sp.peixe <-  i
    qual.sel <- mod_sel_occupancy25 [[sp.coral]][sp.peixe,]
    
    
    selected.models <- (lista_modelos_occupancy25 [[qual.sel]] # ## seleciona o modelo mod, 
                        [[sp.coral]] ## da sp de coral k
                        [[sp.peixe]]) ## sp de peixe i
    ; selected.models
    
  }))

names(selected.models_occupancy25) <- sp_coral

## de agora em diante, trabalhar somente com os modelos selecionados

## efeitos original
efeitos_original25 <-  lapply (selected.models_original25, function (i) 
  
  do.call(cbind,lapply (i, function (k)
    
    k$summary [grep("beta1",rownames(k$summary)),"overlap0"])) ## pegar o subset com efeito significativo
  ## extrair os betas e se ou nao sobrepoe 0
)

tab_efeitos_sp_original25 <- lapply (efeitos_original25, function (i)
  ifelse (i == 1, "No", "Yes")
)

# efeitos occupancy
efeitos_occupancy25 <-  lapply (selected.models_occupancy25, function (i) 
  
  do.call (cbind, lapply (i, function (k)
    
    k$summary [grep("beta1",rownames(k$summary)),"overlap0"])) ## pegar o subset com efeito significativo
  ## extrair os betas e se ou nao sobrepoe 0
)

tab_efeitos_sp_occupancy25 <- lapply (efeitos_occupancy25, function (i)
  ifelse (i == 1, "No", "Yes")
)

#### coeficientes de regressao
# original cover
coefic_original25 <-  lapply (selected.models_original25, function (i) 
  
  do.call (cbind,lapply (i, function (k)
    
    k$summary [grep("beta1",rownames(k$summary)),"mean"])) ## pegar o subset com efeito significativo
  ## extrair os betas e se ou nao sobrepoe 0
)
# site-occupancy
coefic_occupancy25 <-  lapply (selected.models_occupancy25, function (i) 
  
  do.call (cbind,lapply (i, function (k)
    
    k$summary [grep("beta1",rownames(k$summary)),"mean"])) ## pegar o subset com efeito significativo
  ## extrair os betas e se ou nao sobrepoe 0
)


## finite sample size (n.occ)
#### original cover
nocc_original25 <- lapply (selected.models_original25, function (i) 
  
  unlist (lapply (i, function (k)
    
    k$summary [grep("n.occ",rownames(k$summary)),"mean"])) ## pegar o subset com efeito significativo
  ## extrair os betas e se ou nao sobrepoe 0
)

#### site-occupancy
nocc_occupancy25 <- lapply (selected.models_occupancy25, function (i) 
  
  unlist (lapply (i, function (k)
    
    k$summary [grep("n.occ",rownames(k$summary)),"mean"])) ## pegar o subset com efeito significativo
  ## extrair os betas e se ou nao sobrepoe 0
)

cbind(nocc_original25[[1]],nocc_occupancy25[[1]])

######## detection probability
det_prob_original25 <-  lapply (selected.models_original25, function (i) 
  
  unlist (lapply (i, function (k)
    
    k$summary [grep("mean.p",rownames(k$summary)),"mean"])) ## pegar o subset com efeito significativo
  ## extrair os betas e se ou nao sobrepoe 0
)

det_prob_occupancy25 <-  lapply (selected.models_occupancy25, function (i) 
  
  unlist (lapply (i, function (k)
    
    k$summary [grep("mean.p",rownames(k$summary)),"mean"])) ## pegar o subset com efeito significativo
  ## extrair os betas e se ou nao sobrepoe 0
)

cbind(det_prob_original25[[1]],det_prob_occupancy25[[1]])

## bayesian p-value dos selecionados
# original
bpv.selected_original25 <- lapply (selected.models_original25, function (i)
  
  unlist(lapply (i, function (k)
    
    sum (k$sims.list$Chi2repClosed > k$sims.list$Chi2Closed)/
      length(k$sims.list$Chi2repClosed)
  )
  )
)

# occupancy
bpv.selected_occupancy25 <- lapply (selected.models_occupancy25, function (i)
  
  unlist(lapply (i, function (k)
    
    sum (k$sims.list$Chi2repClosed > k$sims.list$Chi2Closed)/
      length(k$sims.list$Chi2repClosed)
  )
  )
)

cbind(bpv.selected_original25[[1]], bpv.selected_occupancy25[[1]])

## tabela com as info do efeito das covs de coral nas spp
tab_summarized_original25 <- lapply(seq(1,length(tab_efeitos_sp_original25)), function(i)
  data.frame(sp=especie, 
             effectReg1 = tab_efeitos_sp_original25[[i]][1,],
             effectReg2 = tab_efeitos_sp_original25[[i]][2,],
             coefReg1 = coefic_original25[[i]][1,],
             coefReg2 = coefic_original25[[i]][2,],
             #int = intercept[[i]],
             nocc = nocc_original25[[i]],
             det = det_prob_original25 [[i]],
             bpv = bpv.selected_original25[[i]],
             which_model=mod_sel_original25[[i]]))


## nomes das sp de coral
names(tab_summarized_original25) <- sp_coral

## occupancy
tab_summarized_occupancy25 <- lapply(seq(1,length(tab_efeitos_sp_occupancy25)), function(i)
  data.frame(sp=especie, 
             effectReg1 = tab_efeitos_sp_occupancy25[[i]][1,],
             effectReg2 = tab_efeitos_sp_occupancy25[[i]][2,],
             coefReg1 = coefic_occupancy25[[i]][1,],
             coefReg2 = coefic_occupancy25[[i]][2,],
             #int = intercept[[i]],
             nocc = nocc_occupancy25[[i]],
             det = det_prob_occupancy25 [[i]],
             bpv = bpv.selected_occupancy25[[i]],
             which_model=mod_sel_occupancy25[[i]])
)

## nomes das sp de coral
names(tab_summarized_occupancy25) <- sp_coral
## effect (regression coefficient for each region)
## original cover
efeito_para_bipartite_original25_reg1 <- sapply (tab_summarized_original25, "[[","coefReg1")
efeito_para_bipartite_original25_reg2 <- sapply (tab_summarized_original25, "[[","coefReg2")
## setting rownames
rownames(efeito_para_bipartite_original25_reg1)<- especie
rownames(efeito_para_bipartite_original25_reg2)<- especie

## site occupancy
efeito_para_bipartite_occupancy25_reg1 <-sapply (tab_summarized_occupancy25, "[[","coefReg1")
efeito_para_bipartite_occupancy25_reg2 <-sapply (tab_summarized_occupancy25, "[[","coefReg2")
## setting rownames
rownames(efeito_para_bipartite_occupancy25_reg1)<- especie
rownames(efeito_para_bipartite_occupancy25_reg2)<- especie

## significance of the effect
## original cover
significance_original25_reg1 <- sapply (tab_summarized_original25, "[[","effectReg1")
significance_original25_reg2 <- sapply (tab_summarized_original25, "[[","effectReg2")
## transforming No and yes into 1 (significant effect) and 0 (no effect)
significance_original25_reg1 <- ifelse (significance_original25_reg1 =="Yes",1,0)
significance_original25_reg2 <- ifelse (significance_original25_reg2 =="Yes",1,0)
## setting rownames
rownames(significance_original25_reg1 )<- especie
rownames(significance_original25_reg2)<- especie

## site occupancy
significance_occupancy25_reg1 <-sapply (tab_summarized_occupancy25, "[[","effectReg1")
significance_occupancy25_reg2 <-sapply (tab_summarized_occupancy25, "[[","effectReg2")
## transforming No and yes into 1 (significant effect) and 0 (no effect)
significance_occupancy25_reg1 <- ifelse (significance_occupancy25_reg1 =="Yes",1,0)
significance_occupancy25_reg2 <- ifelse (significance_occupancy25_reg2 =="Yes",1,0)
## setting rownames
rownames(significance_occupancy25_reg1)<- especie
rownames(significance_occupancy25_reg2)<- especie

## saving
save(efeito_para_bipartite_original25_reg1 ,
     efeito_para_bipartite_original25_reg2 ,
     efeito_para_bipartite_occupancy25_reg1 ,
     efeito_para_bipartite_occupancy25_reg2 ,
     significance_original25_reg1 ,
     significance_original25_reg2 ,
     significance_occupancy25_reg1 ,
     significance_occupancy25_reg2,
     file=here("output","coefficients_DISCOUNTING25.RData")) 


###### LOSS OF 50% OF COVER OR OCCUPANCY PROBABILITY

## LOAD MODEL RESULTS

load(here("output","samples_OCCcoral_PdepthObsID.RData")) 
res1_orig50 <- samples_OCCcoral_PdepthObsID$cob_desc50
res1_occ50 <- samples_OCCcoral_PdepthObsID$ocupacao_desc50
rm(samples_OCCcoral_PdepthObsID)
gc()
load(here("output","samples_OCCcoral_PdepthObsIDRndm.RData"))
res2_orig50 <- samples_OCCcoral_PdepthObsIDRndm$cob_desc50
res2_occ50 <- samples_OCCcoral_PdepthObsIDRndm$ocupacao_desc50
rm(samples_OCCcoral_PdepthObsIDRndm)
gc()

load(here("output","samples_OCCcoralDepth_PObsIDRndm.RData"))
res3_orig50 <- samples_OCCcoralDepth_PObsIDRndm$cob_desc50
res3_occ50 <- samples_OCCcoralDepth_PObsIDRndm$ocupacao_desc50
rm(samples_OCCcoralDepth_PObsIDRndm)
gc()

load(here("output","StaticModelOccCoral_IDobsRdmP.RData"))
res4_orig50 <- StaticModelOccCoral_IDobsRdmP$cob_desc50
res4_occ50 <- StaticModelOccCoral_IDobsRdmP$ocupacao_desc50
rm(StaticModelOccCoral_IDobsRdmP)
gc()

##

## Creating a list with the results of each model
lista_modelos_original50 <- list(res1_orig50,
                                 res2_orig50,
                                 res3_orig50,
                                 res4_orig50)
lista_modelos_occupancy50 <- list(res1_occ50,
                                  res2_occ50,
                                  res3_occ50,
                                  res4_occ50)

# nomear a lista de resultados
lista_modelos_original50 <- lapply (lista_modelos_original50, function (i) {names (i) <- sp_coral; i})
lista_modelos_occupancy50 <- lapply (lista_modelos_occupancy50, function (i) {names (i) <- sp_coral; i})

## table of bayesian P value for models with original coral cover
bvclosed_original50 <- lapply (lista_modelos_original50, function (model)
  
  lapply (model, function (i)
    
    unlist(lapply (i, function (k)
      
      sum (k$sims.list$Chi2repClosed > k$sims.list$Chi2Closed)/
        length(k$sims.list$Chi2repClosed)
    )
    )
  )
)
## table of bayesian P value for models with estimated site occupancy
bvclosed_occupancy50 <- lapply (lista_modelos_occupancy50, function (model)
  
  lapply (model, function (i)
    
    unlist(lapply (i, function (k)
      
      sum (k$sims.list$Chi2repClosed > k$sims.list$Chi2Closed)/
        length(k$sims.list$Chi2repClosed)
    )
    )
  )
)
# nomear a lista de resultados
bvclosed_original50 <- lapply (bvclosed_original50, function (i) {names (i) <- sp_coral; i})
bvclosed_occupancy50 <- lapply (bvclosed_occupancy50, function (i) {names (i) <- sp_coral; i})

# reorganizing the list
tab_bpv_sp_original50 <- lapply (sp_coral, function (sp)
  lapply (bvclosed_original50, function (i) 
    i [ which (names(i) == sp)])
)
tab_bpv_sp_occupancy50 <- lapply (sp_coral, function (sp)
  lapply (bvclosed_occupancy50, function (i) 
    i [ which (names(i) == sp)])
)

## transforming into matrix
tab_bpv_sp_original50 <- lapply (tab_bpv_sp_original50, function (i) 
  data.frame (matrix(unlist(i),ncol= length(bvclosed_original50),byrow = F )))
names(tab_bpv_sp_original50)<- sp_coral

tab_bpv_sp_occupancy50 <- lapply (tab_bpv_sp_occupancy50, function (i) 
  data.frame (matrix(unlist(i),ncol= length(bvclosed_occupancy50),byrow = F )))
names(tab_bpv_sp_occupancy50)<- sp_coral

## selecao do melhor modelo por especie 
## de acordo com o  BPV mais proximo a 0.5 (using the function 'closest')

# model with BPV closest to 0.5 (better fit to data)
# original cover
mod_sel_original50 <- lapply (seq (1,length(tab_bpv_sp_original50)), function (i) # sp coral - lista
  
  do.call(rbind, lapply (seq (1,nrow (tab_bpv_sp_original50 [[i]])), function (k) { # sp peixe - linhas
    
    
    ## obter o valor de bpv mais proxima a 0.5
    closestbpv <-as.numeric(closest (tab_bpv_sp_original50[[i]][k,],0.5)[1])
    
    ## no caso de BPV=0 em todos, pega o primeiro modelo que eh o mais parcimonioso (menos parametros)
    qual.melhor <- ifelse (closestbpv == 0, 1, which(tab_bpv_sp_original50[[i]][k,] == closestbpv)) 
    
    
  }
  )
  )
)

names(mod_sel_original50)<- sp_coral
# site-occupancy probability
mod_sel_occupancy50 <- lapply (seq (1,length(tab_bpv_sp_occupancy50)), function (i) # sp coral - lista
  
  do.call(rbind, lapply (seq (1,nrow (tab_bpv_sp_occupancy50 [[i]])), function (k) { # sp peixe - linhas
    
    
    ## obter o valor de bpv mais proxima a 0.5
    closestbpv <-as.numeric(closest (tab_bpv_sp_occupancy50[[i]][k,],0.5)[1])
    
    ## no caso de BPV=0 em todos, pega o primeiro modelo que eh o mais parcimonioso (menos parametros)
    qual.melhor <- ifelse (closestbpv == 0, 1, which(tab_bpv_sp_occupancy50[[i]][k,] == closestbpv)) 
    
    
  }
  )
  )
)

names(mod_sel_occupancy50)<- sp_coral

## selecting models
# original
selected.models_original50 <- lapply (seq (1,length (lista_modelos_original50 [[1]])), function (k)
  lapply (seq (1,length (lista_modelos_original50 [[1]][[1]])), function (i) {
    
    sp.coral <-  k
    sp.peixe <-  i
    qual.sel <- mod_sel_original50 [[sp.coral]][sp.peixe,]
    
    
    selected.models <- (lista_modelos_original50 [[qual.sel]] # ## seleciona o modelo mod, 
                        [[sp.coral]] ## da sp de coral k
                        [[sp.peixe]]) ## sp de peixe i
    ; selected.models
    
  }))

names(selected.models_original50) <- sp_coral

# occupancy
selected.models_occupancy50 <- lapply (seq (1,length (lista_modelos_occupancy50 [[1]])), function (k)
  lapply (seq (1,length (lista_modelos_occupancy50 [[1]][[1]])), function (i) {
    
    sp.coral <-  k
    sp.peixe <-  i
    qual.sel <- mod_sel_occupancy50 [[sp.coral]][sp.peixe,]
    
    
    selected.models <- (lista_modelos_occupancy50 [[qual.sel]] # ## seleciona o modelo mod, 
                        [[sp.coral]] ## da sp de coral k
                        [[sp.peixe]]) ## sp de peixe i
    ; selected.models
    
  }))

names(selected.models_occupancy50) <- sp_coral

## de agora em diante, trabalhar somente com os modelos selecionados

## efeitos original
efeitos_original50 <-  lapply (selected.models_original50, function (i) 
  
  do.call(cbind,lapply (i, function (k)
    
    k$summary [grep("beta1",rownames(k$summary)),"overlap0"])) ## pegar o subset com efeito significativo
  ## extrair os betas e se ou nao sobrepoe 0
)

tab_efeitos_sp_original50 <- lapply (efeitos_original50, function (i)
  ifelse (i == 1, "No", "Yes")
)

# efeitos occupancy
efeitos_occupancy50 <-  lapply (selected.models_occupancy50, function (i) 
  
  do.call (cbind, lapply (i, function (k)
    
    k$summary [grep("beta1",rownames(k$summary)),"overlap0"])) ## pegar o subset com efeito significativo
  ## extrair os betas e se ou nao sobrepoe 0
)

tab_efeitos_sp_occupancy50 <- lapply (efeitos_occupancy50, function (i)
  ifelse (i == 1, "No", "Yes")
)

#### coeficientes de regressao
# original cover
coefic_original50 <-  lapply (selected.models_original50, function (i) 
  
  do.call (cbind,lapply (i, function (k)
    
    k$summary [grep("beta1",rownames(k$summary)),"mean"])) ## pegar o subset com efeito significativo
  ## extrair os betas e se ou nao sobrepoe 0
)
# site-occupancy
coefic_occupancy50 <-  lapply (selected.models_occupancy50, function (i) 
  
  do.call (cbind,lapply (i, function (k)
    
    k$summary [grep("beta1",rownames(k$summary)),"mean"])) ## pegar o subset com efeito significativo
  ## extrair os betas e se ou nao sobrepoe 0
)


## finite sample size (n.occ)
#### original cover
nocc_original50 <- lapply (selected.models_original50, function (i) 
  
  unlist (lapply (i, function (k)
    
    k$summary [grep("n.occ",rownames(k$summary)),"mean"])) ## pegar o subset com efeito significativo
  ## extrair os betas e se ou nao sobrepoe 0
)

#### site-occupancy
nocc_occupancy50 <- lapply (selected.models_occupancy50, function (i) 
  
  unlist (lapply (i, function (k)
    
    k$summary [grep("n.occ",rownames(k$summary)),"mean"])) ## pegar o subset com efeito significativo
  ## extrair os betas e se ou nao sobrepoe 0
)

cbind(nocc_original50[[1]],nocc_occupancy50[[1]])

######## detection probability
det_prob_original50 <-  lapply (selected.models_original50, function (i) 
  
  unlist (lapply (i, function (k)
    
    k$summary [grep("mean.p",rownames(k$summary)),"mean"])) ## pegar o subset com efeito significativo
  ## extrair os betas e se ou nao sobrepoe 0
)

det_prob_occupancy50 <-  lapply (selected.models_occupancy50, function (i) 
  
  unlist (lapply (i, function (k)
    
    k$summary [grep("mean.p",rownames(k$summary)),"mean"])) ## pegar o subset com efeito significativo
  ## extrair os betas e se ou nao sobrepoe 0
)

cbind(det_prob_original50[[1]],det_prob_occupancy50[[1]])

## bayesian p-value dos selecionados
# original
bpv.selected_original50 <- lapply (selected.models_original50, function (i)
  
  unlist(lapply (i, function (k)
    
    sum (k$sims.list$Chi2repClosed > k$sims.list$Chi2Closed)/
      length(k$sims.list$Chi2repClosed)
  )
  )
)

# occupancy
bpv.selected_occupancy50 <- lapply (selected.models_occupancy50, function (i)
  
  unlist(lapply (i, function (k)
    
    sum (k$sims.list$Chi2repClosed > k$sims.list$Chi2Closed)/
      length(k$sims.list$Chi2repClosed)
  )
  )
)

cbind(bpv.selected_original50[[1]], bpv.selected_occupancy50[[1]])

## tabela com as info do efeito das covs de coral nas spp
tab_summarized_original50 <- lapply(seq(1,length(tab_efeitos_sp_original50)), function(i)
  data.frame(sp=especie, 
             effectReg1 = tab_efeitos_sp_original50[[i]][1,],
             effectReg2 = tab_efeitos_sp_original50[[i]][2,],
             coefReg1 = coefic_original50[[i]][1,],
             coefReg2 = coefic_original50[[i]][2,],
             #int = intercept[[i]],
             nocc = nocc_original50[[i]],
             det = det_prob_original50 [[i]],
             bpv = bpv.selected_original50[[i]],
             which_model=mod_sel_original50[[i]]))


## nomes das sp de coral
names(tab_summarized_original50) <- sp_coral

## occupancy
tab_summarized_occupancy50 <- lapply(seq(1,length(tab_efeitos_sp_occupancy50)), function(i)
  data.frame(sp=especie, 
             effectReg1 = tab_efeitos_sp_occupancy50[[i]][1,],
             effectReg2 = tab_efeitos_sp_occupancy50[[i]][2,],
             coefReg1 = coefic_occupancy50[[i]][1,],
             coefReg2 = coefic_occupancy50[[i]][2,],
             #int = intercept[[i]],
             nocc = nocc_occupancy50[[i]],
             det = det_prob_occupancy50 [[i]],
             bpv = bpv.selected_occupancy50[[i]],
             which_model=mod_sel_occupancy50[[i]])
)

## nomes das sp de coral
names(tab_summarized_occupancy50) <- sp_coral
## effect (regression coefficient for each region)
## original cover
efeito_para_bipartite_original50_reg1 <- sapply (tab_summarized_original50, "[[","coefReg1")
efeito_para_bipartite_original50_reg2 <- sapply (tab_summarized_original50, "[[","coefReg2")
## setting rownames
rownames(efeito_para_bipartite_original50_reg1)<- especie
rownames(efeito_para_bipartite_original50_reg2)<- especie

## site occupancy
efeito_para_bipartite_occupancy50_reg1 <-sapply (tab_summarized_occupancy50, "[[","coefReg1")
efeito_para_bipartite_occupancy50_reg2 <-sapply (tab_summarized_occupancy50, "[[","coefReg2")
## setting rownames
rownames(efeito_para_bipartite_occupancy50_reg1)<- especie
rownames(efeito_para_bipartite_occupancy50_reg2)<- especie

## significance of the effect
## original cover
significance_original50_reg1 <- sapply (tab_summarized_original50, "[[","effectReg1")
significance_original50_reg2 <- sapply (tab_summarized_original50, "[[","effectReg2")
## transforming No and yes into 1 (significant effect) and 0 (no effect)
significance_original50_reg1 <- ifelse (significance_original50_reg1 =="Yes",1,0)
significance_original50_reg2 <- ifelse (significance_original50_reg2 =="Yes",1,0)
## setting rownames
rownames(significance_original50_reg1 )<- especie
rownames(significance_original50_reg2)<- especie

## site occupancy
significance_occupancy50_reg1 <-sapply (tab_summarized_occupancy50, "[[","effectReg1")
significance_occupancy50_reg2 <-sapply (tab_summarized_occupancy50, "[[","effectReg2")
## transforming No and yes into 1 (significant effect) and 0 (no effect)
significance_occupancy50_reg1 <- ifelse (significance_occupancy50_reg1 =="Yes",1,0)
significance_occupancy50_reg2 <- ifelse (significance_occupancy50_reg2 =="Yes",1,0)
## setting rownames
rownames(significance_occupancy50_reg1)<- especie
rownames(significance_occupancy50_reg2)<- especie

## saving
save(efeito_para_bipartite_original50_reg1 ,
     efeito_para_bipartite_original50_reg2 ,
     efeito_para_bipartite_occupancy50_reg1 ,
     efeito_para_bipartite_occupancy50_reg2 ,
     significance_original50_reg1 ,
     significance_original50_reg2 ,
     significance_occupancy50_reg1 ,
     significance_occupancy50_reg2,
     file=here("output","coefficients_DISCOUNTING50.RData")) 

